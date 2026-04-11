//! Rewindable VM state snapshots.
//!
//! A `Snapshot` captures the entire mutable execution state of a `Vm` paused
//! at a fork: stack, frames, iterators, jump stacks, globals, variation
//! state, RNG state, and the currently-presented choices. Snapshots are
//! taken inside the `.fork` opcode handler (see `Vm.captureFork`) and held
//! in a bounded ring buffer on the VM, indexed by `vm.history_cursor` for
//! undo/redo.
//!
//! ## Heap clone strategy
//! Mutable heap objects (`list`, `map`, `set`, `instance`) are deep-cloned
//! into freshly allocated GC objects via `vm.gc.create`. Immutable types
//! (`string`, `enum`, `function`, `extern`, `builtin`, `class`, `anchor`)
//! are shared by pointer — they are never mutated, so reference sharing is
//! safe. A dedup map (`*Obj` → `*Obj`) ensures cyclic graphs terminate and
//! aliasing is preserved across the clone.
//!
//! ## GC interaction
//! `Gc.create` may trigger collection if `gc.allocated > gc.threshold`. Mid
//! clone, freshly created clones are not yet held by any root and would be
//! swept. Both `capture` and `restore` raise `gc.threshold` to "infinity"
//! for the duration of the clone walk and restore it afterward.
//!
//! Snapshots themselves act as GC roots: `Vm.markRoots` walks
//! `vm.history` and calls `Snapshot.markRoots` on each so the cloned objects
//! survive collections that happen between fork captures.
//!
//! ## Choice content lifetime
//! `Choice.content` and each tag string are slices into heap-string objects.
//! At capture time we dupe the actual bytes into snapshot-owned vm.alloc
//! buffers; on restore we allocate fresh slice headers (so `selectChoice`
//! can free them) but the slices borrow the snapshot's bytes. This works
//! because the snapshot lives in `vm.history` until at least the next fork
//! is captured, well after the user picks at the rewound fork.

const std = @import("std");

const types = @import("../types/index.zig");
const Value = types.Value;
const Iterator = types.Iterator;
const Class = types.Class;

const utils = @import("../utils/index.zig");
const C = utils.C;

const Vm = @import("vm.zig").Vm;
const Frame = @import("frame.zig").Frame;
const Gc = @import("gc.zig").Gc;
const builtins = @import("builtins.zig");
const runner = @import("runner.zig");
const Choice = runner.Choice;

/// Map from original object pointer to its clone, used during a clone walk
/// to terminate cycles and preserve aliasing.
pub const DedupMap = std.AutoHashMap(*Value.Obj, *Value.Obj);

/// One immutable snapshot of VM state taken at a fork pause point.
pub const Snapshot = struct {
    // Operand stack contents (count = stack_items.len)
    stack_items: []Value,

    // Call frames (each is bit-copied; func ptrs are immortal)
    frames: []Frame,

    // Active iterators
    iterators: []Iterator,

    // Cached vm.ip at capture time (mostly redundant with frames[top].ip
    // but captured for safety; restored before re-entering vm.run)
    ip: usize,

    // Jump-control stacks (slices of integers)
    jump_backups: []C.JUMP,
    jump_requests: []C.JUMP,
    anchor_stack: []C.CONSTANT,

    // Full globals snapshot. Length matches vm.globals.len at capture time.
    globals: []Value,

    // Deep-cloned variation state map (cycle/sequence/shuffle indices,
    // shuffle order ArrayList is owned).
    variation_state: builtins.VariationMap,

    is_waiting: bool,
    can_continue: bool,

    // Currently-presented choices, paused at fork. Slice header is
    // snapshot-owned. Each Choice's `content` and `tags` slice header are
    // also snapshot-owned. Each tag string and the content bytes are
    // dup'd into `choice_bytes` (an arena of dup'd []u8 slices).
    current_choices: []Choice,
    choice_bytes: std.ArrayList([]u8),

    // Captured RNG state (null if RNG was never initialised)
    rng_state: ?std.Random.DefaultPrng,

    pub fn deinit(self: *Snapshot, alloc: std.mem.Allocator) void {
        alloc.free(self.stack_items);
        alloc.free(self.frames);
        alloc.free(self.iterators);
        alloc.free(self.jump_backups);
        alloc.free(self.jump_requests);
        alloc.free(self.anchor_stack);
        alloc.free(self.globals);

        var vs_it = self.variation_state.iterator();
        while (vs_it.next()) |entry| {
            if (entry.value_ptr.* == .shuffle) {
                entry.value_ptr.shuffle.order.deinit(alloc);
            }
        }
        self.variation_state.deinit(alloc);

        for (self.current_choices) |c| {
            alloc.free(c.tags);
        }
        alloc.free(self.current_choices);

        for (self.choice_bytes.items) |buf| alloc.free(buf);
        self.choice_bytes.deinit(alloc);

        self.* = undefined;
    }

    /// Mark every Value reachable from this snapshot so the GC does not
    /// sweep cloned objects between fork captures.
    pub fn markRoots(self: *const Snapshot) void {
        for (self.stack_items) |v| Gc.markValue(v);
        for (self.iterators) |it| Gc.markValue(it.value);
        for (self.globals) |v| Gc.markValue(v);
        // Frames hold *Obj pointers to function objects. The main frame's
        // function obj is a separately-allocated, non-GC object; all others
        // come from bytecode.constants and are likewise non-GC. We still
        // mark them defensively in case that ever changes.
        for (self.frames) |f| Gc.markValue(.{ .obj = f.func });
    }

    /// Capture a snapshot of `vm`'s mutable state. Caller (Vm.captureFork)
    /// is responsible for raising `vm.gc.threshold` for the duration of
    /// this call so freshly-cloned objects survive a mid-clone collection.
    pub fn capture(vm: *Vm) !Snapshot {
        var dedup = DedupMap.init(vm.alloc);
        defer dedup.deinit();

        // Operand stack
        const stack_items = try vm.alloc.alloc(Value, vm.stack.count);
        errdefer vm.alloc.free(stack_items);
        for (vm.stack.items[0..vm.stack.count], 0..) |v, i| {
            stack_items[i] = try cloneValue(vm, v, &dedup);
        }

        // Frames (bit-copied; func ptrs are immortal)
        const frames = try vm.alloc.alloc(Frame, vm.frames.count);
        errdefer vm.alloc.free(frames);
        @memcpy(frames, vm.frames.items[0..vm.frames.count]);

        // Iterators
        const iterators = try vm.alloc.alloc(Iterator, vm.iterators.count);
        errdefer vm.alloc.free(iterators);
        for (vm.iterators.items[0..vm.iterators.count], 0..) |it, i| {
            iterators[i] = .{
                .value = try cloneValue(vm, it.value, &dedup),
                .index = it.index,
            };
        }

        // Jump-control stacks
        const jump_backups = try vm.alloc.dupe(C.JUMP, vm.jump_backups.items);
        errdefer vm.alloc.free(jump_backups);
        const jump_requests = try vm.alloc.dupe(C.JUMP, vm.jump_requests.items);
        errdefer vm.alloc.free(jump_requests);
        const anchor_stack = try vm.alloc.dupe(C.CONSTANT, vm.anchor_stack.items);
        errdefer vm.alloc.free(anchor_stack);

        // Globals
        const globals = try vm.alloc.alloc(Value, vm.globals.len);
        errdefer vm.alloc.free(globals);
        for (vm.globals, 0..) |v, i| {
            globals[i] = try cloneValue(vm, v, &dedup);
        }

        // Variation state
        var variation_state = builtins.VariationMap.empty;
        errdefer {
            var it = variation_state.iterator();
            while (it.next()) |e| {
                if (e.value_ptr.* == .shuffle) e.value_ptr.shuffle.order.deinit(vm.alloc);
            }
            variation_state.deinit(vm.alloc);
        }
        var vs_it = vm.variation_state.iterator();
        while (vs_it.next()) |entry| {
            const cloned: builtins.VariationState = switch (entry.value_ptr.*) {
                .cycle => |i| .{ .cycle = i },
                .sequence => |i| .{ .sequence = i },
                .shuffle => |s| blk: {
                    var order: std.ArrayList(u32) = .empty;
                    try order.appendSlice(vm.alloc, s.order.items);
                    break :blk .{ .shuffle = .{ .index = s.index, .order = order } };
                },
            };
            try variation_state.put(vm.alloc, entry.key_ptr.*, cloned);
        }

        // Current choices: dupe tags slice and bytes for each choice's
        // content + each tag string into snapshot-owned buffers.
        var choice_bytes: std.ArrayList([]u8) = .empty;
        errdefer {
            for (choice_bytes.items) |b| vm.alloc.free(b);
            choice_bytes.deinit(vm.alloc);
        }

        const choices_src: []Choice = if (vm.choices_freed) &[_]Choice{} else vm.current_choices;
        const current_choices = try vm.alloc.alloc(Choice, choices_src.len);
        errdefer vm.alloc.free(current_choices);

        for (choices_src, 0..) |c, i| {
            const content_buf = try vm.alloc.dupe(u8, c.content);
            try choice_bytes.append(vm.alloc, content_buf);

            const tags = try vm.alloc.alloc([]const u8, c.tags.len);
            errdefer vm.alloc.free(tags);
            for (c.tags, 0..) |t, j| {
                const tag_buf = try vm.alloc.dupe(u8, t);
                try choice_bytes.append(vm.alloc, tag_buf);
                tags[j] = tag_buf;
            }
            current_choices[i] = .{
                .content = content_buf,
                .tags = tags,
                .visit_count = c.visit_count,
                .ip = c.ip,
            };
        }

        return .{
            .stack_items = stack_items,
            .frames = frames,
            .iterators = iterators,
            .ip = vm.ip,
            .jump_backups = jump_backups,
            .jump_requests = jump_requests,
            .anchor_stack = anchor_stack,
            .globals = globals,
            .variation_state = variation_state,
            .is_waiting = vm.is_waiting,
            .can_continue = vm.can_continue,
            .current_choices = current_choices,
            .choice_bytes = choice_bytes,
            .rng_state = builtins.snapshotRng(),
        };
    }

    /// Restore the snapshot's state into `vm`. Allocates fresh GC clones
    /// of mutable heap objects out of the snapshot, leaving the snapshot
    /// itself untouched (so it remains valid for subsequent re-rewinds).
    /// Caller (Vm.rewind/redo) is responsible for raising gc.threshold
    /// for the duration of this call.
    pub fn restore(self: *const Snapshot, vm: *Vm) !void {
        var dedup = DedupMap.init(vm.alloc);
        defer dedup.deinit();

        // Operand stack
        vm.stack.resize(self.stack_items.len);
        for (self.stack_items, 0..) |v, i| {
            vm.stack.items[i] = try cloneValue(vm, v, &dedup);
        }

        // Frames
        // Pop everything except the main frame, then re-push from snapshot.
        while (vm.frames.count > 0) _ = vm.frames.pop();
        for (self.frames) |f| vm.frames.push(f);

        // Iterators
        vm.iterators.resize(self.iterators.len);
        for (self.iterators, 0..) |it, i| {
            vm.iterators.items[i] = .{
                .value = try cloneValue(vm, it.value, &dedup),
                .index = it.index,
            };
        }

        // Jump-control stacks
        vm.jump_backups.clearRetainingCapacity();
        try vm.jump_backups.appendSlice(vm.alloc, self.jump_backups);
        vm.jump_requests.clearRetainingCapacity();
        try vm.jump_requests.appendSlice(vm.alloc, self.jump_requests);
        vm.anchor_stack.clearRetainingCapacity();
        try vm.anchor_stack.appendSlice(vm.alloc, self.anchor_stack);

        // Globals (length is fixed at vm.init; assert match)
        std.debug.assert(vm.globals.len == self.globals.len);
        for (self.globals, 0..) |v, i| {
            vm.globals[i] = try cloneValue(vm, v, &dedup);
        }

        // Variation state
        {
            var it = vm.variation_state.iterator();
            while (it.next()) |e| {
                if (e.value_ptr.* == .shuffle) e.value_ptr.shuffle.order.deinit(vm.alloc);
            }
            vm.variation_state.clearRetainingCapacity();
        }
        var vs_it = self.variation_state.iterator();
        while (vs_it.next()) |entry| {
            const cloned: builtins.VariationState = switch (entry.value_ptr.*) {
                .cycle => |i| .{ .cycle = i },
                .sequence => |i| .{ .sequence = i },
                .shuffle => |s| blk: {
                    var order: std.ArrayList(u32) = .empty;
                    try order.appendSlice(vm.alloc, s.order.items);
                    break :blk .{ .shuffle = .{ .index = s.index, .order = order } };
                },
            };
            try vm.variation_state.put(vm.alloc, entry.key_ptr.*, cloned);
        }

        // Current choices: free what's there, then re-allocate fresh slice
        // headers borrowing from snapshot's owned bytes.
        if (!vm.choices_freed) {
            for (vm.current_choices) |c| vm.alloc.free(c.tags);
            vm.alloc.free(vm.current_choices);
            vm.choices_freed = true;
        }
        const restored_choices = try vm.alloc.alloc(Choice, self.current_choices.len);
        for (self.current_choices, 0..) |c, i| {
            const tags = try vm.alloc.alloc([]const u8, c.tags.len);
            for (c.tags, 0..) |t, j| tags[j] = t;
            restored_choices[i] = .{
                .content = c.content,
                .tags = tags,
                .visit_count = c.visit_count,
                .ip = c.ip,
            };
        }
        vm.current_choices = restored_choices;
        vm.choices_freed = false;

        // Scalars. Resync vm.ip from the (now-restored) current frame so the
        // run-loop's `while (self.ip < instructions.len)` check holds even
        // if the captured vm.ip was stale relative to the captured frame.
        vm.ip = if (vm.frames.count > 0) vm.frames.peek().ip else self.ip;
        vm.is_waiting = self.is_waiting;
        vm.can_continue = self.can_continue;

        // RNG
        builtins.restoreRng(self.rng_state);
    }
};

/// Recursively clone a Value, deep-cloning mutable heap objects via
/// `vm.gc.create` and sharing immutable ones by pointer. Cycles and
/// aliasing are preserved via the dedup map.
pub fn cloneValue(vm: *Vm, value: Value, dedup: *DedupMap) std.mem.Allocator.Error!Value {
    return switch (value) {
        .obj => |o| .{ .obj = try cloneObj(vm, o, dedup) },
        // Map pairs hold pointers into a map's storage; in practice they
        // are transient values produced inside iteration and never live
        // across a fork. Bit-copy is fine.
        else => value,
    };
}

fn cloneObj(vm: *Vm, obj: *Value.Obj, dedup: *DedupMap) std.mem.Allocator.Error!*Value.Obj {
    if (dedup.get(obj)) |existing| return existing;

    // Immutable types: share by reference. Strings/enums/functions/classes/
    // externs/builtins/anchors are never mutated after creation.
    switch (obj.data) {
        .string,
        .@"enum",
        .function,
        .@"extern",
        .builtin,
        .class,
        .anchor,
        => {
            try dedup.put(obj, obj);
            return obj;
        },
        .list, .map, .set, .instance => {},
    }

    // Mutable types: allocate the new GC object first (with empty contents),
    // register in the dedup map, then recursively clone children. Order is
    // critical for cycle handling: a list that contains itself must see its
    // own clone in the dedup map before the recursive walk encounters it.
    switch (obj.data) {
        .list => |l| {
            var new_list: std.ArrayList(Value) = .empty;
            try new_list.ensureTotalCapacity(vm.alloc, l.items.len);
            const new_value = try vm.gc.create(vm, .{ .list = new_list });
            try dedup.put(obj, new_value.obj);
            for (l.items) |item| {
                const cloned = try cloneValue(vm, item, dedup);
                try new_value.obj.data.list.append(vm.alloc, cloned);
            }
            return new_value.obj;
        },
        .map => |m| {
            var new_map = Value.Obj.MapType.empty;
            try new_map.ensureTotalCapacity(vm.alloc, @intCast(m.count()));
            const new_value = try vm.gc.create(vm, .{ .map = new_map });
            try dedup.put(obj, new_value.obj);
            const keys = m.keys();
            const values = m.values();
            for (keys, values) |k, v| {
                const ck = try cloneValue(vm, k, dedup);
                const cv = try cloneValue(vm, v, dedup);
                try new_value.obj.data.map.put(vm.alloc, ck, cv);
            }
            return new_value.obj;
        },
        .set => |s| {
            var new_set = Value.Obj.SetType.empty;
            try new_set.ensureTotalCapacity(vm.alloc, @intCast(s.count()));
            const new_value = try vm.gc.create(vm, .{ .set = new_set });
            try dedup.put(obj, new_value.obj);
            for (s.keys()) |k| {
                const ck = try cloneValue(vm, k, dedup);
                try new_value.obj.data.set.put(vm.alloc, ck, {});
            }
            return new_value.obj;
        },
        .instance => |inst| {
            const new_fields = try vm.alloc.alloc(Value, inst.fields.len);
            // Initialise to void so any GC sweep mid-clone sees a coherent obj.
            @memset(new_fields, .{ .void = {} });
            const new_value = try vm.gc.create(vm, .{ .instance = .{
                .base = inst.base, // class is immutable, share
                .fields = new_fields,
            } });
            try dedup.put(obj, new_value.obj);
            for (inst.fields, 0..) |f, i| {
                new_value.obj.data.instance.fields[i] = try cloneValue(vm, f, dedup);
            }
            return new_value.obj;
        },
        else => unreachable,
    }
}
