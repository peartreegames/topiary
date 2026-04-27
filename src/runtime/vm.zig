const std = @import("std");

const backend = @import("../backend/index.zig");
const Compiler = backend.Compiler;
const Bytecode = backend.Bytecode;
const OpCode = backend.OpCode;

const types = @import("../types/index.zig");
const Class = types.Class;
const Value = types.Value;
const Nil = types.Nil;
const True = types.True;
const False = types.False;
const Void = types.Void;
const Iterator = types.Iterator;
const Function = types.Function;
const Extern = types.Extern;

const utils = @import("../utils/index.zig");
const UUID = utils.UUID;
const C = utils.C;

const Stack = @import("stack.zig").Stack;
const Gc = @import("gc.zig").Gc;
const Frame = @import("frame.zig").Frame;
const builtins = @import("builtins.zig");
const Snapshot = @import("snapshot.zig").Snapshot;

const runners = @import("runner.zig");
const Runner = runners.Runner;
const Line = runners.Line;
const Choice = runners.Choice;
const RuntimeErr = @import("error.zig").RuntimeErr;

const LocaleProvider = @import("../locale.zig").LocaleProvider;

const stack_size = 4096;
const frame_size = 255;
const iterator_size = 255;
const globals_size = 65535;

const InterpretResult = union(enum) {
    Completed,
    Paused,
};

/// Virtual Machine
/// Executes bytecode
pub const Vm = struct {
    alloc: std.mem.Allocator,
    io: std.Io,
    frames: Stack(Frame),
    err: RuntimeErr,
    gc: Gc,
    globals: []Value,
    value_subscribers: std.StringHashMapUnmanaged(void) = .empty,
    notifiable_objects: std.AutoHashMapUnmanaged(UUID.ID, usize) = .empty,
    variation_state: builtins.VariationMap = .empty,
    builtin_cache: std.AutoHashMapUnmanaged(*const fn (*Vm, []Value) Value, Value) = .empty,

    stack: Stack(Value),
    /// Current iterators to allow easy nesting
    iterators: Stack(Iterator),
    /// List of positions to jump back to using `^`
    jump_backups: std.ArrayList(Backup) = .empty,
    /// Used to ensure preceding code is executed before arriving at Bough
    jump_requests: std.ArrayList(C.JUMP) = .empty,
    anchor_stack: std.ArrayList(C.CONSTANT) = .empty,

    bytecode: Bytecode,
    /// Current instruction position
    ip: usize = 0,
    break_on_assert: bool = true,

    /// Used to cache the choices
    choices_list: std.ArrayList(Choice) = .empty,
    /// Used to send to the on_choices method
    current_choices: []Choice = undefined,
    choices_freed: bool = true,
    /// Determines if the vm is waiting on input
    /// hopefully will remove with async?
    is_waiting: bool = false,
    can_continue: bool = false,

    loc_provider: ?*LocaleProvider = null,

    /// Bounded fork-history ring buffer for rewind/redo. When
    /// `history_capacity == 0`, snapshots are never taken (zero cost).
    /// `history_cursor` indexes the next free slot — i.e. it equals the
    /// number of "live" snapshots ahead of any redo entries. Snapshots
    /// at indices `[history_cursor..history.items.len)` are redo entries.
    history: std.ArrayList(Snapshot) = .empty,
    history_cursor: usize = 0,
    history_capacity: usize = 0,
    /// Set by `rewind`/`redo` to signal `run()` that it should re-fire
    /// `runner.onChoices` with the restored fork's choices instead of
    /// continuing instruction execution.
    rewind_pending: bool = false,

    runner: *Runner,

    pub const Error = error{
        RuntimeError,
        BoughNotFound,
        InvalidChoice,
        Uninitialized,
    } || Compiler.Error;

    pub const Backup = struct {
        ip: C.JUMP,
        anchor_depth: usize,
        is_fork: bool,
    };

    /// Initialize Vm
    pub fn init(allocator: std.mem.Allocator, io: std.Io, bytecode: *const Bytecode, runner: anytype) !Vm {
        const globals = try allocator.alloc(Value, bytecode.global_symbols.len);
        @memset(globals, .void);
        for (bytecode.constants) |c| {
            if (c != .obj) continue;
            switch (c.obj.data) {
                // init visit
                .anchor => globals[c.obj.data.anchor.visit_index] = .{ .visit = 0 },
                else => {},
            }
        }

        const main_func_obj = try allocator.create(Value.Obj);
        main_func_obj.* = .{
            .id = UUID.new(),
            .data = .{
                .function = .{
                    .arity = 0,
                    .instructions = bytecode.instructions,
                    .locals_count = bytecode.locals_count,
                    .debug_info = bytecode.debug_info,
                },
            },
        };
        var vm = Vm{
            .alloc = allocator,
            .io = io,
            .bytecode = bytecode.*,
            .frames = try Stack(Frame).init(allocator, frame_size),
            .err = .{},
            .globals = globals,
            .runner = runner,
            .gc = Gc.init(allocator),
            .stack = try Stack(Value).init(allocator, stack_size),
            .iterators = try Stack(Iterator).init(allocator, iterator_size),
        };

        // Initialize the entire operand stack backing to .void. Local slots
        // are reserved by `resize` (here and at function-call sites) without
        // being explicitly written; the compiler emits set_local before any
        // get_local in normal execution, but snapshot capture and GC root
        // marking iterate `stack.items[0..count]` unconditionally and would
        // otherwise observe uninitialized memory.
        @memset(vm.stack.backing, Void);
        vm.stack.resize(bytecode.locals_count);
        vm.frames.push(try Frame.create(main_func_obj, 0));
        return vm;
    }

    pub fn deinit(self: *Vm) void {
        self.removeLocale();
        for (self.history.items) |*snap| snap.deinit(self.alloc);
        self.history.deinit(self.alloc);
        self.alloc.destroy(self.frames.backing[0].func);
        self.frames.deinit();
        self.gc.deinit();

        self.stack.deinit();
        self.iterators.deinit();
        self.choices_list.deinit(self.alloc);
        self.jump_backups.deinit(self.alloc);
        self.anchor_stack.deinit(self.alloc);
        self.jump_requests.deinit(self.alloc);
        self.notifiable_objects.deinit(self.alloc);
        self.value_subscribers.deinit(self.alloc);
        {
            var it = self.variation_state.iterator();
            while (it.next()) |entry| {
                if (entry.value_ptr.* == .shuffle) {
                    entry.value_ptr.shuffle.order.deinit(self.alloc);
                }
            }
            self.variation_state.deinit(self.alloc);
        }
        {
            var it = self.builtin_cache.valueIterator();
            while (it.next()) |v| self.alloc.destroy(v.obj);
            self.builtin_cache.deinit(self.alloc);
        }
        self.alloc.free(self.globals);
        if (!self.choices_freed) {
            // Each Choice owns its `tags` slice (allocated by the `.choice`
            // opcode and reallocated fresh by `Snapshot.restore` after a
            // rewind/redo). selectChoice frees both, but if the VM is
            // destroyed while still paused at a fork the tag arrays must
            // be released here too.
            for (self.current_choices) |c| self.alloc.free(c.tags);
            self.alloc.free(self.current_choices);
        }
        self.err.deinit(self.alloc);
    }

    fn builtinValue(self: *Vm, b: builtins.Builtin) !Value {
        const gop = try self.builtin_cache.getOrPut(self.alloc, b.backing);
        if (!gop.found_existing) {
            const obj = try self.alloc.create(Value.Obj);
            obj.* = .{ .data = .{ .builtin = b } };
            gop.value_ptr.* = .{ .obj = obj };
        }
        return gop.value_ptr.*;
    }

    pub fn setLocale(self: *Vm, path: []const u8) !void {
        if (self.loc_provider) |lp| {
            if (std.mem.eql(u8, lp.key, path)) return;
        }
        const file = try std.Io.Dir.cwd().openFile(self.io, path, .{});
        defer file.close(self.io);

        const size = (try file.stat(self.io)).size;
        var buf: [1024]u8 = undefined;
        var reader = file.reader(self.io, &buf);
        const buffer = try reader.interface.readAlloc(self.alloc, size);
        errdefer self.alloc.free(buffer);
        self.removeLocale();
        self.loc_provider = try LocaleProvider.init(self.alloc, path, buffer);
    }

    pub fn setLocaleFromBuffer(self: *Vm, key: []const u8, buffer: []const u8) !void {
        const owned = try self.alloc.dupe(u8, buffer);
        errdefer self.alloc.free(owned);
        self.removeLocale();
        self.loc_provider = try LocaleProvider.init(self.alloc, key, owned);
    }

    pub fn removeLocale(self: *Vm) void {
        if (self.loc_provider) |lp| {
            lp.deinit(self.alloc);
            self.loc_provider = null;
        }
    }

    /// Called by the GC to enumerate every reachable Value root. Scans:
    /// - globals (all symbol storage)
    /// - the live portion of the operand stack
    /// - open iterators (each holds a `Value` being iterated)
    /// - rewind history snapshots (each holds cloned Values that must
    ///   survive collections between fork captures)
    /// Note: `variation_state` currently only holds `u32` indices, not Values,
    /// so it is not scanned. If that ever changes, add it here.
    pub fn markRoots(self: *Vm) void {
        for (self.globals) |v| Gc.markValue(v);
        for (self.stack.items) |v| Gc.markValue(v);
        for (self.iterators.items) |it| Gc.markValue(it.value);
        for (self.history.items) |*snap| snap.markRoots();
    }

    fn currentFrame(self: *Vm) *Frame {
        return self.frames.peek();
    }

    /// Continue execution
    pub fn selectContinue(self: *Vm) void {
        self.is_waiting = false;
    }

    pub fn selectChoice(self: *Vm, index: usize) Error!void {
        if (index < 0 or index >= self.current_choices.len) {
            return Error.InvalidChoice;
        }

        const choice = self.current_choices[index];
        self.currentFrame().ip = choice.ip;
        self.is_waiting = false;
        for (self.current_choices) |c| {
            self.alloc.free(c.tags);
        }
        self.alloc.free(self.current_choices);
        self.choices_freed = true;
    }

    /// Snapshot the current paused-at-fork state into the rewind history.
    /// No-op when `history_capacity == 0`. Called from the `.fork` opcode
    /// after `is_waiting=true` and before `runner.onChoices` is invoked.
    /// Drops any forward redo entries past `history_cursor` and evicts
    /// the oldest snapshot if the ring buffer is at capacity.
    pub fn captureFork(self: *Vm) !void {
        if (self.history_capacity == 0) return;

        // Drop forward redo entries: any new fork invalidates any future
        // we may have rewound from.
        while (self.history.items.len > self.history_cursor) {
            var dropped = self.history.pop().?;
            dropped.deinit(self.alloc);
        }

        // Evict oldest if at capacity.
        if (self.history.items.len >= self.history_capacity) {
            var dropped = self.history.orderedRemove(0);
            dropped.deinit(self.alloc);
            self.history_cursor -= 1;
        }

        // Disable GC for the clone walk so freshly created clones aren't
        // swept before they're held by the snapshot.
        const saved_threshold = self.gc.threshold;
        self.gc.threshold = std.math.maxInt(usize);
        defer self.gc.threshold = saved_threshold;

        const snap = try Snapshot.capture(self);
        try self.history.append(self.alloc, snap);
        self.history_cursor = self.history.items.len;
    }

    /// True iff there is a previous fork snapshot to rewind to.
    pub fn canRewind(self: *Vm) bool {
        // We can rewind iff there is a snapshot strictly older than the
        // currently-displayed fork. The newest snapshot in history matches
        // the *current* paused fork, so we need at least 2 entries to step
        // back.
        return self.history_cursor >= 2;
    }

    /// True iff there is a forward (redo) snapshot to advance to.
    pub fn canRedo(self: *Vm) bool {
        return self.history_cursor < self.history.items.len;
    }

    /// Rewind to the previous fork snapshot. After this call, `run()` will
    /// re-fire `runner.onChoices` for the rewound fork (via the
    /// `rewind_pending` re-entry path), letting the host present the
    /// previous fork's choices again.
    pub fn rewind(self: *Vm) Error!void {
        if (!self.canRewind()) return Error.InvalidChoice;
        self.history_cursor -= 1;
        const target = &self.history.items[self.history_cursor - 1];
        const saved_threshold = self.gc.threshold;
        self.gc.threshold = std.math.maxInt(usize);
        defer self.gc.threshold = saved_threshold;
        target.restore(self) catch return Error.RuntimeError;
        self.rewind_pending = true;
    }

    /// Re-do a previously-rewound fork.
    pub fn redo(self: *Vm) Error!void {
        if (!self.canRedo()) return Error.InvalidChoice;
        self.history_cursor += 1;
        const target = &self.history.items[self.history_cursor - 1];
        const saved_threshold = self.gc.threshold;
        self.gc.threshold = std.math.maxInt(usize);
        defer self.gc.threshold = saved_threshold;
        target.restore(self) catch return Error.RuntimeError;
        self.rewind_pending = true;
    }

    pub fn getGlobalsIndex(self: *Vm, name: []const u8) !usize {
        for (self.bytecode.global_symbols) |s| {
            if (!std.mem.eql(u8, name, s.name)) continue;
            return s.index;
        }
        return error.SymbolNotFound;
    }

    fn getExtern(self: *Vm, name: []const u8) !Value {
        return for (self.bytecode.constants) |e| {
            if (e == .obj and e.obj.data == .@"extern" and std.mem.eql(u8, name, e.obj.data.@"extern".name))
                break e;
        } else error.NotFound;
    }

    pub fn setExtern(self: *Vm, name: []const u8, arity: u8, context_ptr: *anyopaque, backing: Extern.Backing, destroy: Extern.Destroy) !void {
        const value = try self.getExtern(name);
        var ext = &value.obj.data.@"extern";
        if (ext.arity != arity) return error.InvalidArity;
        if (ext.context_ptr != null) return error.AlreadySet;
        ext.context_ptr = context_ptr;
        ext.backing = backing;
        ext.destroy = destroy;
    }

    pub fn removeExtern(self: *Vm, name: []const u8) !void {
        const value = try self.getExtern(name);
        var ext = &value.obj.data.@"extern";
        ext.context_ptr = null;
        ext.backing = undefined;
        ext.destroy = undefined;
    }

    pub fn subscribeToValueChange(self: *Vm, name: []const u8) !bool {
        if (self.value_subscribers.contains(name)) return true;
        for (self.bytecode.global_symbols) |s| {
            if (!std.mem.eql(u8, name, s.name)) continue;
            try self.value_subscribers.put(self.alloc, s.name, {});
            return true;
        }
        return false;
    }

    pub fn unsubscribeToValueChange(self: *Vm, name: []const u8) bool {
        return self.value_subscribers.remove(name);
    }

    pub fn notifyValueObjChange(self: *Vm, uuid: UUID.ID, new_value: Value) void {
        if (self.notifiable_objects.get(uuid)) |i| {
            return self.notifyValueChange(i, Void, new_value);
        }
    }

    pub fn notifyValueChange(self: *Vm, index: usize, old_value: Value, new_value: Value) void {
        const name = self.bytecode.global_symbols[index].name;
        if (self.value_subscribers.contains(name) and !old_value.eql(new_value)) {
            self.runner.onValueChanged(self, name, new_value);
        }
    }

    pub fn interpret(self: *Vm) !void {
        const path: ?[]const u8 = for (self.bytecode.constants) |v| {
            if (v == .obj and v.obj.data == .anchor) break v.obj.data.anchor.name;
        } else null;
        try self.start(path);
        while (self.can_continue) {
            try self.run();
        }
    }

    pub fn start(self: *Vm, start_path: ?[]const u8) !void {
        self.can_continue = true;
        self.stack.resize(self.bytecode.locals_count);
        while (self.frames.count > 1) {
            _ = self.frames.pop();
        }
        self.currentFrame().ip = 0;
        if (start_path) |path| {
            for (self.bytecode.constants, 0..) |v, i| {
                if (v == .obj and v.obj.data == .anchor) {
                    if (!std.mem.eql(u8, v.obj.data.anchor.name, path)) continue;
                    try self.prepareDivert(@intCast(i));
                    return;
                }
            } else {
                self.err.msg = try std.fmt.allocPrint(self.alloc, "Could not find starting path {s}", .{path});
                return Error.BoughNotFound;
            }
        }
    }

    fn suggestClassMember(self: *Vm, c: Class, name: []const u8) !?[]const u8 {
        var names: std.ArrayList([]const u8) = .empty;
        defer names.deinit(self.alloc);
        for (c.fields) |f| try names.append(self.alloc, f.name);
        for (c.methods) |m| try names.append(self.alloc, m.name);
        const match = (try backend.suggest.closest(self.alloc, name, names.items)) orelse return null;
        defer self.alloc.free(match);
        return try std.fmt.allocPrint(self.alloc, "did you mean '{s}'?", .{match});
    }

    fn failUnknownMethod(self: *Vm, comptime kind: []const u8, name: []const u8, valid: []const []const u8) !void {
        var public: std.ArrayList([]const u8) = .empty;
        defer public.deinit(self.alloc);
        for (valid) |v| if (v.len == 0 or v[0] != '_') try public.append(self.alloc, v);
        const valid_list = try std.mem.join(self.alloc, ", ", public.items);
        defer self.alloc.free(valid_list);
        const hint = blk: {
            const match = backend.suggest.closest(self.alloc, name, public.items) catch null;
            if (match) |m| {
                defer self.alloc.free(m);
                break :blk std.fmt.allocPrint(self.alloc, "did you mean '{s}'?", .{m}) catch null;
            }
            break :blk null;
        };
        return self.failWithHelp(
            "Unknown method '{s}' on " ++ kind ++ ". Valid methods are: {s}.",
            .{ name, valid_list },
            hint,
        );
    }

    fn failWithHelp(self: *Vm, comptime msg: []const u8, args: anytype, suggestion: ?[]const u8) !void {
        self.err.suggestion = suggestion;
        return self.fail(msg, args);
    }

    fn fail(self: *Vm, comptime msg: []const u8, args: anytype) !void {
        self.err.msg = try std.fmt.allocPrint(self.alloc, msg, args);

        var i: usize = self.frames.count;
        while (i > 0) {
            i -= 1;
            const frame = &self.frames.items[i];
            const ip = if (i == self.frames.count - 1) self.ip else frame.ip;
            const func_obj = frame.func;

            var trace_entry = RuntimeErr.Trace{
                .function_name = func_obj.data.function.name,
            };
            cont: for (func_obj.data.function.debug_info) |d| {
                for (d.ranges.items) |r| {
                    if (ip >= r.start and ip <= r.end) {
                        trace_entry.line = r.line;
                        trace_entry.file = d.file;
                        break :cont;
                    }
                }
            }

            try self.err.trace.append(self.alloc, trace_entry);
        }

        while (self.frames.count > 1) {
            _ = self.frames.pop();
        }
        self.can_continue = false;
        return Error.RuntimeError;
    }

    fn takeByte(self: *Vm) u8 {
        var frame = self.currentFrame();
        const byte = frame.instructions()[frame.ip];
        frame.ip += 1;
        return byte;
    }

    fn takeInt(self: *Vm, comptime T: type) T {
        var frame = self.currentFrame();
        if (T == u8) {
            return self.takeByte();
        } else {
            const result = std.mem.readVarInt(T, frame.instructions()[frame.ip..(frame.ip + @sizeOf(T))], .little);
            frame.ip += @sizeOf(T);
            return result;
        }
    }

    fn end(self: *Vm) void {
        self.can_continue = false;
    }

    pub fn run(self: *Vm) !void {
        // Rewind/redo re-entry: a `vm.rewind()` or `vm.redo()` call from
        // inside the previous `runner.onChoices` invocation has restored a
        // snapshot. Re-fire `onChoices` for the restored fork instead of
        // advancing instructions, then return to the outer dispatch loop.
        if (self.rewind_pending) {
            self.rewind_pending = false;
            self.runner.onChoices(self, self.current_choices);
            return;
        }
        if (!self.can_continue or self.is_waiting) return;
        while (self.ip < self.currentFrame().instructions().len) : (self.ip = self.currentFrame().ip) {
            const instruction = self.takeByte();
            if (instruction == 170) {
                self.end();
                return;
            }
            const op: OpCode = @enumFromInt(instruction);
            const is_in_jump = self.jump_requests.items.len > 0;

            switch (op) {
                .constant => {
                    const index = self.takeInt(C.CONSTANT);
                    if (index >= self.bytecode.constants.len)
                        return self.fail("Constant index {d} outside of bounds length {d}", .{ index, self.bytecode.constants.len });
                    const value = self.bytecode.constants[index];
                    if (value == .obj and value.obj.data == .function and index < self.bytecode.constants.len - 1) {
                        const ext = self.bytecode.constants[index + 1];
                        if (ext == .obj and ext.obj.data == .@"extern" and ext.obj.data.@"extern".context_ptr != null) {
                            try self.push(ext);
                            continue;
                        }
                    }
                    try self.push(value);
                },
                .pop => _ = try self.pop(),
                .dup => {
                    const value = self.stack.items[self.stack.items.len - 1];
                    try self.push(value);
                },
                .add => {
                    var right = try self.pop();
                    var left = try self.pop();
                    if (right == .visit) {
                        const value: f32 = @floatFromInt(right.visit);
                        right = .{ .number = value };
                    }
                    if (left == .visit) {
                        const value: f32 = @floatFromInt(left.visit);
                        left = .{ .number = value };
                    }
                    if (left == .obj and left.obj.data == .string) {
                        const str = left.obj.data.string;
                        left = .{ .const_string = str };
                    }
                    if (right == .obj and right.obj.data == .string) {
                        const str = right.obj.data.string;
                        right = .{ .const_string = str };
                    }

                    if (@intFromEnum(right) != @intFromEnum(left)) {
                        return self.fail("Cannot add types {s} and {s}: {f} {f}", .{ left.typeName(), right.typeName(), left, right });
                    }
                    switch (right) {
                        .const_string => |s| try self.pushAlloc(.{ .string = try std.mem.concat(self.alloc, u8, &.{ std.mem.trimEnd(u8, left.const_string, &[_]u8{0}), s }) }),
                        .number => try self.push(.{ .number = right.number + left.number }),
                        .obj => |o| {
                            switch (o.data) {
                                else => return self.fail("Cannot add types {s} and {s}: {f} {f}", .{ left.typeName(), right.typeName(), left, right }),
                            }
                        },
                        else => return self.fail("Cannot add types {s} and {s}: {f} {f}", .{ left.typeName(), right.typeName(), left, right }),
                    }
                },
                .subtract, .multiply, .divide, .modulus => try self.binaryNumberOp(op),
                .equal, .not_equal, .greater_than, .greater_than_equal => try self.comparisonOp(op),
                .true => try self.push(True),
                .false => try self.push(False),
                .nil => try self.push(Nil),
                .negate => try self.push(.{ .number = -(try self.pop()).number }),
                .not => {
                    const value = try self.pop();
                    switch (value) {
                        .bool => |b| try self.push(if (b) False else True),
                        .nil => try self.push(True),
                        .number => |n| try self.push(if (@abs(n) < 0.00001) False else True),
                        else => try self.push(False),
                    }
                },
                .@"or" => {
                    const right = try self.pop();
                    const left = try self.pop();
                    if (right != .bool or left != .bool) {
                        return self.fail("Conditionals must be of type bool not types {s} and {s}", .{ left.typeName(), right.typeName() });
                    }
                    try self.push(.{ .bool = right.bool or left.bool });
                },
                .@"and" => {
                    const right = try self.pop();
                    const left = try self.pop();
                    if (right != .bool or left != .bool) {
                        return self.fail("Conditionals must be of type bool not types {s} and {s}", .{ left.typeName(), right.typeName() });
                    }
                    try self.push(.{ .bool = right.bool and left.bool });
                },
                .jump => {
                    const dest = self.takeInt(C.JUMP);
                    if (self.jump_requests.pop()) |req| {
                        self.currentFrame().ip = req;
                        continue;
                    }
                    if (dest > self.currentFrame().instructions().len) break;
                    self.currentFrame().ip = dest;
                },
                .jump_if_false => {
                    const dest = self.takeInt(C.JUMP);
                    var condition = try self.pop();
                    const truthy = condition.isTruthy() catch return self.fail(
                        "Cannot evaluate condition of type '{s}'",
                        .{condition.typeName()},
                    );
                    if (!truthy) {
                        self.currentFrame().ip = dest;
                    }
                },
                .prong => {
                    const dest = self.takeInt(C.JUMP);
                    const values_count = self.takeInt(u8);
                    const index = self.stack.count - values_count - 1;
                    const capture = self.stack.items[index];
                    var i: usize = 0;
                    var match = false;
                    while (i < values_count) : (i += 1) {
                        const value = try self.pop();
                        if (!self.prongIsMatch(capture, value)) continue;
                        match = true;
                        self.stack.count -= values_count - i - 1;
                        break;
                    }

                    if (match or values_count == 0) {
                        self.currentFrame().ip = dest;
                    }
                },
                .decl_global => {
                    const index = self.takeInt(C.GLOBAL);
                    if (index > globals_size) return self.fail("Globals index {} is out of bounds of max size", .{index});
                    const value = try self.pop();
                    // global already set from loaded state
                    if (self.globals[index] != .void) {
                        if (self.globals[index] == .obj) try self.notifiable_objects.put(self.alloc, self.globals[index].obj.id, @intCast(index));
                        continue;
                    }
                    if (value == .obj) try self.notifiable_objects.put(self.alloc, value.obj.id, @intCast(index));
                    self.globals[index] = value;
                },
                .set_global => {
                    const index = self.takeInt(C.GLOBAL);
                    if (index > globals_size) return self.fail("Globals index {} is out of bounds of max size", .{index});

                    if (index >= self.globals.len) return self.fail("Globals index {} is out of bounds of current size {}", .{ index, self.globals.len });
                    var value = try self.pop();
                    const current = self.globals[index];
                    if (current == .enum_value and value == .enum_value and current.enum_value.base == value.enum_value.base and current.enum_value.base.data.@"enum".is_seq) {
                        if (current.enum_value.index > value.enum_value.index) value = current;
                    }
                    const old_value = self.globals[index];
                    self.globals[index] = value;
                    self.notifyValueChange(index, old_value, value);
                },
                .get_global => {
                    const index = self.takeInt(C.GLOBAL);
                    const value = self.globals[index];
                    try self.push(value);
                },
                .set_local => {
                    const index = self.takeInt(C.LOCAL);
                    const frame = self.currentFrame();
                    var value = try self.pop();
                    const slot = frame.bp + index;
                    if (slot >= self.stack.count)
                        return self.fail("Internal error: local variable index {d} out of range (stack size {d}, base {d})", .{ index, self.stack.count, frame.bp });
                    const current = self.stack.items[slot];
                    if (current == .enum_value and value == .enum_value and current.enum_value.base == value.enum_value.base and current.enum_value.base.data.@"enum".is_seq) {
                        if (current.enum_value.index > value.enum_value.index) value = current;
                    }
                    self.stack.items[slot] = value;
                },
                .get_local => {
                    const index = self.takeInt(C.LOCAL);
                    const frame = self.currentFrame();
                    const slot = frame.bp + index;
                    if (slot >= self.stack.count)
                        return self.fail("Internal error: local variable index {d} out of range (stack size {d}, base {d})", .{ index, self.stack.count, frame.bp });
                    const value = self.stack.items[slot];
                    try self.push(value);
                },
                .set_property => {
                    const field_value = try self.pop();
                    const instance_value = try self.pop();
                    const new_value = try self.pop();
                    if (instance_value != .obj) return self.fail("Property may only be set on object types, found '{s}'", .{instance_value.typeName()});

                    switch (instance_value.obj.data) {
                        .list => |l| {
                            const n: usize = @intFromFloat(field_value.number);
                            if (n >= l.items.len) return self.fail("Index {d} out of bounds for list of length {d}", .{ n, l.items.len });
                            l.items[n] = new_value;
                        },
                        .map => |*m| {
                            try m.*.put(self.alloc, field_value, new_value);
                        },
                        .instance => |inst| {
                            const field_name = field_value.asString() orelse return self.fail("Instance field name must be of type string but found '{s}'", .{field_value.typeName()});
                            if (inst.base.data.class.getFieldIndex(field_name)) |idx| {
                                inst.fields[idx] = new_value;
                            } else {
                                return self.fail("Instance of '{s}' does not contain field '{s}'", .{ inst.base.data.class.name, field_name });
                            }
                        },
                        // todo add string indexing
                        else => return self.fail("Cannot index '{s}' into type {s}", .{ field_value.typeName(), instance_value.typeName() }),
                    }
                },
                .get_upvalue => {
                    const frames_to_skip = self.takeInt(u8);
                    const index = self.takeInt(C.LOCAL);
                    const frame_count = self.frames.items.len;

                    if (frame_count < frames_to_skip) {
                        return self.fail("Cannot read captured variable: its frame has already returned", .{});
                    }

                    const target_frame = self.frames.items[frame_count - 1 - frames_to_skip];
                    const value = self.stack.items[target_frame.bp + index];
                    self.stack.push(value);
                },
                .set_upvalue => {
                    const frames_to_skip = self.takeInt(u8);
                    const index = self.takeInt(C.LOCAL);
                    const frame_count = self.frames.items.len;

                    if (frame_count < frames_to_skip) {
                        return self.fail("Cannot write captured variable: its frame has already returned", .{});
                    }

                    const target_frame = self.frames.items[frame_count - frames_to_skip];
                    self.stack.items[target_frame.bp + index] = try self.pop();
                },
                .string, .loc => {
                    const index = self.takeInt(C.CONSTANT);
                    const string_obj = self.bytecode.constants[index].obj;
                    var str: []const u8 = string_obj.data.string;

                    if (op == .loc) {
                        if (!UUID.isEmpty(string_obj.id)) {
                            if (self.loc_provider) |lp| {
                                str = lp.map.get(string_obj.id) orelse str;
                            }
                        }
                    }

                    var count = self.takeInt(u8);
                    if (count == 0) {
                        try self.push(.{ .const_string = str });
                        continue;
                    }

                    var args = try self.alloc.alloc(Value, count);
                    defer self.alloc.free(args);
                    while (count > 0) : (count -= 1) {
                        args[count - 1] = try self.pop();
                    }
                    var allocating_writer: std.Io.Writer.Allocating = .init(self.alloc);
                    defer allocating_writer.deinit();
                    const writer = &allocating_writer.writer;
                    // index
                    var i: usize = 0;
                    // start
                    var s: usize = 0;
                    // need to implement our own formatter due to runtime values
                    while (i < str.len) : (i += 1) {
                        var c = str[i];
                        if (c == '{') {
                            try writer.writeAll(str[s..i]);
                            const open = i + 1;
                            var close = open;
                            while (c != '}') : (i += 1) {
                                c = str[i];
                                close = i;
                            }
                            const arg_index = try std.fmt.parseInt(u8, str[open..close], 10);
                            const val = args[arg_index];
                            switch (val) {
                                .number => |n| {
                                    try writer.print("{d}", .{n});
                                },
                                .timestamp => |t| {
                                    try writer.print("{d}", .{t});
                                },
                                .bool => |b| try writer.writeAll(if (b) "true" else "false"),
                                .enum_value => |e| {
                                    if (e.index >= e.base.data.@"enum".values.len)
                                        return self.fail("Enum index {d} is out of bounds for '{s}'", .{ e.index, e.base.data.@"enum".name });
                                    try writer.writeAll(e.base.data.@"enum".values[e.index]);
                                },
                                .const_string => |cs| try writer.writeAll(std.mem.trimEnd(u8, cs, &[_]u8{0})),
                                .obj => |o| {
                                    switch (o.data) {
                                        // remove final 0
                                        .string => try writer.writeAll(std.mem.trimEnd(u8, o.data.string, &[_]u8{0})),
                                        else => return self.fail("Unsupported interpolated type '{s}' for '{s}'", .{ val.typeName(), str }),
                                    }
                                },
                                .visit => |v| try writer.print("{}", .{v}),
                                else => return self.fail("Unsupported interpolated type '{s}' for '{s}'", .{ val.typeName(), str }),
                            }
                            s = i;
                        }
                    }
                    try writer.writeAll(str[s..]);
                    try self.pushAlloc(.{ .string = try allocating_writer.toOwnedSlice() });
                },
                .list => {
                    var count = self.takeInt(C.COLLECTION);
                    var list = try std.ArrayList(Value).initCapacity(self.alloc, count);
                    errdefer list.deinit(self.alloc);
                    while (count > 0) : (count -= 1) {
                        list.appendAssumeCapacity(try self.pop());
                    }
                    std.mem.reverse(Value, list.items);
                    try self.pushAlloc(.{ .list = list });
                },
                .map => {
                    var count = self.takeInt(C.COLLECTION);
                    var map = Value.Obj.MapType.empty;
                    errdefer map.deinit(self.alloc);
                    try map.ensureTotalCapacity(self.alloc, count);
                    while (count > 0) : (count -= 1) {
                        const value = try self.pop();
                        const key = try self.pop();
                        try map.put(self.alloc, key, value);
                    }
                    map.sort(Value.adapter);
                    try self.pushAlloc(.{ .map = map });
                },
                .set => {
                    var count = self.takeInt(C.COLLECTION);
                    var set = Value.Obj.SetType.empty;
                    errdefer set.deinit(self.alloc);
                    try set.ensureTotalCapacity(self.alloc, count);
                    while (count > 0) : (count -= 1) {
                        try set.put(self.alloc, try self.pop(), {});
                    }
                    set.sort(Value.adapter);
                    try self.pushAlloc(.{ .set = set });
                },
                .iter_start => {
                    var value = try self.pop();
                    if (self.iterators.count >= iterator_size) return self.fail("Iterator overflow: too many nested iterators (max {d})", .{iterator_size});
                    self.iterators.push(.{
                        .value = value,
                        .index = 0,
                    });
                    if (value.len() > 0) try self.push(value.getAtIndex(0));
                },
                .iter_next => {
                    const iter = self.iterators.peek().*;
                    var value = iter.value;
                    if (iter.index >= value.len()) {
                        try self.push(Nil);
                        try self.push(False);
                    } else {
                        try self.push(value.getAtIndex(iter.index));
                        try self.push(True);
                    }
                    self.iterators.peek().index += 1;
                },
                .iter_end => {
                    _ = self.iterators.pop();
                },
                .instance => {
                    const value = try self.pop();
                    if (value != .obj and value.obj.data != .class)
                        return self.fail("Instance expected type class, but found '{s}'", .{value.typeName()});

                    const count = self.takeInt(C.FIELDS);
                    // Field pairs remain on stack as [field_0, name_0, field_1, name_1, ...]
                    // so they stay GC-rooted while createInstance clones class defaults
                    // and calls gc.create. We resize the stack to drop them after.
                    const pairs_base = self.stack.count - (count * 2);
                    const fields = try self.alloc.alloc(Class.Member, count);
                    defer self.alloc.free(fields);
                    var i: usize = 0;
                    while (i < count) : (i += 1) {
                        const field = self.stack.backing[pairs_base + (i * 2)];
                        const name = self.stack.backing[pairs_base + (i * 2) + 1];
                        fields[i] = .{
                            .name = name.asString().?,
                            .value = field,
                        };
                    }
                    const instance_val = try self.createInstance(value.obj, fields);
                    self.stack.resize(pairs_base);
                    try self.push(instance_val);
                },
                .range => {
                    const left = try self.pop();
                    const right = try self.pop();

                    if (left != .number or right != .number)
                        return self.fail("Range must be two number, found '{s}' and '{s}'", .{ left.typeName(), right.typeName() });
                    try self.push(.{
                        .range = .{
                            .start = @as(i32, @intFromFloat(left.number)),
                            .end = @as(i32, @intFromFloat(right.number)),
                        },
                    });
                },
                .index => {
                    const index = try self.pop();
                    const raw_target = try self.pop();

                    // Normalize const_string to obj.string so method dispatch
                    // only needs to handle one string representation
                    const target = if (raw_target == .const_string)
                        try self.gc.create(self, .{ .string = try self.alloc.dupe(u8, std.mem.trimEnd(u8, raw_target.const_string, &[_]u8{0})) })
                    else
                        raw_target;

                    switch (target) {
                        .obj => |o| switch (o.data) {
                            .string => {
                                if (index.asString()) |name| {
                                    if (builtins.string_methods.get(name)) |method| {
                                        try self.push(try self.builtinValue(method));
                                        try self.push(target);
                                    } else return self.failUnknownMethod("string", name, builtins.string_methods.keys());
                                }
                            },
                            .list => |l| {
                                if (index.asString()) |name| {
                                    if (builtins.collection_methods.get(name)) |method| {
                                        try self.push(try self.builtinValue(method));
                                        try self.push(target);
                                    } else return self.failUnknownMethod("list", name, builtins.collection_methods.keys());
                                } else if (index == .number) {
                                    const i = @as(u32, @intFromFloat(index.number));
                                    if (i < 0 or i >= l.items.len) {
                                        try self.push(Nil);
                                    } else try self.push(l.items[i]);
                                } else try self.push(Nil);
                            },
                            .map => |m| {
                                if (m.get(index)) |v| {
                                    try self.push(v);
                                } else if (index.asString()) |name| {
                                    if (std.mem.eql(u8, name, "add")) {
                                        try self.push(try self.builtinValue(builtins.collection_methods.get("__addmap").?));
                                        try self.push(target);
                                    } else if (builtins.collection_methods.get(name)) |method| {
                                        try self.push(try self.builtinValue(method));
                                        try self.push(target);
                                    } else return self.failUnknownMethod("map", name, builtins.collection_methods.keys());
                                } else try self.push(Nil);
                            },
                            .set => {
                                const name = index.asString() orelse return self.fail("Can only query set methods by string name, found '{s}'", .{index.typeName()});
                                const method = builtins.collection_methods.get(name) orelse return self.failUnknownMethod("set", name, builtins.collection_methods.keys());
                                try self.push(try self.builtinValue(method));
                                try self.push(target);
                            },
                            .instance => |i| {
                                const name = index.asString() orelse return self.fail("Can only query instance fields by string name, found '{s}'", .{index.typeName()});
                                const value = try i.getProperty(name) orelse {
                                    const hint = try self.suggestClassMember(i.base.data.class, name);
                                    return self.failWithHelp("Unknown field '{s}' on instance of '{s}'", .{ name, i.base.data.class.name }, hint);
                                };
                                try self.push(value);
                                if (value == .obj and value.obj.data == .function) {
                                    // push instance self
                                    try self.push(target);
                                }
                            },
                            .@"enum" => |e| {
                                const name = index.asString() orelse return self.fail("Can only query enum values by string name, found '{s}'", .{index.typeName()});
                                for (e.values, 0..) |value, i| {
                                    if (std.mem.eql(u8, value, name)) {
                                        try self.push(.{ .enum_value = .{ .index = @intCast(i), .base = target.obj } });
                                        break;
                                    }
                                } else {
                                    const hint = blk: {
                                        const match = backend.suggest.closest(self.alloc, name, e.values) catch null;
                                        if (match) |m| {
                                            defer self.alloc.free(m);
                                            break :blk std.fmt.allocPrint(self.alloc, "did you mean '{s}'?", .{m}) catch null;
                                        }
                                        break :blk null;
                                    };
                                    return self.failWithHelp("Unknown value '{s}' on enum '{s}'", .{ name, e.name }, hint);
                                }
                            },
                            .class => |c| {
                                const name = index.asString() orelse return self.fail("Can only query class fields by string name, found '{s}'", .{index.typeName()});
                                for (c.fields) |field| {
                                    if (std.mem.eql(u8, field.name, name)) {
                                        try self.push(field.value);
                                        break;
                                    }
                                } else {
                                    for (c.methods) |method| {
                                        if (std.mem.eql(u8, method.name, name)) {
                                            try self.push(method.value);
                                            if (method.value == .obj and method.value.obj.data == .function) {
                                                try self.push(Nil); // add nil for "self" in static method
                                            }
                                            break;
                                        }
                                    } else {
                                    const hint = try self.suggestClassMember(c, name);
                                    return self.failWithHelp("Unknown value '{s}' on Class '{s}'", .{ name, c.name }, hint);
                                }
                                }
                            },
                            else => return self.fail("Unknown target type '{s}' to index. Only lists, maps, sets, or instances can be indexed.", .{index.typeName()}),
                        },
                        .map_pair => |mp| {
                            const name = index.asString() orelse return self.fail("Unknown index key '{s}' on map key/value pair. Only \"key\" or \"value\" are allowed.", .{index.typeName()});
                            if (std.mem.eql(u8, name, "key")) {
                                try self.push(mp.key.*);
                            } else if (std.mem.eql(u8, name, "value")) {
                                try self.push(mp.value.*);
                            } else return self.fail("Unknown index key '{s}' on map key/value pair. Only \"key\" or \"value\" are allowed.", .{index.typeName()});
                        },
                        else => {
                            return self.fail("Invalid index '{s}' on target type '{s}'", .{ index.typeName(), target.typeName() });
                        },
                    }
                },
                .dialogue => {
                    const has_speaker = self.takeInt(u8) == 1;
                    var speaker: ?[]const u8 = null;
                    if (has_speaker) {
                        const speaker_value = try self.pop();
                        const str = speaker_value.asString() orelse
                            return self.fail("Speaker id must be of type string, but found '{s}'", .{speaker_value.typeName()});
                        speaker = str;
                    }

                    const dialogue_value = try self.pop();
                    const dialogue_str = dialogue_value.asString() orelse return self.fail("Dialogue must be of type string, but found '{s}'", .{dialogue_value.typeName()});

                    const tag_count = self.takeInt(u8);
                    var tags = try self.alloc.alloc([]const u8, tag_count);
                    defer self.alloc.free(tags);
                    var i: usize = 0;
                    while (i < tag_count) : (i += 1) {
                        const tag_value = try self.pop();
                        const str = tag_value.asString() orelse return self.fail("Tag must be of type string, but found '{s}'", .{tag_value.typeName()});
                        tags[tag_count - i - 1] = str;
                    }
                    if (is_in_jump) continue;
                    self.is_waiting = true;
                    const line = Line{
                        .content = dialogue_str,
                        .speaker = speaker,
                        .tags = tags,
                    };
                    self.runner.onLine(self, &line);
                    return;
                },
                .call => {
                    const arg_count = self.takeInt(C.ARGS);
                    const value = self.stack.items[self.stack.count - 1 - arg_count];
                    if (value != .obj)
                        return self.fail("Cannot call non-function type {s}", .{value.typeName()});
                    switch (value.obj.data) {
                        .function => |f| {
                            if (f.arity != arg_count) {
                                return self.fail(
                                    "Function expected {} arguments, but found {}",
                                    .{ f.arity, arg_count },
                                );
                            }
                            const frame = try Frame.create(value.obj, self.stack.count - arg_count);
                            if (self.frames.count >= frame_size) return self.fail("Stack overflow: too many nested function calls (max {d})", .{frame_size});
                            self.frames.push(frame);
                            const locals_start = frame.bp + arg_count;
                            self.stack.resize(frame.bp + f.locals_count);
                            // Clear newly-reserved local slots (above the
                            // arguments) so neither GC root marking nor
                            // snapshot capture observes stale Values left
                            // over from previous frame usage of these slots.
                            @memset(self.stack.backing[locals_start..(frame.bp + f.locals_count)], Void);
                        },
                        .@"extern" => |e| {
                            if (e.context_ptr) |ptr| {
                                const result = e.backing(ptr, self.stack.items[self.stack.count - arg_count .. self.stack.count]);
                                self.stack.count -= arg_count + 1;
                                try self.push(result);
                            } else return self.fail("Extern function {s} not set", .{e.name});
                        },
                        .builtin => |b| {
                            if (b.arity != arg_count)
                                return self.fail(
                                    "Builtin Function expected {} arguments, but found {}",
                                    .{ b.arity, arg_count },
                                );
                            const result = b.backing(self, self.stack.items[self.stack.count - arg_count .. self.stack.count]);
                            if (self.break_on_assert and std.mem.eql(u8, b.name, "assert") and result != .void) {
                                return self.fail("Assertion Failed: {s}", .{result.asString() orelse unreachable});
                            }
                            if (std.mem.eql(u8, b.name, "weighted") and result == .const_string) {
                                const items_len = self.stack.items[self.stack.count - arg_count].obj.data.list.items.len;
                                const weights_len = self.stack.items[self.stack.count - arg_count + 1].obj.data.list.items.len;
                                return self.fail("'weighted' expects items and weights to have the same length, but got {d} items and {d} weights", .{ items_len, weights_len });
                            }
                            self.stack.count -= arg_count + 1;
                            try self.push(result);
                        },
                        else => {
                            return self.fail("Cannot call non-function type '{s}'", .{value.typeName()});
                        },
                    }
                },
                .return_value => {
                    const value = try self.pop();
                    const frame = self.frames.pop();
                    self.stack.count = frame.bp - 1;
                    try self.push(value);
                },
                .return_void => {
                    const frame = self.frames.pop();
                    self.stack.count = frame.bp - 1;
                    try self.push(Void);
                },
                .fork => {
                    self.current_choices = try self.choices_list.toOwnedSlice(self.alloc);
                    self.choices_freed = false;
                    self.choices_list.clearRetainingCapacity();
                    if (is_in_jump) {
                        const next_target = self.jump_requests.getLast();
                        // If the current IP is the target, pop it.
                        if (self.currentFrame().ip >= next_target) {
                            _ = self.jump_requests.pop();
                        } else continue;
                    }

                    self.is_waiting = true;
                    try self.captureFork();
                    self.runner.onChoices(self, self.current_choices);
                    return;
                },
                .choice => {
                    const ip = self.takeInt(C.JUMP); // The skip-jump
                    const is_unique = self.takeInt(u8) == 1;

                    const anchor_idx = self.takeInt(C.CONSTANT);
                    const anchor_val = self.bytecode.constants[anchor_idx];
                    const anchor = anchor_val.obj.data.anchor;
                    const visit_count = self.globals[anchor.visit_index].visit;

                    const content_value = try self.pop();
                    const content_str = content_value.asString() orelse return self.fail("Choice content must be of type string, but found '{s}'", .{content_value.typeName()});

                    const tag_count = self.takeInt(u8);
                    var tags = try self.alloc.alloc([]const u8, tag_count);
                    var i: usize = 0;
                    while (i < tag_count) : (i += 1) {
                        const tag_value = try self.pop();
                        const str = tag_value.asString() orelse
                            return self.fail("Tag must be of type string, but found '{s}'", .{tag_value.typeName()});
                        tags[tag_count - i - 1] = str;
                    }
                    if (visit_count > 0 and is_unique) continue;

                    try self.choices_list.append(self.alloc, .{
                        .content = content_str,
                        .tags = tags,
                        .visit_count = visit_count,
                        .ip = ip,
                    });
                },
                .visit => {
                    const anchor_idx = self.takeInt(C.CONSTANT);
                    const value = self.bytecode.constants[anchor_idx];
                    const anchor = value.obj.data.anchor;
                    self.globals[anchor.visit_index].visit += 1;
                    try self.anchor_stack.append(self.alloc, anchor_idx);
                },
                .divert => {
                    // If in a jump, go to the next jump instead
                    if (self.jump_requests.pop()) |req| {
                        self.currentFrame().ip = req;
                        continue;
                    }
                    const anchor_idx = self.takeInt(C.CONSTANT);
                    try self.prepareDivert(anchor_idx);

                    if (self.jump_requests.pop()) |dest| {
                        const len = self.currentFrame().instructions().len;
                        if (dest > len) return self.fail("Divert {d} is out of range {d}", .{ dest, len });
                        self.currentFrame().ip = dest;
                    }
                },
                .backup => {
                    const ip = self.takeInt(C.JUMP);
                    const is_fork_backup = self.takeInt(u8) == 1;
                    if (is_in_jump) continue;
                    try self.jump_backups.append(self.alloc, .{
                        .ip = ip,
                        .anchor_depth = self.anchor_stack.items.len,
                        .is_fork = is_fork_backup,
                    });
                },
                .end => {
                    if (self.anchor_stack.items.len > 0) {
                        _ = self.anchor_stack.pop();
                    }

                    if (self.jump_requests.pop()) |req| {
                        self.currentFrame().ip = req;
                        continue;
                    }
                    if (self.jump_backups.pop()) |backup| {
                        self.currentFrame().ip = backup.ip;
                        continue;
                    }
                    self.end();
                    break;
                },
                .fin => {
                    self.end();
                    break;
                }
            }
        }
        self.end();
    }

    fn performBin(comptime T: type, op: OpCode, left: T, right: T) !T {
        return switch (op) {
            .subtract => left - right,
            .multiply => left * right,
            .divide => if (@typeInfo(T) == .int) @divExact(left, right) else left / right,
            .modulus => @mod(left, right),
            else => return error.InvalidOperation,
        };
    }

    fn prepareDivert(self: *Vm, anchor_idx: C.CONSTANT) !void {
        var current_idx: ?C.CONSTANT = anchor_idx;
        var is_target = true;
        while (current_idx) |idx| {
            // If this is an ancestor (not the target itself) and it's already active,
            // we stop climbing because the environment for the target is already set up.
            // Trim any backups from scopes deeper than this ancestor.
            if (!is_target) {
                for (self.anchor_stack.items, 0..) |active_idx, i| {
                    if (active_idx == idx) {
                        self.trimForkBackups(i);
                        return;
                    }
                }
            }

            const anchor = self.bytecode.constants[idx].obj.data.anchor;
            try self.jump_requests.append(self.alloc, anchor.ip);

            // If the target itself is already in the stack, we still want to
            // jump to its IP, but we should treat it as "leaving" and "re-entering".
            // We pop it from the anchor_stack so the visit logic treats it as a fresh arrival.
            // Also trim any fork^ backups from scopes being exited (but keep
            // =>^ backups, which are explicit return points that must survive).
            if (is_target) {
                for (self.anchor_stack.items, 0..) |active_idx, i| {
                    if (active_idx == idx) {
                        self.anchor_stack.shrinkRetainingCapacity(i);
                        self.trimForkBackups(i);
                        break;
                    }
                }
            }

            current_idx = anchor.parent_anchor_index;
            is_target = false;
        }
    }

    /// Remove fork^ backups that belong to scopes deeper than `depth`.
    /// Divert backups (=>^) are kept — they are explicit return points.
    fn trimForkBackups(self: *Vm, depth: usize) void {
        while (self.jump_backups.items.len > 0) {
            const last = self.jump_backups.getLast();
            if (last.is_fork and last.anchor_depth > depth) {
                _ = self.jump_backups.pop();
            } else break;
        }
    }

    fn binaryNumberOp(self: *Vm, op: OpCode) !void {
        var right = try self.pop();
        var left = try self.pop();
        if (right == .timestamp and left == .timestamp) {
            try self.push(.{ .timestamp = performBin(i64, op, left.timestamp, right.timestamp) catch
                return self.fail("Unknown binary operator '{s}'", .{@tagName(op)}) });
            return;
        }
        if (right == .visit) {
            const value: f32 = @floatFromInt(right.visit);
            right = .{ .number = value };
        }
        if (left == .visit) {
            const value: f32 = @floatFromInt(left.visit);
            left = .{ .number = value };
        }
        if (right != .number or left != .number)
            return self.fail("Cannot perform binary operation on types of '{s}' and '{s}'", .{ left.typeName(), right.typeName() });
        const right_num = right.number;
        const left_num = left.number;
        try self.push(.{ .number = performBin(f32, op, left_num, right_num) catch
            return self.fail("Unknown binary operator '{s}'", .{@tagName(op)}) });
    }

    fn comparisonOp(self: *Vm, op: OpCode) !void {
        var right = try self.pop();
        var left = try self.pop();
        if (right == .visit) {
            const value: f32 = @floatFromInt(right.visit);
            right = .{ .number = value };
        }
        if (left == .visit) {
            const value: f32 = @floatFromInt(left.visit);
            left = .{ .number = value };
        }

        if (@intFromEnum(right) != @intFromEnum(left)) {
            return self.fail("Cannot compare mismatched types '{s}' and '{s}': {f} {f}", .{ left.typeName(), right.typeName(), left, right });
        }
        if (right == .enum_value) {
            const value: f32 = @floatFromInt(right.enum_value.index);
            right = .{ .number = value };
        }
        if (left == .enum_value) {
            const value: f32 = @floatFromInt(left.enum_value.index);
            left = .{ .number = value };
        }
        switch (op) {
            .equal => try self.push(.{ .bool = right.eql(left) }),
            .not_equal => try self.push(.{ .bool = !right.eql(left) }),
            .greater_than => try self.push(.{ .bool = left.number > right.number }),
            .greater_than_equal => try self.push(.{ .bool = left.number >= right.number }),
            else => return self.fail("Unknown comparison operator '{s}'", .{@tagName(op)}),
        }
    }

    fn prongIsMatch(self: *Vm, capture: Value, case: Value) bool {
        _ = self;
        const result = switch (case) {
            .range => |r| @as(i32, @intFromFloat(capture.number)) >= r.start and @as(i32, @intFromFloat(capture.number)) <= r.end,
            else => capture.eql(case),
        };
        return result;
    }

    /// Child clones are push-protected onto `self.stack` during construction
    /// of each container so that a GC trigger in a later iteration cannot
    /// collect clones sitting in a local (non-rooted) `ArrayList`/`HashMap`.
    /// The protective pushes are popped before returning, so callers see no
    /// stack side-effects.
    fn clone(self: *Vm, value: Value) anyerror!Value {
        if (value != .obj) return value;

        const allocator = self.alloc;
        const obj = value.obj;
        switch (obj.data) {
            .list => |l| {
                const base = self.stack.count;
                errdefer self.stack.resize(base);
                var new_list = try std.ArrayList(Value).initCapacity(allocator, l.items.len);
                errdefer new_list.deinit(allocator);
                for (l.items) |item| {
                    const child = try self.clone(item);
                    try new_list.append(allocator, child);
                    if (child == .obj) self.stack.push(child);
                }
                const result = try self.gc.create(self, .{ .list = new_list });
                self.stack.resize(base);
                return result;
            },
            .map => |m| {
                const base = self.stack.count;
                errdefer self.stack.resize(base);
                var new_map = Value.Obj.MapType.empty;
                errdefer new_map.deinit(allocator);
                var it = m.iterator();
                while (it.next()) |entry| {
                    const k = try self.clone(entry.key_ptr.*);
                    if (k == .obj) self.stack.push(k);
                    const v = try self.clone(entry.value_ptr.*);
                    if (v == .obj) self.stack.push(v);
                    try new_map.put(allocator, k, v);
                }
                const result = try self.gc.create(self, .{ .map = new_map });
                self.stack.resize(base);
                return result;
            },
            .set => |s| {
                const base = self.stack.count;
                errdefer self.stack.resize(base);
                var new_set = Value.Obj.SetType.empty;
                errdefer new_set.deinit(allocator);
                var it = s.iterator();
                while (it.next()) |entry| {
                    const k = try self.clone(entry.key_ptr.*);
                    if (k == .obj) self.stack.push(k);
                    try new_set.put(allocator, k, {});
                }
                const result = try self.gc.create(self, .{ .set = new_set });
                self.stack.resize(base);
                return result;
            },
            .instance => |inst| {
                const base = self.stack.count;
                errdefer self.stack.resize(base);
                var values = try self.alloc.alloc(Value, inst.fields.len);
                errdefer self.alloc.free(values);
                for (inst.fields, 0..) |inst_value, i| {
                    const child = try self.clone(inst_value);
                    values[i] = child;
                    if (child == .obj) self.stack.push(child);
                }
                const result = try self.gc.create(self, .{ .instance = .{
                    .base = inst.base,
                    .fields = values,
                } });
                self.stack.resize(base);
                return result;
            },
            else => return value,
        }
    }

    /// Cloned default field values are push-protected on `self.stack` while
    /// the instance is being built, so a GC trigger in a subsequent clone
    /// or in the final `gc.create` cannot collect earlier clones. Caller
    /// is responsible for keeping `inst_fields` values GC-rooted.
    fn createInstance(self: *Vm, base: *Value.Obj, inst_fields: []Class.Member) !Value {
        const class = base.data.class;
        const stack_base = self.stack.count;
        var values = try self.alloc.alloc(Value, class.fields.len);
        for (class.fields, 0..) |f, i| {
            const cloned = try self.clone(f.value);
            values[i] = cloned;
            if (cloned == .obj) self.stack.push(cloned);
        }
        for (inst_fields) |inst_field| {
            if (class.getFieldIndex(inst_field.name)) |i| values[i] = inst_field.value;
        }
        const result = try self.gc.create(self, .{ .instance = .{
            .base = base,
            .fields = values,
        } });
        self.stack.resize(stack_base);
        return result;
    }

    fn push(self: *Vm, value: Value) !void {
        if (self.stack.items.len >= stack_size) return self.fail("Stack is full", .{});
        self.stack.push(value);
    }

    fn pushAlloc(self: *Vm, data: Value.Obj.Data) !void {
        const value = try self.gc.create(self, data);
        self.stack.push(value);
    }

    fn pop(self: *Vm) !Value {
        if (self.stack.count == 0) {
            self.fail("Trying to pop an empty stack", .{}) catch |err| return err;
            return Nil;
        }
        return self.stack.pop();
    }

    pub fn print(self: *Vm, writer: *std.Io.Writer) void {
        writer.print("==STACK==\n", .{});
        for (self.stack.items) |*item| {
            writer.print("[", .{});
            item.print(writer);
            writer.print("]\n", .{});
        }
        writer.print("\n", .{});
    }
};
