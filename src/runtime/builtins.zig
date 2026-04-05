const std = @import("std");

const types = @import("../types/index.zig");
const Value = types.Value;
const Void = types.Void;
const True = types.True;
const False = types.False;

const utils = @import("../utils/index.zig");
const UUID = utils.UUID;

const Vm = @import("vm.zig").Vm;

pub const Builtin = struct {
    arity: u8,
    backing: Fn,
    is_method: bool,
    name: []const u8,

    pub const Fn = *const fn (vm: *Vm, args: []Value) Value;
};

pub const VariationState = union(enum) {
    cycle: u32,
    sequence: u32,
    shuffle: struct {
        index: u32,
        order: std.ArrayList(u32),
    },
};

pub const VariationMap = std.AutoHashMapUnmanaged(u32, VariationState);

var r: ?std.Random.DefaultPrng = null;

pub const functions = std.StaticStringMap(Value).initComptime(.{ .{
    "rnd",
    create("rnd", 2, false, rnd),
}, .{
    "rnd01",
    create("rnd01", 0, false, rnd01),
}, .{
    "print",
    create("print", 1, false, print),
}, .{
    "round",
    create("round", 1, false, round),
}, .{
    "abs",
    create("abs", 1, false, abs),
}, .{
    "assert",
    create("assert", 2, false, assert),
}, .{
    "mstime",
    create("mstime", 0, false, mstime),
}, .{
    "weighted",
    create("weighted", 2, false, weighted),
}, .{
    "cycle",
    create("cycle", 1, false, cycle),
}, .{
    "shuffle",
    create("shuffle", 1, false, shuffle),
}, .{
    "sequence",
    create("sequence", 1, false, sequence),
}, .{
    "random",
    create("random", 1, false, random),
} });

pub const methods = std.StaticStringMap(Value).initComptime(.{ .{
    "count",
    create("count", 1, true, count_method),
}, .{
    "add",
    create("add", 2, true, add_method),
}, .{
    "__addmap",
    create("add", 3, true, addmap_method),
}, .{
    "remove",
    create("remove", 2, true, remove_method),
}, .{
    "has",
    create("has", 2, true, has_method),
}, .{
    "clear",
    create("clear", 1, true, clear_method),
}, .{
    "upper",
    create("upper", 1, true, upper_method),
}, .{
    "lower",
    create("lower", 1, true, lower_method),
}, .{
    "replace",
    create("replace", 3, true, replace_method),
}, .{
    "split",
    create("split", 2, true, split_method),
}, .{
    "substr",
    create("substr", 3, true, substr_method),
}, .{
    "trim",
    create("trim", 1, true, trim_method),
} });

pub const string_methods = std.StaticStringMap(Value).initComptime(.{ .{
    "has",
    create("has", 2, true, has_method),
}, .{
    "length",
    create("length", 1, true, length_method),
}, .{
    "upper",
    create("upper", 1, true, upper_method),
}, .{
    "lower",
    create("lower", 1, true, lower_method),
}, .{
    "replace",
    create("replace", 3, true, replace_method),
}, .{
    "split",
    create("split", 2, true, split_method),
}, .{
    "substr",
    create("substr", 3, true, substr_method),
}, .{
    "trim",
    create("trim", 1, true, trim_method),
} });

fn create(name: []const u8, arity: u8, is_method: bool, backing: *const fn (vm: *Vm, args: []Value) Value) Value {
    const obj = Value.Obj{
        .data = .{
            .builtin = .{
                .backing = backing,
                .arity = arity,
                .is_method = is_method,
                .name = name,
            },
        },
    };
    return .{ .obj = @constCast(&obj) };
}

fn rnd(_: *Vm, args: []Value) Value {
    if (r == null) r = std.Random.DefaultPrng.init(std.crypto.random.int(u64));
    const start = @as(i32, @intFromFloat(args[0].number));
    const end = @as(i32, @intFromFloat(args[1].number));
    return .{ .number = @as(f32, @floatFromInt(r.?.random().intRangeAtMost(i32, start, end))) };
}

fn rnd01(_: *Vm, args: []Value) Value {
    if (r == null) r = std.Random.DefaultPrng.init(std.crypto.random.int(u64));
    _ = args;
    return .{ .number = r.?.random().float(f32) };
}

fn round(_: *Vm, args: []Value) Value {
    return .{ .number = @floatFromInt(@as(i64, @intFromFloat(args[0].number))) };
}

fn abs(_: *Vm, args: []Value) Value {
    return .{ .number = @abs(args[0].number) };
}

fn print(vm: *Vm, args: []Value) Value {
    var stderr_buffer: [128]u8 = undefined;
    var stderr_writer = std.fs.File.stderr().writer(&stderr_buffer);
    const stderr = &stderr_writer.interface;
    var arr = std.AutoArrayHashMap(UUID.ID, void).init(vm.alloc);
    defer arr.deinit();
    args[0].print(stderr, &arr) catch unreachable;
    stderr.print("\n", .{}) catch unreachable;
    stderr.flush() catch unreachable;
    return Void;
}

fn mstime(_: *Vm, _: []Value) Value {
    return .{ .timestamp = std.time.milliTimestamp() };
}

fn assert(_: *Vm, args: []Value) Value {
    const expr = args[0];
    const msg = args[1];
    if (!expr.eql(True)) return msg;
    return Void;
}

fn count_method(_: *Vm, args: []Value) Value {
    const data = args[0].obj.data;
    const count = switch (data) {
        .list => |l| l.items.len,
        .map => |m| m.count(),
        .set => |s| s.count(),
        else => 0,
    };
    return .{ .number = @as(f32, @floatFromInt(count)) };
}

fn add_method(vm: *Vm, args: []Value) Value {
    const item = args[1];
    switch (args[0].obj.data) {
        .list => args[0].obj.data.list.append(vm.alloc, item) catch {},
        .set => args[0].obj.data.set.put(vm.alloc, item, {}) catch {},
        else => unreachable,
    }
    vm.notifyValueObjChange(args[0].obj.id, args[0]);
    return Void;
}

fn addmap_method(vm: *Vm, args: []Value) Value {
    const key = args[1];
    const item = args[2];
    switch (args[0].obj.data) {
        .map => args[0].obj.data.map.put(vm.alloc, key, item) catch {},
        else => unreachable,
    }
    vm.notifyValueObjChange(args[0].obj.id, args[0]);
    return Void;
}
fn remove_method(vm: *Vm, args: []Value) Value {
    const item = args[1];
    switch (args[0].obj.data) {
        .list => {
            for (args[0].obj.data.list.items, 0..) |it, i| {
                if (!Value.eql(it, item)) continue;
                _ = args[0].obj.data.list.orderedRemove(i);
                break;
            }
        },
        .set => _ = args[0].obj.data.set.orderedRemove(item),
        .map => _ = args[0].obj.data.map.orderedRemove(item),
        else => unreachable,
    }
    vm.notifyValueObjChange(args[0].obj.id, args[0]);
    return Void;
}

fn has_method(_: *Vm, args: []Value) Value {
    const item = args[1];
    const result = switch (args[0].obj.data) {
        .list => blk: {
            for (args[0].obj.data.list.items) |it| {
                if (!Value.eql(it, item)) continue;
                break :blk true;
            }
            break :blk false;
        },
        .set => args[0].obj.data.set.contains(item),
        .map => args[0].obj.data.map.contains(item),
        .string => |s| blk: {
            const needle = getStr(item);
            if (needle.len == 0) break :blk false;
            break :blk std.mem.indexOf(u8, s, needle) != null;
        },
        else => false,
    };
    if (result) return True;
    return False;
}
fn clear_method(vm: *Vm, args: []Value) Value {
    var data = args[0].obj.data;
    switch (data) {
        .list => data.list.clearAndFree(vm.alloc),
        .map => data.map.clearAndFree(vm.alloc),
        .set => data.set.clearAndFree(vm.alloc),
        else => unreachable,
    }
    vm.notifyValueObjChange(args[0].obj.id, args[0]);
    return Void;
}

fn length_method(_: *Vm, args: []Value) Value {
    return .{ .number = @as(f32, @floatFromInt(args[0].obj.data.string.len)) };
}

fn getStr(val: Value) []const u8 {
    return switch (val) {
        .const_string => |s| std.mem.trimRight(u8, s, &[_]u8{0}),
        .obj => |o| if (o.data == .string) o.data.string else "",
        else => "",
    };
}

fn upper_method(vm: *Vm, args: []Value) Value {
    const s = getStr(args[0]);
    const buf = vm.alloc.alloc(u8, s.len) catch return .{ .nil = {} };
    for (s, 0..) |c, i| {
        buf[i] = std.ascii.toUpper(c);
    }
    return vm.gc.create(vm, .{ .string = buf }) catch .{ .nil = {} };
}

fn lower_method(vm: *Vm, args: []Value) Value {
    const s = getStr(args[0]);
    const buf = vm.alloc.alloc(u8, s.len) catch return .{ .nil = {} };
    for (s, 0..) |c, i| {
        buf[i] = std.ascii.toLower(c);
    }
    return vm.gc.create(vm, .{ .string = buf }) catch .{ .nil = {} };
}

fn replace_method(vm: *Vm, args: []Value) Value {
    const haystack = getStr(args[0]);
    const needle = getStr(args[1]);
    const replacement = getStr(args[2]);

    if (needle.len == 0) {
        // nothing to replace, return copy
        const buf = vm.alloc.dupe(u8, haystack) catch return .{ .nil = {} };
        return vm.gc.create(vm, .{ .string = buf }) catch .{ .nil = {} };
    }

    // count occurrences to calculate output size
    var count: usize = 0;
    var pos: usize = 0;
    while (pos <= haystack.len -| needle.len) {
        if (std.mem.eql(u8, haystack[pos..][0..needle.len], needle)) {
            count += 1;
            pos += needle.len;
        } else {
            pos += 1;
        }
    }

    if (count == 0) {
        const buf = vm.alloc.dupe(u8, haystack) catch return .{ .nil = {} };
        return vm.gc.create(vm, .{ .string = buf }) catch .{ .nil = {} };
    }

    const new_len = haystack.len - (count * needle.len) + (count * replacement.len);
    const buf = vm.alloc.alloc(u8, new_len) catch return .{ .nil = {} };

    var src: usize = 0;
    var dst: usize = 0;
    while (src <= haystack.len -| needle.len) {
        if (std.mem.eql(u8, haystack[src..][0..needle.len], needle)) {
            @memcpy(buf[dst..][0..replacement.len], replacement);
            dst += replacement.len;
            src += needle.len;
        } else {
            buf[dst] = haystack[src];
            dst += 1;
            src += 1;
        }
    }
    // copy remaining bytes
    while (src < haystack.len) {
        buf[dst] = haystack[src];
        dst += 1;
        src += 1;
    }

    return vm.gc.create(vm, .{ .string = buf }) catch .{ .nil = {} };
}

fn split_method(vm: *Vm, args: []Value) Value {
    const s = getStr(args[0]);
    const delim = getStr(args[1]);

    var list: std.ArrayList(Value) = .empty;
    // Strings are pushed to vm.stack as we create them so the GC sees them as
    // roots (via Vm.markRoots). Without this, an allocation in a later iteration
    // could collect strings created in earlier iterations — they are only
    // otherwise reachable via `list`, which lives on the C stack and is not a
    // GC root until the final `gc.create(.list)` call below.
    const protect_base = vm.stack.count;

    if (delim.len == 0) {
        // split into individual characters
        for (s) |c| {
            const char_buf = vm.alloc.alloc(u8, 1) catch {
                vm.stack.resize(protect_base);
                return .{ .nil = {} };
            };
            char_buf[0] = c;
            const str_val = vm.gc.create(vm, .{ .string = char_buf }) catch {
                vm.stack.resize(protect_base);
                return .{ .nil = {} };
            };
            list.append(vm.alloc, str_val) catch {
                vm.stack.resize(protect_base);
                return .{ .nil = {} };
            };
            vm.stack.push(str_val);
        }
    } else {
        var pos: usize = 0;
        while (pos <= s.len) {
            const next = if (pos > s.len -| delim.len)
                null
            else
                std.mem.indexOf(u8, s[pos..], delim);

            if (next) |idx| {
                const part = vm.alloc.dupe(u8, s[pos .. pos + idx]) catch {
                    vm.stack.resize(protect_base);
                    return .{ .nil = {} };
                };
                const str_val = vm.gc.create(vm, .{ .string = part }) catch {
                    vm.stack.resize(protect_base);
                    return .{ .nil = {} };
                };
                list.append(vm.alloc, str_val) catch {
                    vm.stack.resize(protect_base);
                    return .{ .nil = {} };
                };
                vm.stack.push(str_val);
                pos += idx + delim.len;
            } else {
                const part = vm.alloc.dupe(u8, s[pos..]) catch {
                    vm.stack.resize(protect_base);
                    return .{ .nil = {} };
                };
                const str_val = vm.gc.create(vm, .{ .string = part }) catch {
                    vm.stack.resize(protect_base);
                    return .{ .nil = {} };
                };
                list.append(vm.alloc, str_val) catch {
                    vm.stack.resize(protect_base);
                    return .{ .nil = {} };
                };
                vm.stack.push(str_val);
                break;
            }
        }
    }

    const result: Value = vm.gc.create(vm, .{ .list = list }) catch .{ .nil = {} };
    vm.stack.resize(protect_base);
    return result;
}

fn substr_method(vm: *Vm, args: []Value) Value {
    const s = getStr(args[0]);
    const start_f = args[1].number;
    const end_f = args[2].number;

    const start: usize = if (start_f < 0) 0 else @intFromFloat(start_f);
    const end_inclusive: usize = if (end_f < 0) 0 else @intFromFloat(end_f);
    const end = end_inclusive + 1; // inclusive to exclusive

    if (start >= s.len) {
        const buf = vm.alloc.alloc(u8, 0) catch return .{ .nil = {} };
        return vm.gc.create(vm, .{ .string = buf }) catch .{ .nil = {} };
    }

    const clamped_end = @min(end, s.len);
    const clamped_start = @min(start, clamped_end);
    const buf = vm.alloc.dupe(u8, s[clamped_start..clamped_end]) catch return .{ .nil = {} };
    return vm.gc.create(vm, .{ .string = buf }) catch .{ .nil = {} };
}

fn trim_method(vm: *Vm, args: []Value) Value {
    const s = getStr(args[0]);
    const trimmed = std.mem.trim(u8, s, " \t\n\r");
    const buf = vm.alloc.dupe(u8, trimmed) catch return .{ .nil = {} };
    return vm.gc.create(vm, .{ .string = buf }) catch .{ .nil = {} };
}

fn contentHashList(items: []const Value) u32 {
    var hasher = std.hash.Wyhash.init(0);
    for (items) |item| {
        switch (item) {
            .number => |n| std.hash.autoHash(&hasher, @as(u32, @intFromFloat(n * 10000.0))),
            .bool => |b| std.hash.autoHash(&hasher, b),
            .visit => |v| std.hash.autoHash(&hasher, v),
            .timestamp => |t| std.hash.autoHash(&hasher, t),
            .const_string => |s| hasher.update(s),
            .obj => |o| switch (o.data) {
                .string => |s| hasher.update(s),
                else => std.hash.autoHash(&hasher, o.id),
            },
            .range => |rng| {
                std.hash.autoHash(&hasher, rng.start);
                std.hash.autoHash(&hasher, rng.end);
            },
            else => std.hash.autoHash(&hasher, @intFromEnum(item)),
        }
    }
    return @as(u32, @truncate(hasher.final()));
}

fn cycle(vm: *Vm, args: []Value) Value {
    const items = args[0].obj.data.list.items;
    if (items.len == 0) return .{ .nil = {} };
    const key = contentHashList(items);
    const entry = vm.variation_state.getOrPut(vm.alloc, key) catch return .{ .nil = {} };
    if (!entry.found_existing) entry.value_ptr.* = .{ .cycle = 0 };
    const idx = entry.value_ptr.cycle;
    const result = items[idx];
    entry.value_ptr.*.cycle = @intCast((@as(usize, idx) + 1) % items.len);
    return result;
}

fn sequence(vm: *Vm, args: []Value) Value {
    const items = args[0].obj.data.list.items;
    if (items.len == 0) return .{ .nil = {} };
    const key = contentHashList(items);
    const entry = vm.variation_state.getOrPut(vm.alloc, key) catch return .{ .nil = {} };
    if (!entry.found_existing) entry.value_ptr.* = .{ .sequence = 0 };
    const idx = entry.value_ptr.sequence;
    const result = items[idx];
    if (idx < @as(u32, @intCast(items.len)) - 1) {
        entry.value_ptr.*.sequence = idx + 1;
    }
    return result;
}

fn shuffle(vm: *Vm, args: []Value) Value {
    if (r == null) r = std.Random.DefaultPrng.init(std.crypto.random.int(u64));
    const items = args[0].obj.data.list.items;
    if (items.len == 0) return .{ .nil = {} };
    const key = contentHashList(items);
    const entry = vm.variation_state.getOrPut(vm.alloc, key) catch return .{ .nil = {} };
    if (!entry.found_existing) {
        entry.value_ptr.* = .{ .shuffle = .{ .index = @intCast(items.len), .order = .empty } };
    }
    // reshuffle when exhausted or on first call
    if (entry.value_ptr.shuffle.index >= @as(u32, @intCast(items.len))) {
        var order = &entry.value_ptr.shuffle.order;
        order.shrinkRetainingCapacity(0);
        order.ensureTotalCapacity(vm.alloc, items.len) catch return .{ .nil = {} };
        for (0..items.len) |i| order.appendAssumeCapacity(@intCast(i));
        // Fisher-Yates shuffle
        var i: usize = items.len - 1;
        while (i > 0) : (i -= 1) {
            const j = r.?.random().intRangeAtMost(usize, 0, i);
            const tmp = order.items[i];
            order.items[i] = order.items[j];
            order.items[j] = tmp;
        }
        entry.value_ptr.shuffle.index = 0;
    }
    const shuffle_idx = entry.value_ptr.shuffle.index;
    const actual_idx = entry.value_ptr.shuffle.order.items[shuffle_idx];
    entry.value_ptr.shuffle.index = shuffle_idx + 1;
    return items[actual_idx];
}

fn random(_: *Vm, args: []Value) Value {
    if (r == null) r = std.Random.DefaultPrng.init(std.crypto.random.int(u64));
    const items = args[0].obj.data.list.items;
    if (items.len == 0) return .{ .nil = {} };
    const idx = r.?.random().intRangeLessThan(usize, 0, items.len);
    return items[idx];
}

fn weighted(_: *Vm, args: []Value) Value {
    if (r == null) r = std.Random.DefaultPrng.init(std.crypto.random.int(u64));
    const items = args[0].obj.data.list;
    const weights = args[1].obj.data.list;

    if (items.items.len == 0 or weights.items.len == 0) return .{ .nil = {} };

    if (items.items.len != weights.items.len) return .{ .const_string = "length_mismatch" };

    var total: f32 = 0;
    for (weights.items) |w| {
        total += w.number;
    }

    if (total <= 0) return .{ .nil = {} };

    var roll = r.?.random().float(f32) * total;
    for (weights.items, 0..) |w, i| {
        roll -= w.number;
        if (roll <= 0) return items.items[i];
    }

    return items.items[items.items.len - 1];
}
