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
    var stderr_buffer: [1024]u8 = undefined;
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
            if (item.obj.data != .string) break :blk false;
            // need to remove the null termination of the needle
            const indexOf = std.mem.indexOf(u8, s, std.mem.trimRight(u8, item.obj.data.string, &[_]u8{0}));
            break :blk indexOf != null;
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
