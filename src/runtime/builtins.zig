const std = @import("std");

const types = @import("../types/index.zig");
const Value = types.Value;
const Void = types.Void;
const True = types.True;
const False = types.False;

const Vm = @import("vm.zig").Vm;
pub const Builtin = *const fn (vm: *Vm, args: []Value) Value;
var r: ?std.rand.DefaultPrng = null;

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
    "ustime",
    create("ustime", 0, false, ustime),
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
    var obj = .{
        .data = .{
            .builtin = .{
                .backing = backing,
                .arity = arity,
                .is_method = is_method,
                .name = name,
            },
        },
    };
    return .{ .obj = &obj };
}

fn rnd(_: *Vm, args: []Value) Value {
    if (r == null) r = std.rand.DefaultPrng.init(std.crypto.random.int(u64));
    const start = @as(i32, @intFromFloat(args[0].number));
    const end = @as(i32, @intFromFloat(args[1].number));
    return .{ .number = @as(f32, @floatFromInt(r.?.random().intRangeAtMost(i32, start, end))) };
}

fn rnd01(_: *Vm, args: []Value) Value {
    if (r == null) r = std.rand.DefaultPrng.init(std.crypto.random.int(u64));
    _ = args;
    return .{ .number = r.?.random().float(f32) };
}

fn round(_: *Vm, args: []Value) Value {
    return .{ .number = @floatFromInt(@as(i64, @intFromFloat(args[0].number))) };
}

fn abs(_: *Vm, args: []Value) Value {
    return .{ .number = @abs(args[0].number) };
}

fn print(_: *Vm, args: []Value) Value {
    const writer = std.io.getStdErr().writer();
    args[0].print(writer) catch unreachable;
    writer.print("\n", .{}) catch unreachable;
    return Void;
}

fn mstime(_: *Vm, _: []Value) Value {
    return .{ .number = @floatFromInt(std.time.milliTimestamp()) };
}
fn ustime(_: *Vm, _: []Value) Value {
    return .{ .number = @floatFromInt(std.time.microTimestamp()) };
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
        .list => args[0].obj.data.list.append(item) catch {},
        .set => args[0].obj.data.set.put(item, {}) catch {},
        else => unreachable,
    }
    if (args[0].obj.index) |i| {
        vm.notifyValueChange(i, Void, args[0]);
    }
    return Void;
}

fn addmap_method(vm: *Vm, args: []Value) Value {
    const key = args[1];
    const item = args[2];
    switch (args[0].obj.data) {
        .map => args[0].obj.data.map.put(key, item) catch {},
        else => unreachable,
    }
    if (args[0].obj.index) |i| {
        vm.notifyValueChange(i, Void, args[0]);
    }
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
    if (args[0].obj.index) |i| {
        vm.notifyValueChange(i, Void, args[0]);
    }
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
        .list => data.list.clearAndFree(),
        .map => data.map.clearAndFree(),
        .set => data.set.clearAndFree(),
        else => unreachable,
    }
    if (args[0].obj.index) |i| {
        vm.notifyValueChange(i, Void, args[0]);
    }
    return Void;
}
