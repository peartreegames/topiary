const std = @import("std");

const types = @import("../types/index.zig");
const Value = types.Value;
const Void = types.Void;
const True = types.True;
const False = types.False;

const Vm = @import("vm.zig").Vm;
pub const Builtin = *const fn (vm: *Vm, args: []Value) Value;

pub const Rnd = struct {
    const Self = @This();
    var r: ?std.rand.DefaultPrng = null;
    var value: Value = .{
        .obj = &Self.obj,
    };
    var obj: Value.Obj = .{
        .data = .{
            .builtin = .{ .backing = Self.builtin, .arity = 2, .is_method = false, .name = "rnd" },
        },
    };
    fn builtin(_: *Vm, args: []Value) Value {
        if (r == null) r = std.rand.DefaultPrng.init(std.crypto.random.int(u64));
        const start = @as(i64, @intFromFloat(args[0].number));
        const end = @as(i64, @intFromFloat(args[1].number));
        return .{ .number = @as(f64, @floatFromInt(r.?.random().intRangeAtMost(i64, start, end))) };
    }
};

const Rnd01 = struct {
    const Self = @This();
    var r: ?std.rand.DefaultPrng = null;
    var value: Value = .{
        .obj = &Self.obj,
    };
    var obj: Value.Obj = .{
        .data = .{
            .builtin = .{
                .backing = Self.builtin,
                .arity = 0,
                .is_method = false,
                .name = "rnd01",
            },
        },
    };
    fn builtin(_: *Vm, args: []Value) Value {
        if (r == null) r = std.rand.DefaultPrng.init(std.crypto.random.int(u64));
        _ = args;
        return .{ .number = r.?.random().float(f64) };
    }
};

const Round = struct {
    const Self = @This();

    var value: Value = .{
        .obj = &Self.obj,
    };
    var obj: Value.Obj = .{
        .data = .{
            .builtin = .{
                .backing = Self.builtin,
                .arity = 1,
                .is_method = false,
                .name = "round",
            },
        },
    };
    fn builtin(_: *Vm, args: []Value) Value {
        return .{ .number = @floatFromInt(@as(i64, @intFromFloat(args[0].number))) };
    }
};

const Print = struct {
    const Self = @This();
    var value: Value = .{
        .obj = &Self.obj,
    };
    var obj: Value.Obj = .{
        .data = .{
            .builtin = .{
                .backing = Self.builtin,
                .arity = 1,
                .is_method = false,
                .name = "print",
            },
        },
    };
    fn builtin(_: *Vm, args: []Value) Value {
        const writer = std.debug;
        args[0].print(writer, null);
        writer.print("\n", .{});
        return Void;
    }
};

pub const Time = struct {
    const Self = @This();
    var mstime: Value = .{ .obj = &Self.mstime_obj };
    var ustime: Value = .{ .obj = &Self.ustime_obj };
    var mstime_obj: Value.Obj = .{
        .data = .{
            .builtin = .{
                .backing = Self.msbuiltin,
                .arity = 0,
                .is_method = false,
                .name = "mstime",
            },
        },
    };
    var ustime_obj: Value.Obj = .{
        .data = .{
            .builtin = .{
                .backing = Self.usbuiltin,
                .arity = 0,
                .is_method = false,
                .name = "ustime",
            },
        },
    };
    fn msbuiltin(_: *Vm, _: []Value) Value {
        return .{ .number = @floatFromInt(std.time.milliTimestamp()) };
    }
    fn usbuiltin(_: *Vm, _: []Value) Value {
        return .{ .number = @floatFromInt(std.time.microTimestamp()) };
    }
};

pub const Assert = struct {
    const Self = @This();
    var value: Value = .{
        .obj = &Self.obj,
    };
    var obj: Value.Obj = .{
        .data = .{
            .builtin = .{
                .backing = Self.builtin,
                .arity = 2,
                .is_method = false,
                .name = "assert",
            },
        },
    };
    fn builtin(_: *Vm, args: []Value) Value {
        const expr = args[0];
        const msg = args[1];
        if (!expr.eql(True)) return msg;
        return Void;
    }
};

const Definition = struct {
    name: []const u8,
    value: *Value,
};

pub const definitions = [_]Definition{ .{
    .name = "rnd",
    .value = &Rnd.value,
}, .{
    .name = "rnd01",
    .value = &Rnd01.value,
}, .{
    .name = "print",
    .value = &Print.value,
}, .{
    .name = "round",
    .value = &Round.value,
}, .{
    .name = "assert",
    .value = &Assert.value,
}, .{
    .name = "mstime",
    .value = &Time.mstime,
}, .{
    .name = "ustime",
    .value = &Time.ustime,
} };

pub const Count = struct {
    const Self = @This();
    pub var value: Value = .{
        .obj = &Self.obj,
    };
    var obj: Value.Obj = .{
        .data = .{
            .builtin = .{ .backing = Self.builtin, .arity = 1, .is_method = true, .name = "count" },
        },
    };
    fn builtin(_: *Vm, args: []Value) Value {
        const data = args[0].obj.data;
        const count = switch (data) {
            .list => |l| l.items.len,
            .map => |m| m.count(),
            .set => |s| s.count(),
            else => 0,
        };
        return .{ .number = @as(f64, @floatFromInt(count)) };
    }
};

pub const Add = struct {
    const Self = @This();
    pub var value: Value = .{
        .obj = &Self.obj,
    };
    var obj: Value.Obj = .{
        .data = .{
            .builtin = .{ .backing = Self.builtin, .arity = 2, .is_method = true, .name = "add" },
        },
    };
    fn builtin(vm: *Vm, args: []Value) Value {
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
};

pub const AddMap = struct {
    const Self = @This();
    pub var value: Value = .{
        .obj = &Self.obj,
    };
    var obj: Value.Obj = .{
        .data = .{
            .builtin = .{ .backing = Self.builtin, .arity = 3, .is_method = true, .name = "addMap" },
        },
    };
    fn builtin(vm: *Vm, args: []Value) Value {
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
};
pub const Remove = struct {
    const Self = @This();
    pub var value: Value = .{
        .obj = &Self.obj,
    };
    var obj: Value.Obj = .{
        .data = .{
            .builtin = .{ .backing = Self.builtin, .arity = 2, .is_method = true, .name = "remove" },
        },
    };
    fn builtin(vm: *Vm, args: []Value) Value {
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
};

pub const Has = struct {
    const Self = @This();
    pub var value: Value = .{
        .obj = &Self.obj,
    };
    var obj: Value.Obj = .{
        .data = .{
            .builtin = .{ .backing = Self.builtin, .arity = 2, .is_method = true, .name = "has" },
        },
    };
    fn builtin(_: *Vm, args: []Value) Value {
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
};
pub const Clear = struct {
    const Self = @This();
    pub var value: Value = .{
        .obj = &Self.obj,
    };
    var obj: Value.Obj = .{
        .data = .{
            .builtin = .{ .backing = Self.builtin, .arity = 1, .is_method = true, .name = "clear" },
        },
    };
    fn builtin(vm: *Vm, args: []Value) Value {
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
};
