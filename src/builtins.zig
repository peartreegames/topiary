const std = @import("std");
const Value = @import("./values.zig").Value;
const Gc = @import("./gc.zig").Gc;

pub const Builtin = *const fn (gc: *Gc, args: []Value) Value;

const Rnd = struct {
    const Self = @This();
    var r: ?std.rand.DefaultPrng = null;
    var value: Value = .{
        .obj = &Self.obj,
    };
    var obj: Value.Obj = .{
        .data = .{
            .builtin = .{
                .backing = Self.builtin,
                .arity = 2,
            },
        },
    };
    fn builtin(_: *Gc, args: []Value) Value {
        if (r == null) r = std.rand.DefaultPrng.init(std.crypto.random.int(u64));
        const start = @intFromFloat(i32, args[0].number);
        const end = @intFromFloat(i32, args[1].number);
        return .{ .number = @floatFromInt(f32, r.?.random().intRangeAtMost(i32, start, end)) };
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
            },
        },
    };
    fn builtin(_: *Gc, args: []Value) Value {
        if (r == null) r = std.rand.DefaultPrng.init(std.crypto.random.int(u64));
        _ = args;
        return .{ .number = r.?.random().float(f32) };
    }
};

const Definition = struct {
    name: []const u8,
    value: *Value,
};

pub const builtins = [_]Definition{
    .{
        .name = "rnd",
        .value = &Rnd.value,
    },
    .{
        .name = "rnd01",
        .value = &Rnd01.value,
    },
};
