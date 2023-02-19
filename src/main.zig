const std = @import("std");

const Struct = struct {
    const Type = std.meta.Tag(UnionType);
    type: UnionType,
    const UnionType = union(enum) {
        one: void,
        two: struct {
            inner: i8
        }
    };
};

test "UnionType" {
    const s = Struct{
        .type = .{
            .two = .{ .inner = 1 }
        }
    };
    std.debug.print("{}", .{s});
}

