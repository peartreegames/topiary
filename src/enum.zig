const std = @import("std");

pub const Enum = struct {
    allocator: std.mem.Allocator,
    name: []const u8,
    values: [][]const u8,

    pub const Value = struct {
        index: u8,
        base: *const Enum,
    };
};
