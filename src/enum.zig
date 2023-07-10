const std = @import("std");

pub const Enum = struct {
    allocator: std.mem.Allocator,
    name: []const u8,
    values: []Value,

    pub const Value = struct {
        index: usize,
        name: []const u8,
        base: *Enum,
    };
};
