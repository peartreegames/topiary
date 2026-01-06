const std = @import("std");
const Value = @import("value.zig").Value;

pub const Enum = struct {
    name: []const u8,
    values: [][]const u8,
    is_seq: bool,
    is_gc_managed: bool = false,

    pub fn init(name: []const u8, values: [][]const u8, is_seq: bool) Enum {
        return .{ .name = name, .values = values, .is_seq = is_seq };
    }

    pub fn deinit(self: *const Enum, allocator: std.mem.Allocator) void {
        allocator.free(self.name);
        if (!self.is_gc_managed) for (self.values) |val| allocator.free(val);
        allocator.free(self.values);
    }

    pub const Val = struct {
        base: *Value.Obj,
        index: u8,
    };
};
