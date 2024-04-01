const std = @import("std");
const Value = @import("values.zig").Value;

pub const Enum = struct {
    name: []const u8,
    values: [][]const u8,

    pub fn init(name: []const u8, values: [][]const u8) Enum {
        return .{
            .name = name,
            .values = values,
        };
    }

    pub const Val = struct {
        index: u8,
        base: *const Value,
    };
};
