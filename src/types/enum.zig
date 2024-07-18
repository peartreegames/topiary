const std = @import("std");
const Value = @import("value.zig").Value;

pub const Enum = struct {
    name: []const u8,
    values: [][]const u8,
    is_seq: bool,

    pub fn init(name: []const u8, values: [][]const u8, is_seq: bool) Enum {
        return .{ .name = name, .values = values, .is_seq = is_seq };
    }

    pub const Val = struct {
        base: *Value.Obj,
        index: u8,
    };
};
