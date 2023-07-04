const std = @import("std");
const Value = @import("./values.zig").Value;

pub const Field = struct {
    name: []const u8,
    default_value: Value,
};

pub fn Define(allocator: std.mem.Allocator, fields: []Field) type {
    const default_fields = std.StringHashMap(Value).init(allocator);
    for (fields) |field| {
        try default_fields.put(field.name, field.default_value);
    }
    return struct {
        fields: std.StringHashMap(Value) = default_fields,
    };
}
