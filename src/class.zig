const std = @import("std");
const Value = @import("./values.zig").Value;

pub const Class = struct {
    allocator: std.mem.Allocator,
    name: []const u8,
    defaults: []Field,

    pub const Field = struct {
        name: []const u8,
        value: Value,
    };

    pub fn init(allocator: std.mem.Allocator, name: []const u8, fields: []Field) !Class {
        return .{
            .allocator = allocator,
            .name = name,
            .defaults = fields,
        };
    }

    pub fn deinit(self: *const Class) void {
        self.allocator.free(self.defaults);
    }

    pub fn createInstance(self: *Class, fields: []Field) !Instance {
        var values = std.StringHashMap(Value).init(self.allocator);
        for (self.defaults) |field| {
            try values.put(field.name, field.value);
        }
        for (fields) |field| {
            try values.put(field.name, field.value);
        }

        return .{
            .class = self,
            .fields = values,
        };
    }

    pub const Instance = struct {
        class: *Class,
        fields: std.StringHashMap(Value),
    };
};
