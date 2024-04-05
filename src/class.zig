const std = @import("std");
const Value = @import("./values.zig").Value;

pub const Class = struct {
    allocator: std.mem.Allocator,
    name: []const u8,
    fields: []Field,

    pub const Field = struct {
        name: []const u8,
        value: Value,
    };

    pub fn init(allocator: std.mem.Allocator, name: []const u8, fields: []Field) !Class {
        return .{
            .allocator = allocator,
            .name = name,
            .fields = fields,
        };
    }

    pub fn deinit(self: *const Class) void {
        self.allocator.free(self.fields);
    }

    pub fn getIndex(self: *const Class, field_name: []const u8) ?usize {
        var i: usize = 0;
        while (i < self.fields.len) : (i += 1) {
            if (!std.mem.eql(u8, self.fields[i].name, field_name)) continue;
            return i;
        }
        return null;
    }

    pub fn createInstance(self: *Class, base: *Value.Obj, inst_fields: []Field) !Instance {
        var values = try self.allocator.alloc(Value, self.fields.len);
        for (self.fields, 0..) |f, i| values[i] = f.value;
        for (inst_fields) |inst_field| {
            if (self.getIndex(inst_field.name)) |i| values[i] = inst_field.value;
        }
        return .{
            .base = base,
            .fields = values,
        };
    }

    pub const Instance = struct {
        base: *Value.Obj,
        fields: []Value,
    };
};
