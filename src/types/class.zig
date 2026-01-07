const std = @import("std");
const Value = @import("value.zig").Value;

pub const Class = struct {
    name: []const u8,
    fields: []Member,
    methods: []Member,
    is_gc_managed: bool = false,

    pub const Member = struct {
        name: []const u8,
        value: Value,
    };

    pub fn init(name: []const u8, fields: []Member, methods: []Member) !Class {
        return .{
            .name = name,
            .fields = fields,
            .methods = methods,
        };
    }

    pub fn deinit(self: *const Class, allocator: std.mem.Allocator) void {
        allocator.free(self.name);
        for (self.fields) |f| {
            allocator.free(f.name);
            if (self.is_gc_managed) continue;
            f.value.destroy(allocator);
        }
        allocator.free(self.fields);
        for (self.methods) |f| {
            allocator.free(f.name);
            if (self.is_gc_managed) continue;
            f.value.destroy(allocator);
        }
        allocator.free(self.methods);
    }

    pub fn getFieldIndex(self: *const Class, field_name: []const u8) ?usize {
        var i: usize = 0;
        while (i < self.fields.len) : (i += 1) {
            if (!std.mem.eql(u8, self.fields[i].name, std.mem.trim(u8, field_name, &[_]u8{0}))) continue;
            return i;
        }
        return null;
    }

    pub fn getMethodIndex(self: *const Class, method_name: []const u8) ?usize {
        var i: usize = 0;
        while (i < self.methods.len) : (i += 1) {
            if (!std.mem.eql(u8, self.methods[i].name, std.mem.trim(u8, method_name, &[_]u8{0}))) continue;
            return i;
        }
        return null;
    }

    pub fn getProperty(self: *const Class, name: []const u8) ?Value {
        var i: usize = 0;
        while (i < self.fields.len) : (i += 1) {
            if (!std.mem.eql(u8, self.fields[i].name, std.mem.trim(u8, name, &[_]u8{0}))) continue;
            return self.fields[i].value;
        }
        i = 0;
        while (i < self.methods.len) : (i += 1) {
            if (!std.mem.eql(u8, self.methods[i].name, std.mem.trim(u8, name, &[_]u8{0}))) continue;
            return self.methods[i].value;
        }
        return null;
    }

    pub const Instance = struct {
        base: *Value.Obj,
        fields: []Value,

        pub fn getProperty(self: *const Instance, name: []const u8) !?Value {
            var i: usize = 0;
            while (i < self.fields.len) : (i += 1) {
                if (!std.mem.eql(u8, self.base.data.class.fields[i].name, std.mem.trim(u8, name, &[_]u8{0}))) continue;
                return self.fields[i];
            }
            i = 0;
            while (i < self.base.data.class.methods.len) : (i += 1) {
                if (!std.mem.eql(u8, self.base.data.class.methods[i].name, std.mem.trim(u8, name, &[_]u8{0}))) continue;
                return self.base.data.class.methods[i].value;
            }
            return null;
        }
    };
};
