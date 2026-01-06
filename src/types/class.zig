const std = @import("std");
const Value = @import("value.zig").Value;

pub const Class = struct {
    allocator: std.mem.Allocator,
    name: []const u8,
    fields: []Member,
    methods: []Member,
    is_gc_managed: bool = false,

    pub const Member = struct {
        name: []const u8,
        value: Value,
    };

    pub fn init(allocator: std.mem.Allocator, name: []const u8, fields: []Member, methods: []Member) !Class {
        return .{
            .allocator = allocator,
            .name = name,
            .fields = fields,
            .methods = methods,
        };
    }

    pub fn deinit(self: *const Class) void {
        self.allocator.free(self.name);
        for (self.fields) |f| {
            self.allocator.free(f.name);
            if (self.is_gc_managed) continue;
            f.value.destroy(self.allocator);
        }
        self.allocator.free(self.fields);
        for (self.methods) |f| {
            self.allocator.free(f.name);
            if (self.is_gc_managed) continue;
            f.value.destroy(self.allocator);
        }
        self.allocator.free(self.methods);
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

    pub fn createInstance(self: *Class, base: *Value.Obj, inst_fields: []Member) !Instance {
        var values = try self.allocator.alloc(Value, self.fields.len);
        for (self.fields, 0..) |f, i| values[i] = try f.value.clone(self.allocator);
        for (inst_fields) |inst_field| {
            if (self.getFieldIndex(inst_field.name)) |i| values[i] = inst_field.value;
        }
        return .{
            .base = base,
            .fields = values,
        };
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
