const std = @import("std");
const values = @import("./values.zig");
const Vm = @import("vm.zig").Vm;
const Value = values.Value;

const Tag = enum(u8) {
    nil,
    bool,
    number,
    string,
    list,
    set,
    map,
    enum_value,
};

pub const ExportValue = extern struct {
    tag: Tag,
    data: extern union {
        nil: void,
        bool: bool,
        number: f32,
        string: [*:0]const u8,
        list: extern struct {
            items: [*c]ExportValue,
            count: u16,
        },
        enum_value: extern struct {
            name: [*:0]const u8,
            value: [*:0]const u8,
        },
    },

    pub const Nil: ExportValue = .{ .tag = Tag.nil, .data = .{ .nil = {} } };
    pub const True: ExportValue = .{ .tag = Tag.bool, .data = .{ .bool = true } };
    pub const False: ExportValue = .{ .tag = Tag.bool, .data = .{ .bool = false } };

    pub fn fromValue(value: Value, allocator: std.mem.Allocator) ExportValue {
        return switch (value) {
            .bool => |b| if (b) True else False,
            .number => |n| .{ .tag = Tag.number, .data = .{ .number = n } },
            .enum_value => |e| .{ .tag = Tag.enum_value, .data = .{ .enum_value = .{
                .name = e.base.data.@"enum".name[0..:0],
                .value = e.base.data.@"enum".values[e.index][0..:0],
            } } },
            .obj => |o| switch (o.data) {
                .string => |s| .{ .tag = Tag.string, .data = .{ .string = s[0..:0] } },
                .list => |l| blk: {
                    var list = allocator.alloc(ExportValue, l.items.len) catch @panic("Could not allocate list items");
                    var i: usize = 0;
                    while (i < l.items.len) : (i += 1) {
                        list[i] = fromValue(l.items[i], allocator);
                    }
                    break :blk .{ .tag = Tag.list, .data = .{ .list = .{ .items = list.ptr, .count = @intCast(list.len) } } };
                },
                .set => |s| blk: {
                    var list = allocator.alloc(ExportValue, s.count()) catch @panic("Could not allocate list items");
                    for (s.keys(), 0..) |key, i| {
                        list[i] = fromValue(key, allocator);
                    }
                    break :blk .{ .tag = Tag.set, .data = .{ .list = .{ .items = list.ptr, .count = @intCast(list.len) } } };
                },
                .map => |m| blk: {
                    const count = m.count();
                    var list = allocator.alloc(ExportValue, count * 2) catch @panic("Could not allocate list items");
                    var it = m.iterator();
                    var i: usize = 0;
                    while (it.next()) |kvp| {
                        list[i] = fromValue(kvp.key_ptr.*, allocator);
                        list[i + 1] = fromValue(kvp.value_ptr.*, allocator);
                        i += 2;
                    }
                    break :blk .{ .tag = Tag.map, .data = .{ .list = .{ .items = list.ptr, .count = @intCast(count) } } };
                },
                else => Nil,
            },
            else => Nil,
        };
    }

    pub fn toValue(self: *const ExportValue, vm: *Vm) !Value {
        return switch (self.tag) {
            .nil => values.Nil,
            .bool => if (self.data.bool) values.True else values.False,
            .number => .{ .number = self.data.number },
            .string => {
                const str = try vm.allocator.dupe(u8, std.mem.sliceTo(self.data.string, 0));
                return vm.gc.create(vm, .{ .string = str });
            },
            .list => {
                var list = std.ArrayList(Value).init(vm.allocator);
                for (0..self.data.list.count) |i| {
                    const item: *ExportValue = @ptrCast(&self.data.list.items[i]);
                    try list.append(try item.toValue(vm));
                }
                return vm.gc.create(vm, .{ .list = list });
            },
            .set => {
                var set = Value.Obj.SetType.initContext(vm.allocator, values.adapter);
                const length = self.data.list.count;
                var i: usize = 0;
                while (i < length) : (i += 1) {
                    const item: *ExportValue = @ptrCast(&self.data.list.items[i]);
                    try set.put(try item.toValue(vm), {});
                }
                return vm.gc.create(vm, .{ .set = set });
            },
            .map => {
                var map = Value.Obj.MapType.initContext(vm.allocator, values.adapter);
                const length = self.data.list.count * 2;
                var i: usize = 0;
                while (i < length) : (i += 2) {
                    const key: *ExportValue = @ptrCast(&self.data.list.items[i]);
                    const value: *ExportValue = @ptrCast(&self.data.list.items[i + 1]);
                    try map.put(try key.toValue(vm), try value.toValue(vm));
                }
                return  vm.gc.create(vm, .{ .map = map });
            },
            else => unreachable,
        };
    }
    pub fn print(self: ExportValue, writer: anytype) void {
        switch (self.tag) {
            .nil => writer.print("nil", .{}),
            .bool => writer.print("{}", .{self.data.bool}),
            .number => writer.print("{d:.5}", .{self.data.number}),
            .enum_value => writer.print("{s}.{s}", .{ self.data.enum_value.name, self.data.enum_value.value }),
            .string => writer.print("{s}", .{self.data.string}),
            .list => {
                writer.print("List{{", .{});
                for (0..self.data.list.count) |i| {
                    self.data.list.items[i].print(writer);
                }
                writer.print("}}", .{});
            },
            .set => {
                writer.print("Set{{", .{});
                for (0..self.data.list.count) |i| {
                    self.data.list.items[i].print(writer);
                }
                writer.print("}}", .{});
            },
            .map => {
                writer.print("Map{{", .{});
                var i: usize = 0;
                while (i < self.data.list.count) : (i += 2) {
                    self.data.list.items[i].print(writer);
                    writer.print(":", .{});
                    self.data.list.items[i + 1].print(writer);
                    writer.print(",", .{});
                }
                writer.print("}}", .{});
            },
        }
    }

    pub fn deinit(self: *const ExportValue, allocator: std.mem.Allocator) void {
        switch (self.tag) {
            .list, .set, .map => {
                var count = self.data.list.count;
                if (self.tag == .map) count *= 2;
                for (self.data.list.items[0..count]) |item| {
                    item.deinit(allocator);
                }
                allocator.free(self.data.list.items[0..count]);
            },
            else => {},
        }
    }
};
