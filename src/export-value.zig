const std = @import("std");
const values = @import("./values.zig");
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
        string: [*c]const u8,
        list: extern struct {
            items: [*c]ExportValue,
            count: u16,
        },
        enum_value: extern struct {
            enum_name: [*c]const u8,
            value_name: [*c]const u8,
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
                .enum_name = e.base.data.@"enum".name.ptr,
                .value_name = e.base.data.@"enum".values[e.index].ptr,
            } } },
            .obj => |o| switch (o.data) {
                // We're mixing memory management here, which is a very bad idea,
                // Tried passing in a "ExportAllocator" and copying the values to the memory
                // but that didn't work out. Will revisit one day
                .string => |s| .{ .tag = Tag.string, .data = .{ .string = s.ptr } },
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

    pub fn toValue(self: *const ExportValue) Value {
        return switch (self.tag) {
            .nil => values.Nil,
            .bool => if (self.data.bool) values.True else values.False,
            .number => .{ .number = self.data.number },
            else => unreachable,
        };
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
