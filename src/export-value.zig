const std = @import("std");
const Value = @import("./values.zig").Value;

const Tag = enum(u8) {
    nil,
    bool,
    number,
    string,
    list,
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
    },

    pub const Nil: ExportValue = .{ .tag = Tag.nil, .data = .{ .nil = {} } };
    pub const True: ExportValue = .{ .tag = Tag.bool, .data = .{ .bool = true } };
    pub const False: ExportValue = .{ .tag = Tag.bool, .data = .{ .bool = false } };

    pub fn fromValue(value: Value, allocator: std.mem.Allocator) ExportValue {
        return switch (value) {
            .bool => |b| if (b) True else False,
            .number => |n| .{ .tag = Tag.number, .data = .{ .number = n } },
            .obj => |o| switch (o.data) {
                .string => |s| .{ .tag = Tag.string, .data = .{ .string = s.ptr } },
                .list => |l| blk: {
                    var list = allocator.alloc(ExportValue, l.items.len) catch @panic("Could not allocate list items");
                    for (l.items, 0..) |item, i| {
                        list[i] = fromValue(item, allocator);
                    }
                    break :blk .{ .tag = Tag.list, .data = .{ .list = .{ .items = list.ptr, .count = @intCast(list.len) } } };
                },
                else => Nil,
            },
            else => Nil,
        };
    }

    pub fn deinit(self: *const ExportValue, allocator: std.mem.Allocator) void {
        if (self.tag == .list) {
            for (self.data.list.items[0..self.data.list.count]) |item| {
                item.deinit(allocator);
            }
            allocator.free(self.data.list.items[0..self.data.list.count]);
        }
    }

};
