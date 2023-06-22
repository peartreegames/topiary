const std = @import("std");
const ast = @import("./ast.zig");
const Allocator = std.mem.Allocator;

pub const True = Value{ .bool = true };
pub const False = Value{ .bool = false };
pub const Void = Value{.void};
pub const Nil = Value{.nil};
// pub const NativeFn = fn (gc: *Gc, args: []*Value) anyerror!*Value;

const Type = enum(u8) {
    void,
    nil,
    bool,
    number,
    string,
    @"enum",
    range,
    // list,
    // set,
    // map,
};

pub const Value = union(Type) {
    void: void,
    nil: void,
    bool: bool,
    number: f32,
    range: struct {
        start: i32,
        end: i32,
    },

    string: String,
    @"enum": [][]const u8,
    // list: std.ArrayListUnmanaged(*Value),

    pub fn create(comptime T: type, value: T) Value {
        return switch (T) {
            f32 => .{ .number = value },
            bool => .{ .bool = value },
            else => .{.void},
        };
    }

    pub fn is(self: Value, tag_type: Type) bool {
        return self.tag() == tag_type;
    }

    pub fn tag(self: Value) Type {
        return @as(Type, self);
    }

    pub fn equals(self: Value, other: Value) bool {
        if (@enumToInt(self) != @enumToInt(other)) return false;
        return switch (self) {
            .void => true,
            .number => |n| n == other.number,
            .bool => |b| b == other.bool,
            .range => |r| r.start == other.range.start and r.end == other.range.end,
            else => false,
        };
    }

    pub fn print(self: Value, writer: anytype) void {
        switch (self) {
            .number => |n| writer.print("{d}", .{n}),
            .bool => |b| writer.print("{}", .{b}),
            .string => |s| writer.print("{s}", .{s}),
            .nil => writer.print("nil", .{}),
            // .list => {
            //     const list = self.toList();
            //     writer.print("[", .{});
            //     for (list.items, 0..) |item, i| {
            //         item.*.print(writer);
            //         if (i != list.items.len - 1)
            //             writer.print(",\n", .{});
            //     }
            //     writer.print("]\n", .{});
            // },
            // .map => |m| {
            //     writer.print("{", .{});
            //     for (m.entries.items, 0..) |item, i| {
            //         item.key.print(writer);
            //         writer.print(":", .{});
            //         item.value.print(writer);
            //         if (i != m.items().len - 1)
            //             writer.print(",\n", .{});
            //     }
            //     writer.print("}\n", .{});
            // },
            // .set => |s| {
            //     writer.print("{", .{});
            //     for (s.entries.items, 0..) |item, i| {
            //         item.key.print(writer);
            //         if (i != s.items().len - 1)
            //             writer.print(",\n", .{});
            //     }
            //     writer.print("}\n", .{});
            // },
            // .range => |r| writer.print("{}..{}", .{ r.start, r.end }),
            // .@"enum" => |e| {
            //     writer.print("{", .{});
            //     for (e, 0..) |item, i| {
            //         writer.print("{s}", .{item});
            //         if (i != e.len - 1)
            //             writer.print(",\n", .{});
            //     }
            //     writer.print("}\n", .{});
            // },
            else => writer.print("void", .{}),
        }
    }

    fn hash(key: *Value) u32 {
        const hashFn = std.hash.autoHash;
        var hasher = std.hash.Wyhash.init(0);

        switch (key.type) {
            .number => |n| hashFn(&hasher, n),
            .bool => |b| hashFn(&hasher, b),
            .string => |s| hasher.update(s),
            // .function |f| => {
            //     if (f.name) |name|
            //         hasher.update(name);
            //     hashFn(&hasher, f.arg_len);
            //     hashFn(&hasher, f.locals);
            //     hashFn(&hasher, f.entry);
            // },
            .list => |l| {
                hashFn(&hasher, l.items.len);
                hashFn(&hasher, l.items.ptr);
            },
            .map => |m| {
                hashFn(&hasher, m.items().len);
                hashFn(&hasher, m.items().ptr);
            },
            // .native => |n| {
            //     const native = key.toNative();
            //     hashFn(&hasher, native.arg_len);
            //     hashFn(&hasher, native.func);
            // },
            .range => |r| {
                hashFn(&hasher, r.start);
                hashFn(&hasher, r.end);
            },
            else => unreachable,
        }
        return @truncate(u32, hasher.final());
    }

    fn eql(a: *Value, b: *Value) bool {
        if (a != b) return false;
        return switch (a) {
            .number => |n| @fabs(n - b.number) < 0.000001,
            .bool => |bl| bl == b.bool,
            .nil => b == .nil,
            .string => |s| std.mem.eql(u8, s, b.string),
            .list => |l| {
                const l_b = b.list;

                if (l.items.len != l_b.items.len) return false;
                for (l.items, 0..) |item, i| {
                    if (!item.eql(l_b.items[i])) return false;
                }
                return true;
            },
            .map => |m| {
                const map_b = b.map;

                if (m.items().len != map_b.items().len) return false;
                for (a.items(), 0..) |entry, i| {
                    if (entry.hash != map_b.items()[i].hash) return false;
                }
                return true;
            },
            .range => |r| {
                const range_b = b.range;
                return r.start == range_b.start and r.end == range_b.end;
            },
            else => unreachable,
        };
    }
};

pub const String = struct {
    data: []const u8,

    pub fn create(gc: *Gc, value: []const u8) !*String {
        return try gc.create(String, .{ .data = try gc.allocator.dupe(u8, value) });
    }

    pub fn destroy(self: *String, gc: *Gc) void {
        gc.allocator.free(self.data);
        gc.allocator.destroy(self);
    }
};
