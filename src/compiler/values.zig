const std = @import("std");
const ast = @import("./ast.zig");
const Allocator = std.mem.Allocator;

pub const True = Value{ .type = .{ .bool = true } };
pub const False = Value{ .type = .{ .bool = false } };
pub const Void = Value{ .type = .void };
pub const Nil = Value{ .type = .nil };
// pub const NativeFn = fn (gc: *Gc, args: []*Value) anyerror!*Value;

pub const Value = struct {
    is_mutable: bool = false,
    type: Type,
    collection_type: CollectionType,

    const Type = union(enum) {
        void: void,
        nil: void,
        bool: bool,
        number: f64,
        string: []const u8,
        @"enum": [][]const u8,

        range: struct {
            start: i32,
            end: i32,
        },
    };

    const CollectionType = union(enum) {
        list: std.ArrayListUnmanaged(Type),
        set: std.AutoArrayHashMapUnmanaged(Value, void),
        map: std.AutoArrayHashMapUnmanaged(Value, Value),
    };

    pub fn print(self: *Value, writer: anytype) !void {
        switch (self.type) {
            .number => |n| try writer.print("{d}", .{n}),
            .bool => |b| try writer.print("{}", .{b}),
            .string => |s| try writer.writeAll(s),
            .nil => try writer.writeAll("nil"),
            .list => |l| {
                try writer.writeAll("[");
                for (l.items, 0..) |item, i| {
                    try item.print(writer);
                    if (i != l.items.len - 1)
                        try writer.writeAll(",\n");
                }
                try writer.writeAll("]\n");
            },
            .map => |m| {
                try writer.writeAll("{");
                for (m.entries.items, 0..) |item, i| {
                    try item.key.print(writer);
                    try writer.writeAll(":");
                    try item.value.print(writer);
                    if (i != m.items().len - 1)
                        try writer.writeAll(",\n");
                }
                try writer.writeAll("}\n");
            },
            .set => |s| {
                try writer.writeAll("{");
                for (s.entries.items, 0..) |item, i| {
                    try item.key.print(writer);
                    if (i != s.items().len - 1)
                        try writer.writeAll(",\n");
                }
                try writer.writeAll("}\n");
            },
            .range => |r| try writer.print("{}..{}", .{ r.start, r.end }),
            .@"enum" => |e| {
                try writer.writeAll("{");
                for (e, 0..) |item, i| {
                    try writer.writeAll(item);
                    if (i != e.len - 1)
                        try writer.writeAll(",\n");
                }
                try writer.writeAll("}\n");
            },
            else => try writer.writeAll("void"),
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
        if (a.type != b.type) return false;
        return switch (a.type) {
            .int => |i| @fabs(n - b.type.number) < 0.000001,
            .bool => |bl| bl == b.type.bool,
            .nil => b.type == .nil,
            .string => |s| std.mem.eql(u8, s, b.type.string),
            .list => |l| {
                const l_b = b.type.list;

                if (l.items.len != l_b.items.len) return false;
                for (l.items, 0..) |item, i| {
                    if (!item.eql(l_b.items[i])) return false;
                }
                return true;
            },
            .map => |m| {
                const map_b = b.type.map;

                if (m.items().len != map_b.items().len) return false;
                for (a.items(), 0..) |entry, i| {
                    if (entry.hash != map_b.items()[i].hash) return false;
                }
                return true;
            },
            .range => |r| {
                const range_b = b.type.range;
                return r.start == range_b.start and r.end == range_b.end;
            },
            else => unreachable,
        };
    }
};
