const std = @import("std");
const ast = @import("./ast.zig");
const Gc = @import("./gc.zig").Gc;
const ByteCode = @import("./compiler.zig").ByteCode;
const Builtin = @import("./builtins.zig").Builtin;
const Allocator = std.mem.Allocator;

pub const True = Value{ .bool = true };
pub const False = Value{ .bool = false };
pub const Nil = Value.nil;
pub const Void = Value.void;

pub const Type = enum(u8) {
    void,
    nil,
    bool,
    number,
    range,
    obj,
};

pub const adapter = Value.Adapter{};

pub const Value = union(Type) {
    void: void,
    nil: void,
    bool: bool,
    number: f32,
    range: struct {
        start: i32,
        end: i32,
    },
    obj: *Obj,

    pub const Obj = struct {
        is_marked: bool = false,
        next: ?*Obj = null,
        data: Data,

        pub const Data = union(enum(u4)) {
            string: []const u8,
            @"enum": []const u8,
            list: std.ArrayList(Value),
            map: MapType,
            set: SetType,
            function: struct {
                arity: u8,
                instructions: []const u8,
                locals_count: usize,
            },
            builtin: struct {
                arity: u8,
                backing: Builtin,
            },
            closure: struct {
                data: *Data,
                free_values: []Value,
            },
        };
        pub const MapType = std.ArrayHashMap(Value, Value, Adapter, true);
        pub const SetType = std.ArrayHashMap(Value, void, Adapter, true);

        pub fn add(self: *Data, value: Value) !void {
            switch (self) {
                .list => |l| try l.append(value),
                .set => |s| try s.putNoClobber(value, void),
                else => return error.RuntimeError,
            }
        }

        pub fn addMap(self: *Data, key: Value, value: Value) !void {
            switch (self) {
                .map => |m| try m.putNoClobber(key, value),
                else => return error.RuntimeError,
            }
        }

        pub fn remove(self: *Data, key: Value) !void {
            switch (self) {
                .list => |l| _ = l.orderedRemove(key),
                .set => |s| _ = s.orderedRemove(key),
                .map => |m| _ = m.orderedRemove(key),
                else => return error.RuntimeError,
            }
        }

        pub fn toValue(self: *Obj) *Value {
            return @fieldParentPtr(Value, "obj", self);
        }

        pub fn destroy(allocator: std.mem.Allocator, obj: *Obj) void {
            switch (obj.data) {
                .string => |s| allocator.free(s),
                .@"enum" => |e| allocator.free(e),
                .list => |l| l.deinit(),
                .map => obj.data.map.deinit(),
                .set => obj.data.set.deinit(),
                .function => |f| allocator.free(f.instructions),
                .builtin => {},
                .closure => |c| allocator.free(c.free_values),
            }
            allocator.destroy(obj);
        }
    };

    pub fn is(self: Value, tag_type: Type) bool {
        return self.tag() == tag_type;
    }

    pub fn tag(self: Value) Type {
        return @as(Type, self);
    }

    pub fn eql(a: Value, b: Value) bool {
        return adapter.eql(a, b, 0);
    }

    pub fn isTruthy(self: Value) !bool {
        return switch (self) {
            .bool => |b| b,
            .nil => false,
            else => return error.InvalidType,
        };
    }

    pub fn print(self: Value, writer: anytype) void {
        switch (self) {
            .number => |n| writer.print("{d}", .{n}),
            .bool => |b| writer.print("{}", .{b}),
            .nil => writer.print("nil", .{}),
            .obj => |o| {
                switch (o.data) {
                    .string => |s| writer.print("{s}", .{s}),
                    .list => |l| {
                        writer.print("[", .{});
                        for (l.items, 0..) |item, i| {
                            item.print(writer);
                            if (i != l.items.len - 1)
                                writer.print(", ", .{});
                        }
                        writer.print("]", .{});
                    },
                    .map => |m| {
                        writer.print("{{", .{});
                        var keys = m.keys();
                        for (keys, 0..) |k, i| {
                            k.print(writer);
                            writer.print(":", .{});
                            m.get(k).?.print(writer);
                            if (i != keys.len - 1)
                                writer.print(", ", .{});
                        }
                        writer.print("}}", .{});
                    },
                    .set => |s| {
                        var keys = s.keys();
                        writer.print("{{", .{});
                        for (keys, 0..) |k, i| {
                            k.print(writer);
                            if (i != keys.len - 1)
                                writer.print(", ", .{});
                        }
                        writer.print("}}", .{});
                    },
                    .function => |f| {
                        ByteCode.printInstructions(writer, f.instructions);
                    },
                    .closure => |c| {
                        ByteCode.printInstructions(writer, c.data.function.instructions);
                    },
                    else => {},
                }
            },
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

    pub const Adapter = struct {
        pub fn hash(_: @This(), v: Value) u32 {
            const hashFn = std.hash.autoHash;
            var hasher = std.hash.Wyhash.init(0);

            switch (v) {
                .number => |n| hashFn(&hasher, @intFromFloat(u32, n * 10000.0)),
                .bool => |b| hashFn(&hasher, b),
                .obj => |o| {
                    switch (o.data) {
                        .string => |s| hasher.update(s),
                        .function => |f| {
                            hashFn(&hasher, f.locals_count);
                            hashFn(&hasher, f.instructions.len);
                            hashFn(&hasher, f.instructions.ptr);
                        },
                        .list => |l| {
                            hashFn(&hasher, l.items.len);
                            hashFn(&hasher, l.items.ptr);
                        },
                        .map => |m| {
                            hashFn(&hasher, m.keys().len);
                            hashFn(&hasher, m.keys().ptr);
                        },
                        .builtin => |b| {
                            hashFn(&hasher, b.arity);
                            hashFn(&hasher, b.backing);
                        },
                        else => return 0,
                    }
                },
                .range => |r| {
                    hashFn(&hasher, r.start);
                    hashFn(&hasher, r.end);
                },
                else => unreachable,
            }
            return @truncate(u32, hasher.final());
        }

        pub fn lessThan(self: @This(), a_index: usize, b_index: usize) bool {
            _ = self;
            return b_index < a_index;
        }

        pub fn eql(self: @This(), a: Value, b: Value, _: usize) bool {
            _ = self;
            if (@intFromEnum(a) != @intFromEnum(b)) return false;
            return switch (a) {
                .number => |n| @fabs(n - b.number) < 0.00001,
                .bool => |bl| bl == b.bool,
                .nil => b == .nil,
                .obj => |o| {
                    const b_data = b.obj.*.data;
                    return switch (o.data) {
                        .string => |s| std.mem.eql(u8, s, b_data.string),
                        .list => |l| {
                            const l_b = b_data.list;

                            if (l.items.len != l_b.items.len) return false;
                            for (l.items, 0..) |item, i| {
                                if (!adapter.eql(item, l_b.items[i], 0)) return false;
                            }
                            return true;
                        },
                        else => return false,
                    };
                },
                // .map => |m| {
                //     const map_b = b.map;

                //     if (m.items().len != map_b.items().len) return false;
                //     for (a.items(), 0..) |entry, i| {
                //         if (entry.hash != map_b.items()[i].hash) return false;
                //     }
                //     return true;
                // },
                .range => |r| {
                    const range_b = b.range;
                    return r.start == range_b.start and r.end == range_b.end;
                },
                else => unreachable,
            };
        }
    };
};

// TODO: Decide if this is needed,
// if not remove
pub const String = struct {
    data: []const u8,

    pub fn create(allocator: std.mem.Allocator, value: []const u8) !*String {
        const str = try allocator.create(String);
        str.* = .{
            .data = try allocator.dupe(u8, value),
        };
        return str;
    }

    pub fn destroy(self: *String, allocator: std.mem.Allocator) void {
        allocator.free(self.data);
        allocator.destroy(self);
    }
};
