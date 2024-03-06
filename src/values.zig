const std = @import("std");
const ast = @import("./ast.zig");
const Gc = @import("./gc.zig").Gc;
const UUID = @import("./utils/uuid.zig").UUID;
const Bytecode = @import("./bytecode.zig").Bytecode;
const Builtin = @import("./builtins.zig").Builtin;
const OpCode = @import("./opcode.zig").OpCode;
const Enum = @import("./enum.zig").Enum;
const Class = @import("./class.zig").Class;
const ExportFunction = @import("./export.zig").ExportFunction;

const ID = UUID.ID;
const Allocator = std.mem.Allocator;

pub const True = Value{ .bool = true };
pub const False = Value{ .bool = false };
pub const Nil = Value{ .nil = {} };
pub const Void = Value{ .void = {} };

pub const Type = enum(u8) {
    void,
    nil,
    bool,
    number,
    range,
    obj,
    map_pair,
    visit,
    enum_value,
};

pub const Iterator = struct {
    value: Value,
    index: usize,
};

pub const ExportFunctionDelegate = *const fn (args: []Value) Value;

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
    map_pair: struct {
        key: *Value,
        value: *Value,
    },
    visit: u32,
    enum_value: Enum.Value,

    pub const Obj = struct {
        is_marked: bool = false,
        // used for serializing references
        id: ID = UUID.Empty,
        next: ?*Obj = null,
        data: Data,

        pub const DataType = enum(u8) {
            string,
            @"enum",
            list,
            map,
            set,
            function,
            ext_function,
            builtin,
            closure,
            class,
            instance,
        };

        pub const Data = union(DataType) {
            string: []const u8,
            @"enum": Enum,
            list: std.ArrayList(Value),
            map: MapType,
            set: SetType,
            function: struct {
                arity: u8,
                instructions: []const u8,
                lines: []const u32,
                locals_count: usize,
                is_method: bool = false,
            },
            ext_function: struct {
                arity: u8,
                context_ptr: usize,
                backing: *const fn (context_ptr: usize, args: []Value) Value,
            },
            builtin: struct {
                arity: u8,
                backing: Builtin,
                is_method: bool,
                name: []const u8,
            },
            closure: struct {
                data: *Data,
                free_values: []Value,
            },
            class: Class,
            instance: Class.Instance,
        };

        pub const MapType = std.ArrayHashMap(Value, Value, Adapter, true);
        pub const SetType = std.ArrayHashMap(Value, void, Adapter, true);

        pub fn toValue(self: *Obj) *Value {
            return @fieldParentPtr(Value, "obj", self);
        }

        pub fn destroy(allocator: std.mem.Allocator, obj: *Obj) void {
            switch (obj.data) {
                .string => |s| allocator.free(s),
                .@"enum" => |e| {
                    allocator.free(e.name);
                    for (e.values) |val| allocator.free(val);
                    allocator.free(e.values);
                },
                .list => |l| l.deinit(),
                .map => obj.data.map.deinit(),
                .set => obj.data.set.deinit(),
                .function => |f| {
                    allocator.free(f.instructions);
                    allocator.free(f.lines);
                },
                .ext_function => |e| {
                    const func: *ExportFunction = @ptrFromInt(e.context_ptr);
                    allocator.destroy(func);
                },
                .builtin => {},
                .closure => |c| allocator.free(c.free_values),
                .class => |c| c.deinit(),
                .instance => obj.data.instance.fields.deinit(),
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

    pub fn typeName(self: Value) []const u8 {
        return switch (self) {
            .void => "void",
            .nil => "nil",
            .bool => "bool",
            .number => "number",
            .range => "range",
            .map_pair => "map_pair",
            .visit => "visit",
            .enum_value => "enum_value",
            .obj => |o| switch (o.data) {
                .string => "string",
                .list => "list",
                .set => "set",
                .map => "map",
                .closure => "closure",
                .function => "function",
                .ext_function => "extern_function",
                .class => "class",
                .instance => "instance",
                .@"enum" => "enum",
                .builtin => "builtin",
            },
        };
    }

    pub fn len(self: Value) usize {
        return switch (self) {
            .range => |r| @as(usize, @intCast(@abs(r.end - r.start))) + 1,
            .obj => |o| switch (o.data) {
                .string => |s| s.len,
                .list => |l| l.items.len,
                .map => |m| m.count(),
                .set => |s| s.count(),
                else => unreachable,
            },
            else => unreachable,
        };
    }

    pub fn getAtIndex(self: Value, index: usize) Value {
        return switch (self) {
            .range => |r| .{ .number = @as(f32, @floatFromInt(r.start + @as(i32, @intCast(index)))) },
            .obj => |o| switch (o.data) {
                .list => |l| l.items[index],
                .map => |m| .{
                    .map_pair = .{
                        .key = &m.keys()[index],
                        .value = &m.values()[index],
                    },
                },
                .set => |s| s.keys()[index],
                else => Nil,
            },
            else => Nil,
        };
    }

    pub fn createFrom(comptime T: type, value: T) Value {
        return switch (T) {
            bool => if (value) True else False,
            @TypeOf(null) => Nil,
            f32 => .{ .number = value },
            else => unreachable,
        };
    }

    pub fn isTruthy(self: Value) !bool {
        return switch (self) {
            .bool => |b| b,
            .nil => false,
            .number => |n| @abs(n) > 0.00001,
            .visit => |v| v != 0,
            else => return error.InvalidType,
        };
    }

    pub fn serialize(self: Value, writer: anytype) !void {
        try writer.writeByte(@intFromEnum(@as(Type, self)));
        switch (self) {
            .bool => |b| try writer.writeByte(if (b) '1' else '0'),
            .number => |n| {
                var buf: [128]u8 = undefined;
                var buf_stream = std.io.fixedBufferStream(&buf);
                try std.fmt.formatFloatDecimal(n, .{
                    .precision = 5,
                }, buf_stream.writer());
                try writer.writeInt(u16, @as(u16, @intCast(buf_stream.pos)), .little);
                try writer.writeAll(buf[0..buf_stream.pos]);
            },
            .range => |r| {
                try writer.writeInt(i32, r.start, .little);
                try writer.writeInt(i32, r.end, .little);
            },
            .map_pair => |mp| {
                try serialize(mp.key.*, writer);
                try serialize(mp.value.*, writer);
            },
            .visit => |v| {
                try writer.writeInt(u32, v, .little);
            },
            .enum_value => |e| {
                try writer.writeByte(e.index);
                try writer.writeByte(@intCast(e.base.name.len));
                try writer.writeAll(e.base.name);
            },
            .obj => |o| {
                try writer.writeByte(@intFromEnum(@as(Obj.DataType, o.data)));
                try writer.writeAll(&o.id);
                switch (o.data) {
                    .string => |s| {
                        try writer.writeInt(u16, @as(u16, @intCast(s.len)), .little);
                        try writer.writeAll(s);
                    },
                    .list => |l| {
                        try writer.writeInt(u16, @as(u16, @intCast(l.items.len)), .little);
                        for (l.items) |i| try serialize(i, writer);
                    },
                    .map => |m| {
                        try writer.writeInt(u16, @as(u16, @intCast(m.count())), .little);
                        for (m.keys()) |k| {
                            try serialize(k, writer);
                            try serialize(m.get(k) orelse Nil, writer);
                        }
                    },
                    .set => |s| {
                        try writer.writeInt(u16, @as(u16, @intCast(s.count())), .little);
                        for (s.keys()) |k| {
                            try serialize(k, writer);
                        }
                    },
                    .function => |f| {
                        try writer.writeByte(f.arity);
                        try writer.writeByte(if (f.is_method) 1 else 0);
                        try writer.writeInt(u16, @as(u16, @intCast(f.locals_count)), .little);
                        try writer.writeInt(u16, @as(u16, @intCast(f.instructions.len)), .little);
                        try writer.writeAll(f.instructions);
                        try writer.writeInt(u16, @as(u16, @intCast(f.lines.len)), .little);
                        for (f.lines) |l| try writer.writeInt(u32, l, .little);
                    },
                    .instance => |i| {
                        _ = i;
                    },
                    else => {},
                }
            },
            .nil, .void => {},
        }
    }

    pub fn deserialize(reader: anytype, allocator: std.mem.Allocator) !Value {
        const value_type: Type = @enumFromInt(try reader.readByte());
        return switch (value_type) {
            .nil => Nil,
            .bool => if (try reader.readByte() == '1') True else False,
            .number => {
                const length = try reader.readInt(u16, .little);
                const buf = try allocator.alloc(u8, length);
                defer allocator.free(buf);
                try reader.readNoEof(buf);
                return .{ .number = try std.fmt.parseFloat(f32, buf) };
            },
            .visit => {
                return .{ .visit = try reader.readInt(u32, .little) };
            },
            .enum_value => {
                const index = try reader.readByte();
                return .{ .enum_value = .{ .index = index, .base = undefined } };
            },
            .obj => {
                const data_type: Obj.DataType = @enumFromInt(try reader.readByte());
                var id: ID = undefined;
                try reader.readNoEof(id[0..]);
                switch (data_type) {
                    .string => {
                        const length = try reader.readInt(u16, .little);
                        const buf = try allocator.alloc(u8, length);
                        try reader.readNoEof(buf);
                        const obj = try allocator.create(Value.Obj);
                        obj.* = .{ .id = id, .data = .{ .string = buf } };
                        return .{ .obj = obj };
                    },
                    .list => {
                        const length = try reader.readInt(u16, .little);
                        var list = try std.ArrayList(Value).initCapacity(allocator, length);
                        var i: usize = 0;
                        while (i < length) : (i += 1) {
                            list.items[i] = try Value.deserialize(reader, allocator);
                        }
                        const obj = try allocator.create(Value.Obj);
                        obj.* = .{ .id = id, .data = .{ .list = list } };
                        return .{ .obj = obj };
                    },
                    .map => {
                        const length = try reader.readInt(u16, .little);
                        var map = Value.Obj.MapType.initContext(allocator, adapter);
                        var i: usize = 0;
                        while (i < length) : (i += 1) {
                            const key = try Value.deserialize(reader, allocator);
                            const value = try Value.deserialize(reader, allocator);
                            try map.put(key, value);
                        }
                        const obj = try allocator.create(Value.Obj);
                        obj.* = .{ .id = id, .data = .{ .map = map } };
                        return .{ .obj = obj };
                    },
                    .set => {
                        const length = try reader.readInt(u16, .little);
                        var set = Value.Obj.SetType.initContext(allocator, adapter);
                        var i: usize = 0;
                        while (i < length) : (i += 1) {
                            const key = try Value.deserialize(reader, allocator);
                            try set.put(key, {});
                        }
                        const obj = try allocator.create(Value.Obj);
                        obj.* = .{ .id = id, .data = .{ .set = set } };
                        return .{ .obj = obj };
                    },
                    .function => {
                        const arity = try reader.readByte();
                        const is_method = if (try reader.readByte() == 1) true else false;
                        const locals_count = try reader.readInt(u16, .little);
                        const instructions_count = try reader.readInt(u16, .little);
                        const buf = try allocator.alloc(u8, instructions_count);
                        try reader.readNoEof(buf);
                        const lines_count = try reader.readInt(u16, .little);
                        var lines = try allocator.alloc(u32, lines_count);
                        for (0..lines_count) |i| {
                            lines[i] = try reader.readInt(u32, .little);
                        }
                        const obj = try allocator.create(Value.Obj);
                        obj.* = .{ .id = id, .data = .{
                            .function = .{
                                .arity = arity,
                                .is_method = is_method,
                                .locals_count = locals_count,
                                .instructions = buf,
                                .lines = lines,
                            },
                        } };
                        return .{ .obj = obj };
                    },
                    .@"enum" => {
                        const name_length = try reader.readByte();
                        const name_buf = try allocator.alloc(u8, name_length);
                        try reader.readNoEof(name_buf);
                        const values_length = try reader.readByte();
                        const obj = try allocator.create(Value.Obj);
                        obj.* = .{ .data = .{ .@"enum" = .{ .allocator = allocator, .name = name_buf, .values = try allocator.alloc([]const u8, values_length) } } };
                        for (0..values_length) |i| {
                            const value_name_length = try reader.readByte();
                            const value_name_buf = try allocator.alloc(u8, value_name_length);
                            try reader.readNoEof(value_name_buf);
                            obj.data.@"enum".values[i] = value_name_buf;
                        }
                        return .{ .obj = obj };
                    },
                    else => return error.Unknown,
                }
            },
            else => return error.Unknown,
        };
    }

    pub fn print(self: Value, writer: anytype, constants: ?[]Value) void {
        switch (self) {
            .number => |n| writer.print("{d:.5}", .{n}),
            .bool => |b| writer.print("{}", .{b}),
            .nil => writer.print("nil", .{}),
            .visit => |v| writer.print("{d}", .{v}),
            .enum_value => |e| writer.print("{s}.{s}", .{ e.base.name, e.base.values[e.index] }),
            .obj => |o| {
                switch (o.data) {
                    .string => |s| writer.print("{s}", .{s}),
                    .list => |l| {
                        writer.print("List{{", .{});
                        for (l.items, 0..) |item, i| {
                            item.print(writer, constants);
                            if (i != l.items.len - 1)
                                writer.print(", ", .{});
                        }
                        writer.print("}}", .{});
                    },
                    .map => |m| {
                        writer.print("Map{{", .{});
                        const keys = m.keys();
                        for (keys, 0..) |k, i| {
                            k.print(writer, constants);
                            writer.print(":", .{});
                            m.get(k).?.print(writer, constants);
                            if (i != keys.len - 1)
                                writer.print(", ", .{});
                        }
                        writer.print("}}", .{});
                    },
                    .set => |s| {
                        const keys = s.keys();
                        writer.print("Set{{", .{});
                        for (keys, 0..) |k, i| {
                            k.print(writer, constants);
                            if (i != keys.len - 1)
                                writer.print(", ", .{});
                        }
                        writer.print("}}", .{});
                    },
                    .function => |f| {
                        writer.print("\nfn---\n", .{});
                        Bytecode.printInstructions(writer, f.instructions, constants);
                        writer.print("---", .{});
                    },
                    .closure => |c| {
                        writer.print("\ncl---\n", .{});
                        Bytecode.printInstructions(writer, c.data.function.instructions, constants);
                        writer.print("---", .{});
                    },
                    .class => |c| {
                        writer.print("{s}", .{c.name});
                    },
                    .instance => |i| {
                        writer.print("{s}.instance", .{i.class.name});
                        writer.print(" {{\n", .{});
                        var it = i.fields.keyIterator();
                        while (it.next()) |key| {
                            writer.print("    {s}: ", .{key.*});
                            i.fields.get(key.*).?.print(writer, constants);
                            writer.print("\n", .{});
                        }
                        writer.print("}}", .{});
                    },
                    .@"enum" => |e| {
                        writer.print("{s}{{", .{e.name});
                        for (e.values, 0..) |val, i| {
                            writer.print("{s}", .{val});
                            if (i != e.values.len - 1)
                                writer.print(", ", .{});
                        }
                        writer.print("}}", .{});
                    },
                    .builtin => |b| {
                        writer.print("builtin {s}", .{b.name});
                    },
                    else => {
                        writer.print("{s}", .{@tagName(o.data)});
                    },
                }
            },
            .range => |r| writer.print("{}..{}", .{ r.start, r.end }),
            // .@"enum" => |e| {
            //     writer.print("{", .{});
            //     for (e, 0..) |item, i| {
            //         writer.print("{s}", .{item});
            //         if (i != e.len - 1)
            //             writer.print(",\n", .{});
            //     }
            //     writer.print("}\n", .{});
            // },
            else => writer.print("{s}", .{@tagName(self)}),
        }
    }

    pub const Adapter = struct {
        pub fn hash(_: @This(), v: Value) u32 {
            const hashFn = std.hash.autoHash;
            var hasher = std.hash.Wyhash.init(0);

            switch (v) {
                .number => |n| hashFn(&hasher, @as(u32, @intFromFloat(n * 10000.0))),
                .bool => |b| hashFn(&hasher, b),
                .visit => |visit| hashFn(&hasher, visit),
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
            return @as(u32, @truncate(hasher.final()));
        }

        pub fn lessThan(self: @This(), a_index: usize, b_index: usize) bool {
            _ = self;
            return b_index < a_index;
        }

        pub fn eql(self: @This(), a: Value, b: Value, _: usize) bool {
            _ = self;
            if (@intFromEnum(a) != @intFromEnum(b)) return false;
            return switch (a) {
                .number => |n| @abs(n - b.number) < 0.00001,
                .bool => |bl| bl == b.bool,
                .nil => b == .nil,
                .visit => |v| v == b.visit,
                .enum_value => |e| e.base == b.enum_value.base and e.index == b.enum_value.index,
                .obj => |o| {
                    const b_data = b.obj.data;
                    if (@intFromEnum(o.data) != @intFromEnum(b_data))
                        return false;
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
                        .map => |m| {
                            const a_keys = m.keys();
                            const b_keys = b_data.map.keys();
                            if (a_keys.len != b_keys.len) return false;
                            for (a_keys) |a_key| {
                                if (!b_data.map.contains(a_key)) return false;
                                const a_value = m.get(a_key);
                                const b_value = b_data.map.get(a_key);
                                if (a_value == null and b_value != null) return false;
                                if (a_value != null and b_value == null) return false;
                                if (!adapter.eql(a_value.?, b_value.?, 0)) return false;
                            }
                            return true;
                        },
                        .set => |s| {
                            const a_keys = s.keys();
                            const b_keys = b_data.map.keys();
                            if (a_keys.len != b_keys.len) return false;
                            for (a_keys) |a_key| {
                                if (!b_data.map.contains(a_key)) return false;
                            }
                            return true;
                        },
                        .function => |f| {
                            const b_f = b_data.function;
                            const inst = std.mem.eql(u8, f.instructions, b_f.instructions);
                            return inst and f.locals_count == b_f.locals_count and f.arity == b_f.arity and f.is_method == b_f.is_method;
                        },
                        else => return false,
                    };
                },
                .range => |r| {
                    const range_b = b.range;
                    return r.start == range_b.start and r.end == range_b.end;
                },
                else => unreachable,
            };
        }
    };
};

test "Serialize" {
    const alloc = std.testing.allocator;
    var data = std.ArrayList(u8).init(alloc);
    defer data.deinit();

    var value = Value{ .number = 15 };
    try value.serialize(data.writer());

    try std.testing.expectEqualSlices(
        u8,
        &[_]u8{ 0x03, 0x08, 0x00, 0x31, 0x35, 0x2e, 0x30, 0x30, 0x30, 0x30, 0x30 },
        data.items,
    );
}
