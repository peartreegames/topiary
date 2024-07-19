const std = @import("std");

const runtime = @import("../runtime/index.zig");
const Gc = runtime.Gc;
const Vm = runtime.Vm;
const Builtin = runtime.Builtin;

const utils = @import("../utils/index.zig");
const UUID = utils.UUID;
const C = utils.C;

const backend = @import("../backend/index.zig");
const Bytecode = backend.Bytecode;
const DebugInfo = backend.DebugInfo;

const Enum = @import("enum.zig").Enum;
const Class = @import("class.zig").Class;
const Allocator = std.mem.Allocator;

pub const True = Value{ .bool = true };
pub const False = Value{ .bool = false };
pub const Nil = Value{ .nil = {} };
pub const Void = Value{ .void = {} };

pub const OnValueChanged = *const fn (vm: *Vm, []const u8, value: Value) void;
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
    ref,
};

pub const Iterator = struct {
    value: Value,
    index: usize,
};

pub const Value = union(Type) {
    void: void,
    nil: void,
    bool: bool,
    number: f64,
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
    enum_value: Enum.Val,
    ref: UUID.ID,

    pub const adapter = Value.Adapter{};

    /// Allocated Value
    pub const Obj = struct {
        is_marked: bool = false,
        next: ?*Obj = null,
        // used for serializing references
        id: UUID.ID = UUID.Empty,
        index: ?C.GLOBAL = null,
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
                locals_count: usize,
                is_method: bool = false,
                debug_info: []DebugInfo,
            },
            ext_function: struct {
                arity: u8,
                context_ptr: usize,
                backing: *const fn (context_ptr: usize, args: []Value) Value,
                destroy: *const fn (context_ptr: usize, allocator: std.mem.Allocator) void,
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
                    for (f.debug_info) |d| d.deinit(allocator);
                    allocator.free(f.debug_info);
                },
                .ext_function => |e| {
                    e.destroy(e.context_ptr, allocator);
                },
                .builtin => {},
                .closure => |c| allocator.free(c.free_values),
                .class => |c| c.deinit(),
                .instance => {
                    allocator.free(obj.data.instance.fields);
                },
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
            .ref => "reference",
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
            .range => |r| .{ .number = @as(f64, @floatFromInt(r.start + @as(i64, @intCast(index)))) },
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
            f64 => .{ .number = value },
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

    // used for constant values only
    pub fn serialize(self: Value, writer: anytype) !void {
        try writer.writeByte(@intFromEnum(@as(Type, self)));
        switch (self) {
            .bool => |b| try writer.writeByte(if (b) '1' else '0'),
            .number => |n| {
                try writer.print("{d:.5}", .{n});
                try writer.writeByte(0);
            },
            .visit => |v| {
                try writer.writeInt(u32, v, .little);
            },
            .obj => |o| {
                try writer.writeByte(@intFromEnum(@as(Obj.DataType, o.data)));
                try writer.writeAll(&o.id);
                switch (o.data) {
                    .string => |s| {
                        try writer.writeInt(u16, @as(u16, @intCast(s.len)), .little);
                        try writer.writeAll(s);
                    },
                    .function => |f| {
                        try writer.writeByte(f.arity);
                        try writer.writeByte(if (f.is_method) 1 else 0);
                        try writer.writeInt(u16, @as(u16, @intCast(f.locals_count)), .little);
                        try writer.writeInt(u16, @as(u16, @intCast(f.instructions.len)), .little);
                        try writer.writeAll(f.instructions);
                        try writer.writeInt(u32, @intCast(f.debug_info.len), .little);
                        for (f.debug_info) |d| try d.serialize(writer);
                    },
                    .@"enum" => |e| {
                        try writer.writeByte(@intCast(e.name.len));
                        try writer.writeAll(e.name);
                        try writer.writeByte(if (e.is_seq) 1 else 0);
                        try writer.writeByte(@intCast(e.values.len));
                        for (e.values) |value| {
                            try writer.writeByte(@intCast(value.len));
                            try writer.writeAll(value);
                        }
                    },
                    else => return error.InvalidConstant,
                }
            },
            .nil, .void => {},
            else => return error.InvalidConstant,
        }
    }

    // used for constant values only
    pub fn deserialize(reader: anytype, allocator: std.mem.Allocator) !Value {
        const value_type: Type = @enumFromInt(try reader.readByte());
        return switch (value_type) {
            .nil => Nil,
            .bool => if (try reader.readByte() == '1') True else False,
            .number => {
                const val = try reader.readUntilDelimiterAlloc(allocator, 0, 128);
                defer allocator.free(val);
                return .{ .number = try std.fmt.parseFloat(f64, val) };
            },
            .visit => {
                return .{ .visit = try reader.readInt(u32, .little) };
            },
            .obj => {
                const data_type: Obj.DataType = @enumFromInt(try reader.readByte());
                var id: UUID.ID = undefined;
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
                    .function => {
                        const arity = try reader.readByte();
                        const is_method = if ((try reader.readByte()) == 1) true else false;
                        const locals_count = try reader.readInt(u16, .little);
                        const instructions_count = try reader.readInt(u16, .little);
                        const buf = try allocator.alloc(u8, instructions_count);
                        try reader.readNoEof(buf);
                        const debug_info_count = try reader.readInt(u32, .little);
                        var debug_info = try allocator.alloc(DebugInfo, debug_info_count);
                        for (0..debug_info_count) |i| {
                            debug_info[i] = try DebugInfo.deserialize(reader, allocator);
                        }
                        const obj = try allocator.create(Value.Obj);
                        obj.* = .{ .id = id, .data = .{
                            .function = .{
                                .arity = arity,
                                .is_method = is_method,
                                .locals_count = locals_count,
                                .instructions = buf,
                                .debug_info = debug_info,
                            },
                        } };
                        return .{ .obj = obj };
                    },
                    .@"enum" => {
                        const name_length = try reader.readByte();
                        const name_buf = try allocator.alloc(u8, name_length);
                        try reader.readNoEof(name_buf);
                        const is_seq = try reader.readByte() == 1;
                        const values_length = try reader.readByte();
                        const obj = try allocator.create(Value.Obj);
                        obj.* = .{ .data = .{ .@"enum" = .{ .name = name_buf, .values = try allocator.alloc([]const u8, values_length), .is_seq = is_seq } } };
                        for (0..values_length) |i| {
                            const value_name_length = try reader.readByte();
                            const value_name_buf = try allocator.alloc(u8, value_name_length);
                            try reader.readNoEof(value_name_buf);
                            obj.data.@"enum".values[i] = value_name_buf;
                        }
                        return .{ .obj = obj };
                    },
                    else => return error.InvalidConstant,
                }
            },
            else => return error.InvalidConstant,
        };
    }

    pub fn print(self: Value, writer: anytype, constants: ?[]Value) void {
        switch (self) {
            .number => |n| writer.print("{d:.5}", .{n}),
            .bool => |b| writer.print("{}", .{b}),
            .nil => writer.print("nil", .{}),
            .visit => |v| writer.print("{d}", .{v}),
            .enum_value => |e| writer.print("{s}.{s}", .{ e.base.data.@"enum".name, e.base.data.@"enum".values[e.index] }),
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
                        writer.print("{s}.instance", .{i.base.data.class.name});
                        writer.print(" {{\n", .{});
                        for (i.fields, 0..) |v, idx| {
                            writer.print("    {s}: ", .{i.base.data.class.fields[idx].name});
                            v.print(writer, constants);
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
                        else => hashFn(&hasher, o.id),
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
                        else => std.mem.eql(u8, &o.id, &b.obj.id),
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
        &[_]u8{ 0x03, 0x31, 0x35, 0x2e, 0x30, 0x30, 0x30, 0x30, 0x30, 0x00 },
        data.items,
    );
}
