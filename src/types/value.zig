const std = @import("std");

const runtime = @import("../runtime/index.zig");
const Gc = runtime.Gc;
const Vm = runtime.Vm;
const Builtin = runtime.Builtin;
const runtime_builtins = runtime.builtins;

const utils = @import("../utils/index.zig");
const UUID = utils.UUID;
const C = utils.C;

const backend = @import("../backend/index.zig");
const Bytecode = backend.Bytecode;
const DebugInfo = backend.DebugInfo;

const Enum = @import("enum.zig").Enum;
const Class = @import("class.zig").Class;
const Function = @import("function.zig").Function;
const Anchor = @import("anchor.zig").Anchor;
const Extern = @import("extern.zig").Extern;

const Allocator = std.mem.Allocator;

pub const True = Value{ .bool = true };
pub const False = Value{ .bool = false };
pub const Nil = Value{ .nil = {} };
pub const Void = Value{ .void = {} };
pub const Zero = Value{ .number = 0 };
pub const One = Value{ .number = 1 };

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
    timestamp,
    const_string,
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
    enum_value: Enum.Val,
    timestamp: i64,
    const_string: []const u8,
    ref: UUID.ID,

    pub const adapter = Value.Adapter{};

    /// Allocated Value
    pub const Obj = struct {
        // used for serializing references
        id: UUID.ID = UUID.Empty,
        data: Data,

        pub const DataType = enum(u8) {
            string,
            @"enum",
            list,
            map,
            set,
            function,
            @"extern",
            builtin,
            class,
            instance,
            anchor,
        };

        pub const Data = union(DataType) {
            string: []const u8,
            @"enum": Enum,
            list: std.ArrayList(Value),
            map: MapType,
            set: SetType,
            function: Function,
            @"extern": Extern,
            builtin: Builtin,
            class: Class,
            instance: Class.Instance,
            anchor: Anchor,
        };

        pub const MapType = std.ArrayHashMapUnmanaged(Value, Value, Adapter, true);
        pub const SetType = std.ArrayHashMapUnmanaged(Value, void, Adapter, true);

        pub fn deinit(obj: *Obj, allocator: std.mem.Allocator) void {
            switch (obj.data) {
                .anchor => |a| allocator.free(a.name),
                .string => |s| allocator.free(s),
                .@"enum" => |e| e.deinit(allocator),
                .list => obj.data.list.deinit(allocator),
                .map => obj.data.map.deinit(allocator),
                .set => obj.data.set.deinit(allocator),
                .function => |f| f.deinit(allocator),
                .@"extern" => |e| if (e.context_ptr) |ptr| e.destroy(ptr, allocator),
                .builtin => {},
                .class => |c| c.deinit(allocator),
                .instance => {
                    allocator.free(obj.data.instance.fields);
                },
            }
        }
    };

    pub fn destroy(self: Value, alloc: std.mem.Allocator) void {
        switch (self) {
            .obj => |o| {
                o.deinit(alloc);
                alloc.destroy(o);
            },
            else => {},
        }
    }

    /// Recursive destroy for compile-time-owned values. Unlike `destroy`,
    /// this walks list/set/map containers and destroys their element
    /// Values before freeing the container storage. Intended only for
    /// values produced by the compiler's static `evaluateLiteral` path,
    /// where no GC exists to separately own inner `Obj`s.
    pub fn destroyStatic(self: Value, alloc: std.mem.Allocator) void {
        switch (self) {
            .obj => |o| {
                switch (o.data) {
                    .list => |*l| {
                        for (l.items) |item| item.destroyStatic(alloc);
                    },
                    .set => |*s| {
                        for (s.keys()) |k| k.destroyStatic(alloc);
                    },
                    .map => |*m| {
                        var it = m.iterator();
                        while (it.next()) |entry| {
                            entry.key_ptr.*.destroyStatic(alloc);
                            entry.value_ptr.*.destroyStatic(alloc);
                        }
                    },
                    else => {},
                }
                o.deinit(alloc);
                alloc.destroy(o);
            },
            else => {},
        }
    }

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
            .timestamp => "timestamp",
            .const_string => "const_string",
            .obj => |o| switch (o.data) {
                .anchor => "anchor",
                .string => "string",
                .list => "list",
                .set => "set",
                .map => "map",
                .function => "function",
                .@"extern" => "extern_function",
                .class => "class",
                .instance => "instance",
                .@"enum" => "enum",
                .builtin => "builtin",
            },
        };
    }

    pub fn len(self: Value) usize {
        return switch (self) {
            .const_string => |s| s.len,
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

    pub fn asString(self: Value) ?[]const u8 {
        return switch (self) {
            .const_string => |s| s,
            .obj => |o| if (o.data == .string) o.data.string else null,
            else => null,
        };
    }

    pub fn getAtIndex(self: Value, index: usize) Value {
        return switch (self) {
            .range => |r| .{ .number = @as(f32, @floatFromInt(r.start + @as(i64, @intCast(index)))) },
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
            i64 => .{ .timestamp = value },
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
    pub fn serialize(self: Value, writer: *std.Io.Writer) !void {
        try writer.writeByte(@intFromEnum(@as(Type, self)));
        switch (self) {
            .bool => |b| try writer.writeByte(if (b) '1' else '0'),
            .number => |n| {
                var buf: [128]u8 = undefined;
                var fbs = std.Io.fixedBufferStream(buf[0..]);
                try fbs.writer().print("{d:.5}", .{n});
                try writer.writeInt(u8, @as(u8, @intCast(fbs.pos)), .little);
                try writer.writeAll(fbs.getWritten());
            },
            .const_string => |s| {
                try writer.writeInt(u16, @as(u16, @intCast(s.len)), .little);
                try writer.writeAll(s);
            },
            .timestamp => |t| {
                try writer.writeInt(i64, t, .little);
            },
            .visit => |v| {
                try writer.writeInt(u32, v, .little);
            },
            .obj => |o| {
                try writer.writeByte(@intFromEnum(@as(Obj.DataType, o.data)));
                try writer.writeAll(&o.id);
                switch (o.data) {
                    .anchor => |a| {
                        try writer.writeInt(u16, @as(u16, @intCast(a.name.len)), .little);
                        try writer.writeAll(a.name);
                        try writer.writeInt(C.JUMP, a.ip, .little);
                        try writer.writeInt(C.GLOBAL, a.visit_index, .little);
                        if (a.parent_anchor_index) |idx| {
                            try writer.writeByte(1);
                            try writer.writeInt(C.CONSTANT, idx, .little);
                        } else {
                            try writer.writeByte(0);
                        }
                    },
                    .string => |s| {
                        try writer.writeInt(u16, @as(u16, @intCast(s.len)), .little);
                        try writer.writeAll(s);
                    },
                    .builtin => |b| {
                        try writer.writeByte(@intCast(b.name.len));
                        try writer.writeAll(b.name);
                    },
                    .class => |c| {
                        try writer.writeByte(@intCast(c.name.len));
                        try writer.writeAll(c.name);

                        try writer.writeByte(@intCast(c.fields.len));
                        for (c.fields) |f| {
                            try writer.writeByte(@intCast(f.name.len));
                            try writer.writeAll(f.name);
                            try f.value.serialize(writer);
                        }

                        try writer.writeByte(@intCast(c.methods.len));
                        for (c.methods) |m| {
                            try writer.writeByte(@intCast(m.name.len));
                            try writer.writeAll(m.name);
                            try m.value.serialize(writer);
                        }
                    },
                    .function => |f| {
                        if (f.name) |n| {
                            try writer.writeInt(u16, @intCast(n.len), .little);
                            try writer.writeAll(n);
                        } else {
                            try writer.writeInt(u16, 0, .little);
                        }
                        try writer.writeByte(f.arity);
                        try writer.writeByte(if (f.is_method) 1 else 0);
                        try writer.writeInt(u16, @as(u16, @intCast(f.locals_count)), .little);
                        try writer.writeInt(u16, @as(u16, @intCast(f.instructions.len)), .little);
                        try writer.writeAll(f.instructions);
                        try writer.writeInt(u32, @intCast(f.debug_info.len), .little);
                        for (f.debug_info) |d| try d.serialize(writer);
                    },
                    .@"extern" => |e| {
                        try writer.writeByte(@intCast(e.name.len));
                        try writer.writeAll(e.name);
                        try writer.writeByte(e.arity);
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
                    .list => |l| {
                        try writer.writeInt(u16, @as(u16, @intCast(l.items.len)), .little);
                        for (l.items) |item| {
                            try item.serialize(writer);
                        }
                    },
                    .set => |s| {
                        const count = s.count();
                        try writer.writeInt(u16, @as(u16, @intCast(count)), .little);
                        for (s.keys()) |key| {
                            try key.serialize(writer);
                        }
                    },
                    .map => |m| {
                        const count = m.count();
                        try writer.writeInt(u16, @as(u16, @intCast(count)), .little);
                        for (m.keys(), m.values()) |key, val| {
                            try key.serialize(writer);
                            try val.serialize(writer);
                        }
                    },
                    else => return error.InvalidConstant,
                }
            },
            .nil, .void => {},
            else => return error.InvalidConstant,
        }
    }

    pub const max_deserialize_depth: u32 = 64;

    // used for constant values only
    pub fn deserialize(reader: *std.Io.Reader, allocator: std.mem.Allocator) !Value {
        return deserializeDepth(reader, allocator, 0);
    }

    fn deserializeDepth(reader: *std.Io.Reader, allocator: std.mem.Allocator, depth: u32) !Value {
        if (depth > max_deserialize_depth) return error.BytecodeTooDeep;
        const value_type: Type = @enumFromInt(try reader.takeByte());
        return switch (value_type) {
            .nil => Nil,
            .bool => if (try reader.takeByte() == '1') True else False,
            .number => {
                const length = try reader.takeInt(u8, .little);
                const buf = try allocator.alloc(u8, length);
                defer allocator.free(buf);
                try reader.readSliceAll(buf);
                return .{ .number = try std.fmt.parseFloat(f32, buf) };
            },
            .timestamp => {
                return .{ .timestamp = try reader.takeInt(i64, .little) };
            },
            .const_string => {
                const length = try reader.takeInt(u16, .little);
                return .{ .const_string = try reader.readAlloc(allocator, length) };
            },
            .visit => {
                return .{ .visit = try reader.takeInt(u32, .little) };
            },
            .obj => {
                const data_type: Obj.DataType = @enumFromInt(try reader.takeByte());
                var id: UUID.ID = undefined;
                try reader.readSliceAll(id[0..]);
                switch (data_type) {
                    .anchor => {
                        const length = try reader.takeInt(u16, .little);
                        const buf = try reader.readAlloc(allocator, length);
                        const ip = try reader.takeInt(C.JUMP, .little);
                        const visit_idx = try reader.takeInt(C.GLOBAL, .little);
                        const has_parent = (try reader.takeByte()) == 1;
                        const parent_anchor_idx = if (has_parent)
                            try reader.takeInt(C.CONSTANT, .little)
                        else
                            null;
                        const obj = try allocator.create(Value.Obj);
                        obj.* = .{ .id = id, .data = .{ .anchor = .{
                            .name = buf,
                            .uuid = id,
                            .ip = ip,
                            .visit_index = visit_idx,
                            .parent_anchor_index = parent_anchor_idx,
                        } } };
                        return .{ .obj = obj };
                    },
                    .string => {
                        const length = try reader.takeInt(u16, .little);
                        const buf = try reader.readAlloc(allocator, length);
                        const obj = try allocator.create(Value.Obj);
                        obj.* = .{ .id = id, .data = .{ .string = buf } };
                        return .{ .obj = obj };
                    },
                    .builtin => {
                        const name_len = try reader.takeByte();
                        const name_buf = try reader.readAlloc(allocator, name_len);
                        defer allocator.free(name_buf);

                        const builtin = runtime_builtins.get(name_buf) orelse
                            return error.BuiltinNotFound;

                        const obj = try allocator.create(Value.Obj);
                        obj.* = .{
                            .id = id,
                            .data = .{ .builtin = builtin },
                        };
                        return .{ .obj = obj };
                    },
                    .class => {
                        const name_len = try reader.takeByte();
                        const name = try reader.readAlloc(allocator, name_len);

                        const fields_len = try reader.takeByte();
                        var fields = try allocator.alloc(Class.Member, fields_len);
                        for (0..fields_len) |i| {
                            const f_name_len = try reader.takeByte();
                            const f_name = try reader.readAlloc(allocator, f_name_len);
                            const f_val = try Value.deserializeDepth(reader, allocator, depth + 1);
                            fields[i] = .{ .name = f_name, .value = f_val };
                        }

                        const methods_len = try reader.takeByte();
                        var methods = try allocator.alloc(Class.Member, methods_len);
                        for (0..methods_len) |i| {
                            const m_name_len = try reader.takeByte();
                            const m_name = try reader.readAlloc(allocator, m_name_len);
                            const m_val = try Value.deserializeDepth(reader, allocator, depth + 1);
                            methods[i] = .{ .name = m_name, .value = m_val };
                        }

                        const obj = try allocator.create(Value.Obj);
                        obj.* = .{
                            .id = id,
                            .data = .{
                                .class = try Class.init(name, fields, methods),
                            },
                        };
                        return .{ .obj = obj };
                    },
                    .function => {
                        const name_len = try reader.takeInt(u16, .little);
                        const name = if (name_len > 0) try reader.readAlloc(allocator, name_len) else null;
                        const arity = try reader.takeByte();
                        const is_method = if ((try reader.takeByte()) == 1) true else false;
                        const locals_count = try reader.takeInt(u16, .little);
                        const instructions_count = try reader.takeInt(u16, .little);
                        const buf = try reader.readAlloc(allocator, instructions_count);
                        const debug_info_count = try reader.takeInt(u32, .little);
                        var debug_info = try allocator.alloc(DebugInfo, debug_info_count);
                        for (0..debug_info_count) |i| {
                            debug_info[i] = try DebugInfo.deserialize(reader, allocator);
                        }
                        const obj = try allocator.create(Value.Obj);
                        obj.* = .{ .id = id, .data = .{
                            .function = .{
                                .name = name,
                                .arity = arity,
                                .is_method = is_method,
                                .locals_count = locals_count,
                                .instructions = buf,
                                .debug_info = debug_info,
                            },
                        } };
                        return .{ .obj = obj };
                    },
                    .@"extern" => {
                        const name_len = try reader.takeByte();
                        const name_buf = try reader.readAlloc(allocator, name_len);
                        const arity = try reader.takeByte();

                        const obj = try allocator.create(Value.Obj);
                        obj.* = .{ .id = id, .data = .{ .@"extern" = .{
                            .name = name_buf,
                            .arity = arity,
                        } } };
                        return .{ .obj = obj };
                    },
                    .@"enum" => {
                        const name_len = try reader.takeByte();
                        const name_buf = try reader.readAlloc(allocator, name_len);

                        const is_seq = try reader.takeByte() == 1;
                        const values_length = try reader.takeByte();
                        const obj = try allocator.create(Value.Obj);
                        obj.* = .{
                            .id = id,
                            .data = .{
                                .@"enum" = .{
                                    .name = name_buf,
                                    .values = try allocator.alloc([]const u8, values_length),
                                    .is_seq = is_seq,
                                },
                            },
                        };
                        for (0..values_length) |i| {
                            const value_name_len = try reader.takeByte();
                            const value_name_buf = try reader.readAlloc(allocator, value_name_len);
                            obj.data.@"enum".values[i] = value_name_buf;
                        }
                        return .{ .obj = obj };
                    },
                    .list => {
                        const count = try reader.takeInt(u16, .little);
                        var list = try std.ArrayList(Value).initCapacity(allocator, count);
                        for (0..count) |_| {
                            try list.append(allocator, try Value.deserializeDepth(reader, allocator, depth + 1));
                        }
                        const obj = try allocator.create(Value.Obj);
                        obj.* = .{ .id = id, .data = .{ .list = list } };
                        return .{ .obj = obj };
                    },
                    .set => {
                        const count = try reader.takeInt(u16, .little);
                        var set = Obj.SetType.empty;
                        for (0..count) |_| {
                            const key = try Value.deserializeDepth(reader, allocator, depth + 1);
                            try set.put(allocator, key, {});
                        }
                        const obj = try allocator.create(Value.Obj);
                        obj.* = .{ .id = id, .data = .{ .set = set } };
                        return .{ .obj = obj };
                    },
                    .map => {
                        const count = try reader.takeInt(u16, .little);
                        var map = Obj.MapType.empty;
                        for (0..count) |_| {
                            const key = try Value.deserializeDepth(reader, allocator, depth + 1);
                            const val = try Value.deserializeDepth(reader, allocator, depth + 1);
                            try map.put(allocator, key, val);
                        }
                        const obj = try allocator.create(Value.Obj);
                        obj.* = .{ .id = id, .data = .{ .map = map } };
                        return .{ .obj = obj };
                    },
                    else => return error.InvalidConstant,
                }
            },
            else => return error.InvalidConstant,
        };
    }

    pub fn format(self: Value, writer: *std.Io.Writer) std.Io.Writer.Error!void {
        return self.print(writer, null) catch |err| {
            std.debug.print("Could not print value {t}", .{err});
            return std.Io.Writer.Error.WriteFailed;
        };
    }

    /// Prints value to writer
    /// Optional AutoArrayHashMap can be passed in to detect circular references
    pub fn print(self: Value, writer: *std.Io.Writer, set: ?*std.AutoArrayHashMap(UUID.ID, void)) !void {
        switch (self) {
            .number => |n| try writer.print("{d:.5}", .{n}),
            .bool => |b| try writer.print("{}", .{b}),
            .nil => try writer.print("nil", .{}),
            .const_string => |s| try writer.print("{s}", .{s}),
            .visit, .timestamp => |v| try writer.print("{t} {d}", .{ self, v }),
            .enum_value => |e| try writer.print("{s}.{s}", .{ e.base.data.@"enum".name, e.base.data.@"enum".values[e.index] }),
            .obj => |o| {
                const is_container: bool = switch (o.data) {
                    .list, .map, .set, .instance => true,
                    else => false,
                };
                if (is_container) {
                    if (set) |a| {
                        if (a.contains(o.id)) {
                            try writer.print("CIRCULAR", .{});
                            return;
                        }
                        try a.putNoClobber(o.id, {});
                    }
                }
                switch (o.data) {
                    .anchor => |a| {
                        try writer.print("[anchor] {s} {d} ({d})", .{ a.name, a.ip, a.visit_index });
                    },
                    .string => |s| try writer.print("{s}", .{s}),
                    .list => |l| {
                        try writer.print("List{{", .{});
                        for (l.items, 0..) |item, i| {
                            try item.print(writer, set);
                            if (i != l.items.len - 1)
                                try writer.print(", ", .{});
                        }
                        try writer.print("}}", .{});
                    },
                    .map => |m| {
                        try writer.print("Map{{", .{});
                        const keys = m.keys();
                        for (keys, 0..) |k, i| {
                            try k.print(writer, set);
                            try writer.print(":", .{});
                            try m.get(k).?.print(writer, set);
                            if (i != keys.len - 1)
                                try writer.print(", ", .{});
                        }
                        try writer.print("}}", .{});
                    },
                    .set => |s| {
                        const keys = s.keys();
                        try writer.print("Set{{", .{});
                        for (keys, 0..) |k, i| {
                            try k.print(writer, set);
                            if (i != keys.len - 1)
                                try writer.print(", ", .{});
                        }
                        try writer.print("}}", .{});
                    },
                    .function => |f| {
                        try writer.print("[fn]\n", .{});
                        try Bytecode.printInstructions(writer, f.instructions);
                    },
                    .@"extern" => |e| {
                        try writer.print("[extern fn] {s}", .{e.name});
                    },
                    .class => |c| {
                        try writer.print("{s}", .{c.name});
                    },
                    .instance => |i| {
                        try writer.print("{s}.instance", .{i.base.data.class.name});
                        try writer.print(" {{\n", .{});
                        for (i.fields, 0..) |v, idx| {
                            try writer.print("    {s}: ", .{i.base.data.class.fields[idx].name});
                            try v.print(writer, set);
                            try writer.print("\n", .{});
                        }
                        try writer.print("}}", .{});
                    },
                    .@"enum" => |e| {
                        try writer.print("{s}{{", .{e.name});
                        for (e.values, 0..) |val, i| {
                            try writer.print("{s}", .{val});
                            if (i != e.values.len - 1)
                                try writer.print(", ", .{});
                        }
                        try writer.print("}}", .{});
                    },
                    .builtin => |b| {
                        try writer.print("[builtin] {s}", .{b.name});
                    },
                }
            },
            .range => |r| try writer.print("{}..{}", .{ r.start, r.end }),
            else => try writer.print("{s}", .{@tagName(self)}),
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
                .timestamp => |t| hashFn(&hasher, t),
                .const_string => |s| hasher.update(s),
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
                .timestamp => |t| t == b.timestamp,
                .const_string => |s| std.mem.eql(u8, s, b.const_string),
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
