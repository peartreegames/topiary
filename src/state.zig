const std = @import("std");
const values = @import("./values.zig");
const adapter = @import("./values.zig").adapter;
const Bytecode = @import("./bytecode.zig").Bytecode;
const Vm = @import("./vm.zig").Vm;
const Enum = @import("./enum.zig").Enum;
const UUID = @import("./utils/uuid.zig").UUID;

const testing = std.testing;
const Nil = values.Nil;
const Void = values.Void;
const Value = values.Value;

pub const State = struct {
    pub fn serialize(vm: *Vm, writer: anytype) !void {
        var references = std.ArrayList(Value).init(vm.allocator);
        defer references.deinit();
        var seen = std.AutoHashMap(UUID.ID, void).init(vm.allocator);
        defer seen.deinit();
        var stream = std.json.writeStream(writer, .{ .whitespace = .indent_2 });
        defer stream.deinit();
        try stream.beginObject();
        for (vm.bytecode.global_symbols) |s| {
            if (s.is_extern) continue;

            const value = vm.globals[s.index];
            if (value == .void or switch (value.obj.data) {
                // should never be here, but just in case
                .builtin, .ext_function => true,
                else => false,
            }) continue;

            // we don't need to save 'const' values except collections and enums
            if (!s.is_mutable and (value != .obj or switch (value.obj.data) {
                .function, .string => true,
                else => false,
            })) continue;
            // or empty visits
            if (value == .visit and value.visit == 0) continue;

            try stream.objectField(s.name);
            try serializeValue(value, &stream, &references);
        }
        while (references.popOrNull()) |value| {
            if (seen.contains(value.obj.id)) continue;
            try seen.put(value.obj.id, {});
            try stream.objectField(&value.obj.id);
            try serializeObj(value, &stream, &references);
        }
        try stream.endObject();
    }

    fn serializeObj(value: Value, stream: anytype, references: *std.ArrayList(Value)) !void {
        try stream.beginObject();
        try stream.objectField(@tagName(value.obj.data));
        switch (value.obj.data) {
            .list => |l| {
                try stream.beginArray();
                for (l.items) |item| try serializeValue(item, stream, references);
                try stream.endArray();
            },
            .set => |s| {
                try stream.beginArray();
                for (s.keys()) |item| try serializeValue(item, stream, references);
                try stream.endArray();
            },
            .map => |m| {
                try stream.beginArray();
                for (m.keys()) |key| {
                    try stream.beginObject();
                    try stream.objectField("key");
                    try serializeValue(key, stream, references);
                    try stream.objectField("value");
                    try serializeValue(m.get(key).?, stream, references);
                    try stream.endObject();
                }
                try stream.endArray();
            },
            .@"enum" => |e| {
                try stream.beginObject();
                try stream.objectField("name");
                try stream.write(e.name);
                try stream.beginArray();
                for (e.values) |v| try stream.write(v);
                try stream.endArray();
                try stream.endObject();
            },
            .function => |f| {
                try stream.beginObject();
                try stream.objectField("arity");
                try stream.write(f.arity);
                try stream.objectField("inst");
                try stream.write(&f.instructions);
                try stream.objectField("lines");
                try stream.beginArray();
                for (f.lines) |l| try stream.write(l);
                try stream.endArray();
                try stream.objectField("locals_count");
                try stream.write(f.locals_count);
                try stream.objectField("is_method");
                try stream.write(f.is_method);
                try stream.endObject();
            },
            else => try stream.write("NOT IMPL"),
        }
        try stream.endObject();
    }

    fn serializeValue(value: Value, stream: anytype, references: *std.ArrayList(Value)) !void {
        try stream.beginObject();
        switch (value) {
            .obj => |o| switch (o.data) {
                .string => try stream.objectField(@tagName(.string)),
                else => try stream.objectField(@tagName(.ref)),
            },
            else => try stream.objectField(@tagName(value)),
        }
        switch (value) {
            .void => unreachable,
            .nil => try stream.write(null),
            .bool => |b| try stream.write(b),
            .number => |n| try stream.print("{d:.5}", .{n}),
            .visit => |v| try stream.write(v),
            .enum_value => |e| {
                try stream.beginObject();
                try references.append(e.base.*);
                try stream.objectField("base");
                try stream.write(e.base.obj.id);
                try stream.objectField("index");
                try stream.write(e.index);
                try stream.endObject();
            },
            .ref => |r| try stream.write(&r),
            .obj => |o| {
                switch (o.data) {
                    .string => |s| try stream.write(&s[0 .. s.len - 1]),
                    else => {
                        try references.append(value);
                        try stream.write(&o.id);
                    },
                }
            },
            else => try stream.write("NOT IMPl"),
        }
        try stream.endObject();
    }

    pub fn deserialize(vm: *Vm, json_str: []const u8) !void {
        var parsed = try std.json.parseFromSlice(std.json.Value, vm.allocator, json_str, .{});
        defer parsed.deinit();
        var refs = std.AutoHashMap(UUID.ID, Value).init(vm.allocator);
        defer refs.deinit();
        const root = parsed.value.object;
        for (vm.bytecode.global_symbols) |sym| {
            const maybe_entry = root.get(sym.name);
            if (maybe_entry) |entry| vm.globals[sym.index] = try deserializeEntry(vm, &root, entry, &refs, null);
        }
    }

    fn deserializeEntry(vm: *Vm, root: *const std.json.ObjectMap, entry: std.json.Value, refs: *std.AutoHashMap(UUID.ID, Value), id: ?UUID.ID) !Value {
        if (entry.object.get("number")) |v| return .{ .number = @floatCast(v.float) };
        if (entry.object.get("string")) |v| return try vm.gc.create(vm, .{ .string = try vm.allocator.dupe(u8, v.string) });
        if (entry.object.get("nil") != null) return Nil;
        if (entry.object.get("boolean")) |v| return if (v.bool) values.True else values.False;
        if (entry.object.get("visit")) |v| return .{ .visit = @intCast(v.integer) };
        if (entry.object.get("ref")) |v| {
            if (refs.get(UUID.fromString(v.string))) |ref| return ref;
            if (root.get(v.string)) |ref| {
                const value = try deserializeEntry(vm, root, ref, refs, UUID.fromString(v.string));
                try refs.put(UUID.fromString(v.string), value);
                return value;
            }
            return Void;
        }
        if (entry.object.get("enum_value")) |v| {
            const base_id = UUID.fromString(v.object.get("base").?.string);
            const base = if (refs.get(base_id)) |b| b else blk: {
                const value = try deserializeEntry(vm, root, v.object.get("base").?, refs, base_id);
                try refs.put(base_id, value);
                break :blk value;
            };
            return .{
                .enum_value = .{
                    .base = &base,
                    .index = @intCast(v.object.get("index").?.integer),
                },
            };
        }
        if (entry.object.get("list")) |v| {
            var list = std.ArrayList(Value).init(vm.allocator);
            for (v.array.items) |item| {
                try list.append(try deserializeEntry(vm, root, item, refs, null));
            }
            var result = try vm.gc.create(vm, .{ .list = list });
            result.obj.id = id.?;
            try refs.put(id.?, result);
            return result;
        }
        if (entry.object.get("map")) |v| {
            var map = Value.Obj.MapType.init(vm.allocator);
            for (v.array.items) |item| {
                const key = try deserializeEntry(vm, root, item.object.get("key").?, refs, null);
                const value = try deserializeEntry(vm, root, item.object.get("value").?, refs, null);
                try map.put(key, value);
            }
            var result = try vm.gc.create(vm, .{ .map = map });
            result.obj.id = id.?;
            try refs.put(id.?, result);
            return result;
        }
        if (entry.object.get("set")) |v| {
            var set = Value.Obj.SetType.init(vm.allocator);
            for (v.array.items) |item| {
                try set.put(try deserializeEntry(vm, root, item, refs, null), {});
            }
            var result = try vm.gc.create(vm, .{ .set = set });
            result.obj.id = id.?;
            try refs.put(id.?, result);
            return result;
        }
        if (entry.object.get("enum")) |v| {
            const values_items = v.object.get("values").?.array.items;
            const vals = try vm.allocator.alloc([]const u8, values_items.len);
            for (values_items, 0..) |t, i| vals[i] = try vm.allocator.dupe(u8, t.string);
            var result = try vm.gc.create(vm, .{ .@"enum" = .{
                .name = v.object.get("name").?.string,
                .values = vals,
            } });
            result.obj.id = id.?;
            try refs.put(id.?, result);
            return result;
        }
        if (entry.object.get("function")) |v| {
            const arity = v.object.get("arity").?.integer;
            const inst = v.object.get("inst").?.string;
            const lines_items = v.object.get("lines").?.array.items;
            var lines = try vm.allocator.alloc(u32, lines_items.len);
            for (lines_items, 0..) |t, i| lines[i] = @intCast(t.integer);
            const locals = v.object.get("locals_count").?.integer;
            const is_method = v.object.get("is_method").?.bool;
            var result = try vm.gc.create(vm, .{ .function = .{
                .arity = @intCast(arity),
                .instructions = try vm.allocator.dupe(u8, inst),
                .lines = try vm.allocator.dupe(u32, lines),
                .locals_count = @intCast(locals),
                .is_method = is_method,
            } });
            result.obj.id = id.?;
            try refs.put(id.?, result);
            return result;
        }
        return Void;
    }
};
