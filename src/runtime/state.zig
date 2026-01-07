const std = @import("std");

const types = @import("../types/index.zig");
const Nil = types.Nil;
const Void = types.Void;
const True = types.True;
const False = types.False;
const Value = types.Value;
const Enum = types.Enum;
const Class = types.Class;
const Function = types.Function;

const backend = @import("../backend/index.zig");
const Bytecode = backend.Bytecode;
const DebugInfo = backend.DebugInfo;

const runtime = @import("../runtime/index.zig");
const Vm = runtime.Vm;

const UUID = @import("../utils/index.zig").UUID;

pub const State = struct {
    pub fn calculateSize(vm: *Vm) !usize {
        var buf: [1024]u8 = undefined;
        var counter = std.Io.Writer.Discarding.init(&buf);
        try serialize(vm, &counter.writer);
        return counter.count;
    }

    pub fn serialize(vm: *Vm, writer: *std.Io.Writer) !void {
        var references = std.ArrayList(Value).empty;
        defer references.deinit(vm.alloc);
        var seen = std.AutoHashMapUnmanaged(UUID.ID, void).empty;
        defer seen.deinit(vm.alloc);

        var stream: std.json.Stringify = .{
            .writer = writer,
            .options = .{ .whitespace = .minified },
        };
        try stream.beginObject();
        for (vm.bytecode.global_symbols) |s| {
            const value = vm.globals[s.index];
            if (value == .void) continue;
            if (value == .visit and value.visit == 0) continue;

            var is_str = false;
            const is_mut = s.is_mutable;
            const is_obj = value == .obj;
            if (is_obj) {
                const is_func = value.obj.data == .builtin or value.obj.data == .function;
                if (is_func) continue;
                is_str = value.obj.data == .string or value == .const_string;
            }
            // we don't need to save 'const' values (except visits)
            if (!is_mut and !is_obj and value != .visit) continue;
            // or 'const' strings and functions;
            if (!is_mut and is_str) continue;

            try stream.objectField(s.name);
            try serializeValue(vm.alloc, value, &stream, &references);
        }
        while (references.pop()) |value| {
            if (seen.contains(value.obj.id)) continue;
            try seen.put(vm.alloc, value.obj.id, {});
            try stream.objectField(&value.obj.id);
            try serializeObj(vm.alloc, value, &stream, &references);
        }
        try stream.endObject();
        try writer.flush();
    }

    fn serializeObj(allocator: std.mem.Allocator, value: Value, stream: anytype, references: *std.ArrayList(Value)) !void {
        try stream.beginObject();
        try stream.objectField(@tagName(value.obj.data));
        switch (value.obj.data) {
            .list => |l| {
                try stream.beginArray();
                for (l.items) |item| try serializeValue(allocator, item, stream, references);
                try stream.endArray();
            },
            .set => |s| {
                try stream.beginArray();
                for (s.keys()) |item| try serializeValue(allocator, item, stream, references);
                try stream.endArray();
            },
            .map => |m| {
                try stream.beginArray();
                for (m.keys()) |key| {
                    try stream.beginObject();
                    try stream.objectField("key");
                    try serializeValue(allocator, key, stream, references);
                    try stream.objectField("value");
                    try serializeValue(allocator, m.get(key).?, stream, references);
                    try stream.endObject();
                }
                try stream.endArray();
            },
            .@"enum" => |e| {
                try stream.beginObject();
                try stream.objectField("name");
                try stream.write(e.name);
                try stream.objectField("is_seq");
                try stream.write(e.is_seq);
                try stream.objectField("values");
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
                const buf = try allocator.alloc(u8, std.base64.standard.Encoder.calcSize(f.instructions.len));
                defer allocator.free(buf);
                try stream.write(std.base64.standard.Encoder.encode(buf, f.instructions));
                try stream.objectField("debug");
                try stream.beginArray();
                for (f.debug_info) |d| {
                    try stream.beginObject();
                    try stream.objectField("file");
                    try stream.write(d.file);
                    try stream.objectField("ranges");
                    try stream.beginArray();
                    for (d.ranges.items) |r| {
                        try stream.write(r.start);
                        try stream.write(r.end);
                        try stream.write(r.line);
                    }
                    try stream.endArray();
                    try stream.endObject();
                }
                try stream.endArray();
                try stream.objectField("locals_count");
                try stream.write(f.locals_count);
                try stream.objectField("is_method");
                try stream.write(f.is_method);
                try stream.endObject();
            },
            .class => |c| {
                try stream.beginObject();
                try stream.objectField("name");
                try stream.write(c.name);
                try stream.objectField("fields");
                try stream.beginArray();
                for (c.fields) |f| {
                    try stream.beginObject();
                    try stream.objectField("name");
                    try stream.write(f.name);
                    try stream.objectField("value");
                    try serializeValue(allocator, f.value, stream, references);
                    try stream.endObject();
                }
                try stream.endArray();

                try stream.objectField("methods");
                try stream.beginArray();
                for (c.methods) |m| {
                    try stream.beginObject();
                    try stream.objectField("name");
                    try stream.write(m.name);
                    try stream.objectField("value");
                    try serializeValue(allocator, m.value, stream, references);
                    try stream.endObject();
                }
                try stream.endArray();
                try stream.endObject();
            },
            .instance => |i| {
                try stream.beginObject();
                try stream.objectField("base");
                try serializeValue(allocator, .{ .obj = i.base }, stream, references);
                try stream.objectField("fields");
                try stream.beginArray();
                for (i.fields) |f| try serializeValue(allocator, f, stream, references);
                try stream.endArray();
                try stream.endObject();
            },
            else => return error.NotImplemented,
        }
        try stream.endObject();
    }

    fn serializeValue(allocator: std.mem.Allocator, value: Value, stream: anytype, references: *std.ArrayList(Value)) !void {
        try stream.beginObject();
        switch (value) {
            .const_string => try stream.objectField(@tagName(.string)),
            .obj => |o| switch (o.data) {
                .string => try stream.objectField(@tagName(.string)),
                else => try stream.objectField(@tagName(.ref)),
            },
            else => try stream.objectField(@tagName(value)),
        }
        switch (value) {
            .void => try stream.write(null),
            .nil => try stream.write(null),
            .bool => |b| try stream.write(b),
            .number => |n| try stream.print("{d:.5}", .{n}),
            .visit => |v| try stream.write(v),
            .enum_value => |e| {
                try stream.beginObject();
                try stream.objectField("base");
                try serializeValue(allocator, .{ .obj = e.base }, stream, references);
                try stream.objectField("index");
                try stream.write(e.index);
                try stream.endObject();
            },
            .const_string => |s| try stream.write(s),
            .ref => |r| try stream.write(&r),
            .obj => |o| {
                switch (o.data) {
                    .string => |s| try stream.write(s),
                    else => {
                        try references.append(allocator, value);
                        try stream.write(&o.id);
                    },
                }
            },
            else => return error.NotImplemented,
        }
        try stream.endObject();
    }

    pub fn deserialize(vm: *Vm, reader: *std.Io.Reader) !void {
        var arena = std.heap.ArenaAllocator.init(vm.alloc);
        defer arena.deinit();
        var json = std.json.Reader.init(vm.alloc, reader);
        defer json.deinit();

        const value = try std.json.parseFromTokenSourceLeaky(std.json.Value, arena.allocator(), &json, .{});
        var refs = std.AutoHashMapUnmanaged(UUID.ID, Value).empty;
        defer refs.deinit(vm.alloc);
        const root = value.object;
        for (vm.bytecode.global_symbols) |sym| {
            const maybe_entry = root.get(sym.name);
            if (maybe_entry) |entry| {
                vm.globals[sym.index] = try deserializeEntry(vm, &root, entry, &refs, null);
            }
        }
    }

    fn deserializeEntry(vm: *Vm, root: *const std.json.ObjectMap, entry: std.json.Value, refs: *std.AutoHashMapUnmanaged(UUID.ID, Value), id: ?UUID.ID) !Value {
        if (entry.object.get("void") != null) return Void;
        if (entry.object.get("nil") != null) return Nil;
        if (entry.object.get("number")) |v| return .{ .number = @floatCast(v.float) };
        if (entry.object.get("string")) |v| return try vm.gc.create(vm, .{ .string = try vm.alloc.dupe(u8, v.string) });
        if (entry.object.get("bool")) |v| return if (v.bool) True else False;
        if (entry.object.get("visit")) |v| return .{ .visit = @intCast(v.integer) };
        if (entry.object.get("ref")) |v| {
            const uuid = UUID.fromString(v.string);
            if (refs.get(uuid)) |ref| return ref;
            if (root.get(v.string)) |ref| {
                const value = try deserializeEntry(vm, root, ref, refs, uuid);
                try refs.put(vm.alloc, UUID.fromString(v.string), value);
                return value;
            }
            return Void;
        }
        if (entry.object.get("enum_value")) |v| {
            const base_ref = v.object.get("base").?.object.get("ref").?;
            const base_id = UUID.fromString(base_ref.string);
            const base = if (refs.get(base_id)) |b| b else blk: {
                const value = try deserializeEntry(vm, root, v.object.get("base").?, refs, base_id);
                try refs.put(vm.alloc, base_id, value);
                break :blk value;
            };
            return .{
                .enum_value = .{
                    .base = base.obj,
                    .index = @intCast(v.object.get("index").?.integer),
                },
            };
        }
        if (entry.object.get("list")) |v| {
            var list: std.ArrayList(Value) = .empty;
            for (v.array.items) |item| {
                try list.append(vm.alloc, try deserializeEntry(vm, root, item, refs, null));
            }
            var result = try vm.gc.create(vm, .{ .list = list });
            result.obj.id = id.?;
            try refs.put(vm.alloc, id.?, result);
            return result;
        }
        if (entry.object.get("map")) |v| {
            var map = Value.Obj.MapType.empty;
            for (v.array.items) |item| {
                const key = try deserializeEntry(vm, root, item.object.get("key").?, refs, null);
                const value = try deserializeEntry(vm, root, item.object.get("value").?, refs, null);
                try map.put(vm.alloc, key, value);
            }
            var result = try vm.gc.create(vm, .{ .map = map });
            result.obj.id = id.?;
            try refs.put(vm.alloc, id.?, result);
            return result;
        }
        if (entry.object.get("set")) |v| {
            var set = Value.Obj.SetType.empty;
            for (v.array.items) |item| {
                try set.put(vm.alloc, try deserializeEntry(vm, root, item, refs, null), {});
            }
            var result = try vm.gc.create(vm, .{ .set = set });
            result.obj.id = id.?;
            try refs.put(vm.alloc, id.?, result);
            return result;
        }
        if (entry.object.get("enum")) |v| {
            const values_items = v.object.get("values").?.array.items;
            const vals = try vm.alloc.alloc([]const u8, values_items.len);
            for (values_items, 0..) |t, i| vals[i] = try vm.alloc.dupe(u8, t.string);
            var result = try vm.gc.create(vm, .{
                .@"enum" = .{
                    .name = try vm.alloc.dupe(u8, v.object.get("name").?.string),
                    .is_seq = v.object.get("is_seq").?.bool,
                    .values = vals,
                    .is_gc_managed = true,
                },
            });
            result.obj.id = id.?;
            try refs.put(vm.alloc, id.?, result);
            return result;
        }
        if (entry.object.get("function")) |v| {
            const arity = v.object.get("arity").?.integer;
            const inst = v.object.get("inst").?.string;
            const locals_count = v.object.get("locals_count").?.integer;
            const is_method = v.object.get("is_method").?.bool;
            const inst_alloc = try vm.alloc.alloc(u8, try std.base64.standard.Decoder.calcSizeForSlice(inst));
            try std.base64.standard.Decoder.decode(inst_alloc, inst);

            const debug_items = v.object.get("debug").?.array.items;
            const debug_info = try vm.alloc.alloc(DebugInfo, debug_items.len);
            for (debug_items, 0..) |d, i| {
                const file = d.object.get("file").?.string;
                const range_items = d.object.get("ranges").?.array.items;
                const ranges = try std.ArrayList(DebugInfo.Range).initCapacity(vm.alloc, range_items.len);
                for (range_items, 0..) |r, ri| ranges.items[ri] = .{
                    .start = @intCast(r.array.items[0].integer),
                    .end = @intCast(r.array.items[1].integer),
                    .line = @intCast(r.array.items[2].integer),
                };
                debug_info[i] = .{
                    .allocator = vm.alloc,
                    .file = file,
                    .ranges = ranges,
                };
            }

            var result = try vm.gc.create(vm, .{ .function = .{
                .arity = @intCast(arity),
                .instructions = inst_alloc,
                .locals_count = @intCast(locals_count),
                .debug_info = debug_info,
                .is_method = is_method,
            } });
            result.obj.id = id.?;
            try refs.put(vm.alloc, id.?, result);
            return result;
        }
        if (entry.object.get("class")) |v| {
            const name = try vm.alloc.dupe(u8, v.object.get("name").?.string);
            const ser_fields = v.object.get("fields").?.array.items;
            var fields = try vm.alloc.alloc(Class.Member, ser_fields.len);
            for (ser_fields, 0..) |f, i| {
                fields[i].name = try vm.alloc.dupe(u8, f.object.get("name").?.string);
                fields[i].value = try deserializeEntry(vm, root, f.object.get("value").?, refs, null);
            }
            const ser_methods = v.object.get("methods").?.array.items;
            var methods = try vm.alloc.alloc(Class.Member, ser_methods.len);
            for (ser_methods, 0..) |m, i| {
                methods[i].name = try vm.alloc.dupe(u8, m.object.get("name").?.string);
                methods[i].value = try deserializeEntry(vm, root, m.object.get("value").?, refs, null);
            }
            var class = try Class.init( name, fields, methods);
            class.is_gc_managed = true;
            var result = try vm.gc.create(vm, .{ .class = class });
            result.obj.id = id.?;
            try refs.put(vm.alloc, id.?, result);
            return result;
        }
        if (entry.object.get("instance")) |v| {
            const base_ref = v.object.get("base").?.object.get("ref").?;
            const base_id = UUID.fromString(base_ref.string);
            const base = if (refs.get(base_id)) |b| b else blk: {
                const value = try deserializeEntry(vm, root, v.object.get("base").?, refs, base_id);
                try refs.put(vm.alloc, base_id, value);
                break :blk value;
            };
            const ser_fields = v.object.get("fields").?.array.items;
            var fields = try vm.alloc.alloc(Value, ser_fields.len);
            for (ser_fields, 0..) |f, i| {
                fields[i] = try deserializeEntry(vm, root, f, refs, null);
            }

            var result = try vm.gc.create(vm, .{ .instance = .{
                .base = base.obj,
                .fields = fields,
            } });
            result.obj.id = id.?;
            try refs.put(vm.alloc, id.?, result);
            return result;
        }

        return Void;
    }
};
