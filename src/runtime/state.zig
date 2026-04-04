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
        if (vm.variation_state.count() > 0) {
            try stream.objectField("__variation_state");
            try stream.beginObject();
            var vs_it = vm.variation_state.iterator();
            while (vs_it.next()) |entry| {
                var key_buf: [20]u8 = undefined;
                const key_str = std.fmt.bufPrint(&key_buf, "{d}", .{entry.key_ptr.*}) catch unreachable;
                try stream.objectField(key_str);
                try stream.beginObject();
                switch (entry.value_ptr.*) {
                    .cycle => |idx| {
                        try stream.objectField("cycle");
                        try stream.write(idx);
                    },
                    .sequence => |idx| {
                        try stream.objectField("sequence");
                        try stream.write(idx);
                    },
                    .shuffle => |s| {
                        try stream.objectField("shuffle");
                        try stream.beginObject();
                        try stream.objectField("index");
                        try stream.write(s.index);
                        try stream.objectField("order");
                        try stream.beginArray();
                        for (s.order.items) |o| try stream.write(o);
                        try stream.endArray();
                        try stream.endObject();
                    },
                }
                try stream.endObject();
            }
            try stream.endObject();
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
                try stream.objectField("field_names");
                try stream.beginArray();
                for (c.fields) |f| try stream.write(f.name);
                try stream.endArray();
                try stream.endObject();
            },
            .instance => |i| {
                try stream.beginObject();
                try stream.objectField("class_name");
                try stream.write(i.base.data.class.name);
                try stream.objectField("fields");
                try stream.beginArray();
                for (i.fields, 0..) |f, idx| {
                    try stream.beginObject();
                    try stream.objectField("name");
                    try stream.write(i.base.data.class.fields[idx].name);
                    try stream.objectField("value");
                    try serializeValue(allocator, f, stream, references);
                    try stream.endObject();
                }
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

    fn resolveClassByName(vm: *Vm, name: []const u8) ?Value {
        for (vm.bytecode.constants) |c| {
            if (c == .obj and c.obj.data == .class) {
                if (std.mem.eql(u8, c.obj.data.class.name, name)) return c;
            }
        }
        return null;
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
        if (root.get("__variation_state")) |vs_json| {
            var vs_it = vs_json.object.iterator();
            while (vs_it.next()) |entry| {
                const key = std.fmt.parseInt(u32, entry.key_ptr.*, 10) catch continue;
                const obj = entry.value_ptr.object;
                if (obj.get("cycle")) |idx| {
                    try vm.variation_state.put(vm.alloc, key, .{ .cycle = @intCast(idx.integer) });
                } else if (obj.get("sequence")) |idx| {
                    try vm.variation_state.put(vm.alloc, key, .{ .sequence = @intCast(idx.integer) });
                } else if (obj.get("shuffle")) |s| {
                    const s_obj = s.object;
                    var order = std.ArrayList(u32).empty;
                    for (s_obj.get("order").?.array.items) |o| {
                        try order.append(vm.alloc, @intCast(o.integer));
                    }
                    try vm.variation_state.put(vm.alloc, key, .{ .shuffle = .{
                        .index = @intCast(s_obj.get("index").?.integer),
                        .order = order,
                    } });
                }
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
        if (entry.object.get("class")) |v| {
            const name = v.object.get("name").?.string;
            // Resolve class from module constants if available (correct methods)
            if (resolveClassByName(vm, name)) |class_value| {
                if (id) |uuid| try refs.put(vm.alloc, uuid, class_value);
                return class_value;
            }
            // Create stub class (data-only, no methods) for cross-module case
            const field_names_items = v.object.get("field_names").?.array.items;
            var fields = try vm.alloc.alloc(Class.Member, field_names_items.len);
            for (field_names_items, 0..) |f, i| {
                fields[i] = .{
                    .name = try vm.alloc.dupe(u8, f.string),
                    .value = Void,
                };
            }
            const methods = try vm.alloc.alloc(Class.Member, 0);
            var class = try Class.init(try vm.alloc.dupe(u8, name), fields, methods);
            class.is_gc_managed = true;
            var result = try vm.gc.create(vm, .{ .class = class });
            if (id) |uuid| {
                result.obj.id = uuid;
                try refs.put(vm.alloc, uuid, result);
            }
            return result;
        }
        if (entry.object.get("instance")) |v| {
            const class_name = v.object.get("class_name").?.string;
            const ser_fields = v.object.get("fields").?.array.items;

            // Resolve or create the base class
            const class_obj = if (resolveClassByName(vm, class_name)) |cv| cv.obj else blk: {
                // Create stub class from field names in the instance data
                var stub_fields = try vm.alloc.alloc(Class.Member, ser_fields.len);
                for (ser_fields, 0..) |f, i| {
                    stub_fields[i] = .{
                        .name = try vm.alloc.dupe(u8, f.object.get("name").?.string),
                        .value = Void,
                    };
                }
                const methods = try vm.alloc.alloc(Class.Member, 0);
                var class = try Class.init(try vm.alloc.dupe(u8, class_name), stub_fields, methods);
                class.is_gc_managed = true;
                const stub = try vm.gc.create(vm, .{ .class = class });
                break :blk stub.obj;
            };

            const class_data = class_obj.data.class;

            // Build field values, matching by name to handle field reordering/additions/removals
            var fields = try vm.alloc.alloc(Value, class_data.fields.len);
            for (class_data.fields, 0..) |f, i| fields[i] = f.value;
            for (ser_fields) |sf| {
                const name = sf.object.get("name").?.string;
                for (class_data.fields, 0..) |cf, i| {
                    if (std.mem.eql(u8, cf.name, name)) {
                        fields[i] = try deserializeEntry(vm, root, sf.object.get("value").?, refs, null);
                        break;
                    }
                }
            }

            var result = try vm.gc.create(vm, .{ .instance = .{
                .base = class_obj,
                .fields = fields,
            } });
            if (id) |uuid| {
                result.obj.id = uuid;
                try refs.put(vm.alloc, uuid, result);
            }
            return result;
        }

        return Void;
    }
};
