const std = @import("std");
const ast = @import("./ast.zig");
const Gc = @import("./gc.zig").Gc;
const Allocator = std.mem.Allocator;

pub const True = Boolean{ .base = .{ .type = .boolean }, .value = true };
pub const False = Boolean{ .base = .{ .type = .boolean }, .value = false };
pub const Void = Value{ .type = ._void };
pub const Nil = Value{ .type = .nil };
pub const NativeFn = fn (gc: *Gc, args: []*Value) anyerror!*Value;

pub const Value = struct {
    type: Type,
    is_gc_marked: bool = false,
    next: ?*Value = null,

    pub fn unwrap(self: *Value, comptime v_type: Type) ?*v_type.ValueType() {
        if (self.type != v_type) return null;
        return @fieldParentPtr(v_type.ValueType(), "base", self);
    }

    pub fn toString(self: *Value) *String {
        return @fieldParentPtr(String, "base", self);
    }

    pub fn toInteger(self: *Value) *Integer {
        return @fieldParentPtr(Integer, "base", self);
    }

    pub fn toBool(self: *Value) *Boolean {
        return @fieldParentPtr(Boolean, "base", self);
    }

    pub fn toList(self: *Value) *List {
        return @fieldParentPtr(List, "base", self);
    }

    pub fn toMap(self: *Value) *Map {
        return @fieldParentPtr(Map, "base", self);
    }

    pub fn toSet(self: *Value) *Set {
        return @fieldParentPtr(Set, "base", self);
    }

    pub fn toFunction(self: *Value) *Function {
        return @fieldParentPtr(Function, "base", self);
    }

    pub fn toRange(self: *Value) *Range {
        return @fieldParentPtr(Range, "base", self);
    }

    pub fn toEnum(self: *Value) *Enum {
        return @fieldParentPtr(Enum, "base", self);
    }

    pub fn toIterable(self: *Value) *Iterable {
        return @fieldParentPtr(Iterable, "base", self);
    }

    pub fn isType(self: *const Value, tag: Type) bool {
        return self.ty == tag;
    }
    pub fn toNative(self: *Value) *Native {
        return @fieldParentPtr(Native, "base", self);
    }

    pub fn print(self: *Value, writer: anytype) @TypeOf(writer).Error!void {
        switch (self.type) {
            .integer => try writer.print("{}", .{self.toInteger().value}),
            .boolean => try writer.print("{}", .{self.toBool().value}),
            .string => try writer.writeAll(self.toString().value),
            .nil => try writer.writeAll("nil"),
            .list => {
                const list = self.toList().value;
                try writer.writeAll("[");
                for (list.items) |item, i| {
                    try item.print(writer);
                    if (i != list.items.len - 1)
                        try writer.writeAll(",\n");
                }
                try writer.writeAll("]\n");
            },
            .map => {
                const map = self.toMap().value;
                try writer.writeAll("{");
                for (map.entries.items) |item, i| {
                    try item.key.print(writer);
                    try writer.writeAll(":");
                    try item.value.print(writer);
                    if (i != map.items().len - 1)
                        try writer.writeAll(",\n");
                }
                try writer.writeAll("}\n");
            },
            .set => {
                const map = self.toMap().value;
                try writer.writeAll("{");
                for (map.entries.items) |item, i| {
                    try item.key.print(writer);
                    if (i != map.items().len - 1)
                        try writer.writeAll(",\n");
                }
                try writer.writeAll("}\n");
            },
            .range => try writer.print("{}..{}", .{ self.toRange().start, self.toRange().end }),
            ._enum => {
                const enm = self.toEnum().value;
                try writer.writeAll("{");
                for (enm) |item, i| {
                    try writer.writeAll(item);
                    if (i != enm.len - 1)
                        try writer.writeAll(",\n");
                }
                try writer.writeAll("}\n");
            },
            // .optional => {
            //     if (self.toOptional().child) |child|
            //         try child.print(writer)
            //     else
            //         try writer.writeAll("nil");
            // },
            else => try writer.writeAll("void"),
        }
    }
};

pub const Type = enum {
    import,
    integer,
    float,
    boolean,
    string,
    nil,
    function,
    list,
    set,
    map,
    native,
    // module,
    // optional,
    iterable,
    _enum,
    _void,

    pub fn ValueType(self: Type) type {
        return switch (self) {
            .import => Import,
            .integer => Integer,
            .boolean => Boolean,
            .string => String,
            .nil => @TypeOf(null),
            .function => Function,
            .list => List,
            .map => Map,
            .set => Set,
            .native => Native,
            // .module => Module,
            .iterable => Iterable,
            // .optional => Optional,
            ._enum => Enum,
            ._void => void,
        };
    }

    pub fn fromType(comptime T: type) Type {
        return switch (T) {
            Integer => .integer,
            Boolean => .boolean,
            String => .string,
            Function => .function,
            List => .list,
            Map => .map,
            Native => .native,
            // Module => .module,
            Iterable => .iterable,
            Enum => ._enum,
            // Optional => .optional,
            else => unreachable,
        };
    }
};

pub const Import = struct {
    base: Value,
    value: *Value,
};

pub const Integer = struct {
    base: Value,
    value: i64,

    pub fn create(gc: *Gc, val: i64) !*Value {
        return try gc.newValue(Integer, .{
            .base = .{ .type = .integer },
            .value = val,
        });
    }

    pub fn destroy(self: *Integer, gpa: *Allocator) void {
        gpa.destroy(self);
    }
};

pub const Boolean = struct {
    base: Value,
    value: bool,
};

pub const String = struct {
    base: Value,
    value: []const u8,

    pub fn create(gc: *Gc, val: []const u8) !*Value {
        return try gc.newValue(String, .{
            .base = .{ .type = .string },
            .value = try gc.gpa.dupe(u8, val),
        });
    }
    pub fn copyFrom(self: *String, gpa: *Allocator, other: *String) !void {
        gpa.free(self.value);
        self.value = try gpa.dupe(u8, other.value);
    }
    pub fn destroy(self: *String, gpa: *Allocator) void {
        gpa.free(self.value);
        gpa.destroy(self);
    }
};

pub const ReturnValue = struct {
    base: Value,
    value: *Value,
};

pub const Function = struct {
    base: Value,
    name: ?[]const u8,
    arg_len: usize,
    locals: usize,
    entry: usize,

    pub fn create(gc: *Gc, name: ?[]const u8, arg_len: usize, locals: usize, entry: usize) !*Value {
        const n = if (name) |n| try gc.gpa.dupe(u8, n) else null;
        return try gc.newValue(Function, .{
            .base = .{ .type = .function },
            .arg_len = arg_len,
            .locals = locals,
            .name = n,
            .entry = entry,
        });
    }
    pub fn destroy(self: *Function, gpa: *Allocator) void {
        if (self.name) |n| gpa.free(n);
        gpa.destroy(self);
    }
};

pub const List = struct {
    base: Value,
    value: ListType,

    pub const ListType = std.ArrayListUnmanaged(*Value);

    pub fn create(gc: *Gc, len: ?usize) !*Value {
        var self = List{
            .base = .{ .type = .list },
            .value = ListType{},
        };
        if (len) |n| try self.value.ensureCapacity(gc.gpa, n);
        return try gc.newValue(List, self);
    }
    pub fn destroy(self: *List, gpa: *Allocator) void {
        self.value.deinit(gpa);
        gpa.destroy(self);
    }
};

pub const Range = struct {
    base: Value,
    start: i64,
    end: i64,

    pub fn create(gc: *Gc, start: i64, end: i64) !*Value {
        return try gc.newValue(
            Range,
            .{
                .base = .{ .ty = .range, .next = null, .is_marked = false },
                .start = start,
                .end = end,
            },
        );
    }

    pub fn destroy(self: *Range, gpa: *Allocator) void {
        gpa.destroy(self);
    }
};

pub const Map = struct {
    base: Value,
    value: MapType,

    pub const MapType = std.ArrayHashMapUnmanaged(*Value, *Value, hash, eql, true);

    pub fn create(gc: *Gc, len: ?usize) !*Value {
        var self = Map{
            .base = .{ .type = .map },
            .value = MapType{},
        };

        if (len) |n| try self.value.ensureCapacity(gc.gpa, n);
        return try gc.newValue(Map, self);
    }

    pub fn destroy(self: *Map, gpa: *Allocator) void {
        self.value.deinit(gpa);
        gpa.destroy(self);
    }
};

pub const Set = struct {
    base: Value,
    value: SetType,

    pub const SetType = std.ArrayHashMapUnmanaged(*Value, void, hash, eql, true);

    pub fn create(gc: *Gc, len: ?usize) !*Value {
        var self = Set{
            .base = .{ .type = .map },
            .value = SetType{},
        };

        if (len) |n| try self.value.ensureCapacity(gc.gpa, n);
        return try gc.newValue(Map, self);
    }

    pub fn destroy(self: *Map, gpa: *Allocator) void {
        self.value.deinit(gpa);
        gpa.destroy(self);
    }
};

pub const Native = struct {
    base: Value,
    func: Value.NativeFn,
    arg_len: usize,

    pub fn destroy(self: *Native, gpa: *Allocator) void {
        gpa.destroy(self);
    }
};

pub const Enum = struct {
    base: Value,
    value: [][]const u8,

    /// Creates a new `Enum` value. Takes ownership of the memory of the slice
    pub fn create(gc: *Gc, enums: [][]const u8) !*Value {
        for (enums) |*enm| enm.* = try gc.gpa.dupe(u8, enm.*);
        return try gc.newValue(
            Enum,
            .{
                .base = .{ .ty = ._enum, .next = null, .is_marked = false },
                .value = enums,
            },
        );
    }

    pub fn destroy(self: *Enum, gpa: *Allocator) void {
        for (self.value) |enm| gpa.free(enm);
        gpa.free(self.value);
        gpa.destroy(self);
    }
};
// pub fn toModule(self: *Value) *Module {
//     return @fieldParentPtr(Module, "base", self);
// }

// pub fn toOptional(self: *Value) *Optional {
//     return @fieldParentPtr(Optional, "base", self);
// }

pub const Iterable = struct {
    base: Value,
    expose_index: bool,
    index: usize,
    value: *Value,

    pub fn create(gc: *Gc, expose: bool, index: usize, value: *Value) !*Value {
        return try gc.newValue(
            Iterable,
            .{
                .base = .{ .type = .iterable },
                .expose_index = expose,
                .index = index,
                .value = value,
            },
        );
    }

    pub fn destroy(self: *Iterable, gpa: *Allocator) void {
        gpa.destroy(self);
    }

    pub fn next(self: *@This(), gc: *Gc) !?*Value {
        switch (self.value.type) {
            .list => {
                const list = self.value.toList().value;
                if (list.items.len == 0) return null;
                if (list.items.len == self.index) return null;

                defer self.index += 1;
                return list.items[self.index];
            },
            .string => {
                const string = self.value.toString().value;
                if (string.len == 0) return null;
                if (string.len == self.index) return null;

                defer self.index += 1;
                return String.create(gc, string[self.index .. self.index + 1]);
            },
            .range => {
                const range = self.value.toRange();
                if (range.start == range.end) return null;
                if (self.index == range.end - range.start) return null;

                defer self.index += 1;
                return Integer.create(gc, range.start + @intCast(i64, self.index));
            },
            else => unreachable,
        }
    }
};
fn hash(key: *Value) u32 {
    const hashFn = std.hash.autoHash;
    var hasher = std.hash.Wyhash.init(0);

    switch (key.ty) {
        .integer => hashFn(&hasher, key.toInteger().value),
        .boolean => hashFn(&hasher, key.toBool().value),
        .string => hasher.update(key.toString().value),
        .function => {
            const func = key.toFunction();
            if (func.name) |name|
                hasher.update(name);
            hashFn(&hasher, func.arg_len);
            hashFn(&hasher, func.locals);
            hashFn(&hasher, func.entry);
        },
        .list => {
            const list = key.toList().value;
            hashFn(&hasher, list.items.len);
            hashFn(&hasher, list.items.ptr);
        },
        .map => {
            const map = key.toMap().value;
            hashFn(&hasher, map.items().len);
            hashFn(&hasher, map.items().ptr);
        },
        .native => {
            const native = key.toNative();
            hashFn(&hasher, native.arg_len);
            hashFn(&hasher, native.func);
        },
        .range => {
            const range = key.toRange();
            hashFn(&hasher, range.start);
            hashFn(&hasher, range.end);
        },
        else => unreachable,
    }
    return @truncate(u32, hasher.final());
}

fn eql(a: *Value, b: *Value) bool {
    return switch (a.ty) {
        .integer => a.toInteger().value == b.toInteger().value,
        .boolean => a.toBool().value == b.toBool().value,
        .nil => true,
        .string => std.mem.eql(u8, a.toString().value, b.toString().value),
        .list => {
            const list_a = a.toList().value;
            const list_b = b.toList().value;

            if (list_a.items.len != list_b.items.len) return false;
            for (list_a.items) |item, i| {
                if (!item.eql(list_b.items[i])) return false;
            }
            return true;
        },
        .map => {
            const map_a = a.toMap().value;
            const map_b = a.toMap().value;

            if (map_a.items().len != map_b.items().len) return false;
            for (map_a.items()) |entry, i| {
                if (entry.hash != map_b.items()[i].hash) return false;
            }
            return true;
        },
        .range => {
            const range_a = a.toRange();
            const range_b = b.toRange();

            return range_a.start == range_b.start and range_a.end == range_b.end;
        },
        else => unreachable,
    };
}

/// Prints a `Value` to the given `writer`
pub fn print(self: *Value, writer: anytype) @TypeOf(writer).Error!void {
    switch (self.ty) {
        .integer => try writer.print("{}", .{self.toInteger().value}),
        .boolean => try writer.print("{}", .{self.toBool().value}),
        .string => try writer.writeAll(self.toString().value),
        .nil => try writer.writeAll("nil"),
        .list => {
            const list = self.toList().value;
            try writer.writeAll("[");
            for (list.items) |item, i| {
                try item.print(writer);
                if (i != list.items.len - 1)
                    try writer.writeAll(",\n");
            }
            try writer.writeAll("]\n");
        },
        .map => {
            const map = self.toMap().value;
            try writer.writeAll("{");
            for (map.entries.items) |item, i| {
                try item.key.print(writer);
                try writer.writeAll(":");
                try item.value.print(writer);
                if (i != map.items().len - 1)
                    try writer.writeAll(",\n");
            }
            try writer.writeAll("}\n");
        },
        .range => try writer.print("{}..{}", .{ self.toRange().start, self.toRange().end }),
        ._enum => {
            const enm = self.toEnum().value;
            try writer.writeAll("{");
            for (enm) |item, i| {
                try writer.writeAll(item);
                if (i != enm.len - 1)
                    try writer.writeAll(",\n");
            }
            try writer.writeAll("}\n");
        },
        // .optional => {
        //     if (self.toOptional().child) |child|
        //         try child.print(writer)
        //     else
        //         try writer.writeAll("nil");
        // },
        else => try writer.writeAll("void"),
    }
}
