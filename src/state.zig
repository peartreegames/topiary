const std = @import("std");
const Value = @import("./values.zig").Value;
const ByteCode = @import("./bytecode.zig").ByteCode;
const Vm = @import("./vm.zig").Vm;
const UUID = @import("./utils/uuid.zig").UUID;

const testing = std.testing;

pub const StateMap = struct {
    allocator: std.mem.Allocator,
    map: Map,

    const Map = std.StringHashMap([]u8);

    pub fn init(allocator: std.mem.Allocator) StateMap {
        return .{
            .allocator = allocator,
            .map = Map.init(allocator),
        };
    }

    pub fn deinit(self: *StateMap) void {
        var it = self.map.iterator();
        while (it.next()) |entry| {
            self.allocator.free(entry.key_ptr.*);
            self.allocator.free(entry.value_ptr.*);
        }
        self.map.deinit();
    }

    pub fn put(self: *StateMap, name: []const u8, value: Value) !void {
        var data = std.ArrayList(u8).init(self.allocator);
        errdefer data.deinit();
        try value.serialize(data.writer());
        try self.map.put(try self.allocator.dupe(u8, name), try data.toOwnedSlice());
    }

    pub fn get(self: *StateMap, name: []const u8) !?Value {
        var item = self.map.get(name);
        if (item) |i| {
            var buf = std.io.fixedBufferStream(i);
            var value = try Value.deserialize(buf.reader(), self.allocator);
            return value;
        }
        return null;
    }

    /// Write the given StateMap to a string
    pub fn serialize(self: *StateMap, writer: anytype) !void {
        try writer.writeIntBig(u64, @as(u64, @intCast(self.map.count())));
        var it = self.map.iterator();
        while (it.next()) |entry| {
            try writer.writeIntBig(u16, @as(u8, @intCast(entry.key_ptr.*.len)));
            try writer.writeAll(entry.key_ptr.*);
            try writer.writeAll(":");
            try writer.writeIntBig(u32, @as(u32, @intCast(entry.value_ptr.*.len)));
            try writer.writeAll(entry.value_ptr.*);
            try writer.writeAll("\n");
        }
    }

    /// Parse a serialized state back into a StateMap
    pub fn deserialize(allocator: std.mem.Allocator, reader: anytype) !StateMap {
        var size = try reader.readIntBig(u64);
        var state = StateMap.init(allocator);
        var count: usize = 0;
        while (count < size) : (count += 1) {
            var length = try reader.readIntBig(u16);
            var buf = try allocator.alloc(u8, length);
            try reader.readNoEof(buf);
            _ = try reader.readByte();
            var value_length = try reader.readIntBig(u32);
            var value = try allocator.alloc(u8, value_length);
            try reader.readNoEof(value);
            try state.map.put(buf, value);
            _ = try reader.readByte();
        }
        return state;
    }
};

test "Stringify" {
    var alloc = testing.allocator;
    var map = StateMap.init(alloc);
    defer map.deinit();
    try map.put("a", .{ .number = 71.005 });

    var str = try alloc.create(Value.Obj);
    defer alloc.destroy(str);
    str.* = .{ .data = .{ .string = "some text value" } };
    str.*.id = UUID.new();
    try map.put("b", .{ .obj = str });

    try map.put("c", .{ .bool = true });

    var list_obj = try alloc.create(Value.Obj);
    defer alloc.destroy(list_obj);
    var list = std.ArrayList(Value).init(alloc);
    defer list.deinit();
    try list.append(.{ .number = 5 });
    try list.append(.{ .number = 6 });
    list_obj.* = .{ .data = .{ .list = list } };
    try map.put("d", .{ .obj = list_obj });

    var data = std.ArrayList(u8).init(testing.allocator);
    defer data.deinit();
    try map.serialize(data.writer());

    var buf = std.io.fixedBufferStream(data.items);
    var copy = try StateMap.deserialize(alloc, buf.reader());
    defer copy.deinit();
    try testing.expect(copy.map.contains("a"));
    try testing.expect((try copy.get("a")).?.eql(.{ .number = 71.005 }));
    std.log.warn("\n{s}\n", .{data.items});
}
