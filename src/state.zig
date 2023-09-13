const std = @import("std");
const Value = @import("./values.zig").Value;
const ByteCode = @import("./bytecode.zig").ByteCode;

const StateMap = std.json.ArrayHashMap(Value);
const testing = std.testing;

/// Add the current state to the StateMap
pub fn save(state: StateMap, globals: std.ArrayList(Value), bytecode: ByteCode) !void {
    const count = bytecode.globals_map.count();
    if (count == 0) return;

    var it = bytecode.globals_map.iterator();
    while (it.next()) |entry| {
        var name = entry.key_ptr.*;
        var value = globals[entry.value_ptr.*];
        try state.put(name, value);
    }
}

/// Load the StateMap into the globals list
pub fn load(state: StateMap, globals: std.ArrayList(Value), bytecode: ByteCode) !void {
    var it = bytecode.globals_map.iterator();
    while (it.next()) |entry| {
        const index = entry.value_ptr.*;
        var value = state.get(entry.key_ptr.*);
        if (value) |val| {
            globals[index] = val;
        }
    }
}

/// Write the given state to a JSON string
pub fn serialize(state: StateMap, writer: anytype) !void {
    try std.json.stringify(state, .{}, writer);
}

/// Parse a JSON state back into a StateMap
pub fn deserialize(allocator: std.mem.Allocator, reader: anytype) !StateMap {

}

/// Consolidate all serialized states into one file
/// reader2 states will override reader1 states
pub fn consolidate(allocator: std.mem.Allocator, reader1: anytype, reader2: anytype, writer: anytype) !void {
    var all = try std.StringHashMap(Value).init(allocator);
    for (.{ reader1, reader2 }) |reader| {
        const count = try reader.readIntBig(u64);
        var i: usize = 0;
        while (i < count) : (i += 1) {
            const length = try reader.readIntBig(u16);
            var name = try allocator.alloc(u8, length);
            try reader.readNoEof(name);
            var value = Value.deserialize(reader, allocator);
            try all.put(name, value);
        }
    }
    try writer.writeIntBig(u64, all.count());

    var it = all.iterator();
    while (it.next()) |entry| {
        var name = entry.key_ptr.*;
        var value = entry.value_ptr.*;
        try writer.writeIntBig(u8, name.len);
        try writer.writeAll(name);
        try value.serialize(writer);
    }
}

test "Stringify" {
    var map = StateMap{};
    defer map.deinit(testing.allocator);
    try map.map.put(testing.allocator, "a", .{ .number = 1.0 });
    const doc = try std.json.stringifyAlloc(
        testing.allocator,
        map,
        .{ .whitespace = .indent_2 },
    );
    defer testing.allocator.free(doc);
    std.debug.print("{s}", .{doc});
}
