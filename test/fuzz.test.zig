const std = @import("std");
const topi = @import("topi");
const Bytecode = topi.backend.Bytecode;

// Fuzz target: the bytecode deserializer. Games load `.topib` files
// at runtime, so malformed input here must fail cleanly (errors, no
// crashes, no unbounded allocation) rather than crash the host.
//
// Run with:
//   zig build fuzz                 (one-shot: runs testOne with empty input)
//   zig build fuzz --fuzz          (continuous fuzzing; Linux-only in Zig 0.16)

fn fuzzBytecode(_: void, smith: *std.testing.Smith) anyerror!void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    var buf: [4096]u8 = undefined;
    const len = smith.sliceWithHash(&buf, 0);
    var reader = std.Io.Reader.fixed(buf[0..len]);
    _ = Bytecode.deserialize(arena.allocator(), &reader) catch return;
}

test "fuzz bytecode deserializer" {
    try std.testing.fuzz({}, fuzzBytecode, .{});
}

// Regression tests for the deserializer hardening.

test "bytecode: rejects missing magic" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const garbage = [_]u8{0} ** 64;
    var reader = std.Io.Reader.fixed(&garbage);
    try std.testing.expectError(error.InvalidBytecodeFormat, Bytecode.deserialize(arena.allocator(), &reader));
}

test "bytecode: rejects unsupported version" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    var buf: [64]u8 = @splat(0);
    @memcpy(buf[0..4], Bytecode.magic);
    std.mem.writeInt(u16, buf[4..6], 999, .little);
    var reader = std.Io.Reader.fixed(&buf);
    try std.testing.expectError(error.UnsupportedBytecodeVersion, Bytecode.deserialize(arena.allocator(), &reader));
}

test "bytecode: rejects oversized globals_count without allocating" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    var buf: [64]u8 = @splat(0);
    @memcpy(buf[0..4], Bytecode.magic);
    std.mem.writeInt(u16, buf[4..6], Bytecode.version, .little);
    // 8 bytes prelude + 32 bytes offsets = 40. globals_count at offset 40.
    std.mem.writeInt(u64, buf[40..48], std.math.maxInt(u64), .little);
    var reader = std.Io.Reader.fixed(&buf);
    try std.testing.expectError(error.CorruptBytecode, Bytecode.deserialize(arena.allocator(), &reader));
}
