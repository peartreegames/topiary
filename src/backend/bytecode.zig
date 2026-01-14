const std = @import("std");

const types = @import("../types/index.zig");
const Value = types.Value;
const utils = @import("../utils/index.zig");
const UUID = utils.UUID;
const C = utils.C;
const DebugInfo = @import("debug.zig").DebugInfo;
const OpCode = @import("opcode.zig").OpCode;

pub const Bytecode = struct {
    locals_count: usize,
    constants: []Value,
    global_symbols: []GlobalSymbol,
    instructions: []u8,
    debug_info: []DebugInfo,

    const section_count = 4;
    const header_size = section_count * @sizeOf(u64);

    pub const GlobalSymbol = struct {
        name: []const u8,
        index: C.GLOBAL,
        is_mutable: bool,
    };

    pub fn free(self: *const Bytecode, allocator: std.mem.Allocator) void {
        allocator.free(self.instructions);
        for (self.debug_info) |*d| d.*.deinit();
        allocator.free(self.debug_info);
        for (self.constants) |item| item.destroy(allocator);
        allocator.free(self.constants);
        for (self.global_symbols) |s| allocator.free(s.name);
        allocator.free(self.global_symbols);
    }

    fn writeGlobals(self: *Bytecode, writer: *std.Io.Writer) !void {
        try writer.writeInt(u64, @as(u64, @intCast(self.global_symbols.len)), .little);
        for (self.global_symbols) |sym| {
            try writer.writeInt(u8, @as(u8, @intCast(sym.name.len)), .little);
            try writer.writeAll(sym.name);
            try writer.writeInt(C.GLOBAL, @as(C.GLOBAL, @intCast(sym.index)), .little);
            try writer.writeByte(if (sym.is_mutable) 1 else 0);
        }
    }

    fn writeInstructions(self: *Bytecode, writer: *std.Io.Writer) !void {
        try writer.writeInt(u64, @as(u64, @intCast(self.instructions.len)), .little);
        try writer.writeAll(self.instructions);
    }

    fn writeDebug(self: *Bytecode, writer: *std.Io.Writer) !void {
        try writer.writeInt(u32, @as(u32, @intCast(self.debug_info.len)), .little);
        for (self.debug_info) |debug| try debug.serialize(writer);
    }

    fn writeConstants(self: *Bytecode, writer: *std.Io.Writer) !void {
        try writer.writeInt(u64, @as(u64, @intCast(self.constants.len)), .little);
        for (self.constants) |constant| try constant.serialize(writer);
    }

    fn writeLocalization(self: *Bytecode, writer: *std.Io.Writer) !void {
        try writer.writeInt(u128, @as(u128, @intCast(self.loc.len)), .little);
        try writer.writeAll(self.loc);
    }

    pub fn serialize(self: *Bytecode, alloc: std.mem.Allocator, writer: *std.io.Writer) !usize {
        var allocating = std.io.Writer.Allocating.init(alloc);
        defer allocating.deinit();
        const alloc_writer = &allocating.writer;

        var offsets: [section_count]u64 = undefined;

        offsets[0] = header_size + allocating.written().len;
        try self.writeGlobals(alloc_writer);

        offsets[1] = header_size + allocating.written().len;
        try self.writeConstants(alloc_writer);

        offsets[2] = header_size + allocating.written().len;
        try self.writeInstructions(alloc_writer);

        offsets[3] = header_size + allocating.written().len;
        try self.writeDebug(alloc_writer);

        const result = header_size + allocating.written().len;
        for (offsets) |offset| {
            try writer.writeInt(u64, offset, .little);
        }
        try writer.writeAll(allocating.written());
        try writer.flush();
        return result;
    }

    pub fn deserialize(allocator: std.mem.Allocator, reader: *std.Io.Reader) !Bytecode {
        // skip headers
        try reader.discardAll(header_size);

        const globals_count = try reader.takeInt(u64, .little);
        var global_symbols = try allocator.alloc(GlobalSymbol, globals_count);
        var count: usize = 0;
        while (count < globals_count) : (count += 1) {
            const length = try reader.takeInt(u8, .little);
            const buf = try allocator.alloc(u8, length);
            errdefer allocator.free(buf);
            try reader.readSliceAll(buf);
            const index = try reader.takeInt(C.GLOBAL, .little);
            const is_mutable = if (try reader.takeByte() == 1) true else false;
            global_symbols[count] = GlobalSymbol{
                .name = buf,
                .index = index,
                .is_mutable = is_mutable,
            };
        }

        const constant_count = try reader.takeInt(u64, .little);
        var constants = try allocator.alloc(Value, constant_count);
        errdefer allocator.free(constants);
        for (0..constant_count) |i| {
            constants[i] = try Value.deserialize(reader, allocator);
        }

        const instruction_count = try reader.takeInt(u64, .little);
        const instructions = try allocator.alloc(u8, instruction_count);
        errdefer allocator.free(instructions);
        try reader.readSliceAll(instructions);

        const debug_info_count = try reader.takeInt(u16, .little);
        var debug_info = try allocator.alloc(DebugInfo, debug_info_count);
        errdefer allocator.free(debug_info);
        for (0..debug_info_count) |i| {
            debug_info[i] = try DebugInfo.deserialize(reader, allocator);
        }

        return .{
            .instructions = instructions,
            .debug_info = debug_info,
            .constants = constants,
            .global_symbols = global_symbols,
            .locals_count = 0,
        };
    }

    pub fn print(code: *Bytecode, writer: *std.Io.Writer) !void {
        try writer.print("\n==BYTECODE==\n", .{});
        try printInstructions(writer, code.instructions);
        try writer.print("\n==GLOBALS==\n", .{});
        for (code.global_symbols) |g| {
            try writer.print("{d} {s}", .{ g.index, g.name });
            try writer.print("\n", .{});
        }
        try writer.print("\n==CONSTANTS==\n", .{});
        for (code.constants, 0..) |value, i| {
            try writer.print("{} ", .{i});
            try value.print(writer, null);
            try writer.print("\n", .{});
        }
        // try writer.print("\n==DEBUG==\n", .{});
        // try printDebugInfo(writer, code.debug_info);
        try writer.flush();
    }

    pub fn printDebugInfo(writer: *std.Io.Writer, debug: []DebugInfo) !void {
        for (debug) |info| {
            try writer.print("{s}\n", .{info.file});
            for (info.ranges.items) |r| {
                try writer.print("    start: {}, end: {}, line: {}\n", .{ r.start, r.end, r.line });
            }
        }
    }

    pub fn printInstructions(writer: *std.Io.Writer, instructions: []const u8) !void {
        var i: usize = 0;
        while (i < instructions.len) {
            try writer.print("{d:0>4} ", .{i});
            const op: OpCode = @enumFromInt(instructions[i]);
            try writer.print("{s: <16} ", .{op.toString()});
            i += 1;
            switch (op) {
                .jump,
                .jump_if_false,
                .backup,
                .decl_global,
                .set_global,
                .get_global,
                .visit,
                => {
                    const dest = std.mem.readVarInt(u32, instructions[i..(i + 4)], .little);
                    try writer.print("{d: >8}", .{dest});
                    i += 4;
                },
                .divert => {
                    const dest = std.mem.readVarInt(u32, instructions[i..(i + 4)], .little);
                    i += 4;
                    try writer.print("{d: >8} ", .{dest});
                },
                .get_local,
                .set_local,
                .list,
                .map,
                .set,
                => {
                    const dest = std.mem.readVarInt(u16, instructions[i..(i + 2)], .little);
                    try writer.print("{d: >8}", .{dest});
                    i += 2;
                },
                .get_upvalue,
                .set_upvalue,
                => {
                    const frames_up = instructions[i];
                    i += 1;
                    const index = std.mem.readVarInt(C.LOCAL, instructions[i..(i + 2)], .little);
                    i += 2;
                    try writer.print("{d: >8} {d}", .{ frames_up, index });
                },
                .call,
                .instance,
                => {
                    const dest = instructions[i];
                    try writer.print("{d: >8}", .{dest});
                    i += 1;
                },
                .constant => {
                    const index = std.mem.readVarInt(u32, instructions[i..(i + 4)], .little);
                    try writer.print("{d: >8} ", .{index});
                    i += 4;
                },
                .dialogue => {
                    const has_speaker = instructions[i] == 1;
                    const tag_count = instructions[i + 1];
                    _ = tag_count;
                    i += 2;
                    const id = std.mem.readVarInt(u32, instructions[i..(i + 4)], .little);
                    i += 4;
                    try writer.print("{: >8}", .{has_speaker});
                    try writer.print("   = ", .{});
                    try writer.print("{}", .{id});
                },
                .choice => {
                    const dest = std.mem.readVarInt(u32, instructions[i..(i + 4)], .little);
                    i += 4;
                    const is_unique = instructions[i] == 1;
                    i += 1;
                    const id = std.mem.readVarInt(u32, instructions[i..(i + 4)], .little);
                    _ = id;
                    i += 4;
                    const visit_id = std.mem.readVarInt(u32, instructions[i..(i + 4)], .little);
                    _ = visit_id;
                    i += 4;
                    const tag_count = instructions[i];
                    i += 1;
                    try writer.print("{d: >8}", .{dest});
                    try writer.print(" unique: {}, tags: {d}", .{ is_unique, tag_count });
                },
                .string => {
                    const index = std.mem.readVarInt(u32, instructions[i..(i + 4)], .little);
                    i += 4;
                    try writer.print("{d: >8}", .{index});
                    i += 1;
                },
                .prong => {
                    const index = std.mem.readVarInt(u32, instructions[i..(i + 4)], .little);
                    i += 4;
                    try writer.print("{d: >8}", .{index});
                    const count = instructions[i];
                    i += 1;
                    try writer.print("   = {d}", .{count});
                },
                else => {},
            }
            try writer.print("\n", .{});
        }
    }
};
