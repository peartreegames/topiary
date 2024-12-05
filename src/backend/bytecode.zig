const std = @import("std");
const types = @import("../types/index.zig");
const Value = types.Value;

const utils = @import("../utils/index.zig");
const UUID = utils.UUID;
const C = utils.C;

const DebugInfo = @import("debug.zig").DebugInfo;
const OpCode = @import("opcode.zig").OpCode;

pub const Bytecode = struct {
    instructions: []u8,
    constants: []Value,
    global_symbols: []GlobalSymbol,
    uuids: []UUID.ID,
    locals_count: usize,
    debug_info: []DebugInfo,
    boughs: []BoughJump,
    loc: []const u8,

    const sectionCount = 7;
    pub const BoughJump = struct { name: []const u8, ip: C.JUMP };

    pub const GlobalSymbol = struct {
        name: []const u8,
        index: C.GLOBAL,
        is_extern: bool,
        is_mutable: bool,
    };

    pub fn free(self: *const Bytecode, allocator: std.mem.Allocator) void {
        allocator.free(self.instructions);
        for (self.debug_info) |debug| debug.deinit(allocator);
        allocator.free(self.debug_info);
        for (self.constants) |item| {
            if (item == .obj) {
                Value.Obj.destroy(allocator, item.obj);
            }
        }
        allocator.free(self.constants);
        allocator.free(self.uuids);
        for (self.global_symbols) |s| allocator.free(s.name);
        allocator.free(self.global_symbols);
        for (self.boughs) |b| allocator.free(b.name);
        allocator.free(self.boughs);
        allocator.free(self.loc);
    }

    pub fn serialize(self: *Bytecode, seekable: anytype) !void {
        var writer = seekable.writer();
        const isSeekable = @hasField(@TypeOf(seekable.*), "getPos");
        const headerPos = if (isSeekable) try seekable.getPos() else 0;
        var section: u8 = 0;
        // globals, boughs, instructions, debug info, constants, uuids, loc
        while (section < sectionCount) : (section += 1) {
                try writer.writeInt(u64, 0, .little); // Placeholder for section offset
        }

        const globalPos = if (isSeekable) try seekable.getPos() else 0;
        try writer.writeInt(u64, @as(u64, @intCast(self.global_symbols.len)), .little);
        for (self.global_symbols) |sym| {
            try writer.writeInt(u8, @as(u8, @intCast(sym.name.len)), .little);
            try writer.writeAll(sym.name);
            try writer.writeInt(C.GLOBAL, @as(C.GLOBAL, @intCast(sym.index)), .little);
            try writer.writeByte(if (sym.is_extern) 1 else 0);
            try writer.writeByte(if (sym.is_mutable) 1 else 0);
        }

        const boughPos = if (isSeekable) try seekable.getPos() else 0;
        try writer.writeInt(u64, @as(u64, @intCast(self.boughs.len)), .little);
        for (self.boughs) |bough| {
            try writer.writeInt(u16, @as(u16, @intCast(bough.name.len)), .little);
            try writer.writeAll(bough.name);
            try writer.writeInt(C.JUMP, bough.ip, .little);
        }

        const instPos = if (isSeekable) try seekable.getPos() else 0;
        try writer.writeInt(u64, @as(u64, @intCast(self.instructions.len)), .little);
        try writer.writeAll(self.instructions);

        const debugPos = if (isSeekable) try seekable.getPos() else 0;
        try writer.writeInt(u16, @as(u16, @intCast(self.debug_info.len)), .little);
        for (self.debug_info) |debug| try debug.serialize(writer);

        const constPos = if (isSeekable) try seekable.getPos() else 0;
        try writer.writeInt(u64, @as(u64, @intCast(self.constants.len)), .little);
        for (self.constants) |constant| try constant.serialize(writer);

        const uuidPos = if (isSeekable) try seekable.getPos() else 0;
        try writer.writeInt(u64, @as(u64, @intCast(self.uuids.len)), .little);
        for (self.uuids) |uuid| try writer.writeAll(&uuid);

        const locPos = if (isSeekable) try seekable.getPos() else 0;
        try writer.writeInt(u128, @as(u128, @intCast(self.loc.len)), .little);
        try writer.writeAll(self.loc);

        if (isSeekable) {
            try seekable.seekTo(headerPos);
            try writer.writeInt(u64, globalPos, .little);
            try writer.writeInt(u64, boughPos, .little);
            try writer.writeInt(u64, instPos, .little);
            try writer.writeInt(u64, debugPos, .little);
            try writer.writeInt(u64, constPos, .little);
            try writer.writeInt(u64, uuidPos, .little);
            try writer.writeInt(u64, locPos, .little);
        }
    }

    pub fn deserialize(allocator: std.mem.Allocator, reader: anytype) !Bytecode {
        // skip headers
        try reader.skipBytes(sectionCount * @sizeOf(u64), .{});
        const globals_count = try reader.readInt(u64, .little);
        var global_symbols = try allocator.alloc(GlobalSymbol, globals_count);
        var count: usize = 0;
        while (count < globals_count) : (count += 1) {
            const length = try reader.readInt(u8, .little);
            const buf = try allocator.alloc(u8, length);
            errdefer allocator.free(buf);
            try reader.readNoEof(buf);
            const index = try reader.readInt(C.GLOBAL, .little);
            const is_extern = if (try reader.readByte() == 1) true else false;
            const is_mutable = if (try reader.readByte() == 1) true else false;
            global_symbols[count] = GlobalSymbol{
                .name = buf,
                .index = index,
                .is_extern = is_extern,
                .is_mutable = is_mutable,
            };
        }

        const bough_count = try reader.readInt(u64, .little);
        var boughs = try allocator.alloc(BoughJump, bough_count);
        errdefer allocator.free(boughs);
        count = 0;
        while (count < bough_count) : (count += 1) {
            const length = try reader.readInt(u16, .little);
            const buf = try allocator.alloc(u8, length);
            errdefer allocator.free(buf);
            try reader.readNoEof(buf);
            boughs[count] = BoughJump{
                .name = buf,
                .ip = try reader.readInt(C.JUMP, .little),
            };
        }

        const instruction_count = try reader.readInt(u64, .little);
        const instructions = try allocator.alloc(u8, instruction_count);
        errdefer allocator.free(instructions);
        try reader.readNoEof(instructions);

        const debug_info_count = try reader.readInt(u16, .little);
        var debug_info = try allocator.alloc(DebugInfo, debug_info_count);
        errdefer allocator.free(debug_info);
        for (0..debug_info_count) |i| {
            debug_info[i] = try DebugInfo.deserialize(reader, allocator);
        }

        const constant_count = try reader.readInt(u64, .little);
        var constants = try allocator.alloc(Value, constant_count);
        errdefer allocator.free(constants);
        for (0..constant_count) |i| {
            constants[i] = try Value.deserialize(reader, allocator);
        }
        const uuid_count = try reader.readInt(u64, .little);
        var uuids = try allocator.alloc(UUID.ID, uuid_count);
        count = 0;
        while (count < uuid_count) : (count += 1) {
            try reader.readNoEof(&uuids[count]);
        }

        const loc_len = try reader.readInt(u128, .little);
        const loc = try allocator.alloc(u8, @intCast(loc_len));
        try reader.readNoEof(loc);
        return .{
            .instructions = instructions,
            .debug_info = debug_info,
            .boughs = boughs,
            .constants = constants,
            .global_symbols = global_symbols,
            .uuids = uuids,
            .locals_count = 0,
            .loc = loc,
        };
    }

    pub fn print(code: *Bytecode, writer: anytype) !void {
        try writer.print("\n==BYTECODE==\n", .{});
        try printInstructions(writer, code.instructions);
        try writer.print("\n==DEBUG==\n", .{});
        try printDebugInfo(writer, code.debug_info);
    }

    pub fn printDebugInfo(writer: anytype, debug: []DebugInfo) !void {
        for (debug) |info| {
            try writer.print("{s}\n", .{info.file});
            for (info.ranges.items) |r| {
                try writer.print("    start: {}, end: {}, line: {}\n", .{ r.start, r.end, r.line });
            }
        }
    }

    pub fn printInstructions(writer: anytype, instructions: []const u8) !void {
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
                    var count = instructions[i];
                    i += 1;
                    const dest = std.mem.readVarInt(u32, instructions[i..(i + 4)], .little);
                    try writer.print("{d: >8} ", .{dest});
                    i += 4;
                    count -= 1;
                    while (count > 0) : (count -= 1) {
                        const next = std.mem.readVarInt(u32, instructions[i..(i + 4)], .little);
                        try writer.print(" {d}", .{next});
                        i += 4;
                    }
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
                .call,
                .class,
                .instance,
                .get_builtin,
                .get_free,
                .set_free,
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
                    const id = std.mem.readVarInt(u32, instructions[(i + 2)..(i + 6)], .little);
                    i += 6;
                    try writer.print("{: >8}", .{has_speaker});
                    try writer.print("   = ", .{});
                    try writer.print("{}", .{id});
                },
                .choice => {
                    const dest = std.mem.readVarInt(u32, instructions[i..(i + 4)], .little);
                    const is_unique = instructions[i + 4] == 1;
                    i += 4;
                    const id = std.mem.readVarInt(u32, instructions[i..(i + 4)], .little);
                    _ = id;
                    i += 4;
                    const visit_id = std.mem.readVarInt(u32, instructions[i..(i + 4)], .little);
                    _ = visit_id;
                    i += 4;
                    const tag_count = instructions[i + 1];
                    _ = tag_count;
                    i += 1;
                    try writer.print("{d: >8}", .{dest});
                    try writer.print(" unique: {}", .{is_unique});
                },
                .string, .closure => {
                    const index = std.mem.readVarInt(u32, instructions[i..(i + 4)], .little);
                    i += 4;
                    try writer.print("{d: >8}", .{index});
                    i += 1;
                    try writer.print("   = ", .{});
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
