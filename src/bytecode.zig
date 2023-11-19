const std = @import("std");
const Value = @import("./values.zig").Value;
const OpCode = @import("./opcode.zig").OpCode;
const UUID = @import("./utils/uuid.zig").UUID;

pub const ByteCode = struct {
    instructions: []u8,
    constants: []Value,
    global_symbols: []GlobalSymbol,
    uuids: []UUID.ID,
    locals_count: usize,
    token_lines: []u32,

    pub const GlobalSymbol = struct {
        name: []const u8,
        index: OpCode.Size(.get_global),
        is_extern: bool,
    };

    pub fn free(self: *const ByteCode, allocator: std.mem.Allocator) void {
        allocator.free(self.instructions);
        allocator.free(self.token_lines);
        for (self.constants) |item| {
            if (item == .obj) {
                Value.Obj.destroy(allocator, item.obj);
            }
        }
        allocator.free(self.constants);
        allocator.free(self.uuids);
        for (self.global_symbols) |s| allocator.free(s.name);
        allocator.free(self.global_symbols);
    }

    pub fn serialize(self: *ByteCode, writer: anytype) !void {
        // we could make this base64 encoded as well to reduce size, but the extra cost
        // to decode it might not be worth it
        try writer.writeIntBig(u64, @as(u64, @intCast(self.instructions.len)));
        try writer.writeAll(self.instructions);
        try writer.writeIntBig(u64, @as(u64, @intCast(self.token_lines.len)));
        for (self.token_lines) |line| try writer.writeIntBig(u32, line);
        try writer.writeIntBig(u64, @as(u64, @intCast(self.constants.len)));
        for (self.constants) |constant| try constant.serialize(writer);
        try writer.writeIntBig(u64, @as(u64, @intCast(self.uuids.len)));
        for (self.uuids) |uuid| try writer.writeAll(&uuid);
        try writer.writeIntBig(u64, @as(u64, @intCast(self.global_symbols.len)));
        for (self.global_symbols) |sym| {
            try writer.writeIntBig(u8, @as(u8, @intCast(sym.name.len)));
            try writer.writeAll(sym.name);
            try writer.writeIntBig(OpCode.Size(.get_global), @as(OpCode.Size(.get_global), @intCast(sym.index)));
            try writer.writeByte(if (sym.is_extern) 1 else 0);
        }
    }

    pub fn deserialize(allocator: std.mem.Allocator, reader: anytype) !ByteCode {
        const instruction_count = try reader.readIntBig(u64);
        var instructions = try allocator.alloc(u8, instruction_count);
        try reader.readNoEof(instructions);
        const token_count = try reader.readIntBig(u64);
        var token_lines = try allocator.alloc(u32, token_count);
        for (0..token_count) |i| {
            token_lines[i] = try reader.readIntBig(u32);
        }
        var constant_count = try reader.readIntBig(u64);
        var constants = try allocator.alloc(Value, constant_count);
        for (0..constant_count) |i| {
            constants[i] = try Value.deserialize(reader, allocator);
        }
        var uuid_count = try reader.readIntBig(u64);
        var uuids = try allocator.alloc(UUID.ID, uuid_count);
        var count: usize = 0;
        while (count < uuid_count) : (count += 1) {
            try reader.readNoEof(&uuids[count]);
        }

        var globals_count = try reader.readIntBig(u64);
        var global_symbols = try allocator.alloc(GlobalSymbol, globals_count);
        count = 0;
        while (count < globals_count) : (count += 1) {
            var length = try reader.readIntBig(u8);
            var buf = try allocator.alloc(u8, length);
            try reader.readNoEof(buf);
            const index = try reader.readIntBig(OpCode.Size(.get_global));
            const is_extern = if (try reader.readByte() == 1) true else false;
            global_symbols[count] = GlobalSymbol{
                .name = buf,
                .index = index,
                .is_extern = is_extern,
            };
        }
        return .{
            .instructions = instructions,
            .token_lines = token_lines,
            .constants = constants,
            .global_symbols = global_symbols,
            .uuids = uuids,
            .locals_count = 0,
        };
    }

    pub fn print(code: *ByteCode, writer: anytype) void {
        writer.print("\n==BYTECODE==\n", .{});
        printInstructions(writer, code.instructions, code.constants);
    }

    pub fn printInstructions(writer: anytype, instructions: []const u8, constants: ?[]Value) void {
        var i: usize = 0;
        while (i < instructions.len) {
            writer.print("{d:0>4} ", .{i});
            const op: OpCode = @enumFromInt(instructions[i]);
            writer.print("{s: <16} ", .{op.toString()});
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
                    const dest = std.mem.readIntSliceBig(u32, instructions[i..(i + 4)]);
                    writer.print("{d: >8}", .{dest});
                    i += 4;
                },
                .get_local,
                .set_local,
                .list,
                .map,
                .set,
                => {
                    const dest = std.mem.readIntSliceBig(u16, instructions[i..(i + 2)]);
                    writer.print("{d: >8}", .{dest});
                    i += 2;
                },
                .call,
                .class,
                .instance,
                .get_builtin,
                .get_free,
                .set_free,
                => {
                    var dest = std.mem.readIntSliceBig(u8, instructions[i..(i + 1)]);
                    writer.print("{d: >8}", .{dest});
                    i += 1;
                },
                .constant => {
                    var index = std.mem.readIntSliceBig(u32, instructions[i..(i + 4)]);
                    writer.print("{d: >8} ", .{index});
                    i += 4;
                    if (constants) |c| {
                        var value = c[index];
                        writer.print("  = ", .{});
                        value.print(writer, c);
                    }
                },
                .dialogue => {
                    const has_speaker = instructions[i] == 1;
                    const tag_count = instructions[i + 1];
                    _ = tag_count;
                    var id = std.mem.readIntSliceBig(u32, instructions[(i + 2)..(i + 6)]);
                    i += 6;
                    writer.print("{: >8}", .{has_speaker});
                    writer.print("   = ", .{});
                    writer.print("{}", .{id});
                },
                .choice => {
                    const dest = std.mem.readIntSliceBig(u32, instructions[i..(i + 4)]);
                    const is_unique = instructions[i + 4] == 1;
                    i += 4;
                    const id = std.mem.readIntSliceBig(u32, instructions[i..(i + 4)]);
                    _ = id;
                    i += 4;
                    const visit_id = std.mem.readIntSliceBig(u32, instructions[i..(i + 4)]);
                    i += 4;
                    _ = visit_id;
                    writer.print("{d: >8}", .{dest});
                    writer.print(" {}", .{is_unique});
                },
                .string, .closure => {
                    var index = std.mem.readIntSliceBig(u32, instructions[i..(i + 4)]);
                    i += 4;
                    writer.print("{d: >8}", .{index});
                    i += 1;
                    writer.print("   = ", .{});
                    if (constants) |c| {
                        var value = c[index];
                        value.print(writer, constants);
                    }
                },
                .prong => {
                    var index = std.mem.readIntSliceBig(u16, instructions[i..(i + 2)]);
                    i += 2;
                    writer.print("{d: >8}", .{index});
                    var count = std.mem.readIntSliceBig(u8, instructions[i..(i + 1)]);
                    i += 1;
                    writer.print("   = {d}", .{count});
                },
                else => {},
            }
            writer.print("\n", .{});
        }
    }
};
