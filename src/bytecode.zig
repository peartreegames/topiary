const std = @import("std");
const Value = @import("./values.zig").Value;
const OpCode = @import("./opcode.zig").OpCode;
const DebugToken = @import("./debug.zig").DebugToken;

pub const ByteCode = struct {
    instructions: []u8,
    constants: []Value,
    tokens: []DebugToken,

    pub fn free(self: *ByteCode, allocator: std.mem.Allocator) void {
        allocator.free(self.instructions);
        for (self.constants) |item| {
            if (item == .obj) {
                Value.Obj.destroy(allocator, item.obj);
            }
        }
        allocator.free(self.constants);
        allocator.free(self.tokens);
    }

    pub fn print(code: *ByteCode, writer: anytype) void {
        writer.print("\n==BYTECODE==\n", .{});
        printInstructions(writer, code.instructions, code.constants);
    }

    pub fn printInstructions(writer: anytype, instructions: []const u8, constants: ?[]Value) void {
        var i: usize = 0;
        while (i < instructions.len) {
            writer.print("{d:0>4} ", .{i});
            const op = @enumFromInt(OpCode, instructions[i]);
            writer.print("{s: <16} ", .{op.toString()});
            i += 1;
            switch (op) {
                .jump, .jump_if_false, .set_global, .get_global, .list, .map, .set, .choice => {
                    var dest = std.mem.readIntSliceBig(u16, instructions[i..(i + 2)]);
                    writer.print("{d: >8}", .{dest});
                    i += 2;
                },
                .get_local, .set_local, .get_free, .set_free, .call => {
                    var dest = std.mem.readIntSliceBig(u8, instructions[i..(i + 1)]);
                    writer.print("{d: >8}", .{dest});
                    i += 1;
                },
                .constant => {
                    var index = std.mem.readIntSliceBig(u16, instructions[i..(i + 2)]);
                    writer.print("{d: >8} ", .{index});
                    i += 2;
                    if (constants) |c| {
                        var value = c[index];
                        writer.print("  = ", .{});
                        value.print(writer, c);
                    }
                },
                .dialogue => {
                    var has_speaker = instructions[i] == 1;
                    var tag_count = instructions[i + 1];
                    i += 2;
                    writer.print("{: >8}", .{has_speaker});
                    writer.print("{d: >4}", .{tag_count});
                },
                .string, .closure => {
                    var index = std.mem.readIntSliceBig(u16, instructions[i..(i + 2)]);
                    i += 2;
                    writer.print("{d: >8}", .{index});
                    i += 1;
                    writer.print("  = ", .{});
                    if (constants) |c| {
                        var value = c[index];
                        value.print(writer, c);
                    }
                },
                else => {},
            }
            writer.print("\n", .{});
        }
    }
};
