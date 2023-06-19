const std = @import("std");
const ast = @import("./ast.zig");
const parser = @import("./parser.zig");
const Token = @import("./token.zig").Token;
const OpCode = @import("./opcode.zig").OpCode;
const Errors = @import("./error.zig").Errors;

const testing = std.testing;

pub const ByteCode = struct {
    instructions: []u8,
    constants: []Constant,

    pub fn free(self: *ByteCode, allocator: std.mem.Allocator) void {
        allocator.free(self.instructions);
        allocator.free(self.constants);
    }
};

pub const Constant = union(enum) {
    number: f64,
};

pub const Compiler = struct {
    tokens: std.ArrayList(*const Token), // used for debug information

    instructions: std.ArrayList(u8),
    constants: std.ArrayList(Constant),

    pub fn init(allocator: std.mem.Allocator) Compiler {
        return .{
            .tokens = std.ArrayList(*const Token).init(allocator),
            .instructions = std.ArrayList(u8).init(allocator),
            .constants = std.ArrayList(Constant).init(allocator),
        };
    }

    pub fn deinit(self: *Compiler) void {
        self.tokens.deinit();
        self.instructions.deinit();
        self.constants.deinit();
    }

    pub fn bytecode(self: *Compiler) !ByteCode {
        return .{
            .instructions = try self.instructions.toOwnedSlice(),
            .constants = try self.constants.toOwnedSlice(),
        };
    }

    pub fn compile(self: *Compiler, tree: ast.Tree) !void {
        for (tree.root) |stmt| {
            try self.compileStatement(stmt);
        }
    }

    pub fn compileStatement(self: *Compiler, stmt: ast.Statement) !void {
        switch (stmt.type) {
            .expression => |exp| try self.compileExpression(&exp),
            else => return,
        }
    }

    pub fn compileExpression(self: *Compiler, expr: *const ast.Expression) !void {
        switch (expr.type) {
            .binary => |bin| {
                try self.compileExpression(bin.left);
                try self.compileExpression(bin.right);
            },
            .number => |n| {
                const i = try self.addConstant(.{ .number = n });
                _ = try self.writeInstruction(.constant, @intCast(u16, i), &expr.token);
            },
            else => return,
        }
    }

    pub fn writeInstruction(self: *Compiler, comptime op: OpCode, value: anytype, token: *const Token) !usize {
        const T: type = op.Type();
        if (T != @TypeOf(value)) return error.TypeMismatch;

        var start: usize = self.instructions.items.len;
        var buf: [@sizeOf(T) + 1]u8 = undefined;
        buf[0] = @enumToInt(op);
        var i: usize = 0;
        while (i < buf.len) : (i += 1) {
            try self.tokens.append(token);
        }
        switch (T) {
            u16 => std.mem.writeIntBig(u16, buf[1..], value),
            else => return start,
        }
        try self.instructions.writer().writeAll(&buf);
        return start;
    }

    pub fn addConstant(self: *Compiler, value: Constant) !usize {
        try self.constants.append(value);
        return self.constants.items.len - 1;
    }

    pub fn print(self: *Compiler, writer: anytype) !void {
        var i: usize = 0;
        writer.print("\n", .{});
        while (i < self.instructions.items.len) {
            writer.print("{d:0>4} ", .{i});
            if (i > 0 and self.tokens.items[i].line == self.tokens.items[i - 1].line) {
                writer.print("[{s}] ", .{"  | "});
            } else {
                writer.print("[{d:0>4}] ", .{self.tokens.items[i].line});
            }
            const op = @intToEnum(OpCode, self.instructions.items[i]);
            writer.print("{s: <16} ", .{op.toString()});
            i += 1;
            switch (op) {
                .constant => {
                    var index = std.mem.readIntSliceBig(u16, self.instructions.items[i..(i + 2)]);
                    writer.print("{d: >8} ", .{index});
                    i += 2;
                    var value = self.constants.items[index];
                    switch (value) {
                        .number => |n| writer.print(": {d: >8}\n", .{n}),
                    }
                },
            }
        }
    }
};

test "Basic Compile" {
    const test_cases = .{
        .{
            .input = "1 + 2",
            .expectedConstants = [_]Constant{ .{ .number = 1 }, .{ .number = 2 } },
            .expectedInstructions = [_]u8{ @enumToInt(OpCode.constant), 0, 0, @enumToInt(OpCode.constant), 0, 1 },
        },
    };

    inline for (test_cases) |case| {
        var allocator = testing.allocator;
        var errors = Errors.init(allocator);
        defer errors.deinit();
        const tree = parser.parse(allocator, case.input, &errors) catch |err| {
            try errors.write(case.input, std.io.getStdErr().writer());
            return err;
        };
        defer tree.deinit();
        var compiler = Compiler.init(allocator);
        defer compiler.deinit();
        try compiler.compile(tree);
        try compiler.print(std.debug);

        var bytecode = try compiler.bytecode();
        defer bytecode.free(testing.allocator);
        for (case.expectedInstructions, 0..) |instruction, i| {
            try testing.expectEqual(instruction, bytecode.instructions[i]);
        }
        for (case.expectedConstants, 0..) |constant, i| {
            switch (constant) {
                .number => |n| try testing.expectEqual(n, bytecode.constants[i].number),
            }
        }
    }
}
