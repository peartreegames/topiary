const std = @import("std");
const ast = @import("./ast.zig");
const parser = @import("./parser.zig");
const Token = @import("./token.zig").Token;
const OpCode = @import("./opcode.zig").OpCode;
const Errors = @import("./error.zig").Errors;
const Value = @import("./values.zig").Value;

const testing = std.testing;

const CompilerError = error{
    IllegalOperation,
    OutOfMemory,
};

pub const ByteCode = struct {
    instructions: []u8,
    constants: []Value,

    pub fn free(self: *ByteCode, allocator: std.mem.Allocator) void {
        allocator.free(self.instructions);
        allocator.free(self.constants);
    }
};

pub const Compiler = struct {
    lines: std.ArrayList(usize), // used for debug information

    instructions: std.ArrayList(u8),
    constants: std.ArrayList(Value),

    pub fn init(allocator: std.mem.Allocator) Compiler {
        return .{
            .lines = std.ArrayList(usize).init(allocator),
            .instructions = std.ArrayList(u8).init(allocator),
            .constants = std.ArrayList(Value).init(allocator),
        };
    }

    pub fn deinit(self: *Compiler) void {
        self.lines.deinit();
        self.instructions.deinit();
        self.constants.deinit();
    }

    pub fn bytecode(self: *Compiler) !ByteCode {
        return .{
            .instructions = try self.instructions.toOwnedSlice(),
            .constants = try self.constants.toOwnedSlice(),
        };
    }

    pub fn compile(self: *Compiler, tree: ast.Tree) CompilerError!void {
        for (tree.root) |stmt| {
            try self.compileStatement(stmt);
        }
    }

    pub fn compileStatement(self: *Compiler, stmt: ast.Statement) CompilerError!void {
        var line = stmt.token.line;
        switch (stmt.type) {
            .@"if" => |i| {
                try self.compileExpression(i.condition);
                try self.writeOp(.jump_if_false, line);
                // temp garbage value
                const pos = try self.writeInt(u16, std.math.maxInt(u16), line);
                try self.compileBlock(i.then_branch);
                if (i.else_branch == null) {
                    try self.replaceValue(pos, u16, self.u16Pos());
                } else {
                    try self.writeOp(.jump, line);
                    // temp garbage value
                    const nextPos = try self.writeInt(u16, std.math.maxInt(u16), line);
                    try self.replaceValue(pos, u16, self.u16Pos());
                    try self.compileBlock(i.else_branch.?);
                    try self.replaceValue(nextPos, u16, self.u16Pos());
                }
            },
            .block => |b| try self.compileBlock(b),
            .expression => |exp| {
                try self.compileExpression(&exp);
                try self.writeOp(.pop, line);
            },
            else => {},
        }
    }

    pub fn removeLastPop(self: *Compiler) void {
        if (self.instructions.items[self.instructions.items.len - 1] == @enumToInt(OpCode.pop)) {
            self.instructions.items = self.instructions.items[0 .. self.instructions.items.len - 1];
        }
    }

    pub fn compileBlock(self: *Compiler, stmts: []const ast.Statement) CompilerError!void {
        for (stmts) |stmt| {
            try self.compileStatement(stmt);
        }
    }

    pub fn compileExpression(self: *Compiler, expr: *const ast.Expression) CompilerError!void {
        var line = expr.token.line;
        switch (expr.type) {
            .binary => |bin| {
                if (bin.operator == .less_than) {
                    try self.compileExpression(bin.right);
                    try self.compileExpression(bin.left);
                    try self.writeOp(.greater_than, line);
                }
                try self.compileExpression(bin.left);
                try self.compileExpression(bin.right);
                const op: OpCode = switch (bin.operator) {
                    .add => .add,
                    .subtract => .subtract,
                    .multiply => .multiply,
                    .divide => .divide,
                    .modulus => .modulus,
                    .equal => .equal,
                    .not_equal => .not_equal,
                    .greater_than => .greater_than,
                    else => return error.IllegalOperation,
                };
                try self.writeOp(op, line);
            },
            .number => |n| {
                const i = try self.addConstant(.{ .number = n });
                try self.writeOp(.constant, line);
                _ = try self.writeInt(u16, i, line);
            },
            .boolean => |b| try self.writeOp(if (b) .true else .false, line),
            .unary => |u| {
                try self.compileExpression(u.value);
                switch (u.operator) {
                    .negate => try self.writeOp(.negate, line),
                    .not => try self.writeOp(.not, line),
                }
            },
            .@"if" => |i| {
                try self.compileExpression(i.condition);
                try self.writeOp(.jump_if_false, line);
                // temp garbage value
                const pos = try self.writeInt(u16, std.math.maxInt(u16), line);
                try self.compileExpression(i.then_value);

                try self.writeOp(.jump, line);
                const nextPos = try self.writeInt(u16, std.math.maxInt(u16), line);
                try self.replaceValue(pos, u16, self.u16Pos());

                try self.compileExpression(i.else_value);
                try self.replaceValue(nextPos, u16, self.u16Pos());
            },
            else => {},
        }
    }

    fn u16Pos(self: *Compiler) u16 {
        return @intCast(u16, self.instructions.items.len);
    }

    fn writeOp(self: *Compiler, op: OpCode, line: usize) !void {
        try self.instructions.append(@enumToInt(op));
        try self.lines.append(line);
    }

    fn writeValue(self: *Compiler, buf: []const u8, line: usize) !void {
        try self.instructions.writer().writeAll(buf);
        var i: usize = 0;
        while (i < buf.len) : (i += 1) {
            try self.lines.append(line);
        }
    }

    fn writeInt(self: *Compiler, comptime T: type, value: T, line: usize) !usize {
        var start = self.instructions.items.len;
        var buf: [@sizeOf(T)]u8 = undefined;
        std.mem.writeIntBig(T, buf[0..], value);
        try self.writeValue(&buf, line);
        return start;
    }

    pub fn replaceValue(self: *Compiler, pos: usize, comptime T: type, value: T) !void {
        var buf: [@sizeOf(T)]u8 = undefined;
        std.mem.writeIntBig(T, buf[0..], value);
        for (buf, 0..) |v, i| {
            self.instructions.items[pos + i] = v;
        }
    }

    pub fn addConstant(self: *Compiler, value: Value) !u16 {
        try self.constants.append(value);
        return @intCast(u16, self.constants.items.len - 1);
    }

    pub fn print(code: ByteCode, lines: ?[]usize, writer: anytype) void {
        var i: usize = 0;
        writer.print("\n==BYTECODE==\n", .{});
        const instructions = code.instructions;
        const constants = code.constants;
        while (i < instructions.len) {
            writer.print("{d:0>4} ", .{i});
            if (lines) |t| {
                if (i > 0 and t[i] == t[i - 1]) {
                    writer.print("[{s}] ", .{"  | "});
                } else {
                    writer.print("[{d:0>4}] ", .{t[i]});
                }
            }
            const op = @intToEnum(OpCode, instructions[i]);
            writer.print("{s: <16} ", .{op.toString()});
            i += 1;
            switch (op) {
                .jump, .jump_if_false => {
                    var dest = std.mem.readIntSliceBig(u16, instructions[i..(i + 2)]);
                    writer.print("{d: >8}", .{dest});
                    i += 2;
                },
                .constant => {
                    var index = std.mem.readIntSliceBig(u16, instructions[i..(i + 2)]);
                    writer.print("{d: >8} ", .{index});
                    i += 2;
                    var value = constants[index];
                    switch (value) {
                        .number => |n| writer.print(": {d: >8}", .{n}),
                        else => {},
                    }
                },
                else => {},
            }
            writer.print("\n", .{});
        }
    }
};

test "Basic Compile" {
    const test_cases = .{
        .{
            .input = "1 + 2",
            .expectedConstants = [_]Value{ .{ .number = 1 }, .{ .number = 2 } },
            .expectedInstructions = [_]u8{
                @enumToInt(OpCode.constant),
                0,
                0,
                @enumToInt(OpCode.constant),
                0,
                1,
                @enumToInt(OpCode.add),
                @enumToInt(OpCode.pop),
            },
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

        var bytecode = try compiler.bytecode();
        defer bytecode.free(testing.allocator);
        Compiler.print(bytecode, compiler.lines.items, std.debug);

        for (case.expectedInstructions, 0..) |instruction, i| {
            try testing.expectEqual(instruction, bytecode.instructions[i]);
        }
        for (case.expectedConstants, 0..) |constant, i| {
            switch (constant) {
                .number => |n| try testing.expectEqual(n, bytecode.constants[i].number),
                else => continue,
            }
        }
    }
}

test "Conditionals Compile" {
    const test_cases = .{
        .{
            .input = "if (true) { 10 } 333",
            .expectedConstants = [_]Value{ .{ .number = 10 }, .{ .number = 333 } },
            .expectedInstructions = [_]u8{
                @enumToInt(OpCode.true),
                @enumToInt(OpCode.jump_if_false),
                0,
                8,
                @enumToInt(OpCode.constant),
                0,
                0,
                @enumToInt(OpCode.pop),
                @enumToInt(OpCode.constant),
                0,
                1,
                @enumToInt(OpCode.pop),
            },
        },
        .{
            .input = "if (true) { 10 } else { 20 } 333",
            .expectedConstants = [_]Value{ .{ .number = 10 }, .{ .number = 20 }, .{ .number = 333 } },
            .expectedInstructions = [_]u8{
                @enumToInt(OpCode.true),
                @enumToInt(OpCode.jump_if_false),
                0,
                11,
                @enumToInt(OpCode.constant),
                0,
                0,
                @enumToInt(OpCode.pop),
                @enumToInt(OpCode.jump),
                0,
                15,
                @enumToInt(OpCode.constant),
                0,
                1,
                @enumToInt(OpCode.pop),
                @enumToInt(OpCode.constant),
                0,
                2,
                @enumToInt(OpCode.pop),
            },
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

        var bytecode = try compiler.bytecode();
        defer bytecode.free(testing.allocator);

        Compiler.print(bytecode, compiler.lines.items, std.debug);
        for (case.expectedInstructions, 0..) |instruction, i| {
            try testing.expectEqual(instruction, bytecode.instructions[i]);
        }
        for (case.expectedConstants, 0..) |constant, i| {
            switch (constant) {
                .number => |n| try testing.expectEqual(n, bytecode.constants[i].number),
                else => continue,
            }
        }
    }
}
