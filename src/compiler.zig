const std = @import("std");
const ast = @import("./ast.zig");
const parser = @import("./parser.zig");
const Token = @import("./token.zig").Token;
const OpCode = @import("./opcode.zig").OpCode;
const Errors = @import("./error.zig").Errors;
const Value = @import("./values.zig").Value;
const String = @import("./values.zig").String;
const SymbolTable = @import("./symbols.zig").SymbolTable;
const Symbol = @import("./symbols.zig").Symbol;

const testing = std.testing;

const CompilerError = error{
    IllegalOperation,
    OutOfMemory,
};

pub const ByteCode = struct {
    instructions: []u8,
    constants: []Value,
    tokens: []DebugToken,

    pub fn free(self: *ByteCode, allocator: std.mem.Allocator) void {
        allocator.free(self.instructions);
        for (self.constants) |c| {
            c.destroy(allocator);
        }
        allocator.free(self.constants);
        allocator.free(self.tokens);
    }
};

pub const DebugTokens = struct {
    pub fn append(tokens: *std.ArrayList(DebugToken), token: Token) !void {
        if (tokens.items.len == 0) {
            try tokens.append(.{ .token = token, .length = 0 });
            return;
        }
        var current = tokens.items[tokens.items.len - 1];
        if (current.token.eql(token)) {
            current.length += 1;
            tokens.items[tokens.items.len - 1] = current;
            return;
        }
        try tokens.append(.{ .token = token, .length = 0 });
    }

    pub fn get(tokens: []DebugToken, index: usize) ?Token {
        var current: usize = 0;
        for (tokens, 0..) |token, i| {
            current += i;
            current += token.length;
            if (current >= index) return token.token;
        }
        return null;
    }
};

pub const DebugToken = struct {
    token: Token,
    length: usize,
};

pub const Compiler = struct {
    allocator: std.mem.Allocator,
    tokens: std.ArrayList(DebugToken),

    instructions: std.ArrayList(u8),
    constants: std.ArrayList(Value),
    symbols: SymbolTable,

    pub fn init(allocator: std.mem.Allocator) Compiler {
        return .{
            .allocator = allocator,
            .tokens = std.ArrayList(DebugToken).init(allocator),
            .instructions = std.ArrayList(u8).init(allocator),
            .constants = std.ArrayList(Value).init(allocator),
            .symbols = SymbolTable.init(allocator),
        };
    }

    pub fn deinit(self: *Compiler) void {
        self.tokens.deinit();
        self.instructions.deinit();
        self.constants.deinit();
        self.symbols.deinit();
    }

    pub fn bytecode(self: *Compiler) !ByteCode {
        return .{
            .instructions = try self.instructions.toOwnedSlice(),
            .constants = try self.constants.toOwnedSlice(),
            .tokens = try self.tokens.toOwnedSlice(),
        };
    }

    pub fn compile(self: *Compiler, tree: ast.Tree) CompilerError!void {
        for (tree.root) |stmt| {
            try self.compileStatement(stmt);
        }
    }

    pub fn compileStatement(self: *Compiler, stmt: ast.Statement) CompilerError!void {
        var token = stmt.token;
        switch (stmt.type) {
            .@"if" => |i| {
                try self.compileExpression(i.condition);
                try self.writeOp(.jump_if_false, token);
                const falsePos = try self.writeInt(u16, std.math.maxInt(u16), token);
                try self.compileBlock(i.then_branch);
                self.removeLastPop();

                try self.writeOp(.jump, token);
                const jumpPos = try self.writeInt(u16, std.math.maxInt(u16), token);
                try self.replaceValue(falsePos, u16, self.u16Pos());

                if (i.else_branch == null) {
                    try self.writeOp(.nil, token);
                    try self.writeOp(.pop, token);
                    try self.replaceValue(jumpPos, u16, self.u16Pos() - 1);
                    return;
                }
                try self.compileBlock(i.else_branch.?);
                try self.replaceValue(jumpPos, u16, self.u16Pos() - 1);
            },
            .block => |b| try self.compileBlock(b),
            .expression => |exp| {
                try self.compileExpression(&exp);
                try self.writeOp(.pop, token);
            },
            .variable => |v| {
                try self.compileExpression(&v.initializer);
                var symbol = try self.symbols.define(v.name);
                try self.writeOp(.set_global, token);
                _ = try self.writeInt(OpCode.Type(.set_global), symbol.*.index, token);
            },
            else => {},
        }
    }

    pub fn removeLastPop(self: *Compiler) void {
        if (self.instructions.items[self.instructions.items.len - 1] == @intFromEnum(OpCode.pop)) {
            self.instructions.items = self.instructions.items[0 .. self.instructions.items.len - 1];
        }
    }

    pub fn compileBlock(self: *Compiler, stmts: []const ast.Statement) CompilerError!void {
        for (stmts) |stmt| {
            try self.compileStatement(stmt);
        }
    }

    pub fn compileExpression(self: *Compiler, expr: *const ast.Expression) CompilerError!void {
        var token = expr.token;
        switch (expr.type) {
            .binary => |bin| {
                if (bin.operator == .less_than) {
                    try self.compileExpression(bin.right);
                    try self.compileExpression(bin.left);
                    try self.writeOp(.greater_than, token);
                    return;
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
                    else => {
                        return error.IllegalOperation;
                    },
                };
                try self.writeOp(op, token);
            },
            .number => |n| {
                const i = try self.addConstant(.{ .number = n });
                try self.writeOp(.constant, token);
                _ = try self.writeInt(u16, i, token);
            },
            .boolean => |b| try self.writeOp(if (b) .true else .false, token),
            .string => |s| {
                // const i = try self.addConstant(.{ .string = try String.create(self.allocator, s.value) });
                const i = try self.addConstant(.{ .string = try self.allocator.dupe(u8, s.value) });
                try self.writeOp(.constant, token);
                _ = try self.writeInt(u16, i, token);
            },
            .unary => |u| {
                try self.compileExpression(u.value);
                switch (u.operator) {
                    .negate => try self.writeOp(.negate, token),
                    .not => try self.writeOp(.not, token),
                }
            },
            .identifier => |id| {
                var symbol = self.symbols.resolve(id);
                if (symbol) |ptr| {
                    try self.writeOp(.get_global, token);
                    _ = try self.writeInt(OpCode.Type(.get_global), ptr.*.index, token);
                    return;
                }
                return error.OutOfMemory;
            },
            .@"if" => |i| {
                try self.compileExpression(i.condition);
                try self.writeOp(.jump_if_false, token);
                // temp garbage value
                const pos = try self.writeInt(u16, std.math.maxInt(u16), token);
                try self.compileExpression(i.then_value);

                try self.writeOp(.jump, token);
                const nextPos = try self.writeInt(u16, std.math.maxInt(u16), token);
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

    fn writeOp(
        self: *Compiler,
        op: OpCode,
        token: Token,
    ) !void {
        try self.instructions.append(@intFromEnum(op));
        try DebugTokens.append(&self.tokens, token);
    }

    fn writeValue(self: *Compiler, buf: []const u8, token: Token) !void {
        try self.instructions.writer().writeAll(buf);
        var i: usize = 0;
        while (i < buf.len) : (i += 1) {
            try DebugTokens.append(&self.tokens, token);
        }
    }

    fn writeInt(self: *Compiler, comptime T: type, value: T, token: Token) !usize {
        var start = self.instructions.items.len;
        var buf: [@sizeOf(T)]u8 = undefined;
        std.mem.writeIntBig(T, buf[0..], value);
        try self.writeValue(&buf, token);
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

    pub fn print(code: ByteCode, tokens: ?[]DebugToken, writer: anytype) void {
        var i: usize = 0;
        writer.print("\n==BYTECODE==\n", .{});
        const instructions = code.instructions;
        const constants = code.constants;
        while (i < instructions.len) {
            writer.print("{d:0>4} ", .{i});
            if (tokens) |ts| {
                const token = DebugTokens.get(ts, i);
                // std.log.warn("{?}", .{token});
                if (token) |t| {
                    if (i > 0 and t.line == DebugTokens.get(ts, i - 1).?.line) {
                        writer.print("[{s}] ", .{"  | "});
                    } else {
                        writer.print("[{d:0>4}] ", .{t.line});
                    }
                }
            }
            const op = @enumFromInt(OpCode, instructions[i]);
            writer.print("{s: <16} ", .{op.toString()});
            i += 1;
            switch (op) {
                .jump, .jump_if_false, .set_global, .get_global => {
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
                @intFromEnum(OpCode.constant),
                0,
                0,
                @intFromEnum(OpCode.constant),
                0,
                1,
                @intFromEnum(OpCode.add),
                @intFromEnum(OpCode.pop),
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
        // Compiler.print(bytecode, compiler.lines.items, std.debug);

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
                @intFromEnum(OpCode.true),
                @intFromEnum(OpCode.jump_if_false),
                0,
                10,
                @intFromEnum(OpCode.constant),
                0,
                0,
                @intFromEnum(OpCode.jump),
                0,
                11,
                @intFromEnum(OpCode.nil),
                @intFromEnum(OpCode.pop),
                @intFromEnum(OpCode.constant),
                0,
                1,
                @intFromEnum(OpCode.pop),
            },
        },
        .{
            .input = "if (true) { 10 } else { 20 } 333",
            .expectedConstants = [_]Value{ .{ .number = 10 }, .{ .number = 20 }, .{ .number = 333 } },
            .expectedInstructions = [_]u8{
                @intFromEnum(OpCode.true),
                @intFromEnum(OpCode.jump_if_false),
                0,
                10,
                @intFromEnum(OpCode.constant),
                0,
                0,
                @intFromEnum(OpCode.jump),
                0,
                13,
                @intFromEnum(OpCode.constant),
                0,
                1,
                @intFromEnum(OpCode.pop),
                @intFromEnum(OpCode.constant),
                0,
                2,
                @intFromEnum(OpCode.pop),
            },
        },
    };

    inline for (test_cases) |case| {
        errdefer std.log.warn("{s}", .{case.input});
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

        // Compiler.print(bytecode, compiler.lines.items, std.debug);
        for (case.expectedInstructions, 0..) |instruction, i| {
            errdefer std.log.warn("{}", .{i});
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

test "Variables" {
    const test_cases = .{
        .{
            .input =
            \\ var one = 1
            \\ var two = 2
            ,
            .expectedConstants = [_]Value{ .{ .number = 1 }, .{ .number = 2 } },
            .expectedInstructions = [_]u8{
                @intFromEnum(OpCode.constant),
                0,
                0,
                @intFromEnum(OpCode.set_global),
                0,
                0,
                @intFromEnum(OpCode.constant),
                0,
                1,
                @intFromEnum(OpCode.set_global),
                0,
                1,
            },
        },
        .{
            .input =
            \\ var one = 1
            \\ var two = one
            \\ two
            ,
            .expectedConstants = [_]Value{.{ .number = 1 }},
            .expectedInstructions = [_]u8{
                @intFromEnum(OpCode.constant),
                0,
                0,
                @intFromEnum(OpCode.set_global),
                0,
                0,
                @intFromEnum(OpCode.get_global),
                0,
                0,
                @intFromEnum(OpCode.set_global),
                0,
                1,
                @intFromEnum(OpCode.get_global),
                0,
                1,
                @intFromEnum(OpCode.pop),
            },
        },
    };

    inline for (test_cases) |case| {
        errdefer std.log.warn("{s}", .{case.input});
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

        // Compiler.print(bytecode, compiler.lines.items, std.debug);
        for (case.expectedInstructions, 0..) |instruction, i| {
            errdefer std.log.warn("{}", .{i});
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

test "Strings" {
    const test_cases = .{
        .{
            .input = "\"testing\"",
            .expectedConstants = [_][]const u8{"testing"},
            .expectedInstructions = [_]u8{
                @intFromEnum(OpCode.constant),
                0,
                0,
                @intFromEnum(OpCode.pop),
            },
        },
        .{
            .input = "\"test\" + \"ing\"",
            .expectedConstants = [_][]const u8{ "test", "ing" },
            .expectedInstructions = [_]u8{
                @intFromEnum(OpCode.constant),
                0,
                0,
                @intFromEnum(OpCode.constant),
                0,
                1,
                @intFromEnum(OpCode.add),
                @intFromEnum(OpCode.pop),
            },
        },
    };

    inline for (test_cases) |case| {
        errdefer std.log.warn("{s}", .{case.input});
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

        Compiler.print(bytecode, bytecode.tokens, std.debug);
        for (case.expectedInstructions, 0..) |instruction, i| {
            errdefer std.log.warn("{}", .{i});
            try testing.expectEqual(instruction, bytecode.instructions[i]);
        }
        for (case.expectedConstants, 0..) |constant, i| {
            try testing.expectEqualStrings(constant, bytecode.constants[i].string);
        }
    }
}
