const std = @import("std");
const ast = @import("./ast.zig");
const parser = @import("./parser.zig");
const Token = @import("./token.zig").Token;
const OpCode = @import("./opcode.zig").OpCode;
const Errors = @import("./error.zig").Errors;
const Value = @import("./values.zig").Value;
const ValueType = @import("./values.zig").Type;
const String = @import("./values.zig").String;
const SymbolTable = @import("./symbols.zig").SymbolTable;
const Symbol = @import("./symbols.zig").Symbol;
const Scope = @import("./scope.zig").Scope;
const DebugToken = @import("./debug.zig").DebugToken;

const testing = std.testing;

const CompilerError = error{
    IllegalOperation,
    OutOfMemory,
    OutOfScope,
};

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
        var i: usize = 0;
        writer.print("\n==BYTECODE==\n", .{});
        const instructions = code.instructions;
        const constants = code.constants;
        const tokens = code.tokens;
        while (i < instructions.len) {
            writer.print("{d:0>4} ", .{i});
            const token = DebugToken.get(tokens, i);
            if (token) |t| {
                if (i > 0 and t.line == DebugToken.get(tokens, i - 1).?.line) {
                    writer.print("[{s}] ", .{"  | "});
                } else {
                    writer.print("[{d:0>4}] ", .{t.line});
                }
            }
            const op = @enumFromInt(OpCode, instructions[i]);
            writer.print("{s: <16} ", .{op.toString()});
            i += 1;
            switch (op) {
                .jump, .jump_if_false, .set_global, .get_global, .list, .map, .set => {
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

pub const Compiler = struct {
    allocator: std.mem.Allocator,

    constants: std.ArrayList(Value),
    symbols: SymbolTable,
    scope: ?*Scope = null,

    pub fn init(allocator: std.mem.Allocator) !Compiler {
        var root_scope = try Scope.create(allocator, null, .global);
        return .{
            .allocator = allocator,
            .constants = std.ArrayList(Value).init(allocator),
            .symbols = SymbolTable.init(allocator),
            .scope = root_scope,
        };
    }

    pub fn deinit(self: *Compiler) void {
        self.scope.?.destroy();
        self.constants.deinit();
        self.symbols.deinit();
    }

    pub fn bytecode(self: *Compiler) !ByteCode {
        var scope = try self.currentScope();
        return .{
            .instructions = try scope.instructions.toOwnedSlice(),
            .constants = try self.constants.toOwnedSlice(),
            .tokens = try scope.debug_tokens.toOwnedSlice(),
        };
    }

    fn currentScope(self: *Compiler) !*Scope {
        if (self.scope) |scope| return scope;
        return error.OutOfScope;
    }

    pub fn compile(self: *Compiler, tree: ast.Tree) CompilerError!void {
        for (tree.root) |stmt| {
            try self.compileStatement(stmt);
        }
    }

    // used for testing
    pub fn compileSource(self: *Compiler, source: []const u8) !void {
        var errors = Errors.init(self.allocator);
        defer errors.deinit();
        const tree = parser.parse(self.allocator, source, &errors) catch |err| {
            try errors.write(source, std.io.getStdErr().writer());
            return err;
        };
        defer tree.deinit();
        try self.compile(tree);
    }

    fn enterScope(self: *Compiler, tag: Scope.Tag) !void {
        self.scope = try Scope.create(self.allocator, try self.currentScope(), tag);
    }

    fn exitScope(self: *Compiler) !struct { instructions: []u8, debug_tokens: []DebugToken } {
        const old_scope = try self.currentScope();
        const result = .{
            .instructions = try old_scope.instructions.toOwnedSlice(),
            .debug_tokens = try old_scope.debug_tokens.toOwnedSlice(),
        };
        self.scope = old_scope.parent;
        old_scope.destroy();
        return result;
    }

    pub fn compileStatement(self: *Compiler, stmt: ast.Statement) CompilerError!void {
        var token = stmt.token;
        switch (stmt.type) {
            .@"if" => |i| {
                try self.compileExpression(i.condition);
                try self.writeOp(.jump_if_false, token);
                const falsePos = try self.writeInt(u16, std.math.maxInt(u16), token);
                try self.compileBlock(i.then_branch);
                try self.removeLastPop();

                try self.writeOp(.jump, token);
                const jumpPos = try self.writeInt(u16, std.math.maxInt(u16), token);
                try self.replaceValue(falsePos, u16, self.scopePos());

                if (i.else_branch == null) {
                    try self.writeOp(.nil, token);
                    try self.writeOp(.pop, token);
                    try self.replaceValue(jumpPos, u16, self.scopePos() - 1);
                    return;
                }
                try self.compileBlock(i.else_branch.?);
                try self.replaceValue(jumpPos, u16, self.scopePos() - 1);
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
                _ = try self.writeInt(OpCode.Size(.set_global), symbol.*.index, token);
            },
            .return_expression => |r| {
                try self.compileExpression(&r);
                try self.writeOp(.return_value, token);
            },
            .return_void => {
                try self.writeOp(.return_void, token);
            },
            else => {},
        }
    }

    fn lastIs(self: *Compiler, op: OpCode) !bool {
        var scope = try self.currentScope();
        var inst = scope.instructions;
        const last = inst.getLastOrNull();
        if (last == null) return false;
        return last.? == @intFromEnum(op);
    }

    fn removeLastPop(self: *Compiler) !void {
        if (try self.lastIs(.pop)) {
            var scope = try self.currentScope();
            var inst = scope.instructions;
            scope.instructions.items = inst.items[0 .. inst.items.len - 1];
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
                _ = try self.writeInt(OpCode.Size(.constant), i, token);
            },
            .boolean => |b| try self.writeOp(if (b) .true else .false, token),
            .string => |s| {
                for (s.expressions) |*item| {
                    try self.compileExpression(item);
                }
                const obj = try self.allocator.create(Value.Obj);
                obj.* = .{ .data = .{ .string = try self.allocator.dupe(u8, s.value) } };
                const i = try self.addConstant(.{ .obj = obj });
                try self.writeOp(.string, token);
                _ = try self.writeInt(OpCode.Size(.constant), i, token);
                _ = try self.writeInt(OpCode.Size(.list), @intCast(u16, s.expressions.len), token);
            },
            .list => |l| {
                for (l) |*item| {
                    try self.compileExpression(item);
                }
                try self.writeOp(.list, token);
                const size = OpCode.Size(.list);
                const length = @intCast(size, l.len);
                _ = try self.writeInt(size, length, token);
            },
            .map => |m| {
                for (m) |*mp| {
                    try self.compileExpression(mp);
                }
                const size = OpCode.Size(.map);
                try self.writeOp(.map, token);
                const length = @intCast(size, m.len);
                _ = try self.writeInt(size, length, token);
            },
            .set => |s| {
                for (s) |*item| {
                    try self.compileExpression(item);
                }
                const size = OpCode.Size(.set);
                try self.writeOp(.set, token);
                const length = @intCast(size, s.len);
                _ = try self.writeInt(size, length, token);
            },
            .map_pair => |mp| {
                try self.compileExpression(mp.key);
                try self.compileExpression(mp.value);
            },
            .indexer => |idx| {
                try self.compileExpression(idx.target);
                try self.compileExpression(idx.index);
                try self.writeOp(.index, token);
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
                    _ = try self.writeInt(OpCode.Size(.get_global), ptr.*.index, token);
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
                try self.replaceValue(pos, u16, self.scopePos());

                try self.compileExpression(i.else_value);
                try self.replaceValue(nextPos, u16, self.scopePos());
            },
            .function => |f| {
                try self.enterScope(.function);
                try self.compileBlock(f.body);
                if (!(try self.lastIs(.return_value)) and !(try self.lastIs(.return_void))) {
                    try self.writeOp(.return_void, token);
                }
                const inst = try self.exitScope();
                const obj = try self.allocator.create(Value.Obj);

                // TODO: use debug tokens
                self.allocator.free(inst.debug_tokens);

                obj.* = .{ .data = .{ .function = inst.instructions } };
                const i = try self.addConstant(.{ .obj = obj });

                try self.writeOp(.constant, token);
                _ = try self.writeInt(OpCode.Size(.constant), i, token);
            },
            .call => |c| {
                try self.compileExpression(c.target);
                try self.writeOp(.call, token);
            },
            else => {},
        }
    }

    fn scopePos(self: *Compiler) u16 {
        return @intCast(u16, self.scope.?.instructions.items.len);
    }

    fn writeOp(self: *Compiler, op: OpCode, token: Token) !void {
        var scope = try self.currentScope();
        try scope.instructions.append(@intFromEnum(op));
        try DebugToken.add(&scope.debug_tokens, token);
    }

    fn writeValue(self: *Compiler, buf: []const u8, token: Token) !void {
        var scope = try self.currentScope();
        try scope.instructions.writer().writeAll(buf);
        var i: usize = 0;
        while (i < buf.len) : (i += 1) {
            try DebugToken.add(&scope.debug_tokens, token);
        }
    }

    fn writeInt(self: *Compiler, comptime T: type, value: T, token: Token) !usize {
        var scope = try self.currentScope();
        var start = scope.instructions.items.len;
        var buf: [@sizeOf(T)]u8 = undefined;
        std.mem.writeIntBig(T, buf[0..], value);
        try self.writeValue(&buf, token);
        return start;
    }

    pub fn replaceValue(self: *Compiler, pos: usize, comptime T: type, value: T) !void {
        var buf: [@sizeOf(T)]u8 = undefined;
        std.mem.writeIntBig(T, buf[0..], value);
        var scope = try self.currentScope();
        for (buf, 0..) |v, i| {
            scope.instructions.items[pos + i] = v;
        }
    }

    pub fn addConstant(self: *Compiler, value: Value) !u16 {
        try self.constants.append(value);
        return @intCast(u16, self.constants.items.len - 1);
    }
};

test "Basic Compile" {
    const test_cases = .{
        .{
            .input = "1 + 2",
            .expectedConstants = [_]Value{ .{ .number = 1 }, .{ .number = 2 } },
            .expectedInstructions = [_]u8{ @intFromEnum(OpCode.constant), 0, 0, @intFromEnum(OpCode.constant), 0, 1, @intFromEnum(OpCode.add), @intFromEnum(OpCode.pop) },
        },
    };

    inline for (test_cases) |case| {
        var allocator = testing.allocator;
        var compiler = try Compiler.init(allocator);
        defer compiler.deinit();
        try compiler.compileSource(case.input);

        var bytecode = try compiler.bytecode();
        defer bytecode.free(testing.allocator);

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
            .expectedInstructions = [_]u8{ @intFromEnum(OpCode.true), @intFromEnum(OpCode.jump_if_false), 0, 10, @intFromEnum(OpCode.constant), 0, 0, @intFromEnum(OpCode.jump), 0, 13, @intFromEnum(OpCode.constant), 0, 1, @intFromEnum(OpCode.pop), @intFromEnum(OpCode.constant), 0, 2, @intFromEnum(OpCode.pop) },
        },
    };

    inline for (test_cases) |case| {
        errdefer std.log.warn("{s}", .{case.input});
        var allocator = testing.allocator;
        var compiler = try Compiler.init(allocator);
        defer compiler.deinit();
        try compiler.compileSource(case.input);

        var bytecode = try compiler.bytecode();
        defer bytecode.free(testing.allocator);

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
            .expectedInstructions = [_]u8{ @intFromEnum(OpCode.constant), 0, 0, @intFromEnum(OpCode.set_global), 0, 0, @intFromEnum(OpCode.constant), 0, 1, @intFromEnum(OpCode.set_global), 0, 1 },
        },
        .{
            .input =
            \\ var one = 1
            \\ var two = one
            \\ two
            ,
            .expectedConstants = [_]Value{.{ .number = 1 }},
            .expectedInstructions = [_]u8{ @intFromEnum(OpCode.constant), 0, 0, @intFromEnum(OpCode.set_global), 0, 0, @intFromEnum(OpCode.get_global), 0, 0, @intFromEnum(OpCode.set_global), 0, 1, @intFromEnum(OpCode.get_global), 0, 1, @intFromEnum(OpCode.pop) },
        },
    };

    inline for (test_cases) |case| {
        errdefer std.log.warn("{s}", .{case.input});
        var allocator = testing.allocator;
        var compiler = try Compiler.init(allocator);
        defer compiler.deinit();
        try compiler.compileSource(case.input);

        var bytecode = try compiler.bytecode();
        defer bytecode.free(testing.allocator);

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
            .expectedInstructions = [_]u8{ @intFromEnum(OpCode.string), 0, 0, 0, 0, @intFromEnum(OpCode.pop) },
        },
        .{
            .input = "\"test\" + \"ing\"",
            .expectedConstants = [_][]const u8{ "test", "ing" },
            .expectedInstructions = [_]u8{
                @intFromEnum(OpCode.string),
                0,
                0,
                0,
                0,
                @intFromEnum(OpCode.string),
                0,
                1,
                0,
                0,
                @intFromEnum(OpCode.add),
                @intFromEnum(OpCode.pop),
            },
        },
    };

    inline for (test_cases) |case| {
        errdefer std.log.warn("{s}", .{case.input});
        var allocator = testing.allocator;
        var compiler = try Compiler.init(allocator);
        defer compiler.deinit();
        try compiler.compileSource(case.input);

        var bytecode = try compiler.bytecode();
        defer bytecode.free(testing.allocator);

        for (case.expectedInstructions, 0..) |instruction, i| {
            errdefer std.log.warn("{}", .{i});
            try testing.expectEqual(instruction, bytecode.instructions[i]);
        }
        for (case.expectedConstants, 0..) |constant, i| {
            try testing.expectEqualStrings(constant, bytecode.constants[i].obj.data.string);
        }
    }
}

test "Lists" {
    const test_cases = .{
        .{
            .input = "[]",
            .expectedConstants = [_]f32{},
            .expectedInstructions = [_]u8{ @intFromEnum(OpCode.list), 0, 0, @intFromEnum(OpCode.pop) },
        },
        .{
            .input = "[1,2,3]",
            .expectedConstants = [_]f32{ 1, 2, 3 },
            .expectedInstructions = [_]u8{ @intFromEnum(OpCode.constant), 0, 0, @intFromEnum(OpCode.constant), 0, 1, @intFromEnum(OpCode.constant), 0, 2, @intFromEnum(OpCode.list), 0, 3, @intFromEnum(OpCode.pop) },
        },
        .{
            .input = "[1 + 2, 3 - 4, 5 * 6]",
            .expectedConstants = [_]f32{ 1, 2, 3, 4, 5, 6 },
            .expectedInstructions = [_]u8{ @intFromEnum(OpCode.constant), 0, 0, @intFromEnum(OpCode.constant), 0, 1, @intFromEnum(OpCode.add), @intFromEnum(OpCode.constant), 0, 2, @intFromEnum(OpCode.constant), 0, 3, @intFromEnum(OpCode.subtract), @intFromEnum(OpCode.constant), 0, 4, @intFromEnum(OpCode.constant), 0, 5, @intFromEnum(OpCode.multiply), @intFromEnum(OpCode.list), 0, 3, @intFromEnum(OpCode.pop) },
        },
    };

    inline for (test_cases) |case| {
        var allocator = testing.allocator;
        var compiler = try Compiler.init(allocator);
        defer compiler.deinit();
        try compiler.compileSource(case.input);

        var bytecode = try compiler.bytecode();
        defer bytecode.free(testing.allocator);

        for (case.expectedInstructions, 0..) |instruction, i| {
            errdefer std.log.warn("{}", .{i});
            try testing.expectEqual(instruction, bytecode.instructions[i]);
        }

        for (case.expectedConstants, 0..) |constant, i| {
            try testing.expect(constant == bytecode.constants[i].number);
        }
    }
}

test "Maps and Sets" {
    // {} denotes a group as well as a map/set,
    // wrap it in a () to force group expression statements
    const test_cases = .{
        .{
            .input = "({:})",
            .expectedConstants = [_]f32{},
            .expectedInstructions = [_]u8{ @intFromEnum(OpCode.map), 0, 0, @intFromEnum(OpCode.pop) },
        },
        .{
            .input = "({1: 2, 3: 4, 5: 6})",
            .expectedConstants = [_]f32{ 1, 2, 3, 4, 5, 6 },
            .expectedInstructions = [_]u8{ @intFromEnum(OpCode.constant), 0, 0, @intFromEnum(OpCode.constant), 0, 1, @intFromEnum(OpCode.constant), 0, 2, @intFromEnum(OpCode.constant), 0, 3, @intFromEnum(OpCode.constant), 0, 4, @intFromEnum(OpCode.constant), 0, 5, @intFromEnum(OpCode.map), 0, 3, @intFromEnum(OpCode.pop) },
        },
        .{
            .input = "({1: 2 + 3, 4: 5 * 6})",
            .expectedConstants = [_]f32{ 1, 2, 3, 4, 5, 6 },
            .expectedInstructions = [_]u8{ @intFromEnum(OpCode.constant), 0, 0, @intFromEnum(OpCode.constant), 0, 1, @intFromEnum(OpCode.constant), 0, 2, @intFromEnum(OpCode.add), @intFromEnum(OpCode.constant), 0, 3, @intFromEnum(OpCode.constant), 0, 4, @intFromEnum(OpCode.constant), 0, 5, @intFromEnum(OpCode.multiply), @intFromEnum(OpCode.map), 0, 2, @intFromEnum(OpCode.pop) },
        },
        .{
            .input = "({})",
            .expectedConstants = [_]f32{},
            .expectedInstructions = [_]u8{ @intFromEnum(OpCode.set), 0, 0, @intFromEnum(OpCode.pop) },
        },
        .{
            .input = "({1, 2, 3})",
            .expectedConstants = [_]f32{ 1, 2, 3 },
            .expectedInstructions = [_]u8{ @intFromEnum(OpCode.constant), 0, 0, @intFromEnum(OpCode.constant), 0, 1, @intFromEnum(OpCode.constant), 0, 2, @intFromEnum(OpCode.set), 0, 3, @intFromEnum(OpCode.pop) },
        },
        .{
            .input = "({1 + 2, 3 * 4, 5 - 6})",
            .expectedConstants = [_]f32{ 1, 2, 3, 4, 5, 6 },
            .expectedInstructions = [_]u8{ @intFromEnum(OpCode.constant), 0, 0, @intFromEnum(OpCode.constant), 0, 1, @intFromEnum(OpCode.add), @intFromEnum(OpCode.constant), 0, 2, @intFromEnum(OpCode.constant), 0, 3, @intFromEnum(OpCode.multiply), @intFromEnum(OpCode.constant), 0, 4, @intFromEnum(OpCode.constant), 0, 5, @intFromEnum(OpCode.subtract), @intFromEnum(OpCode.set), 0, 3, @intFromEnum(OpCode.pop) },
        },
    };

    inline for (test_cases) |case| {
        // std.log.warn("{s}", .{case.input});
        var allocator = testing.allocator;
        var arena_inst = std.heap.ArenaAllocator.init(allocator);
        defer arena_inst.deinit();
        const arena = arena_inst.allocator();
        var compiler = try Compiler.init(arena);
        defer compiler.deinit();
        try compiler.compileSource(case.input);

        var bytecode = try compiler.bytecode();
        defer bytecode.free(arena);
        // bytecode.print(std.debug);
        for (case.expectedInstructions, 0..) |instruction, i| {
            errdefer std.log.warn("Error on: {}", .{i});
            try testing.expectEqual(instruction, bytecode.instructions[i]);
        }

        for (case.expectedConstants, 0..) |constant, i| {
            try testing.expect(constant == bytecode.constants[i].number);
        }
    }
}

test "Index" {
    const test_cases = .{
        .{
            .input = "[1,2,3][1 + 1]",
            .expectedConstants = [_]f32{ 1, 2, 3, 1, 1 },
            .expectedInstructions = [_]u8{ @intFromEnum(OpCode.constant), 0, 0, @intFromEnum(OpCode.constant), 0, 1, @intFromEnum(OpCode.constant), 0, 2, @intFromEnum(OpCode.list), 0, 3, @intFromEnum(OpCode.constant), 0, 3, @intFromEnum(OpCode.constant), 0, 4, @intFromEnum(OpCode.add), @intFromEnum(OpCode.index), @intFromEnum(OpCode.pop) },
        },
        .{
            .input = "({1: 2})[2 - 1]",
            .expectedConstants = [_]f32{ 1, 2, 2, 1 },
            .expectedInstructions = [_]u8{ @intFromEnum(OpCode.constant), 0, 0, @intFromEnum(OpCode.constant), 0, 1, @intFromEnum(OpCode.map), 0, 1, @intFromEnum(OpCode.constant), 0, 2, @intFromEnum(OpCode.constant), 0, 3, @intFromEnum(OpCode.subtract), @intFromEnum(OpCode.index), @intFromEnum(OpCode.pop) },
        },
    };

    inline for (test_cases) |case| {
        // std.log.warn("{s}", .{case.input});
        var allocator = testing.allocator;
        var arena_inst = std.heap.ArenaAllocator.init(allocator);
        defer arena_inst.deinit();
        const arena = arena_inst.allocator();
        var compiler = try Compiler.init(arena);
        defer compiler.deinit();
        try compiler.compileSource(case.input);

        var bytecode = try compiler.bytecode();
        defer bytecode.free(arena);
        // bytecode.print(std.debug);
        for (case.expectedInstructions, 0..) |instruction, i| {
            errdefer std.log.warn("Error on: {}", .{i});
            try testing.expectEqual(instruction, bytecode.instructions[i]);
        }

        for (case.expectedConstants, 0..) |constant, i| {
            try testing.expect(constant == bytecode.constants[i].number);
        }
    }
}

test "Functions" {
    var test_cases = .{
        .{
            .input = "|| return 5 + 10",
            .expectedInstructions = [_]u8{ @intFromEnum(OpCode.constant), 0, 2, @intFromEnum(OpCode.pop) },
            .expectedConstants = [_]f32{ 5, 10 },
            .expectedFunctions = [_][]const u8{
                &[_]u8{},
                &[_]u8{},
                &[_]u8{
                    @intFromEnum(OpCode.constant), 0,                                 0,
                    @intFromEnum(OpCode.constant), 0,                                 1,
                    @intFromEnum(OpCode.add),      @intFromEnum(OpCode.return_value),
                },
            },
        },
        .{
            .input = "|| 5 + 10",
            .expectedInstructions = [_]u8{ @intFromEnum(OpCode.constant), 0, 2, @intFromEnum(OpCode.pop) },
            .expectedConstants = [_]f32{ 5, 10 },
            .expectedFunctions = [_][]const u8{
                &[_]u8{},
                &[_]u8{},
                &[_]u8{
                    @intFromEnum(OpCode.constant),
                    0,
                    0,
                    @intFromEnum(OpCode.constant),
                    0,
                    1,
                    @intFromEnum(OpCode.add),
                    @intFromEnum(OpCode.pop),
                    @intFromEnum(OpCode.return_void),
                },
            },
        },
        .{
            .input = "|| { 5 + 10 return }",
            .expectedInstructions = [_]u8{ @intFromEnum(OpCode.constant), 0, 2, @intFromEnum(OpCode.pop) },
            .expectedConstants = [_]f32{ 5, 10 },
            .expectedFunctions = [_][]const u8{
                &[_]u8{},
                &[_]u8{},
                &[_]u8{
                    @intFromEnum(OpCode.constant), 0,                        0,
                    @intFromEnum(OpCode.constant), 0,                        1,
                    @intFromEnum(OpCode.add),      @intFromEnum(OpCode.pop), @intFromEnum(OpCode.return_void),
                },
            },
        },
        .{
            .input = "|| {}",
            .expectedInstructions = [_]u8{ @intFromEnum(OpCode.constant), 0, 0, @intFromEnum(OpCode.pop) },
            .expectedConstants = [_]f32{ 0, 0, 0 },
            .expectedFunctions = [_][]const u8{
                &[_]u8{
                    @intFromEnum(OpCode.return_void),
                },
            },
        },
        .{
            .input = "|| { return 5 }()",
            .expectedInstructions = [_]u8{ @intFromEnum(OpCode.constant), 0, 1, @intFromEnum(OpCode.call), @intFromEnum(OpCode.pop) },
            .expectedConstants = [_]f32{5},
            .expectedFunctions = [_][]const u8{
                &[_]u8{},
                &[_]u8{
                    @intFromEnum(OpCode.constant),     0, 0,
                    @intFromEnum(OpCode.return_value),
                },
            },
        },
        .{
            .input =
            \\ const five = || return 5
            \\ five()
            ,
            .expectedInstructions = [_]u8{
                @intFromEnum(OpCode.constant),
                0,
                1,
                @intFromEnum(OpCode.set_global),
                0,
                0,
                @intFromEnum(OpCode.get_global),
                0,
                0,
                @intFromEnum(OpCode.call),
                @intFromEnum(OpCode.pop),
            },
            .expectedConstants = [_]f32{5},
            .expectedFunctions = [_][]const u8{
                &[_]u8{},
                &[_]u8{
                    @intFromEnum(OpCode.constant),     0, 0,
                    @intFromEnum(OpCode.return_value),
                },
            },
        },
    };

    inline for (test_cases) |case| {
        // std.log.warn("{s}", .{case.input});
        var allocator = testing.allocator;
        var arena_inst = std.heap.ArenaAllocator.init(allocator);
        defer arena_inst.deinit();
        const arena = arena_inst.allocator();
        var compiler = try Compiler.init(arena);
        defer compiler.deinit();
        try compiler.compileSource(case.input);

        var bytecode = try compiler.bytecode();
        defer bytecode.free(arena);
        // bytecode.print(std.debug);
        for (case.expectedInstructions, 0..) |instruction, i| {
            errdefer std.log.warn("Error on: {}", .{i});
            try testing.expectEqual(instruction, bytecode.instructions[i]);
        }
        for (bytecode.constants, 0..) |constant, i| {
            switch (constant) {
                .number => |n| try testing.expect(case.expectedConstants[i] == n),
                .obj => |o| try testing.expectEqualSlices(u8, case.expectedFunctions[i], o.data.function),
                else => unreachable,
            }
        }
    }
}
