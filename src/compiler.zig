const std = @import("std");
const ast = @import("./ast.zig");
const parser = @import("./parser.zig");
const Token = @import("./token.zig").Token;
const OpCode = @import("./opcode.zig").OpCode;
const Errors = @import("./error.zig").Errors;
const Value = @import("./values.zig").Value;
const ValueType = @import("./values.zig").Type;
const String = @import("./values.zig").String;
const Scope = @import("./scope.zig").Scope;
const Symbol = @import("./scope.zig").Symbol;
const DebugToken = @import("./debug.zig").DebugToken;
const builtins = @import("./builtins.zig").builtins;

const testing = std.testing;
const BREAK_HOLDER = 9000;
const CONTINUE_HOLDER = 9001;

const CompilerError = error{
    IllegalOperation,
    OutOfMemory,
    OutOfScope,
    SymbolNotFound,
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
        writer.print("\n==BYTECODE==\n", .{});
        printInstructions(writer, code.instructions, code.constants);
    }

    pub fn printInstructions(writer: anytype, instructions: []const u8, constants: []Value) void {
        var i: usize = 0;
        while (i < instructions.len) {
            writer.print("{d:0>4} ", .{i});
            const op = @enumFromInt(OpCode, instructions[i]);
            writer.print("{s: <16} ", .{op.toString()});
            i += 1;
            switch (op) {
                .jump, .jump_if_false, .set_global, .get_global, .list, .map, .set => {
                    var dest = std.mem.readIntSliceBig(u16, instructions[i..(i + 2)]);
                    writer.print("{d: >8}", .{dest});
                    i += 2;
                },
                .get_local, .set_local, .get_free, .call => {
                    var dest = std.mem.readIntSliceBig(u8, instructions[i..(i + 1)]);
                    writer.print("{d: >8}", .{dest});
                    i += 1;
                },
                .constant => {
                    var index = std.mem.readIntSliceBig(u16, instructions[i..(i + 2)]);
                    writer.print("{d: >8} ", .{index});
                    i += 2;
                    var value = constants[index];
                    writer.print("  = ", .{});
                    value.print(writer, constants);
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
                    var value = constants[index];
                    value.print(writer, constants);
                },
                else => {},
            }
            writer.print("\n", .{});
        }
    }
};

pub fn compileSource(allocator: std.mem.Allocator, source: []const u8, errors: *Errors) !ByteCode {
    const tree = try parser.parse(allocator, source, errors);
    defer tree.deinit();

    var root_scope = try Scope.create(allocator, null, .global);
    defer root_scope.destroy();
    var compiler = Compiler.init(allocator, root_scope, errors);
    defer compiler.deinit();

    try compiler.compile(tree);
    return try compiler.bytecode();
}

pub const Compiler = struct {
    allocator: std.mem.Allocator,
    builtins: Scope,
    constants: std.ArrayList(Value),
    errors: *Errors,
    scope: *Scope,
    speakers: std.StringHashMap(OpCode.Size(.constant)),

    pub fn init(allocator: std.mem.Allocator, root_scope: *Scope, errors: *Errors) Compiler {
        return .{
            .allocator = allocator,
            .builtins = .{
                .allocator = allocator,
                .parent = null,
                .symbols = std.StringArrayHashMap(*Symbol).init(allocator),
                .tag = .builtin,
                .instructions = undefined,
                .debug_tokens = undefined,
            },
            .constants = std.ArrayList(Value).init(allocator),
            .speakers = std.StringHashMap(OpCode.Size(.constant)).init(allocator),
            .errors = errors,
            .scope = root_scope,
        };
    }

    pub fn deinit(self: *Compiler) void {
        self.constants.deinit();
        for (self.builtins.symbols.values()) |s| {
            self.allocator.destroy(s);
        }
        self.speakers.deinit();
        self.builtins.symbols.deinit();
    }

    pub fn bytecode(self: *Compiler) !ByteCode {
        var scope = self.scope;
        return .{
            .instructions = try scope.instructions.toOwnedSlice(),
            .constants = try self.constants.toOwnedSlice(),
            .tokens = try scope.debug_tokens.toOwnedSlice(),
        };
    }

    pub fn compile(self: *Compiler, tree: ast.Tree) CompilerError!void {
        inline for (builtins) |builtin| {
            _ = try self.builtins.define(builtin.name);
        }
        for (tree.root) |stmt| {
            try self.compileStatement(stmt);
        }
    }

    fn enterScope(self: *Compiler, tag: Scope.Tag) !void {
        self.scope = try Scope.create(self.allocator, self.scope, tag);
    }

    fn exitScope(self: *Compiler) !struct { instructions: []u8, debug_tokens: []DebugToken } {
        const old_scope = self.scope;
        const result = .{
            .instructions = try old_scope.instructions.toOwnedSlice(),
            .debug_tokens = try old_scope.debug_tokens.toOwnedSlice(),
        };
        self.scope = old_scope.parent orelse return CompilerError.OutOfScope;
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
            .@"break" => {
                try self.writeOp(.jump, token);
                _ = try self.writeInt(u16, BREAK_HOLDER, token);
            },
            .@"continue" => {
                try self.writeOp(.jump, token);
                _ = try self.writeInt(u16, CONTINUE_HOLDER, token);
            },
            .@"while" => |w| {
                try self.enterScope(.closure);

                const start = self.scopePos();
                try self.compileExpression(&w.condition);
                try self.writeOp(.jump_if_false, token);
                // temp garbage value
                const pos = try self.writeInt(u16, std.math.maxInt(u16), token);

                try self.compileBlock(w.body);
                try self.removeLastPop();
                const locals_count = self.scope.symbols.count();
                const free_symbols = try self.scope.free_symbols.toOwnedSlice();
                defer self.allocator.free(free_symbols);

                try self.writeOp(.jump, token);
                _ = try self.writeInt(OpCode.Size(.jump), start, token);

                const end = self.scopePos();
                try self.writeOp(.return_void, token);

                try self.replaceValue(pos, u16, end);

                var inst = try self.exitScope();
                for (free_symbols) |s| {
                    try self.loadSymbol(s, token);
                }
                try replaceJumps(inst.instructions, BREAK_HOLDER, end);
                try replaceJumps(inst.instructions, CONTINUE_HOLDER, start);

                const obj = try self.allocator.create(Value.Obj);

                // TODO: use debug tokens
                self.allocator.free(inst.debug_tokens);

                obj.* = .{
                    .data = .{
                        .loop = .{
                            .instructions = inst.instructions,
                            .locals_count = locals_count,
                        },
                    },
                };
                const i = try self.addConstant(.{ .obj = obj });
                try self.writeOp(.closure, token);
                _ = try self.writeInt(OpCode.Size(.constant), i, token);
                _ = try self.writeInt(u8, @intCast(u8, free_symbols.len), token);

                try self.writeOp(.loop, token);
            },
            .@"for" => |f| {
                try self.enterScope(.closure);
                try self.compileStatement(f.index.*);

                const start = self.scopePos();
                try self.compileExpression(&f.condition);
                try self.compileExpression(&f.iterator);
                _ = try self.scope.define(f.capture);

                try self.compileBlock(f.body);
                try self.removeLastPop();
                const locals_count = self.scope.symbols.count();

                try self.compileExpression(&f.increment);
                try self.writeOp(.jump, token);
                _ = try self.writeInt(OpCode.Size(.jump), start, token);

                const end = self.scopePos();
                try self.writeOp(.return_void, token);

                var inst = try self.exitScope();

                try replaceJumps(inst.instructions, BREAK_HOLDER, end);
                try replaceJumps(inst.instructions, CONTINUE_HOLDER, start);

                const obj = try self.allocator.create(Value.Obj);

                // TODO: use debug tokens
                self.allocator.free(inst.debug_tokens);

                obj.* = .{
                    .data = .{
                        .loop = .{
                            .instructions = inst.instructions,
                            .locals_count = locals_count,
                        },
                    },
                };
                const i = try self.addConstant(.{ .obj = obj });
                try self.writeOp(.constant, token);
                _ = try self.writeInt(OpCode.Size(.constant), i, token);

                try self.writeOp(.loop, token);
            },
            .variable => |v| {
                if (self.builtins.symbols.contains(v.name)) return CompilerError.IllegalOperation;
                var symbol = try self.scope.define(v.name);
                try self.compileExpression(&v.initializer);
                if (symbol.tag == .global) {
                    try self.writeOp(.set_global, token);
                    _ = try self.writeInt(OpCode.Size(.set_global), symbol.index, token);
                } else {
                    try self.writeOp(.set_local, token);
                    const size = OpCode.Size(.set_local);
                    _ = try self.writeInt(size, @intCast(size, symbol.index), token);
                }
            },
            .bough => |b| {
                var symbol = try self.scope.define(b.name);

                try self.enterScope(.closure);
                try self.compileBlock(b.body);
                try self.removeLastPop();
                const locals_count = self.scope.symbols.count();
                const free_symbols = try self.scope.free_symbols.toOwnedSlice();
                defer self.allocator.free(free_symbols);
                try self.writeOp(.return_void, token);

                var inst = try self.exitScope();
                for (free_symbols) |s| {
                    try self.loadSymbol(s, token);
                }
                const obj = try self.allocator.create(Value.Obj);

                // TODO: use debug tokens
                self.allocator.free(inst.debug_tokens);

                obj.* = .{
                    .data = .{
                        .bough = .{
                            .instructions = inst.instructions,
                            .locals_count = locals_count,
                        },
                    },
                };
                const i = try self.addConstant(.{ .obj = obj });
                try self.writeOp(.closure, token);
                _ = try self.writeInt(OpCode.Size(.constant), i, token);
                _ = try self.writeInt(u8, @intCast(u8, free_symbols.len), token);
                if (symbol.tag == .global) {
                    try self.writeOp(.set_global, token);
                    _ = try self.writeInt(OpCode.Size(.set_global), symbol.index, token);
                } else {
                    try self.writeOp(.set_local, token);
                    const size = OpCode.Size(.set_local);
                    _ = try self.writeInt(size, @intCast(size, symbol.index), token);
                }
            },
            .divert => |g| {
                for (g) |id| {
                    var symbol = try self.scope.resolve(id);
                    try self.loadSymbol(symbol, token);
                    try self.writeOp(.divert, token);
                }
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
        var scope = self.scope;
        var inst = scope.instructions;
        const last = inst.getLastOrNull();
        if (last) |l| return l == @intFromEnum(op);
        return false;
    }

    fn removeLastPop(self: *Compiler) !void {
        if (try self.lastIs(.pop)) {
            var scope = self.scope;
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
                var symbol: ?*Symbol = null;
                const op: OpCode = switch (bin.operator) {
                    .add, .assign_add => .add,
                    .subtract, .assign_subtract => .subtract,
                    .multiply, .assign_multiply => .multiply,
                    .divide, .assign_divide => .divide,
                    .modulus => .modulus,
                    .equal => .equal,
                    .not_equal => .not_equal,
                    .greater_than => .greater_than,
                    .@"or" => .@"or",
                    .@"and" => .@"and",
                    .assign => switch (bin.left.type) {
                        .identifier => |id| blk: {
                            symbol = try self.scope.resolve(id);
                            if (symbol) |s| {
                                break :blk if (s.tag == .global) .set_global else .set_local;
                            } else unreachable;
                        },
                        .indexer => .set_local,
                        else => unreachable,
                    },
                    else => {
                        return error.IllegalOperation;
                    },
                };
                try self.writeOp(op, token);
                if (symbol) |s| {
                    if (s.tag == .global) {
                        _ = try self.writeInt(OpCode.Size(.get_global), s.index, token);
                    } else {
                        _ = try self.writeInt(OpCode.Size(.get_local), @intCast(OpCode.Size(.get_local), s.index), token);
                    }

                    // check for assignments
                    if (std.mem.eql(u8, @tagName(bin.operator)[0..7], "assign_")) {
                        if (s.tag == .global) {
                            _ = try self.writeInt(OpCode.Size(.set_global), s.index, token);
                        } else {
                            _ = try self.writeInt(OpCode.Size(.set_local), @intCast(OpCode.Size(.get_local), s.index), token);
                        }
                    }
                }
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
                _ = try self.writeInt(u8, @intCast(u8, s.expressions.len), token);
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
                var symbol = try self.builtins.resolve(id) orelse try self.scope.resolve(id);
                try self.loadSymbol(symbol, token);
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
                try self.enterScope(.closure);

                if (f.name) |name| {
                    _ = try self.scope.defineFunction(name);
                }
                for (f.parameters) |param| {
                    _ = try self.scope.define(param);
                }

                try self.compileBlock(f.body);
                if (!(try self.lastIs(.return_value)) and !(try self.lastIs(.return_void))) {
                    try self.writeOp(.return_void, token);
                }

                const free_symbols = try self.scope.free_symbols.toOwnedSlice();
                defer self.allocator.free(free_symbols);
                const locals_count = self.scope.symbols.count();
                const inst = try self.exitScope();
                for (free_symbols) |s| {
                    try self.loadSymbol(s, token);
                }

                const obj = try self.allocator.create(Value.Obj);

                // TODO: use debug tokens
                self.allocator.free(inst.debug_tokens);

                obj.* = .{
                    .data = .{
                        .function = .{
                            .instructions = inst.instructions,
                            .locals_count = locals_count,
                            .arity = @intCast(u8, f.parameters.len),
                        },
                    },
                };
                const i = try self.addConstant(.{ .obj = obj });

                try self.writeOp(.closure, token);
                _ = try self.writeInt(OpCode.Size(.constant), i, token);
                _ = try self.writeInt(u8, @intCast(u8, free_symbols.len), token);
            },
            .call => |c| {
                try self.compileExpression(c.target);
                for (c.arguments) |*arg| {
                    try self.compileExpression(arg);
                }

                try self.writeOp(.call, token);
                const size = OpCode.Size(.call);
                std.debug.assert(c.arguments.len < std.math.maxInt(size));
                _ = try self.writeInt(size, @intCast(size, c.arguments.len), token);
            },
            .dialogue => |d| {
                try self.compileExpression(d.content);
                if (d.speaker) |speaker| {
                    if (self.speakers.get(speaker)) |position| {
                        try self.writeOp(.get_global, token);
                        _ = try self.writeInt(OpCode.Size(.constant), position, token);
                    } else {
                        const obj = try self.allocator.create(Value.Obj);
                        obj.* = .{ .data = .{ .string = try self.allocator.dupe(u8, speaker) } };
                        const i = try self.addConstant(.{ .obj = obj });
                        try self.speakers.put(speaker, i);

                        try self.writeOp(.constant, token);
                        _ = try self.writeInt(OpCode.Size(.constant), i, token);
                        try self.writeOp(.set_global, token);
                        _ = try self.writeInt(OpCode.Size(.constant), i, token);
                        try self.writeOp(.get_global, token);
                        _ = try self.writeInt(OpCode.Size(.constant), i, token);
                    }
                }
                try self.writeOp(.dialogue, d.content.token);
                var has_speaker_value = if (d.speaker == null) @as(u8, 0) else @as(u8, 1);
                _ = try self.writeInt(u8, has_speaker_value, token);
                _ = try self.writeInt(u8, @intCast(u8, d.tags.len), token);
            },
            else => {},
        }
    }

    fn loadSymbol(self: *Compiler, symbol: ?*Symbol, token: Token) !void {
        if (symbol) |ptr| {
            switch (ptr.tag) {
                .global => {
                    try self.writeOp(.get_global, token);
                    _ = try self.writeInt(OpCode.Size(.get_global), ptr.index, token);
                },
                .builtin => {
                    try self.writeOp(.get_builtin, token);
                    const size = OpCode.Size(.get_builtin);
                    _ = try self.writeInt(size, @intCast(size, ptr.index), token);
                },
                .free => {
                    try self.writeOp(.get_free, token);
                    const size = OpCode.Size(.get_free);
                    _ = try self.writeInt(size, @intCast(size, ptr.index), token);
                },
                .function => {
                    try self.writeOp(.current_closure, token);
                },
                else => {
                    try self.writeOp(.get_local, token);
                    const size = OpCode.Size(.get_local);
                    _ = try self.writeInt(size, @intCast(size, ptr.index), token);
                },
            }
        } else return CompilerError.SymbolNotFound;
    }

    fn scopePos(self: *Compiler) u16 {
        return @intCast(u16, self.scope.instructions.items.len);
    }

    fn writeOp(self: *Compiler, op: OpCode, token: Token) !void {
        var scope = self.scope;
        try scope.instructions.append(@intFromEnum(op));
        try DebugToken.add(&scope.debug_tokens, token);
    }

    fn writeValue(self: *Compiler, buf: []const u8, token: Token) !void {
        var scope = self.scope;
        try scope.instructions.writer().writeAll(buf);
        var i: usize = 0;
        while (i < buf.len) : (i += 1) {
            try DebugToken.add(&scope.debug_tokens, token);
        }
    }

    fn writeInt(self: *Compiler, comptime T: type, value: T, token: Token) !usize {
        var scope = self.scope;
        var start = scope.instructions.items.len;
        var buf: [@sizeOf(T)]u8 = undefined;
        std.mem.writeIntBig(T, buf[0..], value);
        try self.writeValue(&buf, token);
        return start;
    }

    pub fn replaceValue(self: *Compiler, pos: usize, comptime T: type, value: T) !void {
        var buf: [@sizeOf(T)]u8 = undefined;
        std.mem.writeIntBig(T, buf[0..], value);
        var scope = self.scope;
        for (buf, 0..) |v, i| {
            scope.instructions.items[pos + i] = v;
        }
    }

    pub fn replaceJumps(instructions: []u8, old_pos: u16, new_pos: u16) !void {
        var i: usize = 0;
        var jump = @intFromEnum(OpCode.jump);
        while (i < instructions.len) : (i += 1) {
            if (instructions[i] == jump and std.mem.readIntSliceBig(u16, instructions[(i + 1)..]) == old_pos) {
                var buf: [@sizeOf(u16)]u8 = undefined;
                std.mem.writeIntBig(u16, buf[0..], new_pos);
                for (buf, 0..) |v, index| {
                    instructions[i + index + 1] = v;
                }
            }
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
            .constants = [_]Value{ .{ .number = 1 }, .{ .number = 2 } },
            .instructions = [_]u8{ @intFromEnum(OpCode.constant), 0, 0, @intFromEnum(OpCode.constant), 0, 1, @intFromEnum(OpCode.add), @intFromEnum(OpCode.pop) },
        },
    };

    inline for (test_cases) |case| {
        var allocator = testing.allocator;
        var errors = Errors.init(allocator);
        defer errors.deinit();
        var bytecode = compileSource(allocator, case.input, &errors) catch |err| {
            try errors.write(case.input, std.io.getStdErr().writer());
            return err;
        };
        defer bytecode.free(allocator);
        for (case.instructions, 0..) |instruction, i| {
            try testing.expectEqual(instruction, bytecode.instructions[i]);
        }
        for (case.constants, 0..) |constant, i| {
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
            .input = "if true { 10 } 333",
            .constants = [_]Value{ .{ .number = 10 }, .{ .number = 333 } },
            .instructions = [_]u8{
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
            .input = "if true { 10 } else { 20 } 333",
            .constants = [_]Value{ .{ .number = 10 }, .{ .number = 20 }, .{ .number = 333 } },
            .instructions = [_]u8{ @intFromEnum(OpCode.true), @intFromEnum(OpCode.jump_if_false), 0, 10, @intFromEnum(OpCode.constant), 0, 0, @intFromEnum(OpCode.jump), 0, 13, @intFromEnum(OpCode.constant), 0, 1, @intFromEnum(OpCode.pop), @intFromEnum(OpCode.constant), 0, 2, @intFromEnum(OpCode.pop) },
        },
    };

    inline for (test_cases) |case| {
        var allocator = testing.allocator;
        var errors = Errors.init(allocator);
        defer errors.deinit();
        var bytecode = compileSource(allocator, case.input, &errors) catch |err| {
            try errors.write(case.input, std.io.getStdErr().writer());
            return err;
        };
        defer bytecode.free(allocator);
        for (case.instructions, 0..) |instruction, i| {
            errdefer std.log.warn("{}", .{i});
            try testing.expectEqual(instruction, bytecode.instructions[i]);
        }
        for (case.constants, 0..) |constant, i| {
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
            .constants = [_]Value{ .{ .number = 1 }, .{ .number = 2 } },
            .instructions = [_]u8{ @intFromEnum(OpCode.constant), 0, 0, @intFromEnum(OpCode.set_global), 0, 0, @intFromEnum(OpCode.constant), 0, 1, @intFromEnum(OpCode.set_global), 0, 1 },
        },
        .{
            .input =
            \\ var one = 1
            \\ var two = one
            \\ two
            ,
            .constants = [_]Value{.{ .number = 1 }},
            .instructions = [_]u8{ @intFromEnum(OpCode.constant), 0, 0, @intFromEnum(OpCode.set_global), 0, 0, @intFromEnum(OpCode.get_global), 0, 0, @intFromEnum(OpCode.set_global), 0, 1, @intFromEnum(OpCode.get_global), 0, 1, @intFromEnum(OpCode.pop) },
        },
    };

    inline for (test_cases) |case| {
        var allocator = testing.allocator;
        var errors = Errors.init(allocator);
        defer errors.deinit();
        var bytecode = compileSource(allocator, case.input, &errors) catch |err| {
            try errors.write(case.input, std.io.getStdErr().writer());
            return err;
        };
        defer bytecode.free(allocator);
        for (case.instructions, 0..) |instruction, i| {
            errdefer std.log.warn("{}", .{i});
            try testing.expectEqual(instruction, bytecode.instructions[i]);
        }
        for (case.constants, 0..) |constant, i| {
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
            .constants = [_][]const u8{"testing"},
            .instructions = [_]u8{ @intFromEnum(OpCode.string), 0, 0, 0, @intFromEnum(OpCode.pop) },
        },
        .{
            .input = "\"test\" + \"ing\"",
            .constants = [_][]const u8{ "test", "ing" },
            .instructions = [_]u8{
                @intFromEnum(OpCode.string),
                0,
                0,
                0,
                @intFromEnum(OpCode.string),
                0,
                1,
                0,
                @intFromEnum(OpCode.add),
                @intFromEnum(OpCode.pop),
            },
        },
    };

    inline for (test_cases) |case| {
        var allocator = testing.allocator;
        var errors = Errors.init(allocator);
        defer errors.deinit();
        var bytecode = compileSource(allocator, case.input, &errors) catch |err| {
            try errors.write(case.input, std.io.getStdErr().writer());
            return err;
        };
        defer bytecode.free(allocator);
        for (case.instructions, 0..) |instruction, i| {
            errdefer std.log.warn("{}", .{i});
            try testing.expectEqual(instruction, bytecode.instructions[i]);
        }
        for (case.constants, 0..) |constant, i| {
            try testing.expectEqualStrings(constant, bytecode.constants[i].obj.data.string);
        }
    }
}

test "Lists" {
    const test_cases = .{
        .{
            .input = "[]",
            .constants = [_]f32{},
            .instructions = [_]u8{ @intFromEnum(OpCode.list), 0, 0, @intFromEnum(OpCode.pop) },
        },
        .{
            .input = "[1,2,3]",
            .constants = [_]f32{ 1, 2, 3 },
            .instructions = [_]u8{ @intFromEnum(OpCode.constant), 0, 0, @intFromEnum(OpCode.constant), 0, 1, @intFromEnum(OpCode.constant), 0, 2, @intFromEnum(OpCode.list), 0, 3, @intFromEnum(OpCode.pop) },
        },
        .{
            .input = "[1 + 2, 3 - 4, 5 * 6]",
            .constants = [_]f32{ 1, 2, 3, 4, 5, 6 },
            .instructions = [_]u8{ @intFromEnum(OpCode.constant), 0, 0, @intFromEnum(OpCode.constant), 0, 1, @intFromEnum(OpCode.add), @intFromEnum(OpCode.constant), 0, 2, @intFromEnum(OpCode.constant), 0, 3, @intFromEnum(OpCode.subtract), @intFromEnum(OpCode.constant), 0, 4, @intFromEnum(OpCode.constant), 0, 5, @intFromEnum(OpCode.multiply), @intFromEnum(OpCode.list), 0, 3, @intFromEnum(OpCode.pop) },
        },
    };

    inline for (test_cases) |case| {
        var allocator = testing.allocator;
        var errors = Errors.init(allocator);
        defer errors.deinit();
        var bytecode = compileSource(allocator, case.input, &errors) catch |err| {
            try errors.write(case.input, std.io.getStdErr().writer());
            return err;
        };
        defer bytecode.free(allocator);
        for (case.instructions, 0..) |instruction, i| {
            errdefer std.log.warn("{}", .{i});
            try testing.expectEqual(instruction, bytecode.instructions[i]);
        }

        for (case.constants, 0..) |constant, i| {
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
            .constants = [_]f32{},
            .instructions = [_]u8{ @intFromEnum(OpCode.map), 0, 0, @intFromEnum(OpCode.pop) },
        },
        .{
            .input = "({1: 2, 3: 4, 5: 6})",
            .constants = [_]f32{ 1, 2, 3, 4, 5, 6 },
            .instructions = [_]u8{ @intFromEnum(OpCode.constant), 0, 0, @intFromEnum(OpCode.constant), 0, 1, @intFromEnum(OpCode.constant), 0, 2, @intFromEnum(OpCode.constant), 0, 3, @intFromEnum(OpCode.constant), 0, 4, @intFromEnum(OpCode.constant), 0, 5, @intFromEnum(OpCode.map), 0, 3, @intFromEnum(OpCode.pop) },
        },
        .{
            .input = "({1: 2 + 3, 4: 5 * 6})",
            .constants = [_]f32{ 1, 2, 3, 4, 5, 6 },
            .instructions = [_]u8{ @intFromEnum(OpCode.constant), 0, 0, @intFromEnum(OpCode.constant), 0, 1, @intFromEnum(OpCode.constant), 0, 2, @intFromEnum(OpCode.add), @intFromEnum(OpCode.constant), 0, 3, @intFromEnum(OpCode.constant), 0, 4, @intFromEnum(OpCode.constant), 0, 5, @intFromEnum(OpCode.multiply), @intFromEnum(OpCode.map), 0, 2, @intFromEnum(OpCode.pop) },
        },
        .{
            .input = "({})",
            .constants = [_]f32{},
            .instructions = [_]u8{ @intFromEnum(OpCode.set), 0, 0, @intFromEnum(OpCode.pop) },
        },
        .{
            .input = "({1, 2, 3})",
            .constants = [_]f32{ 1, 2, 3 },
            .instructions = [_]u8{ @intFromEnum(OpCode.constant), 0, 0, @intFromEnum(OpCode.constant), 0, 1, @intFromEnum(OpCode.constant), 0, 2, @intFromEnum(OpCode.set), 0, 3, @intFromEnum(OpCode.pop) },
        },
        .{
            .input = "({1 + 2, 3 * 4, 5 - 6})",
            .constants = [_]f32{ 1, 2, 3, 4, 5, 6 },
            .instructions = [_]u8{ @intFromEnum(OpCode.constant), 0, 0, @intFromEnum(OpCode.constant), 0, 1, @intFromEnum(OpCode.add), @intFromEnum(OpCode.constant), 0, 2, @intFromEnum(OpCode.constant), 0, 3, @intFromEnum(OpCode.multiply), @intFromEnum(OpCode.constant), 0, 4, @intFromEnum(OpCode.constant), 0, 5, @intFromEnum(OpCode.subtract), @intFromEnum(OpCode.set), 0, 3, @intFromEnum(OpCode.pop) },
        },
    };

    inline for (test_cases) |case| {
        var allocator = testing.allocator;
        var errors = Errors.init(allocator);
        defer errors.deinit();
        var bytecode = compileSource(allocator, case.input, &errors) catch |err| {
            try errors.write(case.input, std.io.getStdErr().writer());
            return err;
        };
        defer bytecode.free(allocator);
        for (case.instructions, 0..) |instruction, i| {
            errdefer std.log.warn("Error on: {}", .{i});
            try testing.expectEqual(instruction, bytecode.instructions[i]);
        }

        for (case.constants, 0..) |constant, i| {
            try testing.expect(constant == bytecode.constants[i].number);
        }
    }
}

test "Index" {
    const test_cases = .{
        .{
            .input = "[1,2,3][1 + 1]",
            .constants = [_]f32{ 1, 2, 3, 1, 1 },
            .instructions = [_]u8{ @intFromEnum(OpCode.constant), 0, 0, @intFromEnum(OpCode.constant), 0, 1, @intFromEnum(OpCode.constant), 0, 2, @intFromEnum(OpCode.list), 0, 3, @intFromEnum(OpCode.constant), 0, 3, @intFromEnum(OpCode.constant), 0, 4, @intFromEnum(OpCode.add), @intFromEnum(OpCode.index), @intFromEnum(OpCode.pop) },
        },
        .{
            .input = "({1: 2})[2 - 1]",
            .constants = [_]f32{ 1, 2, 2, 1 },
            .instructions = [_]u8{ @intFromEnum(OpCode.constant), 0, 0, @intFromEnum(OpCode.constant), 0, 1, @intFromEnum(OpCode.map), 0, 1, @intFromEnum(OpCode.constant), 0, 2, @intFromEnum(OpCode.constant), 0, 3, @intFromEnum(OpCode.subtract), @intFromEnum(OpCode.index), @intFromEnum(OpCode.pop) },
        },
    };

    inline for (test_cases) |case| {
        var allocator = testing.allocator;
        var errors = Errors.init(allocator);
        defer errors.deinit();
        var bytecode = compileSource(allocator, case.input, &errors) catch |err| {
            try errors.write(case.input, std.io.getStdErr().writer());
            return err;
        };
        defer bytecode.free(allocator);
        for (case.instructions, 0..) |instruction, i| {
            errdefer std.log.warn("Error on: {}", .{i});
            try testing.expectEqual(instruction, bytecode.instructions[i]);
        }

        for (case.constants, 0..) |constant, i| {
            try testing.expect(constant == bytecode.constants[i].number);
        }
    }
}

test "Functions" {
    var test_cases = .{
        .{
            .input = "|| return 5 + 10",
            .instructions = [_]u8{ @intFromEnum(OpCode.closure), 0, 2, 0, @intFromEnum(OpCode.pop) },
            .constants = [_]f32{ 5, 10 },
            .functions = [_][]const u8{
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
            .instructions = [_]u8{ @intFromEnum(OpCode.closure), 0, 2, 0, @intFromEnum(OpCode.pop) },
            .constants = [_]f32{ 5, 10 },
            .functions = [_][]const u8{
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
            .instructions = [_]u8{ @intFromEnum(OpCode.closure), 0, 2, 0, @intFromEnum(OpCode.pop) },
            .constants = [_]f32{ 5, 10 },
            .functions = [_][]const u8{
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
            .instructions = [_]u8{ @intFromEnum(OpCode.closure), 0, 0, 0, @intFromEnum(OpCode.pop) },
            .constants = [_]f32{ 0, 0, 0 },
            .functions = [_][]const u8{
                &[_]u8{
                    @intFromEnum(OpCode.return_void),
                },
            },
        },
        .{
            .input = "|| { return 5 }()",
            .instructions = [_]u8{ @intFromEnum(OpCode.closure), 0, 1, 0, @intFromEnum(OpCode.call), 0, @intFromEnum(OpCode.pop) },
            .constants = [_]f32{5},
            .functions = [_][]const u8{
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
            .instructions = [_]u8{
                @intFromEnum(OpCode.closure),
                0,
                1,
                0,
                @intFromEnum(OpCode.set_global),
                0,
                0,
                @intFromEnum(OpCode.get_global),
                0,
                0,
                @intFromEnum(OpCode.call),
                0,
                @intFromEnum(OpCode.pop),
            },
            .constants = [_]f32{5},
            .functions = [_][]const u8{
                &[_]u8{},
                &[_]u8{
                    @intFromEnum(OpCode.constant),     0, 0,
                    @intFromEnum(OpCode.return_value),
                },
            },
        },
        .{
            .input =
            \\ const two = || return 2
            \\ two() + two()
            ,
            .instructions = [_]u8{
                @intFromEnum(OpCode.closure),    0,                        1,                         0,
                @intFromEnum(OpCode.set_global), 0,                        0,                         @intFromEnum(OpCode.get_global),
                0,                               0,                        @intFromEnum(OpCode.call), 0,
                @intFromEnum(OpCode.get_global), 0,                        0,                         @intFromEnum(OpCode.call),
                0,                               @intFromEnum(OpCode.add), @intFromEnum(OpCode.pop),
            },
            .constants = [_]f32{2},
            .functions = [_][]const u8{
                &[_]u8{},
                &[_]u8{
                    @intFromEnum(OpCode.constant),     0, 0,
                    @intFromEnum(OpCode.return_value),
                },
            },
        },
        .{
            .input =
            \\ const value = |a| return a
            \\ value(1) + value(1)
            ,
            .instructions = [_]u8{
                @intFromEnum(OpCode.closure),    0,                         0,                             0,
                @intFromEnum(OpCode.set_global), 0,                         0,                             @intFromEnum(OpCode.get_global),
                0,                               0,                         @intFromEnum(OpCode.constant), 0,
                1,                               @intFromEnum(OpCode.call), 1,                             @intFromEnum(OpCode.get_global),
                0,                               0,                         @intFromEnum(OpCode.constant), 0,
                2,                               @intFromEnum(OpCode.call), 1,                             @intFromEnum(OpCode.add),
                @intFromEnum(OpCode.pop),
            },
            .constants = [_]f32{ 0, 1, 1 },
            .functions = [_][]const u8{
                &[_]u8{
                    @intFromEnum(OpCode.get_local),    0,
                    @intFromEnum(OpCode.return_value),
                },
            },
        },
        .{
            .input =
            \\ const oneArg = |a| { return a }
            \\ oneArg(24)
            ,
            .instructions = [_]u8{
                @intFromEnum(OpCode.closure),    0,                         0,                             0,
                @intFromEnum(OpCode.set_global), 0,                         0,                             @intFromEnum(OpCode.get_global),
                0,                               0,                         @intFromEnum(OpCode.constant), 0,
                1,                               @intFromEnum(OpCode.call), 1,                             @intFromEnum(OpCode.pop),
            },
            .constants = [_]f32{ 0, 24 },
            .functions = [_][]const u8{
                &[_]u8{
                    @intFromEnum(OpCode.get_local),    0,
                    @intFromEnum(OpCode.return_value),
                },
            },
        },
        .{
            .input =
            \\ const multiArg = |a, b, c| { a b return c }
            \\ multiArg(24,25,26)
            ,
            .instructions = [_]u8{
                @intFromEnum(OpCode.closure),    0,                             0,                             0,
                @intFromEnum(OpCode.set_global), 0,                             0,                             @intFromEnum(OpCode.get_global),
                0,                               0,                             @intFromEnum(OpCode.constant), 0,
                1,                               @intFromEnum(OpCode.constant), 0,                             2,
                @intFromEnum(OpCode.constant),   0,                             3,                             @intFromEnum(OpCode.call),
                3,                               @intFromEnum(OpCode.pop),
            },
            .constants = [_]f32{ 0, 24, 25, 26 },
            .functions = [_][]const u8{
                &[_]u8{
                    @intFromEnum(OpCode.get_local),
                    0,
                    @intFromEnum(OpCode.pop),
                    @intFromEnum(OpCode.get_local),
                    1,
                    @intFromEnum(OpCode.pop),
                    @intFromEnum(OpCode.get_local),
                    2,
                    @intFromEnum(OpCode.return_value),
                },
            },
        },
    };

    inline for (test_cases) |case| {
        errdefer std.log.warn("{s}", .{case.input});
        var allocator = testing.allocator;
        var errors = Errors.init(allocator);
        defer errors.deinit();
        var bytecode = compileSource(allocator, case.input, &errors) catch |err| {
            try errors.write(case.input, std.io.getStdErr().writer());
            return err;
        };
        defer bytecode.free(allocator);
        errdefer bytecode.print(std.debug);
        for (case.instructions, 0..) |instruction, i| {
            errdefer std.log.warn("Error on: {}", .{i});
            try testing.expectEqual(instruction, bytecode.instructions[i]);
        }
        for (bytecode.constants, 0..) |constant, i| {
            switch (constant) {
                .number => |n| try testing.expect(case.constants[i] == n),
                .obj => |o| try testing.expectEqualSlices(u8, case.functions[i], o.data.function.instructions),
                else => unreachable,
            }
        }
    }
}

test "Locals" {
    var test_cases = .{
        .{
            .input =
            \\ const num = 5
            \\ || return num
            ,
            .instructions = [_]u8{
                @intFromEnum(OpCode.constant),
                0,
                0,
                @intFromEnum(OpCode.set_global),
                0,
                0,
                @intFromEnum(OpCode.closure),
                0,
                1,
                0,
                @intFromEnum(OpCode.pop),
            },
            .constants = [_]f32{5},
            .functions = [_][]const u8{
                &[_]u8{},
                &[_]u8{
                    @intFromEnum(OpCode.get_global),   0, 0,
                    @intFromEnum(OpCode.return_value),
                },
            },
        },
        .{
            .input =
            \\ || {
            \\     const num = 5
            \\     return num       
            \\ }
            ,
            .instructions = [_]u8{ @intFromEnum(OpCode.closure), 0, 1, 0, @intFromEnum(OpCode.pop) },
            .constants = [_]f32{5},
            .functions = [_][]const u8{
                &[_]u8{},
                &[_]u8{ @intFromEnum(OpCode.constant), 0, 0, @intFromEnum(OpCode.set_local), 0, @intFromEnum(OpCode.get_local), 0, @intFromEnum(OpCode.return_value) },
            },
        },
        .{
            .input =
            \\ || {
            \\    const a = 5
            \\    const b = 7
            \\    return a + b       
            \\ }
            ,
            .instructions = [_]u8{ @intFromEnum(OpCode.closure), 0, 2, 0, @intFromEnum(OpCode.pop) },
            .constants = [_]f32{ 5, 7 },
            .functions = [_][]const u8{
                &[_]u8{},
                &[_]u8{},
                &[_]u8{
                    @intFromEnum(OpCode.constant),
                    0,
                    0,
                    @intFromEnum(OpCode.set_local),
                    0,
                    @intFromEnum(OpCode.constant),
                    0,
                    1,
                    @intFromEnum(OpCode.set_local),
                    1,
                    @intFromEnum(OpCode.get_local),
                    0,
                    @intFromEnum(OpCode.get_local),
                    1,
                    @intFromEnum(OpCode.add),
                    @intFromEnum(OpCode.return_value),
                },
            },
        },
    };

    inline for (test_cases) |case| {
        errdefer std.log.warn("{s}", .{case.input});
        var allocator = testing.allocator;
        var errors = Errors.init(allocator);
        defer errors.deinit();
        var bytecode = compileSource(allocator, case.input, &errors) catch |err| {
            try errors.write(case.input, std.io.getStdErr().writer());
            return err;
        };
        defer bytecode.free(allocator);
        errdefer bytecode.print(std.debug);
        for (case.instructions, 0..) |instruction, i| {
            errdefer std.log.warn("Error on: {}", .{i});
            try testing.expectEqual(instruction, bytecode.instructions[i]);
        }
        for (bytecode.constants, 0..) |constant, i| {
            switch (constant) {
                .number => |n| try testing.expect(case.constants[i] == n),
                .obj => |o| try testing.expectEqualSlices(u8, case.functions[i], o.data.function.instructions),
                else => unreachable,
            }
        }
    }
}

test "Builtin Functions" {
    var test_cases = .{
        .{
            .input =
            \\ rnd(1, 10)
            ,
            .instructions = [_]u8{
                @intFromEnum(OpCode.get_builtin),
                0,
                @intFromEnum(OpCode.constant),
                0,
                0,
                @intFromEnum(OpCode.constant),
                0,
                1,
                @intFromEnum(OpCode.call),
                2,
                @intFromEnum(OpCode.pop),
            },
            .constants = [_]f32{ 1, 10 },
        },
        .{
            .input = "rnd01()",
            .instructions = [_]u8{
                @intFromEnum(OpCode.get_builtin),
                1,
                @intFromEnum(OpCode.call),
                0,
                @intFromEnum(OpCode.pop),
            },
            .constants = [_]f32{0},
        },
    };

    inline for (test_cases) |case| {
        errdefer std.log.warn("{s}", .{case.input});
        var allocator = testing.allocator;
        var errors = Errors.init(allocator);
        defer errors.deinit();
        var bytecode = compileSource(allocator, case.input, &errors) catch |err| {
            try errors.write(case.input, std.io.getStdErr().writer());
            return err;
        };
        defer bytecode.free(allocator);
        errdefer bytecode.print(std.debug);
        for (case.instructions, 0..) |instruction, i| {
            errdefer std.log.warn("Error on: {}", .{i});
            try testing.expectEqual(instruction, bytecode.instructions[i]);
        }
        for (bytecode.constants, 0..) |constant, i| {
            try testing.expect(case.constants[i] == constant.number);
        }
    }
}

test "Closures" {
    var test_cases = .{
        .{
            .input = "|a| { return |b| { return a + b } }",
            .instructions = [_]u8{
                @intFromEnum(OpCode.closure),
                0,
                1,
                0,
                @intFromEnum(OpCode.pop),
            },
            .constants = [_]f32{0},
            .functions = [_][]const u8{
                &[_]u8{
                    @intFromEnum(OpCode.get_free),  0,
                    @intFromEnum(OpCode.get_local), 0,
                    @intFromEnum(OpCode.add),       @intFromEnum(OpCode.return_value),
                },
                &[_]u8{
                    @intFromEnum(OpCode.get_local),    0,
                    @intFromEnum(OpCode.closure),      0,
                    0,                                 1,
                    @intFromEnum(OpCode.return_value),
                },
            },
        },
        .{
            .input =
            \\ |a| {
            \\     return |b| { 
            \\        return |c| return a + b + c 
            \\     }
            \\ }
            ,
            .instructions = [_]u8{
                @intFromEnum(OpCode.closure),
                0,
                2,
                0,
                @intFromEnum(OpCode.pop),
            },
            .constants = [_]f32{0},
            .functions = [_][]const u8{
                &[_]u8{
                    @intFromEnum(OpCode.get_free),     0,
                    @intFromEnum(OpCode.get_free),     1,
                    @intFromEnum(OpCode.add),          @intFromEnum(OpCode.get_local),
                    0,                                 @intFromEnum(OpCode.add),
                    @intFromEnum(OpCode.return_value),
                },
                &[_]u8{
                    @intFromEnum(OpCode.get_free),     0,
                    @intFromEnum(OpCode.get_local),    0,
                    @intFromEnum(OpCode.closure),      0,
                    0,                                 2,
                    @intFromEnum(OpCode.return_value),
                },
                &[_]u8{
                    @intFromEnum(OpCode.get_local),    0,
                    @intFromEnum(OpCode.closure),      0,
                    1,                                 1,
                    @intFromEnum(OpCode.return_value),
                },
            },
        },
        .{
            .input =
            \\ const globalNum = 55
            \\ || {
            \\     const a = 66
            \\     return || {
            \\        const b = 77 
            \\        return || {
            \\            const c = 88 
            \\            return globalNum + a + b + c 
            \\        }
            \\     } 
            \\ }
            ,
            .instructions = [_]u8{
                @intFromEnum(OpCode.constant),   0,                        0,
                @intFromEnum(OpCode.set_global), 0,                        0,
                @intFromEnum(OpCode.closure),    0,                        6,
                0,                               @intFromEnum(OpCode.pop),
            },
            .constants = [_]f32{ 55, 66, 77, 88 },
            .functions = [_][]const u8{
                &[_]u8{},
                &[_]u8{},
                &[_]u8{},
                &[_]u8{},
                &[_]u8{
                    @intFromEnum(OpCode.constant),  0,                        3,
                    @intFromEnum(OpCode.set_local), 0,                        @intFromEnum(OpCode.get_global),
                    0,                              0,                        @intFromEnum(OpCode.get_free),
                    0,                              @intFromEnum(OpCode.add), @intFromEnum(OpCode.get_free),
                    1,                              @intFromEnum(OpCode.add), @intFromEnum(OpCode.get_local),
                    0,                              @intFromEnum(OpCode.add), @intFromEnum(OpCode.return_value),
                },
                &[_]u8{
                    @intFromEnum(OpCode.constant),  0,                                 2,
                    @intFromEnum(OpCode.set_local), 0,                                 @intFromEnum(OpCode.get_free),
                    0,                              @intFromEnum(OpCode.get_local),    0,
                    @intFromEnum(OpCode.closure),   0,                                 4,
                    2,                              @intFromEnum(OpCode.return_value),
                },
                &[_]u8{
                    @intFromEnum(OpCode.constant),  0,                            1,
                    @intFromEnum(OpCode.set_local), 0,                            @intFromEnum(OpCode.get_local),
                    0,                              @intFromEnum(OpCode.closure), 0,
                    5,                              1,                            @intFromEnum(OpCode.return_value),
                },
            },
        },
        .{
            .input =
            \\ const countDown =|x| return countDown(x - 1)
            \\ countDown(1)
            ,
            .instructions = [_]u8{
                @intFromEnum(OpCode.closure),
                0,
                1,
                0,
                @intFromEnum(OpCode.set_global),
                0,
                0,
                @intFromEnum(OpCode.get_global),
                0,
                0,
                @intFromEnum(OpCode.constant),
                0,
                2,
                @intFromEnum(OpCode.call),
                1,
                @intFromEnum(OpCode.pop),
            },
            .constants = [_]f32{ 1, 0, 1 },
            .functions = [_][]const u8{
                &[_]u8{},
                &[_]u8{
                    @intFromEnum(OpCode.current_closure),
                    @intFromEnum(OpCode.get_local),
                    0,
                    @intFromEnum(OpCode.constant),
                    0,
                    0,
                    @intFromEnum(OpCode.subtract),
                    @intFromEnum(OpCode.call),
                    1,
                    @intFromEnum(OpCode.return_value),
                },
            },
        },
    };

    inline for (test_cases) |case| {
        errdefer std.log.warn("{s}", .{case.input});
        var allocator = testing.allocator;
        var errors = Errors.init(allocator);
        defer errors.deinit();
        var bytecode = compileSource(allocator, case.input, &errors) catch |err| {
            try errors.write(case.input, std.io.getStdErr().writer());
            return err;
        };
        defer bytecode.free(allocator);
        errdefer bytecode.print(std.debug);
        for (case.instructions, 0..) |instruction, i| {
            errdefer std.log.warn("Error on: {}", .{i});
            try testing.expectEqual(instruction, bytecode.instructions[i]);
        }
        for (bytecode.constants, 0..) |constant, i| {
            switch (constant) {
                .number => |n| try testing.expect(case.constants[i] == n),
                .obj => |o| try testing.expectEqualSlices(u8, case.functions[i], o.data.function.instructions),
                else => unreachable,
            }
        }
    }
}
