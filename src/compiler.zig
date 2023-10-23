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
const ByteCode = @import("./bytecode.zig").ByteCode;
const JumpTree = @import("./jump-tree.zig").JumpTree;
const VisitTree = @import("./visit-tree.zig").VisitTree;
const Enum = @import("./enum.zig").Enum;
const UUID = @import("./utils/uuid.zig").UUID;

const testing = std.testing;
const BREAK_HOLDER = 9000;
const CONTINUE_HOLDER = 9001;
const CHOICE_HOLDER = 9002;
const FORK_HOLDER = 9003;
const DIVERT_HOLDER = 9004;
const PRONG_HOLDER = 9005;
const SWITCH_END_HOLDER = 9006;
const JUMP_HOLDER = 9999;

pub fn compileSource(allocator: std.mem.Allocator, source: []const u8, errors: *Errors) !ByteCode {
    const tree = try parser.parse(allocator, source, errors);
    defer tree.deinit();

    var compiler = try Compiler.init(allocator, errors);
    defer compiler.deinit();

    try compiler.compile(tree);
    return try compiler.bytecode();
}

pub const Compiler = struct {
    allocator: std.mem.Allocator,
    builtins: *Scope,
    constants: std.ArrayList(Value),
    uuids: std.ArrayList(UUID.ID),
    err: *Errors,
    scope: *Scope,
    root_scope: *Scope,
    identifier_cache: std.StringHashMap(OpCode.Size(.constant)),
    chunk: *Chunk,
    locals_count: usize = 0,

    jump_tree: JumpTree,
    divert_log: std.ArrayList(JumpTree.Entry),
    visit_tree: VisitTree,
    fork_count: u32 = 0,

    pub const Chunk = struct {
        instructions: std.ArrayList(u8),
        tokens: DebugToken.List,
        parent: ?*Chunk,
        allocator: std.mem.Allocator,

        pub fn create(allocator: std.mem.Allocator, parent: ?*Chunk) !*Chunk {
            var chunk = try allocator.create(Chunk);
            chunk.* = .{
                .instructions = std.ArrayList(u8).init(allocator),
                .tokens = DebugToken.List.init(allocator),
                .parent = parent,
                .allocator = allocator,
            };
            return chunk;
        }

        pub fn destroy(self: *Chunk) void {
            self.instructions.deinit();
            self.tokens.deinit();
            self.allocator.destroy(self);
        }
    };

    pub const Error = error{
        CompilerError,
        IllegalOperation,
        OutOfScope,
        NoSpaceLeft,
        SymbolNotFound,
        ExternError,
        NotYetImplemented,
    } || parser.Parser.Error;

    pub fn init(allocator: std.mem.Allocator, errors: *Errors) !Compiler {
        var root_chunk = try Compiler.Chunk.create(allocator, null);
        var root_scope = try Scope.create(allocator, null, .global, 0);
        var root_builtins = try Scope.create(allocator, null, .builtin, 0);
        return .{
            .allocator = allocator,
            .builtins = root_builtins,
            .constants = std.ArrayList(Value).init(allocator),
            .uuids = std.ArrayList(UUID.ID).init(allocator),
            .identifier_cache = std.StringHashMap(OpCode.Size(.constant)).init(allocator),
            .err = errors,
            .chunk = root_chunk,
            .scope = root_scope,
            .root_scope = root_scope,
            .jump_tree = try JumpTree.init(allocator),
            .visit_tree = try VisitTree.init(allocator),
            .divert_log = std.ArrayList(JumpTree.Entry).init(allocator),
        };
    }

    pub fn deinit(self: *Compiler) void {
        self.chunk.destroy();
        self.root_scope.destroy();
        self.jump_tree.deinit();
        self.visit_tree.deinit();
        self.constants.deinit();
        self.divert_log.deinit();
        self.identifier_cache.deinit();
        self.builtins.destroy();
    }

    fn fail(self: *Compiler, comptime msg: []const u8, token: Token, args: anytype) Error {
        try self.err.add(msg, token, .err, args);
        return Error.CompilerError;
    }

    fn failError(self: *Compiler, comptime msg: []const u8, token: Token, args: anytype, err: Error) Error {
        try self.err.add(msg, token, .err, args);
        return err;
    }

    pub fn bytecode(self: *Compiler) !ByteCode {
        if (self.scope.parent != null) return Error.OutOfScope;
        var global_symbols = try self.allocator.alloc(ByteCode.GlobalSymbol, self.scope.symbols.count());
        for (self.scope.symbols.values(), 0..) |s, i| {
            global_symbols[i] = ByteCode.GlobalSymbol{
                .name = try self.allocator.dupe(u8, s.name),
                .index = s.index,
                .is_extern = s.is_extern,
            };
        }
        return .{
            .instructions = try self.chunk.instructions.toOwnedSlice(),
            .constants = try self.constants.toOwnedSlice(),
            .tokens = try self.chunk.tokens.toOwnedSlice(),
            .global_symbols = global_symbols,
            .locals_count = self.locals_count,
            .uuids = try self.uuids.toOwnedSlice(),
        };
    }

    pub fn compile(self: *Compiler, tree: ast.Tree) Error!void {
        inline for (builtins) |builtin| {
            _ = try self.builtins.define(builtin.name, false, false);
        }

        try self.initializeConstants();

        for (tree.root) |stmt| {
            // We first get a list of all boughs and named forks,
            // then we'll switch out all the divert jump locations
            // to the appropriate places.
            // Alternatively, maybe it'd be better to store boughs and named forks
            // as constant values, then just fetch the ip location as needed.
            try self.precompileJumps(stmt, self.jump_tree.root);
        }
        self.visit_tree.print(std.debug);

        for (tree.root) |stmt| {
            try self.compileStatement(stmt);
        }

        try self.replaceDiverts();
    }

    fn enterChunk(self: *Compiler) !void {
        self.chunk = try Chunk.create(self.allocator, self.chunk);
    }

    fn exitChunk(self: *Compiler) !struct { instructions: []u8, tokens: []DebugToken } {
        const old_chunk = self.chunk;
        const result = .{
            .instructions = try old_chunk.instructions.toOwnedSlice(),
            .tokens = try old_chunk.tokens.toOwnedSlice(),
        };
        self.chunk = old_chunk.parent orelse return Error.OutOfScope;
        old_chunk.destroy();
        return result;
    }

    fn enterScope(self: *Compiler, tag: Scope.Tag) !void {
        self.scope = try Scope.create(
            self.allocator,
            self.scope,
            tag,
            if (self.scope.tag == .global or self.scope.tag == .closure) 0 else self.scope.count,
        );
    }

    fn exitScope(self: *Compiler) !void {
        const old_scope = self.scope;
        self.scope = old_scope.parent orelse return Error.OutOfScope;
        const locals_count = self.locals_count + old_scope.count;
        if (locals_count > self.locals_count) self.locals_count = locals_count;
        old_scope.destroy();
    }

    pub fn precompileJumps(self: *Compiler, stmt: ast.Statement, node: *JumpTree.Node) Error!void {
        switch (stmt.type) {
            .include => |i| {
                for (i.contents) |s| try self.precompileJumps(s, node);
            },
            .bough => |b| {
                try self.compileVisitDecl(b.name, stmt.token);
                defer _ = self.visit_tree.list.pop();
                defer self.visit_tree.pop();

                var bough_node = try JumpTree.Node.create(self.allocator, b.name, node);
                for (b.body) |s| try self.precompileJumps(s, bough_node);
                try node.children.append(bough_node);
            },
            .fork => |f| {
                var buf: [11]u8 = undefined;
                var fbs = std.io.fixedBufferStream(&buf);
                try fbs.writer().writeByte('_');
                try std.fmt.formatIntValue(self.fork_count, "", .{}, fbs.writer());
                var fork_count = std.mem.trim(u8, buf[0..], &[_]u8{170});
                self.fork_count += 1;
                defer self.fork_count -= 1;

                try self.compileVisitDecl(f.name orelse fork_count, stmt.token);
                defer _ = self.visit_tree.list.pop();
                defer self.visit_tree.pop();

                if (f.name) |name| {
                    var fork_node = try JumpTree.Node.create(self.allocator, name, node);
                    for (f.body) |s| try self.precompileJumps(s, fork_node);
                    try node.children.append(fork_node);
                } else {
                    for (f.body) |s| try self.precompileJumps(s, node);
                }
                if (f.name != null) {
                    self.visit_tree.pop();
                }
            },
            .choice => |c| {
                var name = c.name orelse &c.id;
                try self.compileVisitDecl(name, stmt.token);
                defer _ = self.visit_tree.list.pop();
                defer self.visit_tree.pop();

                for (c.body) |s| try self.precompileJumps(s, node);
            },
            else => {},
        }
    }

    pub fn compileStatement(self: *Compiler, stmt: ast.Statement) Error!void {
        var token = stmt.token;
        switch (stmt.type) {
            .include => |i| {
                try self.compileBlock(i.contents);
            },
            .@"if" => |i| {
                try self.compileExpression(i.condition);
                try self.writeOp(.jump_if_false, token);
                const falsePos = try self.writeInt(OpCode.Size(.jump), JUMP_HOLDER, token);
                try self.compileBlock(i.then_branch);

                try self.writeOp(.jump, token);
                const jumpPos = try self.writeInt(OpCode.Size(.jump), JUMP_HOLDER, token);
                try self.replaceValue(falsePos, OpCode.Size(.jump), self.instructionPos());

                if (i.else_branch == null) {
                    try self.writeOp(.nil, token);
                    try self.writeOp(.pop, token);
                    try self.replaceValue(jumpPos, OpCode.Size(.jump), self.instructionPos());
                    return;
                }
                try self.compileBlock(i.else_branch.?);
                try self.replaceValue(jumpPos, OpCode.Size(.jump), self.instructionPos());
            },
            .@"switch" => |s| {
                var start = self.instructionPos();
                try self.compileExpression(&s.capture);
                var prong_jumps = try self.allocator.alloc(usize, s.prongs.len);
                defer self.allocator.free(prong_jumps);
                for (s.prongs, 0..) |prong_stmt, i| {
                    var prong = prong_stmt.type.switch_prong;
                    if (prong.values == null) break;
                    for (prong.values.?) |value| {
                        try self.compileExpression(&value);
                    }
                    try self.writeOp(.prong, prong_stmt.token);
                    var prong_jump = try self.writeInt(OpCode.Size(.jump), PRONG_HOLDER, prong_stmt.token);
                    _ = try self.writeInt(u8, @as(u8, @intCast(prong.values.?.len)), prong_stmt.token);
                    prong_jumps[i] = prong_jump;
                }

                for (s.prongs, 0..) |prong_stmt, i| {
                    var prong = prong_stmt.type.switch_prong;
                    if (prong.values != null)
                        try self.replaceValue(prong_jumps[i], OpCode.Size(.jump), self.instructionPos());
                    try self.enterScope(.local);
                    try self.compileBlock(prong.body);
                    try self.writeOp(.jump, prong_stmt.token);
                    _ = try self.writeInt(OpCode.Size(.jump), SWITCH_END_HOLDER, prong_stmt.token);
                    try self.exitScope();
                }
                try replaceJumps(self.chunk.instructions.items[start..], SWITCH_END_HOLDER, self.instructionPos());
                try self.writeOp(.pop, token);
            },
            .block => |b| try self.compileBlock(b),
            .expression => |exp| {
                try self.compileExpression(&exp);
                try self.writeOp(.pop, token);
            },
            .@"break" => {
                try self.writeOp(.jump, token);
                _ = try self.writeInt(OpCode.Size(.jump), BREAK_HOLDER, token);
            },
            .@"continue" => {
                try self.writeOp(.jump, token);
                _ = try self.writeInt(OpCode.Size(.jump), CONTINUE_HOLDER, token);
            },
            .@"while" => |w| {
                try self.enterScope(.local);

                const start = self.instructionPos();
                try self.compileExpression(&w.condition);
                try self.writeOp(.jump_if_false, token);
                const temp_start = try self.writeInt(OpCode.Size(.jump_if_false), JUMP_HOLDER, token);

                try self.compileBlock(w.body);
                try self.writeOp(.jump, token);
                _ = try self.writeInt(OpCode.Size(.jump), start, token);

                const end = self.instructionPos();
                try self.replaceValue(temp_start, OpCode.Size(.jump), end);

                try replaceJumps(self.chunk.instructions.items[start..], BREAK_HOLDER, end);
                try replaceJumps(self.chunk.instructions.items[start..], CONTINUE_HOLDER, start);
                try self.exitScope();
            },
            .@"for" => |f| {
                try self.compileExpression(&f.iterator);
                try self.writeOp(.iter_start, token);
                const start = self.instructionPos();

                try self.writeOp(.iter_next, token);
                try self.writeOp(.jump_if_false, token);
                const jump_end = try self.writeInt(OpCode.Size(.jump), JUMP_HOLDER, token);

                try self.enterScope(.local);
                try self.writeOp(.set_local, token);
                _ = try self.writeInt(OpCode.Size(.set_local), 0, token);
                _ = try self.scope.define(f.capture, false, false);

                try self.compileBlock(f.body);
                try self.writeOp(.jump, token);
                _ = try self.writeInt(OpCode.Size(.jump), start, token);

                const end = self.instructionPos();
                try self.replaceValue(jump_end, OpCode.Size(.jump), end);
                try replaceJumps(self.chunk.instructions.items[start..], BREAK_HOLDER, end);
                try replaceJumps(self.chunk.instructions.items[start..], CONTINUE_HOLDER, start);

                try self.exitScope();
                try self.writeOp(.pop, token);

                try self.writeOp(.iter_end, token);
                // pop item
                try self.writeOp(.pop, token);
            },
            .variable => |v| {
                if (self.builtins.symbols.contains(v.name))
                    return self.failError("{s} is a builtin function and cannot be used as a variable name", stmt.token, .{v.name}, Error.IllegalOperation);
                if (self.scope.parent != null and v.is_extern)
                    return self.failError("Only global variables can be extern.", token, .{}, Error.IllegalOperation);
                var symbol = try self.scope.define(v.name, v.is_mutable, v.is_extern);
                try self.compileExpression(&v.initializer);
                try self.setSymbol(symbol, token, true);
            },

            .class => |c| {
                try self.enterScope(.local);
                for (c.fields, 0..) |field, i| {
                    try self.compileExpression(&field);
                    try self.getOrSetIdentifierConstant(c.field_names[i], token);
                }
                try self.exitScope();

                try self.getOrSetIdentifierConstant(c.name, token);
                try self.writeOp(.class, token);
                _ = try self.writeInt(OpCode.Size(.class), @as(OpCode.Size(.class), @intCast(c.fields.len)), token);
                var symbol = try self.scope.define(c.name, false, false);
                try self.setSymbol(symbol, token, true);
            },
            .@"enum" => |e| {
                var values = std.ArrayList(Enum.Value).init(self.allocator);
                const obj = try self.allocator.create(Value.Obj);

                for (e.values, 0..) |value, i| {
                    try values.append(.{
                        .index = i,
                        .name = try self.allocator.dupe(u8, value),
                        .base = &obj.data.@"enum",
                    });
                }
                obj.* = .{ .data = .{
                    .@"enum" = .{
                        .allocator = self.allocator,
                        .name = try self.allocator.dupe(u8, e.name),
                        .values = try values.toOwnedSlice(),
                    },
                } };
                const i = try self.addConstant(.{ .obj = obj });
                try self.writeOp(.constant, token);
                _ = try self.writeInt(OpCode.Size(.constant), i, token);

                var symbol = try self.scope.define(e.name, false, false);
                try self.setSymbol(symbol, token, true);
            },
            .fork => |f| {
                var buf: [11]u8 = undefined;
                var fbs = std.io.fixedBufferStream(&buf);
                try fbs.writer().writeByte('_');
                try std.fmt.formatIntValue(self.fork_count, "", .{}, fbs.writer());
                var fork_count = std.mem.trim(u8, buf[0..], &[_]u8{170});

                self.fork_count += 1;
                defer self.fork_count -= 1;

                try self.visit_tree.list.append(f.name orelse fork_count);
                defer _ = self.visit_tree.list.pop();

                var path = try std.mem.join(self.allocator, ".", self.visit_tree.list.items);
                defer self.allocator.free(path);
                var symbol = try self.root_scope.resolve(path);
                try self.compileVisit(symbol, path, token);

                if (f.name) |name| {
                    self.jump_tree.current = try self.jump_tree.current.getChild(name);
                    self.jump_tree.current.*.dest_ip = self.instructionPos();
                }
                try self.enterScope(.local);
                try self.compileBlock(f.body);
                try self.exitScope();
                if (f.name != null) {
                    self.jump_tree.pop();
                }

                var backup_pos: usize = 0;
                if (f.is_backup) {
                    try self.writeOp(.backup, token);
                    backup_pos = try self.writeInt(OpCode.Size(.jump), JUMP_HOLDER, token);
                }
                try self.writeOp(.fork, token);
                const end_pos = self.instructionPos();
                if (f.is_backup) {
                    try self.replaceValue(backup_pos, OpCode.Size(.jump), end_pos);
                }
            },
            .choice => |c| {
                var name = c.name orelse &c.id;
                try self.visit_tree.list.append(name);
                defer _ = self.visit_tree.list.pop();

                var path = try std.mem.join(self.allocator, ".", self.visit_tree.list.items);
                defer self.allocator.free(path);
                var visit_symbol = try self.root_scope.resolve(path);

                try self.compileExpression(&c.text);
                try self.writeOp(.choice, token);
                const start_pos = try self.writeInt(OpCode.Size(.jump), CHOICE_HOLDER, token);
                _ = try self.writeInt(u8, if (c.is_unique) 1 else 0, token);
                try self.writeId(c.id, token);
                _ = try self.writeInt(OpCode.Size(.get_global), visit_symbol.?.index, token);

                try self.writeOp(.jump, token);
                const jump_pos = try self.writeInt(OpCode.Size(.jump), JUMP_HOLDER, token);
                try self.replaceValue(start_pos, OpCode.Size(.jump), self.instructionPos());

                try self.enterScope(.local);

                try self.compileVisit(visit_symbol, path, token);
                try self.compileBlock(c.body);
                try self.writeOp(.fin, token);
                try self.exitScope();
                try self.replaceValue(jump_pos, OpCode.Size(.jump), self.instructionPos());
            },
            .bough => |b| {
                try self.visit_tree.list.append(b.name);
                defer _ = self.visit_tree.list.pop();

                // skip over bough when running through instructions
                try self.writeOp(.jump, token);
                const start_pos = try self.writeInt(OpCode.Size(.jump), JUMP_HOLDER, token);

                self.jump_tree.current = try self.jump_tree.current.getChild(b.name);
                self.jump_tree.current.*.dest_ip = self.instructionPos();
                defer self.jump_tree.pop();

                try self.enterScope(.local);
                errdefer self.exitScope() catch {};

                var path = try std.mem.join(self.allocator, ".", self.visit_tree.list.items);
                defer self.allocator.free(path);
                var symbol = try self.root_scope.resolve(path);
                try self.compileVisit(symbol, path, token);

                try self.compileBlock(b.body);
                try self.writeOp(.fin, token);

                try self.exitScope();

                const end = self.instructionPos();
                try self.replaceValue(start_pos, OpCode.Size(.jump), end);
            },
            .dialogue => |d| {
                for (d.tags) |tag| {
                    const obj = try self.allocator.create(Value.Obj);
                    obj.* = .{ .data = .{ .string = try self.allocator.dupe(u8, tag) } };
                    const i = try self.addConstant(.{ .obj = obj });
                    try self.writeOp(.constant, token);
                    _ = try self.writeInt(OpCode.Size(.constant), i, token);
                }

                try self.compileExpression(d.content);
                if (d.speaker) |speaker| {
                    try self.getOrSetIdentifierConstant(speaker, token);
                }
                try self.writeOp(.dialogue, d.content.token);
                var has_speaker_value = if (d.speaker == null) @as(u8, 0) else @as(u8, 1);
                _ = try self.writeInt(u8, has_speaker_value, token);
                _ = try self.writeInt(u8, @as(u8, @intCast(d.tags.len)), token);
                try self.writeId(d.id, token);
            },
            .divert => |d| {
                var node = try self.getDivertNode(d.path);
                var backup_pos: usize = 0;
                if (d.is_backup) {
                    try self.writeOp(.backup, token);
                    backup_pos = try self.writeInt(OpCode.Size(.jump), JUMP_HOLDER, token);
                }
                try self.writeOp(.jump, token);
                try self.divert_log.append(.{ .node = node, .jump_ip = self.instructionPos() });
                _ = try self.writeInt(OpCode.Size(.jump), DIVERT_HOLDER, token);

                const end_pos = self.instructionPos();
                if (d.is_backup) {
                    try self.replaceValue(backup_pos, OpCode.Size(.jump), end_pos);
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

    fn getDivertNode(self: *Compiler, path: [][]const u8) !*JumpTree.Node {
        var node = self.jump_tree.current;
        // traverse up the tree to find the start of the path
        while (!std.mem.eql(u8, node.name, "root")) : (node = node.parent.?) {
            if (std.mem.eql(u8, node.name, path[0])) {
                node = node.parent.?;
                break;
            }
            if (node.contains(path[0])) break;
        }

        // traverse back down to get the leaf node
        for (path) |name| {
            node = try node.getChild(name);
        }
        return node;
    }

    fn replaceDiverts(self: *Compiler) !void {
        for (self.divert_log.items) |entry| {
            const dest = entry.node.dest_ip;
            try self.replaceValue(entry.jump_ip, OpCode.Size(.jump), dest);
        }
    }

    fn lastIs(self: *Compiler, op: OpCode) !bool {
        var inst = self.chunk.instructions;
        const last = inst.getLastOrNull();
        if (last) |l| return l == @intFromEnum(op);
        return false;
    }

    fn removeLast(self: *Compiler, op: OpCode) !void {
        if (try self.lastIs(op)) {
            var inst = self.chunk.instructions;
            self.chunk.instructions.items = inst.items[0 .. inst.items.len - 1];
        }
    }

    pub fn compileBlock(self: *Compiler, stmts: []const ast.Statement) Error!void {
        for (stmts) |stmt| {
            try self.compileStatement(stmt);
        }
    }

    fn compileVisitDecl(self: *Compiler, name: []const u8, token: Token) Error!void {
        try self.visit_tree.list.append(name);
        var path = try std.mem.join(self.allocator, ".", self.visit_tree.list.items);
        defer self.allocator.free(path);
        var symbol = try self.root_scope.define(path, false, false);
        try self.writeOp(.constant, token);
        _ = try self.writeInt(OpCode.Size(.constant), 4, token);
        try self.setSymbol(symbol, token, true);
        try self.visit_tree.push(name, symbol.index);
    }

    fn compileVisit(self: *Compiler, symbol: ?*Symbol, name: []const u8, token: Token) Error!void {
        if (symbol) |sym| {
            try self.writeOp(.visit, token);
            _ = try self.writeInt(OpCode.Size(.get_global), sym.index, token);
        } else return self.failError("Unknown symbol: {s}", token, .{name}, Error.SymbolNotFound);
    }

    pub fn compileExpression(self: *Compiler, expr: *const ast.Expression) Error!void {
        var token = expr.token;
        switch (expr.type) {
            .binary => |bin| {
                if (bin.operator == .less_than) {
                    try self.compileExpression(bin.right);
                    try self.compileExpression(bin.left);
                    try self.writeOp(.greater_than, token);
                    return;
                }
                if (bin.operator == .assign) {
                    switch (bin.left.type) {
                        .identifier => |id| {
                            try self.compileExpression(bin.right);
                            var symbol = try self.scope.resolve(id);
                            try self.setSymbol(symbol, token, false);
                            try self.loadSymbol(symbol, token);
                            return;
                        },
                        .indexer => {
                            try self.compileExpression(bin.right);
                            try self.compileExpression(bin.left);
                            try self.removeLast(.index);
                            try self.writeOp(.set_property, token);
                            try self.compileExpression(bin.left);
                            return;
                        },
                        else => unreachable,
                    }
                }
                try self.compileExpression(bin.left);
                try self.compileExpression(bin.right);

                const op: OpCode = switch (bin.operator) {
                    .add => .add,
                    .subtract => .subtract,
                    .multiply => .multiply,
                    .divide => .divide,
                    .modulus => .modulus,
                    .assign_add => .add,
                    .assign_subtract => .subtract,
                    .assign_multiply => .multiply,
                    .assign_divide => .divide,
                    .assign_modulus => .modulus,
                    .equal => .equal,
                    .not_equal => .not_equal,
                    .greater_than => .greater_than,
                    .@"or" => .@"or",
                    .@"and" => .@"and",
                    else => {
                        return self.failError("Unknown operation {s}", token, .{bin.operator.toString()}, Error.IllegalOperation);
                    },
                };
                try self.writeOp(op, token);

                switch (bin.operator) {
                    .assign_add, .assign_subtract, .assign_multiply, .assign_divide, .assign_modulus => {
                        switch (bin.left.type) {
                            .identifier => |id| {
                                var symbol = try self.scope.resolve(id);
                                try self.setSymbol(symbol, token, false);
                                try self.loadSymbol(symbol, token);
                                return;
                            },
                            .indexer => {
                                try self.compileExpression(bin.left);
                                try self.removeLast(.index);
                                try self.writeOp(.set_property, token);
                                try self.compileExpression(bin.left);
                                return;
                            },
                            else => unreachable,
                        }
                    },
                    else => {},
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
                _ = try self.writeInt(u8, @as(u8, @intCast(s.expressions.len)), token);
            },
            .list => |l| {
                for (l) |*item| {
                    try self.compileExpression(item);
                }
                try self.writeOp(.list, token);
                const size = OpCode.Size(.list);
                const length = @as(size, @intCast(l.len));
                _ = try self.writeInt(size, length, token);
            },
            .map => |m| {
                for (m) |*mp| {
                    try self.compileExpression(mp);
                }
                const size = OpCode.Size(.map);
                try self.writeOp(.map, token);
                const length = @as(size, @intCast(m.len));
                _ = try self.writeInt(size, length, token);
            },
            .set => |s| {
                for (s) |*item| {
                    try self.compileExpression(item);
                }
                const size = OpCode.Size(.set);
                try self.writeOp(.set, token);
                const length = @as(size, @intCast(s.len));
                _ = try self.writeInt(size, length, token);
            },
            .map_pair => |mp| {
                try self.compileExpression(mp.key);
                try self.compileExpression(mp.value);
            },
            .indexer => |idx| {
                if (try self.isVisitExpression(expr, token)) {
                    return;
                }
                try self.compileExpression(idx.target);
                if (token.token_type == .dot) {
                    try self.getOrSetIdentifierConstant(idx.index.type.identifier, token);
                } else try self.compileExpression(idx.index);
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
                if (self.visit_tree.current.getChild(id)) |node| {
                    try self.loadVisit(node, token);
                    return;
                }
                var symbol = try self.builtins.resolve(id) orelse try self.scope.resolve(id);
                try self.loadSymbol(symbol, token);
            },
            .@"if" => |i| {
                try self.compileExpression(i.condition);
                try self.writeOp(.jump_if_false, token);
                // temp garbage value
                const pos = try self.writeInt(OpCode.Size(.jump), JUMP_HOLDER, token);
                try self.compileExpression(i.then_value);

                try self.writeOp(.jump, token);
                const nextPos = try self.writeInt(OpCode.Size(.jump), JUMP_HOLDER, token);
                try self.replaceValue(pos, OpCode.Size(.jump), self.instructionPos());

                try self.compileExpression(i.else_value);
                try self.replaceValue(nextPos, OpCode.Size(.jump), self.instructionPos());
            },
            .function => |f| {
                try self.enterScope(.closure);
                try self.enterChunk();

                if (f.name) |name| {
                    _ = try self.scope.defineFunction(name);
                }

                var length = f.parameters.len;

                if (f.is_method) {
                    length += 1;
                    _ = try self.scope.define("self", false, false);
                }

                for (f.parameters) |param| {
                    _ = try self.scope.define(param, true, false);
                }

                try self.compileBlock(f.body);
                if (!(try self.lastIs(.return_value)) and !(try self.lastIs(.return_void))) {
                    try self.writeOp(.return_void, token);
                }

                var chunk = try self.exitChunk();
                // TODO: use debug tokens
                self.allocator.free(chunk.tokens);
                const count = self.scope.count;
                const free_symbols = self.scope.free_symbols.items;
                for (free_symbols) |s| {
                    try self.loadSymbol(s, token);
                }
                try self.exitScope();
                // defer self.allocator.free(free_symbols);

                const obj = try self.allocator.create(Value.Obj);

                obj.* = .{
                    .data = .{
                        .function = .{
                            .is_method = f.is_method,
                            .instructions = chunk.instructions,
                            .locals_count = count,
                            .arity = @as(u8, @intCast(length)),
                        },
                    },
                };
                const i = try self.addConstant(.{ .obj = obj });

                try self.writeOp(.closure, token);
                _ = try self.writeInt(OpCode.Size(.constant), i, token);
                _ = try self.writeInt(u8, @as(u8, @intCast(free_symbols.len)), token);
            },
            .instance => |ins| {
                for (ins.fields, 0..) |field, i| {
                    try self.compileExpression(&field);
                    try self.getOrSetIdentifierConstant(ins.field_names[i], token);
                }
                var symbol = try self.scope.resolve(ins.name);
                try self.loadSymbol(symbol, token);
                try self.writeOp(.instance, token);
                _ = try self.writeInt(OpCode.Size(.instance), @as(OpCode.Size(.instance), @intCast(ins.fields.len)), token);
            },
            .call => |c| {
                try self.compileExpression(c.target);
                for (c.arguments) |*arg| {
                    try self.compileExpression(arg);
                }
                try self.writeOp(.call, token);
                const size = OpCode.Size(.call);
                std.debug.assert(c.arguments.len < std.math.maxInt(size));
                var length = c.arguments.len;
                if (c.target.type == .indexer) length += 1;
                _ = try self.writeInt(size, @as(size, @intCast(length)), token);
            },
            .range => |r| {
                try self.compileExpression(r.right);
                try self.compileExpression(r.left);
                try self.writeOp(.range, token);
            },
            else => return Error.NotYetImplemented,
        }
    }

    fn setSymbol(self: *Compiler, symbol: ?*Symbol, token: Token, is_decl: bool) !void {
        if (symbol) |ptr| {
            if (!is_decl and !ptr.is_mutable) return self.fail("Cannot assign to constant variable {s}", token, .{ptr.name});
            switch (ptr.tag) {
                .global => {
                    try self.writeOp(if (is_decl) .decl_global else .set_global, token);
                    const size = OpCode.Size(.set_global);
                    _ = try self.writeInt(size, @as(size, @intCast(ptr.index)), token);
                },
                .free => {
                    try self.writeOp(.set_free, token);
                    const size = OpCode.Size(.set_free);
                    _ = try self.writeInt(size, @as(size, @intCast(ptr.index)), token);
                },
                .builtin, .function => return self.failError("Cannot set {s}", token, .{ptr.name}, Error.IllegalOperation),
                else => {
                    try self.writeOp(.set_local, token);
                    const size = OpCode.Size(.set_local);
                    _ = try self.writeInt(size, @as(size, @intCast(ptr.index)), token);
                },
            }
        } else return self.failError("Unknown symbol", token, .{}, Error.SymbolNotFound);
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
                    _ = try self.writeInt(size, @as(size, @intCast(ptr.index)), token);
                },
                .free => {
                    try self.writeOp(.get_free, token);
                    const size = OpCode.Size(.get_free);
                    _ = try self.writeInt(size, @as(size, @intCast(ptr.index)), token);
                },
                .function => {
                    try self.writeOp(.current_closure, token);
                },
                else => {
                    try self.writeOp(.get_local, token);
                    const size = OpCode.Size(.get_local);
                    _ = try self.writeInt(size, @as(size, @intCast(ptr.index)), token);
                },
            }
        } else return self.failError("Unknown symbol", token, .{}, Error.SymbolNotFound);
    }

    fn loadVisit(self: *Compiler, node: *VisitTree.Node, token: Token) !void {
        try self.writeOp(.get_global, token);
        _ = try self.writeInt(OpCode.Size(.get_global), node.index, token);
    }

    fn instructionPos(self: *Compiler) OpCode.Size(.jump) {
        return @as(OpCode.Size(.jump), @intCast(self.chunk.instructions.items.len));
    }

    fn writeOp(self: *Compiler, op: OpCode, token: Token) !void {
        var chunk = self.chunk;
        try chunk.instructions.append(@intFromEnum(op));
        try DebugToken.add(&chunk.tokens, token);
    }

    fn writeValue(self: *Compiler, buf: []const u8, token: Token) !void {
        var chunk = self.chunk;
        try chunk.instructions.writer().writeAll(buf);
        var i: usize = 0;
        while (i < buf.len) : (i += 1) {
            try DebugToken.add(&chunk.tokens, token);
        }
    }

    fn writeInt(self: *Compiler, comptime T: type, value: T, token: Token) !usize {
        var chunk = self.chunk;
        var start = chunk.instructions.items.len;
        var buf: [@sizeOf(T)]u8 = undefined;
        std.mem.writeIntBig(T, buf[0..], value);
        try self.writeValue(&buf, token);
        return start;
    }

    pub fn writeId(self: *Compiler, id: UUID.ID, token: Token) !void {
        try self.uuids.append(id);
        _ = try self.writeInt(OpCode.Size(.constant), @as(OpCode.Size(.constant), @intCast(self.uuids.items.len - 1)), token);
    }

    pub fn replaceValue(self: *Compiler, pos: usize, comptime T: type, value: T) !void {
        var buf: [@sizeOf(T)]u8 = undefined;
        std.mem.writeIntBig(T, buf[0..], value);
        var chunk = self.chunk;
        for (buf, 0..) |v, i| {
            chunk.instructions.items[pos + i] = v;
        }
    }

    pub fn replaceJumps(instructions: []u8, old_pos: OpCode.Size(.jump), new_pos: OpCode.Size(.jump)) !void {
        var i: usize = 0;
        var jump = @intFromEnum(OpCode.jump);
        while (i < instructions.len) : (i += 1) {
            if (instructions[i] == jump and std.mem.readIntSliceBig(OpCode.Size(.jump), instructions[(i + 1)..]) == old_pos) {
                var buf: [@sizeOf(OpCode.Size(.jump))]u8 = undefined;
                std.mem.writeIntBig(OpCode.Size(.jump), buf[0..], new_pos);
                for (buf, 0..) |v, index| {
                    instructions[i + index + 1] = v;
                }
            }
        }
    }

    pub fn isVisitExpression(self: *Compiler, index: *const ast.Expression, token: Token) !bool {
        var idx = index.type.indexer;
        if (idx.target.type != .indexer and idx.target.type != .identifier) return false;

        var list = std.ArrayList([]const u8).init(self.allocator);
        defer list.deinit();
        try list.append(idx.index.type.identifier);
        while (idx.target.type == .indexer) {
            idx = idx.target.type.indexer;
            try list.append(idx.index.type.identifier);
        }
        if (idx.target.type != .identifier) return false;
        try list.append(idx.target.type.identifier);
        std.mem.reverse([]const u8, list.items);

        var joined = try std.mem.join(self.allocator, ".", list.items);
        defer self.allocator.free(joined);
        if (self.visit_tree.resolve(list.items[0])) |visit_node| {
            var node = visit_node;
            for (list.items, 0..) |item, i| {
                if (i == 0) continue;
                if (node.getChild(item)) |child| {
                    node = child;
                } else return false;
            }
            try self.loadVisit(node, token);
            return true;
        } else return false;
    }

    pub fn addConstant(self: *Compiler, value: Value) !OpCode.Size(.constant) {
        try self.constants.append(value);
        return @as(OpCode.Size(.constant), @intCast(self.constants.items.len - 1));
    }

    fn initializeConstants(self: *Compiler) !void {
        _ = try self.addConstant(.{ .number = 0 });
        _ = try self.addConstant(.{ .number = 1 });
        _ = try self.addConstant(.{ .bool = false });
        _ = try self.addConstant(.{ .bool = true });
        _ = try self.addConstant(.{ .visit = 0 });
    }

    fn getOrSetIdentifierConstant(self: *Compiler, name: []const u8, token: Token) !void {
        if (self.identifier_cache.get(name)) |position| {
            try self.writeOp(.constant, token);
            _ = try self.writeInt(OpCode.Size(.constant), position, token);
            return;
        }
        const obj = try self.allocator.create(Value.Obj);
        obj.* = .{ .data = .{ .string = try self.allocator.dupe(u8, name) } };
        const i = try self.addConstant(.{ .obj = obj });
        try self.identifier_cache.putNoClobber(name, i);

        try self.writeOp(.constant, token);
        _ = try self.writeInt(OpCode.Size(.constant), i, token);
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
                11,
                @intFromEnum(OpCode.constant),
                0,
                0,
                @intFromEnum(OpCode.pop),
                @intFromEnum(OpCode.jump),
                0,
                13,
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
            .instructions = [_]u8{
                @intFromEnum(OpCode.true),
                @intFromEnum(OpCode.jump_if_false),
                0,
                11,
                @intFromEnum(OpCode.constant),
                0,
                0,
                @intFromEnum(OpCode.pop),
                @intFromEnum(OpCode.jump),
                0,
                15,
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
        var allocator = testing.allocator;
        var errors = Errors.init(allocator);
        defer errors.deinit();
        var bytecode = compileSource(allocator, case.input, &errors) catch |err| {
            try errors.write(case.input, std.io.getStdErr().writer());
            return err;
        };
        defer bytecode.free(allocator);
        for (case.instructions, 0..) |instruction, i| {
            errdefer std.log.warn("Error on instruction:{}", .{i});
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
            .instructions = [_]u8{
                @intFromEnum(OpCode.constant),
                0,
                0,
                @intFromEnum(OpCode.decl_global),
                0,
                0,
                @intFromEnum(OpCode.constant),
                0,
                1,
                @intFromEnum(OpCode.decl_global),
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
            .constants = [_]Value{.{ .number = 1 }},
            .instructions = [_]u8{
                @intFromEnum(OpCode.constant),
                0,
                0,
                @intFromEnum(OpCode.decl_global),
                0,
                0,
                @intFromEnum(OpCode.get_global),
                0,
                0,
                @intFromEnum(OpCode.decl_global),
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
        var allocator = testing.allocator;
        var errors = Errors.init(allocator);
        defer errors.deinit();
        var bytecode = compileSource(allocator, case.input, &errors) catch |err| {
            try errors.write(case.input, std.io.getStdErr().writer());
            return err;
        };
        defer bytecode.free(allocator);
        for (case.instructions, 0..) |instruction, i| {
            errdefer std.log.warn("{s} -- {}", .{ case.input, i });
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
                @intFromEnum(OpCode.decl_global),
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
                @intFromEnum(OpCode.closure),     0,                        1,                         0,
                @intFromEnum(OpCode.decl_global), 0,                        0,                         @intFromEnum(OpCode.get_global),
                0,                                0,                        @intFromEnum(OpCode.call), 0,
                @intFromEnum(OpCode.get_global),  0,                        0,                         @intFromEnum(OpCode.call),
                0,                                @intFromEnum(OpCode.add), @intFromEnum(OpCode.pop),
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
                @intFromEnum(OpCode.closure),     0,                         0,                             0,
                @intFromEnum(OpCode.decl_global), 0,                         0,                             @intFromEnum(OpCode.get_global),
                0,                                0,                         @intFromEnum(OpCode.constant), 0,
                1,                                @intFromEnum(OpCode.call), 1,                             @intFromEnum(OpCode.get_global),
                0,                                0,                         @intFromEnum(OpCode.constant), 0,
                2,                                @intFromEnum(OpCode.call), 1,                             @intFromEnum(OpCode.add),
                @intFromEnum(OpCode.pop),
            },
            .constants = [_]f32{ 0, 1, 1 },
            .functions = [_][]const u8{
                &[_]u8{
                    @intFromEnum(OpCode.get_local),    0, 0,
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
                @intFromEnum(OpCode.closure),     0,                         0,                             0,
                @intFromEnum(OpCode.decl_global), 0,                         0,                             @intFromEnum(OpCode.get_global),
                0,                                0,                         @intFromEnum(OpCode.constant), 0,
                1,                                @intFromEnum(OpCode.call), 1,                             @intFromEnum(OpCode.pop),
            },
            .constants = [_]f32{ 0, 24 },
            .functions = [_][]const u8{
                &[_]u8{
                    @intFromEnum(OpCode.get_local),    0, 0,
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
                @intFromEnum(OpCode.closure),     0,                             0,                             0,
                @intFromEnum(OpCode.decl_global), 0,                             0,                             @intFromEnum(OpCode.get_global),
                0,                                0,                             @intFromEnum(OpCode.constant), 0,
                1,                                @intFromEnum(OpCode.constant), 0,                             2,
                @intFromEnum(OpCode.constant),    0,                             3,                             @intFromEnum(OpCode.call),
                3,                                @intFromEnum(OpCode.pop),
            },
            .constants = [_]f32{ 0, 24, 25, 26 },
            .functions = [_][]const u8{
                &[_]u8{
                    @intFromEnum(OpCode.get_local),
                    0,
                    0,
                    @intFromEnum(OpCode.pop),
                    @intFromEnum(OpCode.get_local),
                    0,
                    1,
                    @intFromEnum(OpCode.pop),
                    @intFromEnum(OpCode.get_local),
                    0,
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
                @intFromEnum(OpCode.decl_global),
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
                &[_]u8{ @intFromEnum(OpCode.constant), 0, 0, @intFromEnum(OpCode.set_local), 0, 0, @intFromEnum(OpCode.get_local), 0, 0, @intFromEnum(OpCode.return_value) },
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
                    0,
                    @intFromEnum(OpCode.constant),
                    0,
                    1,
                    @intFromEnum(OpCode.set_local),
                    0,
                    1,
                    @intFromEnum(OpCode.get_local),
                    0,
                    0,
                    @intFromEnum(OpCode.get_local),
                    0,
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
                    @intFromEnum(OpCode.get_free),     0,
                    @intFromEnum(OpCode.get_local),    0,
                    0,                                 @intFromEnum(OpCode.add),
                    @intFromEnum(OpCode.return_value),
                },
                &[_]u8{
                    @intFromEnum(OpCode.get_local), 0,                                 0,
                    @intFromEnum(OpCode.closure),   0,                                 0,
                    1,                              @intFromEnum(OpCode.return_value),
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
                    @intFromEnum(OpCode.get_free), 0,
                    @intFromEnum(OpCode.get_free), 1,
                    @intFromEnum(OpCode.add),      @intFromEnum(OpCode.get_local),
                    0,                             0,
                    @intFromEnum(OpCode.add),      @intFromEnum(OpCode.return_value),
                },
                &[_]u8{
                    @intFromEnum(OpCode.get_free),  0,
                    @intFromEnum(OpCode.get_local), 0,
                    0,                              @intFromEnum(OpCode.closure),
                    0,                              0,
                    2,                              @intFromEnum(OpCode.return_value),
                },
                &[_]u8{
                    @intFromEnum(OpCode.get_local), 0,                                 0,
                    @intFromEnum(OpCode.closure),   0,                                 1,
                    1,                              @intFromEnum(OpCode.return_value),
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
                @intFromEnum(OpCode.constant),    0,                        0,
                @intFromEnum(OpCode.decl_global), 0,                        0,
                @intFromEnum(OpCode.closure),     0,                        6,
                0,                                @intFromEnum(OpCode.pop),
            },
            .constants = [_]f32{ 55, 66, 77, 88 },
            .functions = [_][]const u8{
                &[_]u8{},
                &[_]u8{},
                &[_]u8{},
                &[_]u8{},
                &[_]u8{
                    @intFromEnum(OpCode.constant),   0,                                 3,
                    @intFromEnum(OpCode.set_local),  0,                                 0,
                    @intFromEnum(OpCode.get_global), 0,                                 0,
                    @intFromEnum(OpCode.get_free),   0,                                 @intFromEnum(OpCode.add),
                    @intFromEnum(OpCode.get_free),   1,                                 @intFromEnum(OpCode.add),
                    @intFromEnum(OpCode.get_local),  0,                                 0,
                    @intFromEnum(OpCode.add),        @intFromEnum(OpCode.return_value),
                },
                &[_]u8{
                    @intFromEnum(OpCode.constant),     0, 2,
                    @intFromEnum(OpCode.set_local),    0, 0,
                    @intFromEnum(OpCode.get_free),     0, @intFromEnum(OpCode.get_local),
                    0,                                 0, @intFromEnum(OpCode.closure),
                    0,                                 4, 2,
                    @intFromEnum(OpCode.return_value),
                },
                &[_]u8{
                    @intFromEnum(OpCode.constant),  0,                                 1,
                    @intFromEnum(OpCode.set_local), 0,                                 0,
                    @intFromEnum(OpCode.get_local), 0,                                 0,
                    @intFromEnum(OpCode.closure),   0,                                 5,
                    1,                              @intFromEnum(OpCode.return_value),
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
                @intFromEnum(OpCode.decl_global),
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

test "Classes" {
    const TestValue = union(enum(u4)) {
        number: f32,
        string: []const u8,
    };
    const test_cases = .{
        .{
            .input =
            \\ class Test {
            \\     value = 0    
            \\ }
            \\ new Test{}.value
            \\
            ,
            .instructions = [_]u8{
                @intFromEnum(OpCode.constant), 0, 0,
                @intFromEnum(OpCode.constant), 0, 1,
                @intFromEnum(OpCode.constant), 0, 2,
                @intFromEnum(OpCode.class),    1, @intFromEnum(OpCode.decl_global),
                0,                             0, @intFromEnum(OpCode.get_global),
                0,                             0, @intFromEnum(OpCode.instance),
            },
            .constants = [_]TestValue{
                .{ .number = 0.0 },
                .{ .string = "value" },
                .{ .string = "Test" },
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
        for (case.constants, 0..) |constant, i| {
            switch (constant) {
                .number => |n| try testing.expect(n == bytecode.constants[i].number),
                .string => |s| try testing.expectEqualStrings(s, bytecode.constants[i].obj.data.string),
            }
        }
    }
}

test "Serialize" {
    const input =
        \\ var str = "string value"
        \\ const num = 25
        \\ const fun = |x| {
        \\     return x * 2
        \\ }
        \\ var list = ["one", "two"]
        \\ const set = {1, 2, 3.3}
        \\ const map = {1:2.2, 3: 4.4}
    ;

    errdefer std.log.warn("{s}", .{input});
    var allocator = testing.allocator;
    var errors = Errors.init(allocator);
    defer errors.deinit();
    var bytecode = compileSource(allocator, input, &errors) catch |err| {
        try errors.write(input, std.io.getStdErr().writer());
        return err;
    };
    defer bytecode.free(allocator);

    // this doesn't need to be a file, but it's nice to sometimes not delete it and inspect it
    const file = try std.fs.cwd().createFile("tmp.topib", .{ .read = true });
    defer std.fs.cwd().deleteFile("tmp.topib") catch {};
    defer file.close();
    try bytecode.serialize(file.writer());

    try file.seekTo(0);
    var deserialized = try ByteCode.deserialize(allocator, file.reader());
    defer deserialized.free(allocator);
    try testing.expectEqualSlices(u8, bytecode.instructions, deserialized.instructions);
    for (bytecode.constants, 0..) |constant, i| {
        try testing.expect(constant.eql(deserialized.constants[i]));
    }
}
