const std = @import("std");
const ast = @import("ast.zig");
const parser = @import("parser.zig");
const Token = @import("token.zig").Token;
const OpCode = @import("opcode.zig").OpCode;
const CompilerErrors = @import("compiler-error.zig").CompilerErrors;
const Value = @import("values.zig").Value;
const ValueType = @import("values.zig").Type;
const String = @import("values.zig").String;
const Scope = @import("scope.zig").Scope;
const Symbol = @import("scope.zig").Symbol;
const builtins = @import("builtins.zig").builtins;
const Bytecode = @import("bytecode.zig").Bytecode;
const JumpTree = @import("structures/jump-tree.zig").JumpTree;
const VisitTree = @import("structures/visit-tree.zig").VisitTree;
const Enum = @import("enum.zig").Enum;
const Module = @import("module.zig").Module;
const UUID = @import("utils/uuid.zig").UUID;

const testing = std.testing;
const BREAK_HOLDER = 9000;
const CONTINUE_HOLDER = 9001;
const CHOICE_HOLDER = 9002;
const FORK_HOLDER = 9003;
const DIVERT_HOLDER = 9004;
const PRONG_HOLDER = 9005;
const SWITCH_END_HOLDER = 9006;
const JUMP_HOLDER = 9999;

pub const initial_constants = [_]Value{
    .{ .number = 0 },
    .{ .number = 1 },
    .{ .bool = false },
    .{ .bool = true },
    .{ .visit = 0 },
};

fn arrayOfTypeContains(comptime T: type, haystack: []const []const T, needle: []const T) bool {
    for (haystack) |element| {
        if (std.mem.eql(T, element, needle)) return true;
    }
    return false;
}
pub const Compiler = struct {
    allocator: std.mem.Allocator,
    builtins: *Scope,
    constants: std.ArrayList(Value),
    uuids: std.ArrayList(UUID.ID),
    err: *CompilerErrors,
    scope: *Scope,
    root_scope: *Scope,
    identifier_cache: std.StringHashMap(OpCode.Size(.constant)),
    chunk: *Chunk,
    locals_count: usize = 0,
    use_loc: bool = false,

    module: *Module,
    jump_tree: JumpTree,
    divert_log: std.ArrayList(JumpTree.Entry),
    visit_tree: VisitTree,

    types: std.StringHashMap(*const ast.Statement),

    pub const Chunk = struct {
        instructions: std.ArrayList(u8),
        token_lines: std.ArrayList(u32),
        parent: ?*Chunk,
        allocator: std.mem.Allocator,

        pub fn create(allocator: std.mem.Allocator, parent: ?*Chunk) !*Chunk {
            const chunk = try allocator.create(Chunk);
            chunk.* = .{
                .instructions = std.ArrayList(u8).init(allocator),
                .token_lines = std.ArrayList(u32).init(allocator),
                .parent = parent,
                .allocator = allocator,
            };
            return chunk;
        }

        pub fn destroy(self: *Chunk) void {
            self.instructions.deinit();
            self.token_lines.deinit();
            self.allocator.destroy(self);
        }
    };

    pub const Error = error{
        CompilerError,
        IllegalOperation,
        OutOfScope,
        NoSpaceLeft,
        SymbolNotFound,
        SymbolAlreadyDeclared,
        ExternError,
        NotYetImplemented,
    } || parser.Parser.Error;

    pub fn init(allocator: std.mem.Allocator) !Compiler {
        const root_chunk = try Compiler.Chunk.create(allocator, null);
        const root_scope = try Scope.create(allocator, null, .global);
        const root_builtins = try Scope.create(allocator, null, .builtin);
        return .{
            .allocator = allocator,
            .builtins = root_builtins,
            .constants = std.ArrayList(Value).init(allocator),
            .uuids = std.ArrayList(UUID.ID).init(allocator),
            .identifier_cache = std.StringHashMap(OpCode.Size(.constant)).init(allocator),
            .chunk = root_chunk,
            .scope = root_scope,
            .root_scope = root_scope,
            .jump_tree = try JumpTree.init(allocator),
            .visit_tree = try VisitTree.init(allocator),
            .divert_log = std.ArrayList(JumpTree.Entry).init(allocator),
            .types = std.StringHashMap(*const ast.Statement).init(allocator),
            .err = undefined,
            .module = undefined,
        };
    }

    pub fn deinit(self: *Compiler) void {
        self.chunk.destroy();
        self.root_scope.destroy();
        self.jump_tree.deinit();
        self.visit_tree.deinit();
        for (self.constants.items) |item| {
            if (item == .obj) {
                Value.Obj.destroy(self.allocator, item.obj);
            }
        }
        self.constants.deinit();
        self.divert_log.deinit();
        self.identifier_cache.deinit();
        self.builtins.destroy();
        self.types.deinit();
    }

    fn fail(self: *Compiler, comptime msg: []const u8, token: Token, args: anytype) Error {
        try self.err.add(msg, token, .err, args);
        return Error.CompilerError;
    }

    fn failError(self: *Compiler, comptime msg: []const u8, token: Token, args: anytype, err: Error) Error {
        try self.err.add(msg, token, .err, args);
        return err;
    }

    pub fn bytecode(self: *Compiler) !Bytecode {
        if (self.scope.parent != null) return Error.OutOfScope;
        var global_symbols = try self.allocator.alloc(Bytecode.GlobalSymbol, self.scope.symbols.count());
        for (self.scope.symbols.values(), 0..) |s, i| {
            global_symbols[i] = Bytecode.GlobalSymbol{
                .name = try self.allocator.dupe(u8, s.name),
                .index = s.index,
                .is_extern = s.is_extern,
                .is_mutable = s.is_mutable,
            };
        }
        var boughs = std.ArrayList(Bytecode.BoughJump).init(self.allocator);
        defer boughs.deinit();
        var stack = std.ArrayList(*const JumpTree.Node).init(self.allocator);
        defer stack.deinit();
        // reverse iterate so we keep the order when popping off stack
        var i: usize = self.jump_tree.root.children.items.len;
        while (i > 0) {
            i -= 1;
            try stack.append(self.jump_tree.root.children.items[i]);
        }
        while (stack.items.len > 0) {
            var node = stack.pop();
            var path = std.ArrayList(u8).init(self.allocator);
            defer path.deinit();
            try node.writePath(path.writer());
            try boughs.append(.{
                .name = try path.toOwnedSlice(),
                .ip = node.dest_ip,
            });
            var child_i: usize = node.children.items.len;
            while (child_i > 0) {
                child_i -= 1;
                try stack.append(node.children.items[child_i]);
            }
        }

        var loc = std.ArrayList(u8).init(self.allocator);
        defer loc.deinit();
        if (self.use_loc) {
            var it = self.module.includes.iterator();
            while (it.next()) |kvp| {
                const file = kvp.value_ptr.*;
                try file.loadLoc();
                defer file.unloadLoc();
                if (file.loc_loaded) try loc.appendSlice(file.loc);
            }
        }
        return .{
            .instructions = try self.chunk.instructions.toOwnedSlice(),
            .boughs = try boughs.toOwnedSlice(),
            .token_lines = try self.chunk.token_lines.toOwnedSlice(),
            .constants = try self.constants.toOwnedSlice(),
            .global_symbols = global_symbols,
            .locals_count = self.locals_count,
            .uuids = try self.uuids.toOwnedSlice(),
            .loc = try loc.toOwnedSlice(),
        };
    }

    pub fn compile(self: *Compiler, module: *Module) Error!void {
        if (!module.entry.tree_loaded) return error.CompilerError;
        self.module = module;
        self.err = &module.entry.errors;
        const tree = module.entry.tree;
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
        self.visit_tree.reset();

        for (tree.root) |stmt| {
            try self.compileStatement(stmt);
        }

        try self.replaceDiverts();
        // Add one final fin at the end of file to grab the initial jump_request
        if (self.chunk.token_lines.items.len > 0) {
            try self.chunk.token_lines.append(self.chunk.token_lines.items[self.chunk.instructions.items.len - 1]);
            try self.chunk.instructions.append(@intFromEnum(OpCode.fin));
        }
    }

    fn enterChunk(self: *Compiler) !void {
        self.chunk = try Chunk.create(self.allocator, self.chunk);
    }

    fn exitChunk(self: *Compiler) !struct { []u8, []u32 } {
        const old_chunk = self.chunk;
        defer old_chunk.destroy();
        self.chunk = old_chunk.parent orelse return Error.OutOfScope;
        return .{ try old_chunk.instructions.toOwnedSlice(), try old_chunk.token_lines.toOwnedSlice() };
    }

    fn enterScope(self: *Compiler, tag: Scope.Tag) !void {
        self.scope = try Scope.create(self.allocator, self.scope, tag);
    }

    fn exitScope(self: *Compiler) !void {
        const old_scope = self.scope;
        self.scope = old_scope.parent orelse return Error.OutOfScope;
        self.locals_count = @max(self.locals_count, self.scope.count + old_scope.count);
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

                const bough_node = try JumpTree.Node.create(self.allocator, b.name, node);
                for (b.body) |s| try self.precompileJumps(s, bough_node);
                try node.children.append(bough_node);
            },
            .fork => |f| {
                var v_node = self.visit_tree.current;
                const fork_count = try std.fmt.allocPrint(self.allocator, "_{d}", .{v_node.anon_count});
                defer self.allocator.free(fork_count);
                v_node.anon_count += 1;

                try self.compileVisitDecl(f.name orelse fork_count, stmt.token);
                defer _ = self.visit_tree.list.pop();
                defer self.visit_tree.pop();

                if (f.name) |name| {
                    const fork_node = try JumpTree.Node.create(self.allocator, name, node);
                    for (f.body) |s| try self.precompileJumps(s, fork_node);
                    try node.children.append(fork_node);
                } else {
                    for (f.body) |s| try self.precompileJumps(s, node);
                }
            },
            .choice => |c| {
                const name = c.name orelse &c.id;
                try self.compileVisitDecl(name, stmt.token);
                defer _ = self.visit_tree.list.pop();
                defer self.visit_tree.pop();

                for (c.body) |s| try self.precompileJumps(s, node);
            },
            .@"if" => |i| {
                for (i.then_branch) |s| try self.precompileJumps(s, node);
                if (i.else_branch) |e| {
                    for (e) |s| try self.precompileJumps(s, node);
                }
            },
            .@"while" => |w| {
                for (w.body) |s| try self.precompileJumps(s, node);
            },
            .@"for" => |f| {
                for (f.body) |s| try self.precompileJumps(s, node);
            },
            else => {},
        }
    }

    pub fn compileStatement(self: *Compiler, stmt: ast.Statement) Error!void {
        const token = stmt.token;
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
                const start = self.instructionPos();
                try self.compileExpression(&s.capture);
                var prong_jumps = try self.allocator.alloc(usize, s.prongs.len);
                defer self.allocator.free(prong_jumps);
                // compile expressions and jumps
                for (s.prongs, 0..) |prong_stmt, i| {
                    const prong = prong_stmt.type.switch_prong;
                    if (prong.values) |p| {
                        for (p) |value| {
                            try self.compileExpression(&value);
                        }
                    }
                    try self.writeOp(.prong, prong_stmt.token);
                    const prong_jump = try self.writeInt(OpCode.Size(.jump), PRONG_HOLDER, prong_stmt.token);
                    _ = try self.writeInt(u8, @as(u8, @intCast(if (prong.values) |p| p.len else 0)), prong_stmt.token);
                    prong_jumps[i] = prong_jump;
                }

                // replace jumps and compile body
                for (s.prongs, 0..) |prong_stmt, i| {
                    const prong = prong_stmt.type.switch_prong;
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
                    return self.failError("'{s}' is a builtin function and cannot be used as a variable name", stmt.token, .{v.name}, Error.IllegalOperation);
                if (self.scope.parent != null and v.is_extern)
                    return self.failError("Only global variables can be extern.", token, .{}, Error.IllegalOperation);
                const symbol = self.scope.define(v.name, v.is_mutable, v.is_extern) catch {
                    return self.fail("'{s}' is already declared", token, .{v.name});
                };
                try self.compileExpression(&v.initializer);
                try self.setSymbol(symbol, token, true);
            },
            .class => |c| {
                try self.types.put(c.name, &stmt);
                try self.enterScope(.local);
                for (c.fields, 0..) |field, i| {
                    try self.compileExpression(&field);
                    try self.getOrSetIdentifierConstant(c.field_names[i], token);
                }
                try self.exitScope();

                try self.getOrSetIdentifierConstant(c.name, token);
                try self.writeOp(.class, token);
                _ = try self.writeInt(OpCode.Size(.class), @as(OpCode.Size(.class), @intCast(c.fields.len)), token);
                const symbol = self.scope.define(c.name, false, false) catch {
                    return self.fail("'{s}' is already declared", token, .{c.name});
                };
                try self.setSymbol(symbol, token, true);
            },
            .@"enum" => |e| {
                try self.types.put(e.name, &stmt);
                var names = std.ArrayList([]const u8).init(self.allocator);
                defer names.deinit();
                const obj = try self.allocator.create(Value.Obj);

                for (e.values) |value| {
                    try names.append(try self.allocator.dupe(u8, value));
                }

                obj.* = .{
                    .data = .{
                        .@"enum" = .{
                            .is_seq = e.is_seq,
                            .name = try self.allocator.dupe(u8, e.name),
                            .values = try names.toOwnedSlice(),
                        },
                    },
                };
                const i = try self.addConstant(.{ .obj = obj });
                try self.writeOp(.constant, token);
                _ = try self.writeInt(OpCode.Size(.constant), i, token);

                const symbol = try self.scope.define(e.name, false, false);
                try self.setSymbol(symbol, token, true);
            },
            .fork => |f| {
                const anon_count = self.visit_tree.current.anon_count;
                const fork_count = try std.fmt.allocPrint(self.allocator, "_{d}", .{anon_count});
                defer self.allocator.free(fork_count);

                self.visit_tree.current.anon_count += 1;
                const cur = if (self.visit_tree.current.getChild(f.name orelse fork_count)) |n| n else {
                    self.visit_tree.print(std.debug);
                    return self.fail("Could not find '{s}' in visit tree node '{s}'", token, .{ f.name orelse fork_count, self.visit_tree.current.name });
                };
                self.visit_tree.current = cur;
                defer self.visit_tree.current = cur.parent.?;

                try self.visit_tree.list.append(f.name orelse fork_count);
                defer _ = self.visit_tree.list.pop();

                const path = try std.mem.join(self.allocator, ".", self.visit_tree.list.items);
                defer self.allocator.free(path);
                const symbol = try self.root_scope.resolve(path);
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
                for (c.tags) |tag| {
                    try self.getOrSetIdentifierConstant(tag, token);
                }
                const name = c.name orelse &c.id;
                try self.visit_tree.list.append(name);
                const cur = if (self.visit_tree.current.getChild(name)) |n| n else {
                    self.visit_tree.print(std.debug);
                    return self.fail("Could not find '{s}' in visit tree node '{s}'", token, .{ name, self.visit_tree.current.name });
                };
                self.visit_tree.current = cur;
                defer self.visit_tree.current = cur.parent.?;
                defer _ = self.visit_tree.list.pop();

                const path = try std.mem.join(self.allocator, ".", self.visit_tree.list.items);
                defer self.allocator.free(path);
                const visit_symbol = try self.root_scope.resolve(path);

                if (self.use_loc) {
                    const s = c.content.type.string;
                    for (s.expressions) |*item| {
                        try self.compileExpression(item);
                    }
                    try self.writeOp(.loc, token);
                    try self.writeId(c.id, token);
                    _ = try self.writeInt(u8, @as(u8, @intCast(s.expressions.len)), token);
                } else try self.compileExpression(&c.content);

                try self.writeOp(.choice, token);
                const start_pos = try self.writeInt(OpCode.Size(.jump), CHOICE_HOLDER, token);
                _ = try self.writeInt(u8, if (c.is_unique) 1 else 0, token);
                try self.writeId(c.id, token);
                _ = try self.writeInt(OpCode.Size(.get_global), visit_symbol.?.index, token);
                _ = try self.writeInt(u8, @as(u8, @intCast(c.tags.len)), token);

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
                self.visit_tree.current = self.visit_tree.current.getChild(b.name).?;
                defer self.visit_tree.current = self.visit_tree.current.parent.?;
                defer _ = self.visit_tree.list.pop();

                // skip over bough when running through instructions
                try self.writeOp(.jump, token);
                const start_pos = try self.writeInt(OpCode.Size(.jump), JUMP_HOLDER, token);

                self.jump_tree.current = try self.jump_tree.current.getChild(b.name);
                self.jump_tree.current.*.dest_ip = self.instructionPos();
                defer self.jump_tree.pop();

                try self.enterScope(.local);
                errdefer self.exitScope() catch {};

                const path = try std.mem.join(self.allocator, ".", self.visit_tree.list.items);
                defer self.allocator.free(path);
                const symbol = try self.root_scope.resolve(path);
                try self.compileVisit(symbol, path, token);

                try self.compileBlock(b.body);
                try self.writeOp(.fin, token);

                try self.exitScope();

                const end = self.instructionPos();
                try self.replaceValue(start_pos, OpCode.Size(.jump), end);
            },
            .dialogue => |d| {
                for (d.tags) |tag| {
                    try self.getOrSetIdentifierConstant(tag, token);
                }

                if (self.use_loc) {
                    const s = d.content.type.string;
                    for (s.expressions) |*item| {
                        try self.compileExpression(item);
                    }
                    try self.writeOp(.loc, token);
                    try self.writeId(d.id, token);
                    _ = try self.writeInt(u8, @as(u8, @intCast(s.expressions.len)), token);
                } else try self.compileExpression(d.content);

                if (d.speaker) |speaker| {
                    try self.getOrSetIdentifierConstant(speaker, token);
                }
                try self.writeOp(.dialogue, d.content.token);
                const has_speaker_value = if (d.speaker == null) @as(u8, 0) else @as(u8, 1);
                _ = try self.writeInt(u8, has_speaker_value, token);
                _ = try self.writeInt(u8, @as(u8, @intCast(d.tags.len)), token);
                try self.writeId(d.id, token);
            },
            .divert => |d| {
                if (self.scope == self.root_scope) {
                    const path = try std.mem.join(self.allocator, ".", d.path);
                    defer self.allocator.free(path);
                    return self.fail("Cannot execute jump \"{s}\" in global scope", token, .{path});
                }
                var backup_pos: usize = 0;
                if (d.is_backup) {
                    try self.writeOp(.backup, token);
                    backup_pos = try self.writeInt(OpCode.Size(.jump), JUMP_HOLDER, token);
                }
                try self.writeOp(.divert, token);
                _ = try self.writeInt(u8, @as(u8, @intCast(d.path.len)), token);
                var i: usize = d.path.len;
                while (i > 0) : (i -= 1) {
                    const node = try self.getDivertNode(d.path[0..i], token);
                    try self.divert_log.append(.{ .node = node, .jump_ip = self.instructionPos() });
                    _ = try self.writeInt(OpCode.Size(.divert), DIVERT_HOLDER, token);
                }
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

    fn getDivertNode(self: *Compiler, path: [][]const u8, token: Token) !*JumpTree.Node {
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
            node = node.getChild(name) catch {
                return self.fail("Could not find symbol '{s}'", token, .{name});
            };
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
        const path = try std.mem.join(self.allocator, ".", self.visit_tree.list.items);
        defer self.allocator.free(path);
        const symbol = self.root_scope.define(path, false, false) catch {
            self.visit_tree.print(std.debug);
            return self.fail("Visit '{s}' is already declared", token, .{path});
        };
        try self.writeOp(.constant, token);
        _ = try self.writeInt(OpCode.Size(.constant), 4, token);
        try self.setSymbol(symbol, token, true);
        try self.visit_tree.push(name, symbol.index);
    }

    fn compileVisit(self: *Compiler, symbol: ?*Symbol, name: []const u8, token: Token) Error!void {
        if (symbol) |sym| {
            try self.writeOp(.visit, token);
            _ = try self.writeInt(OpCode.Size(.get_global), sym.index, token);
        } else return self.failError("Unknown symbol '{s}'", token, .{name}, Error.SymbolNotFound);
    }

    pub fn compileExpression(self: *Compiler, expr: *const ast.Expression) Error!void {
        const token = expr.token;
        switch (expr.type) {
            .binary => |bin| {
                if (bin.operator == .less_than or bin.operator == .less_than_equal) {
                    try self.compileExpression(bin.right);
                    try self.compileExpression(bin.left);
                    try self.writeOp(switch (bin.operator) {
                        .less_than => .greater_than,
                        .less_than_equal => .greater_than_equal,
                        else => unreachable,
                    }, token);
                    return;
                }
                if (bin.operator == .assign) {
                    switch (bin.left.type) {
                        .identifier => |id| {
                            try self.compileExpression(bin.right);
                            const symbol = try self.scope.resolve(id);
                            try self.setSymbol(symbol, token, false);
                            try self.loadSymbol(symbol, id, token);
                            return;
                        },
                        .indexer => |idx| {
                            if (idx.target.type == .identifier) {
                                if (self.types.get(idx.target.type.identifier)) |stmt|
                                    return self.fail("Cannot assign value to {s}", token, .{@tagName(stmt.type)});
                            }
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
                    .greater_than_equal => .greater_than_equal,
                    .@"or" => .@"or",
                    .@"and" => .@"and",
                    else => {
                        return self.failError("Unknown operation '{s}'", token, .{bin.operator.toString()}, Error.IllegalOperation);
                    },
                };
                try self.writeOp(op, token);

                switch (bin.operator) {
                    .assign_add, .assign_subtract, .assign_multiply, .assign_divide, .assign_modulus => {
                        switch (bin.left.type) {
                            .identifier => |id| {
                                const symbol = try self.scope.resolve(id);
                                try self.setSymbol(symbol, token, false);
                                try self.loadSymbol(symbol, id, token);
                                return;
                            },
                            .indexer => |idx| {
                                if (idx.target.type == .identifier) {
                                    if (self.types.get(idx.target.type.identifier)) |stmt|
                                        return self.fail("Cannot assign value to {s}", token, .{@tagName(stmt.type)});
                                }
                                try self.compileExpression(bin.left);
                                try self.removeLast(.index);
                                try self.writeOp(.set_property, token);
                                try self.compileExpression(bin.left);
                                return;
                            },
                            else => return self.fail("Cannot assign value of type '{s}'", bin.left.token, .{@tagName(bin.left.type)}),
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
                // remove secondary escape double quotes
                var value = try std.ArrayList(u8).initCapacity(self.allocator, s.value.len);
                defer value.deinit();
                var i: usize = 0;
                while (i < s.value.len) : (i += 1) {
                    if (s.value[i] == '"') i += 1;
                    value.appendAssumeCapacity(s.value[i]);
                }
                obj.* = .{ .data = .{ .string = try value.toOwnedSlice() } };
                const index = try self.addConstant(.{ .obj = obj });
                try self.writeOp(.string, token);
                _ = try self.writeInt(OpCode.Size(.constant), index, token);
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
                    if (idx.target.type == .identifier) {
                        if (self.types.get(idx.target.type.identifier)) |stmt| {
                            switch (stmt.type) {
                                .@"enum" => |e| {
                                    if (!arrayOfTypeContains(u8, e.values, idx.index.type.identifier))
                                        return self.fail("Enum {s} does not contain a value '{s}'", idx.index.token, .{ idx.target.type.identifier, idx.index.type.identifier });
                                },
                                .class => |c| {
                                    if (!arrayOfTypeContains(u8, c.field_names, idx.index.type.identifier))
                                        return self.fail("Class {s} does not contain a field '{s}'", idx.index.token, .{ idx.target.type.identifier, idx.index.type.identifier });
                                },
                                else => {},
                            }
                        }
                    }
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
                if (self.visit_tree.resolve(id)) |node| {
                    try self.loadVisit(node, token);
                    return;
                }
                const symbol = try self.builtins.resolve(id) orelse try self.scope.resolve(id);
                try self.loadSymbol(symbol, id, token);
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

                const chunk = try self.exitChunk();
                const count = self.scope.count;
                const free_symbols = self.scope.free_symbols.items;
                for (free_symbols) |s| {
                    try self.loadSymbol(s, s.name, token);
                }
                try self.exitScope();
                const obj = try self.allocator.create(Value.Obj);

                obj.* = .{
                    .id = UUID.new(),
                    .data = .{
                        .function = .{
                            .is_method = f.is_method,
                            .instructions = chunk[0],
                            .lines = chunk[1],
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
                var cls: ?*const ast.Statement = null;
                if (self.types.get(ins.name)) |stmt| {
                    cls = stmt;
                }
                if (cls == null or cls.?.type != .class) return self.fail("Unknown class {s}", token, .{ins.name});
                for (ins.fields, 0..) |field, i| {
                    if (!arrayOfTypeContains(u8, cls.?.type.class.field_names, ins.field_names[i]))
                        return self.fail("Class {s} does not contain a field named '{s}'", token, .{ ins.name, ins.field_names[i] });
                    try self.compileExpression(&field);
                    try self.getOrSetIdentifierConstant(ins.field_names[i], token);
                }
                const symbol = try self.scope.resolve(ins.name);
                try self.loadSymbol(symbol, ins.name, token);
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
            if (!is_decl and !ptr.is_mutable) return self.fail("Cannot assign to constant variable '{s}'", token, .{ptr.name});
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
                .builtin, .function => return self.failError("Cannot set '{s}'", token, .{ptr.name}, Error.IllegalOperation),
                else => {
                    try self.writeOp(.set_local, token);
                    const size = OpCode.Size(.set_local);
                    _ = try self.writeInt(size, @as(size, @intCast(ptr.index)), token);
                },
            }
        } else return self.failError("Unknown symbol", token, .{}, Error.SymbolNotFound);
    }

    fn loadSymbol(self: *Compiler, symbol: ?*Symbol, name: []const u8, token: Token) !void {
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
        } else return self.failError("Unknown symbol '{s}'", token, .{name}, Error.SymbolNotFound);
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
        try chunk.token_lines.append(@intCast(token.line));
        try chunk.instructions.append(@intFromEnum(op));
    }

    fn writeValue(self: *Compiler, buf: []const u8, token: Token) !void {
        var chunk = self.chunk;
        try chunk.token_lines.appendNTimes(@intCast(token.line), buf.len);
        try chunk.instructions.writer().writeAll(buf);
    }

    fn writeInt(self: *Compiler, comptime T: type, value: T, token: Token) !usize {
        const chunk = self.chunk;
        const start = chunk.instructions.items.len;
        var buf: [@sizeOf(T)]u8 = undefined;
        std.mem.writeInt(T, buf[0..], value, .little);
        try self.writeValue(&buf, token);
        return start;
    }

    pub fn writeId(self: *Compiler, id: UUID.ID, token: Token) !void {
        try self.uuids.append(id);
        _ = try self.writeInt(OpCode.Size(.constant), @as(OpCode.Size(.constant), @intCast(self.uuids.items.len - 1)), token);
    }

    pub fn replaceValue(self: *Compiler, pos: usize, comptime T: type, value: T) !void {
        var buf: [@sizeOf(T)]u8 = undefined;
        std.mem.writeInt(T, buf[0..], value, .little);
        var chunk = self.chunk;
        for (buf, 0..) |v, i| {
            chunk.instructions.items[pos + i] = v;
        }
    }

    pub fn replaceJumps(instructions: []u8, old_pos: OpCode.Size(.jump), new_pos: OpCode.Size(.jump)) !void {
        var i: usize = 0;
        const jump = @intFromEnum(OpCode.jump);
        while (i < instructions.len) : (i += 1) {
            if (instructions[i] == jump and std.mem.readVarInt(OpCode.Size(.jump), instructions[(i + 1)..(i + @sizeOf(OpCode.Size(.jump)))], .little) == old_pos) {
                var buf: [@sizeOf(OpCode.Size(.jump))]u8 = undefined;
                std.mem.writeInt(OpCode.Size(.jump), buf[0..], new_pos, .little);
                for (buf, 0..) |v, index| {
                    instructions[i + index + 1] = v;
                }
            }
        }
    }

    pub fn isVisitExpression(self: *Compiler, index: *const ast.Expression, token: Token) !bool {
        var idx = index.type.indexer;
        if (idx.index.type != .identifier) return false;
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

        const joined = try std.mem.join(self.allocator, ".", list.items);
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
        for (initial_constants) |c| {
            _ = try self.addConstant(c);
        }
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
