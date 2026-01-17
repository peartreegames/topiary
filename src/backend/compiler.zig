const std = @import("std");

const frontend = @import("../frontend/index.zig");
const Statement = frontend.Statement;
const Expression = frontend.Expression;
const Token = frontend.Token;

const utils = @import("../utils/index.zig");
const UUID = utils.UUID;
const C = utils.C;
const fmt = utils.fmt;

const builtins = @import("../runtime/index.zig").builtins;

const mod = @import("../module.zig");
const Module = mod.Module;
const File = mod.File;

const types = @import("../types/index.zig");
const Value = types.Value;
const Function = types.Function;
const Anchor = types.Anchor;

const scope = @import("scope.zig");
const Scope = scope.Scope;
const Symbol = scope.Symbol;

const DebugInfo = @import("debug.zig").DebugInfo;
const CompilerErrors = @import("error.zig").CompilerErrors;
const Bytecode = @import("bytecode.zig").Bytecode;
const OpCode = @import("opcode.zig").OpCode;

const BREAK_HOLDER = 9000;
const CONTINUE_HOLDER = 9001;
const CHOICE_HOLDER = 9002;
const FORK_HOLDER = 9003;
const DIVERT_HOLDER = 9004;
const PRONG_HOLDER = 9005;
const SWITCH_END_HOLDER = 9006;
const JUMP_HOLDER = 9999;

fn arrayContains(comptime T: type, haystack: []const []const T, needle: []const T) bool {
    for (haystack) |element| {
        if (std.mem.eql(T, element, needle)) return true;
    }
    return false;
}
pub const Compiler = struct {
    alloc: std.mem.Allocator,
    constants: std.ArrayList(Value) = .empty,
    constants_map: std.StringHashMapUnmanaged(C.CONSTANT) = .empty,
    literal_cache: std.ArrayHashMapUnmanaged(Value, C.CONSTANT, Value.Adapter, true) = .empty,
    scope: *Scope,
    root_scope: *Scope,
    chunk: *Chunk,
    locals_count: usize = 0,

    module: *Module,
    current_file: *File,
    emitted_files: std.StringArrayHashMapUnmanaged(void) = .empty,
    path_stack: std.ArrayList([]const u8) = .empty,
    anon_counters: std.ArrayList(usize) = .empty,

    pub const Chunk = struct {
        instructions: std.ArrayList(u8) = .empty,
        debug_markers: std.ArrayList(Marker) = .empty,
        parent: ?*Chunk,
        module: *Module,
        alloc: std.mem.Allocator,

        const Marker = struct {
            file_index: u32,
            line: u32,
        };

        pub fn init(allocator: std.mem.Allocator, parent: ?*Chunk, module: *Module) !*Chunk {
            const chunk = try allocator.create(Chunk);
            chunk.* = .{
                .module = module,
                .parent = parent,
                .alloc = allocator,
            };
            return chunk;
        }

        pub fn debugInfo(self: *Chunk, allocator: std.mem.Allocator) ![]DebugInfo {
            var infos: std.ArrayList(DebugInfo) = .empty;
            defer infos.deinit(allocator);
            if (self.debug_markers.items.len == 0) return infos.toOwnedSlice(self.alloc);
            var file_index = self.debug_markers.items[0].file_index;
            var line = self.debug_markers.items[0].line;
            var start: u32 = 0;
            var file_name = try allocator.dupe(u8, std.fs.path.basename(self.module.includes.keys()[file_index]));

            try infos.append(allocator, DebugInfo.init(allocator, file_name));
            var info: *DebugInfo = &(infos.items[0]);
            for (self.debug_markers.items, 0..) |d, ip| {
                const end: u32 = @intCast(ip);
                // file changed make a new debug info or find an existing one
                if (file_index != d.file_index and d.file_index < self.module.includes.count()) {
                    try info.ranges.append(allocator, .{ .start = start, .end = end, .line = line });
                    line = d.line;
                    start = end;

                    file_index = d.file_index;
                    const name = std.fs.path.basename(self.module.includes.keys()[file_index]);
                    info = for (infos.items, 0..) |item, i| {
                        if (!std.mem.eql(u8, name, item.file)) continue;
                        break &(infos.items[i]);
                    } else blk: {
                        file_name = try allocator.dupe(u8, file_name);
                        const new_info = DebugInfo.init(allocator, file_name);
                        try infos.append(allocator, new_info);
                        break :blk &(infos.items[infos.items.len - 1]);
                    };
                    continue;
                }
                // line changed, add new range to debug info
                if (d.line != line) {
                    try info.ranges.append(allocator, .{ .start = start, .end = end, .line = line });
                    line = d.line;
                    start = end;
                }
            }
            try info.ranges.append(allocator, .{ .start = start, .end = @intCast(self.debug_markers.items.len), .line = line });
            return try infos.toOwnedSlice(allocator);
        }

        pub fn deinit(self: *Chunk) void {
            self.instructions.deinit(self.alloc);
            self.debug_markers.deinit(self.alloc);
            self.alloc.destroy(self);
        }
    };

    pub const Error = error{
        ParserError,
        CompilerError,
        NotInitialized,
        IllegalOperation,
        OutOfScope,
        NoSpaceLeft,
        OutOfMemory,
        SymbolNotFound,
        SymbolAlreadyDeclared,
        ExternError,
        NotYetImplemented,
    };

    pub fn init(alloc: std.mem.Allocator, module: *Module) !Compiler {
        if (module.entry.tree == null) return error.CompilerError;
        const root_chunk = try Compiler.Chunk.init(alloc, null, module);
        const root_scope = try Scope.create(alloc, null, .global);
        return .{
            .alloc = alloc,
            .chunk = root_chunk,
            .scope = root_scope,
            .root_scope = root_scope,
            .module = module,
            .current_file = module.entry,
        };
    }

    pub fn deinit(self: *Compiler) void {
        self.chunk.deinit();
        var current_scope: ?*Scope = self.scope;
        while (current_scope) |s| {
            current_scope = s.parent;
            s.destroy();
        }
        for (self.constants.items) |item| {
            item.destroy(self.alloc);
        }
        var iter = self.constants_map.keyIterator();
        while (iter.next()) |name| {
            self.alloc.free(name.*);
        }
        self.emitted_files.deinit(self.alloc);
        self.constants_map.deinit(self.alloc);
        self.literal_cache.deinit(self.alloc);
        self.constants.deinit(self.alloc);
        self.path_stack.deinit(self.alloc);
        self.anon_counters.deinit(self.alloc);
    }

    fn fail(self: *Compiler, comptime msg: []const u8, token: Token, args: anytype) Error {
        try self.module.errors.add(self.current_file.path, msg, token, .err, args);
        return Error.CompilerError;
    }

    fn failError(self: *Compiler, comptime msg: []const u8, token: Token, args: anytype, err: Error) Error {
        try self.module.errors.add(self.current_file.path, msg, token, .err, args);
        return err;
    }

    pub fn bytecode(self: *Compiler) !Bytecode {
        if (self.scope.parent != null) return Error.OutOfScope;
        var global_symbols = try self.alloc.alloc(Bytecode.GlobalSymbol, self.scope.symbols.count());
        for (self.scope.symbols.values(), 0..) |s, i| {
            global_symbols[i] = Bytecode.GlobalSymbol{
                .name = try self.alloc.dupe(u8, s.name),
                .index = s.index,
                .is_mutable = s.is_mutable,
            };
        }
        return .{
            .instructions = try self.chunk.instructions.toOwnedSlice(self.alloc),
            .debug_info = try self.chunk.debugInfo(self.alloc),
            .constants = try self.constants.toOwnedSlice(self.alloc),
            .global_symbols = global_symbols,
            .locals_count = self.locals_count,
        };
    }

    pub fn compile(self: *Compiler) Error!void {
        const tree = self.module.entry.tree orelse return Error.IllegalOperation;
        for (builtins.keys()) |name| {
            const obj = try self.alloc.create(Value.Obj);
            obj.* = .{ .data = builtins.get(name).?.obj.data };
            try self.addNamedConstant(name, .{ .obj = obj });
        }
        for (tree.root) |stmt| {
            try self.prepass(stmt);
        }
        self.emitted_files.clearRetainingCapacity();

        for (tree.root) |stmt| {
            try self.compileStatement(stmt);
        }

        // Add one final end at the end of file to grab the initial jump_request
        if (self.chunk.debug_markers.items.len > 0) {
            const dupe = self.chunk.debug_markers.items[self.chunk.debug_markers.items.len - 1];
            try self.chunk.debug_markers.append(self.alloc, dupe);
            try self.chunk.instructions.append(self.alloc, @intFromEnum(OpCode.end));
        }
    }

    fn enterChunk(self: *Compiler) !void {
        self.chunk = try Chunk.init(self.alloc, self.chunk, self.module);
    }

    // Caller owns memory and must deinit returned chunk
    fn exitChunk(self: *Compiler) !*Chunk {
        const old_chunk = self.chunk;
        self.chunk = old_chunk.parent orelse return Error.OutOfScope;
        return old_chunk;
    }

    fn enterScope(self: *Compiler, tag: Scope.Tag) !void {
        self.scope = try Scope.create(self.alloc, self.scope, tag);
    }

    fn exitScope(self: *Compiler) !void {
        const old_scope = self.scope;
        self.scope = old_scope.parent orelse return Error.OutOfScope;
        self.locals_count = @max(self.locals_count, self.scope.count + old_scope.count);
        old_scope.destroy();
    }

    fn prepass(self: *Compiler, stmt: Statement) Error!void {
        switch (stmt.type) {
            .include => |i| {
                const tmp = self.current_file;
                defer self.current_file = tmp;

                const file = self.module.includes.get(i) orelse
                    return self.fail("Unknown include file {s}", stmt.token, .{i});

                if (self.emitted_files.contains(i)) return;
                try self.emitted_files.put(self.alloc, i, {});

                self.current_file = file;
                const tree = file.tree orelse return Error.NotInitialized;
                for (tree.root) |s| try self.prepass(s);
            },
            .function => |f| {
                const full_name = try self.getQualifiedName(f.name);
                defer self.alloc.free(full_name);
                try self.addNamedConstant(full_name, .nil);
                if (f.is_extern) {
                    _ = try self.addConstant(.{ .obj = try self.compileExternFunctionObj(stmt) });
                }
            },
            .class => |c| {
                const full_name = try self.getQualifiedName(c.name);
                defer self.alloc.free(full_name);
                try self.addNamedConstant(full_name, .nil);
            },
            .@"enum" => |e| {
                const full_name = try self.getQualifiedName(e.name);
                defer self.alloc.free(full_name);
                try self.addNamedConstant(full_name, .nil);
            },
            .bough => |b| {
                const full_name = try self.getQualifiedName(b.name);
                self.registerAnchor(full_name) catch |err| return self.fail("Could not register anchor {s}: {t}", stmt.token, .{ b.name, err });
                try self.path_stack.append(self.alloc, b.name);
                try self.anon_counters.append(self.alloc, 0);
                defer _ = self.path_stack.pop();
                defer _ = self.anon_counters.pop();
                for (b.body) |s| try self.prepass(s);
            },
            .fork => |f| {
                var fork_name: []const u8 = undefined;
                if (f.name) |name| {
                    fork_name = try self.alloc.dupe(u8, name);
                } else {
                    const current_depth = self.anon_counters.items.len - 1;
                    const count = self.anon_counters.items[current_depth];
                    fork_name = try std.fmt.allocPrint(self.alloc, "_{d}", .{count});
                    self.anon_counters.items[current_depth] += 1;
                }

                defer self.alloc.free(fork_name);

                const full_name = try self.getQualifiedName(fork_name);
                self.registerAnchor(full_name) catch |err| return self.fail("Could not register anchor {s}: {t}", stmt.token, .{ fork_name, err });
                try self.path_stack.append(self.alloc, fork_name);
                try self.anon_counters.append(self.alloc, 0);
                defer _ = self.path_stack.pop();
                defer _ = self.anon_counters.pop();

                for (f.body) |s| try self.prepass(s);
            },
            .choice => |c| {
                const name = c.name orelse &c.id;
                const full_name = try self.getQualifiedName(name);
                self.registerAnchor(full_name) catch |err| return self.fail("Could not register anchor {s}: {t}", stmt.token, .{ name, err });
                try self.path_stack.append(self.alloc, name);
                defer _ = self.path_stack.pop();
                for (c.body) |s| try self.prepass(s);
            },
            .@"if" => |i| {
                for (i.then_branch) |s| try self.prepass(s);
                if (i.else_branch) |e| {
                    for (e) |s| try self.prepass(s);
                }
            },
            .@"while" => |w| {
                for (w.body) |s| try self.prepass(s);
            },
            .@"for" => |f| {
                for (f.body) |s| try self.prepass(s);
            },
            else => {},
        }
    }

    pub fn compileStatement(self: *Compiler, stmt: Statement) Error!void {
        const token = stmt.token;
        switch (stmt.type) {
            .include => |i| {
                const tmp_file = self.current_file;
                defer self.current_file = tmp_file;

                const file = self.module.includes.get(i) orelse
                    return self.fail("Unknown include file {s}", stmt.token, .{i});

                if (self.emitted_files.contains(i)) return;
                try self.emitted_files.put(self.alloc, i, {});

                self.current_file = file;

                const tree = file.tree orelse return Error.NotInitialized;
                for (tree.root) |s| try self.compileStatement(s);
            },
            .@"if" => |i| {
                try self.compileExpression(i.condition);
                try self.writeOp(.jump_if_false, token);
                const falsePos = try self.writeInt(C.JUMP, JUMP_HOLDER, token);
                try self.compileBlock(i.then_branch);

                try self.writeOp(.jump, token);
                const jumpPos = try self.writeInt(C.JUMP, JUMP_HOLDER, token);
                try self.replaceValue(falsePos, C.JUMP, self.instructionPos());

                if (i.else_branch == null) {
                    try self.writeOp(.nil, token);
                    try self.writeOp(.pop, token);
                    try self.replaceValue(jumpPos, C.JUMP, self.instructionPos());
                    return;
                }
                try self.compileBlock(i.else_branch.?);
                try self.replaceValue(jumpPos, C.JUMP, self.instructionPos());
            },
            .function => |f| {
                if (self.scope.parent != null and f.is_extern)
                    return self.failError("Only global functions can be extern.", token, .{}, Error.IllegalOperation);
                const obj = try self.compileFunctionObj(stmt, token);
                const full_name = try self.getQualifiedName(f.name);
                defer self.alloc.free(full_name);
                try self.replaceConstant(full_name, .{ .obj = obj }, token);
            },
            .@"switch" => |s| {
                const start = self.instructionPos();
                try self.compileExpression(&s.capture);
                var prong_jumps = try self.alloc.alloc(usize, s.prongs.len);
                var inferred_else_jump: ?usize = null;
                defer self.alloc.free(prong_jumps);
                // compile expressions and jumps
                var has_else = false;
                for (s.prongs, 0..) |prong_stmt, i| {
                    const prong = prong_stmt.type.switch_prong;
                    if (prong.values) |p| {
                        for (p) |value| {
                            try self.compileExpression(&value);
                        }
                    } else {
                        has_else = true;
                    }
                    try self.writeOp(.prong, prong_stmt.token);
                    const prong_jump = try self.writeInt(C.JUMP, PRONG_HOLDER, prong_stmt.token);
                    _ = try self.writeInt(u8, @as(u8, @intCast(if (prong.values) |p| p.len else 0)), prong_stmt.token);
                    prong_jumps[i] = prong_jump;
                }
                // add an empty else if none found
                if (!has_else) {
                    try self.writeOp(.prong, token);
                    const prong_jump = try self.writeInt(C.JUMP, PRONG_HOLDER, token);
                    _ = try self.writeInt(u8, 0, token);
                    inferred_else_jump = prong_jump;
                }

                // replace jumps and compile body
                for (s.prongs, 0..) |prong_stmt, i| {
                    const prong = prong_stmt.type.switch_prong;
                    try self.replaceValue(prong_jumps[i], C.JUMP, self.instructionPos());
                    try self.enterScope(.local);
                    try self.compileBlock(prong.body);
                    try self.writeOp(.jump, prong_stmt.token);
                    _ = try self.writeInt(C.JUMP, SWITCH_END_HOLDER, prong_stmt.token);
                    try self.exitScope();
                }

                if (inferred_else_jump) |jump| {
                    try self.replaceValue(jump, C.JUMP, self.instructionPos());
                    try self.enterScope(.local);
                    try self.compileBlock(&[_]Statement{});
                    try self.writeOp(.jump, token);
                    _ = try self.writeInt(C.JUMP, SWITCH_END_HOLDER, token);
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
                _ = try self.writeInt(C.JUMP, BREAK_HOLDER, token);
            },
            .@"continue" => {
                try self.writeOp(.jump, token);
                _ = try self.writeInt(C.JUMP, CONTINUE_HOLDER, token);
            },
            .@"while" => |w| {
                try self.enterScope(.local);

                const start = self.instructionPos();
                try self.compileExpression(&w.condition);
                try self.writeOp(.jump_if_false, token);
                const temp_start = try self.writeInt(C.JUMP, JUMP_HOLDER, token);

                try self.compileBlock(w.body);
                try self.writeOp(.jump, token);
                _ = try self.writeInt(C.JUMP, start, token);

                const end = self.instructionPos();
                try self.replaceValue(temp_start, C.JUMP, end);

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
                const jump_end = try self.writeInt(C.JUMP, JUMP_HOLDER, token);

                try self.enterScope(.local);
                try self.writeOp(.set_local, token);
                _ = try self.writeInt(C.LOCAL, 0, token);
                _ = try self.scope.define(f.capture, false);

                try self.compileBlock(f.body);
                try self.writeOp(.jump, token);
                _ = try self.writeInt(C.JUMP, start, token);

                const end = self.instructionPos();
                try self.replaceValue(jump_end, C.JUMP, end);
                try replaceJumps(self.chunk.instructions.items[start..], BREAK_HOLDER, end);
                try replaceJumps(self.chunk.instructions.items[start..], CONTINUE_HOLDER, start);

                try self.exitScope();
                try self.writeOp(.pop, token);

                try self.writeOp(.iter_end, token);
                // pop item
                try self.writeOp(.pop, token);
            },
            .variable => |v| {
                if (builtins.has(v.name))
                    return self.failError("'{s}' is a builtin function and cannot be used as a variable name", stmt.token, .{v.name}, Error.IllegalOperation);
                const symbol = self.scope.define(v.name, v.is_mutable) catch {
                    return self.fail("'{s}' is already declared", token, .{v.name});
                };
                try self.compileExpression(&v.initializer);
                try self.setSymbol(v.name, symbol, token, true);
            },
            .class => |c| {
                var fields = try self.alloc.alloc(types.Class.Member, c.fields.len);
                errdefer self.alloc.free(fields);

                for (c.fields, 0..) |field_expr, i| {
                    fields[i] = .{
                        .name = try self.alloc.dupe(u8, c.field_names[i]),
                        .value = try self.evaluateLiteral(&field_expr),
                    };
                }
                var methods = try self.alloc.alloc(types.Class.Member, c.methods.len);
                errdefer self.alloc.free(methods);

                for (c.methods, 0..) |method_stmt, i| {
                    const func = method_stmt.type.function;
                    const name = func.name;

                    const func_obj = try self.compileFunctionObj(method_stmt, method_stmt.token);

                    methods[i] = .{
                        .name = try self.alloc.dupe(u8, name),
                        .value = .{ .obj = func_obj },
                    };
                }
                const class_data = try types.Class.init(try self.alloc.dupe(u8, c.name), fields, methods);
                const obj = try self.alloc.create(Value.Obj);
                obj.* = .{
                    .id = UUID.new(),
                    .data = .{ .class = class_data },
                };

                const full_name = try self.getQualifiedName(c.name);
                defer self.alloc.free(full_name);
                try self.replaceConstant(full_name, .{ .obj = obj }, token);
            },
            .@"enum" => |e| {
                var values = try self.alloc.alloc([]const u8, e.values.len);
                const obj = try self.alloc.create(Value.Obj);

                for (e.values, 0..) |value, i| {
                    values[i] = try self.alloc.dupe(u8, value);
                }

                obj.* = .{
                    .id = UUID.fromStringHash(e.name),
                    .data = .{
                        .@"enum" = .{
                            .is_seq = e.is_seq,
                            .name = try self.alloc.dupe(u8, e.name),
                            .values = values,
                        },
                    },
                };

                const full_name = try self.getQualifiedName(e.name);
                defer self.alloc.free(full_name);
                try self.replaceConstant(full_name, .{ .obj = obj }, token);
            },
            .fork => |f| {
                var fork_name: []const u8 = undefined;
                if (f.name) |name| {
                    fork_name = try self.alloc.dupe(u8, name);
                } else {
                    const current_depth = self.anon_counters.items.len - 1;
                    const count = self.anon_counters.items[current_depth];
                    fork_name = try std.fmt.allocPrint(self.alloc, "_{d}", .{count});
                    self.anon_counters.items[current_depth] += 1;
                }
                defer self.alloc.free(fork_name);

                const path = try self.getQualifiedName(fork_name);
                defer self.alloc.free(path);

                try self.path_stack.append(self.alloc, fork_name);
                try self.anon_counters.append(self.alloc, 0);
                defer _ = self.path_stack.pop();
                defer _ = self.anon_counters.pop();

                const start_pos = self.instructionPos();
                const anchor_idx = try self.resolveConstant(path);
                if (anchor_idx) |idx| {
                    self.constants.items[idx].obj.data.anchor.ip = start_pos;
                    try self.compileVisit(idx, token);
                } else return self.fail("Could not find anchor {s}", token, .{path});

                try self.enterScope(.local);
                try self.compileBlock(f.body);
                try self.exitScope();

                var backup_pos: usize = 0;
                if (f.is_backup) {
                    try self.writeOp(.backup, token);
                    backup_pos = try self.writeInt(C.JUMP, JUMP_HOLDER, token);
                }
                try self.writeOp(.fork, token);
                const end_pos = self.instructionPos();
                if (f.is_backup) {
                    try self.replaceValue(backup_pos, C.JUMP, end_pos);
                }
            },
            .choice => |c| {
                for (c.tags) |tag| {
                    try self.addIdentifierConstant(tag, token);
                }
                const name = c.name orelse &c.id;
                const full_name = try self.getQualifiedName(name);
                defer self.alloc.free(full_name);

                try self.path_stack.append(self.alloc, name);
                defer _ = self.path_stack.pop();

                const entry_ip = self.instructionPos();
                const anchor_idx = try self.resolveConstant(full_name) orelse return self.fail("Could not find anchor {s}", token, .{full_name});
                self.constants.items[anchor_idx].obj.data.anchor.ip = entry_ip;

                const s = c.content.type.string;
                for (s.expressions) |*item| {
                    try self.compileExpression(item);
                }

                const obj = try self.alloc.create(Value.Obj);
                obj.* = .{
                    .id = c.id,
                    .data = .{
                        .string = try self.alloc.dupe(u8, c.content.type.string.value),
                    },
                };

                const index = try self.addConstant(.{ .obj = obj });

                try self.writeOp(.loc, token);
                _ = try self.writeInt(C.CONSTANT, index, token);
                _ = try self.writeInt(u8, @as(u8, @intCast(s.expressions.len)), token);

                try self.writeOp(.choice, token);
                const start_pos = try self.writeInt(C.JUMP, CHOICE_HOLDER, token);
                _ = try self.writeInt(u8, if (c.is_unique) 1 else 0, token);

                _ = try self.writeInt(C.CONSTANT, anchor_idx, token);
                _ = try self.writeInt(u8, @as(u8, @intCast(c.tags.len)), token);

                try self.writeOp(.jump, token);
                const jump_pos = try self.writeInt(C.JUMP, JUMP_HOLDER, token);
                try self.replaceValue(start_pos, C.JUMP, self.instructionPos());

                try self.enterScope(.local);

                try self.compileVisit(anchor_idx, token);
                try self.compileBlock(c.body);
                try self.writeOp(.end, token);
                try self.exitScope();
                try self.replaceValue(jump_pos, C.JUMP, self.instructionPos());
            },
            .bough => |b| {
                const full_name = try self.getQualifiedName(b.name);
                defer self.alloc.free(full_name);

                try self.path_stack.append(self.alloc, b.name);
                try self.anon_counters.append(self.alloc, 0);
                defer _ = self.path_stack.pop();
                defer _ = self.anon_counters.pop();

                try self.writeOp(.jump, token);
                const start_pos = try self.writeInt(C.JUMP, JUMP_HOLDER, token);

                const locals_count = self.scope.count;
                try self.enterScope(.local);
                self.scope.count = locals_count;
                errdefer self.exitScope() catch {};

                const entry_ip = self.instructionPos();
                const anchor_idx = try self.resolveConstant(full_name) orelse return self.fail("Could not find anchor {s}", token, .{full_name});
                self.constants.items[anchor_idx].obj.data.anchor.ip = entry_ip;
                try self.compileVisit(anchor_idx, token);

                try self.compileBlock(b.body);
                try self.writeOp(.end, token);
                try self.exitScope();

                const end = self.instructionPos();
                try self.replaceValue(start_pos, C.JUMP, end);
            },
            .dialogue => |d| {
                for (d.tags) |tag| {
                    try self.addIdentifierConstant(tag, token);
                }

                const s = d.content.type.string;
                for (s.expressions) |*item| {
                    try self.compileExpression(item);
                }

                const obj = try self.alloc.create(Value.Obj);
                obj.* = .{
                    .id = d.id,
                    .data = .{
                        .string = try self.alloc.dupe(u8, s.value),
                    },
                };

                const index = try self.addConstant(.{ .obj = obj });

                try self.writeOp(.loc, token);
                _ = try self.writeInt(C.CONSTANT, index, token);
                _ = try self.writeInt(u8, @as(u8, @intCast(s.expressions.len)), token);

                if (d.speaker) |speaker| {
                    try self.addIdentifierConstant(speaker, token);
                }
                try self.writeOp(.dialogue, d.content.token);
                const has_speaker_value = if (d.speaker == null) @as(u8, 0) else @as(u8, 1);
                _ = try self.writeInt(u8, has_speaker_value, token);
                _ = try self.writeInt(u8, @as(u8, @intCast(d.tags.len)), token);
            },
            .divert => |d| {
                const anchor_idx = self.resolveAnchor(d.path) orelse return self.fail("Could not find anchor {f}", token, .{fmt.array("{s}.", d.path)});
                if (d.is_backup) {
                    try self.writeOp(.backup, token);
                    const backup_pos = try self.writeInt(C.JUMP, JUMP_HOLDER, token);
                    try self.writeOp(.divert, token);
                    _ = try self.writeInt(C.CONSTANT, anchor_idx, token);
                    try self.replaceValue(backup_pos, C.JUMP, self.instructionPos());
                } else {
                    try self.writeOp(.divert, token);
                    _ = try self.writeInt(C.CONSTANT, anchor_idx, token);
                }
            },
            .return_expression => |r| {
                try self.compileExpression(&r);
                try self.writeOp(.return_value, token);
            },
            .return_void => {
                try self.writeOp(.return_void, token);
            },
            .fin => {
                try self.writeOp(.fin, token);
            },
            else => {},
        }
    }

    fn evaluateLiteral(self: *Compiler, expr: *const Expression) Error!Value {
        const token = expr.token;
        switch (expr.type) {
            .binary => |bin| {
                const left = try self.evaluateLiteral(bin.left);
                const right = try self.evaluateLiteral(bin.right);

                if (left == .number and right == .number) {
                    return .{
                        .number = switch (bin.operator) {
                            .add => left.number + right.number,
                            .subtract => left.number - right.number,
                            .multiply => left.number * right.number,
                            .divide => if (right.number == 0) return self.failError("Division by zero in static expression", token, .{}, Error.IllegalOperation) else left.number / right.number,
                            else => return self.failError("Operator not supported in static expressions", token, .{}, Error.IllegalOperation),
                        },
                    };
                }
                return self.failError("Static math only supported on numbers", token, .{}, Error.IllegalOperation);
            },
            .unary => |u| {
                const val = try self.evaluateLiteral(u.value);
                if (val == .number and u.operator == .negate) return .{ .number = -val.number };
                if (val == .bool and u.operator == .not) return .{ .bool = !val.bool };
                return self.failError("Unary operator not supported in static expressions", token, .{}, Error.IllegalOperation);
            },
            .number => |n| return .{ .number = n },
            .boolean => |b| return .{ .bool = b },
            .string => |s| {
                if (s.expressions.len > 0) {
                    return self.failError("Interpolated strings are not allowed as static default values", token, .{}, Error.IllegalOperation);
                }
                const obj = try self.alloc.create(Value.Obj);
                obj.* = .{ .data = .{ .string = try self.alloc.dupe(u8, s.value) } };
                return .{ .obj = obj };
            },
            .identifier => |id| {
                // Allow referencing other constants (like Enums or other Classes)
                if (self.constants_map.get(id)) |idx| {
                    return self.constants.items[idx];
                }
                return self.failError("Identifier '{s}' cannot be resolved", token, .{id}, Error.CompilerError);
            },
            .indexer => |idx| {
                // Handle Enum.Value references
                if (idx.target.type == .identifier and token.token_type == .dot) {
                    const target_name = idx.target.type.identifier;
                    if (self.constants_map.get(target_name)) |target_idx| {
                        const target_val = self.constants.items[target_idx];
                        if (target_val == .obj and target_val.obj.data == .@"enum") {
                            const enum_obj = target_val.obj.data.@"enum";
                            const field_name = idx.index.type.identifier;

                            for (enum_obj.values, 0..) |val_name, i| {
                                if (std.mem.eql(u8, val_name, field_name)) {
                                    return .{ .number = @floatFromInt(i) };
                                }
                            }
                        }
                        if (target_val == .obj and target_val.obj.data == .class) {
                            const class_obj = target_val.obj.data.class;
                            const field_name = idx.index.type.identifier;

                            for (class_obj.fields) |member| {
                                if (std.mem.eql(u8, member.name, field_name)) {
                                    return member.value;
                                }
                            }
                        }
                    }
                }
                return self.failError("Expression cannot be evaluated", token, .{}, Error.IllegalOperation);
            },
            .nil => return .nil,
            .list => |l| {
                var list = try std.ArrayList(Value).initCapacity(self.alloc, l.len);
                for (l) |*item| {
                    try list.append(self.alloc, try self.evaluateLiteral(item));
                }
                const obj = try self.alloc.create(Value.Obj);
                obj.* = .{ .id = UUID.new(), .data = .{ .list = list } };
                return .{ .obj = obj };
            },
            .set => |s| {
                var set = Value.Obj.SetType.empty;
                for (s) |*item| {
                    try set.put(self.alloc, try self.evaluateLiteral(item), {});
                }
                const obj = try self.alloc.create(Value.Obj);
                obj.* = .{ .id = UUID.new(), .data = .{ .set = set } };
                return .{ .obj = obj };
            },
            .map => |m| {
                var map = Value.Obj.MapType.empty;
                for (m) |*mp| {
                    const pair = mp.type.map_pair;
                    try map.put(self.alloc, try self.evaluateLiteral(pair.key), try self.evaluateLiteral(pair.value));
                }
                const obj = try self.alloc.create(Value.Obj);
                obj.* = .{ .id = UUID.new(), .data = .{ .map = map } };
                return .{ .obj = obj };
            },
            else => return self.failError("Expression of type '{s}' is not a valid static literal", token, .{@tagName(expr.type)}, Error.IllegalOperation),
        }
    }

    fn compileExternFunctionObj(self: *Compiler, stmt: Statement) !*Value.Obj {
        const f = stmt.type.function;
        const obj = try self.alloc.create(Value.Obj);
        obj.* = .{ .data = .{ .@"extern" = .{
            .name = f.name,
            .arity = @intCast(f.parameters.len),
        } } };
        return obj;
    }

    fn compileFunctionObj(self: *Compiler, stmt: Statement, token: Token) !*Value.Obj {
        try self.enterScope(.function);
        try self.enterChunk();
        const f = stmt.type.function;

        var length = f.parameters.len;
        if (f.is_method) {
            length += 1;
            _ = try self.scope.define("self", false);
        }

        for (f.parameters) |param| {
            _ = try self.scope.define(param, true);
        }

        try self.compileBlock(f.body);
        if (!(try self.lastIs(.return_value)) and !(try self.lastIs(.return_void))) {
            try self.writeOp(.return_void, token);
        }

        const chunk = try self.exitChunk();
        defer chunk.deinit();
        const count = self.scope.count;

        try self.exitScope();
        const obj = try self.alloc.create(Value.Obj);

        obj.* = .{
            .id = UUID.new(),
            .data = .{
                .function = .{
                    .arity = @as(u8, @intCast(length)),
                    .is_method = f.is_method,
                    .instructions = try chunk.instructions.toOwnedSlice(self.alloc),
                    .debug_info = try chunk.debugInfo(self.alloc),
                    .locals_count = count,
                },
            },
        };
        return obj;
    }

    fn getQualifiedName(self: *Compiler, name: []const u8) ![]const u8 {
        if (self.path_stack.items.len == 0) {
            return try self.alloc.dupe(u8, name);
        }

        const path = try std.mem.join(self.alloc, ".", self.path_stack.items);
        defer self.alloc.free(path);
        return try std.fmt.allocPrint(self.alloc, "{s}.{s}", .{ path, name });
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

    pub fn compileBlock(self: *Compiler, stmts: []const Statement) Error!void {
        for (stmts) |stmt| {
            try self.compileStatement(stmt);
        }
    }

    fn registerAnchor(self: *Compiler, full_name: []const u8) !void {
        const visit_sym = try self.root_scope.define(full_name, false);

        var parent_idx: ?C.CONSTANT = null;
        if (self.path_stack.items.len > 0) {
            const parent_path = try std.mem.join(self.alloc, ".", self.path_stack.items);
            defer self.alloc.free(parent_path);
            parent_idx = self.constants_map.get(parent_path);
        }

        const anchor_obj = try self.alloc.create(Value.Obj);
        anchor_obj.* = .{
            .id = UUID.fromStringHash(full_name),
            .data = .{
                .anchor = .{
                    .name = full_name,
                    .visit_index = visit_sym.index,
                    .parent_anchor_index = parent_idx,
                    .ip = 0,
                },
            },
        };

        try self.addNamedConstant(full_name, .{ .obj = anchor_obj });
    }

    fn compileVisit(self: *Compiler, anchor_idx: C.CONSTANT, token: Token) !void {
        try self.writeOp(.visit, token);
        _ = try self.writeInt(C.CONSTANT, anchor_idx, token);
    }

    pub fn compileExpression(self: *Compiler, expr: *const Expression) Error!void {
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
                            try self.setSymbol(id, symbol, token, false);
                            try self.loadSymbol(id, token);
                            return;
                        },
                        .indexer => |idx| {
                            if (idx.target.type == .identifier) {
                                if (self.constants_map.get(idx.target.type.identifier)) |i| {
                                    const val = self.constants.items[i];
                                    return self.fail("Cannot reassign {t} field", token, .{val.obj.data});
                                }
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
                                try self.setSymbol(id, symbol, token, false);
                                try self.loadSymbol(id, token);
                                return;
                            },
                            .indexer => |idx| {
                                if (idx.target.type == .identifier) {
                                    if (self.constants_map.get(idx.target.type.identifier)) |i| {
                                        const val = self.constants.items[i];
                                        if (val == .obj and (val.obj.data == .class or val.obj.data == .@"enum" or val.obj.data == .function)) {
                                            return self.fail("Cannot assign value to {t}", token, .{val.obj.data});
                                        }
                                    }
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
                const i = try self.addLiteralConstant(.{ .number = n });
                try self.writeOp(.constant, token);
                _ = try self.writeInt(C.CONSTANT, i, token);
            },
            .boolean => |b| try self.writeOp(if (b) .true else .false, token),
            .string => |s| {
                for (s.expressions) |*item| {
                    try self.compileExpression(item);
                }

                var value = try std.ArrayList(u8).initCapacity(self.alloc, s.value.len);
                errdefer value.deinit(self.alloc);
                var i: usize = 0;
                while (i < s.value.len) {
                    if (s.value[i] == '\\' and i + 1 < s.value.len) {
                        i += 1; // skip escape
                        const escaped = switch (s.value[i]) {
                            'n' => '\n',
                            't' => '\t',
                            'r' => '\r',
                            '\\' => '\\',
                            '"' => '"',
                            '{' => '{',
                            '}' => '}',
                            else => s.value[i],
                        };
                        value.appendAssumeCapacity(escaped);
                    } else {
                        value.appendAssumeCapacity(s.value[i]);
                    }
                    i += 1;
                }

                const obj = try self.alloc.create(Value.Obj);
                obj.* = .{ .data = .{ .string = try value.toOwnedSlice(self.alloc) } };
                const index = try self.addConstant(.{ .obj = obj });
                try self.writeOp(.string, token);
                _ = try self.writeInt(C.CONSTANT, index, token);
                _ = try self.writeInt(u8, @as(u8, @intCast(s.expressions.len)), token);
            },
            .list => |l| {
                for (l) |*item| {
                    try self.compileExpression(item);
                }
                try self.writeOp(.list, token);
                const size = C.COLLECTION;
                const length = @as(size, @intCast(l.len));
                _ = try self.writeInt(size, length, token);
            },
            .map => |m| {
                for (m) |*mp| {
                    try self.compileExpression(mp);
                }
                const size = C.COLLECTION;
                try self.writeOp(.map, token);
                const length = @as(size, @intCast(m.len));
                _ = try self.writeInt(size, length, token);
            },
            .set => |s| {
                for (s) |*item| {
                    try self.compileExpression(item);
                }
                const size = C.COLLECTION;
                try self.writeOp(.set, token);
                const length = @as(size, @intCast(s.len));
                _ = try self.writeInt(size, length, token);
            },
            .map_pair => |mp| {
                try self.compileExpression(mp.key);
                try self.compileExpression(mp.value);
            },
            .indexer => |idx| {
                var parts: std.ArrayList([]const u8) = .empty;
                defer parts.deinit(self.alloc);
                if (try self.flattenIndexer(expr, &parts)) {
                    if (self.resolveAnchor(parts.items)) |i| {
                        const anchor = self.constants.items[i].obj.data.anchor;
                        try self.writeOp(.get_global, token);
                        _ = try self.writeInt(C.GLOBAL, anchor.visit_index, token);
                        return;
                    }
                }
                try self.compileExpression(idx.target);
                if (token.token_type == .dot) {
                    if (idx.target.type == .identifier) {
                        if (self.constants_map.get(idx.target.type.identifier)) |i| {
                            const val = self.constants.items[i];
                            if (val == .obj) {
                                switch (val.obj.data) {
                                    .anchor => {
                                        // If we're indexing into an anchor (e.g., Parent.Child)
                                        // we check if the path resolves to another anchor
                                        var nested_parts = std.ArrayList([]const u8).empty;
                                        defer nested_parts.deinit(self.alloc);
                                        if (try self.flattenIndexer(expr, &nested_parts)) {
                                            if (self.resolveAnchor(nested_parts.items)) |anchor_idx| {
                                                try self.writeOp(.pop, token); // pop the target anchor
                                                const anchor = self.constants.items[anchor_idx].obj.data.anchor;
                                                try self.writeOp(.get_global, token);
                                                _ = try self.writeInt(C.GLOBAL, anchor.visit_index, token);
                                                return;
                                            }
                                        }
                                    },
                                    .@"enum" => |e| {
                                        if (!arrayContains(u8, e.values, idx.index.type.identifier))
                                            return self.fail("Enum {s} does not contain a value '{s}'", idx.index.token, .{ idx.target.type.identifier, idx.index.type.identifier });
                                    },
                                    .class => |c| {
                                        if (c.getFieldIndex(idx.index.type.identifier) == null and c.getMethodIndex(idx.index.type.identifier) == null)
                                            return self.fail("Class {s} does not contain a field '{s}'", idx.index.token, .{ idx.target.type.identifier, idx.index.type.identifier });
                                    },
                                    else => {},
                                }
                            }
                        }
                    }
                    try self.addIdentifierConstant(idx.index.type.identifier, token);
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
                if (try self.resolveConstant(id)) |i| {
                    const val = self.constants.items[i];
                    if (val == .obj and val.obj.data == .anchor) {
                        try self.writeOp(.get_global, token);
                        _ = try self.writeInt(C.GLOBAL, val.obj.data.anchor.visit_index, token);
                        return;
                    }
                }
                try self.loadSymbol(id, token);
            },
            .@"if" => |i| {
                try self.compileExpression(i.condition);
                try self.writeOp(.jump_if_false, token);
                // temp garbage value
                const pos = try self.writeInt(C.JUMP, JUMP_HOLDER, token);
                try self.compileExpression(i.then_value);

                try self.writeOp(.jump, token);
                const nextPos = try self.writeInt(C.JUMP, JUMP_HOLDER, token);
                try self.replaceValue(pos, C.JUMP, self.instructionPos());

                try self.compileExpression(i.else_value);
                try self.replaceValue(nextPos, C.JUMP, self.instructionPos());
            },
            .instance => |ins| {
                const const_idx = self.constants_map.get(ins.name) orelse
                    return self.fail("Unknown class '{s}'", token, .{ins.name});

                const class_val = self.constants.items[const_idx];
                if (class_val != .obj or class_val.obj.data != .class)
                    return self.fail("'{s}' is not a class", token, .{ins.name});

                const class_def = class_val.obj.data.class;
                for (ins.fields, 0..) |field_expr, i| {
                    if (class_def.getFieldIndex(ins.field_names[i]) == null) {
                        return self.fail("Class {s} has no field '{s}'", token, .{ ins.name, ins.field_names[i] });
                    }

                    try self.compileExpression(&field_expr);
                    // Push the name of the field so the VM knows which one we are setting
                    try self.addIdentifierConstant(ins.field_names[i], token);
                }

                try self.writeOp(.constant, token);
                _ = try self.writeInt(C.CONSTANT, const_idx, token);

                try self.writeOp(.instance, token);
                _ = try self.writeInt(C.FIELDS, @as(C.FIELDS, @intCast(ins.fields.len)), token);
            },
            .call => |c| {
                try self.compileExpression(c.target);
                for (c.arguments) |*arg| {
                    try self.compileExpression(arg);
                }
                try self.writeOp(.call, token);
                const size = C.ARGS;
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

    fn setSymbol(self: *Compiler, name: []const u8, symbol: ?*Symbol, token: Token, is_decl: bool) !void {
        if (try self.resolveConstant(name) != null) {
            return self.fail("Cannot assign to constant '{s}'", token, .{name});
        }

        if (symbol) |s| {
            if (!is_decl and !s.is_mutable) return self.fail("Cannot assign to constant variable '{s}'", token, .{s.name});
            switch (s.tag) {
                .global => {
                    try self.writeOp(if (is_decl) .decl_global else .set_global, token);
                    _ = try self.writeInt(C.GLOBAL, @as(C.GLOBAL, @intCast(s.index)), token);
                },
                .upvalue => {
                    std.debug.assert(is_decl == false);
                    try self.writeOp(.set_upvalue, token);
                    const depth = self.calculateScopeDepth(s);
                    _ = try self.writeInt(u8, @intCast(depth), token);
                    _ = try self.writeInt(C.LOCAL, @as(C.LOCAL, @intCast(s.index)), token);
                },
                .local, .function => {
                    try self.writeOp(.set_local, token);
                    _ = try self.writeInt(C.LOCAL, @as(C.LOCAL, @intCast(s.index)), token);
                },
            }
        } else return self.failError("Unknown symbol {s}", token, .{name}, Error.SymbolNotFound);
    }

    fn loadSymbol(self: *Compiler, name: []const u8, token: Token) !void {
        const symbol = try self.scope.resolve(name);

        if (symbol) |s| {
            switch (s.tag) {
                .local, .function => {
                    try self.writeOp(.get_local, token);
                    _ = try self.writeInt(C.LOCAL, @intCast(s.index), token);
                    return;
                },
                .upvalue => {
                    try self.writeOp(.get_upvalue, token);
                    const depth = self.calculateScopeDepth(s);
                    _ = try self.writeInt(u8, @intCast(depth), token);
                    _ = try self.writeInt(C.LOCAL, @intCast(s.index), token);
                    return;
                },
                .global => {
                    try self.writeOp(.get_global, token);
                    _ = try self.writeInt(C.GLOBAL, @intCast(s.index), token);
                    return;
                },
            }
        }

        if (try self.resolveConstant(name)) |i| {
            try self.writeOp(.constant, token);
            _ = try self.writeInt(C.CONSTANT, i, token);
            return;
        }

        return self.failError("Unknown symbol '{s}'", token, .{name}, Error.SymbolNotFound);
    }

    fn calculateScopeDepth(self: *Compiler, symbol: *Symbol) usize {
        var depth: usize = 0;
        var current: ?*Scope = self.scope;

        while (current) |s| {
            if (s.tag == .function) depth += 1;
            if (s.symbols.get(symbol.name) != null) return depth;

            current = s.parent;
        }
        return depth;
    }

    fn resolveConstant(self: *Compiler, name: []const u8) !?C.CONSTANT {
        var i: usize = self.path_stack.items.len;
        while (i > 0) : (i -= 1) {
            const path = try std.mem.join(self.alloc, ".", self.path_stack.items[0..i]);
            defer self.alloc.free(path);
            const full_name = try std.fmt.allocPrint(self.alloc, "{s}.{s}", .{ path, name });
            defer self.alloc.free(full_name);
            if (self.constants_map.get(full_name)) |idx| return idx;
        }

        return self.constants_map.get(name);
    }

    fn instructionPos(self: *Compiler) C.JUMP {
        return @as(C.JUMP, @intCast(self.chunk.instructions.items.len));
    }

    fn writeOp(self: *Compiler, op: OpCode, token: Token) !void {
        var chunk = self.chunk;
        try chunk.debug_markers.append(self.alloc, .{ .file_index = @intCast(token.file_index), .line = @intCast(token.line) });
        try chunk.instructions.append(self.alloc, @intFromEnum(op));
    }

    fn writeValue(self: *Compiler, buf: []const u8, token: Token) !void {
        var chunk = self.chunk;
        try chunk.debug_markers.appendNTimes(self.alloc, .{ .file_index = @intCast(token.file_index), .line = @intCast(token.line) }, buf.len);
        try chunk.instructions.writer(self.alloc).writeAll(buf);
    }

    fn writeInt(self: *Compiler, comptime T: type, value: T, token: Token) !usize {
        const chunk = self.chunk;
        const start = chunk.instructions.items.len;
        var buf: [@sizeOf(T)]u8 = undefined;
        std.mem.writeInt(T, buf[0..], value, .little);
        try self.writeValue(&buf, token);
        return start;
    }

    pub fn replaceValue(self: *Compiler, pos: usize, comptime T: type, value: T) !void {
        var buf: [@sizeOf(T)]u8 = undefined;
        std.mem.writeInt(T, buf[0..], value, .little);
        var chunk = self.chunk;
        for (buf, 0..) |v, i| {
            chunk.instructions.items[pos + i] = v;
        }
    }

    pub fn replaceConstant(self: *Compiler, name: []const u8, value: Value, token: Token) !void {
        const i = self.constants_map.get(name) orelse return self.fail("Constant {s} not found", token, .{name});
        self.constants.items[i] = value;
    }

    pub fn replaceJumps(instructions: []u8, old_pos: C.JUMP, new_pos: C.JUMP) !void {
        var i: usize = 0;
        const jump = @intFromEnum(OpCode.jump);
        while (i < instructions.len) : (i += 1) {
            if (instructions[i] == jump and std.mem.readVarInt(C.JUMP, instructions[(i + 1)..(i + @sizeOf(C.JUMP))], .little) == old_pos) {
                var buf: [@sizeOf(C.JUMP)]u8 = undefined;
                std.mem.writeInt(C.JUMP, buf[0..], new_pos, .little);
                for (buf, 0..) |v, index| {
                    instructions[i + index + 1] = v;
                }
            }
        }
    }

    fn resolveAnchor(self: *Compiler, path_parts: [][]const u8) ?C.CONSTANT {
        const path = std.mem.join(self.alloc, ".", path_parts) catch return null;
        defer self.alloc.free(path);

        const idx = self.resolveConstant(path) catch return null;
        if (idx) |i| {
            const value = self.constants.items[i];
            if (value == .obj and value.obj.data == .anchor) return i;
        }

        return null;
    }

    fn flattenIndexer(self: *Compiler, expr: *const Expression, list: *std.ArrayList([]const u8)) !bool {
        var current = expr;
        while (current.type == .indexer) {
            const idx = current.type.indexer;
            if (idx.index.type != .identifier) return false;
            try list.append(self.alloc, idx.index.type.identifier);
            current = idx.target;
        }
        if (current.type != .identifier) return false;
        try list.append(self.alloc, current.type.identifier);
        std.mem.reverse([]const u8, list.items);
        return true;
    }

    pub fn addConstant(self: *Compiler, value: Value) !C.CONSTANT {
        try self.constants.append(self.alloc, value);
        return @intCast(self.constants.items.len - 1);
    }

    pub fn addNamedConstant(self: *Compiler, name: []const u8, value: Value) !void {
        const i = try self.addConstant(value);
        try self.constants_map.putNoClobber(self.alloc, try self.alloc.dupe(u8, name), i);
    }

    pub fn addLiteralConstant(self: *Compiler, value: Value) !C.CONSTANT {
        if (self.literal_cache.get(value)) |idx| return idx;

        const i = try self.addConstant(value);
        try self.literal_cache.put(self.alloc, value, i);
        return i;
    }

    fn addIdentifierConstant(self: *Compiler, name: []const u8, token: Token) !void {
        var i = self.constants_map.get(name);
        if (i == null) {
            i = try self.addConstant(.{ .const_string = name });
            try self.constants_map.putNoClobber(self.alloc, try self.alloc.dupe(u8, name), i.?);
        }

        try self.writeOp(.constant, token);
        _ = try self.writeInt(C.CONSTANT, i.?, token);
    }
};
