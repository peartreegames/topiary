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

const scope = @import("../ir/scope.zig");
const Scope = scope.Scope;
const Symbol = scope.Symbol;

const DebugInfo = @import("debug.zig").DebugInfo;
const error_mod = @import("error.zig");
const CompilerErrors = error_mod.CompilerErrors;

const Bytecode = @import("bytecode.zig").Bytecode;
const OpCode = @import("opcode.zig").OpCode;
const suggest = @import("suggest.zig");

const emit = @import("emit.zig");
const Emitter = emit.Emitter;
const Chunk = emit.Chunk;
const JumpPatch = emit.JumpPatch;
const BREAK_HOLDER = emit.BREAK_HOLDER;
const CONTINUE_HOLDER = emit.CONTINUE_HOLDER;
const SWITCH_END_HOLDER = emit.SWITCH_END_HOLDER;
const JUMP_HOLDER = emit.JUMP_HOLDER;

fn arrayContains(comptime T: type, haystack: []const []const T, needle: []const T) bool {
    for (haystack) |element| {
        if (std.mem.eql(T, element, needle)) return true;
    }
    return false;
}


pub const Compiler = struct {
    // `alloc` owns data that outlives the compiler: the constants array, every
    // Value.Obj pointer stored in it, and the chunk instruction buffers that
    // become bytecode. The compiler must NOT use `alloc` for anything else.
    alloc: std.mem.Allocator,
    // `arena` owns transient bookkeeping: scopes, the hash maps below, duped
    // keys, path_stack backing. All bulk-freed by `arena.deinit()`.
    arena: std.heap.ArenaAllocator,
    /// Bytecode emission state — chunk, constant pool, literal cache.
    /// Shared shape with the IR-driven `codegen.zig`.
    emitter: Emitter,
    scope: *Scope,
    root_scope: *Scope,
    locals_count: usize = 0,

    module: *Module,
    current_file: *File,
    emitted_files: std.array_hash_map.String(void) = .empty,
    path_stack: std.ArrayList([]const u8) = .empty,

    // Diagnostics tracking: keys are borrowed from `constants_map` keys
    // on the emitter.
    decl_tokens: std.StringHashMapUnmanaged(Token) = .empty,
    fn_arities: std.StringHashMapUnmanaged(u8) = .empty,

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
        var arena = std.heap.ArenaAllocator.init(alloc);
        errdefer arena.deinit();
        const root_scope = try Scope.create(arena.allocator(), null, .global);
        // Root chunk + constants live on `alloc` so their backing
        // outlives the compiler when `bytecode()` extracts via
        // `toOwnedSlice`.
        var emitter = try Emitter.init(alloc, module);
        errdefer emitter.deinit();
        return .{
            .alloc = alloc,
            .arena = arena,
            .emitter = emitter,
            .scope = root_scope,
            .root_scope = root_scope,
            .module = module,
            .current_file = module.entry,
        };
    }

    pub fn deinit(self: *Compiler) void {
        // Emitter owns chunks and the constants list; their buffers are
        // emptied by `bytecode()` on success and freed here on error
        // paths.
        self.emitter.deinit();
        // Everything else — scopes, all bookkeeping hash maps, duped keys,
        // path_stack backing — is on the arena and freed in one shot.
        self.arena.deinit();
    }

    const FailOpts = struct {
        end: ?Token = null,
        suggestion: ?[]const u8 = null,
        note: ?[]const u8 = null,
        err: Error = Error.CompilerError,
    };

    const WarnOpts = struct {
        end: ?Token = null,
        suggestion: ?[]const u8 = null,
        note: ?[]const u8 = null,
    };

    fn fail(
        self: *Compiler,
        comptime msg: []const u8,
        token: Token,
        args: anytype,
        opts: FailOpts,
    ) Error {
        try self.addDiagnostic(msg, token, args, .err, opts.end, opts.suggestion, opts.note);
        return opts.err;
    }

    fn warn(
        self: *Compiler,
        comptime msg: []const u8,
        token: Token,
        args: anytype,
        opts: WarnOpts,
    ) !void {
        try self.addDiagnostic(msg, token, args, .warn, opts.end, opts.suggestion, opts.note);
    }

    fn addDiagnostic(
        self: *Compiler,
        comptime msg: []const u8,
        token: Token,
        args: anytype,
        severity: error_mod.CompilerErr.Severity,
        end: ?Token,
        suggestion: ?[]const u8,
        note: ?[]const u8,
    ) !void {
        const has_span = end != null;
        const has_help = suggestion != null or note != null;
        const path = self.current_file.path;
        const errs = &self.module.errors;
        if (has_span and has_help) {
            try errs.addSpanWithHelp(path, msg, token, end, severity, args, suggestion, note);
        } else if (has_span) {
            try errs.addSpan(path, msg, token, end, severity, args);
        } else if (has_help) {
            try errs.addWithHelp(path, msg, token, severity, args, suggestion, note);
        } else {
            try errs.add(path, msg, token, severity, args);
        }
    }

    /// Build a "previous declaration at file:line" note string. Caller does
    /// NOT own the returned slice; it is freed by `CompilerErrors.deinit`.
    fn previousDeclNote(self: *Compiler, name: []const u8) !?[]const u8 {
        const prev = self.decl_tokens.get(name) orelse return null;
        const file_name = if (prev.file_index < self.module.includes.count())
            std.fs.path.basename(self.module.includes.keys()[prev.file_index])
        else
            "?";
        return try std.fmt.allocPrint(self.alloc, "previous declaration at {s}:{d}", .{ file_name, prev.line });
    }

    /// Build a "did you mean" suggestion by searching through currently-
    /// visible symbol-table entries and top-level constants.
    fn suggestForSymbol(self: *Compiler, name: []const u8) !?[]const u8 {
        var names: std.ArrayList([]const u8) = .empty;
        defer names.deinit(self.alloc);

        // Collect all symbol names reachable from the current scope.
        var current: ?*Scope = self.scope;
        while (current) |s| : (current = s.parent) {
            for (s.symbols.keys()) |k| try names.append(self.alloc, k);
        }
        // Plus all top-level (unqualified) constants -- classes, enums,
        // functions, boughs with no path prefix.
        var it = self.emitter.constants_map.keyIterator();
        while (it.next()) |k| {
            if (std.mem.indexOfScalar(u8, k.*, '.') == null)
                try names.append(self.alloc, k.*);
        }

        return try self.suggestFromList(name, names.items);
    }

    fn suggestFromConstants(self: *Compiler, name: []const u8) !?[]const u8 {
        // Collect top-level names (those without a '.' in them) as the likely
        // candidates for a typo. This avoids suggesting nested anchor paths.
        var names: std.ArrayList([]const u8) = .empty;
        defer names.deinit(self.alloc);
        var it = self.emitter.constants_map.keyIterator();
        while (it.next()) |k| {
            try names.append(self.alloc, k.*);
        }
        return try self.suggestFromList(name, names.items);
    }

    /// Look up declared function arity by short name, searching through the
    /// current path scope chain. Returns null if the name doesn't resolve to
    /// a known user function (extern funcs / builtins / methods are skipped).
    fn resolveFnArity(self: *Compiler, name: []const u8) ?u8 {
        // Try qualified lookup walking up path_stack, then unqualified.
        var i: usize = self.path_stack.items.len;
        while (i > 0) : (i -= 1) {
            const path = std.mem.join(self.alloc, ".", self.path_stack.items[0..i]) catch return null;
            defer self.alloc.free(path);
            const full = std.fmt.allocPrint(self.alloc, "{s}.{s}", .{ path, name }) catch return null;
            defer self.alloc.free(full);
            if (self.fn_arities.get(full)) |a| return a;
        }
        return self.fn_arities.get(name);
    }

    fn failRedeclared(self: *Compiler, name: []const u8, comptime kind: []const u8, token: Token, end_token: ?Token) Error {
        // Look up the previous declaration's full name in the declarations map.
        // The caller passes the short name; we try the qualified form first.
        var lookup_name: []const u8 = name;
        const qualified = self.getQualifiedName(name) catch null;
        defer if (qualified) |q| self.alloc.free(q);
        if (qualified) |q| {
            if (self.decl_tokens.contains(q)) lookup_name = q;
        }
        const note = try self.previousDeclNote(lookup_name);
        return self.fail(kind ++ " '{s}' is already declared", token, .{name}, .{ .end = end_token, .note = note });
    }

    fn suggestFromList(self: *Compiler, name: []const u8, haystack: []const []const u8) !?[]const u8 {
        const match = (try suggest.closest(self.alloc, name, haystack)) orelse return null;
        defer self.alloc.free(match);
        return try std.fmt.allocPrint(self.alloc, "did you mean '{s}'?", .{match});
    }

    fn suggestClassField(self: *Compiler, class_def: types.Class, name: []const u8) !?[]const u8 {
        var names: std.ArrayList([]const u8) = .empty;
        defer names.deinit(self.alloc);
        for (class_def.fields) |f| try names.append(self.alloc, f.name);
        for (class_def.methods) |m| try names.append(self.alloc, m.name);
        return try self.suggestFromList(name, names.items);
    }

    fn inferExprType(expr: *const Expression) scope.VarType {
        return switch (expr.type) {
            .instance => |i| .{ .instance = i.name },
            .string => .string,
            .list => .list,
            .set => .set,
            .map => .map,
            .number => .number,
            .boolean => .boolean,
            .nil => .nil,
            else => .unknown,
        };
    }

    fn validateDotAccess(self: *Compiler, target: *const Expression, index: *const Expression, op_token: Token, target_token: Token, index_token: Token) !void {
        if (op_token.token_type != .dot) return;
        if (target.type != .identifier) return;
        if (index.type != .identifier) return;
        if (self.emitter.constants_map.get(target.type.identifier) != null) return;

        const sym = (try self.scope.resolve(self.arena.allocator(), target.type.identifier)) orelse return;
        const field = index.type.identifier;

        switch (sym.var_type) {
            .instance => |class_name| {
                const const_idx = self.emitter.constants_map.get(class_name) orelse return;
                const val = self.emitter.constants.items[const_idx];
                if (val != .obj or val.obj.data != .class) return;
                const class_def = val.obj.data.class;
                if (class_def.getFieldIndex(field) == null and class_def.getMethodIndex(field) == null) {
                    const sr = try self.suggestClassField(class_def, field);
                    const hint = sr;

                    return self.fail(
                        "Class '{s}' does not contain a field '{s}'",
                        target_token,
                        .{ class_def.name, field },
                        .{ .end = index_token, .suggestion = hint },
                    );
                }
            },
            .string => {
                if (!builtins.string_methods.has(field)) {
                    const sr = try self.suggestFromList(field, builtins.string_methods.keys());
                    const hint = sr;

                    return self.fail(
                        "Unknown method '{s}' on string",
                        target_token,
                        .{field},
                        .{ .end = index_token, .suggestion = hint },
                    );
                }
            },
            .list, .set, .map => {
                if (!builtins.collection_methods.has(field)) {
                    const type_name: []const u8 = switch (sym.var_type) {
                        .list => "list",
                        .set => "set",
                        .map => "map",
                        else => unreachable,
                    };
                    const sr = try self.suggestFromList(field, builtins.collection_methods.keys());
                    const hint = sr;

                    return self.fail(
                        "Unknown method '{s}' on {s}",
                        target_token,
                        .{ field, type_name },
                        .{ .end = index_token, .suggestion = hint },
                    );
                }
            },
            .number, .boolean, .nil => {
                const type_name: []const u8 = switch (sym.var_type) {
                    .number => "number",
                    .boolean => "boolean",
                    .nil => "nil",
                    else => unreachable,
                };
                return self.fail(
                    "Cannot access field '{s}' on a {s}",
                    target_token,
                    .{ field, type_name },
                    .{ .end = index_token },
                );
            },
            // Enum / class / function names take the constants_map
            // dispatch above (compiler.zig:1816+) or the call-site
            // path; their var_type tags exist for the IR validator
            // and aren't reached here in practice.
            .enum_type, .class_type, .function_type => {},
            .unknown => {},
        }
    }

    /// Writer-friendly name for an expression's syntactic kind. Used in
    /// error messages instead of leaking `@tagName` (which exposes
    /// internal AST tags like "binary" or "call").
    fn expressionKindName(expr: *const Expression) []const u8 {
        return switch (expr.type) {
            .number => "number literal",
            .string => "string literal",
            .boolean => "boolean literal",
            .nil => "nil",
            .list => "list literal",
            .set => "set literal",
            .map => "map literal",
            .map_pair => "map entry",
            .range => "range",
            .identifier => "variable",
            .indexer => "indexer",
            .call => "function call",
            .unary => "unary expression",
            .binary => "binary expression",
            .instance => "instance",
            .@"if" => "ternary expression",
            .@"extern" => "extern",
        };
    }

    /// Writer-friendly name for a Value.Obj's contained type. Used in
    /// error messages instead of leaking `@tagName` / `{t}` debug format
    /// on `Value.Obj.Data`.
    fn objKindName(data: Value.Obj.Data) []const u8 {
        return switch (data) {
            .string => "string",
            .list => "list",
            .set => "set",
            .map => "map",
            .function => "function",
            .class => "class",
            .instance => "instance",
            .@"enum" => "enum",
            .anchor => "anchor",
            .@"extern" => "extern function",
            .builtin => "builtin",
        };
    }

    fn failAssignTarget(self: *Compiler, left: *const Expression) Error {
        return self.fail(
            "Cannot assign to a {s}",
            left.token,
            .{expressionKindName(left)},
            .{ .note = try self.alloc.dupe(u8, "assignment targets must be a variable or an indexer (e.g. `list[0]` or `obj.field`)") },
        );
    }

    fn suggestAnchor(self: *Compiler, path: []const u8) !?[]const u8 {
        var names: std.ArrayList([]const u8) = .empty;
        defer names.deinit(self.alloc);
        var it = self.emitter.constants_map.iterator();
        while (it.next()) |entry| {
            const val = self.emitter.constants.items[entry.value_ptr.*];
            if (val == .obj and val.obj.data == .anchor) {
                try names.append(self.alloc, entry.key_ptr.*);
            }
        }
        return try self.suggestFromList(path, names.items);
    }

    pub fn bytecode(self: *Compiler) !Bytecode {
        if (self.scope.parent != null) return Error.OutOfScope;
        const global_symbols = try self.alloc.alloc(Bytecode.GlobalSymbol, self.scope.symbols.count());
        var filled: usize = 0;
        errdefer {
            for (global_symbols[0..filled]) |gs| self.alloc.free(gs.name);
            self.alloc.free(global_symbols);
        }
        for (self.scope.symbols.values(), 0..) |s, i| {
            global_symbols[i] = Bytecode.GlobalSymbol{
                .name = try self.alloc.dupe(u8, s.name),
                .uuid = s.uuid,
                .index = s.index,
                .is_mutable = s.is_mutable,
            };
            filled = i + 1;
        }
        return .{
            .instructions = try self.emitter.chunk.instructions.toOwnedSlice(self.alloc),
            .debug_info = try self.emitter.chunk.debugInfo(self.alloc),
            .constants = try self.emitter.constants.toOwnedSlice(self.alloc),
            .global_symbols = global_symbols,
            .locals_count = self.locals_count,
        };
    }

    pub fn compile(self: *Compiler) Error!void {
        const tree = self.module.entry.tree orelse return Error.IllegalOperation;
        for (builtins.functions.keys()) |name| {
            const obj = try self.alloc.create(Value.Obj);
            errdefer self.alloc.destroy(obj);
            obj.* = .{ .data = .{ .builtin = builtins.functions.get(name).? } };
            try self.addNamedConstant(name, .{ .obj = obj });
        }
        for (tree.root) |*stmt| {
            try self.prepass(stmt);
        }
        self.emitted_files.clearRetainingCapacity();

        // Two emission passes so non-bough top-level code (var/class/enum/
        // function init, includes) is always emitted before any bough. A
        // bough's skip-over-body jump is the first `.jump` the VM sees, and
        // `Vm.start` uses that to land a `--bough` divert — anything emitted
        // after it would be skipped on targeted runs.
        for (tree.root) |*stmt| {
            try self.emitNonBough(stmt);
        }
        self.emitted_files.clearRetainingCapacity();

        for (tree.root) |*stmt| {
            try self.emitBough(stmt);
        }

        // Add one final end at the end of file to grab the initial jump_request
        const chunk = self.emitter.chunk;
        if (chunk.debug_markers.items.len > 0) {
            const dupe = chunk.debug_markers.items[chunk.debug_markers.items.len - 1];
            try chunk.debug_markers.ensureTotalCapacity(self.alloc, chunk.debug_markers.items.len + 1);
            chunk.debug_markers.appendAssumeCapacity(dupe);
            try chunk.instructions.append(self.alloc, @intFromEnum(OpCode.end));
        }
    }

    fn enterScope(self: *Compiler, tag: Scope.Tag) !void {
        self.scope = try Scope.create(self.arena.allocator(), self.scope, tag);
    }

    fn exitScope(self: *Compiler) !void {
        const old_scope = self.scope;
        self.scope = old_scope.parent orelse return Error.OutOfScope;
        if (old_scope.tag == .local) {
            // Local scopes inherit the parent's count, so old_scope.count
            // already includes the parent range — no need to add both.
            self.locals_count = @max(self.locals_count, old_scope.count);
        } else {
            self.locals_count = @max(self.locals_count, self.scope.count + old_scope.count);
        }
        old_scope.destroy(self.arena.allocator());
    }

    fn resolveForkName(self: *Compiler, name: ?[]const u8, id: UUID.ID) Error![]const u8 {
        if (name) |n| return self.alloc.dupe(u8, n);
        if (self.path_stack.items.len == 0) return Error.CompilerError;
        return self.alloc.dupe(u8, &id);
    }

    fn pushPathScope(self: *Compiler, name: []const u8) Error!void {
        try self.path_stack.append(self.arena.allocator(), name);
    }

    fn popPathScope(self: *Compiler) void {
        _ = self.path_stack.pop();
    }

    const IncludeResult = struct {
        file: *File,
        tree: []const Statement,
    };

    fn resolveInclude(self: *Compiler, raw_path: []const u8, token: Token, end_token: ?Token) Error!?IncludeResult {
        // Resolve original path to module-root-relative form
        const resolved = self.current_file.module.resolveIncludePath(self.current_file, raw_path) catch
            return self.fail("Could not resolve include path '{s}'", token, .{raw_path}, .{ .end = end_token });
        const file = self.module.includes.get(resolved) orelse
            return self.fail("Unknown include file {s}", token, .{raw_path}, .{ .end = end_token });
        if (self.emitted_files.contains(resolved)) return null;
        try self.emitted_files.put(self.arena.allocator(), resolved, {});
        const tree = file.tree orelse return Error.NotInitialized;
        return .{ .file = file, .tree = tree.root };
    }

    fn emitNonBough(self: *Compiler, stmt: *const Statement) Error!void {
        switch (stmt.type) {
            .bough => {},
            .include => |i| {
                const result = try self.resolveInclude(i.path, stmt.token, i.path_token) orelse return;
                const tmp = self.current_file;
                defer self.current_file = tmp;
                self.current_file = result.file;
                for (result.tree) |*s| try self.emitNonBough(s);
            },
            else => try self.compileStatement(stmt),
        }
    }

    fn emitBough(self: *Compiler, stmt: *const Statement) Error!void {
        switch (stmt.type) {
            .include => |i| {
                const result = try self.resolveInclude(i.path, stmt.token, i.path_token) orelse return;
                const tmp = self.current_file;
                defer self.current_file = tmp;
                self.current_file = result.file;
                for (result.tree) |*s| try self.emitBough(s);
            },
            .bough => try self.compileStatement(stmt),
            else => {},
        }
    }

    fn prepass(self: *Compiler, stmt: *const Statement) Error!void {
        switch (stmt.type) {
            .include => |i| {
                const result = try self.resolveInclude(i.path, stmt.token, i.path_token) orelse return;
                const tmp = self.current_file;
                defer self.current_file = tmp;
                self.current_file = result.file;
                for (result.tree) |*s| try self.prepass(s);
            },
            .function => |f| {
                const full_name = try self.getQualifiedName(f.name);
                defer self.alloc.free(full_name);
                self.addNamedConstantTok(full_name, .nil, stmt.token) catch |err| switch (err) {
                    error.SymbolAlreadyDeclared => return self.failRedeclared(full_name, "Function", stmt.token, f.name_token),
                    else => return err,
                };
                // Track arity so call sites can validate argument counts.
                if (self.emitter.constants_map.getKey(full_name)) |persistent_key| {
                    try self.fn_arities.put(self.arena.allocator(), persistent_key, @intCast(f.parameters.len));
                }
                if (f.is_extern) {
                    _ = try self.emitter.addConstant(.{ .obj = try self.compileExternFunctionObj(stmt) });
                }
            },
            .class => |c| {
                const full_name = try self.getQualifiedName(c.name);
                defer self.alloc.free(full_name);
                self.addNamedConstantTok(full_name, .nil, stmt.token) catch |err| switch (err) {
                    error.SymbolAlreadyDeclared => return self.failRedeclared(full_name, "Class", stmt.token, c.name_token),
                    else => return err,
                };
            },
            .@"enum" => |e| {
                const full_name = try self.getQualifiedName(e.name);
                defer self.alloc.free(full_name);
                self.addNamedConstantTok(full_name, .nil, stmt.token) catch |err| switch (err) {
                    error.SymbolAlreadyDeclared => return self.failRedeclared(full_name, "Enum", stmt.token, e.name_token),
                    else => return err,
                };
            },
            .bough => |b| {
                const full_name = try self.getQualifiedName(b.name);
                // On success, registerAnchor hands `full_name` to the new
                // anchor object (freed via compiler.deinit → Obj.deinit).
                // On error it stays with us, so we free it here.
                var transferred = false;
                errdefer if (!transferred) self.alloc.free(full_name);
                self.registerAnchor(full_name, b.id, stmt.token) catch |err| switch (err) {
                    error.SymbolAlreadyDeclared => return self.failRedeclared(b.name, "Bough", stmt.token, b.name_token),
                    else => return err,
                };
                transferred = true;
                try self.pushPathScope(b.name);
                defer self.popPathScope();
                for (b.body) |*s| try self.prepass(s);
            },
            .fork => |f| {
                const fork_name = self.resolveForkName(f.name, f.id) catch
                    return self.fail("fork must be inside a bough", stmt.token, .{}, .{ .end = f.end_token });
                defer self.alloc.free(fork_name);

                const full_name = try self.getQualifiedName(fork_name);
                var transferred = false;
                errdefer if (!transferred) self.alloc.free(full_name);
                self.registerAnchor(full_name, f.id, stmt.token) catch |err| switch (err) {
                    error.SymbolAlreadyDeclared => return self.failRedeclared(fork_name, "Fork", stmt.token, f.end_token),
                    else => return err,
                };
                transferred = true;
                try self.pushPathScope(fork_name);
                defer self.popPathScope();

                for (f.body) |*s| try self.prepass(s);
            },
            .choice => |c| {
                const name = c.name orelse &c.id;
                const full_name = try self.getQualifiedName(name);
                var transferred = false;
                errdefer if (!transferred) self.alloc.free(full_name);
                self.registerAnchor(full_name, c.id, stmt.token) catch |err| switch (err) {
                    error.SymbolAlreadyDeclared => return self.failRedeclared(name, "Choice", stmt.token, null),
                    else => return err,
                };
                transferred = true;
                try self.path_stack.append(self.arena.allocator(), name);
                defer _ = self.path_stack.pop();
                for (c.body) |*s| try self.prepass(s);
            },
            .@"if" => |i| {
                for (i.then_branch) |*s| try self.prepass(s);
                if (i.else_branch) |e| {
                    for (e) |*s| try self.prepass(s);
                }
            },
            .@"while" => |w| {
                for (w.body) |*s| try self.prepass(s);
            },
            .@"for" => |f| {
                for (f.body) |*s| try self.prepass(s);
            },
            else => {},
        }
    }

    pub fn compileStatement(self: *Compiler, stmt: *const Statement) Error!void {
        const token = stmt.token;
        switch (stmt.type) {
            .include => |i| {
                const result = try self.resolveInclude(i.path, stmt.token, i.path_token) orelse return;
                const tmp_file = self.current_file;
                defer self.current_file = tmp_file;
                self.current_file = result.file;
                for (result.tree) |*s| try self.compileStatement(s);
            },
            .@"if" => |i| {
                try self.compileExpression(i.condition);
                const falseJp = try self.emitter.emitJump(.jump_if_false, token);
                try self.compileBlock(i.then_branch);

                const endJp = try self.emitter.emitJump(.jump, token);
                try self.emitter.patchJump(falseJp);

                if (i.else_branch == null) {
                    try self.emitter.writeOp(.nil, token);
                    try self.emitter.writeOp(.pop, token);
                    try self.emitter.patchJump(endJp);
                    return;
                }
                try self.compileBlock(i.else_branch.?);
                try self.emitter.patchJump(endJp);
            },
            .function => |f| {
                if (self.scope.parent != null and f.is_extern)
                    return self.fail("Only global functions can be extern.", token, .{}, .{ .err = Error.IllegalOperation });
                const obj = try self.compileFunctionObj(stmt, token);
                const full_name = try self.getQualifiedName(f.name);
                defer self.alloc.free(full_name);
                try self.replaceConstant(full_name, .{ .obj = obj }, token);
            },
            .@"switch" => |s| {
                const start = self.emitter.instructionPos();
                try self.compileExpression(&s.capture);
                var prong_jumps = try self.alloc.alloc(JumpPatch, s.prongs.len);
                var inferred_else_jump: ?JumpPatch = null;
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
                    prong_jumps[i] = try self.emitter.emitJump(.prong, prong_stmt.token);
                    _ = try self.emitter.writeInt(u8, @as(u8, @intCast(if (prong.values) |p| p.len else 0)), prong_stmt.token);
                }
                // add an empty else if none found
                if (!has_else) {
                    const prong_jp = try self.emitter.emitJump(.prong, token);
                    _ = try self.emitter.writeInt(u8, 0, token);
                    inferred_else_jump = prong_jp;
                }

                // replace jumps and compile body
                for (s.prongs, 0..) |prong_stmt, i| {
                    const prong = prong_stmt.type.switch_prong;
                    try self.emitter.patchJump(prong_jumps[i]);
                    try self.enterScope(.local);
                    try self.compileBlock(prong.body);
                    try self.emitter.writeOp(.jump, prong_stmt.token);
                    _ = try self.emitter.writeInt(C.JUMP, SWITCH_END_HOLDER, prong_stmt.token);
                    try self.exitScope();
                }

                if (inferred_else_jump) |jp| {
                    try self.emitter.patchJump(jp);
                    try self.enterScope(.local);
                    try self.compileBlock(&[_]Statement{});
                    try self.emitter.writeOp(.jump, token);
                    _ = try self.emitter.writeInt(C.JUMP, SWITCH_END_HOLDER, token);
                    try self.exitScope();
                }

                try emit.replaceJumps(self.emitter.chunk.instructions.items[start..], SWITCH_END_HOLDER, self.emitter.instructionPos());
                try self.emitter.writeOp(.pop, token);
            },
            .block => |b| try self.compileBlock(b),
            .expression => |exp| {
                try self.compileExpression(&exp);
                try self.emitter.writeOp(.pop, token);
            },
            .@"break" => {
                try self.emitter.writeOp(.jump, token);
                _ = try self.emitter.writeInt(C.JUMP, BREAK_HOLDER, token);
            },
            .@"continue" => {
                try self.emitter.writeOp(.jump, token);
                _ = try self.emitter.writeInt(C.JUMP, CONTINUE_HOLDER, token);
            },
            .@"while" => |w| {
                try self.enterScope(.local);

                const start = self.emitter.instructionPos();
                try self.compileExpression(&w.condition);
                const exitJp = try self.emitter.emitJump(.jump_if_false, token);

                try self.compileBlock(w.body);
                try self.emitter.writeOp(.jump, token);
                _ = try self.emitter.writeInt(C.JUMP, start, token);

                const end = self.emitter.instructionPos();
                try self.emitter.patchJumpTo(exitJp, end);

                try emit.replaceJumps(self.emitter.chunk.instructions.items[start..], BREAK_HOLDER, end);
                try emit.replaceJumps(self.emitter.chunk.instructions.items[start..], CONTINUE_HOLDER, start);
                try self.exitScope();
            },
            .@"for" => |f| {
                try self.compileExpression(&f.iterator);
                try self.emitter.writeOp(.iter_start, token);
                const start = self.emitter.instructionPos();

                try self.emitter.writeOp(.iter_next, token);
                const exitJp = try self.emitter.emitJump(.jump_if_false, token);

                try self.enterScope(.local);
                const capture = try self.scope.define(self.arena.allocator(), f.capture, false);
                try self.emitter.writeOp(.set_local, token);
                _ = try self.emitter.writeInt(C.LOCAL, @as(C.LOCAL, @intCast(capture.index)), token);

                try self.compileBlock(f.body);
                try self.emitter.writeOp(.jump, token);
                _ = try self.emitter.writeInt(C.JUMP, start, token);

                const end = self.emitter.instructionPos();
                try self.emitter.patchJumpTo(exitJp, end);
                try emit.replaceJumps(self.emitter.chunk.instructions.items[start..], BREAK_HOLDER, end);
                try emit.replaceJumps(self.emitter.chunk.instructions.items[start..], CONTINUE_HOLDER, start);

                try self.exitScope();
                try self.emitter.writeOp(.pop, token);

                try self.emitter.writeOp(.iter_end, token);
                // pop item
                try self.emitter.writeOp(.pop, token);
            },
            .variable => |v| {
                if (builtins.functions.has(v.name))
                    return self.fail("'{s}' is a builtin function and cannot be used as a variable name", stmt.token, .{v.name}, .{ .end = v.name_token, .err = Error.IllegalOperation });
                // Warn when the new name hides a variable from an enclosing
                // scope. Only warn for local-to-local shadowing so global
                // variables intentionally re-used across scopes remain quiet.
                if (self.scope.tag != .global) {
                    if (self.scope.resolveOuter(v.name)) |_| {
                        try self.warn(
                            "'{s}' hides a variable from an outer scope",
                            token,
                            .{v.name},
                            .{ .end = v.name_token },
                        );
                    }
                }
                const symbol = self.scope.define(self.arena.allocator(), v.name, v.is_mutable) catch {
                    return self.fail("'{s}' is already declared in this scope", token, .{v.name}, .{ .end = v.name_token });
                };
                try self.compileExpression(&v.initializer);
                try self.setSymbol(v.name, symbol, token, true);
                symbol.var_type = inferExprType(&v.initializer);
            },
            .class => |c| {
                // `transferred` gates every errdefer below: once the class
                // value is safely handed to the constants pool, ownership
                // moves with it and the pool's teardown (Class.deinit ->
                // destroyStatic) is solely responsible for cleanup.
                var transferred = false;

                const class_name = try self.alloc.dupe(u8, c.name);
                errdefer if (!transferred) self.alloc.free(class_name);

                var fields = try self.alloc.alloc(types.Class.Member, c.fields.len);
                var fields_filled: usize = 0;
                errdefer if (!transferred) {
                    for (fields[0..fields_filled]) |m| {
                        self.alloc.free(m.name);
                        m.value.destroyStatic(self.alloc);
                    }
                    self.alloc.free(fields);
                };

                for (c.fields, 0..) |field_expr, i| {
                    const value = try self.evaluateLiteral(&field_expr);
                    errdefer value.destroyStatic(self.alloc);
                    const name = try self.alloc.dupe(u8, c.field_names[i]);
                    fields[i] = .{ .name = name, .value = value };
                    fields_filled = i + 1;
                }

                var methods = try self.alloc.alloc(types.Class.Member, c.methods.len);
                var methods_filled: usize = 0;
                errdefer if (!transferred) {
                    for (methods[0..methods_filled]) |m| {
                        self.alloc.free(m.name);
                        m.value.destroy(self.alloc);
                    }
                    self.alloc.free(methods);
                };

                for (c.methods, 0..) |*method_stmt, i| {
                    const func = method_stmt.type.function;
                    const func_obj = try self.compileFunctionObj(method_stmt, method_stmt.token);
                    errdefer {
                        const v: Value = .{ .obj = func_obj };
                        v.destroy(self.alloc);
                    }
                    const name = try self.alloc.dupe(u8, func.name);
                    methods[i] = .{ .name = name, .value = .{ .obj = func_obj } };
                    methods_filled = i + 1;
                }

                const class_data = try types.Class.init(class_name, fields, methods);
                const obj = try self.alloc.create(Value.Obj);
                errdefer if (!transferred) self.alloc.destroy(obj);
                obj.* = .{
                    .id = UUID.new(),
                    .data = .{ .class = class_data },
                };

                const full_name = try self.getQualifiedName(c.name);
                defer self.alloc.free(full_name);
                try self.replaceConstant(full_name, .{ .obj = obj }, token);
                transferred = true;
            },
            .@"enum" => |e| {
                var values = try self.alloc.alloc([]const u8, e.values.len);
                var values_filled: usize = 0;
                errdefer {
                    for (values[0..values_filled]) |v| self.alloc.free(v);
                    self.alloc.free(values);
                }

                const obj = try self.alloc.create(Value.Obj);
                errdefer self.alloc.destroy(obj);

                for (e.values, 0..) |value, i| {
                    values[i] = try self.alloc.dupe(u8, value);
                    values_filled = i + 1;
                }

                const enum_name = try self.alloc.dupe(u8, e.name);
                errdefer self.alloc.free(enum_name);
                obj.* = .{
                    .id = UUID.fromStringHash(e.name),
                    .data = .{
                        .@"enum" = .{
                            .is_seq = e.is_seq,
                            .name = enum_name,
                            .values = values,
                        },
                    },
                };

                const full_name = try self.getQualifiedName(e.name);
                defer self.alloc.free(full_name);
                try self.replaceConstant(full_name, .{ .obj = obj }, token);
            },
            .fork => |f| {
                const fork_name = self.resolveForkName(f.name, f.id) catch
                    return self.fail("fork must be inside a bough", stmt.token, .{}, .{ .end = f.end_token });
                defer self.alloc.free(fork_name);

                const path = try self.getQualifiedName(fork_name);
                defer self.alloc.free(path);

                try self.pushPathScope(fork_name);
                defer self.popPathScope();

                const start_pos = self.emitter.instructionPos();
                const anchor_idx = try self.resolveConstant(path);
                if (anchor_idx) |idx| {
                    self.emitter.constants.items[idx].obj.data.anchor.ip = start_pos;
                    try self.compileVisit(idx, token);
                } else {
                    const sr = try self.suggestAnchor(path);
                    const hint = sr;

                    return self.fail("Could not find anchor '{s}'", token, .{path}, .{ .end = f.end_token, .suggestion = hint });
                }

                try self.enterScope(.local);
                try self.compileBlock(f.body);
                try self.exitScope();

                // Warn if a non-backup fork has choices whose bodies don't
                // exit (divert/fin/return). Without fork^, execution ends
                // silently after such a choice — almost never what writers want.
                if (!f.is_backup) {
                    for (f.body) |body_stmt| {
                        if (body_stmt.type == .choice) {
                            const choice = body_stmt.type.choice;
                            if (!blockExits(choice.body)) {
                                try self.warn(
                                    "choice has no divert or 'fin' -- execution will end silently after this choice",
                                    body_stmt.token,
                                    .{},
                                    .{ .suggestion = try self.alloc.dupe(u8, "use 'fork^' to continue after the choice, or add a divert '=>' inside the choice body") },
                                );
                            }
                        }
                    }
                }

                var backup_jp: ?JumpPatch = null;
                if (f.is_backup) {
                    backup_jp = try self.emitter.emitJump(.backup, token);
                    _ = try self.emitter.writeInt(u8, 1, token); // 1 = fork backup
                }
                try self.emitter.writeOp(.fork, token);
                const end_pos = self.emitter.instructionPos();
                if (backup_jp) |jp| try self.emitter.patchJumpTo(jp, end_pos);
            },
            .choice => |c| {
                for (c.tags) |tag| {
                    try self.emitter.addIdentifierConstant(self.arena.allocator(),tag.name, token);
                }
                const name = c.name orelse &c.id;
                const full_name = try self.getQualifiedName(name);
                defer self.alloc.free(full_name);

                try self.path_stack.append(self.arena.allocator(), name);
                defer _ = self.path_stack.pop();

                const entry_ip = self.emitter.instructionPos();
                const anchor_idx = try self.resolveConstant(full_name) orelse {
                    const sr = try self.suggestAnchor(full_name);
                    const hint = sr;

                    return self.fail("Could not find anchor '{s}'", token, .{full_name}, .{ .suggestion = hint });
                };
                self.emitter.constants.items[anchor_idx].obj.data.anchor.ip = entry_ip;

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

                const index = try self.emitter.addConstant(.{ .obj = obj });

                try self.emitter.writeOp(.loc, token);
                _ = try self.emitter.writeInt(C.CONSTANT, index, token);
                _ = try self.emitter.writeInt(u8, @as(u8, @intCast(s.expressions.len)), token);

                const choiceJp = try self.emitter.emitJump(.choice, token);
                _ = try self.emitter.writeInt(u8, if (c.is_unique) 1 else 0, token);

                _ = try self.emitter.writeInt(C.CONSTANT, anchor_idx, token);
                _ = try self.emitter.writeInt(u8, @as(u8, @intCast(c.tags.len)), token);

                const endJp = try self.emitter.emitJump(.jump, token);
                try self.emitter.patchJump(choiceJp);

                try self.enterScope(.local);

                try self.compileVisit(anchor_idx, token);
                try self.compileBlock(c.body);
                try self.emitter.writeOp(.end, token);
                try self.exitScope();
                try self.emitter.patchJump(endJp);
            },
            .bough => |b| {
                const full_name = try self.getQualifiedName(b.name);
                defer self.alloc.free(full_name);

                try self.pushPathScope(b.name);
                defer self.popPathScope();

                const skipJp = try self.emitter.emitJump(.jump, token);

                try self.enterScope(.local);
                errdefer self.exitScope() catch {};

                const entry_ip = self.emitter.instructionPos();
                const anchor_idx = try self.resolveConstant(full_name) orelse {
                    const sr = try self.suggestAnchor(full_name);
                    const hint = sr;

                    return self.fail("Could not find anchor '{s}'", token, .{full_name}, .{ .end = b.name_token, .suggestion = hint });
                };
                self.emitter.constants.items[anchor_idx].obj.data.anchor.ip = entry_ip;
                try self.compileVisit(anchor_idx, token);

                try self.compileBlock(b.body);
                try self.emitter.writeOp(.end, token);
                try self.exitScope();

                const end = self.emitter.instructionPos();
                try self.emitter.patchJumpTo(skipJp, end);
            },
            .dialogue => |d| {
                for (d.tags) |tag| {
                    try self.emitter.addIdentifierConstant(self.arena.allocator(),tag.name, token);
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

                const index = try self.emitter.addConstant(.{ .obj = obj });

                try self.emitter.writeOp(.loc, token);
                _ = try self.emitter.writeInt(C.CONSTANT, index, token);
                _ = try self.emitter.writeInt(u8, @as(u8, @intCast(s.expressions.len)), token);

                if (d.speaker) |speaker| {
                    try self.emitter.addIdentifierConstant(self.arena.allocator(),speaker, token);
                }
                try self.emitter.writeOp(.dialogue, d.content.token);
                const has_speaker_value = if (d.speaker == null) @as(u8, 0) else @as(u8, 1);
                _ = try self.emitter.writeInt(u8, has_speaker_value, token);
                _ = try self.emitter.writeInt(u8, @as(u8, @intCast(d.tags.len)), token);
            },
            .divert => |d| {
                const anchor_idx = self.resolveAnchor(d.path) orelse {
                    // Produce the joined path as a plain string for the message
                    // and the suggestion hint.
                    const joined = try std.mem.join(self.alloc, ".", d.path);
                    defer self.alloc.free(joined);
                    const sr = try self.suggestAnchor(joined);
                    const hint = sr;

                    return self.fail("Could not find path '{s}'", token, .{joined}, .{ .end = d.end_token, .suggestion = hint });
                };
                if (d.is_backup) {
                    const backupJp = try self.emitter.emitJump(.backup, token);
                    _ = try self.emitter.writeInt(u8, 0, token); // 0 = divert backup
                    try self.emitter.writeOp(.divert, token);
                    _ = try self.emitter.writeInt(C.CONSTANT, anchor_idx, token);
                    try self.emitter.patchJump(backupJp);
                } else {
                    try self.emitter.writeOp(.divert, token);
                    _ = try self.emitter.writeInt(C.CONSTANT, anchor_idx, token);
                }
            },
            .return_expression => |r| {
                try self.compileExpression(&r);
                try self.emitter.writeOp(.return_value, token);
            },
            .return_void => {
                try self.emitter.writeOp(.return_void, token);
            },
            .fin => {
                try self.emitter.writeOp(.fin, token);
            },
            else => {},
        }
    }

    fn evaluateLiteral(self: *Compiler, expr: *const Expression) Error!Value {
        const token = expr.token;
        switch (expr.type) {
            .binary => |bin| {
                const left = try self.evaluateLiteral(bin.left);
                errdefer left.destroyStatic(self.alloc);
                const right = try self.evaluateLiteral(bin.right);

                if (left == .number and right == .number) {
                    return .{
                        .number = switch (bin.operator) {
                            .add => left.number + right.number,
                            .subtract => left.number - right.number,
                            .multiply => left.number * right.number,
                            .divide => if (right.number == 0) return self.fail("Division by zero in static expression", token, .{}, .{ .err = Error.IllegalOperation }) else left.number / right.number,
                            else => return self.fail("Operator not supported in static expressions", token, .{}, .{ .err = Error.IllegalOperation }),
                        },
                    };
                }
                return self.fail("Static math only supported on numbers", token, .{}, .{ .err = Error.IllegalOperation });
            },
            .unary => |u| {
                const val = try self.evaluateLiteral(u.value);
                if (val == .number and u.operator == .negate) return .{ .number = -val.number };
                if (val == .bool and u.operator == .not) return .{ .bool = !val.bool };
                return self.fail("Unary operator not supported in static expressions", token, .{}, .{ .err = Error.IllegalOperation });
            },
            .number => |n| return .{ .number = n },
            .boolean => |b| return .{ .bool = b },
            .string => |s| {
                if (s.expressions.len > 0) {
                    return self.fail("Interpolated strings are not allowed as static default values", token, .{}, .{ .err = Error.IllegalOperation });
                }
                const obj = try self.alloc.create(Value.Obj);
                obj.* = .{ .data = .{ .string = try self.alloc.dupe(u8, s.value) } };
                return .{ .obj = obj };
            },
            .identifier => |id| {
                // Allow referencing other constants (like Enums or other Classes)
                if (self.emitter.constants_map.get(id)) |idx| {
                    return self.emitter.constants.items[idx];
                }
                return self.fail("Identifier '{s}' cannot be resolved", token, .{id}, .{});
            },
            .indexer => |idx| {
                // Handle Enum.Value references
                if (idx.target.type == .identifier and token.token_type == .dot) {
                    const target_name = idx.target.type.identifier;
                    if (self.emitter.constants_map.get(target_name)) |target_idx| {
                        const target_val = self.emitter.constants.items[target_idx];
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
                // Try to render the indexer target.field so the writer knows
                // what the compiler was looking at. If we can resolve the target
                // to a known class/enum, offer a did-you-mean for the field.
                var suggestion: ?[]const u8 = null;
                var note: ?[]const u8 = null;
                // `failErrorWithHelp` takes ownership; on the fall-through
                // error paths below, clean up ourselves.
                var transferred = false;
                errdefer if (!transferred) {
                    if (suggestion) |s| self.alloc.free(s);
                    if (note) |n| self.alloc.free(n);
                };
                if (idx.target.type == .identifier and idx.index.type == .identifier) {
                    const target_name = idx.target.type.identifier;
                    const field_name = idx.index.type.identifier;
                    note = try std.fmt.allocPrint(self.alloc, "could not resolve '{s}.{s}' at compile time", .{ target_name, field_name });
                    if (self.emitter.constants_map.get(target_name)) |target_idx| {
                        const target_val = self.emitter.constants.items[target_idx];
                        const sr = if (target_val == .obj and target_val.obj.data == .class)
                            try self.suggestClassField(target_val.obj.data.class, field_name)
                        else if (target_val == .obj and target_val.obj.data == .@"enum")
                            try self.suggestFromList(field_name, target_val.obj.data.@"enum".values)
                        else
                            null;
                        suggestion = sr;
                    }
                }
                transferred = true;
                return self.fail(
                    "Cannot resolve this indexer at compile time",
                    token,
                    .{},
                    .{ .suggestion = suggestion, .note = note, .err = Error.IllegalOperation },
                );
            },
            .nil => return .nil,
            .list => |l| {
                var list = try std.ArrayList(Value).initCapacity(self.alloc, l.len);
                errdefer {
                    for (list.items) |item| item.destroyStatic(self.alloc);
                    list.deinit(self.alloc);
                }
                for (l) |*item| {
                    try list.append(self.alloc, try self.evaluateLiteral(item));
                }
                const obj = try self.alloc.create(Value.Obj);
                obj.* = .{ .id = UUID.new(), .data = .{ .list = list } };
                return .{ .obj = obj };
            },
            .set => |s| {
                var set = Value.Obj.SetType.empty;
                errdefer {
                    for (set.keys()) |k| k.destroyStatic(self.alloc);
                    set.deinit(self.alloc);
                }
                for (s) |*item| {
                    try set.put(self.alloc, try self.evaluateLiteral(item), {});
                }
                const obj = try self.alloc.create(Value.Obj);
                obj.* = .{ .id = UUID.new(), .data = .{ .set = set } };
                return .{ .obj = obj };
            },
            .map => |m| {
                var map = Value.Obj.MapType.empty;
                errdefer {
                    var it = map.iterator();
                    while (it.next()) |entry| {
                        entry.key_ptr.*.destroyStatic(self.alloc);
                        entry.value_ptr.*.destroyStatic(self.alloc);
                    }
                    map.deinit(self.alloc);
                }
                for (m) |*mp| {
                    const pair = mp.type.map_pair;
                    const key = try self.evaluateLiteral(pair.key);
                    errdefer key.destroyStatic(self.alloc);
                    const value = try self.evaluateLiteral(pair.value);
                    try map.put(self.alloc, key, value);
                }
                const obj = try self.alloc.create(Value.Obj);
                obj.* = .{ .id = UUID.new(), .data = .{ .map = map } };
                return .{ .obj = obj };
            },
            else => return self.fail(
                "Only literal values are allowed here",
                token,
                .{},
                .{
                    .note = try std.fmt.allocPrint(
                        self.alloc,
                        "got a {s}; constants must be numbers, strings, bools, nil, or lists/sets/maps of literals",
                        .{expressionKindName(expr)},
                    ),
                    .err = Error.IllegalOperation,
                },
            ),
        }
    }

    fn compileExternFunctionObj(self: *Compiler, stmt: *const Statement) !*Value.Obj {
        const f = stmt.type.function;
        const obj = try self.alloc.create(Value.Obj);
        obj.* = .{ .data = .{ .@"extern" = .{
            .name = f.name,
            .arity = @intCast(f.parameters.len),
        } } };
        return obj;
    }

    fn compileFunctionObj(self: *Compiler, stmt: *const Statement, token: Token) Error!*Value.Obj {
        const saved_locals_count = self.locals_count;
        self.locals_count = 0;
        try self.enterScope(.function);
        try self.emitter.enterChunk();
        const f = stmt.type.function;

        var length = f.parameters.len;
        if (f.is_method) {
            length += 1;
            _ = try self.scope.define(self.arena.allocator(), "self", false);
        }

        for (f.parameters) |param| {
            _ = try self.scope.define(self.arena.allocator(), param, true);
        }

        try self.compileBlock(f.body);
        if (!(try self.emitter.lastIs(.return_value)) and !(try self.emitter.lastIs(.return_void))) {
            try self.emitter.writeOp(.return_void, token);
        }

        const chunk = try self.emitter.exitChunk();
        defer chunk.deinit();
        // Use the high-water mark: the function scope's own count may not
        // include variables from nested local scopes (for/while/switch).
        const count = @max(self.scope.count, self.locals_count);

        try self.exitScope();
        self.locals_count = saved_locals_count;
        const obj = try self.alloc.create(Value.Obj);

        obj.* = .{
            .id = UUID.new(),
            .data = .{
                .function = .{
                    .name = if (f.name.len > 0) try self.alloc.dupe(u8, f.name) else null,
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

    pub fn compileBlock(self: *Compiler, stmts: []const Statement) Error!void {
        var exited_at: ?usize = null;
        for (stmts, 0..) |*stmt, i| {
            if (exited_at == null and statementExits(stmt)) {
                exited_at = i;
            }
            try self.compileStatement(stmt);
        }
        if (exited_at) |i| {
            // Find the first non-bough statement after the exit — bough
            // declarations are independent entry points, not sequential code.
            for (stmts[i + 1 ..]) |*next| {
                if (next.type != .bough) {
                    const exit_stmt = &stmts[i];
                    const note: ?[]const u8 = switch (exit_stmt.type) {
                        .@"if" => try self.alloc.dupe(u8, "all branches of this 'if' exit"),
                        .@"switch" => try self.alloc.dupe(u8, "all prongs of this 'switch' exit"),
                        else => null,
                    };
                    try self.warn(
                        "Unreachable code after '{s}'",
                        next.token,
                        .{exitKeyword(exit_stmt)},
                        .{ .note = note },
                    );
                    break;
                }
            }
        }
    }

    fn statementExits(stmt: *const Statement) bool {
        return switch (stmt.type) {
            .return_expression, .return_void, .fin => true,
            .fork => |f| !f.is_backup,
            .divert => |d| !d.is_backup,
            .@"if" => |i| i.else_branch != null and
                blockExits(i.then_branch) and
                blockExits(i.else_branch.?),
            .@"switch" => |s| switchAlwaysExits(s.prongs),
            else => false,
        };
    }

    fn switchAlwaysExits(prongs: []const Statement) bool {
        var has_explicit_else = false;
        for (prongs) |prong_stmt| {
            const prong = prong_stmt.type.switch_prong;
            if (prong.values == null) has_explicit_else = true;
            if (!blockExits(prong.body)) return false;
        }
        return has_explicit_else;
    }

    fn exitKeyword(stmt: *const Statement) []const u8 {
        return switch (stmt.type) {
            .return_expression, .return_void => "return",
            .fin => "fin",
            .divert => "divert",
            .fork => "fork",
            .@"if" => "if",
            .@"switch" => "switch",
            else => "",
        };
    }

    fn blockExits(body: []const Statement) bool {
        for (body) |*stmt| {
            if (statementExits(stmt)) return true;
        }
        return false;
    }

    fn registerAnchor(self: *Compiler, full_name: []const u8, uuid: UUID.ID, token: Token) !void {
        // Check for duplicate up-front so the visit symbol is not allocated on
        // failure (keeps error paths clean).
        if (self.emitter.constants_map.contains(full_name)) return error.SymbolAlreadyDeclared;

        const visit_sym = try self.root_scope.define(self.arena.allocator(), full_name, false);
        visit_sym.uuid = uuid;

        var parent_idx: ?C.CONSTANT = null;
        if (self.path_stack.items.len > 0) {
            const parent_path = try std.mem.join(self.alloc, ".", self.path_stack.items);
            defer self.alloc.free(parent_path);
            parent_idx = self.emitter.constants_map.get(parent_path);
        }

        const anchor_obj = try self.alloc.create(Value.Obj);
        // Raw destroy only — `full_name` is borrowed from the caller, who
        // remains responsible for freeing it on error. Calling `obj.deinit`
        // would free the caller's slice.
        errdefer self.alloc.destroy(anchor_obj);
        anchor_obj.* = .{
            .id = uuid,
            .data = .{
                .anchor = .{
                    .name = full_name,
                    .uuid = uuid,
                    .visit_index = visit_sym.index,
                    .parent_anchor_index = parent_idx,
                    .ip = 0,
                },
            },
        };

        try self.addNamedConstantTok(full_name, .{ .obj = anchor_obj }, token);
    }

    fn compileVisit(self: *Compiler, anchor_idx: C.CONSTANT, token: Token) !void {
        try self.emitter.writeOp(.visit, token);
        _ = try self.emitter.writeInt(C.CONSTANT, anchor_idx, token);
    }

    pub fn compileExpression(self: *Compiler, expr: *const Expression) Error!void {
        const token = expr.token;
        switch (expr.type) {
            .binary => |bin| {
                if (bin.operator == .less_than or bin.operator == .less_than_equal) {
                    try self.compileExpression(bin.right);
                    try self.compileExpression(bin.left);
                    try self.emitter.writeOp(switch (bin.operator) {
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
                            const symbol = try self.scope.resolve(self.arena.allocator(), id);
                            if (symbol) |s| {
                                s.var_type = inferExprType(bin.right);
                            }
                            try self.setSymbol(id, symbol, token, false);
                            try self.loadSymbol(id, token);
                            return;
                        },
                        .indexer => |idx| {
                            if (idx.target.type == .identifier) {
                                if (self.emitter.constants_map.get(idx.target.type.identifier)) |i| {
                                    const val = self.emitter.constants.items[i];
                                    return self.fail("Cannot reassign field on a {s}", idx.target.token, .{objKindName(val.obj.data)}, .{ .end = idx.index.token });
                                } else {
                                    try self.validateDotAccess(idx.target, idx.index, bin.left.token, idx.target.token, idx.index.token);
                                }
                            }
                            try self.compileExpression(bin.right);
                            try self.emitter.writeOp(.dup, token);
                            try self.compileExpression(bin.left);
                            try self.emitter.removeLast(.index);
                            try self.emitter.writeOp(.set_property, token);
                            return;
                        },
                        else => return self.failAssignTarget(bin.left),
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
                        return self.fail("Unknown operation '{s}'", token, .{bin.operator.toString()}, .{ .err = Error.IllegalOperation });
                    },
                };
                try self.emitter.writeOp(op, token);

                switch (bin.operator) {
                    .assign_add, .assign_subtract, .assign_multiply, .assign_divide, .assign_modulus => {
                        switch (bin.left.type) {
                            .identifier => |id| {
                                const symbol = try self.scope.resolve(self.arena.allocator(), id);
                                try self.setSymbol(id, symbol, token, false);
                                try self.loadSymbol(id, token);
                                return;
                            },
                            .indexer => |idx| {
                                if (idx.target.type == .identifier) {
                                    if (self.emitter.constants_map.get(idx.target.type.identifier)) |i| {
                                        const val = self.emitter.constants.items[i];
                                        if (val == .obj and (val.obj.data == .class or val.obj.data == .@"enum" or val.obj.data == .function)) {
                                            return self.fail("Cannot assign to a {s} member", idx.target.token, .{objKindName(val.obj.data)}, .{ .end = idx.index.token });
                                        }
                                    } else {
                                        try self.validateDotAccess(idx.target, idx.index, bin.left.token, idx.target.token, idx.index.token);
                                    }
                                }
                                try self.emitter.writeOp(.dup, token);
                                try self.compileExpression(bin.left);
                                try self.emitter.removeLast(.index);
                                try self.emitter.writeOp(.set_property, token);
                                return;
                            },
                            else => return self.failAssignTarget(bin.left),
                        }
                    },
                    else => {},
                }
            },
            .number => |n| {
                const i = try self.emitter.addLiteralConstant(self.arena.allocator(),.{ .number = n });
                try self.emitter.writeOp(.constant, token);
                _ = try self.emitter.writeInt(C.CONSTANT, i, token);
            },
            .boolean => |b| try self.emitter.writeOp(if (b) .true else .false, token),
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
                const index = try self.emitter.addConstant(.{ .obj = obj });
                try self.emitter.writeOp(.string, token);
                _ = try self.emitter.writeInt(C.CONSTANT, index, token);
                _ = try self.emitter.writeInt(u8, @as(u8, @intCast(s.expressions.len)), token);
            },
            .list => |l| {
                for (l) |*item| {
                    try self.compileExpression(item);
                }
                try self.emitter.writeOp(.list, token);
                const size = C.COLLECTION;
                const length = @as(size, @intCast(l.len));
                _ = try self.emitter.writeInt(size, length, token);
            },
            .map => |m| {
                for (m) |*mp| {
                    try self.compileExpression(mp);
                }
                const size = C.COLLECTION;
                try self.emitter.writeOp(.map, token);
                const length = @as(size, @intCast(m.len));
                _ = try self.emitter.writeInt(size, length, token);
            },
            .set => |s| {
                for (s) |*item| {
                    try self.compileExpression(item);
                }
                const size = C.COLLECTION;
                try self.emitter.writeOp(.set, token);
                const length = @as(size, @intCast(s.len));
                _ = try self.emitter.writeInt(size, length, token);
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
                        const anchor = self.emitter.constants.items[i].obj.data.anchor;
                        try self.emitter.writeOp(.get_global, token);
                        _ = try self.emitter.writeInt(C.GLOBAL, anchor.visit_index, token);
                        return;
                    }
                }
                try self.compileExpression(idx.target);
                if (token.token_type == .dot) {
                    if (idx.target.type == .identifier) {
                        if (self.emitter.constants_map.get(idx.target.type.identifier)) |i| {
                            const val = self.emitter.constants.items[i];
                            if (val == .obj) {
                                switch (val.obj.data) {
                                    .anchor => {
                                        // If we're indexing into an anchor (e.g., Parent.Child)
                                        // we check if the path resolves to another anchor
                                        var nested_parts = std.ArrayList([]const u8).empty;
                                        defer nested_parts.deinit(self.alloc);
                                        if (try self.flattenIndexer(expr, &nested_parts)) {
                                            if (self.resolveAnchor(nested_parts.items)) |anchor_idx| {
                                                try self.emitter.writeOp(.pop, token); // pop the target anchor
                                                const anchor = self.emitter.constants.items[anchor_idx].obj.data.anchor;
                                                try self.emitter.writeOp(.get_global, token);
                                                _ = try self.emitter.writeInt(C.GLOBAL, anchor.visit_index, token);
                                                return;
                                            }
                                        }
                                    },
                                    .@"enum" => |e| {
                                        const field = idx.index.type.identifier;
                                        if (!arrayContains(u8, e.values, field)) {
                                            const sr = try self.suggestFromList(field, e.values);
                                            const hint = sr;
                        
                                            return self.fail(
                                                "Enum '{s}' does not contain a value '{s}'",
                                                idx.target.token,
                                                .{ idx.target.type.identifier, field },
                                                .{ .end = idx.index.token, .suggestion = hint },
                                            );
                                        }
                                    },
                                    .class => |c| {
                                        const field = idx.index.type.identifier;
                                        if (c.getFieldIndex(field) == null and c.getMethodIndex(field) == null) {
                                            const sr = try self.suggestClassField(c, field);
                                            const hint = sr;
                        
                                            return self.fail(
                                                "Class '{s}' does not contain a field '{s}'",
                                                idx.target.token,
                                                .{ idx.target.type.identifier, field },
                                                .{ .end = idx.index.token, .suggestion = hint },
                                            );
                                        }
                                    },
                                    else => {},
                                }
                            }
                        } else {
                            try self.validateDotAccess(idx.target, idx.index, token, idx.target.token, idx.index.token);
                        }
                    }
                    try self.emitter.addIdentifierConstant(self.arena.allocator(),idx.index.type.identifier, token);
                } else try self.compileExpression(idx.index);
                try self.emitter.writeOp(.index, token);
            },
            .unary => |u| {
                try self.compileExpression(u.value);
                switch (u.operator) {
                    .negate => try self.emitter.writeOp(.negate, token),
                    .not => try self.emitter.writeOp(.not, token),
                }
            },
            .identifier => |id| {
                if (try self.resolveConstant(id)) |i| {
                    const val = self.emitter.constants.items[i];
                    if (val == .obj and val.obj.data == .anchor) {
                        try self.emitter.writeOp(.get_global, token);
                        _ = try self.emitter.writeInt(C.GLOBAL, val.obj.data.anchor.visit_index, token);
                        return;
                    }
                }
                try self.loadSymbol(id, token);
            },
            .@"if" => |i| {
                try self.compileExpression(i.condition);
                const falseJp = try self.emitter.emitJump(.jump_if_false, token);
                try self.compileExpression(i.then_value);

                const endJp = try self.emitter.emitJump(.jump, token);
                try self.emitter.patchJump(falseJp);

                try self.compileExpression(i.else_value);
                try self.emitter.patchJump(endJp);
            },
            .instance => |ins| {
                const const_idx = self.emitter.constants_map.get(ins.name) orelse
                    return self.fail("Unknown class '{s}'", token, .{ins.name}, .{ .end = ins.name_token });

                const class_val = self.emitter.constants.items[const_idx];
                if (class_val != .obj or class_val.obj.data != .class)
                    return self.fail("'{s}' is not a class", token, .{ins.name}, .{ .end = ins.name_token });

                const class_def = class_val.obj.data.class;
                for (ins.fields, 0..) |field_expr, i| {
                    if (class_def.getFieldIndex(ins.field_names[i]) == null) {
                        const sr = try self.suggestClassField(class_def, ins.field_names[i]);
                        const hint = sr;
    
                        return self.fail(
                            "Class '{s}' has no field '{s}'",
                            ins.field_name_tokens[i],
                            .{ ins.name, ins.field_names[i] },
                            .{ .suggestion = hint },
                        );
                    }

                    try self.compileExpression(&field_expr);
                    // Push the name of the field so the VM knows which one we are setting
                    try self.emitter.addIdentifierConstant(self.arena.allocator(),ins.field_names[i], token);
                }

                try self.emitter.writeOp(.constant, token);
                _ = try self.emitter.writeInt(C.CONSTANT, const_idx, token);

                try self.emitter.writeOp(.instance, token);
                _ = try self.emitter.writeInt(C.FIELDS, @as(C.FIELDS, @intCast(ins.fields.len)), token);
            },
            .call => |c| {
                // Arity check for direct calls to known user functions. Skip
                // dynamic calls (method access, calls on expressions) since we
                // can't resolve them statically.
                if (c.target.type == .identifier) {
                    const callee_name = c.target.type.identifier;
                    if (self.resolveFnArity(callee_name)) |expected| {
                        if (expected != c.arguments.len) {
                            const note = try self.previousDeclNote(callee_name);
                            return self.fail(
                                "'{s}' expects {d} argument(s), but got {d}",
                                token,
                                .{ callee_name, expected, c.arguments.len },
                                .{ .note = note },
                            );
                        }
                    }
                }
                try self.compileExpression(c.target);
                for (c.arguments) |*arg| {
                    try self.compileExpression(arg);
                }
                try self.emitter.writeOp(.call, token);
                const size = C.ARGS;
                std.debug.assert(c.arguments.len < std.math.maxInt(size));
                var length = c.arguments.len;
                if (c.target.type == .indexer) length += 1;
                _ = try self.emitter.writeInt(size, @as(size, @intCast(length)), token);
            },
            .range => |r| {
                try self.compileExpression(r.right);
                try self.compileExpression(r.left);
                try self.emitter.writeOp(.range, token);
            },
            else => return Error.NotYetImplemented,
        }
    }

    fn setSymbol(self: *Compiler, name: []const u8, symbol: ?*Symbol, token: Token, is_decl: bool) !void {
        if (try self.resolveConstant(name) != null) {
            return self.fail("Cannot assign to constant '{s}'", token, .{name}, .{});
        }

        if (symbol) |s| {
            if (!is_decl and !s.is_mutable) return self.fail("Cannot assign to constant variable '{s}'", token, .{s.name}, .{});
            switch (s.tag) {
                .global => {
                    try self.emitter.writeOp(if (is_decl) .decl_global else .set_global, token);
                    _ = try self.emitter.writeInt(C.GLOBAL, @as(C.GLOBAL, @intCast(s.index)), token);
                },
                .upvalue => {
                    std.debug.assert(is_decl == false);
                    try self.emitter.writeOp(.set_upvalue, token);
                    const depth = self.calculateScopeDepth(s);
                    _ = try self.emitter.writeInt(u8, @intCast(depth), token);
                    _ = try self.emitter.writeInt(C.LOCAL, @as(C.LOCAL, @intCast(s.index)), token);
                },
                .local, .function => {
                    try self.emitter.writeOp(.set_local, token);
                    _ = try self.emitter.writeInt(C.LOCAL, @as(C.LOCAL, @intCast(s.index)), token);
                },
            }
        } else {
            const sr = try self.suggestForSymbol(name);
            const hint = sr;
            return self.fail("Unknown name '{s}'", token, .{name}, .{ .suggestion = hint, .err = Error.SymbolNotFound });
        }
    }

    fn loadSymbol(self: *Compiler, name: []const u8, token: Token) !void {
        const symbol = try self.scope.resolve(self.arena.allocator(), name);

        if (symbol) |s| {
            switch (s.tag) {
                .local, .function => {
                    try self.emitter.writeOp(.get_local, token);
                    _ = try self.emitter.writeInt(C.LOCAL, @intCast(s.index), token);
                    return;
                },
                .upvalue => {
                    try self.emitter.writeOp(.get_upvalue, token);
                    const depth = self.calculateScopeDepth(s);
                    _ = try self.emitter.writeInt(u8, @intCast(depth), token);
                    _ = try self.emitter.writeInt(C.LOCAL, @intCast(s.index), token);
                    return;
                },
                .global => {
                    try self.emitter.writeOp(.get_global, token);
                    _ = try self.emitter.writeInt(C.GLOBAL, @intCast(s.index), token);
                    return;
                },
            }
        }

        if (try self.resolveConstant(name)) |i| {
            try self.emitter.writeOp(.constant, token);
            _ = try self.emitter.writeInt(C.CONSTANT, i, token);
            return;
        }

        const sr = try self.suggestForSymbol(name);
        const hint = sr;
        return self.fail("Unknown name '{s}'", token, .{name}, .{ .suggestion = hint, .err = Error.SymbolNotFound });
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
            if (self.emitter.constants_map.get(full_name)) |idx| return idx;
        }

        return self.emitter.constants_map.get(name);
    }

    /// Wrapper that emits a "Constant {s} not found" diagnostic on miss.
    /// Codegen callers can drop this and use `self.emitter.replaceConstant`
    /// directly since the path is always known to exist by construction.
    fn replaceConstant(self: *Compiler, name: []const u8, value: Value, token: Token) Error!void {
        self.emitter.replaceConstant(name, value) catch {
            return self.fail("Constant {s} not found", token, .{name}, .{});
        };
    }

    fn resolveAnchor(self: *Compiler, path_parts: [][]const u8) ?C.CONSTANT {
        const path = std.mem.join(self.alloc, ".", path_parts) catch return null;
        defer self.alloc.free(path);

        const idx = self.resolveConstant(path) catch return null;
        if (idx) |i| {
            const value = self.emitter.constants.items[i];
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

    /// Wrapper that records the declaration token in `decl_tokens` for
    /// "previous declaration at" diagnostics. The Emitter's named-constant
    /// add does the actual insert; we look up the duped key it stored
    /// and reuse it as the decl_tokens key.
    fn addNamedConstantTok(self: *Compiler, name: []const u8, value: Value, token: ?Token) !void {
        const aa = self.arena.allocator();
        try self.emitter.addNamedConstantTok(aa, name, value);
        if (token) |t| {
            try self.decl_tokens.ensureUnusedCapacity(aa, 1);
            const persistent_key = self.emitter.constants_map.getKey(name).?;
            self.decl_tokens.putAssumeCapacity(persistent_key, t);
        }
    }

    fn addNamedConstant(self: *Compiler, name: []const u8, value: Value) !void {
        return self.addNamedConstantTok(name, value, null);
    }
};
