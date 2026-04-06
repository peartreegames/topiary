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
const suggest = @import("suggest.zig");

// Placeholder values for jump targets that get patched later.
// Use values near maxInt(u32) to avoid collisions with real instruction addresses.
const max_jump = std.math.maxInt(C.JUMP);
const BREAK_HOLDER: C.JUMP = max_jump - 0;
const CONTINUE_HOLDER: C.JUMP = max_jump - 1;
const CHOICE_HOLDER: C.JUMP = max_jump - 2;
const FORK_HOLDER: C.JUMP = max_jump - 3;
const DIVERT_HOLDER: C.JUMP = max_jump - 4;
const PRONG_HOLDER: C.JUMP = max_jump - 5;
const SWITCH_END_HOLDER: C.JUMP = max_jump - 6;
const JUMP_HOLDER: C.JUMP = max_jump - 7;

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

    // Diagnostics tracking: keys are borrowed from `constants_map` keys.
    decl_tokens: std.StringHashMapUnmanaged(Token) = .empty,
    fn_arities: std.StringHashMapUnmanaged(u8) = .empty,

    pub const Chunk = struct {
        instructions: std.ArrayList(u8) = .empty,
        debug_markers: std.ArrayList(Marker) = .empty,
        parent: ?*Chunk,
        module: *Module,
        alloc: std.mem.Allocator,
        last_op_pos: ?usize = null,

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
                        file_name = try allocator.dupe(u8, name);
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
        var chunk: ?*Chunk = self.chunk;
        while (chunk) |c| {
            chunk = c.parent;
            c.deinit();
        }
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
        self.decl_tokens.deinit(self.alloc);
        self.fn_arities.deinit(self.alloc);
    }

    fn fail(self: *Compiler, comptime msg: []const u8, token: Token, args: anytype) Error {
        try self.module.errors.add(self.current_file.path, msg, token, .err, args);
        return Error.CompilerError;
    }

    fn failError(self: *Compiler, comptime msg: []const u8, token: Token, args: anytype, err: Error) Error {
        try self.module.errors.add(self.current_file.path, msg, token, .err, args);
        return err;
    }

    fn failWithHelp(
        self: *Compiler,
        comptime msg: []const u8,
        token: Token,
        args: anytype,
        suggestion: ?[]const u8,
        note: ?[]const u8,
    ) Error {
        try self.module.errors.addWithHelp(self.current_file.path, msg, token, .err, args, suggestion, note);
        return Error.CompilerError;
    }

    fn failSpanWithHelp(
        self: *Compiler,
        comptime msg: []const u8,
        start_token: Token,
        end_token: ?Token,
        args: anytype,
        suggestion: ?[]const u8,
        note: ?[]const u8,
    ) Error {
        try self.module.errors.addSpanWithHelp(self.current_file.path, msg, start_token, end_token, .err, args, suggestion, note);
        return Error.CompilerError;
    }

    fn failErrorWithHelp(
        self: *Compiler,
        comptime msg: []const u8,
        token: Token,
        args: anytype,
        suggestion: ?[]const u8,
        note: ?[]const u8,
        err: Error,
    ) Error {
        try self.module.errors.addWithHelp(self.current_file.path, msg, token, .err, args, suggestion, note);
        return err;
    }

    fn warnWithHelp(
        self: *Compiler,
        comptime msg: []const u8,
        token: Token,
        args: anytype,
        suggestion: ?[]const u8,
        note: ?[]const u8,
    ) !void {
        try self.module.errors.addWithHelp(self.current_file.path, msg, token, .warn, args, suggestion, note);
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
        // Plus all top-level (unqualified) constants — classes, enums,
        // functions, boughs with no path prefix.
        var it = self.constants_map.keyIterator();
        while (it.next()) |k| {
            if (std.mem.indexOfScalar(u8, k.*, '.') == null)
                try names.append(self.alloc, k.*);
        }

        const match = (try suggest.closest(self.alloc, name, names.items)) orelse return null;
        defer self.alloc.free(match);
        return try std.fmt.allocPrint(self.alloc, "did you mean '{s}'?", .{match});
    }

    fn suggestFromConstants(self: *Compiler, name: []const u8) !?[]const u8 {
        // Collect top-level names (those without a '.' in them) as the likely
        // candidates for a typo. This avoids suggesting nested anchor paths.
        var names: std.ArrayList([]const u8) = .empty;
        defer names.deinit(self.alloc);
        var it = self.constants_map.keyIterator();
        while (it.next()) |k| {
            try names.append(self.alloc, k.*);
        }
        const match = (try suggest.closest(self.alloc, name, names.items)) orelse return null;
        defer self.alloc.free(match);
        return try std.fmt.allocPrint(self.alloc, "did you mean '{s}'?", .{match});
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

    fn failRedeclared(self: *Compiler, name: []const u8, comptime kind: []const u8, token: Token) Error {
        // Look up the previous declaration's full name in the declarations map.
        // The caller passes the short name; we try the qualified form first.
        var lookup_name: []const u8 = name;
        const qualified = self.getQualifiedName(name) catch null;
        defer if (qualified) |q| self.alloc.free(q);
        if (qualified) |q| {
            if (self.decl_tokens.contains(q)) lookup_name = q;
        }
        const note = try self.previousDeclNote(lookup_name);
        return self.failWithHelp(kind ++ " '{s}' is already declared", token, .{name}, null, note);
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

    /// Writer-friendly name for an expression's syntactic kind. Used in
    /// error messages instead of leaking `@tagName` (which exposes
    /// internal AST tags like "binary" or "call").
    fn expressionKindName(expr: Expression) []const u8 {
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
        return self.failWithHelp(
            "Cannot assign to a {s}",
            left.token,
            .{expressionKindName(left.*)},
            null,
            try self.alloc.dupe(u8, "assignment targets must be a variable or an indexer (e.g. `list[0]` or `obj.field`)"),
        );
    }

    fn suggestAnchor(self: *Compiler, path: []const u8) !?[]const u8 {
        var names: std.ArrayList([]const u8) = .empty;
        defer names.deinit(self.alloc);
        var it = self.constants_map.iterator();
        while (it.next()) |entry| {
            const val = self.constants.items[entry.value_ptr.*];
            if (val == .obj and val.obj.data == .anchor) {
                try names.append(self.alloc, entry.key_ptr.*);
            }
        }
        const match = (try suggest.closest(self.alloc, path, names.items)) orelse return null;
        defer self.alloc.free(match);
        return try std.fmt.allocPrint(self.alloc, "did you mean '{s}'?", .{match});
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
        if (old_scope.tag == .local) {
            // Local scopes inherit the parent's count, so old_scope.count
            // already includes the parent range — no need to add both.
            self.locals_count = @max(self.locals_count, old_scope.count);
        } else {
            self.locals_count = @max(self.locals_count, self.scope.count + old_scope.count);
        }
        old_scope.destroy();
    }

    fn resolveForkName(self: *Compiler, name: ?[]const u8) Error![]const u8 {
        if (name) |n| return self.alloc.dupe(u8, n);
        if (self.anon_counters.items.len == 0) return Error.CompilerError;
        const current_depth = self.anon_counters.items.len - 1;
        const count = self.anon_counters.items[current_depth];
        const fork_name = try std.fmt.allocPrint(self.alloc, "_{d}", .{count});
        self.anon_counters.items[current_depth] += 1;
        return fork_name;
    }

    fn pushPathScope(self: *Compiler, name: []const u8) Error!void {
        try self.path_stack.append(self.alloc, name);
        try self.anon_counters.append(self.alloc, 0);
    }

    fn popPathScope(self: *Compiler) void {
        _ = self.path_stack.pop();
        _ = self.anon_counters.pop();
    }

    const IncludeResult = struct {
        file: *File,
        tree: []const Statement,
    };

    fn resolveInclude(self: *Compiler, raw_path: []const u8, token: Token) Error!?IncludeResult {
        // Resolve original path to module-root-relative form
        const resolved = self.current_file.module.resolveIncludePath(self.current_file, raw_path) catch
            return self.fail("Could not resolve include path '{s}'", token, .{raw_path});
        const file = self.module.includes.get(resolved) orelse
            return self.fail("Unknown include file {s}", token, .{raw_path});
        if (self.emitted_files.contains(resolved)) return null;
        try self.emitted_files.put(self.alloc, resolved, {});
        const tree = file.tree orelse return Error.NotInitialized;
        return .{ .file = file, .tree = tree.root };
    }

    fn prepass(self: *Compiler, stmt: Statement) Error!void {
        switch (stmt.type) {
            .include => |i| {
                const result = try self.resolveInclude(i, stmt.token) orelse return;
                const tmp = self.current_file;
                defer self.current_file = tmp;
                self.current_file = result.file;
                for (result.tree) |s| try self.prepass(s);
            },
            .function => |f| {
                const full_name = try self.getQualifiedName(f.name);
                defer self.alloc.free(full_name);
                self.addNamedConstantTok(full_name, .nil, stmt.token) catch |err| switch (err) {
                    error.SymbolAlreadyDeclared => return self.failRedeclared(full_name, "Function", stmt.token),
                    else => return err,
                };
                // Track arity so call sites can validate argument counts.
                if (self.constants_map.getKey(full_name)) |persistent_key| {
                    try self.fn_arities.put(self.alloc, persistent_key, @intCast(f.parameters.len));
                }
                if (f.is_extern) {
                    _ = try self.addConstant(.{ .obj = try self.compileExternFunctionObj(stmt) });
                }
            },
            .class => |c| {
                const full_name = try self.getQualifiedName(c.name);
                defer self.alloc.free(full_name);
                self.addNamedConstantTok(full_name, .nil, stmt.token) catch |err| switch (err) {
                    error.SymbolAlreadyDeclared => return self.failRedeclared(full_name, "Class", stmt.token),
                    else => return err,
                };
            },
            .@"enum" => |e| {
                const full_name = try self.getQualifiedName(e.name);
                defer self.alloc.free(full_name);
                self.addNamedConstantTok(full_name, .nil, stmt.token) catch |err| switch (err) {
                    error.SymbolAlreadyDeclared => return self.failRedeclared(full_name, "Enum", stmt.token),
                    else => return err,
                };
            },
            .bough => |b| {
                const full_name = try self.getQualifiedName(b.name);
                self.registerAnchor(full_name, stmt.token) catch |err| switch (err) {
                    error.SymbolAlreadyDeclared => {
                        self.alloc.free(full_name);
                        return self.failRedeclared(b.name, "Bough", stmt.token);
                    },
                    else => return err,
                };
                try self.pushPathScope(b.name);
                defer self.popPathScope();
                for (b.body) |s| try self.prepass(s);
            },
            .fork => |f| {
                const fork_name = self.resolveForkName(f.name) catch
                    return self.fail("fork must be inside a bough", stmt.token, .{});
                defer self.alloc.free(fork_name);

                const full_name = try self.getQualifiedName(fork_name);
                self.registerAnchor(full_name, stmt.token) catch |err| switch (err) {
                    error.SymbolAlreadyDeclared => {
                        self.alloc.free(full_name);
                        return self.failRedeclared(fork_name, "Fork", stmt.token);
                    },
                    else => return err,
                };
                try self.pushPathScope(fork_name);
                defer self.popPathScope();

                for (f.body) |s| try self.prepass(s);
            },
            .choice => |c| {
                const name = c.name orelse &c.id;
                const full_name = try self.getQualifiedName(name);
                self.registerAnchor(full_name, stmt.token) catch |err| switch (err) {
                    error.SymbolAlreadyDeclared => {
                        self.alloc.free(full_name);
                        return self.failRedeclared(name, "Choice", stmt.token);
                    },
                    else => return err,
                };
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
                const result = try self.resolveInclude(i, stmt.token) orelse return;
                const tmp_file = self.current_file;
                defer self.current_file = tmp_file;
                self.current_file = result.file;
                for (result.tree) |s| try self.compileStatement(s);
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
                const capture = try self.scope.define(f.capture, false);
                try self.writeOp(.set_local, token);
                _ = try self.writeInt(C.LOCAL, @as(C.LOCAL, @intCast(capture.index)), token);

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
                // Warn when the new name hides a variable from an enclosing
                // scope. Only warn for local-to-local shadowing so global
                // variables intentionally re-used across scopes remain quiet.
                if (self.scope.tag != .global) {
                    if (self.scope.resolveOuter(v.name)) |_| {
                        try self.warnWithHelp(
                            "'{s}' hides a variable from an outer scope",
                            token,
                            .{v.name},
                            null,
                            null,
                        );
                    }
                }
                const symbol = self.scope.define(v.name, v.is_mutable) catch {
                    return self.fail("'{s}' is already declared in this scope", token, .{v.name});
                };
                try self.compileExpression(&v.initializer);
                try self.setSymbol(v.name, symbol, token, true);
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

                for (c.methods, 0..) |method_stmt, i| {
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
                const fork_name = self.resolveForkName(f.name) catch
                    return self.fail("fork must be inside a bough", stmt.token, .{});
                defer self.alloc.free(fork_name);

                const path = try self.getQualifiedName(fork_name);
                defer self.alloc.free(path);

                try self.pushPathScope(fork_name);
                defer self.popPathScope();

                const start_pos = self.instructionPos();
                const anchor_idx = try self.resolveConstant(path);
                if (anchor_idx) |idx| {
                    self.constants.items[idx].obj.data.anchor.ip = start_pos;
                    try self.compileVisit(idx, token);
                } else {
                    const hint = try self.suggestAnchor(path);
                    return self.failSpanWithHelp("Could not find anchor '{s}'", token, f.end_token, .{path}, hint, null);
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
                            if (!blockHasExit(choice.body)) {
                                try self.warnWithHelp(
                                    "choice has no divert or 'fin' — execution will end silently after this choice",
                                    body_stmt.token,
                                    .{},
                                    try self.alloc.dupe(u8, "use 'fork^' to continue after the choice, or add a divert '=>' inside the choice body"),
                                    null,
                                );
                            }
                        }
                    }
                }

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
                const anchor_idx = try self.resolveConstant(full_name) orelse {
                    const hint = try self.suggestAnchor(full_name);
                    return self.failWithHelp("Could not find anchor '{s}'", token, .{full_name}, hint, null);
                };
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

                try self.pushPathScope(b.name);
                defer self.popPathScope();

                try self.writeOp(.jump, token);
                const start_pos = try self.writeInt(C.JUMP, JUMP_HOLDER, token);

                try self.enterScope(.local);
                errdefer self.exitScope() catch {};

                const entry_ip = self.instructionPos();
                const anchor_idx = try self.resolveConstant(full_name) orelse {
                    const hint = try self.suggestAnchor(full_name);
                    return self.failSpanWithHelp("Could not find anchor '{s}'", token, b.name_token, .{full_name}, hint, null);
                };
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
                const anchor_idx = self.resolveAnchor(d.path) orelse {
                    // Produce the joined path as a plain string for the message
                    // and the suggestion hint.
                    const joined = try std.mem.join(self.alloc, ".", d.path);
                    defer self.alloc.free(joined);
                    const hint = try self.suggestAnchor(joined);
                    return self.failSpanWithHelp("Could not find path '{s}'", token, d.end_token, .{joined}, hint, null);
                };
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
                errdefer left.destroyStatic(self.alloc);
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
                // Try to render the indexer target.field so the writer knows
                // what the compiler was looking at. If we can resolve the target
                // to a known class/enum, offer a did-you-mean for the field.
                var suggestion: ?[]const u8 = null;
                var note: ?[]const u8 = null;
                if (idx.target.type == .identifier and idx.index.type == .identifier) {
                    const target_name = idx.target.type.identifier;
                    const field_name = idx.index.type.identifier;
                    note = try std.fmt.allocPrint(self.alloc, "could not resolve '{s}.{s}' at compile time", .{ target_name, field_name });
                    if (self.constants_map.get(target_name)) |target_idx| {
                        const target_val = self.constants.items[target_idx];
                        if (target_val == .obj and target_val.obj.data == .class) {
                            suggestion = try self.suggestClassField(target_val.obj.data.class, field_name);
                        } else if (target_val == .obj and target_val.obj.data == .@"enum") {
                            suggestion = try self.suggestFromList(field_name, target_val.obj.data.@"enum".values);
                        }
                    }
                }
                return self.failErrorWithHelp(
                    "Cannot resolve this indexer at compile time",
                    token,
                    .{},
                    suggestion,
                    note,
                    Error.IllegalOperation,
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
            else => return self.failErrorWithHelp(
                "Only literal values are allowed here",
                token,
                .{},
                null,
                try std.fmt.allocPrint(
                    self.alloc,
                    "got a {s}; constants must be numbers, strings, bools, nil, or lists/sets/maps of literals",
                    .{expressionKindName(expr.*)},
                ),
                Error.IllegalOperation,
            ),
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
        const saved_locals_count = self.locals_count;
        self.locals_count = 0;
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

    fn lastIs(self: *Compiler, op: OpCode) !bool {
        const pos = self.chunk.last_op_pos orelse return false;
        return self.chunk.instructions.items[pos] == @intFromEnum(op);
    }

    fn removeLast(self: *Compiler, op: OpCode) !void {
        if (try self.lastIs(op)) {
            const pos = self.chunk.last_op_pos.?;
            self.chunk.instructions.items.len = pos;
            self.chunk.debug_markers.items.len = pos;
            self.chunk.last_op_pos = null;
        }
    }

    pub fn compileBlock(self: *Compiler, stmts: []const Statement) Error!void {
        var exited_at: ?usize = null;
        for (stmts, 0..) |stmt, i| {
            if (exited_at == null and isUnconditionalExit(stmt)) {
                exited_at = i;
            }
            try self.compileStatement(stmt);
        }
        if (exited_at) |i| {
            // Find the first non-bough statement after the exit — bough
            // declarations are independent entry points, not sequential code.
            for (stmts[i + 1 ..]) |next| {
                if (next.type != .bough) {
                    try self.warnWithHelp(
                        "Unreachable code after '{s}'",
                        next.token,
                        .{exitKeyword(stmts[i])},
                        null,
                        null,
                    );
                    break;
                }
            }
        }
    }

    fn isUnconditionalExit(stmt: Statement) bool {
        return switch (stmt.type) {
            .return_expression, .return_void, .fin => true,
            .fork => |f| !f.is_backup,
            .divert => |d| !d.is_backup,
            else => false,
        };
    }

    fn exitKeyword(stmt: Statement) []const u8 {
        return switch (stmt.type) {
            .return_expression, .return_void => "return",
            .fin => "fin",
            .divert => "divert",
            .fork => "fork",
            else => "",
        };
    }

    fn blockHasExit(body: []const Statement) bool {
        for (body) |stmt| {
            if (isUnconditionalExit(stmt)) return true;
        }
        return false;
    }

    fn registerAnchor(self: *Compiler, full_name: []const u8, token: Token) !void {
        // Check for duplicate up-front so the visit symbol is not allocated on
        // failure (keeps error paths clean).
        if (self.constants_map.contains(full_name)) return error.SymbolAlreadyDeclared;

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

        try self.addNamedConstantTok(full_name, .{ .obj = anchor_obj }, token);
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
                                    return self.fail("Cannot reassign field on a {s}", token, .{objKindName(val.obj.data)});
                                }
                            }
                            try self.compileExpression(bin.right);
                            try self.writeOp(.dup, token);
                            try self.compileExpression(bin.left);
                            try self.removeLast(.index);
                            try self.writeOp(.set_property, token);
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
                                            return self.fail("Cannot assign to a {s} member", token, .{objKindName(val.obj.data)});
                                        }
                                    }
                                }
                                try self.writeOp(.dup, token);
                                try self.compileExpression(bin.left);
                                try self.removeLast(.index);
                                try self.writeOp(.set_property, token);
                                return;
                            },
                            else => return self.failAssignTarget(bin.left),
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
                                        const field = idx.index.type.identifier;
                                        if (!arrayContains(u8, e.values, field)) {
                                            const hint = try self.suggestFromList(field, e.values);
                                            return self.failWithHelp(
                                                "Enum '{s}' does not contain a value '{s}'",
                                                idx.index.token,
                                                .{ idx.target.type.identifier, field },
                                                hint,
                                                null,
                                            );
                                        }
                                    },
                                    .class => |c| {
                                        const field = idx.index.type.identifier;
                                        if (c.getFieldIndex(field) == null and c.getMethodIndex(field) == null) {
                                            const hint = try self.suggestClassField(c, field);
                                            return self.failWithHelp(
                                                "Class '{s}' does not contain a field '{s}'",
                                                idx.index.token,
                                                .{ idx.target.type.identifier, field },
                                                hint,
                                                null,
                                            );
                                        }
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
                        const hint = try self.suggestClassField(class_def, ins.field_names[i]);
                        return self.failWithHelp(
                            "Class '{s}' has no field '{s}'",
                            token,
                            .{ ins.name, ins.field_names[i] },
                            hint,
                            null,
                        );
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
                // Arity check for direct calls to known user functions. Skip
                // dynamic calls (method access, calls on expressions) since we
                // can't resolve them statically.
                if (c.target.type == .identifier) {
                    const callee_name = c.target.type.identifier;
                    if (self.resolveFnArity(callee_name)) |expected| {
                        if (expected != c.arguments.len) {
                            const note = try self.previousDeclNote(callee_name);
                            return self.failWithHelp(
                                "'{s}' expects {d} argument(s), but got {d}",
                                token,
                                .{ callee_name, expected, c.arguments.len },
                                null,
                                note,
                            );
                        }
                    }
                }
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
        } else {
            const hint = try self.suggestForSymbol(name);
            try self.module.errors.addWithHelp(
                self.current_file.path,
                "Unknown name '{s}'",
                token,
                .err,
                .{name},
                hint,
                null,
            );
            return Error.SymbolNotFound;
        }
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

        const hint = try self.suggestForSymbol(name);
        try self.module.errors.addWithHelp(
            self.current_file.path,
            "Unknown name '{s}'",
            token,
            .err,
            .{name},
            hint,
            null,
        );
        return Error.SymbolNotFound;
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
        chunk.last_op_pos = chunk.instructions.items.len;
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
            if (instructions[i] == jump and std.mem.readVarInt(C.JUMP, instructions[(i + 1)..(i + 1 + @sizeOf(C.JUMP))], .little) == old_pos) {
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
        return self.addNamedConstantTok(name, value, null);
    }

    /// Register a named constant and optionally record the source token that
    /// declared it. Returns `error.SymbolAlreadyDeclared` if a constant with
    /// that name already exists.
    pub fn addNamedConstantTok(self: *Compiler, name: []const u8, value: Value, token: ?Token) !void {
        if (self.constants_map.contains(name)) return error.SymbolAlreadyDeclared;
        const i = try self.addConstant(value);
        const key = try self.alloc.dupe(u8, name);
        errdefer self.alloc.free(key);
        try self.constants_map.putNoClobber(self.alloc, key, i);
        if (token) |t| try self.decl_tokens.put(self.alloc, key, t);
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
