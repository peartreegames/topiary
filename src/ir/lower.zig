//! AST → IR lowering.
//!
//! One walk over the AST. Anchor declarations (bough/fork/choice/function/
//! class/enum) populate `Program.anchors` *as we encounter them*, so
//! backward references resolve eagerly. Forward references — anchor used
//! before its declaration in source order — leave `AnchorRef.kind = null`
//! with `path` holding the writer's relative form. After the walk, a
//! validation pass walks the produced IR (not the AST), maintains its
//! own path_stack from Bough/Fork/Choice nesting, and patches each
//! `kind == null` AnchorRef against the now-complete `Program.anchors`.
//!
//! No prepass. The validation walk is over the IR only — much smaller
//! than the AST and only touches the few nodes that actually need it.
//!
//! Errors are collected on `module.errors`. Lowering tries to keep going
//! past most errors so the caller sees as many issues as possible.

const std = @import("std");

const ir = @import("program.zig");
const Scope = @import("scope.zig").Scope;
const Symbol = @import("scope.zig").Symbol;

const ast = @import("../frontend/ast.zig");
const Token = @import("../frontend/token.zig").Token;

const utils = @import("../utils/index.zig");
const C = utils.C;
const UUID = utils.UUID;

const VarType = @import("../types/var_type.zig").VarType;

const module_mod = @import("../module.zig");
const Module = module_mod.Module;
const File = module_mod.File;

const error_mod = @import("../backend/error.zig");
const CompilerErrors = error_mod.CompilerErrors;

const builtins = @import("../runtime/index.zig").builtins;

pub const Error = error{
    OutOfMemory,
    LowerError,
};

/// Lower a Module's resolved trees into an IR Program.
/// `module.resolveIncludes()` and `File.buildTree()` must already have run.
pub fn lower(parent_alloc: std.mem.Allocator, module: *Module) Error!ir.Program {
    var lowerer = try Lowerer.init(parent_alloc, module);
    defer lowerer.deinitTransient();

    try lowerer.run();
    return lowerer.takeProgram();
}

const Lowerer = struct {
    program: ir.Program,
    scratch: std.heap.ArenaAllocator,

    module: *Module,
    current_file: *File,

    root_scope: *Scope,
    scope: *Scope,

    path_stack: std.ArrayList([]const u8),

    /// Per-walk de-dup of include resolution.
    seen_files: std.array_hash_map.String(void),

    /// Function arity by anchor path. Populated during validateAnchors;
    /// consumed by validateSemantics for the arity-mismatch check.
    arity_by_path: std.array_hash_map.String(u8),
    /// Class metadata by anchor path. Same lifecycle as arity_by_path.
    class_by_path: std.array_hash_map.String(*const ir.ClassDecl),
    /// Enum metadata by anchor path. Same lifecycle as arity_by_path.
    enum_by_path: std.array_hash_map.String(*const ir.EnumDecl),

    fn init(parent_alloc: std.mem.Allocator, module: *Module) !Lowerer {
        var program = ir.Program.init(parent_alloc);
        errdefer program.deinit();

        var scratch = std.heap.ArenaAllocator.init(parent_alloc);
        errdefer scratch.deinit();

        const root_scope = try Scope.create(scratch.allocator(), null, .global);

        return .{
            .program = program,
            .scratch = scratch,
            .module = module,
            .current_file = module.entry,
            .root_scope = root_scope,
            .scope = root_scope,
            .path_stack = .empty,
            .seen_files = .empty,
            .arity_by_path = .empty,
            .class_by_path = .empty,
            .enum_by_path = .empty,
        };
    }

    fn deinitTransient(self: *Lowerer) void {
        self.scratch.deinit();
    }

    fn takeProgram(self: *Lowerer) ir.Program {
        const p = self.program;
        // Reset our copy so deinitTransient doesn't double-free the arena.
        self.program = ir.Program.init(std.heap.page_allocator);
        self.program.deinit();
        return p;
    }

    fn arena(self: *Lowerer) std.mem.Allocator {
        return self.program.allocator();
    }
    fn scratchAlloc(self: *Lowerer) std.mem.Allocator {
        return self.scratch.allocator();
    }
    fn errors(self: *Lowerer) *CompilerErrors {
        return &self.module.errors;
    }
    /// Resolves the file path for a token's `file_index`. Validation passes
    /// can't rely on `current_file` because that points at whatever file
    /// the emit walk last visited; IR nodes live across multiple included
    /// files.
    fn pathForTok(self: *Lowerer, tok: Token) []const u8 {
        if (tok.file_index < self.program.files.len) return self.program.files[tok.file_index];
        return self.current_file.path;
    }

    // =======================================================================
    // Top-level orchestration
    // =======================================================================

    fn run(self: *Lowerer) Error!void {
        const tree = self.current_file.tree orelse return;

        // One walk: emit IR, registering anchors at their declaration sites.
        var body: std.ArrayList(ir.Stmt) = .empty;
        defer body.deinit(self.scratchAlloc());
        for (tree.root) |*s| try self.appendStatement(&body, s);
        self.program.body = try self.arena().dupe(ir.Stmt, body.items);

        self.program.globals_count = @intCast(self.root_scope.count);
        try self.populateFilesTable();

        // Resolve forward references by walking the IR.
        try self.validateAnchors();

        // Run semantic diagnostics over the now fully-resolved IR.
        try self.validateSemantics();
    }

    fn populateFilesTable(self: *Lowerer) Error!void {
        const count = self.module.includes.count();
        const files = try self.arena().alloc([]const u8, count);
        var it = self.module.includes.iterator();
        var i: usize = 0;
        while (it.next()) |kv| : (i += 1) {
            const idx = self.module.includes.getIndex(kv.key_ptr.*) orelse i;
            // Borrow `File.path` directly — it lives in the module's arena,
            // which outlives the Program. These slices are passed into
            // `CompilerErrors`; storing them in the program arena would
            // dangle once a caller deinits the Program before reading
            // `module.errors`.
            files[idx] = kv.value_ptr.*.path;
        }
        self.program.files = files;
    }

    // =======================================================================
    // Emit walk — statements
    // =======================================================================

    fn lowerBody(self: *Lowerer, stmts: []const ast.Statement) Error![]const ir.Stmt {
        var out: std.ArrayList(ir.Stmt) = .empty;
        defer out.deinit(self.scratchAlloc());
        for (stmts) |*s| try self.appendStatement(&out, s);
        return try self.arena().dupe(ir.Stmt, out.items);
    }

    fn appendStatement(
        self: *Lowerer,
        out: *std.ArrayList(ir.Stmt),
        stmt: *const ast.Statement,
    ) Error!void {
        if (stmt.type == .include) {
            const inc = stmt.type.include;
            try out.append(self.scratchAlloc(), .{
                .loc = locFrom(stmt.token),
                .kind = .{ .include = .{
                    .resolved_path = try self.resolveIncludePath(inc.path),
                } },
            });
            const result = (try self.resolveInclude(inc.path)) orelse return;
            const prev = self.current_file;
            defer self.current_file = prev;
            self.current_file = result.file;
            for (result.tree) |*s| try self.appendStatement(out, s);
            return;
        }
        if (stmt.type == .comment) return;

        if (try self.lowerStatement(stmt)) |lowered| {
            try out.append(self.scratchAlloc(), lowered);
        }
    }

    fn lowerStatement(self: *Lowerer, stmt: *const ast.Statement) Error!?ir.Stmt {
        const tok = stmt.token;
        return switch (stmt.type) {
            .dialogue => |d| .{
                .loc = locFrom(tok),
                .kind = .{ .line = .{
                    .uuid = d.id,
                    .speaker = if (d.speaker) |sp| try self.arena().dupe(u8, sp) else null,
                    .segments = try self.lowerStringExpr(d.content),
                    .tags = try self.lowerTags(d.tags),
                } },
            },
            .choice => |c| try self.lowerChoiceStmt(c, tok),
            .fork => |f| try self.lowerForkStmt(f, tok),
            .bough => |b| try self.lowerBoughStmt(b, tok),
            .divert => |d| try self.lowerDivertStmt(d, tok),
            .fin => .{ .loc = locFrom(tok), .kind = .fin },
            .block => |b| .{
                .loc = locFrom(tok),
                .kind = .{ .block = .{ .body = try self.lowerBody(b) } },
            },
            .@"if" => |i| .{
                .loc = locFrom(tok),
                .kind = .{ .@"if" = .{
                    .condition = try self.lowerExpression(i.condition),
                    .then_branch = try self.lowerBody(i.then_branch),
                    .else_branch = if (i.else_branch) |eb| try self.lowerBody(eb) else null,
                } },
            },
            .@"while" => |w| try self.lowerWhileStmt(w, tok),
            .@"for" => |f| try self.lowerForStmt(f, tok),
            .@"switch" => |s| try self.lowerSwitchStmt(s, tok),
            .function => try self.lowerFunctionStmt(stmt),
            .class => |c| try self.lowerClassStmt(c, tok),
            .@"enum" => |e| try self.lowerEnumStmt(e, tok),
            .variable => |v| try self.lowerVariableStmt(v, tok),
            .return_expression => |r| .{
                .loc = locFrom(tok),
                .kind = .{ .return_value = try self.lowerExpression(&r) },
            },
            .return_void => .{ .loc = locFrom(tok), .kind = .return_void },
            .expression => |e| .{
                .loc = locFrom(tok),
                .kind = .{ .expr_stmt = try self.lowerExpression(&e) },
            },
            .@"break" => .{ .loc = locFrom(tok), .kind = .@"break" },
            .@"continue" => .{ .loc = locFrom(tok), .kind = .@"continue" },
            .switch_prong, .comment, .include => null,
        };
    }

    // ---- Statement-specific helpers --------------------------------------

    fn lowerChoiceStmt(self: *Lowerer, c: anytype, tok: Token) Error!ir.Stmt {
        const choice_name = c.name orelse &c.id;
        const path = try self.qualifyName(choice_name);
        const anchor = self.registerAnchor(path, .choice, c.id) catch |e| switch (e) {
            error.SymbolAlreadyDeclared => blk: {
                try self.errors().add(self.current_file.path, "'{s}' is already declared", tok, .err, .{choice_name});
                break :blk ir.AnchorRef{ .kind = .choice, .path = path, .uuid = c.id };
            },
            error.OutOfMemory => return error.OutOfMemory,
        };

        try self.pushPath(choice_name);
        defer self.popPath();

        try self.enterScope(.local);
        defer self.exitScope();

        var body: std.ArrayList(ir.Stmt) = .empty;
        defer body.deinit(self.scratchAlloc());
        try body.append(self.scratchAlloc(), .{
            .loc = locFrom(tok),
            .kind = .{ .visit = .{ .target = anchor } },
        });
        for (c.body) |*s| try self.appendStatement(&body, s);

        return .{
            .loc = locFrom(tok),
            .kind = .{ .choice = .{
                .uuid = c.id,
                .name = if (c.name) |n| try self.arena().dupe(u8, n) else null,
                .segments = try self.lowerStringExpr(&c.content),
                .is_unique = c.is_unique,
                .body = try self.arena().dupe(ir.Stmt, body.items),
                .tags = try self.lowerTags(c.tags),
                .anchor = anchor,
            } },
        };
    }

    fn lowerForkStmt(self: *Lowerer, f: anytype, tok: Token) Error!ir.Stmt {
        if (self.path_stack.items.len == 0) {
            try self.errors().addSpan(self.current_file.path, "fork must be inside a bough", tok, f.end_token, .err, .{});
            return .{ .loc = locFrom(tok), .kind = .fin };
        }
        const fork_name = try self.forkName(f.name, f.id);
        const path = try self.qualifyName(fork_name);
        const anchor = self.registerAnchor(path, .fork, f.id) catch |e| switch (e) {
            error.SymbolAlreadyDeclared => blk: {
                try self.errors().addSpan(self.current_file.path, "'{s}' is already declared", tok, f.end_token, .err, .{fork_name});
                break :blk ir.AnchorRef{ .kind = .fork, .path = path, .uuid = f.id };
            },
            error.OutOfMemory => return error.OutOfMemory,
        };

        try self.pushPath(fork_name);
        defer self.popPath();

        const body = try self.lowerBody(f.body);
        const fork_payload = ir.Fork{
            .uuid = f.id,
            .name = if (f.name) |n| try self.arena().dupe(u8, n) else null,
            .body = body,
            .anchor = anchor,
        };
        return .{
            .loc = locFromSpan(tok, f.end_token),
            .kind = if (f.is_backup) .{ .backup_fork = fork_payload } else .{ .fork = fork_payload },
        };
    }

    fn lowerBoughStmt(self: *Lowerer, b: anytype, tok: Token) Error!ir.Stmt {
        const path = try self.qualifyName(b.name);
        const anchor = self.registerAnchor(path, .bough, b.id) catch |e| switch (e) {
            error.SymbolAlreadyDeclared => blk: {
                try self.errors().addSpan(self.current_file.path, "Bough '{s}' is already declared", tok, b.name_token, .err, .{b.name});
                break :blk ir.AnchorRef{ .kind = .bough, .path = path, .uuid = b.id };
            },
            error.OutOfMemory => return error.OutOfMemory,
        };

        try self.pushPath(b.name);
        defer self.popPath();

        try self.enterScope(.local);
        defer self.exitScope();

        var body: std.ArrayList(ir.Stmt) = .empty;
        defer body.deinit(self.scratchAlloc());
        try body.append(self.scratchAlloc(), .{
            .loc = locFrom(tok),
            .kind = .{ .visit = .{ .target = anchor } },
        });
        for (b.body) |*s| try self.appendStatement(&body, s);

        return .{
            .loc = locFrom(tok),
            .kind = .{ .bough = .{
                .uuid = b.id,
                .name = try self.arena().dupe(u8, b.name),
                .body = try self.arena().dupe(ir.Stmt, body.items),
                .anchor = anchor,
            } },
        };
    }

    fn lowerDivertStmt(self: *Lowerer, d: anytype, tok: Token) Error!ir.Stmt {
        const anchor = try self.refByPath(d.path);
        const payload = ir.Divert{ .target = anchor };
        return .{
            .loc = locFromSpan(tok, d.end_token),
            .kind = if (d.is_backup) .{ .backup_divert = payload } else .{ .divert = payload },
        };
    }

    fn lowerWhileStmt(self: *Lowerer, w: anytype, tok: Token) Error!ir.Stmt {
        try self.enterScope(.local);
        defer self.exitScope();
        return .{
            .loc = locFrom(tok),
            .kind = .{ .@"while" = .{
                .condition = try self.lowerExpression(&w.condition),
                .body = try self.lowerBody(w.body),
            } },
        };
    }

    fn lowerForStmt(self: *Lowerer, f: anytype, tok: Token) Error!ir.Stmt {
        const iterator = try self.lowerExpression(&f.iterator);

        try self.enterScope(.local);
        defer self.exitScope();

        const sym = self.scope.define(self.scratchAlloc(), f.capture, false) catch |e| switch (e) {
            error.SymbolAlreadyDeclared => {
                try self.errors().add(self.current_file.path, "'{s}' is already declared", f.capture_token, .err, .{f.capture});
                return .{ .loc = locFrom(tok), .kind = .fin };
            },
            error.OutOfMemory => return error.OutOfMemory,
        };
        return .{
            .loc = locFrom(tok),
            .kind = .{ .@"for" = .{
                .capture_slot = try self.slotFromSymbol(sym),
                .capture_name = try self.arena().dupe(u8, f.capture),
                .iterator = iterator,
                .body = try self.lowerBody(f.body),
            } },
        };
    }

    fn lowerSwitchStmt(self: *Lowerer, s: anytype, tok: Token) Error!ir.Stmt {
        const capture = try self.lowerExpression(&s.capture);

        const prongs = try self.arena().alloc(ir.Prong, s.prongs.len);
        for (s.prongs, 0..) |prong_stmt, i| {
            const p = prong_stmt.type.switch_prong;

            try self.enterScope(.local);
            defer self.exitScope();

            const values: ?[]const ir.ExprRef = if (p.values) |vals| blk: {
                const refs = try self.arena().alloc(ir.ExprRef, vals.len);
                for (vals, 0..) |*v, j| refs[j] = try self.lowerExpression(v);
                break :blk refs;
            } else null;

            prongs[i] = .{
                .loc = locFrom(prong_stmt.token),
                .values = values,
                .body = try self.lowerBody(p.body),
            };
        }
        return .{
            .loc = locFrom(tok),
            .kind = .{ .@"switch" = .{ .capture = capture, .prongs = prongs } },
        };
    }

    fn lowerFunctionStmt(self: *Lowerer, stmt: *const ast.Statement) Error!ir.Stmt {
        const f = stmt.type.function;
        const tok = stmt.token;

        // Register the function as an anchor in the current path scope —
        // this mirrors `compiler.zig:prepass` which does the same. Top-level
        // and class-method functions are reachable via path; nested functions
        // also bind to a stack slot.
        const path = try self.qualifyName(f.name);
        const anchor_kind: ir.AnchorRef.Kind = if (f.is_extern) .extern_function else .function;
        const anchor: ?ir.AnchorRef = self.registerAnchor(path, anchor_kind, UUID.Empty) catch |e| switch (e) {
            // If the path was already claimed (e.g., a same-named class or
            // sibling), produce an error but keep going with a non-anchored
            // representation.
            error.SymbolAlreadyDeclared => null_blk: {
                try self.errors().addSpan(self.current_file.path, "'{s}' is already declared", tok, f.name_token, .err, .{f.name});
                break :null_blk null;
            },
            error.OutOfMemory => return error.OutOfMemory,
        };

        // Define the symbol in the *current* scope so callers in the same
        // scope can resolve it.
        const fn_sym = self.scope.define(self.scratchAlloc(), f.name, false) catch null;
        if (fn_sym) |s| s.var_type = .function_type;

        try self.enterScope(.function);
        defer self.exitScope();

        // The function's own name binds inside the body for self-recursion.
        const inner_self = self.scope.define(self.scratchAlloc(), f.name, false) catch null;
        if (inner_self) |s| {
            s.tag = .function;
            s.var_type = .function_type;
        }

        const params = try self.arena().alloc(ir.Parameter, f.parameters.len);
        for (f.parameters, 0..) |pname, i| {
            const psym = self.scope.define(self.scratchAlloc(), pname, false) catch |e| switch (e) {
                error.SymbolAlreadyDeclared => {
                    try self.errors().add(self.current_file.path, "Duplicate parameter '{s}'", tok, .err, .{pname});
                    continue;
                },
                error.OutOfMemory => return error.OutOfMemory,
            };
            params[i] = .{
                .name = try self.arena().dupe(u8, pname),
                .slot = try self.slotFromSymbol(psym),
            };
        }

        const body = try self.lowerBody(f.body);

        const is_top_level = (anchor != null);
        const name_slot: ?ir.Slot = if (is_top_level or fn_sym == null) null else try self.slotFromSymbol(fn_sym.?);

        return .{
            .loc = locFrom(tok),
            .kind = .{ .function = .{
                .name = try self.arena().dupe(u8, f.name),
                .name_slot = name_slot,
                .anchor = anchor,
                .is_method = f.is_method,
                .is_extern = f.is_extern,
                .parameters = params,
                .body = body,
            } },
        };
    }

    fn lowerClassStmt(self: *Lowerer, c: anytype, tok: Token) Error!ir.Stmt {
        const path = try self.qualifyName(c.name);
        const anchor = self.registerAnchor(path, .class, UUID.Empty) catch |e| switch (e) {
            error.SymbolAlreadyDeclared => blk: {
                try self.errors().addSpan(self.current_file.path, "'{s}' is already declared", tok, c.name_token, .err, .{c.name});
                break :blk ir.AnchorRef{ .kind = .class, .path = path };
            },
            error.OutOfMemory => return error.OutOfMemory,
        };

        const inits = try self.arena().alloc(ir.ExprRef, c.fields.len);
        for (c.fields, 0..) |*f_expr, i| inits[i] = try self.lowerExpression(f_expr);

        const field_names = try self.arena().alloc([]const u8, c.field_names.len);
        for (c.field_names, 0..) |fn_, i| field_names[i] = try self.arena().dupe(u8, fn_);

        try self.pushPath(c.name);
        defer self.popPath();

        const methods = try self.arena().alloc(ir.FunctionDecl, c.methods.len);
        for (c.methods, 0..) |*m_stmt, i| {
            const lowered = try self.lowerFunctionStmt(m_stmt);
            methods[i] = lowered.kind.function;
        }

        return .{
            .loc = locFrom(tok),
            .kind = .{ .class = .{
                .name = try self.arena().dupe(u8, c.name),
                .anchor = anchor,
                .field_names = field_names,
                .field_initializers = inits,
                .methods = methods,
            } },
        };
    }

    fn lowerEnumStmt(self: *Lowerer, e: anytype, tok: Token) Error!ir.Stmt {
        const path = try self.qualifyName(e.name);
        const anchor = self.registerAnchor(path, .@"enum", UUID.Empty) catch |err| switch (err) {
            error.SymbolAlreadyDeclared => blk: {
                try self.errors().addSpan(self.current_file.path, "'{s}' is already declared", tok, e.name_token, .err, .{e.name});
                break :blk ir.AnchorRef{ .kind = .@"enum", .path = path };
            },
            error.OutOfMemory => return error.OutOfMemory,
        };

        const values = try self.arena().alloc([]const u8, e.values.len);
        for (e.values, 0..) |v, i| values[i] = try self.arena().dupe(u8, v);

        return .{
            .loc = locFrom(tok),
            .kind = .{ .enum_decl = .{
                .name = try self.arena().dupe(u8, e.name),
                .anchor = anchor,
                .is_seq = e.is_seq,
                .values = values,
            } },
        };
    }

    fn lowerVariableStmt(self: *Lowerer, v: anytype, tok: Token) Error!ir.Stmt {
        if (builtins.functions.has(v.name)) {
            try self.errors().addSpan(
                self.current_file.path,
                "'{s}' is a builtin function and cannot be used as a variable name",
                tok,
                v.name_token,
                .err,
                .{v.name},
            );
            return .{ .loc = locFrom(tok), .kind = .fin };
        }
        const sym = self.scope.define(self.scratchAlloc(), v.name, v.is_mutable) catch |e| switch (e) {
            error.SymbolAlreadyDeclared => {
                try self.errors().addSpan(self.current_file.path, "'{s}' is already declared in this scope", tok, v.name_token, .err, .{v.name});
                return .{ .loc = locFrom(tok), .kind = .fin };
            },
            error.OutOfMemory => return error.OutOfMemory,
        };
        sym.var_type = inferExprType(&v.initializer);

        return .{
            .loc = locFrom(tok),
            .kind = .{ .var_decl = .{
                .name = try self.arena().dupe(u8, v.name),
                .slot = try self.slotFromSymbol(sym),
                .is_mutable = v.is_mutable,
                .initializer = try self.lowerExpression(&v.initializer),
            } },
        };
    }

    // =======================================================================
    // Expression lowering
    // =======================================================================

    fn lowerExpression(self: *Lowerer, expr: *const ast.Expression) Error!ir.ExprRef {
        const node = try self.arena().create(ir.Expr);
        const kind = try self.lowerExprKind(expr);
        var var_type = inferExprType(expr);
        if (var_type == .unknown) var_type = loadVarType(kind);
        node.* = .{
            .loc = locFrom(expr.token),
            .var_type = var_type,
            .kind = kind,
        };
        return node;
    }

    /// Identifier loads inherit type from the resolved slot. `inferExprType`
    /// only looks at the AST expression kind, so loads come back `.unknown`
    /// even when the slot already knows its type.
    fn loadVarType(kind: ir.Expr.Kind) VarType {
        return switch (kind) {
            .load => |s| switch (s) {
                .local => |l| l.var_type,
                .upvalue => |u| u.var_type,
                .global => |g| g.var_type,
            },
            // `kind` may be null at lowering time for forward-referenced
            // anchors; validateAnchors patches it later but this load's
            // var_type is already frozen, so forward refs stay `.unknown`
            // (silently permissive — matches Part B's posture).
            .load_const => |lc| blk: {
                const k = lc.target.kind orelse break :blk .unknown;
                break :blk switch (k) {
                    .@"enum" => .{ .enum_type = lc.target.path },
                    .class => .{ .class_type = lc.target.path },
                    .function, .extern_function => .function_type,
                    else => .unknown,
                };
            },
            else => .unknown,
        };
    }

    fn lowerExpressions(self: *Lowerer, exprs: []const ast.Expression) Error![]const ir.ExprRef {
        const out = try self.arena().alloc(ir.ExprRef, exprs.len);
        for (exprs, 0..) |*e, i| out[i] = try self.lowerExpression(e);
        return out;
    }

    fn lowerExprKind(self: *Lowerer, expr: *const ast.Expression) Error!ir.Expr.Kind {
        return switch (expr.type) {
            .number => |n| .{ .number = n },
            .boolean => |b| .{ .bool = b },
            .nil => .nil,
            .string => .{ .text = try self.lowerStringExpr(expr) },
            .list => |l| .{ .list = try self.lowerExpressions(l) },
            .set => |s| .{ .set = try self.lowerExpressions(s) },
            .map => |m| .{ .map = try self.lowerMapPairs(m) },
            .map_pair => unreachable,
            .range => |r| .{ .range = .{
                .left = try self.lowerExpression(r.left),
                .right = try self.lowerExpression(r.right),
            } },
            .unary => |u| .{ .un_op = .{
                .op = u.operator,
                .operand = try self.lowerExpression(u.value),
            } },
            .binary => |b| try self.lowerBinaryOp(b),
            .@"if" => |i| .{ .if_expr = .{
                .condition = try self.lowerExpression(i.condition),
                .then_value = try self.lowerExpression(i.then_value),
                .else_value = try self.lowerExpression(i.else_value),
            } },
            .identifier => |id| try self.lowerIdentifier(id, expr.token),
            .call => |c| .{ .call = .{
                .target = try self.lowerExpression(c.target),
                .arguments = try self.lowerExpressions(c.arguments),
            } },
            .indexer => |idx| blk: {
                if (expr.token.token_type == .dot) {
                    std.debug.assert(idx.index.type == .identifier);
                    break :blk .{ .field = .{
                        .target = try self.lowerExpression(idx.target),
                        .name = idx.index.type.identifier,
                    } };
                }
                break :blk .{ .index = .{
                    .target = try self.lowerExpression(idx.target),
                    .index = try self.lowerExpression(idx.index),
                } };
            },
            .instance => |i| try self.lowerInstance(i, expr.token),
            .@"extern" => .@"extern",
        };
    }

    fn lowerMapPairs(self: *Lowerer, pairs: []const ast.Expression) Error![]const ir.MapPair {
        const out = try self.arena().alloc(ir.MapPair, pairs.len);
        for (pairs, 0..) |*p, i| {
            std.debug.assert(p.type == .map_pair);
            const mp = p.type.map_pair;
            out[i] = .{
                .key = try self.lowerExpression(mp.key),
                .value = try self.lowerExpression(mp.value),
            };
        }
        return out;
    }

    fn lowerBinaryOp(self: *Lowerer, b: anytype) Error!ir.Expr.Kind {
        var target_slot: ?ir.Slot = null;
        if (isAssignOp(b.operator)) {
            try self.validateAssignTarget(b.left);
            if (b.left.type == .identifier) {
                const name = b.left.type.identifier;
                if (try self.resolveSymbol(name)) |sym| {
                    target_slot = try self.slotFromSymbol(sym);
                }
            }
        }
        return .{ .bin_op = .{
            .op = b.operator,
            .left = try self.lowerExpression(b.left),
            .right = try self.lowerExpression(b.right),
            .target_slot = target_slot,
        } };
    }

    /// Diagnose an assignment whose LHS is a constant. The parser already
    /// rejects non-identifier/non-indexer assignment targets, so by the
    /// time we see one here it's been shape-checked.
    fn validateAssignTarget(self: *Lowerer, left: *const ast.Expression) Error!void {
        if (left.type != .identifier) return;
        const name = left.type.identifier;
        if (try self.resolveSymbol(name)) |sym| {
            if (!sym.is_mutable) {
                try self.errors().add(
                    self.current_file.path,
                    "Cannot assign to constant variable '{s}'",
                    left.token,
                    .err,
                    .{name},
                );
            }
            return;
        }
        if (self.program.anchors.contains(name)) {
            try self.errors().add(
                self.current_file.path,
                "Cannot assign to constant '{s}'",
                left.token,
                .err,
                .{name},
            );
        }
    }

    fn lowerIdentifier(self: *Lowerer, name: []const u8, tok: Token) Error!ir.Expr.Kind {
        _ = tok;
        if (try self.resolveSymbol(name)) |sym| return .{ .load = try self.slotFromSymbol(sym) };
        // Could be a constant / anchor — single-segment path.
        const single = [_][]const u8{name};
        const anchor = try self.refByPath(&single);
        return .{ .load_const = .{ .target = anchor } };
    }

    fn lowerInstance(self: *Lowerer, i: anytype, tok: Token) Error!ir.Expr.Kind {
        _ = tok;
        const single = [_][]const u8{i.name};
        const class_anchor = try self.refByPath(&single);

        const fields = try self.arena().alloc(ir.ExprRef, i.fields.len);
        for (i.fields, 0..) |*f_expr, idx| fields[idx] = try self.lowerExpression(f_expr);

        const field_names = try self.arena().alloc([]const u8, i.field_names.len);
        for (i.field_names, 0..) |n, idx| field_names[idx] = try self.arena().dupe(u8, n);

        return .{ .instance = .{
            .class = class_anchor,
            .field_names = field_names,
            .fields = fields,
        } };
    }

    // =======================================================================
    // Strings & tags
    // =======================================================================

    fn lowerStringExpr(self: *Lowerer, expr: *const ast.Expression) Error![]const ir.TextSegment {
        const s = expr.type.string;
        var segs: std.ArrayList(ir.TextSegment) = .empty;
        defer segs.deinit(self.scratchAlloc());

        var literal: std.ArrayList(u8) = .empty;
        defer literal.deinit(self.scratchAlloc());

        var i: usize = 0;
        while (i < s.value.len) {
            const c = s.value[i];
            if (c == '\\' and i + 1 < s.value.len) {
                const escaped: u8 = switch (s.value[i + 1]) {
                    'n' => '\n',
                    't' => '\t',
                    'r' => '\r',
                    '\\' => '\\',
                    '"' => '"',
                    '{' => '{',
                    '}' => '}',
                    else => s.value[i + 1],
                };
                try literal.append(self.scratchAlloc(), escaped);
                i += 2;
                continue;
            }
            if (c == '{') {
                if (literal.items.len > 0) {
                    try segs.append(self.scratchAlloc(), .{
                        .literal = try self.arena().dupe(u8, literal.items),
                    });
                    literal.clearRetainingCapacity();
                }
                var j = i + 1;
                var idx: usize = 0;
                while (j < s.value.len and s.value[j] >= '0' and s.value[j] <= '9') : (j += 1) {
                    idx = idx * 10 + @as(usize, s.value[j] - '0');
                }
                if (j < s.value.len and s.value[j] == '}' and idx < s.expressions.len) {
                    try segs.append(self.scratchAlloc(), .{
                        .interp = try self.lowerExpression(&s.expressions[idx]),
                    });
                    i = j + 1;
                    continue;
                }
                try literal.append(self.scratchAlloc(), c);
                i += 1;
                continue;
            }
            try literal.append(self.scratchAlloc(), c);
            i += 1;
        }
        if (literal.items.len > 0) {
            try segs.append(self.scratchAlloc(), .{
                .literal = try self.arena().dupe(u8, literal.items),
            });
        }
        return try self.arena().dupe(ir.TextSegment, segs.items);
    }

    fn lowerTags(self: *Lowerer, tags: []const ast.Tag) Error![]const ir.Tag {
        if (tags.len == 0) return &.{};
        const out = try self.arena().alloc(ir.Tag, tags.len);
        for (tags, 0..) |t, i| out[i] = .{
            .name = try self.arena().dupe(u8, t.name),
            .loc = locFrom(t.token),
        };
        return out;
    }

    // =======================================================================
    // Anchors — path resolution
    // =======================================================================

    /// Build an `AnchorRef` for a reference to anchor `parts` at the
    /// current source position.
    /// - If a registered anchor exists at the longest viable qualification,
    ///   the returned ref has `kind` set and `path` fully qualified.
    /// - If no match yet (forward reference), the returned ref has
    ///   `kind = null` and `path = parts.join(".")` (the writer's
    ///   relative form). The post-walk validator patches it.
    fn refByPath(self: *Lowerer, parts: []const []const u8) Error!ir.AnchorRef {
        // Eager: try progressive shorten against current path_stack.
        var i: usize = self.path_stack.items.len;
        while (i > 0) : (i -= 1) {
            const candidate = try self.joinScopedPath(self.path_stack.items[0..i], parts);
            if (self.program.anchors.get(candidate)) |a| return a;
        }
        const bare = try std.mem.join(self.arena(), ".", parts);
        if (self.program.anchors.get(bare)) |a| return a;

        // Forward reference. Stash the writer's relative form.
        return .{ .kind = null, .path = bare };
    }

    /// Joins `prefix` and `suffix` segments with ".". The result lives in
    /// the scratch arena: callers that store it long-term must dupe it
    /// into the program arena themselves.
    fn joinScopedPath(
        self: *Lowerer,
        prefix: []const []const u8,
        suffix: []const []const u8,
    ) Error![]const u8 {
        const all = try self.scratchAlloc().alloc([]const u8, prefix.len + suffix.len);
        @memcpy(all[0..prefix.len], prefix);
        @memcpy(all[prefix.len..], suffix);
        return try std.mem.join(self.scratchAlloc(), ".", all);
    }

    /// Insert an anchor into `program.anchors`. Returns the canonical
    /// `AnchorRef`. Returns `error.SymbolAlreadyDeclared` if the path is
    /// already taken.
    fn registerAnchor(
        self: *Lowerer,
        path: []const u8,
        kind: ir.AnchorRef.Kind,
        uuid: UUID.ID,
    ) !ir.AnchorRef {
        const gop = try self.program.anchors.getOrPut(self.arena(), path);
        if (gop.found_existing) return error.SymbolAlreadyDeclared;
        gop.value_ptr.* = .{ .kind = kind, .path = path, .uuid = uuid };
        return gop.value_ptr.*;
    }

    fn qualifyName(self: *Lowerer, name: []const u8) Error![]const u8 {
        if (self.path_stack.items.len == 0) return try self.arena().dupe(u8, name);
        const prefix = try std.mem.join(self.scratchAlloc(), ".", self.path_stack.items);
        return try std.fmt.allocPrint(self.arena(), "{s}.{s}", .{ prefix, name });
    }

    fn pushPath(self: *Lowerer, name: []const u8) Error!void {
        try self.path_stack.append(self.scratchAlloc(), name);
    }
    fn popPath(self: *Lowerer) void {
        _ = self.path_stack.pop();
    }

    fn forkName(self: *Lowerer, name: ?[]const u8, id: UUID.ID) Error![]const u8 {
        if (name) |n| return n;
        return try self.scratchAlloc().dupe(u8, &id);
    }

    // =======================================================================
    // Anchor validation pass — patches forward refs on the produced IR
    // =======================================================================

    /// Walk the just-emitted IR. For every `AnchorRef` with `kind == null`
    /// (a forward reference recorded during the main walk), do a progressive-
    /// shorten lookup against `program.anchors` using the IR's nesting as
    /// the path_stack. Patch `kind`/`path` in place. Emit a CompilerErr
    /// for refs that still don't resolve.
    fn validateAnchors(self: *Lowerer) Error!void {
        var stack: std.ArrayList([]const u8) = .empty;
        defer stack.deinit(self.scratchAlloc());
        try self.validateBody(self.program.body, &stack);
    }

    fn validateBody(
        self: *Lowerer,
        stmts: []const ir.Stmt,
        stack: *std.ArrayList([]const u8),
    ) Error!void {
        for (stmts) |*s| try self.validateStmt(s, stack);
    }

    fn validateStmt(
        self: *Lowerer,
        stmt: *const ir.Stmt,
        stack: *std.ArrayList([]const u8),
    ) Error!void {
        const stmt_mut: *ir.Stmt = @constCast(stmt);
        switch (stmt_mut.kind) {
            .line => |*l| {
                for (l.segments) |seg| try self.validateSegment(seg, stack);
            },
            .choice => |*c| {
                try self.patchAnchor(&c.anchor, stack, stmt.loc.start);
                for (c.segments) |seg| try self.validateSegment(seg, stack);
                try stack.append(self.scratchAlloc(), c.anchor.path);
                defer _ = stack.pop();
                try self.validateBody(c.body, stack);
            },
            .fork, .backup_fork => |*f| {
                try self.patchAnchor(&f.anchor, stack, stmt.loc.start);
                try stack.append(self.scratchAlloc(), f.anchor.path);
                defer _ = stack.pop();
                try self.validateBody(f.body, stack);
            },
            .divert, .backup_divert => |*d| {
                try self.patchAnchor(&d.target, stack, stmt.loc.start);
            },
            .bough => |*b| {
                try self.patchAnchor(&b.anchor, stack, stmt.loc.start);
                try stack.append(self.scratchAlloc(), b.anchor.path);
                defer _ = stack.pop();
                try self.validateBody(b.body, stack);
            },
            .visit => |*v| try self.patchAnchor(&v.target, stack, stmt.loc.start),
            .class => |*c| {
                try self.patchAnchor(&c.anchor, stack, stmt.loc.start);
                if (c.anchor.kind != null) try self.class_by_path.put(self.scratchAlloc(), c.anchor.path, c);
                for (c.field_initializers) |e| try self.validateExpr(e, stack);
                try stack.append(self.scratchAlloc(), c.anchor.path);
                defer _ = stack.pop();
                for (c.methods) |*m| try self.validateFunction(@constCast(m), stack, stmt.loc.start);
            },
            .enum_decl => |*e| {
                try self.patchAnchor(&e.anchor, stack, stmt.loc.start);
                if (e.anchor.kind != null)
                    try self.enum_by_path.put(self.scratchAlloc(), e.anchor.path, e);
            },
            .block => |*b| try self.validateBody(b.body, stack),
            .@"if" => |*i| {
                try self.validateExpr(i.condition, stack);
                try self.validateBody(i.then_branch, stack);
                if (i.else_branch) |eb| try self.validateBody(eb, stack);
            },
            .@"while" => |*w| {
                try self.validateExpr(w.condition, stack);
                try self.validateBody(w.body, stack);
            },
            .@"for" => |*f| {
                try self.validateExpr(f.iterator, stack);
                try self.validateBody(f.body, stack);
            },
            .@"switch" => |*sw| {
                try self.validateExpr(sw.capture, stack);
                for (sw.prongs) |*p| {
                    if (p.values) |vs| for (vs) |e| try self.validateExpr(e, stack);
                    try self.validateBody(p.body, stack);
                }
            },
            .function => |*f| try self.validateFunction(f, stack, stmt.loc.start),
            .return_value, .expr_stmt => |e| try self.validateExpr(e, stack),
            .var_decl => |*v| try self.validateExpr(v.initializer, stack),
            .return_void, .@"break", .@"continue", .fin, .include => {},
        }
    }

    fn validateFunction(
        self: *Lowerer,
        f: *ir.FunctionDecl,
        stack: *std.ArrayList([]const u8),
        tok: Token,
    ) Error!void {
        if (f.anchor) |*a| {
            try self.patchAnchor(@constCast(a), stack, tok);
            if (a.kind != null) {
                try self.arity_by_path.put(self.scratchAlloc(), a.path, @intCast(f.parameters.len));
            }
        }
        try self.validateBody(f.body, stack);
    }

    fn validateSegment(
        self: *Lowerer,
        seg: ir.TextSegment,
        stack: *std.ArrayList([]const u8),
    ) Error!void {
        switch (seg) {
            .literal => {},
            .interp => |e| try self.validateExpr(e, stack),
        }
    }

    fn validateExpr(
        self: *Lowerer,
        expr: ir.ExprRef,
        stack: *std.ArrayList([]const u8),
    ) Error!void {
        const e: *ir.Expr = @constCast(expr);
        switch (e.kind) {
            .number, .bool, .nil, .@"extern", .load => {},
            .load_const => |*lc| try self.patchAnchor(&lc.target, stack, e.loc.start),
            .text => |segs| for (segs) |seg| try self.validateSegment(seg, stack),
            .list => |xs| for (xs) |x| try self.validateExpr(x, stack),
            .set => |xs| for (xs) |x| try self.validateExpr(x, stack),
            .map => |pairs| for (pairs) |p| {
                try self.validateExpr(p.key, stack);
                try self.validateExpr(p.value, stack);
            },
            .range => |*r| {
                try self.validateExpr(r.left, stack);
                try self.validateExpr(r.right, stack);
            },
            .bin_op => |*b| {
                try self.validateExpr(b.left, stack);
                try self.validateExpr(b.right, stack);
            },
            .un_op => |*u| try self.validateExpr(u.operand, stack),
            .if_expr => |*ie| {
                try self.validateExpr(ie.condition, stack);
                try self.validateExpr(ie.then_value, stack);
                try self.validateExpr(ie.else_value, stack);
            },
            .index => |*i| {
                try self.validateExpr(i.target, stack);
                try self.validateExpr(i.index, stack);
            },
            .field => |*f| try self.validateExpr(f.target, stack),
            .call => |*c| {
                try self.validateExpr(c.target, stack);
                for (c.arguments) |a| try self.validateExpr(a, stack);
            },
            .instance => |*ins| {
                try self.patchAnchor(&ins.class, stack, e.loc.start);
                for (ins.fields) |fe| try self.validateExpr(fe, stack);
            },
            .builtin => |*bi| for (bi.arguments) |a| try self.validateExpr(a, stack),
        }
    }

    fn patchAnchor(
        self: *Lowerer,
        anchor: *ir.AnchorRef,
        stack: *std.ArrayList([]const u8),
        tok: Token,
    ) Error!void {
        if (anchor.kind != null) return;
        // anchor.path holds the writer's relative form (joined with ".").
        // Try progressive shorten against current scope.
        const writer_path = anchor.path;
        var parts = std.ArrayList([]const u8).empty;
        defer parts.deinit(self.scratchAlloc());
        var it = std.mem.splitScalar(u8, writer_path, '.');
        while (it.next()) |p| try parts.append(self.scratchAlloc(), p);

        var i: usize = stack.items.len;
        while (i > 0) : (i -= 1) {
            const candidate = try self.joinScopedPath(stack.items[0..i], parts.items);
            if (self.program.anchors.get(candidate)) |a| {
                anchor.* = a;
                return;
            }
        }
        if (self.program.anchors.get(writer_path)) |a| {
            anchor.* = a;
            return;
        }

        try self.errors().add(self.pathForTok(tok), "Unknown name '{s}'", tok, .err, .{writer_path});
    }

    // =======================================================================
    // Semantic validation pass — diagnostics that need fully-resolved IR
    // =======================================================================

    /// Walk the now fully-resolved IR to apply semantic checks: arity
    /// mismatch, instance-field validation, unreachable code, and
    /// fork-has-no-exit. Reads `self.arity_by_path` and `self.class_by_path`,
    /// which `validateAnchors` populated during its own walk.
    fn validateSemantics(self: *Lowerer) Error!void {
        var stack: std.ArrayList([]const u8) = .empty;
        defer stack.deinit(self.scratchAlloc());
        try self.semBody(self.program.body, &stack);
    }

    fn semBody(
        self: *Lowerer,
        stmts: []const ir.Stmt,
        stack: *std.ArrayList([]const u8),
    ) Error!void {
        try self.checkUnreachable(stmts);
        for (stmts) |*s| try self.semStmt(s, stack);
    }

    fn semStmt(
        self: *Lowerer,
        stmt: *const ir.Stmt,
        stack: *std.ArrayList([]const u8),
    ) Error!void {
        const a = self.scratchAlloc();
        switch (stmt.kind) {
            .line => |*l| {
                for (l.segments) |seg| try self.semSegment(seg, stack);
            },
            .choice => |*c| {
                for (c.segments) |seg| try self.semSegment(seg, stack);
                try stack.append(a, c.anchor.path);
                defer _ = stack.pop();
                try self.semBody(c.body, stack);
            },
            .fork => |*f| {
                // Non-backup fork: warn on each choice with no exit.
                for (f.body) |*body_stmt| {
                    if (body_stmt.kind == .choice) {
                        const choice = body_stmt.kind.choice;
                        if (!blockExits(choice.body)) {
                            try self.errors().addWithHelp(
                                self.pathForTok(body_stmt.loc.start),
                                "choice has no divert or 'fin' -- execution will end silently after this choice",
                                body_stmt.loc.start,
                                .warn,
                                .{},
                                try self.errors().allocator.dupe(u8, "use 'fork^' to continue after the choice, or add a divert '=>' inside the choice body"),
                                null,
                            );
                        }
                    }
                }
                try stack.append(a, f.anchor.path);
                defer _ = stack.pop();
                try self.semBody(f.body, stack);
            },
            .backup_fork => |*f| {
                try stack.append(a, f.anchor.path);
                defer _ = stack.pop();
                try self.semBody(f.body, stack);
            },
            .bough => |*b| {
                try stack.append(a, b.anchor.path);
                defer _ = stack.pop();
                try self.semBody(b.body, stack);
            },
            .class => |*c| {
                for (c.field_initializers) |e| {
                    try self.checkNotFunctionRef(e, e.loc.start);
                    try self.checkStaticInitializer(e);
                    try self.semExpr(e, stack);
                }
                try stack.append(a, c.anchor.path);
                defer _ = stack.pop();
                for (c.methods) |*m| try self.semBody(m.body, stack);
            },
            .function => |*f| try self.semBody(f.body, stack),
            .block => |*b| try self.semBody(b.body, stack),
            .@"if" => |*i| {
                try self.semExpr(i.condition, stack);
                try self.semBody(i.then_branch, stack);
                if (i.else_branch) |eb| try self.semBody(eb, stack);
            },
            .@"while" => |*w| {
                try self.semExpr(w.condition, stack);
                try self.semBody(w.body, stack);
            },
            .@"for" => |*f| {
                try self.semExpr(f.iterator, stack);
                try self.semBody(f.body, stack);
            },
            .@"switch" => |*sw| {
                try self.semExpr(sw.capture, stack);
                for (sw.prongs) |*p| {
                    if (p.values) |vs| for (vs) |e| try self.semExpr(e, stack);
                    try self.semBody(p.body, stack);
                }
            },
            .return_value => |e| {
                try self.checkNotFunctionRef(e, e.loc.start);
                try self.semExpr(e, stack);
            },
            .expr_stmt => |e| try self.semExpr(e, stack),
            .var_decl => |*v| {
                try self.checkNotFunctionRef(v.initializer, v.initializer.loc.start);
                try self.semExpr(v.initializer, stack);
            },
            .divert, .backup_divert, .visit, .enum_decl,
            .return_void, .@"break", .@"continue", .fin, .include,
            => {},
        }
    }

    fn semSegment(
        self: *Lowerer,
        seg: ir.TextSegment,
        stack: *std.ArrayList([]const u8),
    ) Error!void {
        switch (seg) {
            .literal => {},
            .interp => |e| try self.semExpr(e, stack),
        }
    }

    fn semExpr(
        self: *Lowerer,
        expr: ir.ExprRef,
        stack: *std.ArrayList([]const u8),
    ) Error!void {
        const e: *const ir.Expr = expr;
        switch (e.kind) {
            .number, .bool, .nil, .@"extern", .load, .load_const => {},
            .text => |segs| for (segs) |seg| try self.semSegment(seg, stack),
            .list => |xs| for (xs) |x| {
                try self.checkNotFunctionRef(x, x.loc.start);
                try self.semExpr(x, stack);
            },
            .set => |xs| for (xs) |x| {
                try self.checkNotFunctionRef(x, x.loc.start);
                try self.semExpr(x, stack);
            },
            .map => |pairs| for (pairs) |p| {
                try self.semExpr(p.key, stack);
                try self.checkNotFunctionRef(p.value, p.value.loc.start);
                try self.semExpr(p.value, stack);
            },
            .range => |*r| {
                try self.semExpr(r.left, stack);
                try self.semExpr(r.right, stack);
            },
            .bin_op => |*b| {
                if (isAssignOp(b.op))
                    try self.checkNotFunctionRef(b.right, b.right.loc.start);
                try self.semExpr(b.left, stack);
                try self.semExpr(b.right, stack);
            },
            .un_op => |*u| try self.semExpr(u.operand, stack),
            .if_expr => |*ie| {
                try self.semExpr(ie.condition, stack);
                try self.semExpr(ie.then_value, stack);
                try self.semExpr(ie.else_value, stack);
            },
            .index => |*i| {
                try self.semExpr(i.target, stack);
                try self.semExpr(i.index, stack);
            },
            .field => |*f| {
                try self.checkFieldAccess(f, e.loc.start);
                try self.semExpr(f.target, stack);
            },
            .call => |*c| {
                try self.checkCallArity(c, stack, e.loc.start);
                try self.semExpr(c.target, stack);
                for (c.arguments) |arg| try self.semExpr(arg, stack);
            },
            .instance => |*ins| {
                try self.checkInstanceFields(ins, e.loc.start);
                for (ins.fields) |fe| {
                    try self.checkNotFunctionRef(fe, fe.loc.start);
                    try self.semExpr(fe, stack);
                }
            },
            .builtin => |*bi| for (bi.arguments) |a| try self.semExpr(a, stack),
        }
    }

    fn checkCallArity(
        self: *Lowerer,
        call: *const ir.Call,
        stack: *std.ArrayList([]const u8),
        tok: Token,
    ) Error!void {
        const name = callTargetName(call.target) orelse return;
        const arity = self.lookupArity(name, stack.items) orelse return;
        if (arity != call.arguments.len) {
            try self.errors().add(
                self.pathForTok(tok),
                "'{s}' expects {d} argument(s), but got {d}",
                tok,
                .err,
                .{ name, arity, call.arguments.len },
            );
        }
    }

    fn lookupArity(self: *Lowerer, name: []const u8, stack: []const []const u8) ?u8 {
        var i: usize = stack.len;
        while (i > 0) : (i -= 1) {
            const candidate = self.joinScopedPath(stack[0..i], &.{name}) catch return null;
            if (self.arity_by_path.get(candidate)) |a| return a;
        }
        return self.arity_by_path.get(name);
    }

    fn checkInstanceFields(
        self: *Lowerer,
        ins: *const ir.Instance,
        tok: Token,
    ) Error!void {
        if (ins.class.kind == null) return; // unresolved; reported elsewhere
        const class = self.class_by_path.get(ins.class.path) orelse return;
        for (ins.field_names) |fname| {
            if (!classHasField(class, fname)) {
                try self.errors().add(
                    self.pathForTok(tok),
                    "Class '{s}' has no field '{s}'",
                    tok,
                    .err,
                    .{ class.name, fname },
                );
            }
        }
    }

    /// Validate `obj.field` against the target's resolved type. Mirrors
    /// the AST-walking `compiler.zig:validateDotAccess` behavior:
    /// instance/string/collection get name-table checks; primitives are
    /// rejected outright; `.unknown` is silently permissive (chained
    /// access, call returns, etc.).
    fn checkFieldAccess(
        self: *Lowerer,
        f: *const ir.Field,
        tok: Token,
    ) Error!void {
        switch (f.target.var_type) {
            .instance => |class_name| {
                const class = self.class_by_path.get(class_name) orelse return;
                if (classHasField(class, f.name)) return;
                if (classHasMethod(class, f.name)) return;
                try self.errors().add(
                    self.pathForTok(tok),
                    "Class '{s}' does not contain a field '{s}'",
                    tok,
                    .err,
                    .{ class.name, f.name },
                );
            },
            .string => {
                if (builtins.string_methods.has(f.name)) return;
                try self.errors().add(
                    self.pathForTok(tok),
                    "Unknown method '{s}' on string",
                    tok,
                    .err,
                    .{f.name},
                );
            },
            .list, .set, .map => {
                if (builtins.collection_methods.has(f.name)) return;
                const type_name: []const u8 = switch (f.target.var_type) {
                    .list => "list",
                    .set => "set",
                    .map => "map",
                    else => unreachable,
                };
                try self.errors().add(
                    self.pathForTok(tok),
                    "Unknown method '{s}' on {s}",
                    tok,
                    .err,
                    .{ f.name, type_name },
                );
            },
            .number, .boolean, .nil => {
                const type_name: []const u8 = switch (f.target.var_type) {
                    .number => "number",
                    .boolean => "boolean",
                    .nil => "nil",
                    else => unreachable,
                };
                try self.errors().add(
                    self.pathForTok(tok),
                    "Cannot access field '{s}' on a {s}",
                    tok,
                    .err,
                    .{ f.name, type_name },
                );
            },
            .enum_type => |path| {
                const e = self.enum_by_path.get(path) orelse return;
                for (e.values) |v| if (std.mem.eql(u8, v, f.name)) return;
                try self.errors().add(
                    self.pathForTok(tok),
                    "Enum '{s}' does not contain a value '{s}'",
                    tok,
                    .err,
                    .{ e.name, f.name },
                );
            },
            .class_type => |path| {
                const c = self.class_by_path.get(path) orelse return;
                if (classHasField(c, f.name)) return;
                if (classHasMethod(c, f.name)) return;
                try self.errors().add(
                    self.pathForTok(tok),
                    "Class '{s}' does not contain a field '{s}'",
                    tok,
                    .err,
                    .{ c.name, f.name },
                );
            },
            .function_type => {
                try self.errors().add(
                    self.pathForTok(tok),
                    "Cannot access field '{s}' on a function",
                    tok,
                    .err,
                    .{f.name},
                );
            },
            .unknown => {},
        }
    }

    /// True when `expr` evaluates to a function value: a bare reference
    /// to a top-level function or extern fn, or a method-as-value
    /// (`Class.method` / `instance.method` without a call). Used to
    /// reject storing such values, since `runtime/state.zig` can't
    /// round-trip them through save/load.
    fn isFunctionRef(self: *const Lowerer, expr: ir.ExprRef) bool {
        if (expr.var_type == .function_type) return true;
        return switch (expr.kind) {
            .field => |f| blk: {
                const class_path = switch (f.target.var_type) {
                    .instance => |p| p,
                    .class_type => |p| p,
                    else => break :blk false,
                };
                const class = self.class_by_path.get(class_path) orelse break :blk false;
                break :blk classHasMethod(class, f.name);
            },
            else => false,
        };
    }

    /// Emit a diagnostic if `expr` would store a function as a value.
    /// Allowed positions (call target, call argument) skip this check.
    fn checkNotFunctionRef(self: *Lowerer, expr: ir.ExprRef, tok: Token) Error!void {
        if (!self.isFunctionRef(expr)) return;
        const fn_note = try self.errors().allocator.dupe(u8, "functions are not preserved across save/load — use them only as call targets or call arguments");
        const method_note = "methods are not preserved across save/load — call them directly instead";
        switch (expr.kind) {
            .load_const => |lc| try self.errors().addWithHelp(
                self.pathForTok(tok),
                "Cannot store function '{s}' as a value",
                tok,
                .err,
                .{lc.target.path},
                null,
                fn_note,
            ),
            .load => |s| {
                const name = switch (s) {
                    .local => |l| l.name,
                    .upvalue => |u| u.name,
                    .global => |g| g.name,
                };
                try self.errors().addWithHelp(
                    self.pathForTok(tok),
                    "Cannot store function '{s}' as a value",
                    tok,
                    .err,
                    .{name},
                    null,
                    fn_note,
                );
            },
            .field => |f| {
                self.errors().allocator.free(fn_note);
                try self.errors().addWithHelp(
                    self.pathForTok(tok),
                    "Cannot store method '{s}' as a value",
                    tok,
                    .err,
                    .{f.name},
                    null,
                    try self.errors().allocator.dupe(u8, method_note),
                );
            },
            else => {
                self.errors().allocator.free(fn_note);
                unreachable;
            },
        }
    }

    /// Reject class field initializers that aren't a literal-shape
    /// expression (numbers, bools, nil, non-interpolated strings,
    /// recursive lists/sets/maps/ranges of literals, arithmetic on
    /// numbers, references to other compile-time constants, enum/class
    /// dot-access, or `new C{}`). The actual evaluation into a runtime
    /// `Value` stays in codegen — this is a structural pre-check only.
    fn checkStaticInitializer(self: *Lowerer, expr: ir.ExprRef) Error!void {
        switch (expr.kind) {
            // Literals + references to compile-time constants.
            .number, .bool, .nil, .load_const => {},
            .text => |segs| for (segs) |seg| switch (seg) {
                .literal => {},
                .interp => {
                    try self.errors().add(
                        self.pathForTok(expr.loc.start),
                        "Interpolated strings are not allowed as static default values",
                        expr.loc.start,
                        .err,
                        .{},
                    );
                    return;
                },
            },
            // Recursive cases.
            .list => |xs| for (xs) |x| try self.checkStaticInitializer(x),
            .set => |xs| for (xs) |x| try self.checkStaticInitializer(x),
            .map => |pairs| for (pairs) |p| {
                try self.checkStaticInitializer(p.key);
                try self.checkStaticInitializer(p.value);
            },
            .range => |r| {
                try self.checkStaticInitializer(r.left);
                try self.checkStaticInitializer(r.right);
            },
            .bin_op => |b| {
                try self.checkStaticInitializer(b.left);
                try self.checkStaticInitializer(b.right);
            },
            .un_op => |u| try self.checkStaticInitializer(u.operand),
            // `Enum.Value` / `Class.field` — recurse into target.
            .field => |f| try self.checkStaticInitializer(f.target),
            // `new C{...}` — recurse into each field.
            .instance => |ins| for (ins.fields) |fe| try self.checkStaticInitializer(fe),
            // Everything else is non-static: variable loads, calls,
            // computed indexers, if-expressions, builtins.
            .load, .if_expr, .index, .call, .builtin, .@"extern" => {
                try self.errors().addWithHelp(
                    self.pathForTok(expr.loc.start),
                    "Only literal values are allowed here",
                    expr.loc.start,
                    .err,
                    .{},
                    null,
                    try self.errors().allocator.dupe(u8, "class field defaults must be numbers, strings, bools, nil, or lists/sets/maps of literals"),
                );
            },
        }
    }

    fn checkUnreachable(self: *Lowerer, stmts: []const ir.Stmt) Error!void {
        var exited_at: ?usize = null;
        for (stmts, 0..) |*s, i| {
            if (exited_at == null and statementExits(s)) {
                exited_at = i;
                break;
            }
        }
        if (exited_at) |i| {
            for (stmts[i + 1 ..]) |*next| {
                if (next.kind != .bough) {
                    const note: ?[]const u8 = switch (stmts[i].kind) {
                        .@"if" => try self.errors().allocator.dupe(u8, "all branches of this 'if' exit"),
                        .@"switch" => try self.errors().allocator.dupe(u8, "all prongs of this 'switch' exit"),
                        else => null,
                    };
                    try self.errors().addWithHelp(
                        self.pathForTok(next.loc.start),
                        "Unreachable code after '{s}'",
                        next.loc.start,
                        .warn,
                        .{exitKeyword(&stmts[i])},
                        null,
                        note,
                    );
                    break;
                }
            }
        }
    }

    // =======================================================================
    // Scope wrappers
    // =======================================================================

    fn enterScope(self: *Lowerer, tag: Scope.Tag) Error!void {
        self.scope = try Scope.create(self.scratchAlloc(), self.scope, tag);
    }
    fn exitScope(self: *Lowerer) void {
        self.scope = self.scope.parent orelse self.scope;
    }

    fn resolveSymbol(self: *Lowerer, name: []const u8) Error!?*Symbol {
        return self.scope.resolve(self.scratchAlloc(), name);
    }

    /// Build the IR slot descriptor for a resolved scope symbol. Used
    /// both at declaration sites (as `VarDecl.slot`) and at use sites
    /// (wrapped in `Expr.Kind.load`). `sym.name` is duped into the IR
    /// arena so the slot outlives the scratch-lifetime symbol.
    fn slotFromSymbol(self: *Lowerer, sym: *Symbol) Error!ir.Slot {
        const name = try self.arena().dupe(u8, sym.name);
        return switch (sym.tag) {
            .local, .function => .{ .local = .{
                .index = @intCast(sym.index),
                .is_mutable = sym.is_mutable,
                .var_type = sym.var_type,
                .name = name,
            } },
            .upvalue => .{ .upvalue = .{
                .index = @intCast(sym.index),
                .depth = self.calculateScopeDepth(sym),
                .is_mutable = sym.is_mutable,
                .var_type = sym.var_type,
                .name = name,
            } },
            .global => .{ .global = .{
                .index = @intCast(sym.index),
                .is_mutable = sym.is_mutable,
                .var_type = sym.var_type,
                .name = name,
            } },
        };
    }

    fn calculateScopeDepth(self: *Lowerer, sym: *Symbol) u8 {
        var depth: u8 = 0;
        var cur: ?*Scope = self.scope;
        while (cur) |s| : (cur = s.parent) {
            if (s.tag == .function) depth += 1;
            if (s.symbols.get(sym.name) != null) return depth;
        }
        return depth;
    }

    // =======================================================================
    // Includes
    // =======================================================================

    const IncludeResult = struct {
        file: *File,
        tree: []const ast.Statement,
    };

    fn resolveInclude(self: *Lowerer, raw_path: []const u8) Error!?IncludeResult {
        const resolved = self.module.resolveIncludePath(self.current_file, raw_path) catch
            return null;
        const file = self.module.includes.get(resolved) orelse return null;
        const gop = try self.seen_files.getOrPut(self.scratchAlloc(), resolved);
        if (gop.found_existing) return null;
        const tree = file.tree orelse return null;
        return .{ .file = file, .tree = tree.root };
    }

    fn resolveIncludePath(self: *Lowerer, raw_path: []const u8) Error![]const u8 {
        const resolved = self.module.resolveIncludePath(self.current_file, raw_path) catch
            return try self.arena().dupe(u8, raw_path);
        return try self.arena().dupe(u8, resolved);
    }
};

// ===========================================================================
// Helpers
// ===========================================================================

fn locFrom(tok: Token) ir.Loc {
    return .{ .file_index = tok.file_index, .start = tok };
}

fn locFromSpan(start: Token, end: Token) ir.Loc {
    return .{ .file_index = start.file_index, .start = start, .end = end };
}

fn inferExprType(expr: *const ast.Expression) VarType {
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

fn statementExits(stmt: *const ir.Stmt) bool {
    return switch (stmt.kind) {
        .return_value, .return_void, .fin, .fork, .divert => true,
        .backup_fork, .backup_divert => false,
        .@"if" => |i| i.else_branch != null and
            blockExits(i.then_branch) and
            blockExits(i.else_branch.?),
        .@"switch" => |s| switchAlwaysExits(s.prongs),
        else => false,
    };
}

fn switchAlwaysExits(prongs: []const ir.Prong) bool {
    var has_explicit_else = false;
    for (prongs) |p| {
        if (p.values == null) has_explicit_else = true;
        if (!blockExits(p.body)) return false;
    }
    return has_explicit_else;
}

fn blockExits(body: []const ir.Stmt) bool {
    for (body) |*s| {
        if (statementExits(s)) return true;
    }
    return false;
}

fn exitKeyword(stmt: *const ir.Stmt) []const u8 {
    return switch (stmt.kind) {
        .return_value, .return_void => "return",
        .fin => "fin",
        .divert, .backup_divert => "divert",
        .fork, .backup_fork => "fork",
        .@"if" => "if",
        .@"switch" => "switch",
        else => "",
    };
}

fn callTargetName(target: ir.ExprRef) ?[]const u8 {
    return switch (target.kind) {
        .load => |s| switch (s) {
            .local => |l| l.name,
            .upvalue => |u| u.name,
            .global => |g| g.name,
        },
        .load_const => |lc| lastPathSegment(lc.target.path),
        else => null,
    };
}

fn lastPathSegment(path: []const u8) []const u8 {
    if (std.mem.lastIndexOfScalar(u8, path, '.')) |i| return path[i + 1 ..];
    return path;
}

fn classHasField(class: *const ir.ClassDecl, name: []const u8) bool {
    for (class.field_names) |fname| {
        if (std.mem.eql(u8, fname, name)) return true;
    }
    return false;
}

fn classHasMethod(class: *const ir.ClassDecl, name: []const u8) bool {
    for (class.methods) |m| {
        if (std.mem.eql(u8, m.name, name)) return true;
    }
    return false;
}

fn isAssignOp(op: ast.BinaryOp) bool {
    return switch (op) {
        .assign,
        .assign_add,
        .assign_subtract,
        .assign_multiply,
        .assign_divide,
        .assign_modulus,
        => true,
        else => false,
    };
}
