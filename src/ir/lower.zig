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
const suggest = @import("../backend/suggest.zig");

const builtins = @import("../runtime/index.zig").builtins;

pub const Error = error{
    OutOfMemory,
    LowerError,
};

/// Pre-walk-allocated visit slot for a bough/fork/choice. Mirrors the
/// shape of a scope `Symbol` for the bits codegen needs but lives in a
/// separate list so anchors don't pollute `root_scope.symbols`.
const VisitSymbol = struct {
    path: []const u8,
    uuid: UUID.ID,
    index: C.GLOBAL,
};

/// Lower a Module's resolved trees into an IR Program. The result has
/// anchors registered and forward references resolved, but semantic
/// diagnostics (arity, dot-access, function-as-value, etc.) have NOT
/// run yet — callers should invoke `ir.validate` after this returns,
/// gated on `module.errors` to avoid validating malformed IR.
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

    /// Token of each anchor's first declaration, keyed by qualified path.
    /// Used to point "previously declared at" notes on duplicate-decl
    /// diagnostics. Same lifetime as the IR program (scratch arena).
    decl_tokens: std.array_hash_map.String(Token),

    /// Visit symbols allocated by the pre-walk (`defineAnchorVisitSlots`),
    /// in slot-index order (one per bough/fork/choice). These reserve
    /// `globals[0..N-1]` for runtime visit counters and feed
    /// `program.globals` at the end of lowering. Scratch-lifetime.
    /// Kept out of `root_scope.symbols` on purpose so `resolveSymbol`
    /// can't pick them up as variables — anchor lookups go through
    /// `program.anchors`.
    visit_symbols: std.ArrayList(VisitSymbol),
    /// Path → visit slot index, for the main walk to wire onto each
    /// `AnchorRef.visit_index` via `registerAnchor`. Scratch-lifetime.
    visit_index_by_path: std.array_hash_map.String(C.GLOBAL),

    /// High-water mark of local-scope counts seen since the last
    /// reset (saved/restored around `function` scopes). Used to
    /// populate `FunctionDecl.locals_count` for codegen.
    locals_count: u32 = 0,

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
            .decl_tokens = .empty,
            .visit_symbols = .empty,
            .visit_index_by_path = .empty,
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

        try self.registerBuiltinAnchors();

        // Pre-walk anchor declarations (boughs / forks / choices) and
        // reserve a visit slot for each. The runtime layout reserves
        // globals[0..N-1] for these visit counters, then [N..] for
        // plain var_decls. Visit symbols deliberately live OUTSIDE
        // `root_scope.symbols` (in `visit_symbols`) so `resolveSymbol`
        // can't find them as variables — anchor-name lookups go through
        // `program.anchors`. We bump `root_scope.count` afterwards to
        // reserve the slot range so var indices start at N.
        try self.defineAnchorVisitSlots(tree.root);
        self.seen_files.clearRetainingCapacity();
        self.root_scope.count = @intCast(self.visit_symbols.items.len);

        // One walk: emit IR, registering anchors at their declaration sites.
        var body: std.ArrayList(ir.Stmt) = .empty;
        defer body.deinit(self.scratchAlloc());
        for (tree.root) |*s| try self.appendStatement(&body, s);
        self.program.body = try self.arena().dupe(ir.Stmt, body.items);

        self.program.globals_count = @intCast(self.root_scope.count);
        self.program.top_level_locals_count = self.locals_count;
        try self.populateFilesTable();
        try self.materializeGlobals();

        // Resolve forward references by walking the IR. Semantic
        // validation is dispatched separately by `module.generateBytecode`
        // so it can be skipped if lowering itself recorded any errors.
        try self.validateAnchors();
    }

    /// Build `program.globals` from the pre-walk's `visit_symbols`
    /// followed by the var symbols defined on `root_scope` during the
    /// main walk. Visit symbols come first (indices 0..N-1, matching
    /// the runtime layout); vars come after (indices N..). Names are
    /// duped into the program arena since both source lists live in
    /// scratch and die with `deinitTransient`.
    fn materializeGlobals(self: *Lowerer) Error!void {
        const var_symbols = self.root_scope.symbols.values();
        const total = self.visit_symbols.items.len + var_symbols.len;
        const globals = try self.arena().alloc(ir.GlobalSymbol, total);
        for (self.visit_symbols.items, 0..) |v, i| {
            globals[i] = .{
                .name = try self.arena().dupe(u8, v.path),
                .uuid = v.uuid,
                .index = v.index,
                .is_mutable = false,
            };
        }
        for (var_symbols, 0..) |s, i| {
            globals[self.visit_symbols.items.len + i] = .{
                .name = try self.arena().dupe(u8, s.name),
                .uuid = s.uuid,
                .index = s.index,
                .is_mutable = s.is_mutable,
            };
        }
        self.program.globals = globals;
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
            .block => |b| try self.lowerBlockStmt(b, tok),
            .@"if" => |i| try self.lowerIfStmt(i, tok),
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
        const visit_index = self.visit_index_by_path.get(path);
        const anchor = self.registerAnchor(path, .choice, c.id, visit_index) catch |e| switch (e) {
            error.SymbolAlreadyDeclared => blk: {
                try self.errors().addWithHelp(self.current_file.path, "'{s}' is already declared", tok, .err, .{choice_name}, null, try self.previousDeclNote(path));
                break :blk ir.AnchorRef{ .kind = .choice, .path = path, .uuid = c.id, .visit_index = visit_index };
            },
            error.OutOfMemory => return error.OutOfMemory,
        };
        try self.recordDeclToken(path, tok);

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
        const visit_index = self.visit_index_by_path.get(path);
        const anchor = self.registerAnchor(path, .fork, f.id, visit_index) catch |e| switch (e) {
            error.SymbolAlreadyDeclared => blk: {
                try self.errors().addSpanWithHelp(self.current_file.path, "'{s}' is already declared", tok, f.end_token, .err, .{fork_name}, null, try self.previousDeclNote(path));
                break :blk ir.AnchorRef{ .kind = .fork, .path = path, .uuid = f.id, .visit_index = visit_index };
            },
            error.OutOfMemory => return error.OutOfMemory,
        };
        try self.recordDeclToken(path, tok);

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
            .kind = if (f.is_backup)
                .{ .backup_fork = fork_payload }
            else if (f.is_cycle)
                .{ .cycle_fork = fork_payload }
            else
                .{ .fork = fork_payload },
        };
    }

    fn lowerBoughStmt(self: *Lowerer, b: anytype, tok: Token) Error!ir.Stmt {
        const path = try self.qualifyName(b.name);
        const visit_index = self.visit_index_by_path.get(path);
        const anchor = self.registerAnchor(path, .bough, b.id, visit_index) catch |e| switch (e) {
            error.SymbolAlreadyDeclared => blk: {
                try self.errors().addSpanWithHelp(self.current_file.path, "Bough '{s}' is already declared", tok, b.name_token, .err, .{b.name}, null, try self.previousDeclNote(path));
                break :blk ir.AnchorRef{ .kind = .bough, .path = path, .uuid = b.id, .visit_index = visit_index };
            },
            error.OutOfMemory => return error.OutOfMemory,
        };
        try self.recordDeclToken(path, tok);

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

    fn lowerBlockStmt(self: *Lowerer, body: []const ast.Statement, tok: Token) Error!ir.Stmt {
        try self.enterScope(.local);
        defer self.exitScope();
        return .{
            .loc = locFrom(tok),
            .kind = .{ .block = .{ .body = try self.lowerBody(body) } },
        };
    }

    fn lowerIfStmt(self: *Lowerer, i: anytype, tok: Token) Error!ir.Stmt {
        const condition = try self.lowerExpression(i.condition);
        const then_branch = blk: {
            try self.enterScope(.local);
            defer self.exitScope();
            break :blk try self.lowerBody(i.then_branch);
        };
        const else_branch = if (i.else_branch) |eb| blk: {
            try self.enterScope(.local);
            defer self.exitScope();
            break :blk try self.lowerBody(eb);
        } else null;
        return .{
            .loc = locFrom(tok),
            .kind = .{ .@"if" = .{
                .condition = condition,
                .then_branch = then_branch,
                .else_branch = else_branch,
            } },
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
        // top-level functions are reachable via path. Methods are looked
        // up by name on their Class object at runtime, so they don't go
        // in `program.anchors`; `lowerClassStmt` runs a per-class member
        // dedup that handles duplicate-method diagnostics.
        var anchor: ?ir.AnchorRef = null;
        if (!f.is_method) {
            const path = try self.qualifyName(f.name);
            const anchor_kind: ir.AnchorRef.Kind = if (f.is_extern) .extern_function else .function;
            anchor = self.registerAnchor(path, anchor_kind, UUID.Empty, null) catch |e| switch (e) {
                // If the path was already claimed (e.g., a same-named class or
                // sibling), produce an error but keep going with a non-anchored
                // representation.
                error.SymbolAlreadyDeclared => null_blk: {
                    try self.errors().addSpanWithHelp(self.current_file.path, "'{s}' is already declared", tok, f.name_token, .err, .{f.name}, null, try self.previousDeclNote(path));
                    break :null_blk null;
                },
                error.OutOfMemory => return error.OutOfMemory,
            };
            if (anchor != null) try self.recordDeclToken(path, tok);
        }

        // Functions are reachable via their anchor path (load_const),
        // not via scope. Mirrors the AST compiler, which never adds
        // function names to the scope chain — every reference falls
        // through `loadSymbol` to `resolveConstant`. This matters for
        // self-recursion: `fact(n-1)` inside `fn fact` resolves via
        // `refByPath` walking path_stack, finding the anchor in
        // `program.anchors`, and emitting a constant load.

        // Save and reset locals_count around the function body — local
        // counts inside this function shouldn't leak to the outer
        // function or to top-level.
        const saved_locals_count = self.locals_count;
        self.locals_count = 0;

        try self.enterScope(.function);

        // Methods receive an implicit `self` as the first parameter at
        // runtime — define it in the IR's function scope so identifier
        // resolution inside the body finds it.
        if (f.is_method) {
            _ = self.scope.define(self.scratchAlloc(), "self", false) catch |e| switch (e) {
                error.SymbolAlreadyDeclared => {},
                error.OutOfMemory => return error.OutOfMemory,
            };
        }

        const params = try self.arena().alloc(ir.Parameter, f.parameters.len);
        for (f.parameters, 0..) |pname, i| {
            // Parameters are mutable bindings — the body may reassign
            // them (e.g. `while count > 0 { count -= 1 }`).
            const psym = self.scope.define(self.scratchAlloc(), pname, true) catch |e| switch (e) {
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

        // Capture locals_count BEFORE exitScope — the function scope's
        // own count still includes parameters / `self` / top-level
        // var_decls in the body, while `self.locals_count` holds the
        // high-water mark from any nested local scopes inside the body.
        const fn_locals_count = @max(self.scope.count, self.locals_count);
        self.exitScope();
        self.locals_count = saved_locals_count;

        return .{
            .loc = locFrom(tok),
            .kind = .{ .function = .{
                .name = try self.arena().dupe(u8, f.name),
                .name_slot = null,
                .anchor = anchor,
                .is_method = f.is_method,
                .is_extern = f.is_extern,
                .parameters = params,
                .body = body,
                .locals_count = fn_locals_count,
            } },
        };
    }

    fn lowerClassStmt(self: *Lowerer, c: anytype, tok: Token) Error!ir.Stmt {
        const path = try self.qualifyName(c.name);
        const anchor = self.registerAnchor(path, .class, UUID.Empty, null) catch |e| switch (e) {
            error.SymbolAlreadyDeclared => blk: {
                try self.errors().addSpanWithHelp(self.current_file.path, "'{s}' is already declared", tok, c.name_token, .err, .{c.name}, null, try self.previousDeclNote(path));
                break :blk ir.AnchorRef{ .kind = .class, .path = path };
            },
            error.OutOfMemory => return error.OutOfMemory,
        };
        try self.recordDeclToken(path, tok);

        const inits = try self.arena().alloc(ir.ExprRef, c.fields.len);
        for (c.fields, 0..) |*f_expr, i| inits[i] = try self.lowerExpression(f_expr);

        const field_names = try self.arena().alloc([]const u8, c.field_names.len);
        for (c.field_names, 0..) |fn_, i| field_names[i] = try self.arena().dupe(u8, fn_);

        // Per-class member-name dedup: fields and methods share a single
        // namespace at runtime (`instance.name` / `Class.name` finds either),
        // so a field and method may not share a name, nor may two fields
        // or two methods. Walk in source order and emit on the second
        // occurrence pointing back to the first.
        var members: std.array_hash_map.String(Token) = .empty;
        defer members.deinit(self.scratchAlloc());
        for (c.field_names, c.field_name_tokens) |fname, ftok| {
            const gop = try members.getOrPut(self.scratchAlloc(), fname);
            if (gop.found_existing) {
                try self.errors().addWithHelp(self.current_file.path, "'{s}' is already declared", ftok, .err, .{fname}, null, try self.formatDeclNote(gop.value_ptr.*));
            } else {
                gop.value_ptr.* = ftok;
            }
        }
        for (c.methods) |*m_stmt| {
            const m = m_stmt.type.function;
            const gop = try members.getOrPut(self.scratchAlloc(), m.name);
            if (gop.found_existing) {
                try self.errors().addWithHelp(self.current_file.path, "'{s}' is already declared", m.name_token, .err, .{m.name}, null, try self.formatDeclNote(gop.value_ptr.*));
            } else {
                gop.value_ptr.* = m.name_token;
            }
        }

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
        const anchor = self.registerAnchor(path, .@"enum", UUID.Empty, null) catch |err| switch (err) {
            error.SymbolAlreadyDeclared => blk: {
                try self.errors().addSpanWithHelp(self.current_file.path, "'{s}' is already declared", tok, e.name_token, .err, .{e.name}, null, try self.previousDeclNote(path));
                break :blk ir.AnchorRef{ .kind = .@"enum", .path = path };
            },
            error.OutOfMemory => return error.OutOfMemory,
        };
        try self.recordDeclToken(path, tok);

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
        // Warn when a local declaration hides an outer-scope variable.
        // Only locals trigger the warning — global re-use across scopes
        // is intentional and stays quiet.
        if (self.scope.tag != .global) {
            if (self.scope.resolveOuter(v.name)) |_| {
                try self.errors().addSpan(
                    self.current_file.path,
                    "'{s}' hides a variable from an outer scope",
                    tok,
                    v.name_token,
                    .warn,
                    .{v.name},
                );
            }
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
        switch (left.type) {
            .identifier => |name| {
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
            },
            .indexer => |idx| {
                // `Type.field = ...` for dot-access on a class or enum
                // anchor — both are immutable. The parser produces
                // .indexer for both `[i]` and `.field`; the dot form
                // has token_type == .dot.
                if (left.token.token_type != .dot) return;
                if (idx.target.type != .identifier) return;
                const target_name = idx.target.type.identifier;
                if (self.program.anchors.get(target_name)) |a| switch (a.kind orelse return) {
                    .class, .@"enum" => try self.errors().add(
                        self.current_file.path,
                        "Cannot assign to constant '{s}'",
                        left.token,
                        .err,
                        .{target_name},
                    ),
                    else => {},
                };
            },
            else => {},
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

    /// Pre-register every builtin function as an anchor so name lookups
    /// flow through the normal `refByPath` path. Codegen pre-populates
    /// `constants_map` with the same names, so the standard `load_const`
    /// arm resolves to the builtin's constant slot at runtime.
    fn registerBuiltinAnchors(self: *Lowerer) Error!void {
        for (builtins.functions.keys()) |name| {
            const path = try self.arena().dupe(u8, name);
            try self.program.anchors.put(self.arena(), path, .{
                .kind = .function,
                .path = path,
                .uuid = UUID.Empty,
            });
        }
    }

    /// Insert an anchor into `program.anchors`. Returns the canonical
    /// `AnchorRef`. Returns `error.SymbolAlreadyDeclared` if the path is
    /// already taken. `visit_index` is the global slot reserved for
    /// visit-tracked anchors (bough/fork/choice) by `defineAnchorVisitSlots`;
    /// pass `null` for class/enum/function anchors.
    fn registerAnchor(
        self: *Lowerer,
        path: []const u8,
        kind: ir.AnchorRef.Kind,
        uuid: UUID.ID,
        visit_index: ?C.GLOBAL,
    ) !ir.AnchorRef {
        const gop = try self.program.anchors.getOrPut(self.arena(), path);
        if (gop.found_existing) return error.SymbolAlreadyDeclared;
        gop.value_ptr.* = .{
            .kind = kind,
            .path = path,
            .uuid = uuid,
            .visit_index = visit_index,
        };
        return gop.value_ptr.*;
    }

    /// "previous declaration at file:line" note for duplicate-decl
    /// diagnostics. Caller transfers ownership to `addWithHelp`.
    fn previousDeclNote(self: *Lowerer, path: []const u8) Error!?[]const u8 {
        const tok = self.decl_tokens.get(path) orelse return null;
        return try self.formatDeclNote(tok);
    }

    /// Format a "previous declaration at file:line" note from a token
    /// directly. Used for per-class member dedup which doesn't go through
    /// `decl_tokens`.
    fn formatDeclNote(self: *Lowerer, tok: Token) Error![]const u8 {
        const alloc = self.errors().allocator;
        const file_name = if (tok.file_index < self.program.files.len)
            std.fs.path.basename(self.program.files[tok.file_index])
        else
            "?";
        return try std.fmt.allocPrint(alloc, "previous declaration at {s}:{d}", .{ file_name, tok.line });
    }

    /// Record an anchor's first-declaration token so duplicate-decl
    /// errors can point back to it.
    fn recordDeclToken(self: *Lowerer, path: []const u8, tok: Token) Error!void {
        try self.decl_tokens.put(self.scratchAlloc(), path, tok);
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

    /// Last `.`-separated segment of an anchor path. Identifier names
    /// and UUID bytes (`[0-9A-Z-]`) never contain `.`, so the last dot
    /// is the exact parent/segment boundary that `qualifyName` wrote.
    fn lastSegment(path: []const u8) []const u8 {
        if (std.mem.lastIndexOfScalar(u8, path, '.')) |i| return path[i + 1 ..];
        return path;
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
                try stack.append(self.scratchAlloc(), lastSegment(c.anchor.path));
                defer _ = stack.pop();
                try self.validateBody(c.body, stack);
            },
            .fork, .backup_fork, .cycle_fork => |*f| {
                try self.patchAnchor(&f.anchor, stack, stmt.loc.start);
                try stack.append(self.scratchAlloc(), lastSegment(f.anchor.path));
                defer _ = stack.pop();
                try self.validateBody(f.body, stack);
            },
            .divert, .backup_divert => |*d| {
                try self.patchAnchor(&d.target, stack, stmt.loc.start);
            },
            .bough => |*b| {
                try self.patchAnchor(&b.anchor, stack, stmt.loc.start);
                try stack.append(self.scratchAlloc(), lastSegment(b.anchor.path));
                defer _ = stack.pop();
                try self.validateBody(b.body, stack);
            },
            .visit => |*v| try self.patchAnchor(&v.target, stack, stmt.loc.start),
            .class => |*c| {
                try self.patchAnchor(&c.anchor, stack, stmt.loc.start);
                for (c.field_initializers) |e| try self.validateExpr(e, stack);
                try stack.append(self.scratchAlloc(), lastSegment(c.anchor.path));
                defer _ = stack.pop();
                for (c.methods) |*m| try self.validateFunction(@constCast(m), stack, stmt.loc.start);
            },
            .enum_decl => |*e| {
                try self.patchAnchor(&e.anchor, stack, stmt.loc.start);
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
            .field => |*f| {
                try self.validateExpr(f.target, stack);
                try self.maybeRewriteAnchorChain(e);
            },
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

        try self.errors().addWithHelp(
            self.pathForTok(tok),
            "Unknown name '{s}'",
            tok,
            .err,
            .{writer_path},
            try self.suggestAnchorName(writer_path),
            null,
        );
    }

    /// If `e` is the outermost `.field` of an anchor chain (e.g.
    /// `START.NAMED.ONE`) whose joined path resolves to a visit-tracked
    /// anchor, collapse the chain into a single `.load_const`. Codegen
    /// then emits `get_global visit_index` from the standard load_const
    /// arm, with no chain re-walking.
    fn maybeRewriteAnchorChain(self: *Lowerer, e: *ir.Expr) Error!void {
        var parts: std.ArrayList([]const u8) = .empty;
        defer parts.deinit(self.scratchAlloc());

        var cur: *const ir.Expr = e;
        while (true) {
            switch (cur.kind) {
                .field => |f| {
                    try parts.append(self.scratchAlloc(), f.name);
                    cur = f.target;
                },
                .load_const => |lc| {
                    try parts.append(self.scratchAlloc(), lc.target.path);
                    break;
                },
                else => return,
            }
        }
        std.mem.reverse([]const u8, parts.items);
        const joined = try std.mem.join(self.scratchAlloc(), ".", parts.items);
        defer self.scratchAlloc().free(joined);

        const anchor = self.program.anchors.get(joined) orelse return;
        switch (anchor.kind orelse return) {
            .bough, .fork, .choice => {},
            else => return,
        }
        e.kind = .{ .load_const = .{ .target = anchor } };
    }

    /// "did you mean 'X'?" suggestion across the program's anchor names,
    /// used when a divert / call / load_const can't be resolved.
    fn suggestAnchorName(self: *Lowerer, name: []const u8) Error!?[]const u8 {
        const alloc = self.errors().allocator;
        var names: std.ArrayList([]const u8) = .empty;
        defer names.deinit(self.scratchAlloc());
        for (self.program.anchors.keys()) |k| try names.append(self.scratchAlloc(), k);
        const match = (suggest.closest(alloc, name, names.items) catch return null) orelse return null;
        defer alloc.free(match);
        return try std.fmt.allocPrint(alloc, "did you mean '{s}'?", .{match});
    }

    // =======================================================================
    // Scope wrappers
    // =======================================================================

    fn enterScope(self: *Lowerer, tag: Scope.Tag) Error!void {
        self.scope = try Scope.create(self.scratchAlloc(), self.scope, tag);
    }
    fn exitScope(self: *Lowerer) void {
        const old = self.scope;
        self.scope = old.parent orelse old;
        // Local scopes carry the high-water mark for the enclosing
        // frame: their `count` is already the merged total since
        // `Scope.create(.local)` inherits sibling-local count. Function
        // scopes manage their own `locals_count` snapshot in
        // `lowerFunctionStmt` and don't contribute here. Globals never
        // exit during lowering.
        if (old.tag == .local) {
            if (old.count > self.locals_count) self.locals_count = old.count;
        }
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
    // Anchor pre-walk
    // =======================================================================

    /// Walk the AST defining a visit-slot symbol on `root_scope` for
    /// every bough/fork/choice. Mirrors the main walk's naming and
    /// path-stack discipline so each symbol is keyed by the same
    /// fully-qualified path that `registerAnchor` will use later.
    /// Side effect: `root_scope.count` ends at the total visit-slot
    /// count, and `visit_index_by_path` maps each path → its slot.
    /// Reuses `seen_files` for include cycle detection; the caller
    /// must reset that set before the lowering walk re-traverses
    /// includes.
    fn defineAnchorVisitSlots(self: *Lowerer, stmts: []const ast.Statement) Error!void {
        for (stmts) |*s| try self.defineAnchorVisitSlotsInStmt(s);
    }

    fn defineAnchorVisitSlotsInStmt(self: *Lowerer, stmt: *const ast.Statement) Error!void {
        switch (stmt.type) {
            .bough => |b| {
                try self.defineVisitSlot(b.name, b.id);
                try self.pushPath(b.name);
                defer self.popPath();
                try self.defineAnchorVisitSlots(b.body);
            },
            .fork => |f| {
                const fork_name = try self.forkName(f.name, f.id);
                try self.defineVisitSlot(fork_name, f.id);
                try self.pushPath(fork_name);
                defer self.popPath();
                try self.defineAnchorVisitSlots(f.body);
            },
            .choice => |c| {
                const choice_name = c.name orelse &c.id;
                try self.defineVisitSlot(choice_name, c.id);
                try self.pushPath(choice_name);
                defer self.popPath();
                try self.defineAnchorVisitSlots(c.body);
            },
            // Plain control flow doesn't introduce a new path segment but
            // can host nested anchor declarations (e.g. a choice inside an
            // `if` inside a fork). Descend so those still get visit slots.
            .@"if" => |i| {
                try self.defineAnchorVisitSlots(i.then_branch);
                if (i.else_branch) |eb| try self.defineAnchorVisitSlots(eb);
            },
            .@"while" => |w| try self.defineAnchorVisitSlots(w.body),
            .@"for" => |f| try self.defineAnchorVisitSlots(f.body),
            .@"switch" => |sw| {
                for (sw.prongs) |*p| {
                    if (p.type == .switch_prong) try self.defineAnchorVisitSlots(p.type.switch_prong.body);
                }
            },
            .block => |b| try self.defineAnchorVisitSlots(b),
            .include => |inc| {
                const result = (try self.resolveInclude(inc.path)) orelse return;
                const prev = self.current_file;
                defer self.current_file = prev;
                self.current_file = result.file;
                try self.defineAnchorVisitSlots(result.tree);
            },
            else => {},
        }
    }

    /// Reserve a visit-slot for the anchor with the given segment name
    /// under the current `path_stack`. Records into `visit_symbols` (for
    /// the final `program.globals`) and `visit_index_by_path` (for the
    /// main walk to wire onto `AnchorRef.visit_index`). On duplicate
    /// paths, skip silently — the main walk's `registerAnchor` will
    /// diagnose the duplicate with token spans via `previousDeclNote`.
    fn defineVisitSlot(self: *Lowerer, segment: []const u8, uuid: UUID.ID) Error!void {
        const path = try self.qualifyName(segment);
        const gop = try self.visit_index_by_path.getOrPut(self.scratchAlloc(), path);
        if (gop.found_existing) return;
        const idx: C.GLOBAL = @intCast(self.visit_symbols.items.len);
        gop.value_ptr.* = idx;
        try self.visit_symbols.append(self.scratchAlloc(), .{
            .path = path,
            .uuid = uuid,
            .index = idx,
        });
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
