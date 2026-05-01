//! IR semantic validation.
//!
//! Walks a fully-resolved IR Program and emits diagnostics that need the
//! IR to be complete: arity mismatch, instance-field validation,
//! dot-access type checks, function-as-value rejection, static-initializer
//! shape, unreachable code, and choice-without-exit.
//!
//! Dispatched from `module.generateBytecode` after `ir.lower` returns,
//! gated on the absence of lowering errors so we don't validate IR
//! whose placeholders may not satisfy this pass's structural assumptions.

const std = @import("std");

const ir = @import("program.zig");
const ast = @import("../frontend/ast.zig");
const Token = @import("../frontend/token.zig").Token;

const module_mod = @import("../module.zig");
const Module = module_mod.Module;

const error_mod = @import("../backend/error.zig");
const CompilerErrors = error_mod.CompilerErrors;
const suggest = @import("../backend/suggest.zig");

const builtins = @import("../runtime/index.zig").builtins;

pub const Error = error{OutOfMemory};

pub fn validate(
    parent_alloc: std.mem.Allocator,
    module: *Module,
    program: *ir.Program,
) Error!void {
    var v = try Validator.init(parent_alloc, module, program);
    defer v.deinit();
    try v.run();
}

const Validator = struct {
    scratch: std.heap.ArenaAllocator,
    module: *Module,
    program: *ir.Program,

    /// Function arity by canonical anchor path. Built up-front from the
    /// resolved IR; consumed by the arity-mismatch check.
    arity_by_path: std.array_hash_map.String(u8),
    /// Class metadata by canonical anchor path. Same lifecycle as
    /// arity_by_path. Mutable so `evaluateLiteral` can publish
    /// `field_values` onto each class as it walks them in source order.
    class_by_path: std.array_hash_map.String(*ir.ClassDecl),
    /// Enum metadata by canonical anchor path. Same lifecycle as
    /// arity_by_path.
    enum_by_path: std.array_hash_map.String(*const ir.EnumDecl),

    fn init(
        parent_alloc: std.mem.Allocator,
        module: *Module,
        program: *ir.Program,
    ) !Validator {
        var scratch = std.heap.ArenaAllocator.init(parent_alloc);
        errdefer scratch.deinit();

        var v: Validator = .{
            .scratch = scratch,
            .module = module,
            .program = program,
            .arity_by_path = .empty,
            .class_by_path = .empty,
            .enum_by_path = .empty,
        };
        try v.collectMetadata(program.body);
        return v;
    }

    fn deinit(self: *Validator) void {
        self.scratch.deinit();
    }

    fn scratchAlloc(self: *Validator) std.mem.Allocator {
        return self.scratch.allocator();
    }

    fn errors(self: *Validator) *CompilerErrors {
        return &self.module.errors;
    }

    /// Resolve the file path for a token's `file_index`. Falls back to
    /// the module entry path for tokens whose file_index is out of range.
    fn pathForTok(self: *Validator, tok: Token) []const u8 {
        if (tok.file_index < self.program.files.len) return self.program.files[tok.file_index];
        return self.module.entry.path;
    }

    /// "did you mean 'X'?" suggestion against a list of candidate names,
    /// or null if nothing close enough. Returned slice is owned by the
    /// errors allocator (matches `addWithHelp` ownership).
    fn suggestFromList(self: *Validator, name: []const u8, haystack: []const []const u8) Error!?[]const u8 {
        const alloc = self.errors().allocator;
        const match = (suggest.closest(alloc, name, haystack) catch return null) orelse return null;
        defer alloc.free(match);
        return try std.fmt.allocPrint(alloc, "did you mean '{s}'?", .{match});
    }

    fn suggestClassMember(self: *Validator, class: *const ir.ClassDecl, name: []const u8) Error!?[]const u8 {
        var names: std.ArrayList([]const u8) = .empty;
        defer names.deinit(self.scratchAlloc());
        for (class.field_names) |f| try names.append(self.scratchAlloc(), f);
        for (class.methods) |m| try names.append(self.scratchAlloc(), m.name);
        return self.suggestFromList(name, names.items);
    }

    /// Joins `prefix` and `suffix` segments with ".". Result lives in
    /// the scratch arena.
    fn joinScopedPath(
        self: *Validator,
        prefix: []const []const u8,
        suffix: []const []const u8,
    ) Error![]const u8 {
        const all = try self.scratchAlloc().alloc([]const u8, prefix.len + suffix.len);
        @memcpy(all[0..prefix.len], prefix);
        @memcpy(all[prefix.len..], suffix);
        return try std.mem.join(self.scratchAlloc(), ".", all);
    }

    // =======================================================================
    // Metadata collection — pre-walk to populate arity / class / enum tables
    // =======================================================================

    fn collectMetadata(self: *Validator, stmts: []const ir.Stmt) Error!void {
        for (stmts) |*s| try self.collectStmt(s);
    }

    fn collectStmt(self: *Validator, stmt: *const ir.Stmt) Error!void {
        const a = self.scratchAlloc();
        switch (stmt.kind) {
            .function => |*f| {
                if (f.anchor) |anc| {
                    if (anc.kind != null) try self.arity_by_path.put(a, anc.path, @intCast(f.parameters.len));
                }
                try self.collectMetadata(f.body);
            },
            .class => |*c| {
                if (c.anchor.kind != null) {
                    // `evaluateLiteral` writes back to `c.field_values`,
                    // so we cast away the const we got from walking
                    // `*const ir.Stmt`. Body slices stay `const` to all
                    // other consumers; this is the one place that
                    // publishes results onto the IR.
                    const c_mut: *ir.ClassDecl = @constCast(c);
                    try self.class_by_path.put(a, c.anchor.path, c_mut);
                }
                for (c.methods) |*m| {
                    if (m.anchor) |anc| {
                        if (anc.kind != null) try self.arity_by_path.put(a, anc.path, @intCast(m.parameters.len));
                    }
                    try self.collectMetadata(m.body);
                }
            },
            .enum_decl => |*e| {
                if (e.anchor.kind != null) try self.enum_by_path.put(a, e.anchor.path, e);
            },
            .bough => |*b| try self.collectMetadata(b.body),
            .fork, .backup_fork => |*f| try self.collectMetadata(f.body),
            .choice => |*c| try self.collectMetadata(c.body),
            .block => |*b| try self.collectMetadata(b.body),
            .@"if" => |*i| {
                try self.collectMetadata(i.then_branch);
                if (i.else_branch) |eb| try self.collectMetadata(eb);
            },
            .@"while" => |*w| try self.collectMetadata(w.body),
            .@"for" => |*f| try self.collectMetadata(f.body),
            .@"switch" => |*sw| {
                for (sw.prongs) |p| try self.collectMetadata(p.body);
            },
            else => {},
        }
    }

    // =======================================================================
    // Semantic walk
    // =======================================================================

    fn run(self: *Validator) Error!void {
        var stack: std.ArrayList([]const u8) = .empty;
        defer stack.deinit(self.scratchAlloc());
        try self.semBody(self.program.body, &stack);
    }

    fn semBody(
        self: *Validator,
        stmts: []const ir.Stmt,
        stack: *std.ArrayList([]const u8),
    ) Error!void {
        try self.checkUnreachable(stmts);
        for (stmts) |*s| try self.semStmt(s, stack);
    }

    fn semStmt(
        self: *Validator,
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
                // Track whether every initializer passed structural
                // checks. If any failed we skip evaluation — the class's
                // `field_values` stays empty and codegen has already
                // been short-circuited by the module-level `.err` gate.
                var all_static = true;
                for (c.field_initializers) |e| {
                    const before = self.errors().list.items.len;
                    try self.checkNotFunctionRef(e, e.loc.start);
                    try self.checkStaticInitializer(e);
                    try self.semExpr(e, stack);
                    if (self.errors().list.items.len != before) all_static = false;
                }
                if (all_static and c.anchor.kind != null) {
                    try self.evaluateClassFieldValues(c.anchor.path, c.field_initializers);
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
                // Returning a function value is permitted: the value is
                // ephemeral on the stack and the caller decides what to
                // do with it. Storage positions (var_decl, assignments,
                // list/set/map/instance fields) are still rejected.
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
        self: *Validator,
        seg: ir.TextSegment,
        stack: *std.ArrayList([]const u8),
    ) Error!void {
        switch (seg) {
            .literal => {},
            .interp => |e| try self.semExpr(e, stack),
        }
    }

    fn semExpr(
        self: *Validator,
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
        self: *Validator,
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

    fn lookupArity(self: *Validator, name: []const u8, stack: []const []const u8) ?u8 {
        var i: usize = stack.len;
        while (i > 0) : (i -= 1) {
            const candidate = self.joinScopedPath(stack[0..i], &.{name}) catch return null;
            if (self.arity_by_path.get(candidate)) |a| return a;
        }
        return self.arity_by_path.get(name);
    }

    fn checkInstanceFields(
        self: *Validator,
        ins: *const ir.Instance,
        tok: Token,
    ) Error!void {
        if (ins.class.kind == null) return; // unresolved; reported elsewhere
        const class = self.class_by_path.get(ins.class.path) orelse return;
        for (ins.field_names) |fname| {
            if (!classHasField(class, fname)) {
                try self.errors().addWithHelp(
                    self.pathForTok(tok),
                    "Class '{s}' has no field '{s}'",
                    tok,
                    .err,
                    .{ class.name, fname },
                    try self.suggestClassMember(class, fname),
                    null,
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
        self: *Validator,
        f: *const ir.Field,
        tok: Token,
    ) Error!void {
        switch (f.target.var_type) {
            .instance => |class_name| {
                const class = self.class_by_path.get(class_name) orelse return;
                if (classHasField(class, f.name)) return;
                if (classHasMethod(class, f.name)) return;
                try self.errors().addWithHelp(
                    self.pathForTok(tok),
                    "Class '{s}' does not contain a field '{s}'",
                    tok,
                    .err,
                    .{ class.name, f.name },
                    try self.suggestClassMember(class, f.name),
                    null,
                );
            },
            .string => {
                if (builtins.string_methods.has(f.name)) return;
                try self.errors().addWithHelp(
                    self.pathForTok(tok),
                    "Unknown method '{s}' on string",
                    tok,
                    .err,
                    .{f.name},
                    try self.suggestFromList(f.name, builtins.string_methods.keys()),
                    null,
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
                try self.errors().addWithHelp(
                    self.pathForTok(tok),
                    "Unknown method '{s}' on {s}",
                    tok,
                    .err,
                    .{ f.name, type_name },
                    try self.suggestFromList(f.name, builtins.collection_methods.keys()),
                    null,
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
                try self.errors().addWithHelp(
                    self.pathForTok(tok),
                    "Class '{s}' does not contain a field '{s}'",
                    tok,
                    .err,
                    .{ c.name, f.name },
                    try self.suggestClassMember(c, f.name),
                    null,
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
    fn isFunctionRef(self: *const Validator, expr: ir.ExprRef) bool {
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
    fn checkNotFunctionRef(self: *Validator, expr: ir.ExprRef, tok: Token) Error!void {
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
    fn checkStaticInitializer(self: *Validator, expr: ir.ExprRef) Error!void {
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
            // Ranges aren't a permissible static value — the AST
            // compiler's `evaluateLiteral` has no `.range` arm and falls
            // through to its "Only literal values" rejection.
            .bin_op => |b| {
                try self.checkStaticInitializer(b.left);
                try self.checkStaticInitializer(b.right);
            },
            .un_op => |u| try self.checkStaticInitializer(u.operand),
            // `Enum.Value` / `Class.field` — recurse into target.
            .field => |f| try self.checkStaticInitializer(f.target),
            // Everything else is non-static: variable loads, calls,
            // computed indexers, if-expressions, builtins, ranges,
            // `new C{...}` (writers' instance constructors aren't
            // statically evaluable — class field defaults can reference
            // existing classes via bare-name `load_const` instead).
            .instance, .load, .if_expr, .index, .call, .builtin, .@"extern", .range => {
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

    /// Walk a class's already-validated `field_initializers` and
    /// publish their statically-evaluated `LiteralValue`s onto the
    /// matching `ClassDecl.field_values`. Caller has confirmed every
    /// initializer passed `checkStaticInitializer` + `checkNotFunctionRef`
    /// + `semExpr` without errors.
    ///
    /// Forward references between classes (A's default reads from B,
    /// where B is declared later) leave A's `field_values` empty —
    /// codegen would have hit `unreachable` on the same input under
    /// the old `evaluateLiteral`, so the gap is preserved, just
    /// expressed as a no-op rather than a panic.
    fn evaluateClassFieldValues(
        self: *Validator,
        class_path: []const u8,
        inits: []const ir.ExprRef,
    ) Error!void {
        const class = self.class_by_path.get(class_path) orelse return;
        const arena = self.program.allocator();
        const values = try arena.alloc(ir.LiteralValue, inits.len);
        for (inits, 0..) |init_expr, i| {
            const lv = self.evaluateLiteral(init_expr) catch |err| switch (err) {
                error.OutOfMemory => return error.OutOfMemory,
                error.ForwardClassRef => return,
            };
            values[i] = lv;
        }
        class.field_values = values;
    }

    const EvalError = Error || error{ForwardClassRef};

    /// Pure walker: ExprRef → LiteralValue. Trusts `checkStaticInitializer`
    /// to have rejected anything outside the literal-shape grammar.
    /// Strings and recursive containers are allocated in the program
    /// arena.
    fn evaluateLiteral(self: *Validator, expr: ir.ExprRef) EvalError!ir.LiteralValue {
        const arena = self.program.allocator();
        switch (expr.kind) {
            .number => |n| return .{ .number = n },
            .bool => |b| return .{ .bool = b },
            .nil => return .nil,
            .text => |segs| {
                var len: usize = 0;
                for (segs) |s| switch (s) {
                    .literal => |l| len += l.len,
                    .interp => unreachable,
                };
                const buf = try arena.alloc(u8, len);
                var pos: usize = 0;
                for (segs) |s| switch (s) {
                    .literal => |l| {
                        @memcpy(buf[pos .. pos + l.len], l);
                        pos += l.len;
                    },
                    .interp => unreachable,
                };
                return .{ .string = buf };
            },
            .un_op => |u| {
                const inner = try self.evaluateLiteral(u.operand);
                return switch (u.op) {
                    .negate => .{ .number = -inner.number },
                    .not => .{ .bool = !inner.bool },
                };
            },
            .bin_op => |bin| {
                const left = try self.evaluateLiteral(bin.left);
                const right = try self.evaluateLiteral(bin.right);
                return .{ .number = switch (bin.op) {
                    .add => left.number + right.number,
                    .subtract => left.number - right.number,
                    .multiply => left.number * right.number,
                    .divide => left.number / right.number,
                    else => unreachable,
                } };
            },
            .load_const => |lc| return .{ .constant_ref = lc.target.path },
            .field => |*f| return try self.evaluateField(f),
            .list => |items| {
                const xs = try arena.alloc(ir.LiteralValue, items.len);
                for (items, 0..) |item, i| xs[i] = try self.evaluateLiteral(item);
                return .{ .list = xs };
            },
            .set => |items| {
                const xs = try arena.alloc(ir.LiteralValue, items.len);
                for (items, 0..) |item, i| xs[i] = try self.evaluateLiteral(item);
                return .{ .set = xs };
            },
            .map => |pairs| {
                const xs = try arena.alloc(ir.LiteralMapPair, pairs.len);
                for (pairs, 0..) |p, i| {
                    xs[i] = .{
                        .key = try self.evaluateLiteral(p.key),
                        .value = try self.evaluateLiteral(p.value),
                    };
                }
                return .{ .map = xs };
            },
            else => unreachable,
        }
    }

    /// Resolve `Enum.Value` / `Class.field` static accesses. Reads
    /// `enum_by_path` / `class_by_path` rather than introspecting a
    /// runtime `Value.Obj` (the old codegen path).
    fn evaluateField(self: *Validator, f: *const ir.Field) EvalError!ir.LiteralValue {
        // checkStaticInitializer guarantees the target chain bottoms
        // out at a `.load_const`. Anything else would have been
        // rejected.
        const target_path = switch (f.target.kind) {
            .load_const => |lc| lc.target.path,
            else => unreachable,
        };
        if (self.enum_by_path.get(target_path)) |e| {
            for (e.values, 0..) |v, idx| {
                if (std.mem.eql(u8, v, f.name)) return .{ .number = @floatFromInt(idx) };
            }
            unreachable; // checkFieldAccess would have rejected
        }
        if (self.class_by_path.get(target_path)) |class| {
            // Forward ref — class hasn't been visited yet, so its
            // field_values is still empty. Bail; the caller skips
            // publishing for this class.
            if (class.field_values.len == 0 and class.field_names.len > 0) {
                return error.ForwardClassRef;
            }
            for (class.field_names, 0..) |name, idx| {
                if (std.mem.eql(u8, name, f.name)) return class.field_values[idx];
            }
            unreachable; // checkFieldAccess would have rejected
        }
        unreachable; // anchor isn't an enum or class — rejected upstream
    }

    fn checkUnreachable(self: *Validator, stmts: []const ir.Stmt) Error!void {
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
};

// ===========================================================================
// Helpers
// ===========================================================================

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
