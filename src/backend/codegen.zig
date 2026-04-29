//! IR → bytecode codegen.
//!
//! Walks an `ir.Program` and produces a `Bytecode` value via the shared
//! `Emitter`. Replaces the AST-driven `compiler.zig` end state.
//!
//! The pipeline at this point has already run:
//!   - parser → AST per file
//!   - lower (`src/ir/lower.zig`) → IR Program with all names → Slot,
//!     anchors → AnchorRef, interpolated strings → []TextSegment
//!   - validate (`src/ir/validate.zig`) → semantic diagnostics
//!     (arity, dot-access, instance fields, static initializers,
//!     unreachable, fork-has-no-exit, function-as-value)
//!
//! So this pass is purely about emission: walk the IR, emit bytecode,
//! drain the emitter into Bytecode. Diagnostic emission is limited to
//! emission-time concerns (constant-pool overflow, bytecode size limits).

const std = @import("std");

const ir = @import("../ir/index.zig");
const ast = @import("../frontend/ast.zig");
const Module = @import("../module.zig").Module;

const utils = @import("../utils/index.zig");
const C = utils.C;
const UUID = utils.UUID;

const types = @import("../types/index.zig");
const Value = types.Value;
const Anchor = types.Anchor;

const builtins = @import("../runtime/index.zig").builtins;

const scope_mod = @import("../ir/scope.zig");
const Scope = scope_mod.Scope;

const emit = @import("emit.zig");
const Emitter = emit.Emitter;
const BREAK_HOLDER = emit.BREAK_HOLDER;
const CONTINUE_HOLDER = emit.CONTINUE_HOLDER;
const SWITCH_END_HOLDER = emit.SWITCH_END_HOLDER;
const JUMP_HOLDER = emit.JUMP_HOLDER;
const OpCode = @import("opcode.zig").OpCode;

const Token = @import("../frontend/token.zig").Token;

const Bytecode = @import("bytecode.zig").Bytecode;

pub const Error = error{
    OutOfMemory,
    OutOfScope,
    SymbolAlreadyDeclared,
    NotYetImplemented,
};

pub const Codegen = struct {
    /// Long-lived: backs constants and chunk buffers (drained into
    /// Bytecode via toOwnedSlice).
    alloc: std.mem.Allocator,
    /// Transient state — root scope, path_stack backing.
    arena: std.heap.ArenaAllocator,
    emitter: Emitter,
    program: *const ir.Program,
    module: *Module,
    root_scope: *Scope,
    /// Offset added to every IR-side `Slot.Global.index` to translate it
    /// into the runtime global slot. The IR's lowering scope only counts
    /// plain `VarDecl` symbols, but the runtime layout reserves indices
    /// 0..N-1 for anchor visit symbols (registered during prepass) and
    /// N.. for vars. So `runtime_index = ir_index + global_offset`.
    /// Set to `root_scope.count` after prepass completes.
    global_offset: C.GLOBAL = 0,

    pub fn emit(
        alloc: std.mem.Allocator,
        module: *Module,
        program: *const ir.Program,
    ) !Bytecode {
        var arena = std.heap.ArenaAllocator.init(alloc);
        errdefer arena.deinit();
        const root_scope = try Scope.create(arena.allocator(), null, .global);
        const emitter = try Emitter.init(alloc, module);

        var cg: Codegen = .{
            .alloc = alloc,
            .arena = arena,
            .emitter = emitter,
            .program = program,
            .module = module,
            .root_scope = root_scope,
        };
        // After bytecode() drains the chunk and constants via toOwnedSlice,
        // emitter.deinit() still has to free the *Chunk allocation itself
        // (and any error-path constants/buffers). So this always runs.
        defer cg.deinit();

        try cg.run();
        return try cg.bytecode();
    }

    fn deinit(self: *Codegen) void {
        self.emitter.deinit();
        self.arena.deinit();
    }

    fn run(self: *Codegen) Error!void {
        // Builtins are added first (mirrors compiler.zig:564-569). Their
        // constant indices are fixed by builtins.functions iteration order.
        for (builtins.functions.keys()) |name| {
            const obj = try self.alloc.create(Value.Obj);
            errdefer self.alloc.destroy(obj);
            obj.* = .{ .data = .{ .builtin = builtins.functions.get(name).? } };
            try self.emitter.addNamedConstant(self.arena.allocator(), name, .{ .obj = obj });
        }

        // Pre-walk: reserve constant-pool slots and visit symbols for
        // every declared anchor. Walks `program.body` in source order so
        // constant indices match the AST compiler's prepass.
        for (self.program.body) |*stmt| {
            try self.prepass(stmt);
        }
        // Vars come after every anchor visit symbol in the runtime
        // global layout. Lock the offset now that prepass is done.
        self.global_offset = @intCast(self.root_scope.count);

        // Two-pass top-level emission: non-bough first (so that the
        // first `.jump` in the bytecode stream is the bough skip-over-
        // body that `Vm.start` expects for `--bough` diverts), then
        // boughs.
        for (self.program.body) |*stmt| try self.emitNonBough(stmt);
        for (self.program.body) |*stmt| try self.emitBough(stmt);

        // Final `.end` opcode so the VM can grab the initial
        // jump_request after the program completes.
        const chunk = self.emitter.chunk;
        if (chunk.debug_markers.items.len > 0) {
            const dupe = chunk.debug_markers.items[chunk.debug_markers.items.len - 1];
            try chunk.debug_markers.ensureTotalCapacity(self.alloc, chunk.debug_markers.items.len + 1);
            chunk.debug_markers.appendAssumeCapacity(dupe);
            try chunk.instructions.append(self.alloc, @intFromEnum(OpCode.end));
        }
    }

    fn emitNonBough(self: *Codegen, stmt: *const ir.Stmt) Error!void {
        switch (stmt.kind) {
            .bough, .include => {},
            else => try self.compileStmt(stmt),
        }
    }

    fn emitBough(self: *Codegen, stmt: *const ir.Stmt) Error!void {
        switch (stmt.kind) {
            .bough => try self.compileStmt(stmt),
            .include => {},
            else => {},
        }
    }

    fn prepass(self: *Codegen, stmt: *const ir.Stmt) Error!void {
        switch (stmt.kind) {
            .include => {
                // IR lowering inlined included statements directly into
                // body, so the include marker has no children to descend
                // into here.
            },
            .function => |f| {
                // Top-level / method declarations carry an anchor; nested
                // function bindings (name_slot != null) live as locals
                // and have no constant slot.
                if (f.anchor) |a| {
                    try self.emitter.addNamedConstantTok(self.arena.allocator(), a.path, .nil);
                    if (f.is_extern) {
                        const ext = try self.alloc.create(Value.Obj);
                        errdefer self.alloc.destroy(ext);
                        ext.* = .{ .data = .{ .@"extern" = .{
                            .name = f.name,
                            .arity = @intCast(f.parameters.len),
                        } } };
                        _ = try self.emitter.addConstant(.{ .obj = ext });
                    }
                }
            },
            .class => |c| {
                try self.emitter.addNamedConstantTok(self.arena.allocator(), c.anchor.path, .nil);
            },
            .enum_decl => |e| {
                try self.emitter.addNamedConstantTok(self.arena.allocator(), e.anchor.path, .nil);
            },
            .bough => |b| {
                try self.registerAnchor(b.anchor.path, b.uuid, stmt.loc.start);
                for (b.body) |*s| try self.prepass(s);
            },
            .fork, .backup_fork => |f| {
                try self.registerAnchor(f.anchor.path, f.uuid, stmt.loc.start);
                for (f.body) |*s| try self.prepass(s);
            },
            .choice => |c| {
                try self.registerAnchor(c.anchor.path, c.uuid, stmt.loc.start);
                for (c.body) |*s| try self.prepass(s);
            },
            else => {},
        }
    }

    /// Register an anchor: define its visit symbol on the root scope and
    /// reserve a constants slot holding the Anchor Value.Obj. Mirrors
    /// `compiler.zig:registerAnchor`.
    fn registerAnchor(self: *Codegen, full_path: []const u8, uuid: UUID.ID, token: Token) Error!void {
        _ = token;
        if (self.emitter.constants_map.contains(full_path)) return error.SymbolAlreadyDeclared;

        const visit_sym = try self.root_scope.define(self.arena.allocator(), full_path, false);
        visit_sym.uuid = uuid;

        // Parent index: derived by stripping the last `.<segment>` from
        // the path. Top-level anchors have no parent.
        var parent_idx: ?C.CONSTANT = null;
        if (std.mem.lastIndexOfScalar(u8, full_path, '.')) |dot_pos| {
            const parent_path = full_path[0..dot_pos];
            parent_idx = self.emitter.constants_map.get(parent_path);
        }

        const anchor_obj = try self.alloc.create(Value.Obj);
        errdefer self.alloc.destroy(anchor_obj);
        anchor_obj.* = .{
            .id = uuid,
            .data = .{
                .anchor = .{
                    .name = try self.alloc.dupe(u8, full_path),
                    .uuid = uuid,
                    .visit_index = visit_sym.index,
                    .parent_anchor_index = parent_idx,
                    .ip = 0,
                },
            },
        };
        try self.emitter.addNamedConstantTok(self.arena.allocator(), full_path, .{ .obj = anchor_obj });
    }

    // =======================================================================
    // Statement emission
    // =======================================================================

    fn compileStmt(self: *Codegen, stmt: *const ir.Stmt) Error!void {
        const token = stmt.loc.start;
        switch (stmt.kind) {
            .expr_stmt => |e| {
                try self.compileExpr(e);
                try self.emitter.writeOp(.pop, token);
            },
            .var_decl => |v| try self.compileVarDecl(v, token),
            .return_value => |e| {
                try self.compileExpr(e);
                try self.emitter.writeOp(.return_value, token);
            },
            .return_void => try self.emitter.writeOp(.return_void, token),
            .fin => try self.emitter.writeOp(.fin, token),
            .@"break" => {
                try self.emitter.writeOp(.jump, token);
                _ = try self.emitter.writeInt(C.JUMP, BREAK_HOLDER, token);
            },
            .@"continue" => {
                try self.emitter.writeOp(.jump, token);
                _ = try self.emitter.writeInt(C.JUMP, CONTINUE_HOLDER, token);
            },
            .block => |b| {
                for (b.body) |*s| try self.compileStmt(s);
            },
            .include => {
                // Lowering inlined the included statements directly into
                // body; the marker has nothing to emit.
            },
            else => return error.NotYetImplemented,
        }
    }

    fn compileVarDecl(self: *Codegen, v: ir.VarDecl, token: Token) Error!void {
        try self.compileExpr(v.initializer);
        switch (v.slot) {
            .global => {
                // Define the symbol on root_scope so it appears in
                // bytecode.global_symbols. Its index lands at the
                // current root_scope.count, which equals
                // global_offset + slot.global.index by construction.
                const sym = self.root_scope.define(self.arena.allocator(), v.name, v.is_mutable) catch
                    return error.SymbolAlreadyDeclared;
                try self.emitter.writeOp(.decl_global, token);
                _ = try self.emitter.writeInt(C.GLOBAL, sym.index, token);
            },
            .local => |l| {
                try self.emitter.writeOp(.set_local, token);
                _ = try self.emitter.writeInt(C.LOCAL, l.index, token);
            },
            .upvalue => unreachable, // upvalues are never declaration sites
        }
    }

    // =======================================================================
    // Expression emission
    // =======================================================================

    fn compileExpr(self: *Codegen, expr: ir.ExprRef) Error!void {
        const token = expr.loc.start;
        switch (expr.kind) {
            .number => |n| {
                const i = try self.emitter.addLiteralConstant(self.arena.allocator(), .{ .number = n });
                try self.emitter.writeOp(.constant, token);
                _ = try self.emitter.writeInt(C.CONSTANT, i, token);
            },
            .bool => |b| try self.emitter.writeOp(if (b) .true else .false, token),
            .nil => try self.emitter.writeOp(.nil, token),
            .load => |slot| try self.emitLoad(slot, token),
            .load_const => |lc| {
                const idx = self.emitter.constants_map.get(lc.target.path) orelse
                    return error.NotYetImplemented;
                try self.emitter.writeOp(.constant, token);
                _ = try self.emitter.writeInt(C.CONSTANT, idx, token);
            },
            .un_op => |u| {
                try self.compileExpr(u.operand);
                switch (u.op) {
                    .negate => try self.emitter.writeOp(.negate, token),
                    .not => try self.emitter.writeOp(.not, token),
                }
            },
            .bin_op => |bin| try self.compileBinOp(bin, token),
            .if_expr => |ie| {
                try self.compileExpr(ie.condition);
                const false_jp = try self.emitter.emitJump(.jump_if_false, token);
                try self.compileExpr(ie.then_value);
                const end_jp = try self.emitter.emitJump(.jump, token);
                try self.emitter.patchJump(false_jp);
                try self.compileExpr(ie.else_value);
                try self.emitter.patchJump(end_jp);
            },
            .range => |r| {
                try self.compileExpr(r.right);
                try self.compileExpr(r.left);
                try self.emitter.writeOp(.range, token);
            },
            else => return error.NotYetImplemented,
        }
    }

    fn compileBinOp(self: *Codegen, bin: ir.BinOp, token: Token) Error!void {
        // `<` / `<=` are emitted as `>` / `>=` with operands swapped, to
        // keep the runtime opcode set small (mirrors compiler.zig:1504).
        if (bin.op == .less_than or bin.op == .less_than_equal) {
            try self.compileExpr(bin.right);
            try self.compileExpr(bin.left);
            try self.emitter.writeOp(switch (bin.op) {
                .less_than => .greater_than,
                .less_than_equal => .greater_than_equal,
                else => unreachable,
            }, token);
            return;
        }
        // Assignment ops belong to Step 8.
        if (isAssignOp(bin.op)) return error.NotYetImplemented;
        try self.compileExpr(bin.left);
        try self.compileExpr(bin.right);
        const op: OpCode = switch (bin.op) {
            .add => .add,
            .subtract => .subtract,
            .multiply => .multiply,
            .divide => .divide,
            .modulus => .modulus,
            .equal => .equal,
            .not_equal => .not_equal,
            .greater_than => .greater_than,
            .greater_than_equal => .greater_than_equal,
            .@"or" => .@"or",
            .@"and" => .@"and",
            else => return error.NotYetImplemented,
        };
        try self.emitter.writeOp(op, token);
    }

    fn emitLoad(self: *Codegen, slot: ir.Slot, token: Token) Error!void {
        switch (slot) {
            .local => |l| {
                try self.emitter.writeOp(.get_local, token);
                _ = try self.emitter.writeInt(C.LOCAL, l.index, token);
            },
            .upvalue => |u| {
                try self.emitter.writeOp(.get_upvalue, token);
                _ = try self.emitter.writeInt(u8, u.depth, token);
                _ = try self.emitter.writeInt(C.LOCAL, u.index, token);
            },
            .global => |g| {
                try self.emitter.writeOp(.get_global, token);
                _ = try self.emitter.writeInt(C.GLOBAL, g.index + self.global_offset, token);
            },
        }
    }

    // =======================================================================
    // Bytecode finalization
    // =======================================================================

    fn bytecode(self: *Codegen) !Bytecode {
        const global_symbols = try self.alloc.alloc(Bytecode.GlobalSymbol, self.root_scope.symbols.count());
        var filled: usize = 0;
        errdefer {
            for (global_symbols[0..filled]) |gs| self.alloc.free(gs.name);
            self.alloc.free(global_symbols);
        }
        for (self.root_scope.symbols.values(), 0..) |s, i| {
            global_symbols[i] = .{
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
            .locals_count = 0,
        };
    }
};

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
