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

const emit_mod = @import("emit.zig");
const Emitter = emit_mod.Emitter;
const JumpPatch = emit_mod.JumpPatch;
const replaceJumps = emit_mod.replaceJumps;
const BREAK_HOLDER = emit_mod.BREAK_HOLDER;
const CONTINUE_HOLDER = emit_mod.CONTINUE_HOLDER;
const SWITCH_END_HOLDER = emit_mod.SWITCH_END_HOLDER;
const JUMP_HOLDER = emit_mod.JUMP_HOLDER;
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
    /// Set to `root_scope.count` after prepass completes (= number of
    /// anchor visit symbols, before any global var_decl is defined).
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
            .@"if" => |i| try self.compileIf(i, token),
            .@"while" => |w| try self.compileWhile(w, token),
            .@"for" => |f| try self.compileFor(f, token),
            .@"switch" => |s| try self.compileSwitch(s, token),
            .include => {
                // Lowering inlined the included statements directly into
                // body; the marker has nothing to emit.
            },
            else => return error.NotYetImplemented,
        }
    }

    fn compileIf(self: *Codegen, i: ir.IfStmt, token: Token) Error!void {
        try self.compileExpr(i.condition);
        const false_jp = try self.emitter.emitJump(.jump_if_false, token);
        for (i.then_branch) |*s| try self.compileStmt(s);
        const end_jp = try self.emitter.emitJump(.jump, token);
        try self.emitter.patchJump(false_jp);
        if (i.else_branch) |eb| {
            for (eb) |*s| try self.compileStmt(s);
            try self.emitter.patchJump(end_jp);
        } else {
            // Mirrors compiler.zig:693-697: when there's no else, push
            // nil and pop so the if's stack effect matches an
            // expression-style if.
            try self.emitter.writeOp(.nil, token);
            try self.emitter.writeOp(.pop, token);
            try self.emitter.patchJump(end_jp);
        }
    }

    fn compileWhile(self: *Codegen, w: ir.WhileStmt, token: Token) Error!void {
        const start = self.emitter.instructionPos();
        try self.compileExpr(w.condition);
        const exit_jp = try self.emitter.emitJump(.jump_if_false, token);

        for (w.body) |*s| try self.compileStmt(s);
        try self.emitter.writeOp(.jump, token);
        _ = try self.emitter.writeInt(C.JUMP, start, token);

        const end = self.emitter.instructionPos();
        try self.emitter.patchJumpTo(exit_jp, end);

        try replaceJumps(self.emitter.chunk.instructions.items[start..], BREAK_HOLDER, end);
        try replaceJumps(self.emitter.chunk.instructions.items[start..], CONTINUE_HOLDER, start);
    }

    fn compileFor(self: *Codegen, f: ir.ForStmt, token: Token) Error!void {
        try self.compileExpr(f.iterator);
        try self.emitter.writeOp(.iter_start, token);
        const start = self.emitter.instructionPos();

        try self.emitter.writeOp(.iter_next, token);
        const exit_jp = try self.emitter.emitJump(.jump_if_false, token);

        // The capture binding's slot is already resolved by IR lowering.
        try self.emitter.writeOp(.set_local, token);
        _ = try self.emitter.writeInt(C.LOCAL, f.capture_slot.local.index, token);

        for (f.body) |*s| try self.compileStmt(s);
        try self.emitter.writeOp(.jump, token);
        _ = try self.emitter.writeInt(C.JUMP, start, token);

        const end = self.emitter.instructionPos();
        try self.emitter.patchJumpTo(exit_jp, end);
        try replaceJumps(self.emitter.chunk.instructions.items[start..], BREAK_HOLDER, end);
        try replaceJumps(self.emitter.chunk.instructions.items[start..], CONTINUE_HOLDER, start);

        try self.emitter.writeOp(.pop, token);
        try self.emitter.writeOp(.iter_end, token);
        // pop the iterator value
        try self.emitter.writeOp(.pop, token);
    }

    fn compileSwitch(self: *Codegen, s: ir.SwitchStmt, token: Token) Error!void {
        const start = self.emitter.instructionPos();
        try self.compileExpr(s.capture);

        var prong_jumps = try self.alloc.alloc(JumpPatch, s.prongs.len);
        defer self.alloc.free(prong_jumps);
        var inferred_else_jump: ?JumpPatch = null;

        var has_else = false;
        for (s.prongs, 0..) |prong, i| {
            if (prong.values) |vals| {
                for (vals) |v| try self.compileExpr(v);
            } else {
                has_else = true;
            }
            prong_jumps[i] = try self.emitter.emitJump(.prong, prong.loc.start);
            const value_count: u8 = if (prong.values) |v| @intCast(v.len) else 0;
            _ = try self.emitter.writeInt(u8, value_count, prong.loc.start);
        }
        if (!has_else) {
            const jp = try self.emitter.emitJump(.prong, token);
            _ = try self.emitter.writeInt(u8, 0, token);
            inferred_else_jump = jp;
        }

        for (s.prongs, 0..) |prong, i| {
            try self.emitter.patchJump(prong_jumps[i]);
            for (prong.body) |*b| try self.compileStmt(b);
            try self.emitter.writeOp(.jump, prong.loc.start);
            _ = try self.emitter.writeInt(C.JUMP, SWITCH_END_HOLDER, prong.loc.start);
        }

        if (inferred_else_jump) |jp| {
            try self.emitter.patchJump(jp);
            // Empty inferred-else body — just jump straight to the end.
            try self.emitter.writeOp(.jump, token);
            _ = try self.emitter.writeInt(C.JUMP, SWITCH_END_HOLDER, token);
        }

        try replaceJumps(self.emitter.chunk.instructions.items[start..], SWITCH_END_HOLDER, self.emitter.instructionPos());
        try self.emitter.writeOp(.pop, token);
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
            .text => |segs| try self.compileText(segs, token),
            .list => |items| try self.compileCollection(.list, items, token),
            .set => |items| try self.compileCollection(.set, items, token),
            .map => |pairs| try self.compileMap(pairs, token),
            .index => |idx| {
                try self.compileExpr(idx.target);
                try self.compileExpr(idx.index);
                try self.emitter.writeOp(.index, token);
            },
            .field => |f| try self.compileField(expr, f, token),
            .instance => |ins| try self.compileInstance(ins, token),
            .call => |c| try self.compileCall(c, token),
            else => return error.NotYetImplemented,
        }
    }

    fn compileText(self: *Codegen, segments: []const ir.TextSegment, token: Token) Error!void {
        // Push interp values in segment order. The reconstructed string
        // uses `{N}` to mark each interp by its position in the push
        // sequence — same convention the runtime expects.
        var interp_count: usize = 0;
        for (segments) |seg| {
            switch (seg) {
                .interp => |e| {
                    try self.compileExpr(e);
                    interp_count += 1;
                },
                .literal => {},
            }
        }

        // Reconstruct the joined string.
        var buf: std.ArrayList(u8) = .empty;
        errdefer buf.deinit(self.alloc);
        var idx: usize = 0;
        for (segments) |seg| {
            switch (seg) {
                .literal => |s| try buf.appendSlice(self.alloc, s),
                .interp => {
                    var num_buf: [32]u8 = undefined;
                    const n = std.fmt.bufPrint(&num_buf, "{d}", .{idx}) catch unreachable;
                    try buf.append(self.alloc, '{');
                    try buf.appendSlice(self.alloc, n);
                    try buf.append(self.alloc, '}');
                    idx += 1;
                },
            }
        }
        const obj = try self.alloc.create(Value.Obj);
        obj.* = .{ .data = .{ .string = try buf.toOwnedSlice(self.alloc) } };
        const c_idx = try self.emitter.addConstant(.{ .obj = obj });
        try self.emitter.writeOp(.string, token);
        _ = try self.emitter.writeInt(C.CONSTANT, c_idx, token);
        _ = try self.emitter.writeInt(u8, @intCast(interp_count), token);
    }

    fn compileCollection(
        self: *Codegen,
        comptime op: OpCode,
        items: []const ir.ExprRef,
        token: Token,
    ) Error!void {
        for (items) |item| try self.compileExpr(item);
        try self.emitter.writeOp(op, token);
        _ = try self.emitter.writeInt(C.COLLECTION, @intCast(items.len), token);
    }

    fn compileMap(self: *Codegen, pairs: []const ir.MapPair, token: Token) Error!void {
        for (pairs) |p| {
            try self.compileExpr(p.key);
            try self.compileExpr(p.value);
        }
        try self.emitter.writeOp(.map, token);
        _ = try self.emitter.writeInt(C.COLLECTION, @intCast(pairs.len), token);
    }

    fn compileField(self: *Codegen, expr: ir.ExprRef, f: ir.Field, token: Token) Error!void {
        // Dot-access into a constant-pool entry can resolve to another
        // anchor (e.g. `Parent.Child`) — emit get_global on its visit
        // index instead of an `.index` lookup.
        if (try self.resolveAnchorChain(expr)) |anchor_idx| {
            const anchor = self.emitter.constants.items[anchor_idx].obj.data.anchor;
            try self.emitter.writeOp(.get_global, token);
            _ = try self.emitter.writeInt(C.GLOBAL, anchor.visit_index, token);
            return;
        }
        try self.compileExpr(f.target);
        try self.emitter.addIdentifierConstant(self.arena.allocator(), f.name, token);
        try self.emitter.writeOp(.index, token);
    }

    fn compileInstance(self: *Codegen, ins: ir.Instance, token: Token) Error!void {
        const const_idx = self.emitter.constants_map.get(ins.class.path) orelse
            return error.NotYetImplemented;

        for (ins.fields, 0..) |field_expr, i| {
            try self.compileExpr(field_expr);
            try self.emitter.addIdentifierConstant(self.arena.allocator(), ins.field_names[i], token);
        }
        try self.emitter.writeOp(.constant, token);
        _ = try self.emitter.writeInt(C.CONSTANT, const_idx, token);
        try self.emitter.writeOp(.instance, token);
        _ = try self.emitter.writeInt(C.FIELDS, @intCast(ins.fields.len), token);
    }

    fn compileCall(self: *Codegen, c: ir.Call, token: Token) Error!void {
        try self.compileExpr(c.target);
        for (c.arguments) |arg| try self.compileExpr(arg);
        try self.emitter.writeOp(.call, token);
        // Method calls (target is `index` or `field`) include the
        // receiver as an additional argument on the stack.
        var length = c.arguments.len;
        if (c.target.kind == .index or c.target.kind == .field) length += 1;
        std.debug.assert(c.arguments.len < std.math.maxInt(C.ARGS));
        _ = try self.emitter.writeInt(C.ARGS, @intCast(length), token);
    }

    /// Walk a `.field` chain and check whether the fully-qualified path
    /// is a registered anchor. Returns its constant-pool index if so.
    /// Mirrors `compiler.zig:flattenIndexer + resolveAnchor`.
    fn resolveAnchorChain(self: *Codegen, expr: ir.ExprRef) Error!?C.CONSTANT {
        var parts: std.ArrayList([]const u8) = .empty;
        defer parts.deinit(self.alloc);

        var cur: ir.ExprRef = expr;
        while (true) {
            switch (cur.kind) {
                .field => |f| {
                    try parts.append(self.alloc, f.name);
                    cur = f.target;
                },
                .load_const => |lc| {
                    // Reached the chain head: a constant-pool name.
                    // The path so far is `lc.target.path . parts...`
                    // but we accumulated suffixes only — split lc's path
                    // and prepend so the lookup is the full path.
                    try parts.append(self.alloc, lc.target.path);
                    break;
                },
                .load => |slot| {
                    // Reached an identifier load. Use the slot's name as
                    // the chain head so e.g. `MyEnum.Value` works when
                    // `MyEnum` was pulled in via load (not load_const).
                    const name = switch (slot) {
                        .local => |l| l.name,
                        .upvalue => |u| u.name,
                        .global => |g| g.name,
                    };
                    if (name.len == 0) return null;
                    try parts.append(self.alloc, name);
                    break;
                },
                else => return null,
            }
        }
        // parts is in reverse order: [field_n ... field_1, head_path].
        // The head may itself contain dots (anchor.path). Build the
        // candidate path by reversing and joining.
        std.mem.reverse([]const u8, parts.items);
        const path = try std.mem.join(self.alloc, ".", parts.items);
        defer self.alloc.free(path);

        if (self.emitter.constants_map.get(path)) |i| {
            const v = self.emitter.constants.items[i];
            if (v == .obj and v.obj.data == .anchor) return i;
        }
        return null;
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
            .locals_count = self.computeTopLevelLocalsCount(),
        };
    }

    /// Returns the runtime stack-frame size for top-level code.
    ///
    /// Mirrors the AST compiler quirk: `Scope.create(.local)` inherits
    /// `parent.count`, so a local scope nested under the global root
    /// includes every global slot in its `count`. `exitScope` then
    /// updates `locals_count = max(locals_count, old_scope.count)`.
    /// This means an empty `for`/`while`/`switch`-prong nested at top
    /// level still bumps `locals_count` to the number of globals seen
    /// so far. We replicate that walk here so byte-parity holds.
    ///
    /// `current` starts at the number of anchor visit symbols
    /// (root_scope.count after prepass) and grows by 1 per global
    /// `var_decl` as we walk in source order.
    fn computeTopLevelLocalsCount(self: *Codegen) usize {
        var hi: usize = 0;
        // Start at the post-prepass anchor count. Var_decls increment as
        // we walk, mirroring the AST's incremental scope tracking. Using
        // `root_scope.count` post-emission would double-count globals.
        var current: usize = self.global_offset;
        computeBodyLocals(self.program.body, &current, &hi);
        return hi;
    }
};

/// Walks `body` updating `current` and `hi` to mirror the AST
/// compiler's scope-count tracking. Local-scope bodies (for/while/
/// switch/etc.) save and restore `current` around the body — vars
/// declared inside them go out of scope on exit. `if`/`block` do NOT
/// open a new scope in the AST compiler, so their bodies extend the
/// outer count.
fn computeBodyLocals(body: []const ir.Stmt, current: *usize, hi: *usize) void {
    for (body) |*s| {
        switch (s.kind) {
            .var_decl => current.* += 1,
            .@"for" => |f| {
                const saved = current.*;
                current.* += 1; // capture binding
                if (current.* > hi.*) hi.* = current.*;
                computeBodyLocals(f.body, current, hi);
                current.* = saved;
            },
            .@"while" => |w| {
                const saved = current.*;
                if (current.* > hi.*) hi.* = current.*;
                computeBodyLocals(w.body, current, hi);
                current.* = saved;
            },
            .@"switch" => |sw| for (sw.prongs) |p| {
                const saved = current.*;
                if (current.* > hi.*) hi.* = current.*;
                computeBodyLocals(p.body, current, hi);
                current.* = saved;
            },
            .@"if" => |i| {
                computeBodyLocals(i.then_branch, current, hi);
                if (i.else_branch) |eb| computeBodyLocals(eb, current, hi);
            },
            .block => |b| computeBodyLocals(b.body, current, hi),
            .bough => |b| computeBodyLocals(b.body, current, hi),
            .fork, .backup_fork => |f| computeBodyLocals(f.body, current, hi),
            .choice => |c| computeBodyLocals(c.body, current, hi),
            // Function bodies have their own frame, skip.
            .function => {},
            else => {},
        }
    }
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
