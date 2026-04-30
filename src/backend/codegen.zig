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
const Function = types.Function;
const Class = types.Class;
const Enum = types.Enum;

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

    pub fn emit(
        alloc: std.mem.Allocator,
        module: *Module,
        program: *const ir.Program,
    ) !Bytecode {
        var arena = std.heap.ArenaAllocator.init(alloc);
        // Pre-cg failure paths need explicit cleanup since cg.deinit isn't
        // deferred yet. Once cg owns arena+emitter, defer cg.deinit() is
        // the sole owner — using errdefer here would double-free with the
        // defer on the error path.
        const root_scope = Scope.create(arena.allocator(), null, .global) catch |e| {
            arena.deinit();
            return e;
        };
        const emitter = Emitter.init(alloc, module) catch |e| {
            arena.deinit();
            return e;
        };

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
        // visit indices align with what lowering pre-counted (lowering
        // seeded `root_scope.count` with the same anchor total, so a
        // var's IR slot index already lands at the right runtime slot).
        for (self.program.body) |*stmt| {
            try self.prepass(stmt);
        }

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
            .function => |f| try self.compileFunctionStmt(f, token),
            .class => |c| try self.compileClassStmt(c, token),
            .enum_decl => |e| try self.compileEnumStmt(e, token),
            .line => |l| try self.compileLine(l, token),
            .choice => |c| try self.compileChoice(c, token),
            .fork => |f| try self.compileFork(f, false, token),
            .backup_fork => |f| try self.compileFork(f, true, token),
            .divert => |d| try self.compileDivert(d, false, token),
            .backup_divert => |d| try self.compileDivert(d, true, token),
            .bough => |b| try self.compileBough(b, token),
            .visit => |v| {
                const idx = try self.anchorIdx(v.target.path);
                try self.compileVisit(idx, token);
            },
            .include => {
                // Lowering inlined the included statements directly into
                // body; the marker has nothing to emit.
            },
        }
    }

    // =======================================================================
    // Narrative emission
    // =======================================================================

    fn anchorIdx(self: *Codegen, path: []const u8) Error!C.CONSTANT {
        return self.emitter.constants_map.get(path) orelse {
            // Lowering should have caught unresolved anchors. If we get
            // here it's a codegen bug, not a writer error.
            return error.NotYetImplemented;
        };
    }

    fn compileVisit(self: *Codegen, anchor_idx: C.CONSTANT, token: Token) Error!void {
        try self.emitter.writeOp(.visit, token);
        _ = try self.emitter.writeInt(C.CONSTANT, anchor_idx, token);
    }

    /// Common pattern for `.loc`-bearing nodes (line + choice). Pushes
    /// each interp value, builds the joined `{N}`-marked string, and
    /// emits `.loc <const_idx> <interp_count>`. Returns the interp
    /// count for callers that still need to write follow-up trailers.
    fn emitLocString(
        self: *Codegen,
        segments: []const ir.TextSegment,
        uuid: UUID.ID,
        token: Token,
    ) Error!u8 {
        var interp_count: u8 = 0;
        for (segments) |seg| {
            switch (seg) {
                .interp => |e| {
                    try self.compileExpr(e);
                    interp_count += 1;
                },
                .literal => {},
            }
        }
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
        obj.* = .{
            .id = uuid,
            .data = .{ .string = try buf.toOwnedSlice(self.alloc) },
        };
        const c_idx = try self.emitter.addConstant(.{ .obj = obj });
        try self.emitter.writeOp(.loc, token);
        _ = try self.emitter.writeInt(C.CONSTANT, c_idx, token);
        _ = try self.emitter.writeInt(u8, interp_count, token);
        return interp_count;
    }

    fn compileLine(self: *Codegen, line: ir.Line, token: Token) Error!void {
        for (line.tags) |tag| {
            try self.emitter.addIdentifierConstant(self.arena.allocator(), tag.name, token);
        }
        _ = try self.emitLocString(line.segments, line.uuid, token);
        if (line.speaker) |speaker| {
            try self.emitter.addIdentifierConstant(self.arena.allocator(), speaker, token);
        }
        try self.emitter.writeOp(.dialogue, token);
        _ = try self.emitter.writeInt(u8, if (line.speaker == null) 0 else 1, token);
        _ = try self.emitter.writeInt(u8, @intCast(line.tags.len), token);
    }

    fn compileChoice(self: *Codegen, c: ir.Choice, token: Token) Error!void {
        for (c.tags) |tag| {
            try self.emitter.addIdentifierConstant(self.arena.allocator(), tag.name, token);
        }
        const anchor_idx = try self.anchorIdx(c.anchor.path);

        // The choice's anchor entry IP is the position before the loc;
        // codegen patches the IP on the Anchor Value.Obj.
        const entry_ip = self.emitter.instructionPos();
        self.emitter.constants.items[anchor_idx].obj.data.anchor.ip = entry_ip;

        _ = try self.emitLocString(c.segments, c.uuid, token);

        const choice_jp = try self.emitter.emitJump(.choice, token);
        _ = try self.emitter.writeInt(u8, if (c.is_unique) 1 else 0, token);
        _ = try self.emitter.writeInt(C.CONSTANT, anchor_idx, token);
        _ = try self.emitter.writeInt(u8, @intCast(c.tags.len), token);

        const end_jp = try self.emitter.emitJump(.jump, token);
        try self.emitter.patchJump(choice_jp);

        // The IR injects `.visit` as the first stmt of every choice body
        // (lower.zig:lowerChoiceStmt), so we don't auto-emit one here —
        // descending into body covers it.
        for (c.body) |*s| try self.compileStmt(s);
        try self.emitter.writeOp(.end, token);
        try self.emitter.patchJump(end_jp);
    }

    fn compileFork(self: *Codegen, f: ir.Fork, is_backup: bool, token: Token) Error!void {
        const anchor_idx = try self.anchorIdx(f.anchor.path);
        const start_pos = self.emitter.instructionPos();
        self.emitter.constants.items[anchor_idx].obj.data.anchor.ip = start_pos;
        try self.compileVisit(anchor_idx, token);

        for (f.body) |*s| try self.compileStmt(s);

        var backup_jp: ?JumpPatch = null;
        if (is_backup) {
            backup_jp = try self.emitter.emitJump(.backup, token);
            _ = try self.emitter.writeInt(u8, 1, token); // 1 = fork backup
        }
        try self.emitter.writeOp(.fork, token);
        const end_pos = self.emitter.instructionPos();
        if (backup_jp) |jp| try self.emitter.patchJumpTo(jp, end_pos);
    }

    fn compileDivert(self: *Codegen, d: ir.Divert, is_backup: bool, token: Token) Error!void {
        const anchor_idx = try self.anchorIdx(d.target.path);
        if (is_backup) {
            const backup_jp = try self.emitter.emitJump(.backup, token);
            _ = try self.emitter.writeInt(u8, 0, token); // 0 = divert backup
            try self.emitter.writeOp(.divert, token);
            _ = try self.emitter.writeInt(C.CONSTANT, anchor_idx, token);
            try self.emitter.patchJump(backup_jp);
        } else {
            try self.emitter.writeOp(.divert, token);
            _ = try self.emitter.writeInt(C.CONSTANT, anchor_idx, token);
        }
    }

    fn compileBough(self: *Codegen, b: ir.Bough, token: Token) Error!void {
        const anchor_idx = try self.anchorIdx(b.anchor.path);
        const skip_jp = try self.emitter.emitJump(.jump, token);

        const entry_ip = self.emitter.instructionPos();
        self.emitter.constants.items[anchor_idx].obj.data.anchor.ip = entry_ip;
        // The IR injects `.visit` as the first stmt of every bough body
        // (lower.zig:lowerBoughStmt), so we don't auto-emit one here —
        // descending into body covers it.

        for (b.body) |*s| try self.compileStmt(s);
        try self.emitter.writeOp(.end, token);

        const end = self.emitter.instructionPos();
        try self.emitter.patchJumpTo(skip_jp, end);
    }

    fn compileFunctionStmt(self: *Codegen, f: ir.FunctionDecl, token: Token) Error!void {
        // Extern functions: prepass already added a separate extern
        // Value.Obj for FFI dispatch. The named slot still gets a
        // compiled function obj (whose body is whatever the writer
        // wrote — typically a stub the FFI override replaces).
        const obj = try self.compileFunctionToObj(f, token);
        if (f.anchor) |a| {
            self.emitter.replaceConstant(a.path, .{ .obj = obj }) catch {};
        }
        // Nested-function bindings (name_slot != null) are not yet
        // supported in this codegen — they require an explicit closure
        // emission path. Leaving the obj orphaned would leak; the
        // current AST compiler also doesn't emit closures, so we mirror
        // that gap. If the IR ever lowers a true nested function, this
        // path will need extending.
    }

    fn compileFunctionToObj(self: *Codegen, f: ir.FunctionDecl, token: Token) Error!*Value.Obj {
        try self.emitter.enterChunk();

        for (f.body) |*s| try self.compileStmt(s);

        // Ensure every function ends with a return — matches the AST
        // compiler at compiler.zig:1346.
        if (!(try self.emitter.lastIs(.return_value)) and !(try self.emitter.lastIs(.return_void))) {
            try self.emitter.writeOp(.return_void, token);
        }

        const chunk = try self.emitter.exitChunk();
        defer chunk.deinit();

        // Methods receive an implicit `self` as the first parameter —
        // bumps reported arity by one.
        var arity: u8 = @intCast(f.parameters.len);
        if (f.is_method) arity += 1;

        const obj = try self.alloc.create(Value.Obj);
        errdefer self.alloc.destroy(obj);
        obj.* = .{
            .id = UUID.new(),
            .data = .{
                .function = .{
                    .name = if (f.name.len > 0) try self.alloc.dupe(u8, f.name) else null,
                    .arity = arity,
                    .is_method = f.is_method,
                    .instructions = try chunk.instructions.toOwnedSlice(self.alloc),
                    .debug_info = try chunk.debugInfo(self.alloc),
                    .locals_count = f.locals_count,
                },
            },
        };
        return obj;
    }

    fn compileClassStmt(self: *Codegen, c: ir.ClassDecl, token: Token) Error!void {
        const class_name = try self.alloc.dupe(u8, c.name);
        errdefer self.alloc.free(class_name);

        var fields = try self.alloc.alloc(Class.Member, c.field_names.len);
        errdefer self.alloc.free(fields);
        var fields_filled: usize = 0;
        errdefer {
            for (fields[0..fields_filled]) |m| {
                self.alloc.free(m.name);
                m.value.destroyStatic(self.alloc);
            }
        }
        for (c.field_names, c.field_initializers, 0..) |name, init_expr, i| {
            const value = try self.evaluateLiteral(init_expr);
            errdefer value.destroyStatic(self.alloc);
            const dup_name = try self.alloc.dupe(u8, name);
            fields[i] = .{ .name = dup_name, .value = value };
            fields_filled = i + 1;
        }

        var methods = try self.alloc.alloc(Class.Member, c.methods.len);
        errdefer self.alloc.free(methods);
        var methods_filled: usize = 0;
        errdefer {
            for (methods[0..methods_filled]) |m| {
                self.alloc.free(m.name);
                m.value.destroy(self.alloc);
            }
        }
        for (c.methods, 0..) |method, i| {
            const fn_obj = try self.compileFunctionToObj(method, token);
            errdefer (Value{ .obj = fn_obj }).destroy(self.alloc);
            const dup_name = try self.alloc.dupe(u8, method.name);
            methods[i] = .{ .name = dup_name, .value = .{ .obj = fn_obj } };
            methods_filled = i + 1;
        }

        const class_data = try Class.init(class_name, fields, methods);
        const obj = try self.alloc.create(Value.Obj);
        errdefer self.alloc.destroy(obj);
        obj.* = .{
            .id = UUID.new(),
            .data = .{ .class = class_data },
        };
        self.emitter.replaceConstant(c.anchor.path, .{ .obj = obj }) catch {};
    }

    fn compileEnumStmt(self: *Codegen, e: ir.EnumDecl, token: Token) Error!void {
        _ = token;
        var values = try self.alloc.alloc([]const u8, e.values.len);
        errdefer self.alloc.free(values);
        var values_filled: usize = 0;
        errdefer for (values[0..values_filled]) |v| self.alloc.free(v);

        for (e.values, 0..) |v, i| {
            values[i] = try self.alloc.dupe(u8, v);
            values_filled = i + 1;
        }

        const enum_name = try self.alloc.dupe(u8, e.name);
        errdefer self.alloc.free(enum_name);

        const obj = try self.alloc.create(Value.Obj);
        errdefer self.alloc.destroy(obj);
        obj.* = .{
            .id = UUID.fromStringHash(e.name),
            .data = .{ .@"enum" = .{
                .name = enum_name,
                .values = values,
                .is_seq = e.is_seq,
            } },
        };
        self.emitter.replaceConstant(e.anchor.path, .{ .obj = obj }) catch {};
    }

    /// Static evaluation of literal-shape IR expressions to a `Value`.
    /// `validate.zig:checkStaticInitializer` already verified the input
    /// is a permissible literal-shape, so this function trusts its
    /// inputs and only handles the legal cases.
    fn evaluateLiteral(self: *Codegen, expr: ir.ExprRef) Error!Value {
        switch (expr.kind) {
            .number => |n| return .{ .number = n },
            .bool => |b| return .{ .bool = b },
            .nil => return .nil,
            .text => |segs| {
                // checkStaticInitializer rejects interpolated text, so
                // segments are guaranteed all-literal here.
                var len: usize = 0;
                for (segs) |s| switch (s) {
                    .literal => |l| len += l.len,
                    .interp => unreachable,
                };
                const buf = try self.alloc.alloc(u8, len);
                errdefer self.alloc.free(buf);
                var pos: usize = 0;
                for (segs) |s| switch (s) {
                    .literal => |l| {
                        @memcpy(buf[pos .. pos + l.len], l);
                        pos += l.len;
                    },
                    .interp => unreachable,
                };
                const obj = try self.alloc.create(Value.Obj);
                obj.* = .{ .data = .{ .string = buf } };
                return .{ .obj = obj };
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
                errdefer left.destroyStatic(self.alloc);
                const right = try self.evaluateLiteral(bin.right);
                return .{
                    .number = switch (bin.op) {
                        .add => left.number + right.number,
                        .subtract => left.number - right.number,
                        .multiply => left.number * right.number,
                        .divide => left.number / right.number,
                        else => unreachable,
                    },
                };
            },
            .load_const => |lc| {
                const idx = self.emitter.constants_map.get(lc.target.path).?;
                return self.emitter.constants.items[idx];
            },
            .field => |f| {
                // Enum or class static value access — validated to point
                // at an existing member.
                const target = try self.evaluateLiteral(f.target);
                if (target == .obj) switch (target.obj.data) {
                    .@"enum" => |e| {
                        for (e.values, 0..) |v, i| {
                            if (std.mem.eql(u8, v, f.name)) return .{ .number = @floatFromInt(i) };
                        }
                        unreachable;
                    },
                    .class => |cls| {
                        for (cls.fields) |m| {
                            if (std.mem.eql(u8, m.name, f.name)) return m.value;
                        }
                        unreachable;
                    },
                    else => unreachable,
                };
                unreachable;
            },
            .list => |items| {
                var list = try std.ArrayList(Value).initCapacity(self.alloc, items.len);
                errdefer {
                    for (list.items) |item| item.destroyStatic(self.alloc);
                    list.deinit(self.alloc);
                }
                for (items) |item| {
                    list.appendAssumeCapacity(try self.evaluateLiteral(item));
                }
                const obj = try self.alloc.create(Value.Obj);
                obj.* = .{ .id = UUID.new(), .data = .{ .list = list } };
                return .{ .obj = obj };
            },
            .set => |items| {
                var set = Value.Obj.SetType.empty;
                errdefer {
                    for (set.keys()) |k| k.destroyStatic(self.alloc);
                    set.deinit(self.alloc);
                }
                for (items) |item| {
                    try set.put(self.alloc, try self.evaluateLiteral(item), {});
                }
                const obj = try self.alloc.create(Value.Obj);
                obj.* = .{ .id = UUID.new(), .data = .{ .set = set } };
                return .{ .obj = obj };
            },
            .map => |pairs| {
                var map = Value.Obj.MapType.empty;
                errdefer {
                    var it = map.iterator();
                    while (it.next()) |entry| {
                        entry.key_ptr.*.destroyStatic(self.alloc);
                        entry.value_ptr.*.destroyStatic(self.alloc);
                    }
                    map.deinit(self.alloc);
                }
                for (pairs) |p| {
                    const k = try self.evaluateLiteral(p.key);
                    errdefer k.destroyStatic(self.alloc);
                    const v = try self.evaluateLiteral(p.value);
                    try map.put(self.alloc, k, v);
                }
                const obj = try self.alloc.create(Value.Obj);
                obj.* = .{ .id = UUID.new(), .data = .{ .map = map } };
                return .{ .obj = obj };
            },
            // Static evaluation should be guarded by
            // `validate.checkStaticInitializer`, but if a non-literal
            // slips through we'd rather surface NotYetImplemented than
            // hit `unreachable` in production.
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
            .global => |g| {
                // Define the symbol on root_scope so it appears in
                // bytecode.global_symbols. The index lands at the
                // current root_scope.count, which equals `g.index`
                // because lowering seeded the count with the anchor
                // visit-slot total before defining any var.
                _ = self.root_scope.define(self.arena.allocator(), v.name, v.is_mutable) catch
                    return error.SymbolAlreadyDeclared;
                try self.emitter.writeOp(.decl_global, token);
                _ = try self.emitter.writeInt(C.GLOBAL, g.index, token);
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
                // Anchor references resolve to the visit count (a number)
                // at runtime — load via get_global on the visit symbol's
                // index. Mirrors compiler.zig:loadSymbol's identifier
                // arm. Functions / classes / enums load as plain
                // constants.
                const v = self.emitter.constants.items[idx];
                if (v == .obj and v.obj.data == .anchor) {
                    try self.emitter.writeOp(.get_global, token);
                    _ = try self.emitter.writeInt(C.GLOBAL, v.obj.data.anchor.visit_index, token);
                } else {
                    try self.emitter.writeOp(.constant, token);
                    _ = try self.emitter.writeInt(C.CONSTANT, idx, token);
                }
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
            .field => |f| try self.compileField(f, token),
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

    fn compileField(self: *Codegen, f: ir.Field, token: Token) Error!void {
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
        if (isAssignOp(bin.op)) return self.compileAssign(bin, token);
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

    /// Lvalue-aware emission for `=` / `+=` / `-=` / `*=` / `/=` / `%=`.
    /// IR sets `bin.target_slot` when the LHS resolves to a slot;
    /// otherwise the LHS is an `.index` expression and we go through
    /// `set_property`. Mirrors compiler.zig's two assignment paths.
    fn compileAssign(self: *Codegen, bin: ir.BinOp, token: Token) Error!void {
        const arith_op: ?OpCode = switch (bin.op) {
            .assign_add => .add,
            .assign_subtract => .subtract,
            .assign_multiply => .multiply,
            .assign_divide => .divide,
            .assign_modulus => .modulus,
            .assign => null,
            else => unreachable,
        };

        if (bin.target_slot) |slot| {
            // identifier = ... | identifier op= ...
            if (arith_op) |op| {
                try self.compileExpr(bin.left);
                try self.compileExpr(bin.right);
                try self.emitter.writeOp(op, token);
            } else {
                try self.compileExpr(bin.right);
            }
            try self.emitSetSlot(slot, token);
            // Assignment is an expression — re-load so the value sits
            // on the stack for any enclosing expr_stmt to pop.
            try self.emitLoad(slot, token);
            return;
        }

        // Indexer/dot assignment: LHS is `.index` (`xs[0]`) or `.field`
        // (`obj.x`). Both produce a trailing `.index` opcode in the
        // compiled stream that we strip below before emitting
        // `.set_property`.
        std.debug.assert(bin.left.kind == .index or bin.left.kind == .field);

        if (arith_op) |op| {
            // Compound: load current value, add, dup, recompile lvalue
            // sans the trailing `.index` op, then set_property.
            try self.compileExpr(bin.left);
            try self.compileExpr(bin.right);
            try self.emitter.writeOp(op, token);
        } else {
            try self.compileExpr(bin.right);
        }
        try self.emitter.writeOp(.dup, token);
        try self.compileExpr(bin.left);
        try self.emitter.removeLast(.index);
        try self.emitter.writeOp(.set_property, token);
    }

    fn emitSetSlot(self: *Codegen, slot: ir.Slot, token: Token) Error!void {
        switch (slot) {
            .local => |l| {
                try self.emitter.writeOp(.set_local, token);
                _ = try self.emitter.writeInt(C.LOCAL, l.index, token);
            },
            .upvalue => |u| {
                try self.emitter.writeOp(.set_upvalue, token);
                _ = try self.emitter.writeInt(u8, u.depth, token);
                _ = try self.emitter.writeInt(C.LOCAL, u.index, token);
            },
            .global => |g| {
                try self.emitter.writeOp(.set_global, token);
                _ = try self.emitter.writeInt(C.GLOBAL, g.index, token);
            },
        }
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
                _ = try self.emitter.writeInt(C.GLOBAL, g.index, token);
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
            .locals_count = self.program.top_level_locals_count,
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
