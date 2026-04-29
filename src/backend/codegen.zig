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

        // Body emission is implemented in subsequent steps.
        if (self.program.body.len != 0) return error.NotYetImplemented;
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
