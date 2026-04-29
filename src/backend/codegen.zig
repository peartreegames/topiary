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

const emit = @import("emit.zig");
const Emitter = emit.Emitter;

const Bytecode = @import("bytecode.zig").Bytecode;

pub const Error = error{
    OutOfMemory,
    NotYetImplemented,
};

pub const Codegen = struct {
    pub fn emit(
        alloc: std.mem.Allocator,
        module: *Module,
        program: *const ir.Program,
    ) Error!Bytecode {
        _ = alloc;
        _ = module;
        _ = program;
        return error.NotYetImplemented;
    }
};
