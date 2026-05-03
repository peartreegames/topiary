//! Bytecode emission machinery used by `codegen.zig`.
//!
//! `Emitter` owns the active chunk and the constant pool. It exposes the
//! primitives codegen needs to write opcodes, integers, values, and
//! placeholder jumps; manage chunk enter/exit; and register constants.
//!
//! `Emitter` is purely about producing bytecode bytes. It does NOT emit
//! diagnostics, walk an IR, resolve scopes, or know about anchor paths.
//! Diagnostic emission stays on the consumer.
//!
//! Memory contract:
//!   - `alloc` outlives the emitter and owns the chunk instruction
//!     buffers and the constants list. These transfer to `Bytecode` via
//!     `toOwnedSlice` on success.
//!   - `arena_alloc` is borrowed from the consumer; the constants_map
//!     keys and literal_cache live on it. Freed by the consumer's arena.

const std = @import("std");

const utils = @import("../utils/index.zig");
const C = utils.C;

const types = @import("../types/index.zig");
const Value = types.Value;

const Token = @import("../frontend/token.zig").Token;

const mod = @import("../module.zig");
const Module = mod.Module;

const DebugInfo = @import("debug.zig").DebugInfo;
const OpCode = @import("opcode.zig").OpCode;

// Placeholder values for jump targets that get patched later.
// Use values near maxInt(u32) to avoid collisions with real instruction
// addresses.
const max_jump = std.math.maxInt(C.JUMP);
pub const BREAK_HOLDER: C.JUMP = max_jump - 0;
pub const CONTINUE_HOLDER: C.JUMP = max_jump - 1;
pub const SWITCH_END_HOLDER: C.JUMP = max_jump - 2;
pub const JUMP_HOLDER: C.JUMP = max_jump - 3;

pub const Chunk = struct {
    instructions: std.ArrayList(u8) = .empty,
    /// One marker per `(file_index, line)` run, in emit order. The
    /// run starts at `ip` (a byte position in `instructions`) and
    /// extends to the next marker's `ip` — or, for the last marker,
    /// to `instructions.items.len`. Operand bytes inherit the
    /// most-recent marker, so this list grows by at most one entry
    /// per source line, not per instruction byte.
    debug_markers: std.ArrayList(Marker) = .empty,
    parent: ?*Chunk,
    module: *Module,
    alloc: std.mem.Allocator,
    last_op_pos: ?usize = null,

    const Marker = struct {
        ip: u32,
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
        // Root chunk ends up holding most of the bytecode. Pre-size from
        // total source bytes to avoid ArrayList doubling reallocations.
        if (parent == null) {
            const estimate = module.timings.source_bytes / 4;
            if (estimate > 0) {
                try chunk.instructions.ensureTotalCapacity(allocator, estimate);
                try chunk.debug_markers.ensureTotalCapacity(allocator, estimate / 4);
            }
        }
        return chunk;
    }

    pub fn debugInfo(self: *Chunk, allocator: std.mem.Allocator) ![]DebugInfo {
        var infos: std.ArrayList(DebugInfo) = .empty;
        // `infos.deinit` only frees the list backing — each DebugInfo
        // owns its `file` dupe and `ranges`, so deinit them individually.
        errdefer {
            for (infos.items) |*di| di.deinit();
            infos.deinit(allocator);
        }
        if (self.debug_markers.items.len == 0) {
            infos.deinit(allocator);
            return &.{};
        }
        const markers = self.debug_markers.items;
        const total_bytes: u32 = @intCast(self.instructions.items.len);
        const initial_name = try allocator.dupe(u8, std.fs.path.basename(self.module.includes.keys()[markers[0].file_index]));
        {
            errdefer allocator.free(initial_name);
            try infos.append(allocator, DebugInfo.init(allocator, initial_name));
        }
        var info: *DebugInfo = &(infos.items[0]);
        var current_file = markers[0].file_index;
        for (markers, 0..) |m, i| {
            const end: u32 = if (i + 1 < markers.len) markers[i + 1].ip else total_bytes;
            if (m.file_index != current_file and m.file_index < self.module.includes.count()) {
                current_file = m.file_index;
                const name = std.fs.path.basename(self.module.includes.keys()[m.file_index]);
                info = for (infos.items, 0..) |item, j| {
                    if (!std.mem.eql(u8, name, item.file)) continue;
                    break &(infos.items[j]);
                } else blk: {
                    const new_file_name = try allocator.dupe(u8, name);
                    {
                        errdefer allocator.free(new_file_name);
                        try infos.append(allocator, DebugInfo.init(allocator, new_file_name));
                    }
                    break :blk &(infos.items[infos.items.len - 1]);
                };
            }
            try info.ranges.append(allocator, .{ .start = m.ip, .end = end, .line = m.line });
        }
        return try infos.toOwnedSlice(allocator);
    }

    pub fn deinit(self: *Chunk) void {
        self.instructions.deinit(self.alloc);
        self.debug_markers.deinit(self.alloc);
        self.alloc.destroy(self);
    }
};

pub const JumpPatch = struct { pos: usize };

pub const Error = error{
    OutOfMemory,
    OutOfScope,
    SymbolAlreadyDeclared,
    ConstantNotFound,
};

pub const Emitter = struct {
    /// Long-lived allocator: owns chunk instruction buffers and the
    /// `constants` list, both of which become part of the final
    /// `Bytecode` value via `toOwnedSlice`.
    alloc: std.mem.Allocator,
    module: *Module,
    chunk: *Chunk,
    constants: std.ArrayList(Value) = .empty,
    constants_map: std.StringHashMapUnmanaged(C.CONSTANT) = .empty,
    literal_cache: std.ArrayHashMapUnmanaged(Value, C.CONSTANT, Value.Adapter, true) = .empty,

    pub fn init(alloc: std.mem.Allocator, module: *Module) !Emitter {
        const root_chunk = try Chunk.init(alloc, null, module);
        return .{
            .alloc = alloc,
            .module = module,
            .chunk = root_chunk,
        };
    }

    /// Free any chunks still in the chain and any constants still owned
    /// by the emitter. After a successful `bytecode()` extraction the
    /// constants list and root chunk buffers are empty (ownership
    /// transferred), so this is a no-op for the success path.
    pub fn deinit(self: *Emitter) void {
        var c: ?*Chunk = self.chunk;
        while (c) |cur| {
            c = cur.parent;
            cur.deinit();
        }
        for (self.constants.items) |item| item.destroy(self.alloc);
        self.constants.deinit(self.alloc);
    }

    pub fn enterChunk(self: *Emitter) !void {
        self.chunk = try Chunk.init(self.alloc, self.chunk, self.module);
    }

    /// Caller owns the returned chunk and must `deinit` it.
    pub fn exitChunk(self: *Emitter) error{OutOfScope}!*Chunk {
        const old_chunk = self.chunk;
        self.chunk = old_chunk.parent orelse return error.OutOfScope;
        return old_chunk;
    }

    pub fn instructionPos(self: *Emitter) C.JUMP {
        return @as(C.JUMP, @intCast(self.chunk.instructions.items.len));
    }

    pub fn writeOp(self: *Emitter, op: OpCode, token: Token) !void {
        var chunk = self.chunk;
        const ip: u32 = @intCast(chunk.instructions.items.len);
        const file_index: u32 = @intCast(token.file_index);
        const line: u32 = @intCast(token.line);
        const start_run = chunk.debug_markers.items.len == 0 or
            chunk.debug_markers.items[chunk.debug_markers.items.len - 1].file_index != file_index or
            chunk.debug_markers.items[chunk.debug_markers.items.len - 1].line != line;
        if (start_run) {
            try chunk.debug_markers.append(self.alloc, .{ .ip = ip, .file_index = file_index, .line = line });
        }
        chunk.last_op_pos = ip;
        try chunk.instructions.append(self.alloc, @intFromEnum(op));
    }

    pub fn writeValue(self: *Emitter, buf: []const u8, _: Token) !void {
        // Operand bytes inherit the most-recent marker, the surrounding
        // op already pushed (or merged into) a run.
        try self.chunk.instructions.appendSlice(self.alloc, buf);
    }

    pub fn writeInt(self: *Emitter, comptime T: type, value: T, token: Token) !usize {
        const chunk = self.chunk;
        const start = chunk.instructions.items.len;
        var buf: [@sizeOf(T)]u8 = undefined;
        std.mem.writeInt(T, buf[0..], value, .little);
        try self.writeValue(&buf, token);
        return start;
    }

    pub fn replaceValue(self: *Emitter, pos: usize, comptime T: type, value: T) !void {
        var buf: [@sizeOf(T)]u8 = undefined;
        std.mem.writeInt(T, buf[0..], value, .little);
        var chunk = self.chunk;
        for (buf, 0..) |v, i| {
            chunk.instructions.items[pos + i] = v;
        }
    }

    pub fn emitJump(self: *Emitter, op: OpCode, token: Token) !JumpPatch {
        try self.writeOp(op, token);
        const pos = try self.writeInt(C.JUMP, JUMP_HOLDER, token);
        return .{ .pos = pos };
    }

    pub fn patchJump(self: *Emitter, jp: JumpPatch) !void {
        try self.replaceValue(jp.pos, C.JUMP, self.instructionPos());
    }

    pub fn patchJumpTo(self: *Emitter, jp: JumpPatch, target: C.JUMP) !void {
        try self.replaceValue(jp.pos, C.JUMP, target);
    }

    pub fn lastIs(self: *Emitter, op: OpCode) !bool {
        const pos = self.chunk.last_op_pos orelse return false;
        return self.chunk.instructions.items[pos] == @intFromEnum(op);
    }

    pub fn removeLast(self: *Emitter, op: OpCode) !void {
        if (try self.lastIs(op)) {
            const pos = self.chunk.last_op_pos.?;
            self.chunk.instructions.items.len = pos;
            // Drop any trailing markers whose run starts at or after
            // the removed op. Usually that's at most one (this op
            // started its own run); a continuation op pushed no marker
            // and leaves the prior run intact.
            const markers = &self.chunk.debug_markers;
            while (markers.items.len > 0 and markers.items[markers.items.len - 1].ip >= pos) {
                _ = markers.pop();
            }
            self.chunk.last_op_pos = null;
        }
    }

    pub fn addConstant(self: *Emitter, value: Value) !C.CONSTANT {
        try self.constants.append(self.alloc, value);
        return @intCast(self.constants.items.len - 1);
    }

    /// `arena_alloc` is the consumer's arena allocator (re-derived from
    /// the consumer's stable `*ArenaAllocator` at the call site). We
    /// don't cache it on the emitter because the arena's `Allocator`
    /// struct embeds `&arena`, which is invalidated whenever the
    /// containing struct is moved (e.g., when packed into a return).
    pub fn addNamedConstant(self: *Emitter, arena_alloc: std.mem.Allocator, name: []const u8, value: Value) !void {
        return self.addNamedConstantTok(arena_alloc, name, value);
    }

    /// Register a named constant. Returns `error.SymbolAlreadyDeclared`
    /// if a constant with that name already exists. Atomic: either
    /// fully succeeds (value placed in constants, key inserted) or
    /// leaves all state untouched. The consumer is responsible for
    /// recording the declaration token in its own `decl_tokens` map
    /// for "previous declaration at" diagnostics.
    pub fn addNamedConstantTok(self: *Emitter, arena_alloc: std.mem.Allocator, name: []const u8, value: Value) !void {
        if (self.constants_map.contains(name)) return error.SymbolAlreadyDeclared;
        // Reserve capacity for every write up front so post-`dupe`
        // inserts cannot OOM.
        try self.constants.ensureUnusedCapacity(self.alloc, 1);
        try self.constants_map.ensureUnusedCapacity(arena_alloc, 1);
        const key = try arena_alloc.dupe(u8, name);
        const i: C.CONSTANT = @intCast(self.constants.items.len);
        self.constants.appendAssumeCapacity(value);
        self.constants_map.putAssumeCapacityNoClobber(key, i);
    }

    pub fn addLiteralConstant(self: *Emitter, arena_alloc: std.mem.Allocator, value: Value) !C.CONSTANT {
        if (self.literal_cache.get(value)) |idx| return idx;
        const i = try self.addConstant(value);
        try self.literal_cache.put(arena_alloc, value, i);
        return i;
    }

    /// Probe-only lookup. Used by callers that need to construct the
    /// pool entry lazily (e.g. allocate a heap Obj only on miss).
    pub fn findLiteralConstant(self: *Emitter, value: Value) ?C.CONSTANT {
        return self.literal_cache.get(value);
    }

    /// Insert a value at the next pool slot AND cache it for future
    /// `findLiteralConstant` hits. Counterpart to `findLiteralConstant`
    /// for the lazy-allocate pattern.
    pub fn addAndCacheLiteralConstant(self: *Emitter, arena_alloc: std.mem.Allocator, value: Value) !C.CONSTANT {
        const i = try self.addConstant(value);
        try self.literal_cache.put(arena_alloc, value, i);
        return i;
    }

    /// Emit a `.constant <idx>` opcode for an identifier name, registering
    /// a string constant for it on first use.
    pub fn addIdentifierConstant(self: *Emitter, arena_alloc: std.mem.Allocator, name: []const u8, token: Token) !void {
        var i = self.constants_map.get(name);
        if (i == null) {
            try self.constants_map.ensureUnusedCapacity(arena_alloc, 1);
            const key = try arena_alloc.dupe(u8, name);
            i = try self.addConstant(.{ .const_string = name });
            self.constants_map.putAssumeCapacityNoClobber(key, i.?);
        }
        try self.writeOp(.constant, token);
        _ = try self.writeInt(C.CONSTANT, i.?, token);
    }

    pub fn replaceConstant(self: *Emitter, name: []const u8, value: Value) error{ConstantNotFound}!void {
        const i = self.constants_map.get(name) orelse return error.ConstantNotFound;
        self.constants.items[i] = value;
    }
};

/// Patch every `.jump <old_pos>` instruction in `instructions` to instead
/// jump to `new_pos`. Used for the bulk break/continue/switch-end patch
/// at end of loop and switch bodies.
pub fn replaceJumps(instructions: []u8, old_pos: C.JUMP, new_pos: C.JUMP) !void {
    var i: usize = 0;
    const jump = @intFromEnum(OpCode.jump);
    while (i + 1 + @sizeOf(C.JUMP) <= instructions.len) : (i += 1) {
        if (instructions[i] == jump and std.mem.readVarInt(C.JUMP, instructions[(i + 1)..(i + 1 + @sizeOf(C.JUMP))], .little) == old_pos) {
            var buf: [@sizeOf(C.JUMP)]u8 = undefined;
            std.mem.writeInt(C.JUMP, buf[0..], new_pos, .little);
            for (buf, 0..) |v, index| {
                instructions[i + index + 1] = v;
            }
        }
    }
}
