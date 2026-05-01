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
        // Root chunk ends up holding most of the bytecode. Pre-size from
        // total source bytes to avoid ArrayList doubling reallocations.
        if (parent == null) {
            const estimate = module.timings.source_bytes / 4;
            if (estimate > 0) {
                try chunk.instructions.ensureTotalCapacity(allocator, estimate);
                try chunk.debug_markers.ensureTotalCapacity(allocator, estimate);
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
        var file_index = self.debug_markers.items[0].file_index;
        var line = self.debug_markers.items[0].line;
        var start: u32 = 0;
        const initial_name = try allocator.dupe(u8, std.fs.path.basename(self.module.includes.keys()[file_index]));
        {
            errdefer allocator.free(initial_name);
            try infos.append(allocator, DebugInfo.init(allocator, initial_name));
        }
        var info: *DebugInfo = &(infos.items[0]);
        for (self.debug_markers.items, 0..) |d, ip| {
            const end: u32 = @intCast(ip);
            // File change: find or create DebugInfo for the new file.
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
                    const new_file_name = try allocator.dupe(u8, name);
                    {
                        errdefer allocator.free(new_file_name);
                        try infos.append(allocator, DebugInfo.init(allocator, new_file_name));
                    }
                    break :blk &(infos.items[infos.items.len - 1]);
                };
                continue;
            }
            // Line change: append the prior range and start a new one.
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
        chunk.last_op_pos = chunk.instructions.items.len;
        try chunk.debug_markers.append(self.alloc, .{ .file_index = @intCast(token.file_index), .line = @intCast(token.line) });
        try chunk.instructions.append(self.alloc, @intFromEnum(op));
    }

    pub fn writeValue(self: *Emitter, buf: []const u8, token: Token) !void {
        var chunk = self.chunk;
        try chunk.debug_markers.appendNTimes(self.alloc, .{ .file_index = @intCast(token.file_index), .line = @intCast(token.line) }, buf.len);
        try chunk.instructions.appendSlice(self.alloc, buf);
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
            self.chunk.debug_markers.items.len = pos;
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
