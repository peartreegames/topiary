const std = @import("std");
const backend = @import("../backend/index.zig");
const DebugInfo = backend.DebugInfo;

const utils = @import("../utils/index.zig");
const C = utils.C;

pub const Function = struct {
    arity: u8,
    instructions: []const u8,
    locals_count: usize,
    is_method: bool = false,
    debug_info: []DebugInfo,
    extern_name: ?[]const u8 = null,
    extern_index: ?C.GLOBAL = null,

    pub fn deinit(self: Function, allocator: std.mem.Allocator) void {
        allocator.free(self.instructions);
        for (self.debug_info) |*d| d.*.deinit();
        allocator.free(self.debug_info);
    }
};
