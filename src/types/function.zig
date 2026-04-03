const std = @import("std");
const backend = @import("../backend/index.zig");
const DebugInfo = backend.DebugInfo;

const utils = @import("../utils/index.zig");
const C = utils.C;

pub const Function = struct {
    name: ?[]const u8 = null,
    arity: u8,
    instructions: []const u8,
    locals_count: usize,
    is_method: bool = false,
    debug_info: []DebugInfo,

    pub fn deinit(self: Function, allocator: std.mem.Allocator) void {
        if (self.name) |n| allocator.free(n);
        allocator.free(self.instructions);
        for (self.debug_info) |*d| d.*.deinit();
        allocator.free(self.debug_info);
    }
};
