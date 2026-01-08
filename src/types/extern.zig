const std = @import("std");
const Value = @import("../types/value.zig").Value;

pub const Extern = struct {
    name: []const u8,
    arity: u8,
    context_ptr: ?usize = null,
    backing: Backing = undefined,
    destroy: Destroy = undefined,
    pub const Backing = *const fn (context_ptr: usize, args: []Value) Value;
    pub const Destroy = *const fn (context_ptr: usize, allocator: std.mem.Allocator) void;
};
