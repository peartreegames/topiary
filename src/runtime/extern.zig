const std = @import("std");
const Value = @import("../types/value.zig").Value;

pub const Extern = struct {
    arity: u8,
    context_ptr: usize,
    backing: *const fn (context_ptr: usize, args: []Value) Value,
    destroy: *const fn (context_ptr: usize, allocator: std.mem.Allocator) void,
};
