pub const std = @import("std");
pub const lexer = @import("lexer.test.zig");
pub const parser = @import("parser.test.zig");
pub const compiler = @import("compiler.test.zig");
pub const vm = @import("vm.test.zig");
pub const locale = @import("locale.test.zig");

test {
    std.testing.refAllDecls(@This());
}
