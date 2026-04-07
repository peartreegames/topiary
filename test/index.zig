pub const std = @import("std");
pub const lexer = @import("lexer.test.zig");
pub const parser = @import("parser.test.zig");
pub const compiler = @import("compiler.test.zig");
pub const vm = @import("vm.test.zig");
pub const locale = @import("locale.test.zig");
pub const stamp = @import("stamp.test.zig");
pub const exp = @import("export.test.zig");
pub const formatter = @import("formatter.test.zig");
pub const fuzz = @import("fuzz.test.zig");

test {
    std.testing.refAllDecls(@This());
}
