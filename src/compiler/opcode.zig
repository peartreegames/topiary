const std = @import("std");

pub const OpCode = enum(u8) {
    constant,

    pub fn toString(self: OpCode) []const u8 {
        return switch (self) {
            .constant => "OP_CONSTANT",
        };
    }

    pub fn Type(comptime self: OpCode) type {
        return switch (self) {
            .constant => u16,
        };
    }
};
