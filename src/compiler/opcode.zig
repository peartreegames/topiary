const std = @import("std");

pub const OpCode = enum(u8) {
    constant,
    pop,
    add,
    subtract,
    multiply,
    divide,
    modulus,
    negate,
    true,
    false,
    equal,
    not_equal,
    greater_than,
    @"return",

    pub fn toString(self: OpCode) []const u8 {
        return switch (self) {
            .constant => "OP_CONSTANT",
            .pop => "OP_POP",
            .add => "OP_ADD",
            .subtract => "OP_SUBTRACT",
            .multiply => "OP_MULTIPLY",
            .divide => "OP_DIVIDE",
            .modulus => "OP_MODULUS",
            .negate => "OP_NEGATE",
            .true => "OP_TRUE",
            .false => "OP_FALSE",
            .equal => "OP_EQUAL",
            .not_equal => "OP_NOT_EQUAL",
            .greater_than => "OP_GREATER_THAN",
            .@"return" => "OP_RETURN",
        };
    }

    pub fn Type(comptime self: OpCode) type {
        return switch (self) {
            .constant => u16,
            else => void,
        };
    }
};
