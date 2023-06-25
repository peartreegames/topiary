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
    not,

    true,
    false,
    nil,

    equal,
    not_equal,
    greater_than,

    jump,
    jump_if_false,

    get_global,
    set_global,

    string,
    list,

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
            .not => "OP_NOT",
            .true => "OP_TRUE",
            .false => "OP_FALSE",
            .nil => "OP_NIL",
            .equal => "OP_EQUAL",
            .not_equal => "OP_NOT_EQUAL",
            .greater_than => "OP_GREATER_THAN",
            .jump => "OP_JUMP",
            .jump_if_false => "OP_JUMP_IF_FALSE",
            .get_global => "OP_GET_GLOBAL",
            .set_global => "OP_SET_GLOBAL",
            .list => "OP_LIST",
            .string => "OP_STRING",
            .@"return" => "OP_RETURN",
        };
    }

    pub fn Size(comptime self: OpCode) type {
        // kept separate to easily change if needed
        return switch (self) {
            .constant => u16,
            .jump, .jump_if_false => u16,
            .set_global, .get_global => u16,
            .list => u16,
            .string => u32, // u16 for constant location, u16 for expressions count
            else => void,
        };
    }
};
