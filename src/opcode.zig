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

    @"or",
    @"and",

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

    get_local,
    set_local,

    set_property,

    get_builtin,
    get_free,
    set_free,

    string,
    list,
    map,
    set,
    class,
    instance,

    index,
    call,
    closure,
    current_closure,
    dialogue,
    fork,
    choice,

    return_void,
    return_value,
    fin,

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
            .@"or" => "OP_OR",
            .@"and" => "OP_AND",
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
            .get_local => "OP_GET_LOCAL",
            .set_local => "OP_SET_LOCAL",
            .set_property => "OP_SET_PROPERTY",
            .get_builtin => "OP_GET_BUILTIN",
            .get_free => "OP_GET_FREE",
            .set_free => "OP_SET_FREE",
            .list => "OP_LIST",
            .string => "OP_STRING",
            .map => "OP_MAP",
            .set => "OP_SET",
            .class => "OP_CLASS",
            .instance => "OP_INSTANCE",
            .index => "OP_INDEX",
            .call => "OP_CALL",
            .closure => "OP_CLOSURE",
            .current_closure => "OP_CURRENT_CLOSURE",
            .dialogue => "OP_DIALOGUE",
            .fork => "OP_FORK",
            .choice => "OP_CHOICE",
            .return_void => "OP_RETURN_VOID",
            .return_value => "OP_RETURN_VALUE",
            .fin => "OP_FIN",
        };
    }

    pub fn Size(comptime self: OpCode) type {
        // kept separate to easily change if needed
        return switch (self) {
            .constant => u16,
            .jump, .jump_if_false => u16,
            .set_global, .get_global, .choice => u16,
            .list, .map, .set => u16,
            .get_local, .set_local, .get_builtin, .get_free, .set_free, .call, .class, .instance => u8,
            .string, .closure => u24, // u16 for constant location, u8 for expressions count
            else => void,
        };
    }
};
