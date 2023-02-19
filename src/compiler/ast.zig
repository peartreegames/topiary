const std = @import("std");
const token = @import("./token.zig");
const val = @import("./values.zig");
const Token = token.Token;

pub const Tree = struct {
    arena: std.heap.ArenaAllocator.State,
    allocator: std.mem.Allocator,

    root: []const Statement,
    // imports: ?[]const *Tree,

    pub fn deinit(self: *const Tree) void {
        self.arena.promote(self.allocator).deinit();
    }
};

pub const Expression = struct {
    pub const Type = std.meta.Tag(ExpressionValue);
    token: Token,
    type: ExpressionValue,

    const ExpressionValue = union(enum) {
        assignment: struct {
            target: *Expression,
            value: *Expression,
        },
        indexer: struct {
            value: *Expression,
            index: *Expression,
        },
        identifier: []const u8,
        list_literal: []Expression,
        call: struct {
            name: *Expression,
            arguments: []Expression,
        },
        @"extern": void,
        boolean_literal: bool,
        integer_literal: i64,
        float_literal: f64,
        string_literal: []const u8,
        enum_literal: []const Expression, // []identifier
        function_literal: *Statement, // block
        nil: void,
        set_value: struct {
            key: *Expression,
        },
        map_value: struct {
            pairs: *Expression,
        },
        map_pair: struct {
            key: *Expression,
            value: *Expression,
        },
        range: struct {
            left: *Expression,
            right: *Expression,
        },
        unary: struct {
            operator: UnaryOp,
            value: *Expression,
        },
        binary: struct {
            operator: BinaryOp,
            left: *Expression,
            right: *Expression,
        },
        @"if": struct {
            condition: *Expression,
            true_body: *Statement,
            false_body: ?*Statement,
        },
        switch_literal: struct { capture: *Expression, cases: []const Expression },
        switch_case: struct { value: *Expression, body: *Statement },
        content: []const u8,
        tag: []const u8,
    };
};

pub const Statement = struct {
    pub const Type = std.meta.Tag(StatementValue);
    token: Token,
    type: StatementValue,

    const StatementValue = union(enum) {
        comment: []const u8,
        import: []const u8,
        declaration: struct {
            name: Expression, // identifer
            value: Expression,
            type_def: *TypeDefValue,
            is_mutable: bool = false,
        },
        block: struct {
            body: []const Statement,
            name: ?Expression, // identifier
        },
        while_loop: struct {
            condition: Expression,
            body: *Statement, // block
        },
        for_loop: struct {
            iterator: Expression,
            capture: Expression, // identifier
            body: *Statement, // block
            pub var index = .{ .token = undefined, .value = 0 };
        },
        @"break": void,
        @"continue": void,
        return_expression: Expression,
        return_void: void,
        expression: Expression,
        function_parameter: struct {
            name: []const u8,
            type_def: *TypeDefValue,
        },
        line: struct {
            speaker: ?Expression, // identifier
            content: []const Statement,
        },
        jump: Expression, // identifier
        split: struct {
            content: []const Statement,
            body: *Statement, // block
        },
    };

    pub const TypeDef = std.meta.Tag(TypeDefValue);
    pub const TypeDefValue = union(enum) {
        void_type: void,
        boolean_type: void,
        integer_type: void,
        float_type: void,
        string_type: void,
        enum_type: void,
        identifier_type: []const u8, // identifier
        function_type: struct {
            params: []const Statement,
            return_type: *TypeDefValue,
        },
        list_type: *TypeDefValue,
        set_type: *TypeDefValue,
        map_type: struct {
            key_type: *TypeDefValue,
            value_type: *TypeDefValue,
        },
        pub fn getType(self: *const Statement) ?val.Type {
            return switch (self.token.token_type) {
                .enum_type => .@"enum",
                .bool_type => .boolean,
                .string_type => .string,
                .integer_type => .integer,
                .void_type => .void,
                .function_type => .function,
                .list_type => .array,
                .set_type => .set,
                .map_type => .map,
                // .query => .optional,
                else => null,
            };
        }
    };
};

pub const UnaryOp = enum {
    not,
    minus,
    pub fn fromToken(tok: Token) UnaryOp {
        return switch (tok.token_type) {
            .bang => .not,
            .minus => .minus,
            else => unreachable,
        };
    }
};

pub const BinaryOp = enum {
    add,
    subtract,
    multiply,
    divide,
    modulus,
    less_than,
    greater_than,
    less_than_equal,
    greater_than_equal,
    equal,
    not_equal,
    assign,
    @"and",
    @"or",
    pub fn fromToken(tok: Token) BinaryOp {
        return switch (tok.token_type) {
            .plus => .add,
            .minus => .subtract,
            .assign => .assign,
            .asterisk => .multiply,
            .percent => .modulus,
            .slash => .divide,
            .less_than => .less_than,
            .greater_than => .greater_than,
            .less_than_equal => .less_than_equal,
            .greater_than_equal => .greater_than_equal,
            .equal => .equal,
            .not_equal => .not_equal,
            .@"and" => .@"and",
            .@"or" => .@"or",
            else => unreachable,
        };
    }
};
