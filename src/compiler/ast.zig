const std = @import("std");
const token = @import("./token.zig");
const Token = token.Token;
const Allocator = std.mem.Allocator;

pub const Tree = struct {
    arena: std.heap.ArenaAllocator.State,
    allocator: Allocator,

    root: []const Statement,
    // imports: ?[]const *Tree,

    pub fn deinit(self: *const Tree) void {
        self.arena.promote(self.allocator).deinit();
    }

    pub fn print(self: *const Tree, writer: anytype) !void {
        try writer.print("\n", .{});
        for (self.root) |state| {
            try state.print(writer, "", 0);
        }
    }
};

pub const Expression = struct {
    token: Token,
    type: ExpressionValue,

    pub const ExpressionValue = union(enum) {
        dialogue: struct {
            speaker: ?[]const u8,
            content: *Expression,
            tags: [][]const u8,
        },
        assignment: struct {
            target: *Expression,
            value: *Expression,
        },
        indexer: struct {
            target: *Expression,
            index: *Expression,
        },
        identifier: []const u8,
        call: struct {
            name: *Expression,
            arguments: []const Expression,
        },
        @"extern": void,
        boolean: bool,
        number: f32,
        string: struct {
            value: []const u8,
            expressions: []const Expression,
        },
        @"enum": []const Expression, // []identifier
        @"struct": struct {
            name: []const u8,
            assignments: []const Expression,
        },
        function: struct {
            parameters: [][]const u8,
            body: []const Statement,
        },
        list: []const Expression,
        set: []const Expression,
        map: []const Expression, // map_pair
        nil: void,
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
            then_value: *Expression,
            else_value: *Expression,
        },
        @"switch": struct {
            capture: *Expression,
            cases: []const Expression,
        },
        switch_case: struct {
            value: *Expression,
            body: []const Statement,
        },
    };
};

pub const Statement = struct {
    token: Token,
    type: StatementValue,

    pub const StatementValue = union(enum) {
        block: []const Statement,
        bough: struct {
            name: []const u8,
            body: []const Statement,
        },
        choice: struct {
            text: Expression,
            body: []const Statement,
        },
        @"enum": struct {
            name: []const u8,
            values: [][]const u8,
        },
        expression: Expression,
        @"for": struct {
            iterator: Expression,
            capture: []const u8,
            body: []const Statement,
        },
        fork: struct {
            name: ?[]const u8,
            body: []const Statement,
        },
        @"if": struct {
            condition: *Expression,
            then_branch: []const Statement,
            else_branch: ?[]const Statement,
        },
        import: struct {
            path: []const u8,
            contents: []const Statement,
        },
        jump: [][]const u8,
        return_expression: Expression,
        return_void: void,
        @"struct": struct {
            name: []const u8,
            fields: []const Expression,
        },
        variable: struct {
            name: []const u8,
            initializer: Expression,
            is_mutable: bool = false,
            is_extern: bool = false,
        },
        @"while": struct {
            condition: Expression,
            body: []const Statement,
        },
        @"break": void,
        @"continue": void,
        comment: []const u8,
    };
};

pub const UnaryOp = enum {
    not,
    negate,
    pub fn fromToken(tok: Token) UnaryOp {
        return switch (tok.token_type) {
            .bang => .not,
            .minus => .negate,
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
    assign_add,
    assign_subtract,
    assign_multiply,
    assign_divide,
    @"and",
    @"or",
    pub fn fromToken(tok: Token) BinaryOp {
        return switch (tok.token_type) {
            .plus => .add,
            .minus => .subtract,
            .equal => .assign,
            .star => .multiply,
            .percent => .modulus,
            .slash => .divide,
            .less => .less_than,
            .greater => .greater_than,
            .less_equal => .less_than_equal,
            .greater_equal => .greater_than_equal,
            .equal_equal => .equal,
            .bang_equal => .not_equal,
            .@"and" => .@"and",
            .@"or" => .@"or",
            .plus_equal => .assign_add,
            .minus_equal => .assign_subtract,
            .star_equal => .assign_multiply,
            .slash_equal => .assign_divide,
            else => unreachable,
        };
    }
};
