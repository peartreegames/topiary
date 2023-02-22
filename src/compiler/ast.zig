const std = @import("std");
const token = @import("./token.zig");
const val = @import("./values.zig");
const Token = token.Token;
const Allocator = std.mem.Allocator;
const lines: []const u8 = "-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|";

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
        call: struct {
            name: *Expression,
            arguments: []const Expression,
        },
        @"extern": void,
        boolean_literal: bool,
        integer_literal: i32,
        float_literal: f64,
        string_literal: []const u8,
        enum_literal: []const Expression, // []identifier
        function_literal: *Statement, // block
        list_literal: []const Expression,
        set_literal: []const Expression,
        map_literal: []const Expression, // map_pair
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
        if_expression: struct {
            condition: *Expression,
            true_value: *Expression,
            false_value: *Expression,
        },
        switch_literal: struct {
            capture: *Expression,
            cases: []const Expression,
        },
        switch_case: struct {
            value: *Expression,
            body: []const Statement,
        },
        content: []const u8,
        tag: []const u8,
    };

    pub fn print(self: Expression, writer: anytype, msg: []const u8, depth: usize) !void {
        const l = lines[0 .. depth * 2];
        try writer.print("|{s} {s} {s}\n", .{ l, msg, @tagName(self.type) });
        switch (self.type) {
            .if_expression => |i| {
                i.condition.print(writer, "condition:", depth + 1) catch {};
                i.true_value.print(writer, "true:", depth + 1) catch {};
                i.false_value.print(writer, "false:", depth + 1) catch {};
            },
            .unary => |unary| {
                try writer.print("|{s} operator: {}\n", .{ l, unary.operator });
                try unary.value.print(writer, "value:", depth + 1);
            },
            .binary => |binary| {
                try binary.left.print(writer, "left:", depth + 1);
                try writer.print("|{s} operator: {}\n", .{ l, binary.operator });
                try binary.right.print(writer, "right:", depth + 1);
            },
            .content => |content| {
                try writer.print("|{s} value: {s}\n", .{ lines[0 .. (depth + 1) * 2], content });
            },
            else => {},
        }
    }
};

pub const Statement = struct {
    pub const Type = std.meta.Tag(StatementValue);
    token: Token,
    type: StatementValue,

    const StatementValue = union(enum) {
        comment: []const u8,
        import: []const u8,
        declaration: struct {
            name: []const u8, // identifer
            value: Expression,
            type_def: *TypeDefValue,
            is_mutable: bool = false,
        },
        if_statement: struct {
            condition: *Expression,
            true_body: *Statement,
            false_body: ?*Statement,
        },
        block: struct {
            body: []const Statement,
            name: ?[]const u8, // identifier
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
        jump: Expression, // identifier or index
        split: struct {
            content: []const Statement,
            body: []const Statement, // block
        },
    };

    pub fn print(self: *const Statement, writer: anytype, msg: []const u8, depth: usize) !void {
        const l = lines[0 .. depth * 2];

        try writer.print("|{s} {s} {s}\n", .{ l, msg, @tagName(self.type) });
        switch (self.type) {
            .block => |block| {
                if (block.name != null) try writer.print("|{s} name: {s} {s}\n", .{ lines[0 .. (depth + 1) * 2], block.name.?, token.fmtString(self.token.token_type) });
                for (block.body) |state| {
                    try state.print(writer, "block body:", depth + 1);
                }
            },
            .if_statement => |i| {
                i.condition.print(writer, "condition:", depth + 1) catch {};
                i.true_body.print(writer, "true_body:", depth + 1) catch {};
                if (i.false_body != null) i.false_body.?.print(writer, "false_body:", depth + 1) catch {};
            },
            .declaration => |decl| {
                try writer.print("|{s} name: {s}\n", .{ lines[0 .. (depth + 1) * 2], decl.name });
                try decl.type_def.print(writer, depth + 1);
                try decl.value.print(writer, "value:", depth + 1);
            },
            .for_loop => |forl| {
                try forl.iterator.print(writer, "iterator:", depth + 1);
                try forl.capture.print(writer, "capture:", depth + 1);
                try forl.body.print(writer, "for body:", depth + 1);
            },
            .while_loop => |whilel| {
                try whilel.condition.print(writer, "condition:", depth + 1);
                try whilel.body.print(writer, "while body:", depth + 1);
            },
            .line => |line| {
                if (line.speaker != null) try line.speaker.?.print(writer, "speaker:", depth + 1);
                for (line.content) |cnt| {
                    try cnt.print(writer, "line content:", depth + 1);
                }
            },
            .jump => |jump| try jump.print(writer, "destination:", depth + 1),
            .split => |split| {
                for (split.content) |cnt| {
                    try cnt.print(writer, "split content:", depth + 1);
                }
                for (split.body) |body| {
                    try body.print(writer, "split body:", depth + 1);
                }
            },
            .expression => |exp| {
                try exp.print(writer, "", depth + 1);
            },
            else => {},
        }
    }

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
        pub fn print(self: TypeDefValue, writer: anytype, depth: usize) !void {
            const l = lines[0 .. depth * 2];
            try writer.print("|{s} type: {s}\n", .{ l, @tagName(self) });
            switch (self) {
                .identifier_type => |i| try writer.print("|{s} name: {s}\n", .{ lines[0 .. (depth + 1) * 2], i }),
                .list_type, .set_type => |t| try t.print(writer, depth + 1),
                .map_type => |m| {
                    try m.key_type.print(writer, depth + 1);
                    try m.value_type.print(writer, depth + 1);
                },
                else => {},
            }
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
