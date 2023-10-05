const std = @import("std");
const token = @import("./token.zig");
const Token = token.Token;
const Allocator = std.mem.Allocator;

pub const Tree = struct {
    arena: std.heap.ArenaAllocator.State,
    allocator: Allocator,

    root: []const Statement,
    source: []const u8,

    pub fn deinit(self: *const Tree) void {
        self.arena.promote(self.allocator).deinit();
    }

    pub fn print(self: *const Tree, writer: anytype) void {
        writer.print("\n===TREE===", .{});
        for (self.root) |state| {
            state.print(writer, "", 0);
        }
        writer.print("\n", .{});
    }
};

pub const Expression = struct {
    token: Token,
    type: ExpressionValue,

    pub var for_index: Expression = .{ .token = undefined, .type = .{ .number = 0 } };
    pub const ExpressionValue = union(enum) {
        indexer: struct {
            target: *Expression,
            index: *Expression,
        },
        identifier: []const u8,
        call: struct {
            target: *Expression,
            arguments: []const Expression,
        },
        @"extern": void,
        boolean: bool,
        number: f32,
        string: struct {
            value: []const u8,
            expressions: []const Expression,
        },
        instance: struct {
            name: []const u8,
            field_names: [][]const u8,
            fields: []const Expression,
        },
        function: struct {
            is_method: bool = false,
            name: ?[]const u8 = null,
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
    };
    pub fn print(self: Expression, writer: anytype, prefix: []const u8, depth: usize) void {
        writer.print("\n", .{});
        var d: usize = 0;
        while (d < depth) : (d += 1) {
            writer.print("    ", .{});
        }
        writer.print("{s}", .{prefix});
        switch (self.type) {
            .binary => |b| {
                writer.print("BINARY::{s}", .{b.operator.toString()});
                b.left.print(writer, "LEFT::", depth + 1);
                b.right.print(writer, "RIGHT::", depth + 1);
            },
            .call => |c| {
                writer.print("CALL::{d}", .{c.arguments.len});
                for (c.arguments) |arg| {
                    arg.print(writer, "ARG::", depth + 1);
                }
            },
            .identifier => |i| writer.print("{s}", .{i}),
            .number => |n| writer.print("{d}", .{n}),
            .function => |f| {
                writer.print("FUNCTION::{s}", .{f.parameters});
                for (f.body) |s| {
                    s.print(writer, "", depth + 1);
                }
            },
            else => writer.print("{any}", .{self}),
        }
    }
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
            name: ?[]const u8,
            text: Expression,
            is_unique: bool,
            body: []const Statement,
            tags: [][]const u8,
        },
        dialogue: struct {
            speaker: ?[]const u8,
            content: *Expression,
            tags: [][]const u8,
        },
        @"enum": struct {
            name: []const u8,
            values: [][]const u8,
        },
        expression: Expression,
        @"for": struct {
            index: Expression,
            iterator: Expression,
            capture: []const u8,
            body: []const Statement,
        },
        fork: struct {
            name: ?[]const u8,
            body: []const Statement,
            is_backup: bool,
        },
        @"if": struct {
            condition: *Expression,
            then_branch: []const Statement,
            else_branch: ?[]const Statement,
        },
        include: struct {
            path: []const u8,
            contents: []const Statement,
        },
        divert: struct {
            path: [][]const u8,
            is_backup: bool,
        },
        return_expression: Expression,
        return_void: void,
        class: struct {
            name: []const u8,
            field_names: [][]const u8,
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
        @"switch": struct {
            capture: Expression,
            prongs: []const Statement,
        },
        switch_prong: struct {
            values: ?[]Expression,
            body: []const Statement,
        },
    };

    pub fn print(self: Statement, writer: anytype, prefix: []const u8, depth: usize) void {
        writer.print("\n", .{});
        var d: usize = 0;
        while (d < depth) : (d += 1) {
            writer.print("  ", .{});
        }
        writer.print("{s}", .{prefix});
        switch (self.type) {
            .block => |b| {
                for (b) |s| s.print(writer, "BLOCK::", depth + 1);
            },
            .expression => |e| e.print(writer, "EXPRESSION::", depth + 1),
            .@"if" => |i| {
                writer.print("IF", .{});
                i.condition.print(writer, "CONDITION", depth + 1);
                for (i.then_branch) |s| s.print(writer, "THEN", depth + 1);
                if (i.else_branch) |eb| {
                    for (eb) |s| s.print(writer, "ELSE", depth + 1);
                }
            },
            .return_expression => |re| re.print(writer, "RETURN VALUE::", depth + 1),
            .return_void => writer.print("RETURN::", .{}),
            .variable => |v| {
                v.initializer.print(writer, "VARIABLE::", depth + 1);
            },
            else => {
                writer.print("{any}", .{self});
            },
        }
    }
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
    pub fn toString(self: UnaryOp) []const u8 {
        return switch (self) {
            .not => "!",
            .negate => "-",
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
    assign_modulus,
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
            .percent_equal => .assign_modulus,
            else => unreachable,
        };
    }
    pub fn toString(self: BinaryOp) []const u8 {
        return switch (self) {
            .add => "+",
            .subtract => "-",
            .multiply => "*",
            .divide => "/",
            .modulus => "%",
            .less_than => "<",
            .greater_than => ">",
            .less_than_equal => "<=",
            .greater_than_equal => ">=",
            .equal => "==",
            .not_equal => "!=",
            .assign => "=",
            .assign_add => "+=",
            .assign_subtract => "-=",
            .assign_multiply => "*=",
            .assign_divide => "/=",
            .assign_modulus => "%=",
            .@"and" => "and",
            .@"or" => "or",
        };
    }
};
