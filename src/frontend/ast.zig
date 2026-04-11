const std = @import("std");
const Token = @import("token.zig").Token;
const utils = @import("../utils/index.zig");
const UUID = utils.UUID;

pub const Tree = struct {
    root: []const Statement,

    pub fn print(self: *const Tree, writer: *std.Io.Writer) void {
        writer.print("\n===TREE===", .{});
        for (self.root) |state| {
            state.print(writer, "", 0);
        }
        writer.print("\n===    ===\n", .{});
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
            raw: []const u8,
            value: []const u8,
            expressions: []const Expression,
        },
        instance: struct {
            name: []const u8,
            name_token: Token,
            field_names: [][]const u8,
            field_name_tokens: []Token,
            fields: []const Expression,
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
    pub fn print(self: Expression, writer: *std.Io.Writer, prefix: []const u8, depth: usize) void {
        writer.print("\n", .{});
        var d: usize = 0;
        while (d < depth) : (d += 1) {
            writer.print("  ", .{});
        }
        writer.print("{s}", .{prefix});
        switch (self.type) {
            .binary => |b| {
                writer.print("BINARY:: {s}", .{@tagName(b.operator)});
                b.left.print(writer, "LEFT:: ", depth + 1);
                b.right.print(writer, "RIGHT:: ", depth + 1);
            },
            .unary => |u| {
                writer.print("UNARY:: {s}", .{u.operator.toString()});
                u.value.print(writer, "VALUE:: ", depth + 1);
            },
            .call => |c| {
                writer.print("CALL::", .{});
                c.target.print(writer, "TARGET:: ", depth + 1);
                for (c.arguments) |arg| {
                    arg.print(writer, "ARG:: ", depth + 1);
                }
            },
            .indexer => |i| {
                writer.print("INDEXER::", .{});
                i.target.print(writer, "TARGET:: ", depth + 1);
                i.index.print(writer, "INDEX:: ", depth + 1);
            },
            .identifier => |i| writer.print("IDENTIFIER:: {s}", .{i}),
            .number => |n| writer.print("NUM:: {d}", .{n}),
            .boolean => |b| writer.print("BOOL:: {}", .{b}),
            .nil => writer.print("NIL", .{}),
            .string => |s| {
                writer.print("STRING:: {s}", .{s.raw});
                for (s.expressions) |e| e.print(writer, "EXP:: ", depth + 1);
            },
            .instance => |i| {
                writer.print("INSTANCE:: {s}", .{i.name});
                for (i.field_names, i.fields) |name, field| {
                    field.print(writer, name, depth + 1);
                }
            },
            .list => |l| {
                writer.print("LIST::", .{});
                for (l) |item| item.print(writer, "ITEM:: ", depth + 1);
            },
            .set => |s| {
                writer.print("SET::", .{});
                for (s) |item| item.print(writer, "ITEM:: ", depth + 1);
            },
            .map => |m| {
                writer.print("MAP::", .{});
                for (m) |pair| pair.print(writer, "PAIR:: ", depth + 1);
            },
            .map_pair => |p| {
                writer.print("MAP_PAIR::", .{});
                p.key.print(writer, "KEY:: ", depth + 1);
                p.value.print(writer, "VALUE:: ", depth + 1);
            },
            .range => |r| {
                writer.print("RANGE::", .{});
                r.left.print(writer, "FROM:: ", depth + 1);
                r.right.print(writer, "TO:: ", depth + 1);
            },
            .@"if" => |i| {
                writer.print("TERNARY::", .{});
                i.condition.print(writer, "COND:: ", depth + 1);
                i.then_value.print(writer, "THEN:: ", depth + 1);
                i.else_value.print(writer, "ELSE:: ", depth + 1);
            },
            .@"extern" => writer.print("EXTERN", .{}),
        }
    }
};

pub const Tag = struct {
    name: []const u8,
    token: Token,
};

pub const Statement = struct {
    token: Token,
    type: StatementValue,

    pub const StatementValue = union(enum) {
        block: []const Statement,
        bough: struct {
            id: UUID.ID,
            id_token: ?Token = null,
            name: []const u8,
            name_token: Token,
            body: []const Statement,
        },
        choice: struct {
            id: UUID.ID,
            id_token: ?Token = null,
            name: ?[]const u8,
            content: Expression,
            is_unique: bool,
            body: []const Statement,
            tags: []const Tag,
        },
        dialogue: struct {
            id: UUID.ID,
            id_token: ?Token = null,
            speaker: ?[]const u8,
            content: *Expression,
            tags: []const Tag,
        },
        @"enum": struct {
            name: []const u8,
            name_token: Token,
            is_seq: bool,
            values: [][]const u8,
        },
        expression: Expression,
        @"for": struct {
            index: Expression,
            iterator: Expression,
            capture: []const u8,
            capture_token: Token,
            body: []const Statement,
        },
        fork: struct {
            id: UUID.ID,
            id_token: ?Token = null,
            name: ?[]const u8,
            end_token: Token,
            body: []const Statement,
            is_backup: bool,
        },
        function: struct {
            is_method: bool = false,
            name: []const u8,
            name_token: Token,
            parameters: [][]const u8,
            body: []const Statement,
            is_extern: bool,
        },
        @"if": struct {
            condition: *Expression,
            then_branch: []const Statement,
            else_branch: ?[]const Statement,
        },
        include: struct {
            path: []const u8,
            path_token: Token,
        },
        divert: struct {
            path: [][]const u8,
            path_tokens: []Token,
            end_token: Token,
            is_backup: bool,
        },
        return_expression: Expression,
        return_void: void,
        fin: void,
        class: struct {
            name: []const u8,
            name_token: Token,
            field_names: [][]const u8,
            field_name_tokens: []Token,
            fields: []const Expression,
            methods: []const Statement,
        },
        variable: struct {
            name: []const u8,
            name_token: Token,
            initializer: Expression,
            is_mutable: bool = false,
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

    pub fn print(self: Statement, writer: *std.Io.Writer, prefix: []const u8, depth: usize) void {
        writer.print("\n", .{});
        var d: usize = 0;
        while (d < depth) : (d += 1) {
            writer.print("  ", .{});
        }
        writer.print("{s}", .{prefix});
        switch (self.type) {
            .include => |inc| writer.print("INCLUDE:: {s}", .{inc.path}),
            .block => |b| {
                writer.print("BLOCK::", .{});
                for (b) |s| s.print(writer, "", depth + 1);
            },
            .bough => |b| {
                writer.print("BOUGH:: {s}", .{b.name});
                for (b.body) |s| s.print(writer, "", depth + 1);
            },
            .fork => |f| {
                writer.print("FORK:: {s}", .{f.name orelse "(anon)"});
                for (f.body) |s| s.print(writer, "", depth + 1);
            },
            .choice => |c| {
                writer.print("CHOICE:: {s}", .{c.name orelse "(anon)"});
                c.content.print(writer, "CONTENT:: ", depth + 1);
                for (c.body) |s| s.print(writer, "", depth + 1);
            },
            .dialogue => |d_| {
                writer.print("DIALOGUE:: speaker={s}", .{d_.speaker orelse "(none)"});
                d_.content.print(writer, "CONTENT:: ", depth + 1);
            },
            .function => |f| {
                writer.print("FUNCTION:: {s}(", .{f.name});
                for (f.parameters, 0..) |p, i| {
                    if (i > 0) writer.print(", ", .{});
                    writer.print("{s}", .{p});
                }
                writer.print(")", .{});
                for (f.body) |s| s.print(writer, "", depth + 1);
            },
            .class => |c| {
                writer.print("CLASS:: {s}", .{c.name});
                for (c.field_names, c.fields) |name, field| {
                    field.print(writer, name, depth + 1);
                }
                for (c.methods) |m| m.print(writer, "METHOD:: ", depth + 1);
            },
            .@"enum" => |e| {
                writer.print("ENUM:: {s} [", .{e.name});
                for (e.values) |v| writer.print("{s},", .{v});
                writer.print("]", .{});
            },
            .variable => |v| {
                writer.print("VARIABLE:: {s} mutable={}", .{ v.name, v.is_mutable });
                v.initializer.print(writer, "INIT:: ", depth + 1);
            },
            .expression => |e| e.print(writer, "EXPRESSION:: ", depth),
            .@"if" => |i| {
                writer.print("IF::", .{});
                i.condition.print(writer, "CONDITION:: ", depth + 1);
                for (i.then_branch) |s| s.print(writer, "THEN:: ", depth + 1);
                if (i.else_branch) |eb| {
                    for (eb) |s| s.print(writer, "ELSE:: ", depth + 1);
                }
            },
            .@"while" => |w| {
                writer.print("WHILE::", .{});
                w.condition.print(writer, "CONDITION:: ", depth + 1);
                for (w.body) |s| s.print(writer, "", depth + 1);
            },
            .@"for" => |f| {
                writer.print("FOR:: {s}", .{f.capture});
                f.iterator.print(writer, "IN:: ", depth + 1);
                for (f.body) |s| s.print(writer, "", depth + 1);
            },
            .divert => |d_| {
                writer.print("DIVERT:: ", .{});
                for (d_.path) |p| writer.print("{s}.", .{p});
            },
            .return_expression => |re| re.print(writer, "RETURN:: ", depth + 1),
            .return_void => writer.print("RETURN VOID", .{}),
            .fin => writer.print("FIN", .{}),
            .@"break" => writer.print("BREAK", .{}),
            .@"continue" => writer.print("CONTINUE", .{}),
            .comment => |c| writer.print("COMMENT:: {s}", .{c}),
            .@"switch" => |s| {
                writer.print("SWITCH::", .{});
                s.capture.print(writer, "CAPTURE:: ", depth + 1);
                for (s.prongs) |p| p.print(writer, "PRONG:: ", depth + 1);
            },
            .switch_prong => |p| {
                if (p.values) |vals| {
                    for (vals) |v| v.print(writer, "VALUE:: ", depth + 1);
                } else {
                    writer.print("DEFAULT::", .{});
                }
                for (p.body) |s| s.print(writer, "", depth + 1);
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
            else => std.debug.panic(
                "Internal compiler error: expected unary operator token (bang/minus), got {s} at line {d} — please report this",
                .{ @tagName(tok.token_type), tok.line },
            ),
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
            else => std.debug.panic(
                "Internal compiler error: expected binary operator token, got {s} at line {d} — please report this",
                .{ @tagName(tok.token_type), tok.line },
            ),
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
