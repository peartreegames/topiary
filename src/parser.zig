const std = @import("std");
const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;
const Arena = std.heap.ArenaAllocator;
const testing = std.testing;
const Lexer = @import("./lexer.zig").Lexer;
const tok = @import("./token.zig");
const ast = @import("./ast.zig");
const Errors = @import("./error.zig").Errors;
const Tree = ast.Tree;
const Statement = ast.Statement;
const Expression = ast.Expression;
const TokenType = tok.TokenType;
const Token = tok.Token;

const Precedence = enum(u4) {
    lowest,
    range,
    @"or",
    @"and",
    assign,
    equals,
    less_greater,
    sum,
    product,
    prefix,
    call,
    index,

    fn val(self: Precedence) u4 {
        return @intFromEnum(self);
    }
};

fn findPrecedence(token_type: TokenType) Precedence {
    return switch (token_type) {
        .dot_dot => .range,
        .@"or" => .@"or",
        .@"and" => .@"and",
        .equal, .plus_equal, .minus_equal, .slash_equal, .star_equal => .assign,
        .equal_equal, .bang_equal => .equals,
        .less, .greater, .less_equal, .greater_equal => .less_greater,
        .plus, .minus => .sum,
        .slash, .star, .percent => .product,
        .left_paren => .call,
        .left_bracket => .index,
        .dot => .index,
        else => .lowest,
    };
}
pub fn parse(allocator: Allocator, source: []const u8, err: *Errors) Parser.Error!Tree {
    var lexer = Lexer.init(source);
    var arena = std.heap.ArenaAllocator.init(allocator);
    errdefer arena.deinit();

    var parser = Parser{
        .current_token = lexer.next(),
        .peek_token = lexer.next(),
        .arena = arena.state,
        .allocator = arena.allocator(),
        .lexer = &lexer,
        .source = source,
        .err = err,
    };

    var nodes = ArrayList(Statement).init(parser.allocator);
    errdefer nodes.deinit();

    while (!parser.currentIs(.eof)) : (parser.next()) {
        try nodes.append(try parser.statement());
    }

    return Tree{
        .root = try nodes.toOwnedSlice(),
        .arena = arena.state,
        .allocator = allocator,
    };
}
pub const Parser = struct {
    current_token: Token,
    peek_token: Token,
    arena: std.heap.ArenaAllocator.State,
    allocator: Allocator,
    lexer: *Lexer,
    source: []const u8,
    err: *Errors,
    depth: usize = 0,

    pub const Error = error{
        ParserError,
        OutOfMemory,
        Overflow,
        InvalidCharacter,
    };

    fn allocate(self: Parser, value: anytype) !*@TypeOf(value) {
        const T = @TypeOf(value);
        std.debug.assert(@typeInfo(T) != .Pointer);
        const ptr = try self.allocator.create(T);
        ptr.* = value;
        std.debug.assert(std.meta.eql(ptr.*, value));
        return ptr;
    }

    fn fail(self: *Parser, comptime msg: []const u8, token: Token, args: anytype) Error {
        try self.err.add(msg, token, .err, args);
        return Error.ParserError;
    }

    fn print(self: *Parser, msg: []const u8) void {
        const peek_source = if (self.peekIs(.eof)) "[eof]" else self.source[self.peek_token.start..self.peek_token.end];
        std.log.warn("=={s}== -- {}:{s} -- {}:{s}", .{ msg, self.current_token.token_type, self.source[self.current_token.start..self.current_token.end], self.peek_token.token_type, peek_source });
    }

    fn next(self: *Parser) void {
        self.current_token = self.peek_token;
        self.peek_token = self.lexer.next();
    }

    fn consumeIdentifier(self: *Parser) ![]const u8 {
        try self.expectCurrent(.identifier);
        const name = try self.getStringValue();
        self.next();
        return name;
    }

    fn getStringValue(self: *Parser) ![]const u8 {
        return try self.allocator.dupe(u8, self.source[self.current_token.start..self.current_token.end]);
    }

    fn statement(self: *Parser) Error!Statement {
        return switch (self.current_token.token_type) {
            .class => try self.classDeclaration(),
            .@"enum" => try self.enumDeclaration(),
            .@"extern", .@"var", .@"const" => try self.varDeclaration(),
            .bough => try self.boughStatement(),
            .divert => try self.divertStatement(),
            .colon => try self.dialogueStatement(),
            .tilde => try self.choiceStatement(),
            .fork => try self.forkStatement(),
            .@"for" => try self.forStatement(),
            .@"if" => try self.ifStatement(),
            // .@"switch" => switchStatement(),
            .@"while" => try self.whileStatement(),
            .@"return" => try self.returnStatement(),
            .@"break" => try self.breakStatement(),
            .@"continue" => try self.continueStatement(),
            .left_brace => .{
                .token = self.current_token,
                .type = .{
                    .block = try self.block(),
                },
            },
            .comment => try self.commentStatement(),
            else => .{
                .token = self.current_token,
                .type = .{
                    .expression = try self.expression(.lowest),
                },
            },
        };
    }

    fn classDeclaration(self: *Parser) Error!Statement {
        var start = self.current_token;
        self.next();
        var name = try self.consumeIdentifier();
        try self.expectCurrent(.left_brace);
        self.next();

        var field_names = std.ArrayList([]const u8).init(self.allocator);
        var fields = std.ArrayList(Expression).init(self.allocator);
        errdefer field_names.deinit();
        errdefer fields.deinit();
        while (!self.currentIs(.right_brace)) {
            try field_names.append(try self.consumeIdentifier());
            try self.expectCurrent(.equal);
            self.next();
            try fields.append(try self.expression(.lowest));
            self.next();
            if (self.currentIs(.comma)) self.next();
        }
        try self.expectCurrent(.right_brace);
        if (field_names.items.len != fields.items.len) return self.fail("Missing field assignment value", self.current_token, .{});
        return .{
            .token = start,
            .type = .{
                .class = .{
                    .name = name,
                    .field_names = try field_names.toOwnedSlice(),
                    .fields = try fields.toOwnedSlice(),
                },
            },
        };
    }

    fn enumDeclaration(self: *Parser) Error!Statement {
        var start = self.current_token;
        self.next();
        var name = try self.consumeIdentifier();
        try self.expectCurrent(.left_brace);
        self.next();
        var values = std.ArrayList([]const u8).init(self.allocator);
        errdefer values.deinit();
        while (!self.currentIs(.right_brace)) {
            try values.append(try self.getStringValue());
            self.next();
            if (self.currentIs(.comma)) self.next();
        }
        return .{
            .token = start,
            .type = .{
                .@"enum" = .{
                    .name = name,
                    .values = try values.toOwnedSlice(),
                },
            },
        };
    }

    fn fieldDeclaration(self: *Parser) Error!Statement {
        var start = self.current_token;
        var name = try self.consumeIdentifier();
        try self.expectPeek(.equal);
        self.next();
        return .{
            .token = start,
            .type = .{
                .variable = .{
                    .name = name,
                    .initializer = try self.expression(.lowest),
                    .is_mutable = true,
                    .is_extern = false,
                },
            },
        };
    }

    fn varDeclaration(self: *Parser) Error!Statement {
        const start_token = self.current_token;
        var is_extern = false;
        if (self.currentIs(.@"extern")) {
            is_extern = true;
            self.next();
        }
        const is_mutable = self.currentIs(.@"var");
        self.next();
        const name = try self.consumeIdentifier();
        try self.expectCurrent(.equal);
        self.next();
        var expr = try self.expression(.lowest);
        if (expr.type == .function) {
            expr.type.function.name = name;
        }
        return .{
            .token = start_token,
            .type = .{
                .variable = .{
                    .name = name,
                    .initializer = expr,
                    .is_mutable = is_mutable,
                    .is_extern = is_extern,
                },
            },
        };
    }

    fn boughStatement(self: *Parser) Error!Statement {
        const start = self.current_token;
        self.next();
        var name = try self.consumeIdentifier();
        var body = try self.block();
        return .{
            .token = start,
            .type = .{
                .bough = .{
                    .name = name,
                    .body = body,
                },
            },
        };
    }

    fn returnStatement(self: *Parser) Error!Statement {
        const start_token = self.current_token;
        self.next();
        if (self.currentIsOneOf(&[_]TokenType{ .eof, .right_brace })) {
            return .{
                .token = start_token,
                .type = .return_void,
            };
        }
        const expr = try self.expression(.lowest);
        return .{
            .token = start_token,
            .type = .{
                .return_expression = expr,
            },
        };
    }

    fn identifierExpression(self: *Parser) Error!Expression {
        return .{
            .token = self.current_token,
            .type = .{
                .identifier = try self.getStringValue(),
            },
        };
    }

    fn expressionStatement(self: *Parser) Error!Statement {
        return .{
            .token = self.current_token,
            .type = .{
                .expression = try self.expression(.lowest),
            },
        };
    }

    fn expression(self: *Parser, prec: Precedence) Error!Expression {
        var start = self.current_token;
        var left: Expression = switch (self.current_token.token_type) {
            .identifier => try self.identifierExpression(),
            .number => blk: {
                const string_number = self.source[self.current_token.start..self.current_token.end];
                const value = try std.fmt.parseFloat(f32, string_number);
                break :blk .{
                    .token = start,
                    .type = .{
                        .number = value,
                    },
                };
            },
            .string => try self.stringExpression(),
            .bang, .minus => blk: {
                self.next();
                break :blk .{
                    .token = start,
                    .type = .{
                        .unary = .{
                            .operator = ast.UnaryOp.fromToken(start),
                            .value = try self.allocate(try self.expression(.prefix)),
                        },
                    },
                };
            },
            .true, .false => .{
                .token = start,
                .type = .{
                    .boolean = self.currentIs(.true),
                },
            },
            .@"if" => try self.ifExpression(),
            // group
            .left_paren => blk: {
                self.next();
                const exp = try self.expression(.lowest);
                try self.expectPeek(.right_paren);
                break :blk exp;
            },
            .left_brace => try self.mapSetExpression(),
            .left_bracket => try self.listExpression(),
            .new => try self.classExpression(),
            .pipe => try self.functionExpression(),
            .nil => .{ .token = self.current_token, .type = .nil },
            else => return self.fail("Unexpected token in expression: {}", self.current_token, .{self.current_token.token_type}),
        };

        const can_not_assign = self.peek_token.token_type == .equal and left.type != .identifier;
        if (can_not_assign) return self.fail("Cannot assign to {}", self.current_token, .{left.token.token_type});
        while (prec.val() < findPrecedence(self.peek_token.token_type).val()) {
            left = switch (self.peek_token.token_type) {
                .left_paren => try self.callExpression(left),
                .left_bracket, .dot => try self.indexExpression(left),
                .dot_dot => try self.range(left),
                .plus,
                .minus,
                .slash,
                .star,
                .percent,
                .equal_equal,
                .bang_equal,
                .less,
                .greater,
                .less_equal,
                .greater_equal,
                .@"and",
                .@"or",
                .equal,
                .plus_equal,
                .minus_equal,
                .slash_equal,
                .star_equal,
                .percent_equal,
                => blk: {
                    self.next();
                    const start_token = self.current_token;
                    const op = ast.BinaryOp.fromToken(self.current_token);
                    const inner_prec = findPrecedence(self.current_token.token_type);
                    self.next();
                    const allocated = try self.allocate(left);
                    break :blk .{
                        .token = start_token,
                        .type = .{
                            .binary = .{
                                .operator = op,
                                .left = allocated,
                                .right = try self.allocate(try self.expression(inner_prec)),
                            },
                        },
                    };
                },
                else => return left,
            };
        }
        return left;
    }

    fn functionExpression(self: *Parser) Error!Expression {
        var start = self.current_token;
        var list = ArrayList([]const u8).init(self.allocator);
        errdefer list.deinit();
        self.next();
        while (!self.currentIs(.pipe)) {
            try list.append(try self.consumeIdentifier());
            if (self.currentIs(.comma) or self.peekIs(.pipe)) self.next();
        }

        self.next();
        return .{
            .token = start,
            .type = .{
                .function = .{
                    .parameters = try list.toOwnedSlice(),
                    .body = try self.block(),
                },
            },
        };
    }

    // if no braces used will parse a single statement into a list
    fn block(self: *Parser) Error![]const Statement {
        var list = std.ArrayList(Statement).init(self.allocator);
        if (!self.currentIs(.left_brace)) {
            try list.append(try self.statement());
            return try list.toOwnedSlice();
        }
        self.next();
        while (!self.currentIsOneOf([2]TokenType{ .right_brace, .eof })) {
            try list.append(try self.statement());
            self.next();
        }
        return try list.toOwnedSlice();
    }

    fn classExpression(self: *Parser) Error!Expression {
        var start = self.current_token;
        self.next();
        const name = try self.consumeIdentifier();
        try self.expectCurrent(.left_brace);
        self.next();

        var field_names = std.ArrayList([]const u8).init(self.allocator);
        var fields = std.ArrayList(Expression).init(self.allocator);
        errdefer field_names.deinit();
        errdefer fields.deinit();
        while (!self.currentIs(.right_brace)) {
            try field_names.append(try self.consumeIdentifier());
            try self.expectCurrent(.equal);
            self.next();
            try fields.append(try self.expression(.lowest));
            self.next();
            if (self.currentIs(.comma)) self.next();
        }
        try self.expectCurrent(.right_brace);
        if (field_names.items.len != fields.items.len) return self.fail("Missing field assignment value", self.current_token, .{});

        return .{
            .token = start,
            .type = .{
                .class = .{
                    .name = name,
                    .field_names = try field_names.toOwnedSlice(),
                    .fields = try fields.toOwnedSlice(),
                },
            },
        };
    }

    fn stringExpression(self: *Parser) Error!Expression {
        var token = self.current_token;
        var value: []const u8 = "";
        var depth: usize = 0;
        var start: usize = token.start;

        var exprs = std.ArrayList(Expression).init(self.allocator);
        errdefer exprs.deinit();
        for (self.source[token.start..token.end], 0..) |char, i| {
            if (char == '{') {
                if (depth == 0) {
                    value = try std.fmt.allocPrint(self.allocator, "{s}{s}{s}", .{ value, self.source[start..(token.start + i)], "{}" });
                    start = token.start + i + 1;
                }
                depth += 1;
            }
            if (char == '}') {
                depth -= 1;
                if (depth == 0) {
                    try self.parseInterpolatedExpression(self.source[start..(token.start + i)], &exprs);
                    start = token.start + i + 1;
                }
            }
        }
        value = try std.fmt.allocPrint(self.allocator, "{s}{s}", .{ value, self.source[start..token.end] });
        return .{
            .token = token,
            .type = .{
                .string = .{
                    .value = value,
                    .expressions = try exprs.toOwnedSlice(),
                },
            },
        };
    }

    fn parseInterpolatedExpression(self: *Parser, source: []const u8, exprs: *std.ArrayList(Expression)) !void {
        var lexer = Lexer.init(source);

        var parser = Parser{
            .current_token = lexer.next(),
            .peek_token = lexer.next(),
            .arena = self.arena,
            .allocator = self.allocator,
            .lexer = &lexer,
            .source = source,
            .err = self.err,
        };

        while (!parser.currentIs(.eof)) : (parser.next()) {
            try exprs.append(try parser.expression(.lowest));
        }
    }

    fn listExpression(self: *Parser) Error!Expression {
        const start_token = self.current_token;
        var list = std.ArrayList(Expression).init(self.allocator);
        errdefer list.deinit();
        self.next();
        if (self.currentIs(.right_bracket)) {
            return .{
                .token = start_token,
                .type = .{
                    .list = try list.toOwnedSlice(),
                },
            };
        }
        try list.append(try self.expression(.lowest));
        while (self.peekIs(.comma)) {
            self.next();
            self.next();
            try list.append(try self.expression(.lowest));
        }
        self.next();
        try self.expectCurrent(.right_bracket);
        return .{
            .token = start_token,
            .type = .{
                .list = try list.toOwnedSlice(),
            },
        };
    }

    fn mapPairSetKey(self: *Parser) Error!Expression {
        const start_token = self.current_token;

        var key = try self.expression(.lowest);
        if (!self.peekIs(.colon)) return key;
        self.next();
        self.next();
        const value = try self.expression(.lowest);
        return .{
            .token = start_token,
            .type = .{
                .map_pair = .{
                    .key = try self.allocate(key),
                    .value = try self.allocate(value),
                },
            },
        };
    }

    fn mapSetExpression(self: *Parser) Error!Expression {
        const start_token = self.current_token;
        var list = std.ArrayList(Expression).init(self.allocator);
        errdefer list.deinit();
        self.next();

        if (self.currentIs(.right_brace)) {
            return .{ .token = start_token, .type = .{ .set = try list.toOwnedSlice() } };
        }
        if (self.currentIs(.colon)) {
            self.next();
            try self.expectCurrent(.right_brace);
            return .{ .token = start_token, .type = .{ .map = try list.toOwnedSlice() } };
        }

        const first = try self.mapPairSetKey();
        self.next();
        if (self.currentIs(.comma)) self.next();
        const is_map = first.type == .map_pair;
        const type_name = if (is_map) "map" else "set";
        try list.append(first);
        while (!self.currentIs(.right_brace)) {
            const item = try self.mapPairSetKey();
            if ((is_map and item.type != .map_pair) or (!is_map and item.type == .map_pair))
                return self.fail("Item type '{s}' cannot be added to type {s}", item.token, .{ @tagName(item.type), type_name });
            try list.append(item);
            self.next();
            if (self.currentIs(.comma) or self.peekIs(.right_brace)) self.next();
        }
        if (is_map) return .{ .token = start_token, .type = .{ .map = try list.toOwnedSlice() } };
        return .{ .token = start_token, .type = .{ .set = try list.toOwnedSlice() } };
    }

    fn ifExpression(self: *Parser) Error!Expression {
        self.next(); // skip if
        const condition = try self.allocate(try self.expression(.lowest));
        self.next();

        const true_value = try self.allocate(try self.expression(.lowest));
        try self.expectPeek(.@"else");
        self.next();
        const false_value = try self.allocate(try self.expression(.lowest));
        return .{
            .token = self.current_token,
            .type = .{
                .@"if" = .{
                    .condition = condition,
                    .then_value = true_value,
                    .else_value = false_value,
                },
            },
        };
    }

    fn ifStatement(self: *Parser) Error!Statement {
        self.next(); // skip if
        const condition = try self.allocate(try self.expression(.lowest));
        self.next();

        const true_body = try self.block();
        if (!self.peekIs(.@"else")) return .{
            .token = self.current_token,
            .type = .{
                .@"if" = .{
                    .condition = condition,
                    .then_branch = true_body,
                    .else_branch = null,
                },
            },
        };
        self.next();
        self.next();
        const false_body = try self.block();
        return .{
            .token = self.current_token,
            .type = .{
                .@"if" = .{
                    .condition = condition,
                    .then_branch = true_body,
                    .else_branch = false_body,
                },
            },
        };
    }

    fn divertStatement(self: *Parser) Error!Statement {
        const start_token = self.current_token;
        // TODO: Perhaps this should just be an expression and indexers used
        var list = std.ArrayList([]const u8).init(self.allocator);
        errdefer list.deinit();
        try self.expectPeek(.identifier);
        try list.append(try self.getStringValue());
        while (self.peekIs(.dot)) {
            self.next();
            self.next();
            try list.append(try self.getStringValue());
        }

        return .{
            .token = start_token,
            .type = .{
                .divert = try list.toOwnedSlice(),
            },
        };
    }

    fn forkStatement(self: *Parser) Error!Statement {
        const start = self.current_token;
        var name: ?[]const u8 = null;
        self.next();
        if (self.currentIs(.identifier)) name = try self.consumeIdentifier();
        return .{
            .token = start,
            .type = .{
                .fork = .{
                    .name = name,
                    .body = try self.block(),
                },
            },
        };
    }

    fn choiceStatement(self: *Parser) Error!Statement {
        const start = self.current_token;
        self.next();
        var text = try self.stringExpression();
        self.next();
        return .{
            .token = start,
            .type = .{
                .choice = .{
                    .text = text,
                    .body = try self.block(),
                },
            },
        };
    }

    fn dialogueStatement(self: *Parser) Error!Statement {
        const start_token = self.current_token;
        self.next();
        var speaker: ?[]const u8 = null;
        if (self.currentIs(.identifier)) {
            speaker = try self.consumeIdentifier();
        }
        try self.expectCurrent(.colon);
        self.next();
        var text = try self.stringExpression();
        var tags = std.ArrayList([]const u8).init(self.allocator);
        while (self.peekIs(.hash)) {
            self.next();
            try tags.append(try self.getStringValue());
        }

        return .{
            .token = start_token,
            .type = .{
                .dialogue = .{
                    .speaker = speaker,
                    .content = try self.allocate(text),
                    .tags = try tags.toOwnedSlice(),
                },
            },
        };
    }

    fn arguments(self: *Parser) Error![]Expression {
        var list = ArrayList(Expression).init(self.allocator);
        errdefer list.deinit();

        // no arguments
        if (self.peekIs(.right_paren)) {
            self.next();
            return list.toOwnedSlice();
        }

        self.next();
        try list.append(try self.expression(.lowest));

        while (self.peekIs(.comma)) {
            self.next();
            self.next();
            try list.append(try self.expression(.lowest));
        }
        try self.expectPeek(.right_paren);
        return list.toOwnedSlice();
    }

    fn callExpression(self: *Parser, func: Expression) Error!Expression {
        var start_token = self.current_token;
        self.next();
        return .{
            .token = start_token,
            .type = .{
                .call = .{
                    .target = try self.allocate(func),
                    .arguments = try self.arguments(),
                },
            },
        };
    }

    fn indexExpression(self: *Parser, target: Expression) Error!Expression {
        self.next();
        const start_token = self.current_token;
        self.next();
        const index = if (start_token.token_type == .dot) blk: {
            if (!self.currentIs(.identifier))
                return self.fail("Expected identifier after index '.', found {}", self.current_token, .{self.current_token.token_type});
            break :blk try self.identifierExpression();
        } else if (start_token.token_type == .left_bracket) blk: {
            break :blk try self.expression(.lowest);
        } else unreachable;

        if (start_token.token_type == .left_bracket) {
            try self.expectPeek(.right_bracket);
        }

        return .{
            .token = start_token,
            .type = .{
                .indexer = .{
                    .target = try self.allocate(target),
                    .index = try self.allocate(index),
                },
            },
        };
    }

    fn whileStatement(self: *Parser) Error!Statement {
        const start_token = self.current_token;
        self.next();
        const condition = try self.expression(.lowest);
        try self.expectPeek(.left_brace);
        return .{
            .token = start_token,
            .type = .{
                .@"while" = .{
                    .condition = condition,
                    .body = try self.block(),
                },
            },
        };
    }

    fn forStatement(self: *Parser) Error!Statement {
        const start_token = self.current_token;
        self.next();
        var iterator = try self.expression(.lowest);
        switch (iterator.type) {
            .range, .identifier => {},
            else => return self.fail("Expected list, set, map, or range in for loop, found {}", iterator.token, .{iterator.token.token_type}),
        }

        try self.expectPeek(.pipe);
        self.next();
        const capture = try self.consumeIdentifier();
        try self.expectPeek(.left_brace);
        const body = try self.block();

        var index_variable = Statement{
            .token = start_token,
            .type = .{
                .variable = .{
                    .name = "__FOR_LOOP_IDX__",
                    .is_mutable = true,
                    .initializer = .{
                        .token = start_token,
                        .type = .{
                            .number = 0,
                        },
                    },
                },
            },
        };

        var one = Expression{
            .token = start_token,
            .type = .{ .number = 1 },
        };

        var increment = Expression{
            .token = start_token,
            .type = .{
                .binary = .{
                    .operator = .add,
                    .left = &index_variable.type.variable.initializer,
                    .right = &one,
                },
            },
        };
        var count = Expression{
            .token = start_token,
            .type = .{
                .identifier = "count",
            },
        };
        var indexer = Expression{
            .token = start_token,
            .type = .{
                .indexer = .{
                    .target = &iterator,
                    .index = &count,
                },
            },
        };
        var condition = Expression{
            .token = start_token,
            .type = .{
                .binary = .{
                    .operator = .less_than,
                    .left = &index_variable.type.variable.initializer,
                    .right = &indexer,
                },
            },
        };
        return .{
            .token = start_token,
            .type = .{
                .@"for" = .{
                    .index = &index_variable,
                    .increment = increment,
                    .condition = condition,
                    .capture = capture,
                    .iterator = iterator,
                    .body = body,
                },
            },
        };
    }

    fn range(self: *Parser, left: Expression) Error!Expression {
        self.next();
        self.next();
        return .{
            .token = self.current_token,
            .type = .{
                .range = .{
                    .left = try self.allocate(left),
                    .right = try self.allocate(try self.expression(.lowest)),
                },
            },
        };
    }
    // fn switchStatement(self: *Parser) Error!Node {
    //     const node = try self.allocator.create(Node.SwitchLiteral);
    //     node.* = .{ .token = self.current_token, .capture = undefined, .prongs = undefined };
    //
    //     try self.expectPeek(.left_paren);
    //
    //     self.next();
    //     node.capture = try self.expression(.lowest);
    //
    //     try self.expectPeek(.right_paren);
    //     try self.expectPeek(.left_brace);
    //
    //     self.next();
    //     var prongs = std.ArrayList(Node).init(self.allocator);
    //
    //     try prongs.append(try self.parseSwitchProng());
    //
    //     while (self.peekIs(.comma)) {
    //         self.next();
    //         self.next();
    //         try prongs.append(try self.parseSwitchProng());
    //     }
    //     node.prongs = try prongs.toOwnedSlice();
    //
    //     try self.expectPeek(.right_brace);
    //
    //     return Node{ .@"switch" = node };
    // }
    //
    // /// Parses a switch prong i.e. x: 5 + 5 into a `Node.SwitchProng`
    // fn switchCase(self: *Parser) Error!Node {
    //     const node = try self.allocator.create(Node.SwitchProng);
    //     node.* = .{ .token = undefined, .left = try self.expression(.lowest), .right = undefined };
    //
    //     try self.expectPeek(.colon);
    //     node.token = self.current_token;
    //     self.next();
    //
    //     node.right = if (self.currentIs(.left_brace))
    //         try self.parseBlockStatement(.right_brace)
    //     else
    //         try self.expressionStatement();
    //
    //     return Node{ .switch_prong = node };
    //
    //
    fn commentStatement(self: *Parser) Error!Statement {
        return .{
            .token = self.current_token,
            .type = .{
                .comment = try self.getStringValue(),
            },
        };
    }

    fn breakStatement(self: *Parser) Error!Statement {
        return .{
            .token = self.current_token,
            .type = .@"break",
        };
    }

    fn continueStatement(self: *Parser) Error!Statement {
        return .{
            .token = self.current_token,
            .type = .@"continue",
        };
    }

    fn expectCurrent(self: *Parser, comptime token_type: TokenType) !void {
        if (self.currentIs(token_type)) return;
        return self.fail(
            "Expected current token to be '" ++ tok.toString(token_type) ++ "', found {}",
            self.current_token,
            .{self.current_token.token_type},
        );
    }

    fn expectPeek(self: *Parser, comptime token_type: TokenType) !void {
        if (self.peekIs(token_type)) {
            self.next();
            return;
        }

        return self.fail(
            "Expected next token to be '" ++ tok.toString(token_type) ++ "', found {}",
            self.peek_token,
            .{self.peek_token.token_type},
        );
    }

    fn peekIs(self: Parser, token_type: TokenType) bool {
        return self.peek_token.token_type == token_type;
    }

    fn peekIsOneOf(self: *Parser, token_types: anytype) bool {
        for (token_types) |token_type| {
            if (self.peekIs(token_type)) return true;
        }
        return false;
    }

    fn currentIs(self: Parser, token_type: TokenType) bool {
        return self.current_token.token_type == token_type;
    }

    fn currentIsOneOf(self: Parser, token_types: anytype) bool {
        for (token_types) |token_type| {
            if (self.currentIs(token_type)) return true;
        }
        return false;
    }
};

// test "Parse Import" {
//     const allocator = testing.allocator;
//     const input = "import \"./globals.topi\"";
//     var errors = Errors.init(allocator);
//     defer errors.deinit();
//     const tree = parse(allocator, input, &errors) catch |err| {
//         try errors.write(input, std.io.getStdErr().writer());
//         return err;
//     };
//     defer tree.deinit();
//     try testing.expectEqualStrings("./globals.topi", tree.root[0].type.import);
// }

test "Parse Declaration" {
    var allocator = testing.allocator;
    var t =
        \\ const intValue = 5
        \\ var mutableValue = 1.2
        \\ class ClassType {
        \\     intField = 0
        \\ }
        \\ var classValue = new ClassType{}
        \\ enum EnumType {
        \\     one,
        \\     two,
        \\ }
        \\ const enumValue = EnumType.one
    ;

    var errors = Errors.init(allocator);
    defer errors.deinit();
    const tree = parse(allocator, t, &errors) catch |err| {
        try errors.write(t, std.io.getStdErr().writer());
        return err;
    };
    defer tree.deinit();
    try testing.expect(tree.root.len == 6);
    try testing.expect(!tree.root[0].type.variable.is_mutable);
    try testing.expect(tree.root[1].type.variable.is_mutable);
    try testing.expect(tree.root[1].type.variable.initializer.type.number == 1.2);

    try testing.expect(tree.root[2].type.class.fields.len == 1);
    try testing.expectEqualStrings("intField", tree.root[2].type.class.field_names[0]);
    try testing.expect(tree.root[2].type.class.fields[0].type.number == 0);

    try testing.expect(tree.root[3].type.variable.is_mutable);
    try testing.expectEqualStrings("ClassType", tree.root[3].type.variable.initializer.type.class.name);
    try testing.expect(tree.root[3].type.variable.initializer.type.class.fields.len == 0);

    try testing.expectEqualStrings("EnumType", tree.root[4].type.@"enum".name);
    try testing.expect(tree.root[4].type.@"enum".values.len == 2);
    try testing.expectEqualStrings("one", tree.root[4].type.@"enum".values[0]);
    try testing.expectEqualStrings("two", tree.root[4].type.@"enum".values[1]);
}

test "Parse Function Declaration" {
    var t =
        \\ const sum = |x, y| return x + y
        \\ const str = |value, count| {
        \\    var result = "This is a string"
        \\    return result    
        \\ }
    ;
    var allocator = testing.allocator;
    var errors = Errors.init(allocator);
    defer errors.deinit();
    const tree = parse(allocator, t, &errors) catch |err| {
        try errors.write(t, std.io.getStdErr().writer());
        return err;
    };
    defer tree.deinit();
    try testing.expect(tree.root.len == 2);
    try testing.expect(!tree.root[0].type.variable.is_mutable);
    try testing.expectEqualStrings("x", tree.root[0].type.variable.initializer.type.function.parameters[0]);
    try testing.expectEqualStrings("y", tree.root[0].type.variable.initializer.type.function.parameters[1]);
    try testing.expect(tree.root[0].type.variable.initializer.type.function.body.len == 1);
}

test "Parse Function Arguments" {
    var t =
        \\ const sum = |x, y| return x + y
        \\ sum(1, 2) + sum(3, 4)
    ;
    var allocator = testing.allocator;
    var errors = Errors.init(allocator);
    defer errors.deinit();
    const tree = parse(allocator, t, &errors) catch |err| {
        try errors.write(t, std.io.getStdErr().writer());
        return err;
    };
    defer tree.deinit();
    // tree.print(std.debug);
    try testing.expect(tree.root.len == 2);
    try testing.expect(tree.root[1].type.expression.type.binary.operator == .add);
    const bin = tree.root[1].type.expression.type.binary;
    try testing.expect(bin.right.type.call.arguments.len == 2);
    try testing.expect(bin.right.type.call.arguments[0].type.number == 3);
    try testing.expect(bin.right.type.call.arguments[1].type.number == 4);
    try testing.expect(bin.left.type.call.arguments.len == 2);
    try testing.expect(bin.left.type.call.arguments[0].type.number == 1);
    try testing.expect(bin.left.type.call.arguments[1].type.number == 2);
}

test "Parse Enums" {
    var t =
        \\ enum Test {
        \\    one,
        \\    two  
        \\ }
        \\ var value = Test.one
    ;
    var allocator = testing.allocator;
    var errors = Errors.init(allocator);
    defer errors.deinit();
    const tree = parse(allocator, t, &errors) catch |err| {
        try errors.write(t, std.io.getStdErr().writer());
        return err;
    };
    defer tree.deinit();
    try testing.expect(tree.root.len == 2);
    const e = tree.root[0].type.@"enum";
    try testing.expectEqualStrings("Test", e.name);
    try testing.expectEqualStrings("one", e.values[0]);
    try testing.expectEqualStrings("two", e.values[1]);

    try testing.expect(tree.root[1].type.variable.initializer.type == .indexer);
}

test "Parse Iterable Types" {
    var allocator = testing.allocator;
    const test_cases = .{
        .{ .input = "const stringList = [\"item\"]", .id = "stringList", .item_value = [_][]const u8{"item"}, .mutable = false, .type = .list },
        .{ .input = "const stringList = [\"item1\", \"item2\"]", .id = "stringList", .item_value = [_][]const u8{ "item1", "item2" }, .mutable = false, .type = .list },
        .{ .input = "var floatSet = {2.0}", .id = "floatSet", .item_value = [_]f32{2.0}, .mutable = true, .type = .set },
        .{ .input = "var floatSet = {2.0, 3.4, 5.6}", .id = "floatSet", .item_value = [_]f32{ 2.0, 3.4, 5.6 }, .mutable = true, .type = .set },
        .{ .input = "var stringBoolMap = {\"key\":true}", .id = "stringBoolMap", .item_value = [_]bool{true}, .mutable = true, .type = .map },
        .{ .input = "var stringBoolMap = {\"key1\":true, \"key2\": false, \"key3\": true}", .id = "stringBoolMap", .item_value = [_]bool{ true, false, true }, .mutable = true, .type = .map },
    };

    inline for (test_cases) |case| {
        var errors = Errors.init(allocator);
        defer errors.deinit();
        const tree = parse(allocator, case.input, &errors) catch |err| {
            try errors.write(case.input, std.io.getStdErr().writer());
            return err;
        };
        defer tree.deinit();
        const node = tree.root[0].type.variable;
        try testing.expectEqualStrings(case.id, node.name);

        for (case.item_value, 0..) |value, i| {
            switch (case.type) {
                .list => try testing.expectEqualStrings(value, node.initializer.type.list[i].type.string.value),
                .set => try testing.expect(value == node.initializer.type.set[i].type.number),
                .map => try testing.expect(value == node.initializer.type.map[i].type.map_pair.value.type.boolean),
                else => unreachable,
            }
        }
        try testing.expect(case.mutable == node.is_mutable);
    }
}

test "Parse Empty Iterable Types" {
    const allocator = testing.allocator;
    const input =
        \\ const emptyMap = {:}
        \\ const emptySet = {}
    ;
    var errors = Errors.init(allocator);
    defer errors.deinit();
    const tree = parse(allocator, input, &errors) catch |err| {
        try errors.write(input, std.io.getStdErr().writer());
        return err;
    };
    defer tree.deinit();
    var decl = tree.root[0].type.variable;
    try testing.expectEqualStrings("emptyMap", decl.name);
    try testing.expect(decl.initializer.type.map.len == 0);

    decl = tree.root[1].type.variable;
    try testing.expectEqualStrings("emptySet", decl.name);
    try testing.expect(decl.initializer.type.set.len == 0);
}

test "Parse Nested Iterable Types" {
    const allocator = testing.allocator;
    const input =
        \\ [[1,2]]
    ;

    var errors = Errors.init(allocator);
    defer errors.deinit();
    const tree = parse(allocator, input, &errors) catch |err| {
        try errors.write(input, std.io.getStdErr().writer());
        return err;
    };
    defer tree.deinit();
    try testing.expect(tree.root[0].type.expression.type == .list);
    try testing.expect(tree.root[0].type.expression.type.list[0].type == .list);
}

test "Parse Extern" {
    var allocator = testing.allocator;
    const test_cases = .{
        .{ .input = "extern const x = 0", .id = "x", .mutable = false, .@"extern" = true },
        .{ .input = "extern var y = 0", .id = "y", .mutable = true, .@"extern" = true },
    };

    inline for (test_cases) |case| {
        var errors = Errors.init(allocator);
        defer errors.deinit();
        const tree = parse(allocator, case.input, &errors) catch |err| {
            try errors.write(case.input, std.io.getStdErr().writer());
            return err;
        };
        defer tree.deinit();
        const decl = tree.root[0].type.variable;
        try testing.expectEqualStrings(case.id, decl.name);
        try testing.expect(case.mutable == decl.is_mutable);
        try testing.expect(case.@"extern" == decl.is_extern);
    }
}

test "Parse Enum" {
    const allocator = testing.allocator;
    const input =
        \\ enum E {
        \\     one,
        \\     two,
        \\ }
        \\
        \\ enum En {
        \\     three,
        \\     four
        \\ }
        \\ const val = En.three
    ;
    var errors = Errors.init(allocator);
    defer errors.deinit();
    const tree = parse(allocator, input, &errors) catch |err| {
        try errors.write(input, std.io.getStdErr().writer());
        return err;
    };
    defer tree.deinit();
    var decl = tree.root[0].type.@"enum";
    try testing.expectEqualStrings("E", decl.name);
    try testing.expectEqualStrings("one", decl.values[0]);
    try testing.expectEqualStrings("two", decl.values[1]);

    decl = tree.root[1].type.@"enum";
    try testing.expectEqualStrings("En", decl.name);
    try testing.expectEqualStrings("three", decl.values[0]);
    try testing.expectEqualStrings("four", decl.values[1]);

    var varDecl = tree.root[2].type.variable;
    try testing.expectEqualStrings("val", varDecl.name);
    try testing.expectEqualStrings("En", varDecl.initializer.type.indexer.target.type.identifier);
    try testing.expectEqualStrings("three", varDecl.initializer.type.indexer.index.type.identifier);
}

test "Parse If" {
    const allocator = testing.allocator;
    const input =
        \\ var value = 0
        \\ if true value = 1
        \\ else if 5 < 1 value = 2
        \\ else value = 3
        \\ const a = if true "true" else "false"
        \\
        \\ if true {
        \\     value = 4  
        \\ } else {
        \\    value = 5    
        \\ }
    ;
    var errors = Errors.init(allocator);
    defer errors.deinit();
    const tree = parse(allocator, input, &errors) catch |err| {
        try errors.write(input, std.io.getStdErr().writer());
        return err;
    };
    defer tree.deinit();

    var if_stmt = tree.root[1].type.@"if";
    try testing.expect(if_stmt.condition.type.boolean);
    try testing.expect(if_stmt.then_branch[0].type.expression.type.binary.right.type.number == 1);
    try testing.expect(if_stmt.else_branch.?[0].type.@"if".then_branch[0].type.expression.type.binary.right.type.number == 2);
    try testing.expect(if_stmt.else_branch.?[0].type.@"if".else_branch.?[0].type.expression.type.binary.right.type.number == 3);

    const decl = tree.root[2].type.variable;
    try testing.expectEqualStrings("true", decl.initializer.type.@"if".then_value.type.string.value);
    try testing.expectEqualStrings("false", decl.initializer.type.@"if".else_value.type.string.value);

    if_stmt = tree.root[3].type.@"if";

    try testing.expect(if_stmt.condition.type.boolean);
    try testing.expect(if_stmt.then_branch[0].type.expression.type.binary.right.type.number == 4);
    try testing.expect(if_stmt.else_branch.?[0].type.expression.type.binary.right.type.number == 5);
}

test "Parse Call expression" {
    const input =
        \\ add(1, 2 * 3, 4 + 5)
    ;
    const allocator = testing.allocator;
    var errors = Errors.init(allocator);
    defer errors.deinit();
    const tree = parse(allocator, input, &errors) catch |err| {
        try errors.write(input, std.io.getStdErr().writer());
        return err;
    };
    defer tree.deinit();

    const call = tree.root[0].type.expression.type.call;
    try testing.expectEqualStrings("add", call.target.type.identifier);
    try testing.expect(call.arguments.len == 3);
    try testing.expect(call.arguments[0].type.number == 1);
    try testing.expect(call.arguments[1].type.binary.operator == .multiply);
    try testing.expect(call.arguments[2].type.binary.right.type.number == 5);
}

test "Parse For loop" {
    const input =
        \\ for list |item| {
        \\ }
        \\ for 0..10 |i| {
        \\ }
        \\ for map |keyValue| {
        \\ }
    ;
    const allocator = testing.allocator;

    var errors = Errors.init(allocator);
    defer errors.deinit();
    const tree = parse(allocator, input, &errors) catch |err| {
        try errors.write(input, std.io.getStdErr().writer());
        return err;
    };
    defer tree.deinit();

    var loop = tree.root[0].type.@"for";
    try testing.expectEqualStrings("list", loop.iterator.type.identifier);
    try testing.expectEqualStrings("item", loop.capture);

    loop = tree.root[1].type.@"for";
    try testing.expect(loop.iterator.type.range.left.type.number == 0);
    try testing.expect(loop.iterator.type.range.right.type.number == 10);
    try testing.expectEqualStrings("i", loop.capture);

    loop = tree.root[2].type.@"for";
    try testing.expectEqualStrings("map", loop.iterator.type.identifier);
    try testing.expectEqualStrings("keyValue", loop.capture);
}

test "Parse While loop" {
    const input =
        \\ while x < y { x }"
    ;
    const allocator = testing.allocator;

    var errors = Errors.init(allocator);
    defer errors.deinit();
    const tree = parse(allocator, input, &errors) catch |err| {
        try errors.write(input, std.io.getStdErr().writer());
        return err;
    };
    defer tree.deinit();
    var loop = tree.root[0].type.@"while";
    try testing.expect(loop.condition.type.binary.operator == .less_than);
    try testing.expectEqualStrings("x", loop.body[0].type.expression.type.identifier);
}

test "Parse Bough" {
    const input =
        \\ === BOUGH {
        \\     :Speaker: "Text goes here" # tagline #tagother#taglast
        \\ }
    ;
    const allocator = testing.allocator;
    var errors = Errors.init(allocator);
    defer errors.deinit();
    const tree = parse(allocator, input, &errors) catch |err| {
        try errors.write(input, std.io.getStdErr().writer());
        return err;
    };
    defer tree.deinit();
    const bough = tree.root[0].type.bough;
    try testing.expectEqualStrings(bough.name, "BOUGH");

    const line = bough.body[0].type.dialogue;
    try testing.expectEqualStrings("Speaker", line.speaker.?);
    try testing.expectEqualStrings("Text goes here", line.content.type.string.value);
    try testing.expectEqualStrings("tagline", line.tags[0]);
    try testing.expectEqualStrings("tagother", line.tags[1]);
    try testing.expectEqualStrings("taglast", line.tags[2]);
}

test "Parse No Speaker" {
    const allocator = testing.allocator;
    const input =
        \\  === BOUGH {
        \\      :: "Text goes here"
        \\  }
    ;
    var errors = Errors.init(allocator);
    defer errors.deinit();
    const tree = parse(allocator, input, &errors) catch |err| {
        try errors.write(input, std.io.getStdErr().writer());
        return err;
    };
    defer tree.deinit();
    const line = tree.root[0].type.bough.body[0].type.dialogue;
    try testing.expect(line.speaker == null);
    try testing.expectEqualStrings("Text goes here", line.content.type.string.value);
}

test "Parse divert" {
    const allocator = testing.allocator;
    const input =
        \\  === BOUGH {}
        \\  => BOUGH
    ;
    var errors = Errors.init(allocator);
    defer errors.deinit();
    const tree = parse(allocator, input, &errors) catch |err| {
        try errors.write(input, std.io.getStdErr().writer());
        return err;
    };
    defer tree.deinit();
    const divert = tree.root[1].type.divert;
    try testing.expectEqualStrings("BOUGH", divert[0]);
}

test "Parse Forks" {
    const allocator = testing.allocator;
    const input =
        \\  === BOUGH {
        \\      fork {
        \\          ~ "choice 1" {
        \\              :Other: "Response"
        \\          }
        \\          ~ "choice 2" => END
        \\      }
        \\  }
        \\  === END {}
    ;
    var errors = Errors.init(allocator);
    defer errors.deinit();
    const tree = parse(allocator, input, &errors) catch |err| {
        try errors.write(input, std.io.getStdErr().writer());
        return err;
    };
    defer tree.deinit();

    var fork = tree.root[0].type.bough.body[0].type.fork;
    try testing.expect(fork.name == null);

    var choice = fork.body[0].type.choice;
    try testing.expectEqualStrings("choice 1", choice.text.type.string.value);
    var line = choice.body[0].type.dialogue;
    try testing.expectEqualStrings("Other", line.speaker.?);
    try testing.expectEqualStrings("Response", line.content.type.string.value);

    choice = fork.body[1].type.choice;
    try testing.expectEqualStrings("choice 2", choice.text.type.string.value);
    try testing.expect(choice.body[0].type == .divert);
}

test "Parse Inline Code" {
    const allocator = testing.allocator;
    const input =
        \\  === BOUGH {
        \\      :Speaker: "{sayHello()}, how are you?"
        \\  }
    ;
    var errors = Errors.init(allocator);
    defer errors.deinit();
    const tree = parse(allocator, input, &errors) catch |err| {
        try errors.write(input, std.io.getStdErr().writer());
        return err;
    };
    defer tree.deinit();
    const dialogue = tree.root[0].type.bough.body[0].type.dialogue;
    const string = dialogue.content.type.string;
    try testing.expectEqualStrings("sayHello", string.expressions[0].type.call.target.type.identifier);
    try testing.expectEqualStrings("{}, how are you?", string.value);
}
