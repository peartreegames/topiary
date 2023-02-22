// Based on a combination of two other parsers:
// https://github.com/Luukdegram/luf/blob/master/src/parser.zig
// https://github.com/MasterQ32/LoLa/blob/master/src/library/compiler/parser.zig

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
        return @enumToInt(self);
    }
};

fn findPrecedence(token_type: TokenType) Precedence {
    return switch (token_type) {
        .double_period => .range,
        .@"or" => .@"or",
        .@"and" => .@"and",
        .assign => .assign,
        .equal, .not_equal => .equals,
        .less_than, .greater_than, .less_than_equal, .greater_than_equal => .less_greater,
        .plus, .minus => .sum,
        .slash, .asterisk, .percent => .product,
        .left_parenthesis => .call,
        .left_bracket => .index,
        .period => .index,
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
        try nodes.append(try parser.parseStatement());
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
    mode: Mode = .root,

    const Mode = enum {
        root,
        code,
        content,
    };

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
        std.log.warn("{s} -- {}", .{ msg, self.current_token.token_type });
    }
    fn printPeek(self: *Parser, msg: []const u8) void {
        std.log.warn("{s} -- {}", .{ msg, self.peek_token.token_type });
    }

    fn skipEol(self: *Parser) void {
        while (self.peekIs(.eol)) {
            self.peek_token = self.lexer.next();
        }
    }

    fn switchMode(self: *Parser) void {
        self.peek_token = self.lexer.next();
        self.mode = if (self.mode == .code) .content else .code;
    }

    fn next(self: *Parser) void {
        self.current_token = self.peek_token;

        self.peek_token = self.lexer.next();
        // switch mode on inline code blocks
        if (self.peekIs(.backtick)) self.switchMode();
        // eol are only needed for content
        if (self.mode == .code) self.skipEol();
    }

    fn parseStatement(self: *Parser) Error!Statement {
        while (self.currentIs(.eol)) self.next();
        return switch (self.mode) {
            .root => return switch (self.current_token.token_type) {
                .blackboard, .bough => self.parseBlockStatement(),
                .import => blk: {
                    self.next(); // skip import keyword
                    const token = self.current_token;
                    break :blk .{
                        .token = token,
                        .type = .{
                            .import = try self.allocator.dupe(u8, self.source[token.start..token.end]),
                        },
                    };
                },
                else => self.fail("Expected blackboard, bough, or import at top level.", self.current_token, .{}),
            },
            .code => return switch (self.current_token.token_type) {
                .comment => self.parseComment(),
                .constant, .variable => self.parseDeclaration(),
                .@"return" => self.parseReturn(),
                // .@"switch" => self.parseSwitchStatement(),
                .while_loop => self.parseWhile(),
                .@"if" => try self.parseIfStatement(),
                .for_loop => self.parseFor(),
                .@"break" => self.parseBreak(),
                .@"continue" => self.parseContinue(),
                .backtick => {
                    self.next();
                    self.mode = .content;
                    return self.parseStatement();
                },
                else => self.parseExpressionStatement(),
            },
            .content => return switch (self.current_token.token_type) {
                .bough => return self.fail("Bough must be top level", self.current_token, .{}),
                .comment => self.parseComment(),
                .twig => self.parseBlockStatement(),
                .jump => self.parseJump(),
                .split => self.parseSplit(),
                .colon => self.parseDialogueLine(),
                // .gather => return self.fail("Unexpected gather", self.current_token, .{}),
                .backtick => {
                    self.next();
                    self.mode = .code;
                    return self.parseExpressionStatement();
                },
                else => self.parseExpressionStatement(),
            },
        };
    }

    fn parseDeclaration(self: *Parser) Error!Statement {
        if (self.mode != .code) return self.fail("Declarations can only be used in code", self.current_token, .{});
        const start_token = self.current_token;
        const is_mutable = self.currentIs(.variable);

        try self.expectPeek(.identifier);
        const name = try self.allocator.dupe(u8, self.source[self.current_token.start..self.current_token.end]);
        try self.expectPeek(.colon);
        self.next();

        var decl_type = self.current_token.token_type;
        const type_def = try self.parseTypeDefValue();
        try self.expectPeek(.assign);
        self.next();

        if (decl_type == .function_keyword and is_mutable) {
            return self.fail("Functions must be 'const'", start_token, .{});
        }

        if (decl_type == .enum_keyword and is_mutable) {
            return self.fail("Enums must be 'const'", start_token, .{});
        }

        const value_type = self.current_token.token_type;
        if (value_type == .@"extern") {
            const value = .{ .token = self.current_token, .type = .@"extern" };
            return .{
                .token = self.current_token,
                .type = .{
                    .declaration = .{
                        .name = name,
                        .value = value,
                        .type_def = type_def,
                        .is_mutable = is_mutable,
                    },
                },
            };
        }

        const value = switch (decl_type) {
            .function_keyword => try self.parseFunctionLiteral(),
            .enum_keyword => try self.parseEnumLiteral(),
            else => try self.parseExpression(.lowest),
        };
        return .{
            .token = self.current_token,
            .type = .{
                .declaration = .{
                    .name = name,
                    .value = value,
                    .type_def = type_def,
                    .is_mutable = is_mutable,
                },
            },
        };
    }

    fn parseReturn(self: *Parser) Error!Statement {
        const start_token = self.current_token;
        if (self.peekIs(.eol)) {
            return .{
                .token = start_token,
                .type = .return_void,
            };
        }

        self.next();
        return .{
            .token = start_token,
            .type = .{
                .return_expression = try self.parseExpression(.lowest),
            },
        };
    }

    fn parseIdentifier(self: *Parser) Error!Expression {
        return .{
            .token = self.current_token,
            .type = .{
                .identifier = try self.allocator.dupe(u8, self.source[self.current_token.start..self.current_token.end]),
            },
        };
    }

    fn parseAssignment(self: *Parser, target: Expression) Error!Expression {
        if (self.mode != .code) return self.fail("Assignemnt can only be used in code", self.current_token, .{});
        if (target.type != .identifier and target.type != .indexer)
            return self.fail("Expected identifier or index", target.token, .{});

        self.next();
        return .{
            .token = self.current_token,
            .type = .{
                .assignment = .{
                    .target = try self.allocate(target),
                    .value = try self.allocate(try self.parseExpression(.lowest)),
                },
            },
        };
    }

    fn parseExpressionStatement(self: *Parser) Error!Statement {
        return .{
            .token = self.current_token,
            .type = .{
                .expression = try if (self.mode == .code) self.parseExpression(.lowest) else self.parseContentExpression(),
            },
        };
    }

    fn parseExpression(self: *Parser, prec: Precedence) Error!Expression {
        var left: Expression = switch (self.current_token.token_type) {
            .identifier => try self.parseIdentifier(),
            .integer => blk: {
                const string_number = self.source[self.current_token.start..self.current_token.end];
                const value = try std.fmt.parseInt(i32, string_number, 10);
                break :blk .{
                    .token = self.current_token,
                    .type = .{
                        .integer_literal = value,
                    },
                };
            },
            .float => blk: {
                const string_number = self.source[self.current_token.start..self.current_token.end];
                const value = try std.fmt.parseFloat(f32, string_number);
                break :blk .{
                    .token = self.current_token,
                    .type = .{
                        .float_literal = value,
                    },
                };
            },
            .string => try self.parseStringLiteral(),
            .bang, .minus => blk: {
                const start_token = self.current_token;
                self.next();
                break :blk .{
                    .token = start_token,
                    .type = .{
                        .unary = .{
                            .operator = ast.UnaryOp.fromToken(start_token),
                            .value = try self.allocate(try self.parseExpression(.prefix)),
                        },
                    },
                };
            },
            .true, .false => .{
                .token = self.current_token,
                .type = .{
                    .boolean_literal = self.currentIs(.true),
                },
            },
            .@"if" => try self.parseIfExpression(),
            // group
            .left_parenthesis => blk: {
                self.next();
                const exp = try self.parseExpression(.lowest);
                try self.expectPeek(.right_parenthesis);
                break :blk exp;
            },
            // map or set
            .left_brace => try self.parseMapSetLiteral(),
            // list
            .left_bracket => blk: {
                const start_token = self.current_token;
                self.next(); // skip bracket
                var list = std.ArrayList(Expression).init(self.allocator);
                errdefer list.deinit();
                if (self.currentIs(.right_bracket)) break :blk .{
                    .token = start_token,
                    .type = .{
                        .list_literal = try list.toOwnedSlice(),
                    },
                };

                try list.append(try self.parseExpression(.lowest));
                while (self.peekIs(.comma)) {
                    self.next(); // skip comma
                    self.next();
                    try list.append(try self.parseExpression(.lowest));
                }
                try self.expectPeek(.right_bracket);
                break :blk .{
                    .token = start_token,
                    .type = .{
                        .list_literal = try list.toOwnedSlice(),
                    },
                };
            },
            .nil => .{ .token = self.current_token, .type = .nil },
            else => return self.fail("Unexpected token in expression: {}", self.current_token, .{self.current_token.token_type}),
        };

        while (prec.val() < findPrecedence(self.peek_token.token_type).val()) {
            left = switch (self.peek_token.token_type) {
                .left_parenthesis => blk: {
                    self.next();
                    break :blk try self.parseCallExpression(left);
                },
                .left_bracket, .period => blk: {
                    self.next();
                    break :blk try self.parseIndexExpression(left);
                },
                .assign => blk: {
                    self.next();
                    break :blk try self.parseAssignment(left);
                },
                .double_period => blk: {
                    self.next();
                    break :blk try self.parseRange(left);
                },
                .plus,
                .minus,
                .slash,
                .asterisk,
                .percent,
                .equal,
                .not_equal,
                .less_than,
                .greater_than,
                .less_than_equal,
                .greater_than_equal,
                .@"and",
                .@"or",
                => blk: {
                    self.next();
                    const start_token = self.current_token;
                    const op = ast.BinaryOp.fromToken(self.current_token);
                    const inner_prec = findPrecedence(self.current_token.token_type);
                    self.next();
                    break :blk .{
                        .token = start_token,
                        .type = .{
                            .binary = .{
                                .operator = op,
                                .left = try self.allocate(left),
                                .right = try self.allocate(try self.parseExpression(inner_prec)),
                            },
                        },
                    };
                },
                else => return left,
            };
        }

        return left;
    }

    fn parseStringLiteral(self: *Parser) Error!Expression {
        const token = self.current_token;
        return .{
            .token = token,
            .type = .{
                .string_literal = try self.allocator.dupe(u8, self.source[token.start..token.end]),
            },
        };
    }

    fn parseMapPairSetKey(self: *Parser) Error!Expression {
        const start_token = self.current_token;
        const key = try self.parseExpression(.lowest);
        // set value
        if (!self.peekIs(.colon)) return key;
        self.next();
        self.next();

        const value = try self.parseExpression(.lowest);
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

    fn parseMapSetLiteral(self: *Parser) Error!Expression {
        const start_token = self.current_token;
        var list = std.ArrayList(Expression).init(self.allocator);
        errdefer list.deinit();
        self.next(); // skip brace

        // empty set
        if (self.currentIs(.right_brace)) {
            self.next();
            return .{ .token = start_token, .type = .{ .set_literal = try list.toOwnedSlice() } };
        }
        // empty map
        if (self.currentIs(.colon)) {
            try self.expectPeek(.right_brace);
            return .{ .token = start_token, .type = .{ .map_literal = try list.toOwnedSlice() } };
        }

        const first = try self.parseMapPairSetKey();
        const is_map = first.type == .map_pair;
        try list.append(first);
        while (self.peekIs(.comma)) {
            self.next(); // skip comma
            self.next();
            const item = try self.parseMapPairSetKey();
            if ((is_map and item.type != .map_literal) or (!is_map and item.type == .map_literal)) return self.fail("Items are not of same type", item.token, .{});
            try list.append(item);
        }
        try self.expectPeek(.right_brace);
        if (is_map) return .{ .token = start_token, .type = .{ .map_literal = try list.toOwnedSlice() } };
        return .{ .token = start_token, .type = .{ .set_literal = try list.toOwnedSlice() } };
    }

    fn parseIfExpression(self: *Parser) Error!Expression {
        try self.expectPeek(.left_parenthesis);
        self.next(); // skip if
        const condition = try self.allocate(try self.parseExpression(.lowest));
        try self.expectPeek(.right_parenthesis);
        self.next();

        const true_value = try self.allocate(try self.parseExpression(.lowest));
        try self.expectPeek(.@"else");
        self.next();
        if (self.peekIs(.@"if")) {
            self.next();
            const false_value = try self.allocate(try self.parseExpression(.lowest));
            return .{
                .token = self.current_token,
                .type = .{
                    .if_expression = .{
                        .condition = condition,
                        .true_value = true_value,
                        .false_value = false_value,
                    },
                },
            };
        }

        const false_value = try self.allocate(try self.parseExpression(.lowest));
        return .{
            .token = self.current_token,
            .type = .{
                .if_expression = .{
                    .condition = condition,
                    .true_value = true_value,
                    .false_value = false_value,
                },
            },
        };
    }

    fn parseIfStatement(self: *Parser) Error!Statement {
        try self.expectPeek(.left_parenthesis);
        self.next(); // skip if
        const condition = try self.allocate(try self.parseExpression(.lowest));
        try self.expectPeek(.right_parenthesis);
        try self.expectPeek(.left_brace);

        const true_body = try self.allocate(try self.parseBlockStatement());

        if (self.peekIs(.@"else")) {
            self.next();
            if (self.peekIs(.@"if")) {
                self.next();
                const false_body = try self.allocate(try self.parseBlockStatement());
                return .{
                    .token = self.current_token,
                    .type = .{
                        .if_statement = .{
                            .condition = condition,
                            .true_body = true_body,
                            .false_body = false_body,
                        },
                    },
                };
            }

            try self.expectPeek(.left_brace);
            const false_body = try self.allocate(try self.parseBlockStatement());
            return .{
                .token = self.current_token,
                .type = .{
                    .if_statement = .{
                        .condition = condition,
                        .true_body = true_body,
                        .false_body = false_body,
                    },
                },
            };
        }

        return .{
            .token = self.current_token,
            .type = .{
                .if_statement = .{
                    .condition = condition,
                    .true_body = true_body,
                    .false_body = null,
                },
            },
        };
    }

    // const x: bool = true => 'bool'
    fn parseTypeDefValue(self: *Parser) Error!*Statement.TypeDefValue {
        return try self.allocate(switch (self.current_token.token_type) {
            .enum_keyword => .enum_type,
            .bool_keyword => .boolean_type,
            .integer_keyword => .integer_type,
            .float_keyword => .float_type,
            .string_keyword => .string_type,
            .void_keyword => .void_type,
            .function_keyword => blk: {
                self.next(); // skip func keyword
                const params = try self.parseFunctionParameters();
                self.next(); // skip right parenthesis
                break :blk Statement.TypeDefValue{
                    .function_type = .{
                        .params = params,
                        .return_type = try self.parseTypeDefValue(),
                    },
                };
            },
            .left_bracket => blk: {
                try self.expectPeek(.right_bracket);
                self.next();
                break :blk Statement.TypeDefValue{
                    .list_type = try self.parseTypeDefValue(),
                };
            },
            .left_brace => blk: {
                try self.expectPeek(.right_brace);
                self.next();
                const key_type = try self.parseTypeDefValue();
                if (self.peekIs(.comma)) {
                    self.next();
                    self.next();
                    const value_type = try self.parseTypeDefValue();
                    break :blk Statement.TypeDefValue{
                        .map_type = .{
                            .key_type = key_type,
                            .value_type = value_type,
                        },
                    };
                }
                break :blk Statement.TypeDefValue{ .set_type = key_type };
            },
            .identifier => Statement.TypeDefValue{
                .identifier_type = try self.allocator.dupe(u8, self.source[self.current_token.start..self.current_token.end]),
            },
            else => return self.fail("Unexpected token as type", self.current_token, .{}),
        });
    }

    fn parseSplit(self: *Parser) Error!Statement {
        self.depth += 1;
        defer self.depth -= 1;
        const start_token = self.current_token;

        // -1 to account for backtick blocks
        const column: usize = self.current_token.column + 1;
        self.next(); // skip split
        var content = try self.parseUntilEndOfLine();
        self.next(); // skip eol
        var list = ArrayList(Statement).init(self.allocator);
        errdefer list.deinit();

        while (!self.peekIs(.eof) and self.peek_token.column > column) {
            var line = try self.parseStatement();
            try list.append(line);
            if (self.peek_token.column <= column) break;
            self.next();
        }

        return .{
            .token = start_token,
            .type = .{
                .split = .{
                    .content = content,
                    .body = try list.toOwnedSlice(),
                },
            },
        };
    }

    fn parseJump(self: *Parser) Error!Statement {
        const start_token = self.current_token;
        try self.expectPeek(.identifier);
        const destination = try self.parseExpression(.lowest);

        if (!self.peekIs(.eol) and !self.peekIs(.eof)) {
            return self.fail("Jumps must be on their own line", self.peek_token, .{});
        }
        self.next(); // skip eol
        return .{
            .token = start_token,
            .type = .{
                .jump = destination,
            },
        };
    }

    fn parseUntilEndOfLine(self: *Parser) Error![]const Statement {
        var list = ArrayList(Statement).init(self.allocator);
        errdefer list.deinit();
        while (!self.currentIsOneOf(&[_]TokenType{ .eol, .eof })) {
            const item = try self.parseStatement();
            try list.append(item);
            self.next();
        }
        return list.toOwnedSlice();
    }

    fn parseDialogueLine(self: *Parser) Error!Statement {
        const start_token = self.current_token;
        var speaker: ?Expression = null;
        if (!self.peekIs(.colon)) {
            try self.expectPeek(.identifier);
            speaker = try self.parseIdentifier();
        }
        try self.expectPeek(.colon);
        self.next();
        return .{
            .token = start_token,
            .type = .{
                .line = .{
                    .speaker = speaker,
                    .content = try self.parseUntilEndOfLine(),
                },
            },
        };
    }

    fn parseContentExpression(self: *Parser) Error!Expression {
        const start_token = self.current_token;
        var end: usize = start_token.end;
        while (!self.peekIsOneOf(&[_]TokenType{ .eol, .eof, .backtick }) and self.mode == .content) {
            self.next();
            end = self.peek_token.start;
        }
        var content = try self.allocator.dupe(u8, self.source[start_token.start..end]);

        // hacky
        if (content.len > 0 and content[content.len - 1] == '`') content = content[0 .. content.len - 1];
        return .{
            .token = start_token,
            .type = .{
                .content = content,
            },
        };
    }

    fn parseTag(self: *Parser) Error!Expression {
        const start_token = self.current_token;
        var end: usize = start_token.end;
        while (!self.peekIsOneOf(&[_]TokenType{ .eol, .eof, .hash })) {
            self.next();
            end = self.current_token.end;
        }
        const tag = try self.allocator.dupe(u8, self.source[start_token.start..end]);
        return .{
            .token = start_token,
            .type = .{ .tag = tag },
        };
    }

    fn parseBlockStatement(self: *Parser) Error!Statement {
        self.depth += 1;
        defer self.depth -= 1;
        const start_token = self.current_token;
        const closing_token = switch (start_token.token_type) {
            .left_brace => .right_brace,
            .split => .gather,
            else => start_token.token_type,
        };

        if (self.currentIs(.bough)) {
            self.mode = .content;
        } else if (self.currentIs(.blackboard)) {
            self.mode = .code;
        }

        self.next(); //skip opening token
        const name = switch (closing_token) {
            .bough, .twig => blk: {
                try self.expectCurrent(.identifier);
                const name = try self.allocator.dupe(u8, self.source[self.current_token.start..self.current_token.end]);
                // omit any additional closing token on this line
                if (self.peekIs(closing_token)) self.next();
                try self.expectPeek(.eol);
                break :blk name;
            },
            else => null,
        };

        var list = ArrayList(Statement).init(self.allocator);
        errdefer list.deinit();
        while (!self.currentIs(closing_token) and !self.currentIs(.eof)) {
            // properly close off content blocks
            if (self.currentIs(.eol) and self.peekIs(closing_token)) {
                self.next();
                break;
            }
            const exp = try self.parseStatement();
            try list.append(exp);
            self.next();
        }

        if (closing_token == .bough or closing_token == .blackboard) {
            self.mode = .root;
        }

        return .{
            .token = start_token,
            .type = .{
                .block = .{
                    .name = name,
                    .body = try list.toOwnedSlice(),
                },
            },
        };
    }

    fn parseFunctionLiteral(self: *Parser) Error!Expression {
        return .{
            .token = self.current_token,
            .type = .{
                .function_literal = try self.allocate(try self.parseBlockStatement()),
            },
        };
    }

    fn parseFunctionParameters(self: *Parser) Error![]const Statement {
        var list = ArrayList(Statement).init(self.allocator);
        errdefer list.deinit();
        self.next(); // skip left_parenthesis
        if (self.currentIs(.right_parenthesis)) {
            return list.toOwnedSlice();
        }

        try list.append(try self.parseParameter());
        while (self.peekIs(.comma)) {
            self.next();
            self.next();
            try list.append(try self.parseParameter());
        }

        try self.expectPeek(.right_parenthesis);
        return list.toOwnedSlice();
    }

    fn parseParameter(self: *Parser) Error!Statement {
        const start_token = self.current_token;
        const name = try self.allocator.dupe(u8, self.source[self.current_token.start..self.current_token.end]);

        try self.expectPeek(.colon);
        self.next();
        const type_def = try self.parseTypeDefValue();
        return .{
            .token = start_token,
            .type = .{
                .function_parameter = .{
                    .name = name,
                    .type_def = type_def,
                },
            },
        };
    }

    fn parseArguments(self: *Parser, comptime end_type: TokenType) Error![]Expression {
        var list = ArrayList(Expression).init(self.allocator);
        errdefer list.deinit();

        // no arguments
        if (self.peekIs(end_type)) {
            self.next();
            return list.toOwnedSlice();
        }

        self.next();
        try list.append(try self.parseExpression(.lowest));

        while (self.peekIs(.comma)) {
            self.next();
            self.next();
            try list.append(try self.parseExpression(.lowest));
        }
        try self.expectPeek(end_type);
        return list.toOwnedSlice();
    }

    fn parseCallExpression(self: *Parser, func: Expression) Error!Expression {
        return .{
            .token = self.current_token,
            .type = .{
                .call = .{
                    .name = try self.allocate(func),
                    .arguments = try self.parseArguments(.right_parenthesis),
                },
            },
        };
    }

    fn parseEnumLiteral(self: *Parser) Error!Expression {
        const start_token = self.current_token;
        var enums = std.ArrayList(Expression).init(self.allocator);
        errdefer enums.deinit();
        self.next(); // skip brace
        try enums.append(try self.parseIdentifier());
        while (self.peekIs(.comma)) {
            self.next(); // skip identifier
            if (self.peekIs(.right_brace)) break;
            self.next(); // skip comma
            try enums.append(try self.parseIdentifier());
        }

        try self.expectPeek(.right_brace);
        return .{ .token = start_token, .type = .{ .enum_literal = try enums.toOwnedSlice() } };
    }

    fn parseIndexExpression(self: *Parser, value: Expression) Error!Expression {
        const start_token = self.current_token;
        self.next();
        const index_node = if (start_token.token_type == .period) blk: {
            if (!self.currentIs(.identifier))
                return self.fail("Expected identifier after index '.', found {}", self.current_token, .{self.current_token.token_type});
            break :blk try self.parseStringLiteral();
        } else if (!self.currentIs(.colon))
            try self.parseExpression(.lowest)
        else
            unreachable;

        if (start_token.token_type != .period) {
            try self.expectPeek(.right_bracket);
        }

        return .{
            .token = start_token,
            .type = .{
                .indexer = .{
                    .index = try self.allocate(index_node),
                    .value = try self.allocate(value),
                },
            },
        };
    }

    fn parseWhile(self: *Parser) Error!Statement {
        const start_token = self.current_token;
        try self.expectPeek(.left_parenthesis);
        self.next();
        const condition = try self.parseExpression(.lowest);
        try self.expectPeek(.right_parenthesis);
        try self.expectPeek(.left_brace);

        const body = try self.parseBlockStatement();
        return .{
            .token = start_token,
            .type = .{
                .while_loop = .{
                    .condition = condition,
                    .body = try self.allocate(body),
                },
            },
        };
    }

    fn parseFor(self: *Parser) Error!Statement {
        const start_token = self.current_token;
        try self.expectPeek(.left_parenthesis);
        self.next();
        const iterator = try self.parseExpression(.lowest);
        switch (iterator.type) {
            .range, .identifier => {},
            else => return self.fail("Expected list, set, map, or range in for loop, found {}", iterator.token, .{iterator.token.token_type}),
        }

        try self.expectPeek(.right_parenthesis);
        try self.expectPeek(.colon);
        self.next();
        const capture = try self.parseIdentifier();
        try self.expectPeek(.left_brace);
        const body = try self.parseBlockStatement();
        return .{
            .token = start_token,
            .type = .{
                .for_loop = .{
                    .capture = capture,
                    .iterator = iterator,
                    .body = try self.allocate(body),
                },
            },
        };
    }

    fn parseRange(self: *Parser, left: Expression) Error!Expression {
        self.next();
        return .{
            .token = self.current_token,
            .type = .{
                .range = .{
                    .left = try self.allocate(left),
                    .right = try self.allocate(try self.parseExpression(.lowest)),
                },
            },
        };
    }
    //
    // fn parseSwitchStatement(self: *Parser) Error!Node {
    //     const node = try self.allocator.create(Node.SwitchLiteral);
    //     node.* = .{ .token = self.current_token, .capture = undefined, .prongs = undefined };
    //
    //     try self.expectPeek(.left_parenthesis);
    //
    //     self.next();
    //     node.capture = try self.parseExpression(.lowest);
    //
    //     try self.expectPeek(.right_parenthesis);
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
    //     return Node{ .switch_statement = node };
    // }
    //
    // /// Parses a switch prong i.e. x: 5 + 5 into a `Node.SwitchProng`
    // fn parseSwitchProng(self: *Parser) Error!Node {
    //     const node = try self.allocator.create(Node.SwitchProng);
    //     node.* = .{ .token = undefined, .left = try self.parseExpression(.lowest), .right = undefined };
    //
    //     try self.expectPeek(.colon);
    //     node.token = self.current_token;
    //     self.next();
    //
    //     node.right = if (self.currentIs(.left_brace))
    //         try self.parseBlockStatement(.right_brace)
    //     else
    //         try self.parseExpressionStatement();
    //
    //     return Node{ .switch_prong = node };
    // }
    //
    fn parseComment(self: *Parser) Error!Statement {
        return .{
            .token = self.current_token,
            .type = .{
                .comment = self.source[self.current_token.start..self.current_token.end],
            },
        };
    }

    fn parseBreak(self: *Parser) Error!Statement {
        return .{
            .token = self.current_token,
            .type = .@"break",
        };
    }

    fn parseContinue(self: *Parser) Error!Statement {
        return .{
            .token = self.current_token,
            .type = .@"continue",
        };
    }

    fn expectCurrent(self: *Parser, comptime token_type: TokenType) !void {
        if (self.currentIs(token_type)) return;
        return self.fail(
            "Expected current token to be '" ++ tok.fmtString(token_type) ++ "', found {}",
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
            "Expected next token to be '" ++ tok.fmtString(token_type) ++ "', found {}",
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

test "Parse Import" {
    const allocator = testing.allocator;
    const input = "import \"./globals.topi\"";
    var errors = Errors.init(allocator);
    defer errors.deinit();
    const tree = parse(allocator, input, &errors) catch |err| {
        try errors.write(input, std.io.getStdErr().writer());
        return err;
    };
    defer tree.deinit();
    try testing.expectEqualStrings("./globals.topi", tree.root[0].type.import);
}

test "Parse Declaration" {
    var allocator = testing.allocator;
    const test_cases = .{
        .{ .input = "```\n const x: int = 5 ```", .id = "x", .value = 5, .mutable = false, .type = .integer },
        .{ .input = "```\n var y: int = 50 ```", .id = "y", .value = 50, .mutable = true, .type = .integer },
        .{ .input = "```\n var x: float = 2.0 ```", .id = "x", .value = 2.0, .mutable = true, .type = .float },
        .{ .input = "```\n var b: bool = true ```", .id = "b", .value = true, .mutable = true, .type = .bool },
        .{ .input = "```\n var s: string = \"STRING\" ```", .id = "s", .value = "STRING", .mutable = true, .type = .string },
    };

    inline for (test_cases) |case| {
        var errors = Errors.init(allocator);
        defer errors.deinit();
        const tree = parse(allocator, case.input, &errors) catch |err| {
            try errors.write(case.input, std.io.getStdErr().writer());
            return err;
        };
        defer tree.deinit();
        const node = tree.root[0].type.block.body[0];
        const decl = node.type.declaration;
        try testing.expectEqualStrings(case.id, decl.name);

        switch (case.type) {
            .integer => try testing.expect(case.value == decl.value.type.integer_literal),
            .float => try testing.expect(case.value == decl.value.type.float_literal),
            .bool => try testing.expect(case.value == decl.value.type.boolean_literal),
            .string => try testing.expectEqualStrings(case.value, decl.value.type.string_literal),
            else => unreachable,
        }
        try testing.expect(case.mutable == decl.is_mutable);
    }
}

test "Parse Iteratable Types" {
    var allocator = testing.allocator;
    const test_cases = .{
        .{ .input = "```\n const stringList: []string = [\"item\"] ```", .id = "stringList", .item_value = "item", .mutable = false, .type = .list, .item_type = .string },
        .{ .input = "```\n var intList: []int = [50] ```", .id = "intList", .item_value = 50, .mutable = true, .type = .list, .item_type = .integer },
        .{ .input = "```\n var floatSet: {}float = {2.0} ```", .id = "floatSet", .item_value = 2.0, .mutable = true, .type = .set, .item_type = .float },
        .{ .input = "```\n var stringBoolMap: {}string,bool = {\"key\":true} ```", .id = "stringBoolMap", .item_value = true, .mutable = true, .type = .map, .item_type = .bool },
    };

    inline for (test_cases) |case| {
        var errors = Errors.init(allocator);
        defer errors.deinit();
        const tree = parse(allocator, case.input, &errors) catch |err| {
            try errors.write(case.input, std.io.getStdErr().writer());
            return err;
        };
        defer tree.deinit();
        const node = tree.root[0].type.block.body[0];
        const decl = node.type.declaration;
        try testing.expectEqualStrings(case.id, decl.name);

        switch (case.type) {
            .list => switch (case.item_type) {
                .integer => try testing.expect(case.item_value == decl.value.type.list_literal[0].type.integer_literal),
                .string => try testing.expectEqualStrings(case.item_value, decl.value.type.list_literal[0].type.string_literal),
                else => unreachable,
            },
            .set => try testing.expect(case.item_value == decl.value.type.set_literal[0].type.float_literal),
            .map => try testing.expect(case.item_value == decl.value.type.map_literal[0].type.map_pair.value.type.boolean_literal),
            else => unreachable,
        }
        try testing.expect(case.mutable == decl.is_mutable);
    }
}

test "Parse Empty Iterable Types" {
    const allocator = testing.allocator;
    const input =
        \\ ```
        \\ const emptyMap: {}int,int = {:}
        \\ const emptySet: {}int = {}
        \\ ```
    ;
    var errors = Errors.init(allocator);
    defer errors.deinit();
    const tree = parse(allocator, input, &errors) catch |err| {
        try errors.write(input, std.io.getStdErr().writer());
        return err;
    };
    defer tree.deinit();
    var decl = tree.root[0].type.block.body[0].type.declaration;
    try testing.expectEqualStrings("emptyMap", decl.name);
    try testing.expect(decl.type_def.map_type.key_type.* == .integer_type);
    try testing.expect(decl.type_def.map_type.value_type.* == .integer_type);
    try testing.expect(decl.value.type.map_literal.len == 0);

    decl = tree.root[0].type.block.body[1].type.declaration;
    try testing.expectEqualStrings("emptySet", decl.name);
    try testing.expect(decl.type_def.set_type.* == .integer_type);
    try testing.expect(decl.value.type.set_literal.len == 0);
}
test "Parse Extern" {
    var allocator = testing.allocator;
    const test_cases = .{
        .{ .input = "```\n const x: int = extern ```", .id = "x", .mutable = false, .type = .integer },
        .{ .input = "```\n var y: int = extern ```", .id = "y", .mutable = true, .type = .integer },
        .{ .input = "```\n var x: float = extern ```", .id = "x", .mutable = true, .type = .float },
        .{ .input = "```\n var b: bool = extern ```", .id = "b", .mutable = true, .type = .bool },
        .{ .input = "```\n var s: string = extern ```", .id = "s", .mutable = true, .type = .string },
    };

    inline for (test_cases) |case| {
        var errors = Errors.init(allocator);
        defer errors.deinit();
        const tree = parse(allocator, case.input, &errors) catch |err| {
            try errors.write(case.input, std.io.getStdErr().writer());
            return err;
        };
        defer tree.deinit();
        const node = tree.root[0].type.block.body[0];
        const decl = node.type.declaration;
        try testing.expectEqualStrings(case.id, decl.name);
        try testing.expect(decl.value.type == Expression.Type.@"extern");
        try testing.expect(case.mutable == decl.is_mutable);
    }
}

test "Parse Enum" {
    const allocator = testing.allocator;
    const input =
        \\ ```
        \\ const e: enum = {
        \\     one,
        \\     two,
        \\ }
        \\
        \\ const en: enum = {
        \\     one,
        \\     two
        \\ }
        \\ const val: en = one
        \\ ```
    ;
    var errors = Errors.init(allocator);
    defer errors.deinit();
    const tree = parse(allocator, input, &errors) catch |err| {
        try errors.write(input, std.io.getStdErr().writer());
        return err;
    };
    defer tree.deinit();
    var node = tree.root[0].type.block.body[0];
    var decl = node.type.declaration;
    try testing.expectEqualStrings("e", decl.name);
    var enum_literal = decl.value.type.enum_literal;
    try testing.expectEqualStrings(enum_literal[0].type.identifier, "one");
    try testing.expectEqualStrings(enum_literal[1].type.identifier, "two");

    node = tree.root[0].type.block.body[1];
    decl = node.type.declaration;
    try testing.expectEqualStrings("en", decl.name);
    enum_literal = decl.value.type.enum_literal;
    try testing.expectEqualStrings(enum_literal[0].type.identifier, "one");
    try testing.expectEqualStrings(enum_literal[1].type.identifier, "two");

    node = tree.root[0].type.block.body[2];
    decl = node.type.declaration;
    try testing.expectEqualStrings("val", decl.name);
    try testing.expectEqualStrings("en", decl.type_def.identifier_type);
    var ident = decl.value.type.identifier;
    try testing.expectEqualStrings(ident, "one");
}

test "Parse Function" {
    const input =
        \\ ```
        \\ const someFunc: func() void = {}
        \\ const sum: func(x: int, y: int) int = {
        \\     return x + 5
        \\ }
        \\ const someExtern: func(str: string) string = extern
        \\ ```
    ;
    const allocator = testing.allocator;
    var errors = Errors.init(allocator);
    defer errors.deinit();
    const tree = parse(allocator, input, &errors) catch |err| {
        try errors.write(input, std.io.getStdErr().writer());
        return err;
    };
    defer tree.deinit();

    const noop = tree.root[0].type.block.body[0].type.declaration.type_def.function_type;
    try testing.expect(noop.params.len == 0);
    try testing.expect(noop.return_type.* == .void_type);

    const sumDecl = tree.root[0].type.block.body[1].type.declaration;
    const sum = sumDecl.type_def.function_type;
    try testing.expect(sum.params.len == 2);
    try testing.expectEqualStrings(sum.params[0].type.function_parameter.name, "x");
    try testing.expect(sum.params[0].type.function_parameter.type_def.* == .integer_type);
    try testing.expectEqualStrings(sum.params[1].type.function_parameter.name, "y");
    try testing.expect(sum.params[1].type.function_parameter.type_def.* == .integer_type);
    try testing.expect(sum.return_type.* == .integer_type);

    const body = sumDecl.value.type.function_literal.type.block.body[0];
    const binary_exp = body.type.return_expression.type.binary;
    try testing.expectEqual(binary_exp.operator, .add);
    try testing.expectEqualStrings(binary_exp.left.type.identifier, "x");
    try testing.expect(binary_exp.right.type.integer_literal == 5);

    const someExternDecl = tree.root[0].type.block.body[2].type.declaration;
    const someExtern = someExternDecl.type_def.function_type;
    try testing.expect(someExtern.params.len == 1);
    try testing.expectEqualStrings(someExtern.params[0].type.function_parameter.name, "str");
    try testing.expect(someExtern.params[0].type.function_parameter.type_def.* == .string_type);
    try testing.expect(someExtern.return_type.* == .string_type);
    try testing.expect(someExternDecl.value.type == .@"extern");
}

test "Parse If" {
    const allocator = testing.allocator;
    const input =
        \\```
        \\ if (x < y) { return x }
        \\ const a: string = if (true) "true" else "false"
        \\ ```
    ;
    var errors = Errors.init(allocator);
    defer errors.deinit();
    const tree = parse(allocator, input, &errors) catch |err| {
        try errors.write(input, std.io.getStdErr().writer());
        return err;
    };
    defer tree.deinit();

    const if_state = tree.root[0].type.block.body[0].type.if_statement;
    const ret_exp = if_state.true_body.type.block.body[0].type;
    try testing.expect(ret_exp == .return_expression);
    try testing.expectEqualStrings(ret_exp.return_expression.type.identifier, "x");
    try tree.print(std.io.getStdErr().writer());

    const decl = tree.root[0].type.block.body[1].type.declaration;
    try testing.expectEqualStrings("true", decl.value.type.if_expression.true_value.type.string_literal);
    try testing.expectEqualStrings("false", decl.value.type.if_expression.false_value.type.string_literal);
}

test "Parse Call expression" {
    const input =
        \\ ```
        \\ add(1, 2 * 3, 4 + 5)
        \\ ```
    ;
    const allocator = testing.allocator;
    var errors = Errors.init(allocator);
    defer errors.deinit();
    const tree = parse(allocator, input, &errors) catch |err| {
        try errors.write(input, std.io.getStdErr().writer());
        return err;
    };
    defer tree.deinit();

    const call = tree.root[0].type.block.body[0].type.expression.type.call;
    try testing.expectEqualStrings(call.name.type.identifier, "add");
    try testing.expect(call.arguments.len == 3);
    try testing.expect(call.arguments[0].type.integer_literal == 1);
    try testing.expectEqual(call.arguments[1].type.binary.operator, .multiply);
    try testing.expect(call.arguments[2].type.binary.right.type.integer_literal == 5);
}

test "Parse Unary Expression" {
    const input =
        \\ ```
        \\ !true
        \\ ```
        \\
        \\ ```
        \\ -y
        \\ ```
    ;
    const allocator = testing.allocator;
    var errors = Errors.init(allocator);
    defer errors.deinit();
    const tree = parse(allocator, input, &errors) catch |err| {
        try errors.write(input, std.io.getStdErr().writer());
        return err;
    };
    defer tree.deinit();
    var unary = tree.root[0].type.block.body[0].type.expression.type.unary;
    try testing.expect(unary.operator == .not);
    try testing.expect(unary.value.type.boolean_literal == true);

    unary = tree.root[1].type.block.body[0].type.expression.type.unary;
    try testing.expect(unary.operator == .minus);
    try testing.expectEqualStrings(unary.value.type.identifier, "y");
}

test "Parse For loop" {
    const input =
        \\ ```
        \\ for (list): item {
        \\ }
        \\ for (0..10): i {
        \\ }
        \\ for (map): key {
        \\     const value: int = map[key]
        \\ }
        \\ ```
    ;
    const allocator = testing.allocator;

    var errors = Errors.init(allocator);
    defer errors.deinit();
    const tree = parse(allocator, input, &errors) catch |err| {
        try errors.write(input, std.io.getStdErr().writer());
        return err;
    };
    defer tree.deinit();

    var loop = tree.root[0].type.block.body[0].type.for_loop;
    try testing.expectEqualStrings("list", loop.iterator.type.identifier);
    try testing.expectEqualStrings("item", loop.capture.type.identifier);

    loop = tree.root[0].type.block.body[1].type.for_loop;
    try testing.expect(loop.iterator.type.range.left.type.integer_literal == 0);
    try testing.expect(loop.iterator.type.range.right.type.integer_literal == 10);
    try testing.expectEqualStrings("i", loop.capture.type.identifier);

    loop = tree.root[0].type.block.body[2].type.for_loop;
    try testing.expectEqualStrings("map", loop.iterator.type.identifier);
    try testing.expectEqualStrings("key", loop.capture.type.identifier);
    try testing.expect(loop.body.type.block.body[0].type == .declaration);
}

test "Parse While loop" {
    const input =
        \\ ```
        \\ while (x < y) { x }"
        \\ ```
    ;
    const allocator = testing.allocator;

    var errors = Errors.init(allocator);
    defer errors.deinit();
    const tree = parse(allocator, input, &errors) catch |err| {
        try errors.write(input, std.io.getStdErr().writer());
        return err;
    };
    defer tree.deinit();
    var loop = tree.root[0].type.block.body[0].type.while_loop;
    try testing.expect(loop.condition.type.binary.operator == .less_than);
    try testing.expectEqualStrings("x", loop.body.type.block.body[0].type.expression.type.identifier);
}

test "Parse Limb > Branch > Line" {
    const allocator = testing.allocator;
    const input =
        \\ === BOUGH ===
        \\     --- TWIG ---
        \\     :Speaker: Text goes here
        \\     ---
        \\ ===
    ;
    var errors = Errors.init(allocator);
    defer errors.deinit();
    const tree = parse(allocator, input, &errors) catch |err| {
        try errors.write(input, std.io.getStdErr().writer());
        return err;
    };
    defer tree.deinit();
    const bough = tree.root[0];
    try testing.expect(bough.token.token_type == .bough);
    try testing.expectEqualStrings(bough.type.block.name.?, "BOUGH");

    const twig = bough.type.block.body[0];
    try testing.expect(twig.token.token_type == .twig);
    try testing.expectEqualStrings(twig.type.block.name.?, "TWIG");

    const line = twig.type.block.body[0].type.line;
    try testing.expectEqualStrings(line.speaker.?.type.identifier, "Speaker");
    try testing.expectEqualStrings(line.content[0].type.expression.type.content, "Text goes here");
}

test "Parse No Speaker" {
    const allocator = testing.allocator;
    const input =
        \\  === BOUGH ===
        \\      :: Text goes here
        \\  ===
    ;
    var errors = Errors.init(allocator);
    defer errors.deinit();
    const tree = parse(allocator, input, &errors) catch |err| {
        try errors.write(input, std.io.getStdErr().writer());
        return err;
    };
    defer tree.deinit();
    const line = tree.root[0].type.block.body[0].type.line;
    try testing.expect(line.speaker == null);
    try testing.expectEqualStrings(line.content[0].type.expression.type.content, "Text goes here");
}

test "Parse Multiple Limbs" {
    const allocator = testing.allocator;
    const input =
        \\  === BOUGH ===
        \\      :: Text goes here
        \\  ===
        \\  === SECOND ===
        \\      :: And here
        \\  ===
    ;
    var errors = Errors.init(allocator);
    defer errors.deinit();
    const tree = parse(allocator, input, &errors) catch |err| {
        try errors.write(input, std.io.getStdErr().writer());
        return err;
    };
    defer tree.deinit();

    var bough = tree.root[0].type.block;
    var line = bough.body[0].type.line;
    try testing.expectEqualStrings("BOUGH", bough.name.?);
    try testing.expectEqualStrings(line.content[0].type.expression.type.content, "Text goes here");

    bough = tree.root[1].type.block;
    line = bough.body[0].type.line;
    try testing.expectEqualStrings("SECOND", bough.name.?);
    try testing.expectEqualStrings(line.content[0].type.expression.type.content, "And here");
}

test "Parse Jump" {
    const allocator = testing.allocator;
    const input =
        \\  === BOUGH ===
        \\      => JUMP_ID
        \\  ===
    ;
    var errors = Errors.init(allocator);
    defer errors.deinit();
    const tree = parse(allocator, input, &errors) catch |err| {
        try errors.write(input, std.io.getStdErr().writer());
        return err;
    };
    defer tree.deinit();
    const jump = tree.root[0].type.block.body[0].type.jump;
    try testing.expectEqualStrings(jump.type.identifier, "JUMP_ID");
}

test "Parse Splits" {
    const allocator = testing.allocator;
    const input =
        \\  === BOUGH ===
        \\      -<  choice 1
        \\          :Other: Response
        \\      -<  choice 2
        \\          :Other: Second Response
        \\  ===
    ;
    var errors = Errors.init(allocator);
    defer errors.deinit();
    const tree = parse(allocator, input, &errors) catch |err| {
        try errors.write(input, std.io.getStdErr().writer());
        return err;
    };
    defer tree.deinit();
    // try tree.print(std.io.getStdErr().writer());

    const body = tree.root[0].type.block.body;
    var split = body[0].type.split;
    try testing.expectEqualStrings(split.content[0].type.expression.type.content, "choice 1");

    var line = split.body[0].type.line;
    try testing.expectEqualStrings(line.speaker.?.type.identifier, "Other");
    try testing.expectEqualStrings(line.content[0].type.expression.type.content, "Response");

    split = body[1].type.split;
    try testing.expectEqualStrings(split.content[0].type.expression.type.content, "choice 2");
    line = split.body[0].type.line;
    try testing.expectEqualStrings(line.speaker.?.type.identifier, "Other");
    try testing.expectEqualStrings(line.content[0].type.expression.type.content, "Second Response");
}

test "Parse Nested Splits" {
    const allocator = testing.allocator;
    const input =
        \\  === BOUGH ===
        \\      -<  choice 1
        \\          :Other: Response
        \\          -<  inner choice 1
        \\              :Other: Inner Response
        \\          -<  inner choice 2
        \\              :Other: Second Inner Response
        \\          :Other: Gather back here
        \\      -<  choice 2
        \\          :Other: Second Response
        \\  ===
    ;
    var errors = Errors.init(allocator);
    defer errors.deinit();
    const tree = parse(allocator, input, &errors) catch |err| {
        try errors.write(input, std.io.getStdErr().writer());
        return err;
    };
    defer tree.deinit();
    // try tree.print(std.io.getStdErr().writer());

    const body = tree.root[0].type.block.body;
    var split = body[0].type.split;
    try testing.expectEqualStrings(split.content[0].type.expression.type.content, "choice 1");
    var line = split.body[0].type.line;
    try testing.expectEqualStrings(line.speaker.?.type.identifier, "Other");
    try testing.expectEqualStrings(line.content[0].type.expression.type.content, "Response");

    var inner_split = split.body[1].type.split;
    try testing.expectEqualStrings(inner_split.content[0].type.expression.type.content, "inner choice 1");
    var inner_line = inner_split.body[0].type.line;
    try testing.expectEqualStrings(inner_line.speaker.?.type.identifier, "Other");
    try testing.expectEqualStrings(inner_line.content[0].type.expression.type.content, "Inner Response");

    inner_split = split.body[2].type.split;
    try testing.expectEqualStrings(inner_split.content[0].type.expression.type.content, "inner choice 2");
    inner_line = inner_split.body[0].type.line;
    try testing.expectEqualStrings(inner_line.speaker.?.type.identifier, "Other");
    try testing.expectEqualStrings(inner_line.content[0].type.expression.type.content, "Second Inner Response");

    split = body[1].type.split;
    try testing.expectEqualStrings(split.content[0].type.expression.type.content, "choice 2");
    line = split.body[0].type.line;
    try testing.expectEqualStrings(line.speaker.?.type.identifier, "Other");
    try testing.expectEqualStrings(line.content[0].type.expression.type.content, "Second Response");
}

test "Parse Inline Code" {
    const allocator = testing.allocator;
    const input =
        \\  === BOUGH ===
        \\      :Speaker: Hi, `sayHello()`, how are you?
        \\  ===
    ;
    var errors = Errors.init(allocator);
    defer errors.deinit();
    const tree = parse(allocator, input, &errors) catch |err| {
        try errors.write(input, std.io.getStdErr().writer());
        return err;
    };
    defer tree.deinit();
    // try tree.print(std.io.getStdErr().writer());
    const content = tree.root[0].type.block.body[0].type.line.content;
    try testing.expectEqualStrings("Hi, ", content[0].type.expression.type.content);
    try testing.expectEqualStrings("sayHello", content[1].type.expression.type.call.name.type.identifier);
    try testing.expectEqualStrings(", how are you?", content[2].type.expression.type.content);
}

test "Parse Inline If" {
    const allocator = testing.allocator;
    const input =
        \\  === BOUGH ===
        \\      :Speaker: Hi, `if (true) {`how are you? `} else {`buzz off! `}`
        \\  ===
    ;
    var errors = Errors.init(allocator);
    defer errors.deinit();
    const tree = parse(allocator, input, &errors) catch |err| {
        try errors.write(input, std.io.getStdErr().writer());
        return err;
    };
    defer tree.deinit();
    // try tree.print(std.io.getStdErr().writer());
    const content = tree.root[0].type.block.body[0].type.line.content;
    try testing.expectEqualStrings("Hi, ", content[0].type.expression.type.content);
    const if_state = content[1].type.if_statement;
    try testing.expectEqualStrings("how are you? ", if_state.true_body.type.block.body[0].type.expression.type.content);
    try testing.expectEqualStrings("buzz off! ", if_state.false_body.?.type.block.body[0].type.expression.type.content);
}

test "Parse Content If Block" {
    const allocator = testing.allocator;
    const input =
        \\  === BOUGH ===
        \\      `if (true) {`
        \\      :Speaker: Say words
        \\      => JUMP.SOMEWHERE
        \\      `}`
        \\  ===
    ;
    var errors = Errors.init(allocator);
    defer errors.deinit();
    const tree = parse(allocator, input, &errors) catch |err| {
        try errors.write(input, std.io.getStdErr().writer());
        return err;
    };
    defer tree.deinit();
    const bough = tree.root[0].type.block;
    const if_exp = bough.body[0].type.if_statement;
    const line = if_exp.true_body.type.block.body[0].type.line;
    try testing.expectEqualStrings("Say words", line.content[0].type.expression.type.content);
}

test "Parse Content If/Else Blocks" {
    const allocator = testing.allocator;
    const input =
        \\  === BOUGH ===
        \\      `if (true) {`
        \\      :Speaker: Say true
        \\      `} else {`
        \\      :Speaker: Say false
        \\      `}`
        \\      => JUMP
        \\  ===
    ;
    var errors = Errors.init(allocator);
    defer errors.deinit();
    const tree = parse(allocator, input, &errors) catch |err| {
        try errors.write(input, std.io.getStdErr().writer());
        return err;
    };
    defer tree.deinit();
    const bough = tree.root[0].type.block;
    const if_exp = bough.body[0].type.if_statement;
    var line = if_exp.true_body.type.block.body[0].type.line;
    try testing.expectEqualStrings("Say true", line.content[0].type.expression.type.content);
    line = if_exp.false_body.?.type.block.body[0].type.line;
    try testing.expectEqualStrings("Say false", line.content[0].type.expression.type.content);
    const last = bough.body[1].type.jump;
    try testing.expectEqualStrings("JUMP", last.type.identifier);
    // try tree.print(std.io.getStdErr().writer());
}

test "Parse Test File" {
    const allocator = testing.allocator;
    const path = "./src/compiler/test/scene.topi";
    const file = std.fs.cwd().openFile(path, .{}) catch |e| {
        return std.log.err("Could not open file: {s}, {}", .{ path, e });
    };
    defer file.close();

    var contents = try file.reader().readAllAlloc(allocator, 4000);
    defer allocator.free(contents);

    var errors = Errors.init(allocator);
    defer errors.deinit();
    const tree = parse(allocator, contents, &errors) catch |err| {
        try errors.write(contents, std.io.getStdErr().writer());
        return err;
    };
    defer tree.deinit();
    // try tree.print(std.io.getStdErr().writer());
}
