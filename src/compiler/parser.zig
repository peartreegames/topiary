// Combination of two other parsers:
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

    fn next(self: *Parser) void {
        // std.log.warn("{}\n", .{ self.current_token.token_type });
        self.current_token = self.peek_token;

        if (self.mode != .root and self.current_token.token_type == .backtick) {
            self.current_token = self.lexer.next();
            self.mode = if (self.mode == .code) .content else .code;
        }
        self.peek_token = self.lexer.next();

        if (self.mode == .code) {
            while (self.peekIs(.eol)) {
                self.peek_token = self.lexer.next();
            }
        }
    }

    fn parseStatement(self: *Parser) Error!Statement {
        if (self.currentIs(.eol)) self.next();

        return switch (self.mode) {
            .root => return switch (self.current_token.token_type) {
                .blackboard, .limb => self.parseBlockStatement(),
                .import => self.parseImport(),
                else => self.fail("Expected blackboard, limb, or import at top level", self.current_token, .{}),
            },
            .code => return switch (self.current_token.token_type) {
                .comment => self.parseComment(),
                .constant, .variable => self.parseDeclaration(),
                .@"return" => self.parseReturn(),
                // .@"switch" => self.parseSwitchStatement(),
                .while_loop => self.parseWhile(),
                .for_loop => self.parseFor(),
                .@"break" => self.parseBreak(),
                .@"continue" => self.parseContinue(),
                else => self.parseExpressionStatement(),
            },
            .content => return switch (self.current_token.token_type) {
                .comment => self.parseComment(),
                .branch => self.parseBlockStatement(),
                .jump => self.parseJump(),
                .split => self.parseSplit(),
                .colon => self.parseDialogueLine(),
                else => unreachable,
            },
        };
    }

    fn parseDeclaration(self: *Parser) Error!Statement {
        if (self.mode != .code) return self.fail("Declarations can only be used in code", self.current_token, .{});
        const start_token = self.current_token;
        const is_mutable = self.currentIs(.variable);

        try self.expectPeek(.identifier);
        const name = try self.parseIdentifier();
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
            const value = try self.parseExtern();
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
                .expression = try self.parseExpression(.lowest),
            },
        };
    }

    fn parseExpression(self: *Parser, prec: Precedence) Error!Expression {
        var left: Expression = switch (self.current_token.token_type) {
            .identifier => try self.parseIdentifier(),
            .integer => try self.parseIntegerLiteral(),
            .float => try self.parseFloatLiteral(),
            .string => try self.parseStringLiteral(),
            .bang, .minus => try self.parseUnaryExpression(),
            .true, .false => try self.parseBooleanLiteral(),
            .@"if" => try self.parseIfExpression(),
            .left_parenthesis => try self.parseGroupedExpression(),
            // .left_bracket => try self.parseDataStructure(false),
            .nil => try self.parseNil(),
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
                    break :blk try self.parseBinaryExpression(left);
                },
                else => return left,
            };
        }

        return left;
    }
    fn parseUnaryExpression(self: *Parser) Error!Expression {
        const start_token = self.current_token;
        self.next();
        return .{
            .token = start_token,
            .type = .{
                .unary = .{
                    .operator = ast.UnaryOp.fromToken(start_token),
                    .value = try self.allocate(try self.parseExpression(.prefix)),
                },
            },
        };
    }

    fn parseBinaryExpression(self: *Parser, left: Expression) Error!Expression {
        const start_token = self.current_token;
        const op = ast.BinaryOp.fromToken(self.current_token);
        const prec = findPrecedence(self.current_token.token_type);
        self.next();
        return .{
            .token = start_token,
            .type = .{
                .binary = .{
                    .operator = op,
                    .left = try self.allocate(left),
                    .right = try self.allocate(try self.parseExpression(prec)),
                },
            },
        };
    }
    fn parseIntegerLiteral(self: *Parser) Error!Expression {
        const string_number = self.source[self.current_token.start..self.current_token.end];
        const value = try std.fmt.parseInt(i64, string_number, 10);
        return .{
            .token = self.current_token,
            .type = .{
                .integer_literal = value,
            },
        };
    }

    fn parseFloatLiteral(self: *Parser) Error!Expression {
        const string_number = self.source[self.current_token.start..self.current_token.end];
        const value = try std.fmt.parseFloat(f32, string_number);
        return .{
            .token = self.current_token,
            .type = .{
                .float_literal = value,
            },
        };
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

    fn parseBooleanLiteral(self: *Parser) Error!Expression {
        return .{
            .token = self.current_token,
            .type = .{
                .boolean_literal = self.currentIs(.true),
            },
        };
    }

    fn parseGroupedExpression(self: *Parser) Error!Expression {
        self.next();
        const exp = try self.parseExpression(.lowest);
        try self.expectPeek(.right_parenthesis);
        return exp;
    }

    fn parseIfExpression(self: *Parser) Error!Expression {
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
                        .@"if" = .{
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
                    .@"if" = .{
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
                .@"if" = .{
                    .condition = condition,
                    .true_body = true_body,
                    .false_body = null,
                },
            },
        };
    }

    fn parseImport(self: *Parser) Error!Statement {
        self.next(); // skip import keyword
        const token = self.current_token;
        return .{
            .token = token,
            .type = .{
                .import = try self.allocator.dupe(u8, self.source[token.start..token.end]),
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
            // .left_bracket => try self.parseDataStructure(true),
            .identifier => Statement.TypeDefValue{
                .identifier_type = try self.allocator.dupe(u8, self.source[self.current_token.start..self.current_token.end]),
            },
            else => return self.fail("Unexpected token as type", self.current_token, .{}),
        });
    }

    // fn parseSplit(self: *Parser) Error!Statement {
    //     const start_token = self.current_token;
    //     var list = ArrayList(Node).init(self.allocator);
    //     errdefer list.deinit();
    //
    //     const depth: usize = self.current_token.column;
    //     while (self.currentIsOneOf(&[_]TokenType{ .split, .gather, .eof, .branch })) {
    //         if (!self.currentIsOneOf(&[_]TokenType{ .split, .gather })) break;
    //         if (self.current_token.column < depth) break;
    //         var choice = try self.parseChoice(depth);
    //         try list.append(choice);
    //     }
    //
    //     split.nodes = try self.parseUntilEndOfLine();
    //     return Node{ .split = split };
    // }

    fn parseJump(self: *Parser) Error!Statement {
        const start_token = self.current_token;
        self.next();
        const destination = try self.parseIdentifier();
        if (!self.peekIs(.eol) or !self.peekIs(.eof)) {
            return self.fail("Jumps must be on their own line", self.peek_token, .{});
        }
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

        while (!self.currentIsOneOf([_]TokenType{ .eol, .eof })) {
            const item = try parseStatement();
            try list.append(item);
            self.next();
        }
        return list.toOwnedSlice();
    }

    fn parseDialogueLine(self: *Parser) Error!Statement {
        const state = try self.allocator.create(Statement);
        state.* = .{
            .token = self.current_token,
            .type = .{
                .line = .{
                    .speaker = null,
                    .content = undefined,
                },
            },
        };
        if (!self.peekIs(.colon)) {
            try self.expectPeek(.identifier);
            state.type.line.speaker = try self.parseIdentifier();
        }

        try self.expectPeek(.colon);
        self.next();
        state.type.line.content = try self.parseUntilEndOfLine();
        return state;
    }

    fn parseContent(self: *Parser) Error!Statement {
        return .{
            .token = self.current_token,
            .type = .{
                .content = try parseContentExpression(),
            },
        };
    }

    fn parseContentExpression(self: *Parser) Error!Expression {
        const start_token = self.current_token;
        var end: usize = start_token.end;
        while (!self.peekIsOneOf(&[_]TokenType{ .eol, .eof, .backtick })) {
            self.next();
            end = self.current_token.end;
        }

        const content = try self.allocator.dupe(u8, self.source[start_token.start..end]);
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

    // fn parseSplitChoice(self: *Parser, depth: usize) Error!Statement {
    //     const start_token = self.current_token;
    //     self.next(); // skip split
    //     var content = try self.parseUntilEndOfLine();
    //     var list = ArrayList(Statement).init(self.allocator);
    //     errdefer list.deinit();
    //
    //     while (!self.peekIsOneOf(&[_]TokenType{ .branch, .limb, .jump, .eof })) {
    //         switch (self.peek_token.token_type) {
    //             .split, .gather => {
    //                 if (self.peek_token.column < depth) break;
    //             },
    //             else => {},
    //         }
    //         var line = try self.parseStatement();
    //         try list.append(line);
    //         self.next();
    //         self.next();
    //     }
    //     choice.nodes = list;
    //     return choice;
    // }

    fn parseBlockStatement(self: *Parser) Error!Statement {
        self.depth += 1;
        defer self.depth -= 1;
        const start_token = self.current_token;
        const closing_token = switch (start_token.token_type) {
            .left_brace => .right_brace,
            .split => .gather,
            else => self.current_token.token_type,
        };

        if (self.currentIs(.limb)) {
            self.mode = .content;
        } else if (self.currentIs(.blackboard)) {
            self.mode = .code;
        }

        self.next(); //skip opening token
        const name = switch (closing_token) {
            .limb, .branch => blk: {
                try self.expectCurrent(.identifier);
                const name = try self.parseIdentifier();
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
            const exp = try self.parseStatement();
            try list.append(exp);
            self.next();
        }
        if (closing_token == .limb or closing_token == .blackboard) {
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

    fn parseMap(self: *Parser) Error!Expression {
        var pairs = std.ArrayList(Expression).init(self.allocator);
        const start_token = self.current_token;
        while (!self.peekIs(.right_brace)) {
            self.next();
            const pair = try self.parsePair();
            try pairs.append(pair);
            if (!self.peekIs(.right_brace))
                try self.expectPeek(.comma);
        }

        try self.expectPeek(.right_brace);
        return .{
            .token = start_token,
            .type = .{
                .map_value = .{ .pairs = try pairs.toOwnedSlice() },
            },
        };
    }

    fn parsePair(self: *Parser) Error!Expression {
        const start_token = self.current_token;
        const key = try self.parseExpression(.lowest);
        if (!self.peekIs(.colon)) {
            return .{ .token = start_token, .type = .{ .set_value = .{ .key = key } } };
        }
        self.next();
        return .{
            .token = start_token,
            .type = .{
                .map_pair = .{
                    .key = key,
                    .value = try self.parseExpression(.lowest),
                },
            },
        };
    }

    fn parseEnumLiteral(self: *Parser) Error!Expression {
        const start_token = self.current_token;
        var enums = std.ArrayList(Expression).init(self.allocator);
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

    fn parseNil(self: *Parser) Error!Expression {
        return .{ .token = self.current_token, .type = .nil };
    }

    fn parseExtern(self: *Parser) Error!Expression {
        return .{ .token = self.current_token, .type = .@"extern" };
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
        try testing.expectEqualStrings(case.id, decl.name.type.identifier);

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
        try testing.expectEqualStrings(case.id, decl.name.type.identifier);
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
    try testing.expectEqualStrings("e", decl.name.type.identifier);
    var enum_literal = decl.value.type.enum_literal;
    try testing.expectEqualStrings(enum_literal[0].type.identifier, "one");
    try testing.expectEqualStrings(enum_literal[1].type.identifier, "two");

    node = tree.root[0].type.block.body[1];
    decl = node.type.declaration;
    try testing.expectEqualStrings("en", decl.name.type.identifier);
    enum_literal = decl.value.type.enum_literal;
    try testing.expectEqualStrings(enum_literal[0].type.identifier, "one");
    try testing.expectEqualStrings(enum_literal[1].type.identifier, "two");

    node = tree.root[0].type.block.body[2];
    decl = node.type.declaration;
    try testing.expectEqualStrings("val", decl.name.type.identifier);
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
        \\ ```
    ;
    var errors = Errors.init(allocator);
    defer errors.deinit();
    const tree = parse(allocator, input, &errors) catch |err| {
        try errors.write(input, std.io.getStdErr().writer());
        return err;
    };
    defer tree.deinit();

    const if_exp = tree.root[0].type.block.body[0].type.expression.type.@"if";
    const ret_exp = if_exp.true_body.type.block.body[0].type;
    try testing.expect(ret_exp == .return_expression);
    try testing.expectEqualStrings(ret_exp.return_expression.type.identifier, "x");
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
        \\ === LIMB ===
        \\     --- BRANCH ---
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
    const limb = tree.root[0];
    try testing.expect(limb.token.token_type == .limb);
    try testing.expectEqualStrings(limb.type.block.name.?.type.identifier, "LIMB");

    const branch = limb.type.block.body[0];
    try testing.expect(branch.token.token_type == .branch);
    try testing.expectEqualStrings(branch.type.block.name.?.type.identifier, "BRANCH");

    const line = branch.type.block.body[0].type.line;
    try testing.expectEqualStrings(line.speaker.?.type.identifier, "Speaker");
    try testing.expectEqualStrings(line.content[0].type.expression.content, "Text goes here");
}

// test "Parse No Speaker" {
//     const allocator = testing.allocator;
//     const input =
//         \\  === LIMB ===
//         \\      :: Text goes here
//         \\  ===
//     ;
//     var errors = Errors.init(allocator);
//     defer errors.deinit();
//     const tree = parse(allocator, input, &errors) catch |err| {
//         try errors.write(input, std.io.getStdErr().writer());
//         return err;
//     };
//     defer tree.deinit();
//     const limb = tree.nodes[0].block_statement;
//
//     const line = limb.nodes[0].line;
//     try testing.expect(line.speaker == null);
//     try testing.expectEqualStrings(line.nodes.?[0].text.value, "Text goes here");
// }
// test "Parse Jump" {
//     const allocator = testing.allocator;
//     const input =
//         \\  === LIMB ===
//         \\      -> JUMP_ID
//         \\  ===
//     ;
//     var errors = Errors.init(allocator);
//     defer errors.deinit();
//     const tree = parse(allocator, input, &errors) catch |err| {
//         try errors.write(input, std.io.getStdErr().writer());
//         return err;
//     };
//     defer tree.deinit();
//     const limb = tree.nodes[0].block_statement;
//     const jump = limb.nodes[0].jump;
//     try testing.expect(jump.token.token_type == .jump);
//     try testing.expectEqualStrings(jump.destination.identifier.value, "JUMP_ID");
// }
//
// test "Parse Choices" {
//     const allocator = testing.allocator;
//     const input =
//         \\  === LIMB ===
//         \\      -<  choice 1
//         \\          :Other: Response
//         \\      -<  choice 2
//         \\          :Other: Second Response
//         \\      >-
//         \\  ===
//     ;
//     var errors = Errors.init(allocator);
//     defer errors.deinit();
//     const tree = parse(allocator, input, &errors) catch |err| {
//         try errors.write(input, std.io.getStdErr().writer());
//         return err;
//     };
//     defer tree.deinit();
//     const limb = tree.nodes[0].block_statement;
//     const jump = limb.nodes[0].jump;
//     try testing.expect(jump.token.token_type == .jump);
//     try testing.expectEqualStrings(jump.destination.identifier.value, "JUMP_ID");
// }
