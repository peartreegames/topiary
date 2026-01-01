const std = @import("std");

const Lexer = @import("lexer.zig").Lexer;
const tok = @import("token.zig");
const ast = @import("ast.zig");
const Statement = ast.Statement;
const Expression = ast.Expression;
const TokenType = tok.TokenType;
const Token = tok.Token;

const File = @import("../module.zig").File;
const UUID = @import("../utils/index.zig").UUID;

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

pub const Parser = struct {
    current_token: Token,
    peek_token: Token,
    allocator: std.mem.Allocator,
    lexer: *Lexer,
    file: *File,
    file_index: usize = 0,
    depth: usize = 0,
    mode: Mode = .standard,

    pub const Error = error{
        ParserError,
        OutOfMemory,
        Overflow,
        InvalidCharacter,
    };

    const Mode = union(enum) {
        standard: void,
        interpolated: []const u8,
    };

    inline fn allocate(self: Parser, value: anytype) !*@TypeOf(value) {
        const T = @TypeOf(value);
        std.debug.assert(@typeInfo(T) != .pointer);
        const ptr = try self.allocator.create(T);
        ptr.* = value;
        return ptr;
    }

    inline fn fail(self: *Parser, comptime msg: []const u8, token: Token, args: anytype) Error {
        try self.file.errors.add(msg, token, .err, args);
        return Error.ParserError;
    }

    fn print(self: *Parser, msg: []const u8) void {
        const peek_source = if (self.peekIs(.eof)) "[eof]" else self.file.source[self.peek_token.start..self.peek_token.end];
        std.log.warn("=={s}== -- {}:{s} -- {}:{s}", .{ msg, self.current_token.token_type, self.file.source[self.current_token.start..self.current_token.end], self.peek_token.token_type, peek_source });
    }

    pub fn next(self: *Parser) void {
        self.current_token = self.peek_token;
        self.peek_token = self.lexer.next(self.file_index);
    }

    fn consumeIdentifier(self: *Parser) ![]const u8 {
        try self.expectCurrent(.identifier);
        const name = try self.getStringValue();
        self.next();
        return name;
    }

    fn getStringValue(self: *Parser) ![]const u8 {
        return try self.allocator.dupe(u8, self.file.source[self.current_token.start..self.current_token.end]);
    }

    pub fn statement(self: *Parser) Error!Statement {
        return switch (self.current_token.token_type) {
            .include => try self.includeStatement(),
            .class => try self.classDeclaration(),
            .@"enum", .enumseq => try self.enumDeclaration(),
            .@"extern", .@"var", .@"const" => try self.varDeclaration(),
            .bough => try self.boughStatement(),
            .divert => try self.divertStatement(),
            .colon => try self.dialogueStatement(),
            .tilde => try self.choiceStatement(),
            .fork => try self.forkStatement(),
            .@"fn" => try self.functionDeclaration(),
            .@"for" => try self.forStatement(),
            .fin => .{ .token = self.current_token, .type = .fin },
            .@"if" => try self.ifStatement(),
            .@"switch" => try self.switchStatement(),
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
            else => .{
                .token = self.current_token,
                .type = .{
                    .expression = try self.expression(.lowest),
                },
            },
        };
    }

    fn includeStatement(self: *Parser) Error!Statement {
        const start = self.current_token;
        self.next();

        const path = self.file.source[self.current_token.start..self.current_token.end];
        if (!self.file.module.allow_includes) {
            return .{
                .token = start,
                .type = .{
                    .include = .{
                        .path = path,
                        .contents = &.{},
                    },
                },
            };
        }

        if (self.file.path.len == 0) return self.fail("Cannot include. Current file path not set", start, .{});
        const full_path = try std.fs.path.resolve(self.allocator, &.{ self.file.dir_name, path });
        defer self.allocator.free(full_path);

        if (self.file.module.includes.getKey(full_path)) |k| {
            return .{
                .token = start,
                .type = .{
                    .include = .{
                        .path = k,
                        .contents = &.{},
                    },
                },
            };
        }

        const file = self.file.module.addFileAtPath(full_path) catch |err| {
            return self.fail("Could not create include file '{s}': {}", self.current_token, .{ path, err });
        };
        file.loadSource() catch |err| {
            return self.fail("Could not load include file '{s}': {}", self.current_token, .{ path, err });
        };
        file.buildTree() catch |err| {
            return self.fail("Could not build include file tree '{s}': {}", self.current_token, .{ path, err });
        };

        return .{
            .token = start,
            .type = .{
                .include = .{
                    .path = file.path,
                    .contents = file.tree.root,
                },
            },
        };
    }

    fn classDeclaration(self: *Parser) Error!Statement {
        const start = self.current_token;
        self.next();
        const name = try self.consumeIdentifier();
        try self.expectCurrent(.left_brace);
        self.next();

        var names = std.ArrayList([]const u8).empty;
        var fields = std.ArrayList(Expression).empty;
        var methods = std.ArrayList(Statement).empty;
        errdefer {
            names.deinit(self.allocator);
            fields.deinit(self.allocator);
            methods.deinit(self.allocator);
        }

        while (!self.currentIs(.right_brace)) {
            if (self.currentIs(.@"fn")) {
                var method = try self.functionDeclaration();
                method.type.function.is_method = true;
                try methods.append(self.allocator, method);
            } else {
                try names.append(self.allocator, try self.consumeIdentifier());
                try self.expectCurrent(.equal);
                self.next();
                const field = try self.expression(.lowest);
                try fields.append(self.allocator, field);
                self.next();
            }

            if (self.currentIs(.comma)) self.next();
            if (self.currentIs(.eof)) return self.fail("Unterminated class body", start, .{});
        }

        try self.expectCurrent(.right_brace);
        if (names.items.len != fields.items.len) return self.fail("Missing field assignment value", self.current_token, .{});
        return .{
            .token = start,
            .type = .{
                .class = .{
                    .name = name,
                    .field_names = try names.toOwnedSlice(self.allocator),
                    .fields = try fields.toOwnedSlice(self.allocator),
                    .methods = try methods.toOwnedSlice(self.allocator)
                },
            },
        };
    }

    fn enumDeclaration(self: *Parser) Error!Statement {
        const start = self.current_token;
        const is_seq = start.token_type == .enumseq;
        self.next();
        const name = try self.consumeIdentifier();
        try self.expectCurrent(.left_brace);
        self.next();
        var values = std.ArrayList([]const u8).empty;
        errdefer values.deinit(self.allocator);
        while (!self.currentIs(.right_brace)) {
            try values.append(self.allocator, try self.consumeIdentifier());
            if (self.currentIs(.comma)) self.next();
        }
        return .{
            .token = start,
            .type = .{
                .@"enum" = .{
                    .is_seq = is_seq,
                    .name = name,
                    .values = try values.toOwnedSlice(self.allocator),
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
        const expr = try self.expression(.lowest);
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

    fn functionDeclaration(self: *Parser) Error!Statement {
        const start = self.current_token;
        self.next();
        const name = try self.consumeIdentifier();
        var list = std.ArrayList([]const u8).empty;
        errdefer list.deinit(self.allocator);

        try self.expectCurrent(.left_paren);
        self.next();
        if (self.currentIs(.identifier)) {
            try list.append(self.allocator, try self.consumeIdentifier());
        }
        while (self.currentIs(.comma)) {
            self.next();
            try list.append(self.allocator, try self.consumeIdentifier());
        }
        try self.expectCurrent(.right_paren);
        self.next();
        return .{
            .token = start,
            .type = .{
                .function = .{
                    .name = name,
                    .parameters = try list.toOwnedSlice(self.allocator),
                    .body = try self.block(),
                },
            },
        };
    }

    fn boughStatement(self: *Parser) Error!Statement {
        const start = self.current_token;
        self.next();
        const name = try self.consumeIdentifier();
        const body = try self.block();
        return .{
            .token = start,
            .type = .{
                .bough = .{
                    .id = UUID.create(std.hash.Wyhash.hash(0, name)),
                    .name = name,
                    .body = body,
                },
            },
        };
    }

    fn returnStatement(self: *Parser) Error!Statement {
        const start_token = self.current_token;
        self.next();
        if (self.currentIs(.void)) {
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
        const start = self.current_token;
        var left: Expression = switch (self.current_token.token_type) {
            .identifier => try self.identifierExpression(),
            .number => blk: {
                const string_number = self.file.source[self.current_token.start..self.current_token.end];
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
            .list => try self.listExpression(),
            .map => try self.mapExpression(),
            .set => try self.setExpression(),
            .new => try self.instanceExpression(),
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

    // if no braces used will parse a single statement into a list
    fn block(self: *Parser) Error![]const Statement {
        var list = std.ArrayList(Statement).empty;
        const has_brace = self.currentIs(.left_brace);
        if (!has_brace) {
            try list.append(self.allocator, try self.statement());
            return try list.toOwnedSlice(self.allocator);
        }
        self.next();
        while (!self.currentIsOneOf([2]TokenType{ .right_brace, .eof })) {
            try list.append(self.allocator, try self.statement());
            self.next();
        }
        if (has_brace and self.currentIs(.eof)) {
            return self.fail("Missing closing brace", self.current_token, .{});
        }
        return try list.toOwnedSlice(self.allocator);
    }

    fn instanceExpression(self: *Parser) Error!Expression {
        const start = self.current_token;
        self.next();
        const name = try self.consumeIdentifier();
        try self.expectCurrent(.left_brace);
        self.next();

        var field_names = std.ArrayList([]const u8).empty;
        var fields = std.ArrayList(Expression).empty;
        errdefer field_names.deinit(self.allocator);
        errdefer fields.deinit(self.allocator);
        while (!self.currentIs(.right_brace)) {
            try field_names.append(self.allocator, try self.consumeIdentifier());
            try self.expectCurrent(.equal);
            self.next();
            const field = try self.expression(.lowest);
            // if (field.type == .function) field.type.function.is_method = true;
            try fields.append(self.allocator, field);
            self.next();
            if (self.currentIs(.comma)) self.next();
        }
        try self.expectCurrent(.right_brace);
        if (field_names.items.len != fields.items.len) return self.fail("Missing field assignment value", self.current_token, .{});

        return .{
            .token = start,
            .type = .{
                .instance = .{
                    .name = name,
                    .field_names = try field_names.toOwnedSlice(self.allocator),
                    .fields = try fields.toOwnedSlice(self.allocator),
                },
            },
        };
    }

    fn stringExpression(self: *Parser) Error!Expression {
        const token = self.current_token;
        var value: []const u8 = "";
        var depth: usize = 0;
        var start: usize = token.start;

        var exprs = std.ArrayList(Expression).empty;
        var expr_index: usize = 0;
        errdefer exprs.deinit(self.allocator);
        for (self.file.source[token.start..token.end], 0..) |char, i| {
            if (char == '{') {
                if (depth == 0) {
                    value = try std.fmt.allocPrint(self.allocator, "{s}{s}{{{d}}}", .{ value, self.file.source[start..(token.start + i)], expr_index });
                    start = token.start + i + 1;
                    expr_index += 1;
                }
                depth += 1;
            }
            if (char == '}') {
                depth -= 1;
                if (depth == 0) {
                    try self.parseInterpolatedExpression(self.file.source[start..(token.start + i)], &exprs, start);
                    start = token.start + i + 1;
                }
            }
        }
        value = try std.fmt.allocPrint(self.allocator, "{s}{s}", .{ value, self.file.source[start..token.end] });
        return .{
            .token = token,
            .type = .{
                .string = .{
                    .raw = try self.allocator.dupe(u8, self.file.source[token.start..token.end]),
                    .value = value,
                    .expressions = try exprs.toOwnedSlice(self.allocator),
                },
            },
        };
    }

    fn parseInterpolatedExpression(self: *Parser, source: []const u8, exprs: *std.ArrayList(Expression), offset: usize) !void {
        var lexer = Lexer.init(source, offset);
        var parser = Parser{
            .current_token = lexer.next(self.file_index),
            .peek_token = lexer.next(self.file_index),
            .allocator = self.allocator,
            .lexer = &lexer,
            .file = self.file,
            .file_index = self.file_index,
        };

        while (!parser.currentIs(.eof)) : (parser.next()) {
            try exprs.append(self.allocator, try parser.expression(.lowest));
        }
    }

    fn listExpression(self: *Parser) Error!Expression {
        const start_token = self.current_token;
        try self.expectPeek(.left_brace);
        var list = std.ArrayList(Expression).empty;
        errdefer list.deinit(self.allocator);
        self.next();
        if (self.currentIs(.right_brace)) {
            return .{
                .token = start_token,
                .type = .{
                    .list = try list.toOwnedSlice(self.allocator),
                },
            };
        }
        try list.append(self.allocator, try self.expression(.lowest));
        while (self.peekIs(.comma)) {
            self.next();
            self.next();
            try list.append(self.allocator, try self.expression(.lowest));
        }
        self.next();
        try self.expectCurrent(.right_brace);
        return .{
            .token = start_token,
            .type = .{
                .list = try list.toOwnedSlice(self.allocator),
            },
        };
    }

    fn mapPairSetKey(self: *Parser) Error!Expression {
        const start_token = self.current_token;

        const key = try self.expression(.lowest);
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

    fn mapExpression(self: *Parser) Error!Expression {
        const start_token = self.current_token;
        try self.expectPeek(.left_brace);
        var list = std.ArrayList(Expression).empty;
        errdefer list.deinit(self.allocator);
        self.next();

        if (self.currentIs(.right_brace)) {
            return .{ .token = start_token, .type = .{ .map = try list.toOwnedSlice(self.allocator) } };
        }

        const first = try self.mapPairSetKey();
        self.next();
        if (self.currentIs(.comma)) self.next();
        try list.append(self.allocator, first);
        while (!self.currentIs(.right_brace)) {
            const item = try self.mapPairSetKey();
            if (item.type != .map_pair)
                return self.fail("Item type '{s}' cannot be added to map", item.token, .{@tagName(item.type)});
            try list.append(self.allocator, item);
            self.next();
            if (self.currentIs(.comma) or self.peekIs(.right_brace)) self.next();
        }
        return .{ .token = start_token, .type = .{ .map = try list.toOwnedSlice(self.allocator) } };
    }

    fn setExpression(self: *Parser) Error!Expression {
        const start_token = self.current_token;
        try self.expectPeek(.left_brace);
        var list = std.ArrayList(Expression).empty;
        errdefer list.deinit(self.allocator);
        self.next();

        if (self.currentIs(.right_brace)) {
            return .{ .token = start_token, .type = .{ .set = try list.toOwnedSlice(self.allocator) } };
        }

        const first = try self.mapPairSetKey();
        self.next();
        if (self.currentIs(.comma)) self.next();
        try list.append(self.allocator, first);
        while (!self.currentIs(.right_brace)) {
            const item = try self.mapPairSetKey();
            if (item.type == .map_pair)
                return self.fail("Item type '{s}' cannot be added set", item.token, .{@tagName(item.type)});
            try list.append(self.allocator, item);
            self.next();
            if (self.currentIs(.comma) or self.peekIs(.right_brace)) self.next();
        }
        return .{ .token = start_token, .type = .{ .set = try list.toOwnedSlice(self.allocator) } };
    }

    fn ifExpression(self: *Parser) Error!Expression {
        const start = self.current_token;
        self.next(); // skip if
        const condition = try self.allocate(try self.expression(.lowest));
        self.next();

        const true_value = try self.allocate(try self.expression(.lowest));
        try self.expectPeek(.@"else");
        self.next();
        const false_value = try self.allocate(try self.expression(.lowest));
        return .{
            .token = start,
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
        const start = self.current_token;
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
            .token = start,
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
        var list = std.ArrayList([]const u8).empty;
        errdefer list.deinit(self.allocator);

        try self.expectPeek(.identifier);

        try list.append(self.allocator, try self.getStringValue());
        while (self.peekIs(.dot)) {
            self.next();
            self.next();
            try list.append(self.allocator, try self.getStringValue());
        }
        var is_backup: bool = false;
        if (self.peekIs(.caret)) {
            is_backup = true;
            self.next();
        }

        return .{
            .token = start_token,
            .type = .{
                .divert = .{
                    .path = try list.toOwnedSlice(self.allocator),
                    .is_backup = is_backup,
                },
            },
        };
    }

    fn forkStatement(self: *Parser) Error!Statement {
        const start = self.current_token;
        self.next();

        var name: ?[]const u8 = null;
        var is_backup: bool = false;
        if (self.currentIs(.caret)) {
            is_backup = true;
            self.next();
        }
        if (self.currentIs(.identifier)) name = try self.consumeIdentifier();

        return .{
            .token = start,
            .type = .{
                .fork = .{
                    .name = name,
                    .body = try self.block(),
                    .is_backup = is_backup,
                },
            },
        };
    }

    fn choiceStatement(self: *Parser) Error!Statement {
        const start = self.current_token;
        const is_unique = self.peekIs(.star);
        if (is_unique) self.next();

        var name: ?[]const u8 = null;
        if (self.peekIs(.identifier)) {
            self.next();
            name = try self.getStringValue();
        }
        self.next();
        const text = try self.stringExpression();

        const id: UUID.ID = if (self.peekIs(.at)) blk: {
            self.next();
            break :blk UUID.fromString(self.file.source[self.current_token.start..self.current_token.end]);
        } else blk: {
            var new_id = UUID.create(std.hash.Wyhash.hash(0, text.type.string.raw));
            UUID.setAuto(&new_id);
            break :blk new_id;
        };
        const tags = try self.getTagsList();
        self.next();
        return .{
            .token = start,
            .type = .{
                .choice = .{
                    .id = id,
                    .name = name,
                    .content = text,
                    .is_unique = is_unique,
                    .body = try self.block(),
                    .tags = tags,
                },
            },
        };
    }

    fn getTagsList(self: *Parser) Error![][]const u8 {
        var tags = std.ArrayList([]const u8).empty;
        while (self.peekIs(.hash)) {
            self.next();
            const tag = try self.getStringValue();
            try tags.append(self.allocator, tag);
        }
        return tags.toOwnedSlice(self.allocator);
    }

    fn dialogueStatement(self: *Parser) Error!Statement {
        const start_token = self.current_token;
        self.next();
        var speaker: ?[]const u8 = null;
        if (self.currentIs(.string)) return self.fail("Strings are not permitted as Speakers", start_token, .{});
        if (!self.currentIs(.colon)) {
            speaker = try self.consumeIdentifier();
        }
        try self.expectCurrent(.colon);
        self.next();
        const text = try self.stringExpression();
        const id: UUID.ID = if (self.peekIs(.at)) blk: {
            self.next();
            break :blk UUID.fromString(self.file.source[self.current_token.start..self.current_token.end]);
        } else blk: {
            var new_id = UUID.create(std.hash.Wyhash.hash(0, text.type.string.raw));
            UUID.setAuto(&new_id);
            break :blk new_id;
        };

        const tags = try self.getTagsList();
        return .{
            .token = start_token,
            .type = .{
                .dialogue = .{
                    .id = id,
                    .speaker = speaker,
                    .content = try self.allocate(text),
                    .tags = tags,
                },
            },
        };
    }

    fn arguments(self: *Parser) Error![]Expression {
        var list = std.ArrayList(Expression).empty;
        errdefer list.deinit(self.allocator);

        // no arguments
        if (self.peekIs(.right_paren)) {
            self.next();
            return list.toOwnedSlice(self.allocator);
        }

        self.next();
        try list.append(self.allocator, try self.expression(.lowest));

        while (self.peekIs(.comma)) {
            self.next();
            self.next();
            try list.append(self.allocator, try self.expression(.lowest));
        }
        try self.expectPeek(.right_paren);
        return list.toOwnedSlice(self.allocator);
    }

    fn callExpression(self: *Parser, func: Expression) Error!Expression {
        const start_token = self.current_token;
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
        const iterator = try self.expression(.lowest);
        switch (iterator.type) {
            .range, .identifier => {},
            else => return self.fail("Expected list, set, map, or range in for loop, found {}", iterator.token, .{iterator.token.token_type}),
        }

        try self.expectPeek(.pipe);
        self.next();
        const capture = try self.consumeIdentifier();
        try self.expectPeek(.left_brace);
        const body = try self.block();
        return .{
            .token = start_token,
            .type = .{
                .@"for" = .{
                    .index = ast.Expression.for_index,
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

    fn switchStatement(self: *Parser) Error!Statement {
        const start_token = self.current_token;
        self.next();
        const capture = try self.expression(.lowest);

        try self.expectPeek(.left_brace);
        var prongs = std.ArrayList(Statement).empty;
        errdefer prongs.deinit(self.allocator);

        self.next();
        try prongs.append(self.allocator, try self.switchProng());

        while (self.peekIs(.comma)) {
            self.next();
            if (self.peekIs(.right_brace)) break;
            self.next();
            try prongs.append(self.allocator, try self.switchProng());
        }

        try self.expectPeek(.right_brace);

        return .{
            .token = start_token,
            .type = .{
                .@"switch" = .{
                    .capture = capture,
                    .prongs = try prongs.toOwnedSlice(self.allocator),
                },
            },
        };
    }

    fn switchProng(self: *Parser) Error!Statement {
        const start_token = self.current_token;
        var values = std.ArrayList(Expression).empty;
        errdefer values.deinit(self.allocator);
        const is_else = start_token.token_type == .@"else";
        if (!is_else) {
            try values.append(self.allocator, try self.expression(.lowest));
            while (self.peekIs(.comma)) {
                self.next();
                self.next();
                try values.append(self.allocator, try self.expression(.lowest));
            }
        }

        try self.expectPeek(.colon);
        self.next();
        return .{
            .token = start_token,
            .type = .{
                .switch_prong = .{
                    .values = if (is_else) null else try values.toOwnedSlice(self.allocator),
                    .body = try self.block(),
                },
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
            "Expected current token to be '" ++ Token.toString(token_type) ++ "', found {}",
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
            "Expected next token to be '" ++ Token.toString(token_type) ++ "', found {}",
            self.peek_token,
            .{self.peek_token.token_type},
        );
    }

    fn peekIs(self: Parser, token_type: TokenType) bool {
        return self.peek_token.token_type == token_type;
    }

    fn peekIsOneOf(self: *Parser, token_types: anytype) bool {
        inline for (token_types) |token_type| {
            if (self.peekIs(token_type)) return true;
        }
        return false;
    }

    pub fn currentIs(self: Parser, token_type: TokenType) bool {
        return self.current_token.token_type == token_type;
    }

    fn currentIsOneOf(self: Parser, token_types: anytype) bool {
        inline for (token_types) |token_type| {
            if (self.currentIs(token_type)) return true;
        }
        return false;
    }
};
