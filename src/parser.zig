const std = @import("std");
const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;
const Arena = std.heap.ArenaAllocator;
const testing = std.testing;
const Lexer = @import("./lexer.zig").Lexer;
const tok = @import("./token.zig");
const ast = @import("./ast.zig");
const File = @import("./module.zig").File;
const Errors = @import("./compiler-error.zig").CompilerErrors;
const UUID = @import("./utils/uuid.zig").UUID;
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

/// Used for parsing files
/// Required if topi file has "include" statements
pub fn parseFile(allocator: Allocator, dir: std.fs.Dir, path: []const u8, err: *Errors) Parser.Error!Tree {
    const file = dir.openFile(path, .{}) catch |e| {
        err.add("Could not open file {s}: {}", undefined, .err, .{ path, e }) catch {};
        return Parser.Error.ParserError;
    };
    defer file.close();

    const stat = file.stat() catch |e| {
        err.add("Could not read file stats {s}: {}", undefined, .err, .{ path, e }) catch {};
        return Parser.Error.ParserError;
    };
    const file_size = stat.size;
    const source = try allocator.alloc(u8, file_size);
    defer allocator.free(source);
    file.reader().readNoEof(source) catch |e| {
        err.add("Could not read file {s}: {}", undefined, .err, .{ path, e }) catch {};
        return Parser.Error.ParserError;
    };
    errdefer err.write(source, std.io.getStdErr().writer()) catch {};
    var lexer = Lexer.init(source);
    var arena = std.heap.ArenaAllocator.init(allocator);
    errdefer arena.deinit();

    var parser = Parser{
        .current_token = lexer.next(),
        .peek_token = lexer.next(),
        .arena = arena.state,
        .allocator = arena.allocator(),
        .lexer = &lexer,
        .directory = dir,
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
        .source = source,
    };
}

pub const Parser = struct {
    current_token: Token,
    peek_token: Token,
    arena: std.heap.ArenaAllocator.State,
    allocator: Allocator,
    lexer: *Lexer,
    file: *File,
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
        try self.file.errors.add(msg, token, .err, args);
        return Error.ParserError;
    }

    fn print(self: *Parser, msg: []const u8) void {
        const peek_source = if (self.peekIs(.eof)) "[eof]" else self.file.source[self.peek_token.start..self.peek_token.end];
        std.log.warn("=={s}== -- {}:{s} -- {}:{s}", .{ msg, self.current_token.token_type, self.file.source[self.current_token.start..self.current_token.end], self.peek_token.token_type, peek_source });
    }

    pub fn next(self: *Parser) void {
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
        return try self.allocator.dupe(u8, self.file.source[self.current_token.start..self.current_token.end]);
    }

    pub fn statement(self: *Parser) Error!Statement {
        return switch (self.current_token.token_type) {
            .include => try self.includeStatement(),
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
            .comment => try self.commentStatement(),
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
        const path = try self.getStringValue();

        if (std.mem.eql(u8, self.file.path, "")) return self.fail("File Path not set", self.current_token, .{});
        const full_path = try std.fs.path.resolve(self.allocator, &.{ self.file.dir_name, path });
        if (self.file.module.includes.contains(full_path)) {
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

        const file = try self.allocator.create(File);
        file.* = File.create(full_path, self.file.module) catch |err| {
            return self.fail("Could not create include file '{s}': {}", self.current_token, .{ path, err });
        };
        file.loadSource(self.allocator) catch |err| {
            return self.fail("Could not load include file '{s}': {}", self.current_token, .{ path, err });
        };

        file.buildTree(self.allocator) catch |err| {
            return self.fail("Could not build include file tree '{s}': {}", self.current_token, .{ path, err });
        };
        try self.file.module.includes.putNoClobber(full_path, file);

        return .{
            .token = start,
            .type = .{
                .include = .{
                    .path = path,
                    .contents = file.tree.root,
                },
            },
        };
    }

    fn classDeclaration(self: *Parser) Error!Statement {
        const start = self.current_token;
        self.next();
        const name = try self.consumeIdentifier();
        try self.expectCurrent(.equal);
        self.next();
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
            var field = try self.expression(.lowest);
            if (field.type == .function) field.type.function.is_method = true;
            try fields.append(field);
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
        const start = self.current_token;
        self.next();
        const name = try self.consumeIdentifier();
        try self.expectCurrent(.equal);
        self.next();
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
        const start = self.current_token;
        const name = try self.consumeIdentifier();
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
        const start = self.current_token;
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

    fn instanceExpression(self: *Parser) Error!Expression {
        const start = self.current_token;
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
            var field = try self.expression(.lowest);
            if (field.type == .function) field.type.function.is_method = true;
            try fields.append(field);
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
                    .field_names = try field_names.toOwnedSlice(),
                    .fields = try fields.toOwnedSlice(),
                },
            },
        };
    }

    fn stringExpression(self: *Parser) Error!Expression {
        const token = self.current_token;
        var value: []const u8 = "";
        var depth: usize = 0;
        var start: usize = token.start;

        var exprs = std.ArrayList(Expression).init(self.allocator);
        errdefer exprs.deinit();
        for (self.file.source[token.start..token.end], 0..) |char, i| {
            if (char == '{') {
                if (depth == 0) {
                    value = try std.fmt.allocPrint(self.allocator, "{s}{s}{s}", .{ value, self.file.source[start..(token.start + i)], "{}" });
                    start = token.start + i + 1;
                }
                depth += 1;
            }
            if (char == '}') {
                depth -= 1;
                if (depth == 0) {
                    try self.parseInterpolatedExpression(self.file.source[start..(token.start + i)], &exprs, token.start);
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
                    .expressions = try exprs.toOwnedSlice(),
                },
            },
        };
    }

    fn parseInterpolatedExpression(self: *Parser, source: []const u8, exprs: *std.ArrayList(Expression), offset: usize) !void {
        var lexer = Lexer.init(source);
        var err = self.file.errors;
        const tmp_pos = err.offset_pos;
        const tmp_col = err.offset_col;
        const tmp_line = err.offset_line;
        const file = self.file;

        err.offset_pos += offset;
        err.offset_col += lexer.column + 1;
        err.offset_line += if (lexer.line >= 2) lexer.line - 2 else 0;

        const str_file = try self.allocator.create(File);
        defer self.allocator.destroy(str_file);
        str_file.* = .{
            .path = file.path,
            .name = file.name,
            .source = source,
            .dir_name = file.dir_name,
            .dir = file.dir,
            .module = file.module,
            .errors = Errors.init(self.allocator),
        };
        self.file = str_file;

        var parser = Parser{
            .current_token = lexer.next(),
            .peek_token = lexer.next(),
            .arena = self.arena,
            .allocator = self.allocator,
            .lexer = &lexer,
            .file = str_file,
        };

        while (!parser.currentIs(.eof)) : (parser.next()) {
            try exprs.append(try parser.expression(.lowest));
        }
        err.offset_pos = tmp_pos;
        err.offset_line = tmp_line;
        err.offset_col = tmp_col;
        self.file = file;
    }

    fn listExpression(self: *Parser) Error!Expression {
        const start_token = self.current_token;
        try self.expectPeek(.left_brace);
        var list = std.ArrayList(Expression).init(self.allocator);
        errdefer list.deinit();
        self.next();
        if (self.currentIs(.right_brace)) {
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
        try self.expectCurrent(.right_brace);
        return .{
            .token = start_token,
            .type = .{
                .list = try list.toOwnedSlice(),
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
        var list = std.ArrayList(Expression).init(self.allocator);
        errdefer list.deinit();
        self.next();

        if (self.currentIs(.right_brace)) {
            return .{ .token = start_token, .type = .{ .map = try list.toOwnedSlice() } };
        }

        const first = try self.mapPairSetKey();
        self.next();
        if (self.currentIs(.comma)) self.next();
        try list.append(first);
        while (!self.currentIs(.right_brace)) {
            const item = try self.mapPairSetKey();
            if (item.type != .map_pair)
                return self.fail("Item type '{s}' cannot be added to map", item.token, .{@tagName(item.type)});
            try list.append(item);
            self.next();
            if (self.currentIs(.comma) or self.peekIs(.right_brace)) self.next();
        }
        return .{ .token = start_token, .type = .{ .map = try list.toOwnedSlice() } };
    }

    fn setExpression(self: *Parser) Error!Expression {
        const start_token = self.current_token;
        try self.expectPeek(.left_brace);
        var list = std.ArrayList(Expression).init(self.allocator);
        errdefer list.deinit();
        self.next();

        if (self.currentIs(.right_brace)) {
            return .{ .token = start_token, .type = .{ .set = try list.toOwnedSlice() } };
        }

        const first = try self.mapPairSetKey();
        self.next();
        if (self.currentIs(.comma)) self.next();
        try list.append(first);
        while (!self.currentIs(.right_brace)) {
            const item = try self.mapPairSetKey();
            if (item.type == .map_pair)
                return self.fail("Item type '{s}' cannot be added set", item.token, .{@tagName(item.type)});
            try list.append(item);
            self.next();
            if (self.currentIs(.comma) or self.peekIs(.right_brace)) self.next();
        }
        return .{ .token = start_token, .type = .{ .set = try list.toOwnedSlice() } };
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
        var list = std.ArrayList([]const u8).init(self.allocator);
        errdefer list.deinit();

        try self.expectPeek(.identifier);

        try list.append(try self.getStringValue());
        while (self.peekIs(.dot)) {
            self.next();
            self.next();
            try list.append(try self.getStringValue());
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
                    .path = try list.toOwnedSlice(),
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
        const tags = try self.getTagsList();
        self.next();
        return .{
            .token = start,
            .type = .{
                .choice = .{
                    .id = UUID.create(std.hash.Wyhash.hash(0, text.type.string.raw)),
                    .name = name,
                    .text = text,
                    .is_unique = is_unique,
                    .body = try self.block(),
                    .tags = tags,
                },
            },
        };
    }

    fn getTagsList(self: *Parser) Error![][]const u8 {
        var tags = std.ArrayList([]const u8).init(self.allocator);
        while (self.peekIs(.hash)) {
            self.next();
            const tag = try self.getStringValue();
            try tags.append(tag);
        }
        return tags.toOwnedSlice();
    }

    fn dialogueStatement(self: *Parser) Error!Statement {
        const start_token = self.current_token;
        self.next();
        var speaker: ?[]const u8 = null;
        if (!self.currentIs(.colon)) {
            const speaker_start = self.current_token.start;
            var speaker_end = self.current_token.end;
            // allow any character including spaces for speaker
            while (!self.currentIs(.colon)) {
                self.next();
                speaker_end = self.current_token.end;
            }
            speaker = try self.allocator.dupe(u8, self.file.source[speaker_start..speaker_end]);
        }
        try self.expectCurrent(.colon);
        self.next();
        const text = try self.stringExpression();
        const tags = try self.getTagsList();
        return .{
            .token = start_token,
            .type = .{
                .dialogue = .{
                    .id = UUID.create(std.hash.Wyhash.hash(0, text.type.string.raw)),
                    .speaker = speaker,
                    .content = try self.allocate(text),
                    .tags = tags,
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
        var prongs = std.ArrayList(Statement).init(self.allocator);

        self.next();
        try prongs.append(try self.switchProng());

        while (self.peekIs(.comma)) {
            self.next();
            if (self.peekIs(.right_brace)) break;
            self.next();
            try prongs.append(try self.switchProng());
        }

        try self.expectPeek(.right_brace);

        return .{
            .token = start_token,
            .type = .{
                .@"switch" = .{
                    .capture = capture,
                    .prongs = try prongs.toOwnedSlice(),
                },
            },
        };
    }

    fn switchProng(self: *Parser) Error!Statement {
        const start_token = self.current_token;
        var values = std.ArrayList(Expression).init(self.allocator);
        defer values.deinit();
        const is_else = start_token.token_type == .@"else";
        if (!is_else) {
            try values.append(try self.expression(.lowest));
            while (self.peekIs(.comma)) {
                self.next();
                self.next();
                try values.append(try self.expression(.lowest));
            }
        }

        try self.expectPeek(.colon);
        self.next();
        return .{
            .token = start_token,
            .type = .{
                .switch_prong = .{
                    .values = if (is_else) null else try values.toOwnedSlice(),
                    .body = try self.block(),
                },
            },
        };
    }

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

    pub fn currentIs(self: Parser, token_type: TokenType) bool {
        return self.current_token.token_type == token_type;
    }

    fn currentIsOneOf(self: Parser, token_types: anytype) bool {
        for (token_types) |token_type| {
            if (self.currentIs(token_type)) return true;
        }
        return false;
    }
};
