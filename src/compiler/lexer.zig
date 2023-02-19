const std = @import("std");
const testing = std.testing;
const token = @import("./token.zig");
const Token = token.Token;
const TokenType = token.TokenType;

pub const Lexer = struct {
    source: []const u8,
    position: usize = 0,
    read_position: usize = 0,
    char: u8 = 0,
    line: usize = 1,
    column: usize = 0,

    pub fn init(source: []const u8) Lexer {
        var lexer = Lexer{ .source = source };
        lexer.readChar();
        return lexer;
    }

    fn createToken(self: *Lexer, token_type: TokenType, start: usize) Token {
        return .{
            .token_type = token_type,
            .start = start,
            .end = self.position,
            .line = self.line,
            .column = self.column - (self.position - start),
        };
    }

    fn readAndCreateToken(self: *Lexer, token_type: TokenType, count: usize) Token {
        const start = self.position;
        self.readChars(count);
        return self.createToken(token_type, start);
    }

    pub fn next(self: *Lexer) Token {
        self.skipWhitespace();

        const token_type: TokenType = switch (self.char) {
            '\n' => .eol,
            '=' => if (std.mem.eql(u8, self.peekChars(2), "==")) {
                return self.readAndCreateToken(.limb, 3);
            } else if (self.peekChar() == '=') {
                return self.readAndCreateToken(.equal, 2);
            }  else if (self.peekChar() == '>') {
                              return self.readAndCreateToken(.jump, 2); } else .assign,
            '(' => .left_parenthesis,
            ')' => .right_parenthesis,
            ',' => .comma,
            '+' => .plus,
            '-' => if (std.mem.eql(u8, self.peekChars(2), "--")) {
                return self.readAndCreateToken(.branch, 3);
            } else if (self.peekChar() == '<') {
                return self.readAndCreateToken(.split, 2);
            } else .minus,
            '!' => .bang,
            '/' => if (self.peekChar() == '/') {
                const start = self.position;
                self.readLine();
                return self.createToken(.comment, start + 2);
            } else .slash,
            '*' => .asterisk,
            '<' => if (self.peekChar() == '=') {
                return self.readAndCreateToken(.less_than_equal, 2);
            } else .less_than,
            '>' => if (self.peekChar() == '=') {
                return self.readAndCreateToken(.greater_than_equal, 2);
            } else if (self.peekChar() == '-') {
                return self.readAndCreateToken(.gather, 2);
            } else .greater_than,
            '{' => .left_brace,
            '}' => .right_brace,
            '.' => if (self.peekChar() == '.') {
                return self.readAndCreateToken(.double_period, 2);
            } else .period,
            '`' => if (std.mem.eql(u8, self.peekChars(2), "``")) {
                return self.readAndCreateToken(.blackboard, 3);
            } else .backtick,
            '#' => .hash,
            '[' => .left_bracket,
            ']' => .right_bracket,
            ':' => .colon,
            '"' => {
                self.readChar();
                const start = self.position;
                self.readString();
                defer self.readChar();
                return self.createToken(.string, start);
            },
            '?' => .query,
            0 => .eof,
            else => |c| if (isLetter(c)) {
                const start = self.position;
                const ident = self.readIdentifier();
                return self.createToken(token.findType(ident), start);
            } else if (isDigit(c)) {
                const start = self.position;
                const number = self.readNumber();
                return self.createToken(token.findNumberType(number), start);
            } else .illegal,
        };

        defer self.readChar();
        const newToken = self.createToken(token_type, self.position);
        if (token_type == .eol) {
            self.column = 0;
            self.line += 1;
        }
        return newToken;
    }

    fn readChar(self: *Lexer) void {
        self.column += 1;
        self.char = self.peekChar();
        self.position = self.read_position;
        self.read_position += 1;
    }

    fn readChars(self: *Lexer, count: usize) void {
        var i: usize = 0;
        while (i < count) : (i += 1) {
            self.readChar();
        }
    }

    fn peekChar(self: *Lexer) u8 {
        if (self.read_position >= self.source.len) return 0 else return self.source[self.read_position];
    }

    fn peekChars(self: *Lexer, count: usize) []const u8 {
        if (self.read_position + count >= self.source.len) {
            return self.source[self.read_position..];
        } else {
            return self.source[self.read_position .. self.read_position + count];
        }
    }

    fn skipWhitespace(self: *Lexer) void {
        while (isWhitespace(self.char)) {
            self.readChar();
        }
    }

    fn readIdentifier(self: *Lexer) []const u8 {
        const pos = self.position;
        while (isLetter(self.char) or isDigit(self.char)) {
            self.readChar();
        }
        return self.source[pos..self.position];
    }

    fn readNumber(self: *Lexer) []const u8 {
        const pos = self.position;
        while (isDigit(self.char) or self.char == '.') {
            if (self.char == '.' and self.peekChar() == '.') break;
            self.readChar();
        }
        return self.source[pos..self.position];
    }

    fn readString(self: *Lexer) void {
        while (self.char != '"' and self.char != 0) {
            self.readChar();
        }
    }

    fn readLine(self: *Lexer) void {
        while (self.char != '\n' and self.char != 0) {
            self.readChar();
        }
    }

    fn isWhitespace(char: u8) bool {
        return switch (char) {
            ' ', '\t', '\r' => true,
            else => false,
        };
    }

    fn isEndOfLine(char: u8) bool {
        return switch (char) {
            '\n' => true,
            else => false,
        };
    }

    fn isLetter(char: u8) bool {
        return switch (char) {
            'a'...'z', 'A'...'Z', '_' => true,
            else => false,
        };
    }

    fn isDigit(char: u8) bool {
        return switch (char) {
            '0'...'9' => true,
            else => false,
        };
    }
};

test "Check supported tokens" {
    const input =
        \\```
        \\const five: int = 5
        \\const two: int = 2
        \\const decimal: float = 1.234
        \\const sum: func = (x: int, y: int): int {
        \\    return x + y
        \\}
        \\
        \\var result: int = sum(five, two)
        \\!-/*5
        \\5 < 10 > 5
        \\if (5 < 10) {
        \\    return true
        \\} else {
        \\    return false
        \\}
        \\
        \\const value: enum = {
        \\  one,
        \\  two,
        \\}
        \\10 == 10
        \\10 != 9
        \\"foo"
        \\"foo bar"
        \\"foo".len
        \\[1, 2]
        \\{"key": 1}
        \\//comment goes here
        \\nil
        \\```
        \\=== LIMB
        \\--- BRANCH
        \\`sum(1, 5)`
        \\=> LIMB.BRANCH
        \\  :Speaker: Text here #tag
        \\---
        \\===
    ;
    const tests = &[_]Token{
        .{ .token_type = .blackboard, .start = 0, .end = 3, .line = 1, .column = 1 },
        .{ .token_type = .eol, .start = 3, .end = 3, .line = 1, .column = 4 },
        .{ .token_type = .constant, .start = 4, .end = 9, .line = 2, .column = 1 },
        .{ .token_type = .identifier, .start = 10, .end = 14, .line = 2, .column = 7 },
        .{ .token_type = .colon, .start = 14, .end = 14, .line = 2, .column = 11 },
        .{ .token_type = .integer_keyword, .start = 16, .end = 19, .line = 2, .column = 13 },
        .{ .token_type = .assign, .start = 20, .end = 20, .line = 2, .column = 17 },
        .{ .token_type = .integer, .start = 22, .end = 23, .line = 2, .column = 19 },
        .{ .token_type = .eol, .start = 23, .end = 23, .line = 2, .column = 20 },
        .{ .token_type = .constant, .start = 24, .end = 29, .line = 3, .column = 1 },
        .{ .token_type = .identifier, .start = 30, .end = 33, .line = 3, .column = 7 },
        .{ .token_type = .colon, .start = 33, .end = 33, .line = 3, .column = 10 },
        .{ .token_type = .integer_keyword, .start = 35, .end = 38, .line = 3, .column = 12 },
        .{ .token_type = .assign, .start = 39, .end = 39, .line = 3, .column = 16 },
        .{ .token_type = .integer, .start = 41, .end = 42, .line = 3, .column = 18 },
        .{ .token_type = .eol, .start = 42, .end = 42, .line = 3, .column = 19 },
        .{ .token_type = .constant, .start = 43, .end = 48, .line = 4, .column = 1 },
        .{ .token_type = .identifier, .start = 49, .end = 56, .line = 4, .column = 7 },
        .{ .token_type = .colon, .start = 56, .end = 56, .line = 4, .column = 14 },
        .{ .token_type = .float_keyword, .start = 58, .end = 63, .line = 4, .column = 16 },
        .{ .token_type = .assign, .start = 64, .end = 64, .line = 4, .column = 22 },
        .{ .token_type = .float, .start = 66, .end = 71, .line = 4, .column = 24 },
        .{ .token_type = .eol, .start = 71, .end = 71, .line = 4, .column = 29 },
        .{ .token_type = .constant, .start = 72, .end = 77, .line = 5, .column = 1 },
        .{ .token_type = .identifier, .start = 78, .end = 81, .line = 5, .column = 7 },
        .{ .token_type = .colon, .start = 81, .end = 81, .line = 5, .column = 10 },
        .{ .token_type = .function_keyword, .start = 83, .end = 87, .line = 5, .column = 12 },
        .{ .token_type = .assign, .start = 88, .end = 88, .line = 5, .column = 17 },
        .{ .token_type = .left_parenthesis, .start = 90, .end = 90, .line = 5, .column = 19 },
        .{ .token_type = .identifier, .start = 91, .end = 92, .line = 5, .column = 20 },
        .{ .token_type = .colon, .start = 92, .end = 92, .line = 5, .column = 21 },
        .{ .token_type = .integer_keyword, .start = 94, .end = 97, .line = 5, .column = 23 },
        .{ .token_type = .comma, .start = 97, .end = 97, .line = 5, .column = 26 },
        .{ .token_type = .identifier, .start = 99, .end = 100, .line = 5, .column = 28 },
        .{ .token_type = .colon, .start = 100, .end = 100, .line = 5, .column = 29 },
        .{ .token_type = .integer_keyword, .start = 102, .end = 105, .line = 5, .column = 31 },
        .{ .token_type = .right_parenthesis, .start = 105, .end = 105, .line = 5, .column = 34 },
        .{ .token_type = .colon, .start = 106, .end = 106, .line = 5, .column = 35 },
        .{ .token_type = .integer_keyword, .start = 108, .end = 111, .line = 5, .column = 37 },
        .{ .token_type = .left_brace, .start = 112, .end = 112, .line = 5, .column = 41 },
        .{ .token_type = .eol, .start = 113, .end = 113, .line = 5, .column = 42 },
        .{ .token_type = .@"return", .start = 118, .end = 124, .line = 6, .column = 5 },
        .{ .token_type = .identifier, .start = 125, .end = 126, .line = 6, .column = 12 },
        .{ .token_type = .plus, .start = 127, .end = 127, .line = 6, .column = 14 },
        .{ .token_type = .identifier, .start = 129, .end = 130, .line = 6, .column = 16 },
        .{ .token_type = .eol, .start = 130, .end = 130, .line = 6, .column = 17 },
        .{ .token_type = .right_brace, .start = 131, .end = 131, .line = 7, .column = 1 },
        .{ .token_type = .eol, .start = 132, .end = 132, .line = 7, .column = 2 },
        .{ .token_type = .eol, .start = 133, .end = 133, .line = 8, .column = 1 },
        .{ .token_type = .variable, .start = 134, .end = 137, .line = 9, .column = 1 },
        .{ .token_type = .identifier, .start = 138, .end = 144, .line = 9, .column = 5 },
        .{ .token_type = .colon, .start = 144, .end = 144, .line = 9, .column = 11 },
        .{ .token_type = .integer_keyword, .start = 146, .end = 149, .line = 9, .column = 13 },
        .{ .token_type = .assign, .start = 150, .end = 150, .line = 9, .column = 17 },
        .{ .token_type = .identifier, .start = 152, .end = 155, .line = 9, .column = 19 },
        .{ .token_type = .left_parenthesis, .start = 155, .end = 155, .line = 9, .column = 22 },
        .{ .token_type = .identifier, .start = 156, .end = 160, .line = 9, .column = 23 },
        .{ .token_type = .comma, .start = 160, .end = 160, .line = 9, .column = 27 },
        .{ .token_type = .identifier, .start = 162, .end = 165, .line = 9, .column = 29 },
        .{ .token_type = .right_parenthesis, .start = 165, .end = 165, .line = 9, .column = 32 },
        .{ .token_type = .eol, .start = 166, .end = 166, .line = 9, .column = 33 },
        .{ .token_type = .bang, .start = 167, .end = 167, .line = 10, .column = 1 },
        .{ .token_type = .minus, .start = 168, .end = 168, .line = 10, .column = 2 },
        .{ .token_type = .slash, .start = 169, .end = 169, .line = 10, .column = 3 },
        .{ .token_type = .asterisk, .start = 170, .end = 170, .line = 10, .column = 4 },
        .{ .token_type = .integer, .start = 171, .end = 172, .line = 10, .column = 5 },
        .{ .token_type = .eol, .start = 172, .end = 172, .line = 10, .column = 6 },
        .{ .token_type = .integer, .start = 173, .end = 174, .line = 11, .column = 1 },
        .{ .token_type = .less_than, .start = 175, .end = 175, .line = 11, .column = 3 },
        .{ .token_type = .integer, .start = 177, .end = 179, .line = 11, .column = 5 },
        .{ .token_type = .greater_than, .start = 180, .end = 180, .line = 11, .column = 8 },
        .{ .token_type = .integer, .start = 182, .end = 183, .line = 11, .column = 10 },
        .{ .token_type = .eol, .start = 183, .end = 183, .line = 11, .column = 11 },
        .{ .token_type = .@"if", .start = 184, .end = 186, .line = 12, .column = 1 },
        .{ .token_type = .left_parenthesis, .start = 187, .end = 187, .line = 12, .column = 4 },
        .{ .token_type = .integer, .start = 188, .end = 189, .line = 12, .column = 5 },
        .{ .token_type = .less_than, .start = 190, .end = 190, .line = 12, .column = 7 },
        .{ .token_type = .integer, .start = 192, .end = 194, .line = 12, .column = 9 },
        .{ .token_type = .right_parenthesis, .start = 194, .end = 194, .line = 12, .column = 11 },
        .{ .token_type = .left_brace, .start = 196, .end = 196, .line = 12, .column = 13 },
        .{ .token_type = .eol, .start = 197, .end = 197, .line = 12, .column = 14 },
        .{ .token_type = .@"return", .start = 202, .end = 208, .line = 13, .column = 5 },
        .{ .token_type = .true, .start = 209, .end = 213, .line = 13, .column = 12 },
        .{ .token_type = .eol, .start = 213, .end = 213, .line = 13, .column = 16 },
        .{ .token_type = .right_brace, .start = 214, .end = 214, .line = 14, .column = 1 },
        .{ .token_type = .@"else", .start = 216, .end = 220, .line = 14, .column = 3 },
        .{ .token_type = .left_brace, .start = 221, .end = 221, .line = 14, .column = 8 },
        .{ .token_type = .eol, .start = 222, .end = 222, .line = 14, .column = 9 },
        .{ .token_type = .@"return", .start = 227, .end = 233, .line = 15, .column = 5 },
        .{ .token_type = .false, .start = 234, .end = 239, .line = 15, .column = 12 },
        .{ .token_type = .eol, .start = 239, .end = 239, .line = 15, .column = 17 },
        .{ .token_type = .right_brace, .start = 240, .end = 240, .line = 16, .column = 1 },
        .{ .token_type = .eol, .start = 241, .end = 241, .line = 16, .column = 2 },
        .{ .token_type = .eol, .start = 242, .end = 242, .line = 17, .column = 1 },
        .{ .token_type = .constant, .start = 243, .end = 248, .line = 18, .column = 1 },
        .{ .token_type = .identifier, .start = 249, .end = 254, .line = 18, .column = 7 },
        .{ .token_type = .colon, .start = 254, .end = 254, .line = 18, .column = 12 },
        .{ .token_type = .enum_keyword, .start = 256, .end = 260, .line = 18, .column = 14 },
        .{ .token_type = .assign, .start = 261, .end = 261, .line = 18, .column = 19 },
        .{ .token_type = .left_brace, .start = 263, .end = 263, .line = 18, .column = 21 },
        .{ .token_type = .eol, .start = 264, .end = 264, .line = 18, .column = 22 },
        .{ .token_type = .identifier, .start = 267, .end = 270, .line = 19, .column = 3 },
        .{ .token_type = .comma, .start = 270, .end = 270, .line = 19, .column = 6 },
        .{ .token_type = .eol, .start = 271, .end = 271, .line = 19, .column = 7 },
        .{ .token_type = .identifier, .start = 274, .end = 277, .line = 20, .column = 3 },
        .{ .token_type = .comma, .start = 277, .end = 277, .line = 20, .column = 6 },
        .{ .token_type = .eol, .start = 278, .end = 278, .line = 20, .column = 7 },
        .{ .token_type = .right_brace, .start = 279, .end = 279, .line = 21, .column = 1 },
        .{ .token_type = .eol, .start = 280, .end = 280, .line = 21, .column = 2 },
        .{ .token_type = .integer, .start = 281, .end = 283, .line = 22, .column = 1 },
        .{ .token_type = .equal, .start = 284, .end = 286, .line = 22, .column = 4 },
        .{ .token_type = .integer, .start = 287, .end = 289, .line = 22, .column = 7 },
        .{ .token_type = .eol, .start = 289, .end = 289, .line = 22, .column = 9 },
        .{ .token_type = .integer, .start = 290, .end = 292, .line = 23, .column = 1 },
        .{ .token_type = .bang, .start = 293, .end = 293, .line = 23, .column = 4 },
        .{ .token_type = .assign, .start = 294, .end = 294, .line = 23, .column = 5 },
        .{ .token_type = .integer, .start = 296, .end = 297, .line = 23, .column = 7 },
        .{ .token_type = .eol, .start = 297, .end = 297, .line = 23, .column = 8 },
        .{ .token_type = .string, .start = 299, .end = 302, .line = 24, .column = 2 },
        .{ .token_type = .eol, .start = 303, .end = 303, .line = 24, .column = 6 },
        .{ .token_type = .string, .start = 305, .end = 312, .line = 25, .column = 2 },
        .{ .token_type = .eol, .start = 313, .end = 313, .line = 25, .column = 10 },
        .{ .token_type = .string, .start = 315, .end = 318, .line = 26, .column = 2 },
        .{ .token_type = .period, .start = 319, .end = 319, .line = 26, .column = 6 },
        .{ .token_type = .identifier, .start = 320, .end = 323, .line = 26, .column = 7 },
        .{ .token_type = .eol, .start = 323, .end = 323, .line = 26, .column = 10 },
        .{ .token_type = .left_bracket, .start = 324, .end = 324, .line = 27, .column = 1 },
        .{ .token_type = .integer, .start = 325, .end = 326, .line = 27, .column = 2 },
        .{ .token_type = .comma, .start = 326, .end = 326, .line = 27, .column = 3 },
        .{ .token_type = .integer, .start = 328, .end = 329, .line = 27, .column = 5 },
        .{ .token_type = .right_bracket, .start = 329, .end = 329, .line = 27, .column = 6 },
        .{ .token_type = .eol, .start = 330, .end = 330, .line = 27, .column = 7 },
        .{ .token_type = .left_brace, .start = 331, .end = 331, .line = 28, .column = 1 },
        .{ .token_type = .string, .start = 333, .end = 336, .line = 28, .column = 3 },
        .{ .token_type = .colon, .start = 337, .end = 337, .line = 28, .column = 7 },
        .{ .token_type = .integer, .start = 339, .end = 340, .line = 28, .column = 9 },
        .{ .token_type = .right_brace, .start = 340, .end = 340, .line = 28, .column = 10 },
        .{ .token_type = .eol, .start = 341, .end = 341, .line = 28, .column = 11 },
        .{ .token_type = .comment, .start = 344, .end = 361, .line = 29, .column = 3 },
        .{ .token_type = .eol, .start = 361, .end = 361, .line = 29, .column = 20 },
        .{ .token_type = .nil, .start = 362, .end = 365, .line = 30, .column = 1 },
        .{ .token_type = .eol, .start = 365, .end = 365, .line = 30, .column = 4 },
        .{ .token_type = .blackboard, .start = 366, .end = 369, .line = 31, .column = 1 },
        .{ .token_type = .eol, .start = 369, .end = 369, .line = 31, .column = 4 },
        .{ .token_type = .limb, .start = 370, .end = 373, .line = 32, .column = 1 },
        .{ .token_type = .identifier, .start = 374, .end = 378, .line = 32, .column = 5 },
        .{ .token_type = .eol, .start = 378, .end = 378, .line = 32, .column = 9 },
        .{ .token_type = .branch, .start = 379, .end = 382, .line = 33, .column = 1 },
        .{ .token_type = .identifier, .start = 383, .end = 389, .line = 33, .column = 5 },
        .{ .token_type = .eol, .start = 389, .end = 389, .line = 33, .column = 11 },
        .{ .token_type = .backtick, .start = 390, .end = 390, .line = 34, .column = 1 },
        .{ .token_type = .identifier, .start = 391, .end = 394, .line = 34, .column = 2 },
        .{ .token_type = .left_parenthesis, .start = 394, .end = 394, .line = 34, .column = 5 },
        .{ .token_type = .integer, .start = 395, .end = 396, .line = 34, .column = 6 },
        .{ .token_type = .comma, .start = 396, .end = 396, .line = 34, .column = 7 },
        .{ .token_type = .integer, .start = 398, .end = 399, .line = 34, .column = 9 },
        .{ .token_type = .right_parenthesis, .start = 399, .end = 399, .line = 34, .column = 10 },
        .{ .token_type = .backtick, .start = 400, .end = 400, .line = 34, .column = 11 },
        .{ .token_type = .eol, .start = 401, .end = 401, .line = 34, .column = 12 },
        .{ .token_type = .jump, .start = 402, .end = 404, .line = 35, .column = 1 },
        .{ .token_type = .identifier, .start = 405, .end = 409, .line = 35, .column = 4 },
        .{ .token_type = .period, .start = 409, .end = 409, .line = 35, .column = 8 },
        .{ .token_type = .identifier, .start = 410, .end = 416, .line = 35, .column = 9 },
        .{ .token_type = .eol, .start = 416, .end = 416, .line = 35, .column = 15 },
        .{ .token_type = .colon, .start = 419, .end = 419, .line = 36, .column = 3 },
        .{ .token_type = .identifier, .start = 420, .end = 427, .line = 36, .column = 4 },
        .{ .token_type = .colon, .start = 427, .end = 427, .line = 36, .column = 11 },
        .{ .token_type = .identifier, .start = 429, .end = 433, .line = 36, .column = 13 },
        .{ .token_type = .identifier, .start = 434, .end = 438, .line = 36, .column = 18 },
        .{ .token_type = .hash, .start = 439, .end = 439, .line = 36, .column = 23 },
        .{ .token_type = .identifier, .start = 440, .end = 443, .line = 36, .column = 24 },
        .{ .token_type = .eol, .start = 443, .end = 443, .line = 36, .column = 27 },
        .{ .token_type = .branch, .start = 444, .end = 447, .line = 37, .column = 1 },
        .{ .token_type = .eol, .start = 447, .end = 447, .line = 37, .column = 4 },
        .{ .token_type = .limb, .start = 448, .end = 451, .line = 38, .column = 1 },
    };

    var lexer = Lexer.init(input);
    // while (lexer.peekChar() != 0) {
    //     std.debug.print("{}\n", .{lexer.next()});
    // }
    // lexer = Lexer.init(input);
    for (tests) |item| {
        const current = lexer.next();
        try testing.expectEqual(item.token_type, current.token_type);
        try testing.expectEqual(item.start, current.start);
        try testing.expectEqual(item.end, current.end);
    }
}
