const std = @import("std");
const testing = std.testing;
const token = @import("token.zig");
const Token = token.Token;
const TokenType = token.TokenType;

pub const Lexer = struct {
    source: []const u8,
    position: usize = 0,
    read_position: usize = 0,
    char: u8 = 0,
    line: usize = 1,
    column: usize = 0,
    offset: usize = 0,

    pub fn init(source: []const u8, offset: usize) Lexer {
        // Skip utf-8 BOM
        const src_start = if (std.mem.startsWith(u8, source, "\xEF\xBB\xBF")) 3 else @as(usize, 0);
        var lexer = Lexer{
            .source = source,
            .position = src_start,
            .read_position = src_start,
            .offset = offset,
        };
        lexer.readChar();
        return lexer;
    }

    fn createToken(self: *Lexer, token_type: TokenType, start: usize, file_index: usize) Token {
        return .{
            .token_type = token_type,
            .start = start + self.offset,
            .end = self.position + self.offset,
            .line = self.line,
            .column = self.column - (self.position - start),
            .file_index = file_index,
        };
    }

    fn readAndCreateToken(self: *Lexer, token_type: TokenType, count: usize, file_index: usize) Token {
        const start = self.position;
        self.readChars(count);
        return self.createToken(token_type, start, file_index);
    }

    pub fn next(self: *Lexer, file_index: usize) Token {
        self.skipWhitespace();

        const token_type: TokenType = switch (self.char) {
            '(' => .left_paren,
            ')' => .right_paren,
            '{' => .left_brace,
            '}' => .right_brace,
            '[' => .left_bracket,
            ']' => .right_bracket,
            '|' => .pipe,
            ',' => .comma,
            '.' => if (self.peekChar() == '.') {
                return self.readAndCreateToken(.dot_dot, 2, file_index);
            } else .dot,
            '-' => if (self.peekChar() == '=') {
                return self.readAndCreateToken(.minus_equal, 2, file_index);
            } else .minus,
            '~' => .tilde,
            '+' => if (self.peekChar() == '=') {
                return self.readAndCreateToken(.plus_equal, 2, file_index);
            } else .plus,
            ':' => .colon,
            '@' => {
                const start = self.position + 1;
                while (!isWhitespace(self.peekChar()) and !isEndOfLine(self.peekChar())) {
                    self.readChar();
                }
                self.readChar();
                return self.createToken(.at, start, file_index);
            },
            '#' => {
                var start = self.position + 1;
                while (isWhitespace(self.peekChar())) {
                    start += 1; // allow for starting spaces
                    self.readChar();
                }
                while (self.peekChar() != '#' and !isWhitespace(self.peekChar()) and !isEndOfLine(self.peekChar())) {
                    self.readChar();
                }
                self.readChar();
                return self.createToken(.hash, start, file_index);
            },
            '*' => if (self.peekChar() == '=') {
                return self.readAndCreateToken(.star_equal, 2, file_index);
            } else .star,
            '%' => if (self.peekChar() == '=') {
                return self.readAndCreateToken(.percent_equal, 2, file_index);
            } else .percent,
            '!' => if (self.peekChar() == '=') {
                return self.readAndCreateToken(.bang_equal, 2, file_index);
            } else .bang,
            '=' => if (std.mem.eql(u8, self.peekChars(2), "==")) {
                return self.readAndCreateToken(.bough, 3, file_index);
            } else if (self.peekChar() == '=') {
                return self.readAndCreateToken(.equal_equal, 2, file_index);
            } else if (self.peekChar() == '>') {
                return self.readAndCreateToken(.divert, 2, file_index);
            } else .equal,
            '<' => if (self.peekChar() == '=') {
                return self.readAndCreateToken(.less_equal, 2, file_index);
            } else .less,
            '>' => if (self.peekChar() == '=') {
                return self.readAndCreateToken(.greater_equal, 2, file_index);
            } else .greater,
            '^' => .caret,
            '/' => if (self.peekChar() == '=') {
                return self.readAndCreateToken(.slash_equal, 2, file_index);
            } else if (self.peekChar() == '/') {
                self.readLine();
                return next(self, file_index);
            } else .slash,
            '"' => {
                self.readChar();
                const start = self.position;
                self.readString();
                defer self.readChar();
                return self.createToken(.string, start, file_index);
            },
            0 => .eof,
            else => |c| if (isLetter(c)) {
                const start = self.position;
                const ident = self.readIdentifier();
                return self.createToken(token.findType(ident), start, file_index);
            } else if (isDigit(c)) {
                const start = self.position;
                const number = self.readNumber();
                _ = number;
                return self.createToken(.number, start, file_index);
            } else .illegal,
        };

        defer self.readChar();
        const newToken = self.createToken(token_type, self.position, file_index);
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
        while (isWhitespace(self.char) or self.char == ';') {
            if (isEndOfLine(self.char)) {
                self.line += 1;
                self.column = 1;
            }
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
        var count: usize = 0;
        var is_string = true;
        while (true) {
            // multi line strings not supported
            if (self.char == '\n' or self.char == 0) break;
            // two double quotes '""' become single double quotes
            if (self.char == '"' and self.peekChar() != '"' and count == 0) break;
            if (self.char == '"' and self.peekChar() == '"') {
                self.readChar();
            }
            if (self.char == '}' or self.char == '{') {
                is_string = !is_string;
                if (!is_string) {
                    count += 1;
                } else count -= 1;
            }
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
            ' ', '\t', '\r', '\n' => true,
            else => false,
        };
    }

    fn isEndOfLine(char: u8) bool {
        return char == '\n' or char == 0;
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
