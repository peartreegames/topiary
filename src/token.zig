const std = @import("std");
const testing = std.testing;

pub const Token = struct {
    token_type: TokenType,
    start: usize,
    end: usize,
    line: usize,
    column: usize,

    pub fn eql(self: Token, other: Token) bool {
        return self.token_type == other.token_type and self.start == other.start and self.end == other.end;
    }
};

pub const TokenType = enum {
    left_paren,
    right_paren,
    left_brace,
    right_brace,
    left_bracket,
    right_bracket,

    colon,
    comma,
    dot,
    dot_dot,
    hash,
    minus,
    percent,
    plus,
    pipe,
    semi_colon,
    slash,
    star,
    tilde,
    // minus_equal,
    // percent_equal,
    // plus_equal,
    // slash_equal,
    // star_equal,

    bang,
    bang_equal,
    equal,
    equal_equal,
    greater,
    greater_equal,
    less,
    less_equal,

    identifier,
    number,
    string,

    @"and",
    @"break",
    @"const",
    @"continue",
    @"else",
    @"enum",
    @"extern",
    false,
    @"for",
    @"if",
    import,
    new,
    nil,
    @"or",
    @"return",
    self,
    @"struct",
    @"switch",
    true,
    @"var",
    @"while",

    bough,
    fork,
    divert,

    comment,
    eof,
    illegal,
};

pub const Keywords = std.ComptimeStringMap(TokenType, .{
    .{ "and", .@"and" },
    .{ "bough", .bough },
    .{ "break", .@"break" },
    .{ "continue", .@"continue" },
    .{ "const", .@"const" },
    .{ "else", .@"else" },
    .{ "enum", .@"enum" },
    .{ "extern", .@"extern" },
    .{ "false", .false },
    .{ "for", .@"for" },
    .{ "fork", .fork },
    .{ "if", .@"if" },
    .{ "import", .import },
    .{ "new", .new },
    .{ "nil", .nil },
    .{ "or", .@"or" },
    .{ "return", .@"return" },
    .{ "struct", .@"struct" },
    .{ "switch", .@"switch" },
    .{ "true", .true },
    .{ "var", .@"var" },
    .{ "while", .@"while" },
});

pub fn toString(token_type: TokenType) []const u8 {
    return switch (token_type) {
        .left_paren => "(",
        .right_paren => ")",
        .left_brace => "{{",
        .right_brace => "}}",
        .left_bracket => "[",
        .right_bracket => "]",

        .colon => ":",
        .comma => ",",
        .dot => ".",
        .dot_dot => "..",
        .hash => "#",
        .minus => "-",
        .percent => "%",
        .plus => "+",
        .pipe => "|",
        .semi_colon => ":",
        .slash => "/",
        .star => "*",
        .tilde => "~",
        // .minus_equal => "-=",
        // .percent_equal => "%=",
        // .plus_equal => "+=",
        // .slash_equal => "/=",
        // .star_equal => "*=",

        .bang => "!",
        .bang_equal => "!=",
        .equal => "=",
        .equal_equal => "==",
        .greater => ">",
        .greater_equal => ">=",
        .less => "<",
        .less_equal => "<=",

        .identifier => "[identifier]",
        .number => "[number]",
        .string => "[string]",

        .@"and" => "and",
        .@"break" => "break",
        .@"const" => "const",
        .@"continue" => "continue",
        .@"else" => "else",
        .@"enum" => "enum",
        .@"extern" => "extern",
        .false => "false",
        .@"for" => "for",
        .@"if" => "if",
        .import => "import",
        .new => "new",
        .nil => "nil",
        .@"or" => "or",
        .@"return" => "return",
        .self => "self",
        .@"struct" => "struct",
        .@"switch" => "switch",
        .true => "true",
        .@"var" => "var",
        .@"while" => "while",

        .bough => "bough",
        .fork => "fork",
        .divert => "=>",

        .comment => "[comment]",
        .eof => "[eof]",
        .illegal => "[illegal]",
    };
}

pub fn findType(identifier: []const u8) TokenType {
    return Keywords.get(identifier) orelse .identifier;
}

test "Identifiers" {
    const idents = &[_][]const u8{
        "a",
        "test",
        "word",
    };
    for (idents) |ident| {
        try testing.expect(findType(ident) == .identifier);
    }
}
