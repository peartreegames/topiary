const std = @import("std");
const testing = std.testing;

pub const Token = struct {
    token_type: TokenType,
    start: usize,
    end: usize,
    line: usize,
    column: usize,
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
    minus_equal,
    percent,
    percent_equal,
    plus,
    plus_equal,
    pipe,
    semi_colon,
    slash,
    slash_equal,
    star,
    star_equal,
    tilde,

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
    void,
    @"while",

    bough,
    fork,
    jump,

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
    .{ "void", .void },
    .{ "while", .@"while" },
});

pub fn fmtString(token_type: TokenType) []const u8 {
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
        .minus_equal => "-=",
        .percent => "%",
        .percent_equal => "%=",
        .plus => "+",
        .plus_equal => "+=",
        .pipe => "|",
        .semi_colon => ":",
        .slash => "/",
        .slash_equal => "/=",
        .star => "*",
        .star_equal => "*=",
        .tilde => "~",

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
        .void => "void",
        .@"while" => "while",

        .bough => "bough",
        .fork => "fork",
        .jump => "=>",

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
