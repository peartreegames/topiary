const std = @import("std");
const testing = std.testing;

pub const Token = struct {
    token_type: TokenType,
    start: usize,
    end: usize,
    line: usize,
    column: usize,
    file_index: usize,

    pub fn eql(self: Token, other: Token) bool {
        return self.token_type == other.token_type and
            self.start == other.start and
            self.end == other.end and
            self.file_index == other.file_index;
    }
};

pub const TokenType = enum {
    left_paren,
    right_paren,
    left_brace,
    right_brace,
    left_bracket,
    right_bracket,

    at,
    caret,
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
    minus_equal,
    percent_equal,
    plus_equal,
    slash_equal,
    star_equal,

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
    enumseq,
    @"extern",
    false,
    @"for",
    @"if",
    include,
    list,
    map,
    new,
    nil,
    @"or",
    @"return",
    self,
    seq,
    set,
    class,
    @"switch",
    true,
    @"var",
    void,
    @"while",

    bough,
    divert,
    fork,
    fin,

    comment,
    eof,
    illegal,
};

pub const Keywords = std.StaticStringMap(TokenType).initComptime(.{
    .{ "and", .@"and" },
    .{ "bough", .bough },
    .{ "break", .@"break" },
    .{ "continue", .@"continue" },
    .{ "const", .@"const" },
    .{ "else", .@"else" },
    .{ "enum", .@"enum" },
    .{ "enumseq", .enumseq },
    .{ "extern", .@"extern" },
    .{ "false", .false },
    .{ "for", .@"for" },
    .{ "fork", .fork },
    .{ "fin", .fin },
    .{ "if", .@"if" },
    .{ "include", .include },
    .{ "List", .list },
    .{ "Map", .map },
    .{ "new", .new },
    .{ "nil", .nil },
    .{ "or", .@"or" },
    .{ "return", .@"return" },
    .{ "class", .class },
    .{ "Set", .set },
    .{ "switch", .@"switch" },
    .{ "true", .true },
    .{ "var", .@"var" },
    .{ "void", .void },
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

        .at => "@",
        .caret => "^",
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
        .minus_equal => "-=",
        .percent_equal => "%=",
        .plus_equal => "+=",
        .slash_equal => "/=",
        .star_equal => "*=",

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
        .enumseq => "enumseq",
        .@"extern" => "extern",
        .false => "false",
        .@"for" => "for",
        .@"if" => "if",
        .include => "include",
        .list => "List",
        .map => "Map",
        .new => "new",
        .nil => "nil",
        .@"or" => "or",
        .@"return" => "return",
        .self => "self",
        .seq => "seq",
        .set => "Set",
        .class => "class",
        .@"switch" => "switch",
        .true => "true",
        .@"var" => "var",
        .void => "void",
        .@"while" => "while",

        .bough => "bough",
        .divert => "=>",
        .fork => "fork",
        .fin => "fin",

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
