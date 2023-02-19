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
    illegal,
    eof,
    eol,
    period,
    identifier,
    blackboard,
    integer,
    float,
    string,
    comment,
    // operators
    assign,
    plus,
    minus,
    percent,
    bang,
    asterisk,
    slash,
    less_than,
    greater_than,
    less_than_equal,
    greater_than_equal,
    equal,
    not_equal,
    double_period,
    // delimiters
    limb,
    branch,
    jump,
    backtick,
    hash,
    gather,
    split,
    comma,
    left_parenthesis,
    right_parenthesis,
    left_brace,
    right_brace,
    left_bracket,
    right_bracket,
    colon,
    query,
    // keywords
    constant,
    variable,
    @"extern",
    while_loop,
    for_loop,
    nil,
    @"and",
    @"or",
    true,
    false,
    @"if",
    @"else",
    @"return",
    @"continue",
    @"break",
    @"switch",
    import,
    function_keyword,
    enum_keyword,
    bool_keyword,
    integer_keyword,
    float_keyword,
    string_keyword,
    void_keyword,
};

pub const Keywords = std.ComptimeStringMap(TokenType, .{
    .{ "const", .constant },
    .{ "var", .variable },
    .{ "extern", .@"extern" },
    .{ "while", .while_loop },
    .{ "for", .for_loop },
    .{ "nil", .nil },
    .{ "true", .true },
    .{ "false", .false },
    .{ "if", .@"if" },
    .{ "else", .@"else" },
    .{ "return", .@"return" },
    .{ "and", .@"and" },
    .{ "or", .@"or" },
    .{ "break", .@"break" },
    .{ "continue", .@"continue" },
    .{ "switch", .@"switch" },
    .{ "import", .import },
    .{ "func", .function_keyword },
    .{ "enum", .enum_keyword },
    .{ "bool", .bool_keyword },
    .{ "int", .integer_keyword },
    .{ "float", .float_keyword },
    .{ "string", .string_keyword },
    .{ "void", .void_keyword },
});

pub fn fmtString(token_type: TokenType) []const u8 {
    return switch (token_type) {
        .illegal => "[illegal]",
        .eof => "[eof]",
        .eol => "[eol]",
        .identifier => "[identifier]",
        .blackboard => "[blackboard]",
        .integer => "[integer]",
        .float => "[float]",
        .string => "[string]",
        .comment => "[comment]",
        // operators
        .period => ".",
        .assign => "=",
        .plus => "+",
        .minus => "-",
        .percent => "%",
        .bang => "!",
        .asterisk => "*",
        .slash => "/",
        .less_than => "<",
        .greater_than => ">",
        .less_than_equal => "<=",
        .greater_than_equal => ">=",
        .equal => "==",
        .not_equal => "!=",
        .double_period => "..",
        // delimiters
        .limb => "===",
        .branch => "---",
        .jump => "=>",
        .backtick => "`",
        .hash => "#",
        .comma => ",",
        .gather => ">-",
        .split => "-<",
        .left_parenthesis => "(",
        .right_parenthesis => ")",
        .left_brace => "{{",
        .right_brace => "}}",
        .left_bracket => "[",
        .right_bracket => "]",
        .colon => ":",
        .query => "?",
        // keywords
        .constant => "const",
        .variable => "var",
        .@"extern" => "extern",
        .while_loop => "while",
        .for_loop => "for",
        .nil => "nil",
        .@"and" => "and",
        .@"or" => "or",
        .true => "true",
        .false => "false",
        .@"if" => "if",
        .@"else" => "else",
        .@"continue" => "continue",
        .@"break" => "break",
        .@"return" => "return",
        .@"switch" => "switch",
        .import => "import",
        .function_keyword => "func",
        .enum_keyword => "enum",
        .bool_keyword => "bool",
        .integer_keyword => "int",
        .float_keyword => "float",
        .string_keyword => "string",
        .void_keyword => "void",
    };
}

pub fn findType(identifier: []const u8) TokenType {
    return Keywords.get(identifier) orelse .identifier;
}

pub fn findNumberType(identifier: []const u8) TokenType {
    for (identifier) |char| {
        if (char == '.') return .float;
    }
    return .integer;
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
