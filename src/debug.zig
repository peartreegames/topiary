const std = @import("std");
const Token = @import("./token.zig").Token;

pub const DebugToken = struct {
    token: Token,
    length: usize,

    pub const List = std.ArrayList(DebugToken);

    pub fn add(tokens: *List, token: Token) !void {
        if (tokens.items.len == 0) {
            try tokens.append(.{ .token = token, .length = 0 });
            return;
        }
        var current = tokens.getLast();
        if (current.token.eql(token)) {
            current.length += 1;
            tokens.items[tokens.items.len - 1] = current;
            return;
        }
        try tokens.append(.{ .token = token, .length = 0 });
    }

    pub fn get(tokens: []DebugToken, index: usize) ?Token {
        var current: usize = 0;
        for (tokens, 0..) |token, i| {
            current += i;
            current += token.length;
            if (current >= index) return token.token;
        }
        return null;
    }
};
