const std = @import("std");
const topi = @import("topi");
const Lexer = topi.frontend.Lexer;
const Token = topi.frontend.Token;

test "Check supported tokens" {
    const input =
        \\ const five = 5
        \\ const two = 2
        \\ const decimal = 1.234
        \\ fn sum(x, y) {
        \\   return x + y
        \\ }
        \\ var seven = sum(five, two)
        \\ !-/*5
        \\ 5 < 10 > 5
        \\ if (5 < 10) {
        \\   return true
        \\ } else {
        \\   return false
        \\ }
        \\ 
        \\ enum someEnum {
        \\   one,
        \\   two
        \\ }
        \\ 
        \\ 10 == 10
        \\ 10 != 9
        \\ "foo"
        \\ "foo bar"
        \\ [1,2]
        \\ {"key", 1}
        \\ // comment
        \\ class structValue {
        \\   intValue = 0,
        \\   doubleValue = 0.0
        \\ }
        \\           
        \\ === START {
        \\   :Speaker: "Dialogue text {sum(2,2)}" # tag
        \\ }
        \\ var v = new structValue {}
    ;
    const tests = &[_]Token{
        .{ .token_type = .@"const", .start = 1, .end = 6, .line = 1, .column = 2, .file_index = 0 },
        .{ .token_type = .identifier, .start = 7, .end = 11, .line = 1, .column = 8, .file_index = 0 },
        .{ .token_type = .equal, .start = 12, .end = 12, .line = 1, .column = 13, .file_index = 0 },
        .{ .token_type = .number, .start = 14, .end = 15, .line = 1, .column = 15, .file_index = 0 },
        .{ .token_type = .@"const", .start = 17, .end = 22, .line = 2, .column = 3, .file_index = 0 },
        .{ .token_type = .identifier, .start = 23, .end = 26, .line = 2, .column = 9, .file_index = 0 },
        .{ .token_type = .equal, .start = 27, .end = 27, .line = 2, .column = 13, .file_index = 0 },
        .{ .token_type = .number, .start = 29, .end = 30, .line = 2, .column = 15, .file_index = 0 },
        .{ .token_type = .@"const", .start = 32, .end = 37, .line = 3, .column = 3, .file_index = 0 },
        .{ .token_type = .identifier, .start = 38, .end = 45, .line = 3, .column = 9, .file_index = 0 },
        .{ .token_type = .equal, .start = 46, .end = 46, .line = 3, .column = 17, .file_index = 0 },
        .{ .token_type = .number, .start = 48, .end = 53, .line = 3, .column = 19, .file_index = 0 },
        .{ .token_type = .@"fn", .start = 55, .end = 57, .line = 4, .column = 3, .file_index = 0 },
        .{ .token_type = .identifier, .start = 58, .end = 61, .line = 4, .column = 6, .file_index = 0 },
        .{ .token_type = .left_paren, .start = 61, .end = 61, .line = 4, .column = 9, .file_index = 0 },
        .{ .token_type = .identifier, .start = 62, .end = 63, .line = 4, .column = 10, .file_index = 0 },
        .{ .token_type = .comma, .start = 63, .end = 63, .line = 4, .column = 11, .file_index = 0 },
        .{ .token_type = .identifier, .start = 65, .end = 66, .line = 4, .column = 13, .file_index = 0 },
        .{ .token_type = .right_paren, .start = 66, .end = 66, .line = 4, .column = 14, .file_index = 0 },
        .{ .token_type = .left_brace, .start = 68, .end = 68, .line = 4, .column = 16, .file_index = 0 },
        .{ .token_type = .@"return", .start = 73, .end = 79, .line = 5, .column = 5, .file_index = 0 },
        .{ .token_type = .identifier, .start = 80, .end = 81, .line = 5, .column = 12, .file_index = 0 },
        .{ .token_type = .plus, .start = 82, .end = 82, .line = 5, .column = 14, .file_index = 0 },
        .{ .token_type = .identifier, .start = 84, .end = 85, .line = 5, .column = 16, .file_index = 0 },
        .{ .token_type = .right_brace, .start = 87, .end = 87, .line = 6, .column = 3, .file_index = 0 },
        .{ .token_type = .@"var", .start = 90, .end = 93, .line = 7, .column = 3, .file_index = 0 },
        .{ .token_type = .identifier, .start = 94, .end = 99, .line = 7, .column = 7, .file_index = 0 },
        .{ .token_type = .equal, .start = 100, .end = 100, .line = 7, .column = 13, .file_index = 0 },
        .{ .token_type = .identifier, .start = 102, .end = 105, .line = 7, .column = 15, .file_index = 0 },
        .{ .token_type = .left_paren, .start = 105, .end = 105, .line = 7, .column = 18, .file_index = 0 },
        .{ .token_type = .identifier, .start = 106, .end = 110, .line = 7, .column = 19, .file_index = 0 },
        .{ .token_type = .comma, .start = 110, .end = 110, .line = 7, .column = 23, .file_index = 0 },
        .{ .token_type = .identifier, .start = 112, .end = 115, .line = 7, .column = 25, .file_index = 0 },
        .{ .token_type = .right_paren, .start = 115, .end = 115, .line = 7, .column = 28, .file_index = 0 },
        .{ .token_type = .bang, .start = 118, .end = 118, .line = 8, .column = 3, .file_index = 0 },
        .{ .token_type = .minus, .start = 119, .end = 119, .line = 8, .column = 4, .file_index = 0 },
        .{ .token_type = .slash, .start = 120, .end = 120, .line = 8, .column = 5, .file_index = 0 },
        .{ .token_type = .star, .start = 121, .end = 121, .line = 8, .column = 6, .file_index = 0 },
        .{ .token_type = .number, .start = 122, .end = 123, .line = 8, .column = 7, .file_index = 0 },
        .{ .token_type = .number, .start = 125, .end = 126, .line = 9, .column = 3, .file_index = 0 },
        .{ .token_type = .less, .start = 127, .end = 127, .line = 9, .column = 5, .file_index = 0 },
        .{ .token_type = .number, .start = 129, .end = 131, .line = 9, .column = 7, .file_index = 0 },
        .{ .token_type = .greater, .start = 132, .end = 132, .line = 9, .column = 10, .file_index = 0 },
        .{ .token_type = .number, .start = 134, .end = 135, .line = 9, .column = 12, .file_index = 0 },
        .{ .token_type = .@"if", .start = 137, .end = 139, .line = 10, .column = 3, .file_index = 0 },
        .{ .token_type = .left_paren, .start = 140, .end = 140, .line = 10, .column = 6, .file_index = 0 },
        .{ .token_type = .number, .start = 141, .end = 142, .line = 10, .column = 7, .file_index = 0 },
        .{ .token_type = .less, .start = 143, .end = 143, .line = 10, .column = 9, .file_index = 0 },
        .{ .token_type = .number, .start = 145, .end = 147, .line = 10, .column = 11, .file_index = 0 },
        .{ .token_type = .right_paren, .start = 147, .end = 147, .line = 10, .column = 13, .file_index = 0 },
        .{ .token_type = .left_brace, .start = 149, .end = 149, .line = 10, .column = 15, .file_index = 0 },
        .{ .token_type = .@"return", .start = 154, .end = 160, .line = 11, .column = 5, .file_index = 0 },
        .{ .token_type = .true, .start = 161, .end = 165, .line = 11, .column = 12, .file_index = 0 },
        .{ .token_type = .right_brace, .start = 167, .end = 167, .line = 12, .column = 3, .file_index = 0 },
        .{ .token_type = .@"else", .start = 169, .end = 173, .line = 12, .column = 5, .file_index = 0 },
        .{ .token_type = .left_brace, .start = 174, .end = 174, .line = 12, .column = 10, .file_index = 0 },
        .{ .token_type = .@"return", .start = 179, .end = 185, .line = 13, .column = 5, .file_index = 0 },
        .{ .token_type = .false, .start = 186, .end = 191, .line = 13, .column = 12, .file_index = 0 },
        .{ .token_type = .right_brace, .start = 193, .end = 193, .line = 14, .column = 3, .file_index = 0 },
        .{ .token_type = .@"enum", .start = 198, .end = 202, .line = 16, .column = 3, .file_index = 0 },
        .{ .token_type = .identifier, .start = 203, .end = 211, .line = 16, .column = 8, .file_index = 0 },
        .{ .token_type = .left_brace, .start = 212, .end = 212, .line = 16, .column = 17, .file_index = 0 },
        .{ .token_type = .identifier, .start = 217, .end = 220, .line = 17, .column = 5, .file_index = 0 },
        .{ .token_type = .comma, .start = 220, .end = 220, .line = 17, .column = 8, .file_index = 0 },
        .{ .token_type = .identifier, .start = 225, .end = 228, .line = 18, .column = 5, .file_index = 0 },
        .{ .token_type = .right_brace, .start = 230, .end = 230, .line = 19, .column = 3, .file_index = 0 },
        .{ .token_type = .number, .start = 235, .end = 237, .line = 21, .column = 3, .file_index = 0 },
        .{ .token_type = .equal_equal, .start = 238, .end = 240, .line = 21, .column = 6, .file_index = 0 },
        .{ .token_type = .number, .start = 241, .end = 243, .line = 21, .column = 9, .file_index = 0 },
        .{ .token_type = .number, .start = 245, .end = 247, .line = 22, .column = 3, .file_index = 0 },
        .{ .token_type = .bang_equal, .start = 248, .end = 250, .line = 22, .column = 6, .file_index = 0 },
        .{ .token_type = .number, .start = 251, .end = 252, .line = 22, .column = 9, .file_index = 0 },
        .{ .token_type = .string, .start = 255, .end = 258, .line = 23, .column = 4, .file_index = 0 },
        .{ .token_type = .string, .start = 262, .end = 269, .line = 24, .column = 4, .file_index = 0 },
        .{ .token_type = .left_bracket, .start = 272, .end = 272, .line = 25, .column = 3, .file_index = 0 },
        .{ .token_type = .number, .start = 273, .end = 274, .line = 25, .column = 4, .file_index = 0 },
        .{ .token_type = .comma, .start = 274, .end = 274, .line = 25, .column = 5, .file_index = 0 },
        .{ .token_type = .number, .start = 275, .end = 276, .line = 25, .column = 6, .file_index = 0 },
        .{ .token_type = .right_bracket, .start = 276, .end = 276, .line = 25, .column = 7, .file_index = 0 },
        .{ .token_type = .left_brace, .start = 279, .end = 279, .line = 26, .column = 3, .file_index = 0 },
        .{ .token_type = .string, .start = 281, .end = 284, .line = 26, .column = 5, .file_index = 0 },
        .{ .token_type = .comma, .start = 285, .end = 285, .line = 26, .column = 9, .file_index = 0 },
        .{ .token_type = .number, .start = 287, .end = 288, .line = 26, .column = 11, .file_index = 0 },
        .{ .token_type = .right_brace, .start = 288, .end = 288, .line = 26, .column = 12, .file_index = 0 },
        .{ .token_type = .class, .start = 303, .end = 308, .line = 28, .column = 3, .file_index = 0 },
        .{ .token_type = .identifier, .start = 309, .end = 320, .line = 28, .column = 9, .file_index = 0 },
        .{ .token_type = .left_brace, .start = 321, .end = 321, .line = 28, .column = 21, .file_index = 0 },
        .{ .token_type = .identifier, .start = 326, .end = 334, .line = 29, .column = 5, .file_index = 0 },
        .{ .token_type = .equal, .start = 335, .end = 335, .line = 29, .column = 14, .file_index = 0 },
        .{ .token_type = .number, .start = 337, .end = 338, .line = 29, .column = 16, .file_index = 0 },
        .{ .token_type = .comma, .start = 338, .end = 338, .line = 29, .column = 17, .file_index = 0 },
        .{ .token_type = .identifier, .start = 343, .end = 354, .line = 30, .column = 5, .file_index = 0 },
        .{ .token_type = .equal, .start = 355, .end = 355, .line = 30, .column = 17, .file_index = 0 },
        .{ .token_type = .number, .start = 357, .end = 360, .line = 30, .column = 19, .file_index = 0 },
        .{ .token_type = .right_brace, .start = 362, .end = 362, .line = 31, .column = 3, .file_index = 0 },
        .{ .token_type = .bough, .start = 377, .end = 380, .line = 33, .column = 3, .file_index = 0 },
        .{ .token_type = .identifier, .start = 381, .end = 386, .line = 33, .column = 7, .file_index = 0 },
        .{ .token_type = .left_brace, .start = 387, .end = 387, .line = 33, .column = 13, .file_index = 0 },
        .{ .token_type = .colon, .start = 392, .end = 392, .line = 34, .column = 5, .file_index = 0 },
        .{ .token_type = .identifier, .start = 393, .end = 400, .line = 34, .column = 6, .file_index = 0 },
        .{ .token_type = .colon, .start = 400, .end = 400, .line = 34, .column = 13, .file_index = 0 },
        .{ .token_type = .string, .start = 403, .end = 427, .line = 34, .column = 16, .file_index = 0 },
        .{ .token_type = .hash, .start = 431, .end = 434, .line = 34, .column = 44, .file_index = 0 },
        .{ .token_type = .right_brace, .start = 436, .end = 436, .line = 35, .column = 3, .file_index = 0 },
        .{ .token_type = .@"var", .start = 439, .end = 442, .line = 36, .column = 3, .file_index = 0 },
        .{ .token_type = .identifier, .start = 443, .end = 444, .line = 36, .column = 7, .file_index = 0 },
        .{ .token_type = .equal, .start = 445, .end = 445, .line = 36, .column = 9, .file_index = 0 },
        .{ .token_type = .new, .start = 447, .end = 450, .line = 36, .column = 11, .file_index = 0 },
        .{ .token_type = .identifier, .start = 451, .end = 462, .line = 36, .column = 15, .file_index = 0 },
        .{ .token_type = .left_brace, .start = 463, .end = 463, .line = 36, .column = 27, .file_index = 0 },
        .{ .token_type = .right_brace, .start = 464, .end = 464, .line = 36, .column = 28, .file_index = 0 },
    };

    var lexer = Lexer.init(input, 0);
    // var next = lexer.next(0);
    // while (next.token_type != .eof) : (next = lexer.next(0)) {
    //     std.debug.print("{}\n", .{next});
    // }
    for (tests) |item| {
        const current = lexer.next(0);
        try std.testing.expectEqual(item.token_type, current.token_type);
        try std.testing.expectEqual(item.start, current.start);
        try std.testing.expectEqual(item.end, current.end);
    }
}
