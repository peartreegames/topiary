const std = @import("std");
const topi = @import("topi");
const Lexer = topi.frontend.Lexer;
const Token = topi.frontend.Token;

test "Lexer Check supported tokens" {
    const input =
        \\ const five = 5
        \\ const two = 2
        \\ const decimal = 1.234
        \\ fn sum |x, y| {
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
        .{ .token_type = .pipe, .start = 62, .end = 62, .line = 4, .column = 10, .file_index = 0 },
        .{ .token_type = .identifier, .start = 63, .end = 64, .line = 4, .column = 11, .file_index = 0 },
        .{ .token_type = .comma, .start = 64, .end = 64, .line = 4, .column = 12, .file_index = 0 },
        .{ .token_type = .identifier, .start = 66, .end = 67, .line = 4, .column = 14, .file_index = 0 },
        .{ .token_type = .pipe, .start = 67, .end = 67, .line = 4, .column = 15, .file_index = 0 },
        .{ .token_type = .left_brace, .start = 69, .end = 69, .line = 4, .column = 17, .file_index = 0 },
        .{ .token_type = .@"return", .start = 74, .end = 80, .line = 5, .column = 5, .file_index = 0 },
        .{ .token_type = .identifier, .start = 81, .end = 82, .line = 5, .column = 12, .file_index = 0 },
        .{ .token_type = .plus, .start = 83, .end = 83, .line = 5, .column = 14, .file_index = 0 },
        .{ .token_type = .identifier, .start = 85, .end = 86, .line = 5, .column = 16, .file_index = 0 },
        .{ .token_type = .right_brace, .start = 88, .end = 88, .line = 6, .column = 3, .file_index = 0 },
        .{ .token_type = .@"var", .start = 91, .end = 94, .line = 7, .column = 3, .file_index = 0 },
        .{ .token_type = .identifier, .start = 95, .end = 100, .line = 7, .column = 7, .file_index = 0 },
        .{ .token_type = .equal, .start = 101, .end = 101, .line = 7, .column = 13, .file_index = 0 },
        .{ .token_type = .identifier, .start = 103, .end = 106, .line = 7, .column = 15, .file_index = 0 },
        .{ .token_type = .left_paren, .start = 106, .end = 106, .line = 7, .column = 18, .file_index = 0 },
        .{ .token_type = .identifier, .start = 107, .end = 111, .line = 7, .column = 19, .file_index = 0 },
        .{ .token_type = .comma, .start = 111, .end = 111, .line = 7, .column = 23, .file_index = 0 },
        .{ .token_type = .identifier, .start = 113, .end = 116, .line = 7, .column = 25, .file_index = 0 },
        .{ .token_type = .right_paren, .start = 116, .end = 116, .line = 7, .column = 28, .file_index = 0 },
        .{ .token_type = .bang, .start = 119, .end = 119, .line = 8, .column = 3, .file_index = 0 },
        .{ .token_type = .minus, .start = 120, .end = 120, .line = 8, .column = 4, .file_index = 0 },
        .{ .token_type = .slash, .start = 121, .end = 121, .line = 8, .column = 5, .file_index = 0 },
        .{ .token_type = .star, .start = 122, .end = 122, .line = 8, .column = 6, .file_index = 0 },
        .{ .token_type = .number, .start = 123, .end = 124, .line = 8, .column = 7, .file_index = 0 },
        .{ .token_type = .number, .start = 126, .end = 127, .line = 9, .column = 3, .file_index = 0 },
        .{ .token_type = .less, .start = 128, .end = 128, .line = 9, .column = 5, .file_index = 0 },
        .{ .token_type = .number, .start = 130, .end = 132, .line = 9, .column = 7, .file_index = 0 },
        .{ .token_type = .greater, .start = 133, .end = 133, .line = 9, .column = 10, .file_index = 0 },
        .{ .token_type = .number, .start = 135, .end = 136, .line = 9, .column = 12, .file_index = 0 },
        .{ .token_type = .@"if", .start = 138, .end = 140, .line = 10, .column = 3, .file_index = 0 },
        .{ .token_type = .left_paren, .start = 141, .end = 141, .line = 10, .column = 6, .file_index = 0 },
        .{ .token_type = .number, .start = 142, .end = 143, .line = 10, .column = 7, .file_index = 0 },
        .{ .token_type = .less, .start = 144, .end = 144, .line = 10, .column = 9, .file_index = 0 },
        .{ .token_type = .number, .start = 146, .end = 148, .line = 10, .column = 11, .file_index = 0 },
        .{ .token_type = .right_paren, .start = 148, .end = 148, .line = 10, .column = 13, .file_index = 0 },
        .{ .token_type = .left_brace, .start = 150, .end = 150, .line = 10, .column = 15, .file_index = 0 },
        .{ .token_type = .@"return", .start = 155, .end = 161, .line = 11, .column = 5, .file_index = 0 },
        .{ .token_type = .true, .start = 162, .end = 166, .line = 11, .column = 12, .file_index = 0 },
        .{ .token_type = .right_brace, .start = 168, .end = 168, .line = 12, .column = 3, .file_index = 0 },
        .{ .token_type = .@"else", .start = 170, .end = 174, .line = 12, .column = 5, .file_index = 0 },
        .{ .token_type = .left_brace, .start = 175, .end = 175, .line = 12, .column = 10, .file_index = 0 },
        .{ .token_type = .@"return", .start = 180, .end = 186, .line = 13, .column = 5, .file_index = 0 },
        .{ .token_type = .false, .start = 187, .end = 192, .line = 13, .column = 12, .file_index = 0 },
        .{ .token_type = .right_brace, .start = 194, .end = 194, .line = 14, .column = 3, .file_index = 0 },
        .{ .token_type = .@"enum", .start = 199, .end = 203, .line = 16, .column = 3, .file_index = 0 },
        .{ .token_type = .identifier, .start = 204, .end = 212, .line = 16, .column = 8, .file_index = 0 },
        .{ .token_type = .left_brace, .start = 213, .end = 213, .line = 16, .column = 17, .file_index = 0 },
        .{ .token_type = .identifier, .start = 218, .end = 221, .line = 17, .column = 5, .file_index = 0 },
        .{ .token_type = .comma, .start = 221, .end = 221, .line = 17, .column = 8, .file_index = 0 },
        .{ .token_type = .identifier, .start = 226, .end = 229, .line = 18, .column = 5, .file_index = 0 },
        .{ .token_type = .right_brace, .start = 231, .end = 231, .line = 19, .column = 3, .file_index = 0 },
        .{ .token_type = .number, .start = 236, .end = 238, .line = 21, .column = 3, .file_index = 0 },
        .{ .token_type = .equal_equal, .start = 239, .end = 241, .line = 21, .column = 6, .file_index = 0 },
        .{ .token_type = .number, .start = 242, .end = 244, .line = 21, .column = 9, .file_index = 0 },
        .{ .token_type = .number, .start = 246, .end = 248, .line = 22, .column = 3, .file_index = 0 },
        .{ .token_type = .bang_equal, .start = 249, .end = 251, .line = 22, .column = 6, .file_index = 0 },
        .{ .token_type = .number, .start = 252, .end = 253, .line = 22, .column = 9, .file_index = 0 },
        .{ .token_type = .string, .start = 256, .end = 259, .line = 23, .column = 4, .file_index = 0 },
        .{ .token_type = .string, .start = 263, .end = 270, .line = 24, .column = 4, .file_index = 0 },
        .{ .token_type = .left_bracket, .start = 273, .end = 273, .line = 25, .column = 3, .file_index = 0 },
        .{ .token_type = .number, .start = 274, .end = 275, .line = 25, .column = 4, .file_index = 0 },
        .{ .token_type = .comma, .start = 275, .end = 275, .line = 25, .column = 5, .file_index = 0 },
        .{ .token_type = .number, .start = 276, .end = 277, .line = 25, .column = 6, .file_index = 0 },
        .{ .token_type = .right_bracket, .start = 277, .end = 277, .line = 25, .column = 7, .file_index = 0 },
        .{ .token_type = .left_brace, .start = 280, .end = 280, .line = 26, .column = 3, .file_index = 0 },
        .{ .token_type = .string, .start = 282, .end = 285, .line = 26, .column = 5, .file_index = 0 },
        .{ .token_type = .comma, .start = 286, .end = 286, .line = 26, .column = 9, .file_index = 0 },
        .{ .token_type = .number, .start = 288, .end = 289, .line = 26, .column = 11, .file_index = 0 },
        .{ .token_type = .right_brace, .start = 289, .end = 289, .line = 26, .column = 12, .file_index = 0 },
        .{ .token_type = .class, .start = 304, .end = 309, .line = 28, .column = 3, .file_index = 0 },
        .{ .token_type = .identifier, .start = 310, .end = 321, .line = 28, .column = 9, .file_index = 0 },
        .{ .token_type = .left_brace, .start = 322, .end = 322, .line = 28, .column = 21, .file_index = 0 },
        .{ .token_type = .identifier, .start = 327, .end = 335, .line = 29, .column = 5, .file_index = 0 },
        .{ .token_type = .equal, .start = 336, .end = 336, .line = 29, .column = 14, .file_index = 0 },
        .{ .token_type = .number, .start = 338, .end = 339, .line = 29, .column = 16, .file_index = 0 },
        .{ .token_type = .comma, .start = 339, .end = 339, .line = 29, .column = 17, .file_index = 0 },
        .{ .token_type = .identifier, .start = 344, .end = 355, .line = 30, .column = 5, .file_index = 0 },
        .{ .token_type = .equal, .start = 356, .end = 356, .line = 30, .column = 17, .file_index = 0 },
        .{ .token_type = .number, .start = 358, .end = 361, .line = 30, .column = 19, .file_index = 0 },
        .{ .token_type = .right_brace, .start = 363, .end = 363, .line = 31, .column = 3, .file_index = 0 },
        .{ .token_type = .bough, .start = 378, .end = 381, .line = 33, .column = 3, .file_index = 0 },
        .{ .token_type = .identifier, .start = 382, .end = 387, .line = 33, .column = 7, .file_index = 0 },
        .{ .token_type = .left_brace, .start = 388, .end = 388, .line = 33, .column = 13, .file_index = 0 },
        .{ .token_type = .colon, .start = 393, .end = 393, .line = 34, .column = 5, .file_index = 0 },
        .{ .token_type = .identifier, .start = 394, .end = 401, .line = 34, .column = 6, .file_index = 0 },
        .{ .token_type = .colon, .start = 401, .end = 401, .line = 34, .column = 13, .file_index = 0 },
        .{ .token_type = .string, .start = 404, .end = 428, .line = 34, .column = 16, .file_index = 0 },
        .{ .token_type = .hash, .start = 432, .end = 435, .line = 34, .column = 44, .file_index = 0 },
        .{ .token_type = .right_brace, .start = 437, .end = 437, .line = 35, .column = 3, .file_index = 0 },
        .{ .token_type = .@"var", .start = 440, .end = 443, .line = 36, .column = 3, .file_index = 0 },
        .{ .token_type = .identifier, .start = 444, .end = 445, .line = 36, .column = 7, .file_index = 0 },
        .{ .token_type = .equal, .start = 446, .end = 446, .line = 36, .column = 9, .file_index = 0 },
        .{ .token_type = .new, .start = 448, .end = 451, .line = 36, .column = 11, .file_index = 0 },
        .{ .token_type = .identifier, .start = 452, .end = 463, .line = 36, .column = 15, .file_index = 0 },
        .{ .token_type = .left_brace, .start = 464, .end = 464, .line = 36, .column = 27, .file_index = 0 },
        .{ .token_type = .right_brace, .start = 465, .end = 465, .line = 36, .column = 28, .file_index = 0 },
    };

    var lexer = Lexer.init(input, 0);
    // var next = lexer.next(0);
    // while (next.token_type != .eof) : (next = lexer.next(0)) {
    //     std.debug.print("{},\n", .{next});
    // }
    for (tests) |item| {
        const current = lexer.next(0);
        try std.testing.expectEqual(item.token_type, current.token_type);
        try std.testing.expectEqual(item.start, current.start);
        try std.testing.expectEqual(item.end, current.end);
    }
}
