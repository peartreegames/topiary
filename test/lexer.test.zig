const std = @import("std");
const topi = @import("topi");
const Lexer = topi.frontend.Lexer;
const Token = topi.frontend.Token;

test "Check supported tokens" {
    const input =
        \\ const five = 5
        \\ const two = 2
        \\ const decimal = 1.234
        \\ const sum = |x, y| {
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
        .{ .token_type = .@"const", .start = 55, .end = 60, .line = 4, .column = 3, .file_index = 0 },
        .{ .token_type = .identifier, .start = 61, .end = 64, .line = 4, .column = 9, .file_index = 0 },
        .{ .token_type = .equal, .start = 65, .end = 65, .line = 4, .column = 13, .file_index = 0 },
        .{ .token_type = .pipe, .start = 67, .end = 67, .line = 4, .column = 15, .file_index = 0 },
        .{ .token_type = .identifier, .start = 68, .end = 69, .line = 4, .column = 16, .file_index = 0 },
        .{ .token_type = .comma, .start = 69, .end = 69, .line = 4, .column = 17, .file_index = 0 },
        .{ .token_type = .identifier, .start = 71, .end = 72, .line = 4, .column = 19, .file_index = 0 },
        .{ .token_type = .pipe, .start = 72, .end = 72, .line = 4, .column = 20, .file_index = 0 },
        .{ .token_type = .left_brace, .start = 74, .end = 74, .line = 4, .column = 22, .file_index = 0 },
        .{ .token_type = .@"return", .start = 79, .end = 85, .line = 5, .column = 5, .file_index = 0 },
        .{ .token_type = .identifier, .start = 86, .end = 87, .line = 5, .column = 12, .file_index = 0 },
        .{ .token_type = .plus, .start = 88, .end = 88, .line = 5, .column = 14, .file_index = 0 },
        .{ .token_type = .identifier, .start = 90, .end = 91, .line = 5, .column = 16, .file_index = 0 },
        .{ .token_type = .right_brace, .start = 93, .end = 93, .line = 6, .column = 3, .file_index = 0 },
        .{ .token_type = .@"var", .start = 96, .end = 99, .line = 7, .column = 3, .file_index = 0 },
        .{ .token_type = .identifier, .start = 100, .end = 105, .line = 7, .column = 7, .file_index = 0 },
        .{ .token_type = .equal, .start = 106, .end = 106, .line = 7, .column = 13, .file_index = 0 },
        .{ .token_type = .identifier, .start = 108, .end = 111, .line = 7, .column = 15, .file_index = 0 },
        .{ .token_type = .left_paren, .start = 111, .end = 111, .line = 7, .column = 18, .file_index = 0 },
        .{ .token_type = .identifier, .start = 112, .end = 116, .line = 7, .column = 19, .file_index = 0 },
        .{ .token_type = .comma, .start = 116, .end = 116, .line = 7, .column = 23, .file_index = 0 },
        .{ .token_type = .identifier, .start = 118, .end = 121, .line = 7, .column = 25, .file_index = 0 },
        .{ .token_type = .right_paren, .start = 121, .end = 121, .line = 7, .column = 28, .file_index = 0 },
        .{ .token_type = .bang, .start = 124, .end = 124, .line = 8, .column = 3, .file_index = 0 },
        .{ .token_type = .minus, .start = 125, .end = 125, .line = 8, .column = 4, .file_index = 0 },
        .{ .token_type = .slash, .start = 126, .end = 126, .line = 8, .column = 5, .file_index = 0 },
        .{ .token_type = .star, .start = 127, .end = 127, .line = 8, .column = 6, .file_index = 0 },
        .{ .token_type = .number, .start = 128, .end = 129, .line = 8, .column = 7, .file_index = 0 },
        .{ .token_type = .number, .start = 131, .end = 132, .line = 9, .column = 3, .file_index = 0 },
        .{ .token_type = .less, .start = 133, .end = 133, .line = 9, .column = 5, .file_index = 0 },
        .{ .token_type = .number, .start = 135, .end = 137, .line = 9, .column = 7, .file_index = 0 },
        .{ .token_type = .greater, .start = 138, .end = 138, .line = 9, .column = 10, .file_index = 0 },
        .{ .token_type = .number, .start = 140, .end = 141, .line = 9, .column = 12, .file_index = 0 },
        .{ .token_type = .@"if", .start = 143, .end = 145, .line = 10, .column = 3, .file_index = 0 },
        .{ .token_type = .left_paren, .start = 146, .end = 146, .line = 10, .column = 6, .file_index = 0 },
        .{ .token_type = .number, .start = 147, .end = 148, .line = 10, .column = 7, .file_index = 0 },
        .{ .token_type = .less, .start = 149, .end = 149, .line = 10, .column = 9, .file_index = 0 },
        .{ .token_type = .number, .start = 151, .end = 153, .line = 10, .column = 11, .file_index = 0 },
        .{ .token_type = .right_paren, .start = 153, .end = 153, .line = 10, .column = 13, .file_index = 0 },
        .{ .token_type = .left_brace, .start = 155, .end = 155, .line = 10, .column = 15, .file_index = 0 },
        .{ .token_type = .@"return", .start = 160, .end = 166, .line = 11, .column = 5, .file_index = 0 },
        .{ .token_type = .true, .start = 167, .end = 171, .line = 11, .column = 12, .file_index = 0 },
        .{ .token_type = .right_brace, .start = 173, .end = 173, .line = 12, .column = 3, .file_index = 0 },
        .{ .token_type = .@"else", .start = 175, .end = 179, .line = 12, .column = 5, .file_index = 0 },
        .{ .token_type = .left_brace, .start = 180, .end = 180, .line = 12, .column = 10, .file_index = 0 },
        .{ .token_type = .@"return", .start = 185, .end = 191, .line = 13, .column = 5, .file_index = 0 },
        .{ .token_type = .false, .start = 192, .end = 197, .line = 13, .column = 12, .file_index = 0 },
        .{ .token_type = .right_brace, .start = 199, .end = 199, .line = 14, .column = 3, .file_index = 0 },
        .{ .token_type = .@"enum", .start = 204, .end = 208, .line = 16, .column = 3, .file_index = 0 },
        .{ .token_type = .identifier, .start = 209, .end = 217, .line = 16, .column = 8, .file_index = 0 },
        .{ .token_type = .left_brace, .start = 218, .end = 218, .line = 16, .column = 17, .file_index = 0 },
        .{ .token_type = .identifier, .start = 223, .end = 226, .line = 17, .column = 5, .file_index = 0 },
        .{ .token_type = .comma, .start = 226, .end = 226, .line = 17, .column = 8, .file_index = 0 },
        .{ .token_type = .identifier, .start = 231, .end = 234, .line = 18, .column = 5, .file_index = 0 },
        .{ .token_type = .right_brace, .start = 236, .end = 236, .line = 19, .column = 3, .file_index = 0 },
        .{ .token_type = .number, .start = 241, .end = 243, .line = 21, .column = 3, .file_index = 0 },
        .{ .token_type = .equal_equal, .start = 244, .end = 246, .line = 21, .column = 6, .file_index = 0 },
        .{ .token_type = .number, .start = 247, .end = 249, .line = 21, .column = 9, .file_index = 0 },
        .{ .token_type = .number, .start = 251, .end = 253, .line = 22, .column = 3, .file_index = 0 },
        .{ .token_type = .bang_equal, .start = 254, .end = 256, .line = 22, .column = 6, .file_index = 0 },
        .{ .token_type = .number, .start = 257, .end = 258, .line = 22, .column = 9, .file_index = 0 },
        .{ .token_type = .string, .start = 261, .end = 264, .line = 23, .column = 4, .file_index = 0 },
        .{ .token_type = .string, .start = 268, .end = 275, .line = 24, .column = 4, .file_index = 0 },
        .{ .token_type = .left_bracket, .start = 278, .end = 278, .line = 25, .column = 3, .file_index = 0 },
        .{ .token_type = .number, .start = 279, .end = 280, .line = 25, .column = 4, .file_index = 0 },
        .{ .token_type = .comma, .start = 280, .end = 280, .line = 25, .column = 5, .file_index = 0 },
        .{ .token_type = .number, .start = 281, .end = 282, .line = 25, .column = 6, .file_index = 0 },
        .{ .token_type = .right_bracket, .start = 282, .end = 282, .line = 25, .column = 7, .file_index = 0 },
        .{ .token_type = .left_brace, .start = 285, .end = 285, .line = 26, .column = 3, .file_index = 0 },
        .{ .token_type = .string, .start = 287, .end = 290, .line = 26, .column = 5, .file_index = 0 },
        .{ .token_type = .comma, .start = 291, .end = 291, .line = 26, .column = 9, .file_index = 0 },
        .{ .token_type = .number, .start = 293, .end = 294, .line = 26, .column = 11, .file_index = 0 },
        .{ .token_type = .right_brace, .start = 294, .end = 294, .line = 26, .column = 12, .file_index = 0 },
        .{ .token_type = .comment, .start = 299, .end = 307, .line = 27, .column = 13, .file_index = 0 },
        .{ .token_type = .class, .start = 309, .end = 314, .line = 28, .column = 3, .file_index = 0 },
        .{ .token_type = .identifier, .start = 315, .end = 326, .line = 28, .column = 9, .file_index = 0 },
        .{ .token_type = .left_brace, .start = 327, .end = 327, .line = 28, .column = 21, .file_index = 0 },
        .{ .token_type = .identifier, .start = 332, .end = 340, .line = 29, .column = 5, .file_index = 0 },
        .{ .token_type = .equal, .start = 341, .end = 341, .line = 29, .column = 14, .file_index = 0 },
        .{ .token_type = .number, .start = 343, .end = 344, .line = 29, .column = 16, .file_index = 0 },
        .{ .token_type = .comma, .start = 344, .end = 344, .line = 29, .column = 17, .file_index = 0 },
        .{ .token_type = .identifier, .start = 349, .end = 360, .line = 30, .column = 5, .file_index = 0 },
        .{ .token_type = .equal, .start = 361, .end = 361, .line = 30, .column = 17, .file_index = 0 },
        .{ .token_type = .number, .start = 363, .end = 366, .line = 30, .column = 19, .file_index = 0 },
        .{ .token_type = .right_brace, .start = 368, .end = 368, .line = 31, .column = 3, .file_index = 0 },
        .{ .token_type = .bough, .start = 383, .end = 386, .line = 33, .column = 3, .file_index = 0 },
        .{ .token_type = .identifier, .start = 387, .end = 392, .line = 33, .column = 7, .file_index = 0 },
        .{ .token_type = .left_brace, .start = 393, .end = 393, .line = 33, .column = 13, .file_index = 0 },
        .{ .token_type = .colon, .start = 398, .end = 398, .line = 34, .column = 5, .file_index = 0 },
        .{ .token_type = .identifier, .start = 399, .end = 406, .line = 34, .column = 6, .file_index = 0 },
        .{ .token_type = .colon, .start = 406, .end = 406, .line = 34, .column = 13, .file_index = 0 },
        .{ .token_type = .string, .start = 409, .end = 433, .line = 34, .column = 16, .file_index = 0 },
        .{ .token_type = .hash, .start = 437, .end = 440, .line = 34, .column = 44, .file_index = 0 },
        .{ .token_type = .right_brace, .start = 442, .end = 442, .line = 35, .column = 3, .file_index = 0 },
        .{ .token_type = .@"var", .start = 445, .end = 448, .line = 36, .column = 3, .file_index = 0 },
        .{ .token_type = .identifier, .start = 449, .end = 450, .line = 36, .column = 7, .file_index = 0 },
        .{ .token_type = .equal, .start = 451, .end = 451, .line = 36, .column = 9, .file_index = 0 },
        .{ .token_type = .new, .start = 453, .end = 456, .line = 36, .column = 11, .file_index = 0 },
        .{ .token_type = .identifier, .start = 457, .end = 468, .line = 36, .column = 15, .file_index = 0 },
        .{ .token_type = .left_brace, .start = 469, .end = 469, .line = 36, .column = 27, .file_index = 0 },
        .{ .token_type = .right_brace, .start = 470, .end = 470, .line = 36, .column = 28, .file_index = 0 },
    };

    var lexer = Lexer.init(input, 0);
    // var next = lexer.next();
    // while (next.token_type != .eof) : (next = lexer.next()) {
    //     std.debug.print("{}\n", .{next});
    // }
    for (tests) |item| {
        const current = lexer.next(0);
        try std.testing.expectEqual(item.token_type, current.token_type);
        try std.testing.expectEqual(item.start, current.start);
        try std.testing.expectEqual(item.end, current.end);
    }
}
