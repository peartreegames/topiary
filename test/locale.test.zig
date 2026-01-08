const std = @import("std");
const parser_test = @import("parser.test.zig");
const topi = @import("topi");
const Locale = topi.locale.Locale;

test "Localization Ids" {
    const input =
        \\ const str = "testing"
        \\ var added = "one" + "two"
        \\ const alreadySet = "set"
        \\ === START {
        \\    :Speaker: "Dialogue content without id"
        \\    :Speaker: "More dialogue with id"@JUWH59VY-LRIGSPSB
        \\    fork {
        \\        ~ "Choice without id" => END
        \\    }
        \\    :Speaker: "Interpolated values {if str == "testing" "then value" else "else value"}"
        \\    :Speaker: "Partial id malformed"@1234
        \\    :: "Missing ID"@
        \\ }
    ;

    const mod = try parser_test.parseSource(input);
    defer mod.deinit();
    const file = mod.entry;
    const update = try Locale.validateFile(file, std.testing.allocator);
    defer std.testing.allocator.free(update);
}
test "Update File Localization Ids" {
    const input =
        \\ const str = "testing"
        \\ var added = "one" + "two"
        \\ const alreadySet = "set"
        \\ === START {
        \\    :Speaker: "Dialogue content withhout id"
        \\    :Speaker: "More dialogue with id"@JUWH59VY-LRIGSPSB
        \\    fork {
        \\        ~ "Choice without id" => END
        \\        ~ "Choice two" {
        \\            :Speaker: "Inner dialogue of choice"    
        \\        }
        \\    }
        \\    :Speaker: "Interpolated values {if str == "testing" "then value" else "else value"}"
        \\    :Speaker: "Partial id malformed"@1234
        \\    :: "Missing ID"@
        \\ }
    ;

    const file_name = "test_locale.topi";
    var file = try std.fs.cwd().createFile(file_name, .{ .read = true });
    defer std.fs.cwd().deleteFile(file_name) catch {};
    defer file.close();
    try file.writeAll(input);
    const full_path = try std.fs.cwd().realpathAlloc(std.testing.allocator, file_name);
    defer std.testing.allocator.free(full_path);
    const validated = try Locale.validateFileAtPath(full_path, std.testing.allocator);
    defer std.testing.allocator.free(validated);
    try file.seekTo(0);
    try file.writeAll(validated);

    try std.testing.expectEqual(606,(try file.stat()).size);
}

test "Export Localization CSV Tree" {
    const input =
        \\ const num = 15
        \\ === START {
        \\     :: "A person approaches."@8R955KPX-2WI5R816
        \\     :Stranger: "Hey there."@C5I6VN71-IP0HPJHE
        \\     fork^ {
        \\         ~ "Greet them."@JTCCIIS7-NHTNWTBL {
        \\             :Drew: "Oh, uh, nice to meet you. My name is Drew."@8T8YW3LX-RNGWJE68
        \\             :Drew: "Sorry, I thought you were someone I knew."@8LIQ3QJV-5U3AJJKV
        \\             :Drew: "I'd love to stay and chat, but this is just a short demo."@YPTY00G5-1WX98ONH
        \\         }
        \\         ~ "Say nothing."@AEPZ4SNT-UFN9U9YW {
        \\             :: "The person acts as though they were addressing someone else."@S6MF4G1X-34IOPNOJ
        \\         }
        \\     }
        \\     :: "They walk away... Counting down from {num}"@KPTQNK2P-69OMTGXF
        \\ }
    ;

    const mod = try parser_test.parseSource(input);
    defer mod.deinit();
    const file = mod.entry;
    var output = std.Io.Writer.Allocating.init(std.testing.allocator);
    defer output.deinit();
    try Locale.exportFile(file, &output.writer);

    const expected =
        \\"id","speaker","raw","en"
        \\"8R955KPX-2WI5R816","NONE","A person approaches.","A person approaches."
        \\"C5I6VN71-IP0HPJHE","Stranger","Hey there.","Hey there."
        \\"JTCCIIS7-NHTNWTBL","CHOICE","Greet them.","Greet them."
        \\"8T8YW3LX-RNGWJE68","Drew","Oh, uh, nice to meet you. My name is Drew.","Oh, uh, nice to meet you. My name is Drew."
        \\"8LIQ3QJV-5U3AJJKV","Drew","Sorry, I thought you were someone I knew.","Sorry, I thought you were someone I knew."
        \\"YPTY00G5-1WX98ONH","Drew","I'd love to stay and chat, but this is just a short demo.","I'd love to stay and chat, but this is just a short demo."
        \\"AEPZ4SNT-UFN9U9YW","CHOICE","Say nothing.","Say nothing."
        \\"S6MF4G1X-34IOPNOJ","NONE","The person acts as though they were addressing someone else.","The person acts as though they were addressing someone else."
        \\"KPTQNK2P-69OMTGXF","NONE","They walk away... Counting down from {num}","They walk away... Counting down from {0}"
        \\
        ;
    try std.testing.expectEqualSlices(u8, expected, output.written());
}
