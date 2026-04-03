const std = @import("std");
const parser_test = @import("parser.test.zig");
const topi = @import("topi");
const Locale = topi.locale.Locale;
const LocaleProvider = topi.locale.LocaleProvider;
const C = topi.utils.C;
const UUID = topi.utils.UUID;

test "Localization Duplicate Content Gets Unique Ids" {
    const input =
        \\ === START {
        \\    :: "Hello"
        \\    :: "Hello"
        \\ }
    ;
    const mod = try parser_test.parseSource(input);
    defer mod.deinit();
    const tree = mod.entry.tree.?;
    // The bough body contains two dialogue statements
    const bough = tree.root[0].type.bough;
    const id1 = bough.body[0].type.dialogue.id;
    const id2 = bough.body[1].type.dialogue.id;
    // Both are auto-IDs (suffix zeroed) but must be distinct
    try std.testing.expect(UUID.isAuto(id1));
    try std.testing.expect(UUID.isAuto(id2));
    try std.testing.expect(!std.mem.eql(u8, &id1, &id2));
}

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
test "Localization Id Updates" {
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

    try std.testing.expectEqual(606, (try file.stat()).size);
}

const csv_output =
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

test "Localization Export CSV Tree" {
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
    try Locale.exportFile(file, &output.writer, "en");

    try std.testing.expectEqualSlices(u8, csv_output, output.written());
}

test "Localization Invalid Format" {
    const alloc = std.testing.allocator;
    // Buffer with wrong magic bytes
    const bad_magic = try alloc.dupe(u8, "BADX\x01\x00\x00\x00\x00\x00");
    try std.testing.expectError(error.InvalidLocaleFormat, LocaleProvider.init(alloc, "en", bad_magic));
    alloc.free(bad_magic);
}

test "Localization Corrupt File" {
    const alloc = std.testing.allocator;
    // Build a valid header but with an index entry whose offset+length exceeds the buffer
    var buf = std.ArrayList(u8).empty;
    defer buf.deinit(alloc);
    const w = buf.writer(alloc);
    try w.writeAll(LocaleProvider.magic); // magic
    try w.writeInt(u16, LocaleProvider.version, .little); // version
    try w.writeInt(C.CONSTANT, 1, .little); // count = 1
    try w.writeAll(&UUID.Empty); // id (17 bytes)
    try w.writeInt(C.CONSTANT, 0, .little); // offset = 0
    try w.writeInt(u32, 999, .little); // length = 999 (way past end)
    const owned = try alloc.dupe(u8, buf.items);
    try std.testing.expectError(error.CorruptLocaleFile, LocaleProvider.init(alloc, "en", owned));
    alloc.free(owned);
}

test "Localization CSV with BOM" {
    const alloc = std.testing.allocator;
    var allocating = std.Io.Writer.Allocating.init(alloc);
    const writer = &allocating.writer;
    defer allocating.deinit();

    // Prepend UTF-8 BOM to valid CSV
    const bom_csv = "\xEF\xBB\xBF" ++ csv_output;
    try Locale.generate(alloc, bom_csv, 3, writer);
    const written = try allocating.toOwnedSlice();

    const lp = try LocaleProvider.init(alloc, "en", written);
    defer lp.deinit(alloc);

    try std.testing.expectEqualSlices(u8, "A person approaches.", lp.map.get(UUID.fromString("8R955KPX-2WI5R816")).?);
}

test "Localization CSV with Embedded Newlines" {
    const alloc = std.testing.allocator;
    var allocating = std.Io.Writer.Allocating.init(alloc);
    const writer = &allocating.writer;
    defer allocating.deinit();

    const csv_with_newlines =
        \\"id","speaker","raw","en"
        \\"8R955KPX-2WI5R816","NONE","Hello.","Hello."
        \\
    ++ "\"C5I6VN71-IP0HPJHE\",\"Stranger\",\"Line one.\nLine two.\",\"Line one.\nLine two.\"\n";

    try Locale.generate(alloc, csv_with_newlines, 3, writer);
    const written = try allocating.toOwnedSlice();

    const lp = try LocaleProvider.init(alloc, "en", written);
    defer lp.deinit(alloc);

    try std.testing.expectEqualSlices(u8, "Hello.", lp.map.get(UUID.fromString("8R955KPX-2WI5R816")).?);
    try std.testing.expectEqualSlices(u8, "Line one.\nLine two.", lp.map.get(UUID.fromString("C5I6VN71-IP0HPJHE")).?);
}

test "Localization Generate and Provider" {
    const alloc = std.testing.allocator;
    var allocating = std.Io.Writer.Allocating.init(alloc);
    const writer = &allocating.writer;
    defer allocating.deinit();

    const ids = &[_][]const u8{ "8R955KPX-2WI5R816", "C5I6VN71-IP0HPJHE", "JTCCIIS7-NHTNWTBL", "8T8YW3LX-RNGWJE68", "8LIQ3QJV-5U3AJJKV", "YPTY00G5-1WX98ONH", "AEPZ4SNT-UFN9U9YW", "S6MF4G1X-34IOPNOJ", "KPTQNK2P-69OMTGXF", };
    const texts = &[_][]const u8{ "A person approaches.", "Hey there.", "Greet them.", "Oh, uh, nice to meet you. My name is Drew.", "Sorry, I thought you were someone I knew.", "I'd love to stay and chat, but this is just a short demo.", "Say nothing.", "The person acts as though they were addressing someone else.", "They walk away... Counting down from {0}", };

    try Locale.generate(alloc, csv_output, 3, writer);
    const written = try allocating.toOwnedSlice();

    const lp = try LocaleProvider.init(alloc, "en", written);
    defer lp.deinit(alloc);

    for (ids, 0..) |id, i| {
        try std.testing.expectEqualSlices(u8, lp.map.get(UUID.fromString(id)).?, texts[i]);
    }
}

// Shared topi input for merge tests (same as "Export CSV Tree" test)
const merge_topi_input =
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

test "Localization Export CSV Merge Preserves Translations" {
    const existing_csv =
        \\"id","speaker","raw","en","fr"
        \\"8R955KPX-2WI5R816","NONE","A person approaches.","A person approaches.","Une personne approche."
        \\"C5I6VN71-IP0HPJHE","Stranger","Hey there.","Hey there.","Salut."
        \\"JTCCIIS7-NHTNWTBL","CHOICE","Greet them.","Greet them.","Les saluer."
        \\"8T8YW3LX-RNGWJE68","Drew","Oh, uh, nice to meet you. My name is Drew.","Oh, uh, nice to meet you. My name is Drew.","Oh, euh, ravi de vous rencontrer."
        \\"8LIQ3QJV-5U3AJJKV","Drew","Sorry, I thought you were someone I knew.","Sorry, I thought you were someone I knew.","Desole, je pensais vous connaitre."
        \\"YPTY00G5-1WX98ONH","Drew","I'd love to stay and chat, but this is just a short demo.","I'd love to stay and chat, but this is just a short demo.","J'adorerais rester discuter."
        \\"AEPZ4SNT-UFN9U9YW","CHOICE","Say nothing.","Say nothing.","Ne rien dire."
        \\"S6MF4G1X-34IOPNOJ","NONE","The person acts as though they were addressing someone else.","The person acts as though they were addressing someone else.","La personne fait comme si."
        \\"KPTQNK2P-69OMTGXF","NONE","They walk away... Counting down from {num}","They walk away... Counting down from {0}","Ils s'eloignent... Decompte depuis {0}"
        \\
    ;

    const expected =
        \\"id","speaker","raw","en","fr"
        \\"8R955KPX-2WI5R816","NONE","A person approaches.","A person approaches.","Une personne approche."
        \\"C5I6VN71-IP0HPJHE","Stranger","Hey there.","Hey there.","Salut."
        \\"JTCCIIS7-NHTNWTBL","CHOICE","Greet them.","Greet them.","Les saluer."
        \\"8T8YW3LX-RNGWJE68","Drew","Oh, uh, nice to meet you. My name is Drew.","Oh, uh, nice to meet you. My name is Drew.","Oh, euh, ravi de vous rencontrer."
        \\"8LIQ3QJV-5U3AJJKV","Drew","Sorry, I thought you were someone I knew.","Sorry, I thought you were someone I knew.","Desole, je pensais vous connaitre."
        \\"YPTY00G5-1WX98ONH","Drew","I'd love to stay and chat, but this is just a short demo.","I'd love to stay and chat, but this is just a short demo.","J'adorerais rester discuter."
        \\"AEPZ4SNT-UFN9U9YW","CHOICE","Say nothing.","Say nothing.","Ne rien dire."
        \\"S6MF4G1X-34IOPNOJ","NONE","The person acts as though they were addressing someone else.","The person acts as though they were addressing someone else.","La personne fait comme si."
        \\"KPTQNK2P-69OMTGXF","NONE","They walk away... Counting down from {num}","They walk away... Counting down from {0}","Ils s'eloignent... Decompte depuis {0}"
        \\
    ;

    const mod = try parser_test.parseSource(merge_topi_input);
    defer mod.deinit();
    var output = std.Io.Writer.Allocating.init(std.testing.allocator);
    defer output.deinit();
    try Locale.exportFileWithMerge(mod.entry, &output.writer, "en", existing_csv, std.testing.allocator);

    try std.testing.expectEqualSlices(u8, expected, output.written());
}

test "Localization Export CSV Merge New Entries Get Empty Translations" {
    // Existing CSV only has first 2 rows — remaining should get empty fr column
    const existing_csv =
        \\"id","speaker","raw","en","fr"
        \\"8R955KPX-2WI5R816","NONE","A person approaches.","A person approaches.","Une personne approche."
        \\"C5I6VN71-IP0HPJHE","Stranger","Hey there.","Hey there.","Salut."
        \\
    ;

    const mod = try parser_test.parseSource(merge_topi_input);
    defer mod.deinit();
    var output = std.Io.Writer.Allocating.init(std.testing.allocator);
    defer output.deinit();
    try Locale.exportFileWithMerge(mod.entry, &output.writer, "en", existing_csv, std.testing.allocator);

    const result = output.written();
    // Header should have fr column
    try std.testing.expect(std.mem.startsWith(u8, result, "\"id\",\"speaker\",\"raw\",\"en\",\"fr\"\n"));
    // First row should have French translation
    try std.testing.expect(std.mem.indexOf(u8, result, "\"Une personne approche.\"") != null);
    // A new row (not in existing CSV) should have empty fr column
    try std.testing.expect(std.mem.indexOf(u8, result, "\"Greet them.\",\"\"") != null);
}

test "Localization Export CSV Merge Drops Removed Entries" {
    // Existing CSV has a row with ID not in the AST — it should be dropped
    const existing_csv =
        \\"id","speaker","raw","en","fr"
        \\"8R955KPX-2WI5R816","NONE","A person approaches.","A person approaches.","Une personne approche."
        \\"XXXXXXXXX-XXXXXXXXX","NONE","Removed line.","Removed line.","Ligne supprimee."
        \\"C5I6VN71-IP0HPJHE","Stranger","Hey there.","Hey there.","Salut."
        \\
    ;

    const mod = try parser_test.parseSource(merge_topi_input);
    defer mod.deinit();
    var output = std.Io.Writer.Allocating.init(std.testing.allocator);
    defer output.deinit();
    try Locale.exportFileWithMerge(mod.entry, &output.writer, "en", existing_csv, std.testing.allocator);

    const result = output.written();
    // Removed row should not appear
    try std.testing.expect(std.mem.indexOf(u8, result, "Removed line.") == null);
    try std.testing.expect(std.mem.indexOf(u8, result, "Ligne supprimee.") == null);
    // Existing rows should still be present with translations
    try std.testing.expect(std.mem.indexOf(u8, result, "\"Une personne approche.\"") != null);
    try std.testing.expect(std.mem.indexOf(u8, result, "\"Salut.\"") != null);
}
