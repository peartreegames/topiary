const std = @import("std");
const parser_test = @import("parser.test.zig");
const topi = @import("topi");
const Locale = topi.locale.Locale;
const LocaleProvider = topi.locale.LocaleProvider;
const C = topi.utils.C;
const UUID = topi.utils.UUID;
const Vm = topi.runtime.Vm;
const Module = topi.module.Module;

const compileSource = @import("compiler.test.zig").compileSource;
const TestRunner = @import("runner.zig").TestRunner;

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
    // Both are auto-IDs (no id_token) but must be distinct
    try std.testing.expect(bough.body[0].type.dialogue.id_token == null);
    try std.testing.expect(bough.body[1].type.dialogue.id_token == null);
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
    const test_io = std.testing.io;
    const cwd = std.Io.Dir.cwd();
    {
        const file = try cwd.createFile(test_io, file_name, .{});
        defer file.close(test_io);
        try file.writeStreamingAll(test_io, input);
    }
    defer cwd.deleteFile(test_io, file_name) catch {};
    const full_path = try cwd.realPathFileAlloc(test_io, file_name, std.testing.allocator);
    defer std.testing.allocator.free(full_path);
    const validated = try Locale.validateFileAtPath(full_path, std.testing.allocator);
    defer std.testing.allocator.free(validated);
    {
        const file = try cwd.createFile(test_io, file_name, .{});
        defer file.close(test_io);
        try file.writeStreamingAll(test_io, validated);
    }
    {
        const file = try cwd.openFile(test_io, file_name, .{});
        defer file.close(test_io);
        try std.testing.expectEqual(606, (try file.stat(test_io)).size);
    }
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
    \\"KPTQNK2P-69OMTGXF","NONE","They walk away... Counting down from {num | 0}","They walk away... Counting down from {0}"
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
    try Locale.exportFile(file, &output.writer, "en", std.testing.allocator);

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
    var output: std.Io.Writer.Allocating = .init(alloc);
    defer output.deinit();
    const w = &output.writer;
    try w.writeAll(LocaleProvider.magic); // magic
    try w.writeInt(u16, LocaleProvider.version, .little); // version
    try w.writeInt(C.CONSTANT, 1, .little); // count = 1
    try w.writeAll(&UUID.Empty); // id (17 bytes)
    try w.writeInt(C.CONSTANT, 0, .little); // offset = 0
    try w.writeInt(u32, 999, .little); // length = 999 (way past end)
    const owned = try alloc.dupe(u8, output.written());
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
    try Locale.generate(alloc, bom_csv, 3, writer, null);
    const written = try allocating.toOwnedSlice();

    const lp = try LocaleProvider.init(alloc, "en", written);
    defer lp.deinit(alloc);

    try std.testing.expectEqualSlices(u8, "A person approaches.", lp.map.get(UUID.fromString("8R955KPX-2WI5R816")).?.bytes);
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

    try Locale.generate(alloc, csv_with_newlines, 3, writer, null);
    const written = try allocating.toOwnedSlice();

    const lp = try LocaleProvider.init(alloc, "en", written);
    defer lp.deinit(alloc);

    try std.testing.expectEqualSlices(u8, "Hello.", lp.map.get(UUID.fromString("8R955KPX-2WI5R816")).?.bytes);
    try std.testing.expectEqualSlices(u8, "Line one.\nLine two.", lp.map.get(UUID.fromString("C5I6VN71-IP0HPJHE")).?.bytes);
}

test "Localization Generate and Provider" {
    const alloc = std.testing.allocator;
    var allocating = std.Io.Writer.Allocating.init(alloc);
    const writer = &allocating.writer;
    defer allocating.deinit();

    const ids = &[_][]const u8{ "8R955KPX-2WI5R816", "C5I6VN71-IP0HPJHE", "JTCCIIS7-NHTNWTBL", "8T8YW3LX-RNGWJE68", "8LIQ3QJV-5U3AJJKV", "YPTY00G5-1WX98ONH", "AEPZ4SNT-UFN9U9YW", "S6MF4G1X-34IOPNOJ", "KPTQNK2P-69OMTGXF", };
    const texts = &[_][]const u8{ "A person approaches.", "Hey there.", "Greet them.", "Oh, uh, nice to meet you. My name is Drew.", "Sorry, I thought you were someone I knew.", "I'd love to stay and chat, but this is just a short demo.", "Say nothing.", "The person acts as though they were addressing someone else.", "They walk away... Counting down from {0}", };

    try Locale.generate(alloc, csv_output, 3, writer, null);
    const written = try allocating.toOwnedSlice();

    const lp = try LocaleProvider.init(alloc, "en", written);
    defer lp.deinit(alloc);

    for (ids, 0..) |id, i| {
        try std.testing.expectEqualSlices(u8, lp.map.get(UUID.fromString(id)).?.bytes, texts[i]);
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
        \\"KPTQNK2P-69OMTGXF","NONE","They walk away... Counting down from {num | 0}","They walk away... Counting down from {0}","Ils s'eloignent... Decompte depuis {0}"
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

test "Localization Unsupported Version" {
    const alloc = std.testing.allocator;
    var output: std.Io.Writer.Allocating = .init(alloc);
    defer output.deinit();
    const w = &output.writer;
    try w.writeAll(LocaleProvider.magic);
    try w.writeInt(u16, 99, .little); // unsupported version
    try w.writeInt(C.CONSTANT, 0, .little); // count = 0
    const owned = try alloc.dupe(u8, output.written());
    try std.testing.expectError(error.UnsupportedLocaleVersion, LocaleProvider.init(alloc, "en", owned));
    alloc.free(owned);
}

test "Localization Overflow Count" {
    const alloc = std.testing.allocator;
    var output: std.Io.Writer.Allocating = .init(alloc);
    defer output.deinit();
    const w = &output.writer;
    try w.writeAll(LocaleProvider.magic);
    try w.writeInt(u16, LocaleProvider.version, .little);
    try w.writeInt(C.CONSTANT, std.math.maxInt(C.CONSTANT), .little); // huge count
    try w.writeInt(u32, 0, .little); // string_blob_size — must be present in v2 header
    const owned = try alloc.dupe(u8, output.written());
    try std.testing.expectError(error.CorruptLocaleFile, LocaleProvider.init(alloc, "en", owned));
    alloc.free(owned);
}

test "Localization Header Only CSV" {
    const alloc = std.testing.allocator;
    var allocating = std.Io.Writer.Allocating.init(alloc);
    const writer = &allocating.writer;
    defer allocating.deinit();

    const header_only_csv =
        \\"id","speaker","raw","en"
        \\
    ;

    try Locale.generate(alloc, header_only_csv, 3, writer, null);
    const written = try allocating.toOwnedSlice();

    const lp = try LocaleProvider.init(alloc, "en", written);
    defer lp.deinit(alloc);

    // Should have zero entries
    try std.testing.expectEqual(@as(usize, 0), lp.map.count());
}

test "Localization UUID Validation" {
    // Valid UUID
    const valid = UUID.fromString("8R955KPX-2WI5R816");
    try std.testing.expect(!UUID.isEmpty(valid));

    // Invalid: lowercase letters
    const lowercase = UUID.fromString("8r955kpx-2wi5r816");
    try std.testing.expect(UUID.isEmpty(lowercase));

    // Invalid: missing dash
    const no_dash = UUID.fromString("8R955KPXA2WI5R816");
    try std.testing.expect(UUID.isEmpty(no_dash));

    // Invalid: special characters
    const special = UUID.fromString("8R955KP!-2WI5R816");
    try std.testing.expect(UUID.isEmpty(special));

    // Invalid: wrong length
    const short = UUID.fromString("8R955KPX-2WI5R81");
    try std.testing.expect(UUID.isEmpty(short));
}

test "Localization VM Deinit Cleans Up Locale" {
    const alloc = std.testing.allocator;

    // Generate a valid .topil buffer
    var allocating = std.Io.Writer.Allocating.init(alloc);
    const writer = &allocating.writer;
    defer allocating.deinit();
    try Locale.generate(alloc, csv_output, 3, writer, null);
    const topil_data = try allocating.toOwnedSlice();
    defer alloc.free(topil_data);

    // Compile a minimal topi source and create a VM
    var mod = try Module.initEmpty(alloc, std.testing.io);
    defer mod.deinit();
    var bytecode = try compileSource("const x = 1", mod);
    defer bytecode.free(alloc);
    const test_runner = try TestRunner.init(alloc);
    var vm = try Vm.init(alloc, std.testing.io, &bytecode, &test_runner.runner);
    defer vm.deinit();
    defer test_runner.deinit();

    // Set locale from buffer — VM.deinit should clean this up without leaks
    try vm.setLocaleFromBuffer("test", topil_data);
    try std.testing.expect(vm.loc_provider != null);

    // deinit happens via defer — testing allocator will catch any leaks
}

const TestFile = struct { name: []const u8, content: []const u8 };

fn setupModuleTestFiles(dir: std.Io.Dir, files: []const TestFile) !void {
    const test_io = std.testing.io;
    for (files) |f| {
        if (std.fs.path.dirname(f.name)) |sub_dir| {
            try dir.createDirPath(test_io, sub_dir);
        }
        const file = try dir.createFile(test_io, f.name, .{});
        defer file.close(test_io);
        try file.writeStreamingAll(test_io, f.content);
    }
}

fn cleanupModuleTestFiles(dir: std.Io.Dir, files: []const TestFile) void {
    const test_io = std.testing.io;
    var i = files.len;
    while (i > 0) {
        i -= 1;
        dir.deleteFile(test_io, files[i].name) catch {};
    }
}

test "Localization Generate From Module Single File" {
    const alloc = std.testing.allocator;
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    const topi_content =
        \\=== START {
        \\    :: "Hello world."@AAAAAAAA-BBBBBBBB
        \\    :: "Goodbye."@CCCCCCCC-DDDDDDDD
        \\}
    ;
    const csv_content =
        \\"id","speaker","raw","en","fr"
        \\"AAAAAAAA-BBBBBBBB","NONE","Hello world.","Hello world.","Bonjour le monde."
        \\"CCCCCCCC-DDDDDDDD","NONE","Goodbye.","Goodbye.","Au revoir."
        \\
    ;

    const test_files = &[_]TestFile{
        .{ .name = "story.topi", .content = topi_content },
        .{ .name = "story.topi.csv", .content = csv_content },
    };
    try setupModuleTestFiles(tmp_dir.dir, test_files);
    defer cleanupModuleTestFiles(tmp_dir.dir, test_files);

    const test_io = std.testing.io;
    const full_path = try tmp_dir.dir.realPathFileAlloc(test_io, "story.topi", alloc);
    defer alloc.free(full_path);
    const folder = try tmp_dir.dir.realPathFileAlloc(test_io, ".", alloc);
    defer alloc.free(folder);

    var gen_result = try Locale.generateFromModuleWithIo(alloc, std.testing.io, full_path, folder, null, false, null);
    defer gen_result.deinit();

    try std.testing.expect(!gen_result.hasWarnings());

    // Verify generated .topil file
    const topil_file = try tmp_dir.dir.openFile(test_io, "story.fr.topil", .{});
    defer topil_file.close(test_io);
    var topil_buf: [1024]u8 = undefined;
    var topil_rdr = topil_file.reader(test_io, &topil_buf);
    const topil_data = try topil_rdr.interface.allocRemaining(alloc, .unlimited);
    const lp = try LocaleProvider.init(alloc, "fr", topil_data);
    defer lp.deinit(alloc);

    try std.testing.expectEqualSlices(u8, "Bonjour le monde.", lp.map.get(UUID.fromString("AAAAAAAA-BBBBBBBB")).?.bytes);
    try std.testing.expectEqualSlices(u8, "Au revoir.", lp.map.get(UUID.fromString("CCCCCCCC-DDDDDDDD")).?.bytes);
}

test "Localization Generate From Module With Include" {
    const alloc = std.testing.allocator;
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    const main_topi =
        \\include "./shared.topi"
        \\=== START {
        \\    :: "Main dialogue."@AAAAAAAA-BBBBBBBB
        \\}
    ;
    const shared_topi =
        \\=== SHARED {
        \\    :: "Shared dialogue."@CCCCCCCC-DDDDDDDD
        \\}
    ;
    const main_csv =
        \\"id","speaker","raw","en","fr"
        \\"AAAAAAAA-BBBBBBBB","NONE","Main dialogue.","Main dialogue.","Dialogue principal."
        \\
    ;
    const shared_csv =
        \\"id","speaker","raw","en","fr"
        \\"CCCCCCCC-DDDDDDDD","NONE","Shared dialogue.","Shared dialogue.","Dialogue partage."
        \\
    ;

    const test_files = &[_]TestFile{
        .{ .name = "main.topi", .content = main_topi },
        .{ .name = "shared.topi", .content = shared_topi },
        .{ .name = "main.topi.csv", .content = main_csv },
        .{ .name = "shared.topi.csv", .content = shared_csv },
    };
    try setupModuleTestFiles(tmp_dir.dir, test_files);
    defer cleanupModuleTestFiles(tmp_dir.dir, test_files);

    const full_path = try tmp_dir.dir.realPathFileAlloc(std.testing.io, "main.topi", alloc);
    defer alloc.free(full_path);
    const folder = try tmp_dir.dir.realPathFileAlloc(std.testing.io, ".", alloc);
    defer alloc.free(folder);

    var gen_result = try Locale.generateFromModuleWithIo(alloc, std.testing.io, full_path, folder, null, false, null);
    defer gen_result.deinit();

    try std.testing.expect(!gen_result.hasWarnings());

    // Verify .topil has entries from both files
    const topil_file = try tmp_dir.dir.openFile(std.testing.io, "main.fr.topil", .{});
    defer topil_file.close(std.testing.io);
    var topil_buf2: [1024]u8 = undefined;
    var topil_rdr2 = topil_file.reader(std.testing.io, &topil_buf2);
    const topil_data = try topil_rdr2.interface.allocRemaining(alloc, .unlimited);
    const lp = try LocaleProvider.init(alloc, "fr", topil_data);
    defer lp.deinit(alloc);

    try std.testing.expectEqualSlices(u8, "Dialogue principal.", lp.map.get(UUID.fromString("AAAAAAAA-BBBBBBBB")).?.bytes);
    try std.testing.expectEqualSlices(u8, "Dialogue partage.", lp.map.get(UUID.fromString("CCCCCCCC-DDDDDDDD")).?.bytes);
}

test "Localization Generate From Module Missing CSV Warning" {
    const alloc = std.testing.allocator;
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    const main_topi =
        \\include "./shared.topi"
        \\=== START {
        \\    :: "Main dialogue."@AAAAAAAA-BBBBBBBB
        \\}
    ;
    const shared_topi =
        \\=== SHARED {
        \\    :: "Shared dialogue."@CCCCCCCC-DDDDDDDD
        \\}
    ;
    const main_csv =
        \\"id","speaker","raw","en","fr"
        \\"AAAAAAAA-BBBBBBBB","NONE","Main dialogue.","Main dialogue.","Dialogue principal."
        \\
    ;

    // No shared.topi.csv — should produce a warning
    const test_files = &[_]TestFile{
        .{ .name = "main.topi", .content = main_topi },
        .{ .name = "shared.topi", .content = shared_topi },
        .{ .name = "main.topi.csv", .content = main_csv },
    };
    try setupModuleTestFiles(tmp_dir.dir, test_files);
    defer cleanupModuleTestFiles(tmp_dir.dir, test_files);

    const full_path = try tmp_dir.dir.realPathFileAlloc(std.testing.io, "main.topi", alloc);
    defer alloc.free(full_path);
    const folder = try tmp_dir.dir.realPathFileAlloc(std.testing.io, ".", alloc);
    defer alloc.free(folder);

    var gen_result = try Locale.generateFromModuleWithIo(alloc, std.testing.io, full_path, folder, null, false, null);
    defer gen_result.deinit();

    // Should warn about missing CSV for shared.topi
    try std.testing.expectEqual(@as(usize, 1), gen_result.missing_csv_files.items.len);
    try std.testing.expectEqualSlices(u8, "shared.topi", gen_result.missing_csv_files.items[0]);

    // Should report the shared UUID as missing from CSV
    try std.testing.expectEqual(@as(usize, 1), gen_result.missing_uuids.items.len);
}

test "Localization Generate From Module No CSV For Utility Include" {
    const alloc = std.testing.allocator;
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    const main_topi =
        \\include "./utils.topi"
        \\=== START {
        \\    :: "Main dialogue."@AAAAAAAA-BBBBBBBB
        \\}
    ;
    // Utility file with no dialogue or choices
    const utils_topi =
        \\const helper = 42
    ;
    const main_csv =
        \\"id","speaker","raw","en","fr"
        \\"AAAAAAAA-BBBBBBBB","NONE","Main dialogue.","Main dialogue.","Dialogue principal."
        \\
    ;

    const test_files = &[_]TestFile{
        .{ .name = "main.topi", .content = main_topi },
        .{ .name = "utils.topi", .content = utils_topi },
        .{ .name = "main.topi.csv", .content = main_csv },
    };
    try setupModuleTestFiles(tmp_dir.dir, test_files);
    defer cleanupModuleTestFiles(tmp_dir.dir, test_files);

    const full_path = try tmp_dir.dir.realPathFileAlloc(std.testing.io, "main.topi", alloc);
    defer alloc.free(full_path);
    const folder = try tmp_dir.dir.realPathFileAlloc(std.testing.io, ".", alloc);
    defer alloc.free(folder);

    var gen_result = try Locale.generateFromModuleWithIo(alloc, std.testing.io, full_path, folder, null, false, null);
    defer gen_result.deinit();

    // No warnings — utils.topi has no localizable content, so missing CSV is fine
    try std.testing.expect(!gen_result.hasWarnings());
}

test "Localization Generate From Module Stale CSV Entries" {
    const alloc = std.testing.allocator;
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    // Source only has one dialogue line
    const topi_content =
        \\=== START {
        \\    :: "Hello world."@AAAAAAAA-BBBBBBBB
        \\}
    ;
    // CSV has an extra entry that's no longer in source
    const csv_content =
        \\"id","speaker","raw","en","fr"
        \\"AAAAAAAA-BBBBBBBB","NONE","Hello world.","Hello world.","Bonjour le monde."
        \\"XXXXXXXX-YYYYYYYY","NONE","Removed line.","Removed line.","Ligne supprimee."
        \\
    ;

    const test_files = &[_]TestFile{
        .{ .name = "story.topi", .content = topi_content },
        .{ .name = "story.topi.csv", .content = csv_content },
    };
    try setupModuleTestFiles(tmp_dir.dir, test_files);
    defer cleanupModuleTestFiles(tmp_dir.dir, test_files);

    const full_path = try tmp_dir.dir.realPathFileAlloc(std.testing.io, "story.topi", alloc);
    defer alloc.free(full_path);
    const folder = try tmp_dir.dir.realPathFileAlloc(std.testing.io, ".", alloc);
    defer alloc.free(folder);

    var gen_result = try Locale.generateFromModuleWithIo(alloc, std.testing.io, full_path, folder, null, false, null);
    defer gen_result.deinit();

    // Should report the stale UUID
    try std.testing.expectEqual(@as(usize, 0), gen_result.missing_uuids.items.len);
    try std.testing.expectEqual(@as(usize, 1), gen_result.extra_uuids.items.len);
}

test "Localization Runtime Translates Tagged Dialogue" {
    // Regression guard for the v3 .loc collapse: the unified .string
    // opcode must consult the locale provider when the constant carries
    // a non-empty UUID, and skip it when the UUID is empty. The dialogue
    // line below carries an explicit @id and gets translated; the
    // non-dialogue interpolated string assigned to `greet` does not.
    const alloc = std.testing.allocator;

    const csv =
        \\"id","speaker","raw","en"
        \\"AAAAAAAA-BBBBBBBB","Sp","Hello.","Bonjour."
        \\
    ;
    var allocating = std.Io.Writer.Allocating.init(alloc);
    defer allocating.deinit();
    try Locale.generate(alloc, csv, 3, &allocating.writer, null);
    const topil = try allocating.toOwnedSlice();
    defer alloc.free(topil);

    const source =
        \\ var greet = "ignored {1 + 2}"
        \\ === START {
        \\     :Sp: "Hello."@AAAAAAAA-BBBBBBBB
        \\ }
    ;

    var mod = try Module.initEmpty(alloc, std.testing.io);
    defer mod.deinit();
    var bytecode = try compileSource(source, mod);
    defer bytecode.free(alloc);
    const test_runner = try TestRunner.init(alloc);
    defer test_runner.deinit();
    var vm = try Vm.init(alloc, std.testing.io, &bytecode, &test_runner.runner);
    defer vm.deinit();

    try vm.setLocaleFromBuffer("en", topil);
    try vm.interpret();
    try test_runner.expectOutput(&[_][]const u8{"Bonjour."});

    // The non-dialogue interpolated string was emitted with id=Empty and
    // must surface verbatim — no locale lookup, even though the provider
    // is set. If the v3 collapse mistakenly forwarded `compileText` to a
    // path that reads `Obj.id` of empty as a hit, this would change.
    const greet_idx = try vm.getGlobalsIndex("greet");
    const greet_str = vm.globals[greet_idx].asString() orelse return error.NotAString;
    try std.testing.expectEqualStrings("ignored 3", greet_str);
}

test "Localization Raw Column Dedups Repeated Identifiers" {
    // Both `{bob}` references should share index 0 — the translator only
    // needs `{0}` and a single arg slot.
    const input =
        \\ === START {
        \\     :: "Hi {bob}, are you {bob}?"@AAAAAAAA-BBBBBBBB
        \\ }
    ;
    const expected =
        \\"id","speaker","raw","en"
        \\"AAAAAAAA-BBBBBBBB","NONE","Hi {bob | 0}, are you {bob | 0}?","Hi {0}, are you {1}?"
        \\
    ;

    const mod = try parser_test.parseSource(input);
    defer mod.deinit();
    var output = std.Io.Writer.Allocating.init(std.testing.allocator);
    defer output.deinit();
    try Locale.exportFile(mod.entry, &output.writer, "en", std.testing.allocator);
    try std.testing.expectEqualSlices(u8, expected, output.written());
}

test "Localization Translator Index Out Of Range" {
    // Raw has 1 interp ({n}), so translator may only use {0}. {1} must
    // be rejected by `generate` before the .topil is written.
    const alloc = std.testing.allocator;
    const csv =
        \\"id","speaker","raw","en"
        \\"AAAAAAAA-BBBBBBBB","NONE","Hello {n}.","Bad {1}."
        \\
    ;
    var allocating = std.Io.Writer.Allocating.init(alloc);
    defer allocating.deinit();
    try std.testing.expectError(
        error.TranslatorIndexOutOfRange,
        Locale.generate(alloc, csv, 3, &allocating.writer, null),
    );
}

test "Localization Translator Strips Echoed Name" {
    // A translator that copy-pasted the source format `{name | 0}` should
    // still work — the importer parses the trailing integer and ignores
    // the name.
    const alloc = std.testing.allocator;
    const csv =
        \\"id","speaker","raw","en"
        \\"AAAAAAAA-BBBBBBBB","NONE","Hi {name | 0}!","Bonjour {name | 0}!"
        \\
    ;
    var allocating = std.Io.Writer.Allocating.init(alloc);
    defer allocating.deinit();
    try Locale.generate(alloc, csv, 3, &allocating.writer, null);
    const topil = try allocating.toOwnedSlice();
    // LocaleProvider.deinit frees the buffer — don't double-free.
    const lp = try LocaleProvider.init(alloc, "en", topil);
    defer lp.deinit(alloc);

    const sd = lp.map.get(UUID.fromString("AAAAAAAA-BBBBBBBB")).?;
    try std.testing.expectEqualSlices(u8, "Bonjour {0}!", sd.bytes);
    // Three segments: literal "Bonjour " | interp 0 | literal "!"
    try std.testing.expectEqual(@as(usize, 3), sd.segments.len);
    try std.testing.expect(sd.segments[0] == .literal);
    try std.testing.expect(sd.segments[1] == .interp);
    try std.testing.expectEqual(@as(u8, 0), sd.segments[1].interp);
    try std.testing.expect(sd.segments[2] == .literal);
}

test "Localization v1 File Rejected" {
    // A v1 .topil file must surface as UnsupportedLocaleVersion, not
    // crash, when fed to a v2 provider. Hand-build the smallest valid
    // v1 prelude (magic + version=1 + count=0).
    const alloc = std.testing.allocator;
    var buf = std.ArrayList(u8).empty;
    defer buf.deinit(alloc);
    try buf.appendSlice(alloc, LocaleProvider.magic);
    var int_buf: [4]u8 = undefined;
    std.mem.writeInt(u16, int_buf[0..2], 1, .little);
    try buf.appendSlice(alloc, int_buf[0..2]);
    std.mem.writeInt(C.CONSTANT, int_buf[0..@sizeOf(C.CONSTANT)], 0, .little);
    try buf.appendSlice(alloc, int_buf[0..@sizeOf(C.CONSTANT)]);
    const owned = try buf.toOwnedSlice(alloc);
    try std.testing.expectError(error.UnsupportedLocaleVersion, LocaleProvider.init(alloc, "en", owned));
    alloc.free(owned);
}
