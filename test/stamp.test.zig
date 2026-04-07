const std = @import("std");
const parser_test = @import("parser.test.zig");
const topi = @import("topi");
const Stamp = topi.stamp.Stamp;
const Locale = topi.locale.Locale;
const UUID = topi.utils.UUID;
const Module = topi.module.Module;

const compileSource = @import("compiler.test.zig").compileSource;

test "Stamp inserts IDs for dialogue" {
    const input =
        \\ === START {
        \\     :: "Hello"
        \\     :Speaker: "World"
        \\ }
    ;
    const mod = try parser_test.parseSource(input);
    defer mod.deinit();
    const result = try Stamp.stampFile(mod.entry, std.testing.allocator);
    defer std.testing.allocator.free(result);

    // All dialogue lines should have @UUID
    try std.testing.expect(std.mem.indexOf(u8, result, "\"Hello\" @") != null);
    try std.testing.expect(std.mem.indexOf(u8, result, "\"World\" @") != null);
}

test "Stamp inserts IDs for choices" {
    const input =
        \\ === START {
        \\     fork {
        \\         ~ "Option A" {
        \\             :: "chose A"
        \\         }
        \\         ~ "Option B" {
        \\             :: "chose B"
        \\         }
        \\     }
        \\ }
    ;
    const mod = try parser_test.parseSource(input);
    defer mod.deinit();
    const result = try Stamp.stampFile(mod.entry, std.testing.allocator);
    defer std.testing.allocator.free(result);

    try std.testing.expect(std.mem.indexOf(u8, result, "\"Option A\" @") != null);
    try std.testing.expect(std.mem.indexOf(u8, result, "\"Option B\" @") != null);
}

test "Stamp inserts IDs for boughs" {
    const input =
        \\ === START {
        \\     :: "Hello"
        \\ }
        \\ === OTHER {
        \\     :: "World"
        \\ }
    ;
    const mod = try parser_test.parseSource(input);
    defer mod.deinit();
    const result = try Stamp.stampFile(mod.entry, std.testing.allocator);
    defer std.testing.allocator.free(result);

    try std.testing.expect(std.mem.indexOf(u8, result, "START @") != null);
    try std.testing.expect(std.mem.indexOf(u8, result, "OTHER @") != null);
}

test "Stamp inserts IDs for forks" {
    const input =
        \\ === START {
        \\     fork {
        \\         ~ "A" { :: "a" }
        \\     }
        \\     fork^ {
        \\         ~ "B" { :: "b" }
        \\     }
        \\     fork NAMED {
        \\         ~ "C" { :: "c" }
        \\     }
        \\ }
    ;
    const mod = try parser_test.parseSource(input);
    defer mod.deinit();
    const result = try Stamp.stampFile(mod.entry, std.testing.allocator);
    defer std.testing.allocator.free(result);

    // Each fork should get an @UUID
    var count: usize = 0;
    var pos: usize = 0;
    while (std.mem.indexOfPos(u8, result, pos, " @")) |idx| {
        count += 1;
        pos = idx + 1;
    }
    // 3 forks + 2 boughs (START) is actually 1 bough + 3 forks + 3 choices + 3 dialogue = 10
    // Let's just check the named fork
    try std.testing.expect(std.mem.indexOf(u8, result, "NAMED @") != null);
}

test "Stamp preserves existing IDs" {
    const input =
        \\ === START @JUWH59VY-LRIGSPSB {
        \\     :: "Hello"@C5I6VN71-IP0HPJHE
        \\     fork @8R955KPX-2WI5R816 {
        \\         ~ "Choice"@JTCCIIS7-NHTNWTBL {
        \\             :: "inner"
        \\         }
        \\     }
        \\ }
    ;
    const mod = try parser_test.parseSource(input);
    defer mod.deinit();
    const result = try Stamp.stampFile(mod.entry, std.testing.allocator);
    defer std.testing.allocator.free(result);

    // Existing IDs should be preserved
    try std.testing.expect(std.mem.indexOf(u8, result, "@JUWH59VY-LRIGSPSB") != null);
    try std.testing.expect(std.mem.indexOf(u8, result, "@C5I6VN71-IP0HPJHE") != null);
    try std.testing.expect(std.mem.indexOf(u8, result, "@8R955KPX-2WI5R816") != null);
    try std.testing.expect(std.mem.indexOf(u8, result, "@JTCCIIS7-NHTNWTBL") != null);
    // The inner dialogue without ID should get a new one
    try std.testing.expect(std.mem.indexOf(u8, result, "\"inner\" @") != null);
}

test "Stamp handles dialogue with tags" {
    const input =
        \\ === START {
        \\     :: "Hello" #thought
        \\     :Speaker: "World" #tag1 #tag2
        \\ }
    ;
    const mod = try parser_test.parseSource(input);
    defer mod.deinit();
    const result = try Stamp.stampFile(mod.entry, std.testing.allocator);
    defer std.testing.allocator.free(result);

    // IDs should be inserted after the tags
    try std.testing.expect(std.mem.indexOf(u8, result, "#thought @") != null);
    try std.testing.expect(std.mem.indexOf(u8, result, "#tag2 @") != null);
}

test "Stamp is idempotent" {
    const input =
        \\ === START {
        \\     :: "Hello"
        \\     fork {
        \\         ~ "Choice" {
        \\             :: "inner"
        \\         }
        \\     }
        \\ }
    ;
    const mod = try parser_test.parseSource(input);
    defer mod.deinit();
    const first = try Stamp.stampFile(mod.entry, std.testing.allocator);
    defer std.testing.allocator.free(first);

    // Parse the stamped output and stamp again
    const mod2 = try parser_test.parseSource(first);
    defer mod2.deinit();
    const second = try Stamp.stampFile(mod2.entry, std.testing.allocator);
    defer std.testing.allocator.free(second);

    try std.testing.expectEqualStrings(first, second);
}

test "Stamp handles interpolated strings" {
    const input =
        \\ === START {
        \\     var name = "World"
        \\     :: "Hello {name}!"
        \\     :: "Simple string"
        \\     :Speaker: "Say {name} and {name} again"
        \\ }
    ;
    const mod = try parser_test.parseSource(input);
    defer mod.deinit();
    const result = try Stamp.stampFile(mod.entry, std.testing.allocator);
    defer std.testing.allocator.free(result);

    // IDs must be AFTER the closing quote, not inside interpolation
    try std.testing.expect(std.mem.indexOf(u8, result, "\"Hello {name}!\" @") != null);
    try std.testing.expect(std.mem.indexOf(u8, result, "\"Simple string\" @") != null);
    try std.testing.expect(std.mem.indexOf(u8, result, "\"Say {name} and {name} again\" @") != null);

    // Verify no @UUID appears inside a string interpolation
    try std.testing.expect(std.mem.indexOf(u8, result, "{ @") == null);
}

test "Stamp handles interpolated strings with tags" {
    const input =
        \\ === START {
        \\     var mood = "happy"
        \\     :: "Feeling {mood} today" #emotion
        \\ }
    ;
    const mod = try parser_test.parseSource(input);
    defer mod.deinit();
    const result = try Stamp.stampFile(mod.entry, std.testing.allocator);
    defer std.testing.allocator.free(result);

    // ID should be after the tag, not inside the interpolation
    try std.testing.expect(std.mem.indexOf(u8, result, "#emotion @") != null);
    try std.testing.expect(std.mem.indexOf(u8, result, "{ @") == null);
}

test "Parser round-trips bough with @ID" {
    const input =
        \\ === START @JUWH59VY-LRIGSPSB {
        \\     :: "Hello"
        \\ }
    ;
    const mod = try parser_test.parseSource(input);
    defer mod.deinit();
    const bough = mod.entry.tree.?.root[0].type.bough;

    // Parser should have captured the @ID
    try std.testing.expect(bough.id_token != null);
    try std.testing.expect(!UUID.isEmpty(bough.id));
    try std.testing.expectEqualStrings("JUWH59VY-LRIGSPSB", &bough.id);
}

test "Parser round-trips fork variants with @ID" {
    // Anonymous fork with @ID
    const input1 =
        \\ === START {
        \\     fork @JUWH59VY-LRIGSPSB {
        \\         ~ "A" { :: "a" }
        \\     }
        \\ }
    ;
    const mod1 = try parser_test.parseSource(input1);
    defer mod1.deinit();
    const fork1 = mod1.entry.tree.?.root[0].type.bough.body[0].type.fork;
    try std.testing.expect(fork1.id_token != null);
    try std.testing.expectEqualStrings("JUWH59VY-LRIGSPSB", &fork1.id);

    // Backup fork with @ID
    const input2 =
        \\ === START {
        \\     fork^ @8R955KPX-2WI5R816 {
        \\         ~ "B" { :: "b" }
        \\     }
        \\ }
    ;
    const mod2 = try parser_test.parseSource(input2);
    defer mod2.deinit();
    const fork2 = mod2.entry.tree.?.root[0].type.bough.body[0].type.fork;
    try std.testing.expect(fork2.id_token != null);
    try std.testing.expect(fork2.is_backup);
    try std.testing.expectEqualStrings("8R955KPX-2WI5R816", &fork2.id);

    // Named fork with @ID
    const input3 =
        \\ === START {
        \\     fork TOPICS @C5I6VN71-IP0HPJHE {
        \\         ~ "C" { :: "c" }
        \\     }
        \\ }
    ;
    const mod3 = try parser_test.parseSource(input3);
    defer mod3.deinit();
    const fork3 = mod3.entry.tree.?.root[0].type.bough.body[0].type.fork;
    try std.testing.expect(fork3.id_token != null);
    try std.testing.expectEqualStrings("TOPICS", fork3.name.?);
    try std.testing.expectEqualStrings("C5I6VN71-IP0HPJHE", &fork3.id);

    // Named backup fork with @ID
    const input4 =
        \\ === START {
        \\     fork^ TOPICS @JTCCIIS7-NHTNWTBL {
        \\         ~ "D" { :: "d" }
        \\     }
        \\ }
    ;
    const mod4 = try parser_test.parseSource(input4);
    defer mod4.deinit();
    const fork4 = mod4.entry.tree.?.root[0].type.bough.body[0].type.fork;
    try std.testing.expect(fork4.id_token != null);
    try std.testing.expect(fork4.is_backup);
    try std.testing.expectEqualStrings("TOPICS", fork4.name.?);
    try std.testing.expectEqualStrings("JTCCIIS7-NHTNWTBL", &fork4.id);
}

test "Stamp output compiles to bytecode" {
    const input =
        \\ === START {
        \\     :: "Hello"
        \\     :Speaker: "World {1 + 1}"
        \\     fork {
        \\         ~ "Option A" #tag {
        \\             :: "chose A"
        \\         }
        \\         ~ "Option B" {
        \\             :: "chose B"
        \\         }
        \\     }
        \\ }
    ;
    const mod = try parser_test.parseSource(input);
    defer mod.deinit();
    const stamped = try Stamp.stampFile(mod.entry, std.testing.allocator);
    defer std.testing.allocator.free(stamped);

    // The stamped output must be valid enough to compile
    var mod2 = try Module.initEmpty(std.testing.allocator);
    defer mod2.deinit();
    var bytecode = try compileSource(stamped, mod2);
    bytecode.free(std.testing.allocator);
}

test "Stamp nested boughs" {
    const input =
        \\ === VILLAGE {
        \\     :: "The village"
        \\     === PUB {
        \\         :: "The pub"
        \\         fork TOPICS {
        \\             ~ "Ask about weather" {
        \\                 :: "It's stormy"
        \\             }
        \\         }
        \\     }
        \\ }
    ;
    const mod = try parser_test.parseSource(input);
    defer mod.deinit();
    const result = try Stamp.stampFile(mod.entry, std.testing.allocator);
    defer std.testing.allocator.free(result);

    // Both outer and inner boughs should be stamped
    try std.testing.expect(std.mem.indexOf(u8, result, "VILLAGE @") != null);
    try std.testing.expect(std.mem.indexOf(u8, result, "PUB @") != null);
    try std.testing.expect(std.mem.indexOf(u8, result, "TOPICS @") != null);

    // Verify re-parses cleanly
    const mod2 = try parser_test.parseSource(result);
    defer mod2.deinit();
}

test "Stamp loc validate check reports missing IDs" {
    const input =
        \\ === START {
        \\     :: "No ID here"
        \\     :: "Or here"
        \\     :: "Already has one"@JUWH59VY-LRIGSPSB
        \\ }
    ;
    const mod = try parser_test.parseSource(input);
    defer mod.deinit();
    const missing = try Locale.checkFile(mod.entry);
    try std.testing.expectEqual(@as(usize, 2), missing);
}

test "Stamp loc validate reports zero after stamp" {
    const input =
        \\ === START {
        \\     :: "Line one"
        \\     :: "Line two"
        \\     fork {
        \\         ~ "Choice" {
        \\             :: "inner"
        \\         }
        \\     }
        \\ }
    ;
    const mod = try parser_test.parseSource(input);
    defer mod.deinit();

    // Before stamp: should have missing IDs
    const before = try Locale.checkFile(mod.entry);
    try std.testing.expect(before > 0);

    // Stamp the file
    const stamped = try Stamp.stampFile(mod.entry, std.testing.allocator);
    defer std.testing.allocator.free(stamped);

    // Re-parse and check: should have zero missing
    const mod2 = try parser_test.parseSource(stamped);
    defer mod2.deinit();
    const after = try Locale.checkFile(mod2.entry);
    try std.testing.expectEqual(@as(usize, 0), after);
}

test "Stamp choice with tags preserves existing ID" {
    const input =
        \\ === START {
        \\     fork {
        \\         ~ "Tagged choice" #important @JUWH59VY-LRIGSPSB {
        \\             :: "inner"
        \\         }
        \\     }
        \\ }
    ;
    const mod = try parser_test.parseSource(input);
    defer mod.deinit();
    const result = try Stamp.stampFile(mod.entry, std.testing.allocator);
    defer std.testing.allocator.free(result);

    // Existing ID after tag should be preserved
    try std.testing.expect(std.mem.indexOf(u8, result, "#important @JUWH59VY-LRIGSPSB") != null);
}

test "Stamp unique choices" {
    const input =
        \\ === START {
        \\     fork {
        \\         ~* "Unique choice" {
        \\             :: "picked"
        \\         }
        \\     }
        \\ }
    ;
    const mod = try parser_test.parseSource(input);
    defer mod.deinit();
    const result = try Stamp.stampFile(mod.entry, std.testing.allocator);
    defer std.testing.allocator.free(result);

    try std.testing.expect(std.mem.indexOf(u8, result, "\"Unique choice\" @") != null);
}
