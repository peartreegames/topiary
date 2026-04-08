const std = @import("std");
const topi = @import("topi");
const Formatter = topi.frontend.Formatter;
const Module = topi.module.Module;
const File = topi.module.File;
const Tree = topi.frontend.Tree;
const UUID = topi.utils.UUID;

const testing = std.testing;
const allocator = testing.allocator;

fn formatSource(source: []const u8) ![]const u8 {
    return formatSourceWithIndent(source, 4);
}

fn formatSourceWithIndent(source: []const u8, indent_width: usize) ![]const u8 {
    const mod = try Module.initEmpty(allocator);
    defer mod.deinit();
    const file = try mod.arena.allocator().create(File);
    file.* = .{
        .module = mod,
        .path = "_test_",
        .name = "",
        .dir_name = ".",
        .source = source,
    };
    mod.entry = file;
    try file.buildTree();
    const tree = file.tree orelse return error.NoTree;
    return Formatter.format(source, tree, allocator, indent_width, false);
}

fn formatSourceWithStamp(source: []const u8) ![]const u8 {
    const mod = try Module.initEmpty(allocator);
    defer mod.deinit();
    const file = try mod.arena.allocator().create(File);
    file.* = .{
        .module = mod,
        .path = "_test_",
        .name = "",
        .dir_name = ".",
        .source = source,
    };
    mod.entry = file;
    try file.buildTree();
    const tree = file.tree orelse return error.NoTree;
    return Formatter.format(source, tree, allocator, 4, true);
}

test "format: simple variable declaration" {
    const result = try formatSource("var   x  =  5");
    defer allocator.free(result);
    try testing.expectEqualStrings("var x = 5\n", result);
}

test "format: const declaration" {
    const result = try formatSource("const name = \"hello\"");
    defer allocator.free(result);
    try testing.expectEqualStrings("const name = \"hello\"\n", result);
}

test "format: simple bough" {
    const result = try formatSource("=== START { :: \"hello\" }");
    defer allocator.free(result);
    try testing.expectEqualStrings(
        \\=== START {
        \\    :: "hello"
        \\}
        \\
    , result);
}

test "format: fork with choices (braced preserved)" {
    const result = try formatSource(
        \\=== START { fork { ~ "yes" { :: "ok" } ~ "no" { :: "nope" } } }
    );
    defer allocator.free(result);
    try testing.expectEqualStrings(
        \\=== START {
        \\    fork {
        \\        ~ "yes" {
        \\            :: "ok"
        \\        }
        \\        ~ "no" {
        \\            :: "nope"
        \\        }
        \\    }
        \\}
        \\
    , result);
}

test "format: comment preservation" {
    const result = try formatSource(
        \\// first comment
        \\// second comment
        \\var x = 1
    );
    defer allocator.free(result);
    try testing.expectEqualStrings(
        \\// first comment
        \\// second comment
        \\var x = 1
        \\
    , result);
}

test "format: single-line if with dialogue" {
    const result = try formatSource(
        \\=== S { if   true  :: "yes" }
    );
    defer allocator.free(result);
    try testing.expectEqualStrings(
        \\=== S {
        \\    if true :: "yes"
        \\}
        \\
    , result);
}

test "format: if-else braced preserved" {
    const result = try formatSource(
        \\=== S { if true { :: "yes" } else { :: "no" } }
    );
    defer allocator.free(result);
    try testing.expectEqualStrings(
        \\=== S {
        \\    if true {
        \\        :: "yes"
        \\    } else {
        \\        :: "no"
        \\    }
        \\}
        \\
    , result);
}

test "format: if-else multi then, braced else preserved" {
    const result = try formatSource(
        \\=== S { if true { :: "yes"
        \\:: "more" } else { :: "no" } }
    );
    defer allocator.free(result);
    try testing.expectEqualStrings(
        \\=== S {
        \\    if true {
        \\        :: "yes"
        \\        :: "more"
        \\    } else {
        \\        :: "no"
        \\    }
        \\}
        \\
    , result);
}

test "format: divert" {
    const result = try formatSource(
        \\=== S { => OTHER }
    );
    defer allocator.free(result);
    try testing.expectEqualStrings(
        \\=== S {
        \\    => OTHER
        \\}
        \\
    , result);
}

test "format: backup divert" {
    const result = try formatSource(
        \\=== S { =>^ OTHER }
    );
    defer allocator.free(result);
    try testing.expectEqualStrings(
        \\=== S {
        \\    =>^ OTHER
        \\}
        \\
    , result);
}

test "format: backup divert normalizes spacing" {
    const result = try formatSource(
        \\=== S { => ^OTHER }
    );
    defer allocator.free(result);
    try testing.expectEqualStrings(
        \\=== S {
        \\    =>^ OTHER
        \\}
        \\
    , result);
}

test "format: backup fork normalizes spacing" {
    const result = try formatSource(
        \\=== S { fork ^CHOICES { ~ "x" :: "y" } }
    );
    defer allocator.free(result);
    try testing.expectEqualStrings(
        \\=== S {
        \\    fork^ CHOICES {
        \\        ~ "x" :: "y"
        \\    }
        \\}
        \\
    , result);
}

test "format: function declaration" {
    const result = try formatSource("fn add |x, y| return x + y");
    defer allocator.free(result);
    try testing.expectEqualStrings("fn add |x, y| return x + y\n", result);
}

test "format: function with body" {
    const result = try formatSource(
        \\fn fib |n| { if n < 2 return n
        \\return fib(n - 1) + fib(n - 2) }
    );
    defer allocator.free(result);
    try testing.expectEqualStrings(
        \\fn fib |n| {
        \\    if n < 2 return n
        \\    return fib(n - 1) + fib(n - 2)
        \\}
        \\
    , result);
}

test "format: configurable indent width" {
    const result = try formatSourceWithIndent("=== S { :: \"hi\" }", 2);
    defer allocator.free(result);
    try testing.expectEqualStrings(
        \\=== S {
        \\  :: "hi"
        \\}
        \\
    , result);
}

test "format: enum declaration" {
    const result = try formatSource("enum Color { Red, Green, Blue }");
    defer allocator.free(result);
    try testing.expectEqualStrings(
        \\enum Color {
        \\    Red,
        \\    Green,
        \\    Blue
        \\}
        \\
    , result);
}

test "format: while loop always braced" {
    const result = try formatSource(
        \\=== S { var i = 0
        \\while i < 3 {  i += 1   } }
    );
    defer allocator.free(result);
    try testing.expectEqualStrings(
        \\=== S {
        \\    var i = 0
        \\    while i < 3 {
        \\        i += 1
        \\    }
        \\}
        \\
    , result);
}

test "format: for loop always braced" {
    const result = try formatSource(
        \\=== S { for 0..2 |i| {  :: "hi"   } }
    );
    defer allocator.free(result);
    try testing.expectEqualStrings(
        \\=== S {
        \\    for 0..2 |i| {
        \\        :: "hi"
        \\    }
        \\}
        \\
    , result);
}

test "format: blank line between top-level declarations" {
    const result = try formatSource(
        \\var x = 1
        \\=== START { :: "hi" }
        \\=== END { :: "bye" }
    );
    defer allocator.free(result);
    try testing.expectEqualStrings(
        \\var x = 1
        \\
        \\=== START {
        \\    :: "hi"
        \\}
        \\
        \\=== END {
        \\    :: "bye"
        \\}
        \\
    , result);
}

test "format: choice with inline divert" {
    const result = try formatSource(
        \\=== S { fork { ~ "go" => OTHER } }
    );
    defer allocator.free(result);
    try testing.expectEqualStrings(
        \\=== S {
        \\    fork {
        \\        ~ "go" => OTHER
        \\    }
        \\}
        \\
    , result);
}

test "format: unique choice (braced preserved)" {
    const result = try formatSource(
        \\=== S { fork { ~* "once" { :: "done" } } }
    );
    defer allocator.free(result);
    try testing.expectEqualStrings(
        \\=== S {
        \\    fork {
        \\        ~* "once" {
        \\            :: "done"
        \\        }
        \\    }
        \\}
        \\
    , result);
}

test "format: speaker dialogue" {
    const result = try formatSource(
        \\=== S { :Bob: "hello" }
    );
    defer allocator.free(result);
    try testing.expectEqualStrings(
        \\=== S {
        \\    :Bob: "hello"
        \\}
        \\
    , result);
}

test "format: choice with inline dialogue (unbraced preserved)" {
    const result = try formatSource(
        \\=== S { fork { ~ "go" :: "ok" } }
    );
    defer allocator.free(result);
    try testing.expectEqualStrings(
        \\=== S {
        \\    fork {
        \\        ~ "go" :: "ok"
        \\    }
        \\}
        \\
    , result);
}

test "format: single-line fn body stays inline" {
    const source = "fn greet || return \"hello\"\n";
    const result = try formatSource(source);
    defer allocator.free(result);
    try testing.expectEqualStrings(source, result);
}

test "format: single-line extern fn body stays inline" {
    const source = "extern fn triggerAnim |name, clip| print(\"TriggerAnim: {name} {clip}\")\n";
    const result = try formatSource(source);
    defer allocator.free(result);
    try testing.expectEqualStrings(source, result);
}

test "format: single-line fn body with interpolated string" {
    const source = "fn greet |name| return \"{name}\"\n";
    const result = try formatSource(source);
    defer allocator.free(result);
    try testing.expectEqualStrings(source, result);
}

test "format: multi-line fn body gets braces" {
    const result = try formatSource(
        \\fn add |x, y| { var z = x + y
        \\return z }
    );
    defer allocator.free(result);
    try testing.expectEqualStrings(
        \\fn add |x, y| {
        \\    var z = x + y
        \\    return z
        \\}
        \\
    , result);
}

test "format: single-line if with interpolated string" {
    const result = try formatSource(
        \\=== S { if   true  :Bob: "{greet()}, hi" }
    );
    defer allocator.free(result);
    try testing.expectEqualStrings(
        \\=== S {
        \\    if true :Bob: "{greet()}, hi"
        \\}
        \\
    , result);
}

test "format: interpolated string with function call" {
    const source =
        \\=== S {
        \\    :Bob: "{greet("hello")}, how are you?"
        \\}
        \\
    ;
    const result = try formatSource(source);
    defer allocator.free(result);
    try testing.expectEqualStrings(source, result);
}

test "format: idempotency" {
    const source =
        \\=== START {
        \\    :: "hello"
        \\    fork^ {
        \\        ~ "yes" {
        \\            :: "ok"
        \\            => OTHER
        \\        }
        \\        ~ "no" :: "nope"
        \\    }
        \\}
        \\
    ;
    const result = try formatSource(source);
    defer allocator.free(result);
    try testing.expectEqualStrings(source, result);
}

fn hasUuidAt(haystack: []const u8, pos: usize) bool {
    if (pos + 1 + UUID.Size > haystack.len) return false;
    if (haystack[pos] != '@') return false;
    const candidate = haystack[pos + 1 .. pos + 1 + UUID.Size];
    if (candidate[8] != '-') return false;
    return !UUID.isEmpty(UUID.fromString(candidate));
}

fn countUuids(haystack: []const u8) usize {
    var count: usize = 0;
    var i: usize = 0;
    while (i < haystack.len) : (i += 1) {
        if (hasUuidAt(haystack, i)) count += 1;
    }
    return count;
}

test "format: stamp inserts IDs for unstamped nodes" {
    const source =
        \\=== START {
        \\    :: "hello"
        \\    fork {
        \\        ~ "yes" :: "ok"
        \\    }
        \\}
        \\
    ;
    const result = try formatSourceWithStamp(source);
    defer allocator.free(result);
    // bough, dialogue, fork, choice, inner dialogue = 5 UUIDs
    try testing.expectEqual(@as(usize, 5), countUuids(result));
}

test "format: stamp preserves existing IDs" {
    const id = UUID.create(42);
    var source_buf: [256]u8 = undefined;
    const source = try std.fmt.bufPrint(&source_buf, "=== START @{s} {{\n    :: \"hello\"\n}}\n", .{id});
    const result = try formatSourceWithStamp(source);
    defer allocator.free(result);
    // The original bough ID should be preserved
    try testing.expect(std.mem.indexOf(u8, result, &id) != null);
}

test "format: no stamp by default" {
    const source =
        \\=== START {
        \\    :: "hello"
        \\}
        \\
    ;
    const result = try formatSource(source);
    defer allocator.free(result);
    try testing.expectEqual(@as(usize, 0), countUuids(result));
}
