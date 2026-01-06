const std = @import("std");
const topi = @import("topi");
const Errors = topi.backend.CompilerErrors;

const Parser = topi.frontend.Parser;
const Tree = topi.frontend.Tree;

const Module = topi.module.Module;
const File = topi.module.File;

const testing = std.testing;
const allocator = testing.allocator;

pub fn parseSource(source: []const u8) !*Module {
    const mod = try Module.initEmpty(allocator);
    errdefer mod.deinit();
    const file = try mod.arena.allocator().create(File);
    file.* = .{
        .module = mod,
        .path = "",
        .name = "",
        .dir_name = "",
        .dir = undefined,
        .source = source,
        .source_loaded = true,
        .errors = Errors.init(mod.arena.allocator()),
    };
    try file.errors.add("Unexpected token '{s}'\n", .{
        .start = 0,
        .end = 1,
        .token_type = .identifier,
        .line = 1,
        .column = 1,
        .file_index = 0,
    }, .err, .{"test"});
    mod.entry = file;

    var buffer: [1024]u8 = undefined;
    var writer = std.fs.File.stderr().writer(&buffer);
    const stderr = &writer.interface;
    file.buildTree() catch |err| {
        try file.errors.write("N/A", source, stderr);
        return err;
    };
    return mod;
}

test "Parse Include" {
    const input = "include \"./globals.topi\"";
    const err = parseSource(input);
    try testing.expectError(Parser.Error.ParserError, err);
}

test "Parse Declaration" {
    const t =
        \\ const intValue = 5
        \\ var mutableValue = 1.2
        \\ class ClassType {
        \\     intField = 0
        \\ }
        \\ var classValue = new ClassType{}
        \\ enum EnumType {
        \\     one,
        \\     two,
        \\ }
        \\ const enumValue = EnumType.one
        \\ const str = "string value"
    ;

    const mod = try parseSource(t);
    defer mod.deinit();
    const file = mod.entry;
    const tree = file.tree;
    try testing.expect(tree.root.len == 7);
    try testing.expect(!tree.root[0].type.variable.is_mutable);
    try testing.expect(tree.root[1].type.variable.is_mutable);
    try testing.expect(tree.root[1].type.variable.initializer.type.number == 1.2);

    try testing.expect(tree.root[2].type.class.fields.len == 1);
    try testing.expectEqualStrings("intField", tree.root[2].type.class.field_names[0]);
    try testing.expect(tree.root[2].type.class.fields[0].type.number == 0);

    try testing.expect(tree.root[3].type.variable.is_mutable);
    try testing.expectEqualStrings("ClassType", tree.root[3].type.variable.initializer.type.instance.name);
    try testing.expect(tree.root[3].type.variable.initializer.type.instance.fields.len == 0);

    try testing.expectEqualStrings("EnumType", tree.root[4].type.@"enum".name);
    try testing.expect(tree.root[4].type.@"enum".values.len == 2);
    try testing.expectEqualStrings("one", tree.root[4].type.@"enum".values[0]);
    try testing.expectEqualStrings("two", tree.root[4].type.@"enum".values[1]);
}

test "Parse Function Declaration" {
    const t =
        \\ fn sum |x, y| return x + y
        \\ fn str |value, count| {
        \\    var result = "This is a string"
        \\    return result    
        \\ }
    ;
    const mod = try parseSource(t);
    defer mod.deinit();
    const file = mod.entry;
    const tree = file.tree;
    try testing.expect(tree.root.len == 2);
    try testing.expectEqualStrings("x", tree.root[0].type.function.parameters[0]);
    try testing.expectEqualStrings("y", tree.root[0].type.function.parameters[1]);
    try testing.expect(tree.root[0].type.function.body.len == 1);
}

test "Parse Function Arguments" {
    const t =
        \\ fn sum |x, y| return x + y
        \\ sum(1, 2) + sum(3, 4)
    ;
    const mod = try parseSource(t);
    defer mod.deinit();
    const file = mod.entry;
    const tree = file.tree;
    try testing.expect(tree.root.len == 2);
    try testing.expect(tree.root[1].type.expression.type.binary.operator == .add);
    const bin = tree.root[1].type.expression.type.binary;
    try testing.expect(bin.right.type.call.arguments.len == 2);
    try testing.expect(bin.right.type.call.arguments[0].type.number == 3);
    try testing.expect(bin.right.type.call.arguments[1].type.number == 4);
    try testing.expect(bin.left.type.call.arguments.len == 2);
    try testing.expect(bin.left.type.call.arguments[0].type.number == 1);
    try testing.expect(bin.left.type.call.arguments[1].type.number == 2);
}

test "Parse Enums" {
    const t =
        \\ enum Test {
        \\    one,
        \\    two  
        \\ }
        \\ var value = Test.one
    ;
    const mod = try parseSource(t);
    defer mod.deinit();
    const file = mod.entry;
    const tree = file.tree;
    try testing.expect(tree.root.len == 2);
    const e = tree.root[0].type.@"enum";
    try testing.expectEqualStrings("Test", e.name);
    try testing.expectEqualStrings("one", e.values[0]);
    try testing.expectEqualStrings("two", e.values[1]);

    try testing.expect(tree.root[1].type.variable.initializer.type == .indexer);
}

test "Parse Iterable Types" {
    const test_cases = .{
        .{ .input = "const stringList = List{\"item\"}", .id = "stringList", .item_value = [_][]const u8{"item"}, .mutable = false, .type = .list },
        .{ .input = "const stringList = List{\"item1\", \"item2\"}", .id = "stringList", .item_value = [_][]const u8{ "item1", "item2" }, .mutable = false, .type = .list },
        .{ .input = "var floatSet = Set{2.0}", .id = "floatSet", .item_value = [_]f32{2.0}, .mutable = true, .type = .set },
        .{ .input = "var floatSet = Set{2.0, 3.4, 5.6}", .id = "floatSet", .item_value = [_]f32{ 2.0, 3.4, 5.6 }, .mutable = true, .type = .set },
        .{ .input = "var stringBoolMap = Map{\"key\":true}", .id = "stringBoolMap", .item_value = [_]bool{true}, .mutable = true, .type = .map },
        .{ .input = "var stringBoolMap = Map{\"key1\":true, \"key2\": false, \"key3\": true}", .id = "stringBoolMap", .item_value = [_]bool{ true, false, true }, .mutable = true, .type = .map },
    };

    inline for (test_cases) |case| {
        const mod = try parseSource(case.input);
        defer mod.deinit();
        const file = mod.entry;
        const tree = file.tree;
        const node = tree.root[0].type.variable;
        try testing.expectEqualStrings(case.id, node.name);

        for (case.item_value, 0..) |value, i| {
            switch (case.type) {
                .list => try testing.expectEqualStrings(value, node.initializer.type.list[i].type.string.value),
                .set => try testing.expect(value == node.initializer.type.set[i].type.number),
                .map => try testing.expect(value == node.initializer.type.map[i].type.map_pair.value.type.boolean),
                else => unreachable,
            }
        }
        try testing.expect(case.mutable == node.is_mutable);
    }
}

test "Parse Empty Iterable Types" {
    const input =
        \\ const emptyMap = Map{}
        \\ const emptySet = Set{}
    ;
    const mod = try parseSource(input);
    defer mod.deinit();
    const file = mod.entry;
    const tree = file.tree;
    var decl = tree.root[0].type.variable;
    try testing.expectEqualStrings("emptyMap", decl.name);
    try testing.expect(decl.initializer.type.map.len == 0);

    decl = tree.root[1].type.variable;
    try testing.expectEqualStrings("emptySet", decl.name);
    try testing.expect(decl.initializer.type.set.len == 0);
}

test "Parse Nested Iterable Types" {
    const input =
        \\ List{List{1,2}}
    ;
    const mod = try parseSource(input);
    defer mod.deinit();
    const file = mod.entry;
    const tree = file.tree;

    try testing.expect(tree.root[0].type.expression.type == .list);
    try testing.expect(tree.root[0].type.expression.type.list[0].type == .list);
}

test "Parse Extern" {
    const test_cases = .{
        .{ .input = "extern const x = 0", .id = "x", .mutable = false, .@"extern" = true },
        .{ .input = "extern var y = 0", .id = "y", .mutable = true, .@"extern" = true },
    };

    inline for (test_cases) |case| {
        const mod = try parseSource(case.input);
        defer mod.deinit();
        const file = mod.entry;
        const tree = file.tree;
        const decl = tree.root[0].type.variable;
        try testing.expectEqualStrings(case.id, decl.name);
        try testing.expect(case.mutable == decl.is_mutable);
        try testing.expect(case.@"extern" == decl.is_extern);
    }
}

test "Parse Enum" {
    const input =
        \\ enum E {
        \\     one,
        \\     two,
        \\ }
        \\
        \\ enum En {
        \\     three,
        \\     four
        \\ }
        \\ const val = En.three
    ;
    const mod = try parseSource(input);
    defer mod.deinit();
    const file = mod.entry;
    const tree = file.tree;
    var decl = tree.root[0].type.@"enum";
    try testing.expectEqualStrings("E", decl.name);
    try testing.expectEqualStrings("one", decl.values[0]);
    try testing.expectEqualStrings("two", decl.values[1]);

    decl = tree.root[1].type.@"enum";
    try testing.expectEqualStrings("En", decl.name);
    try testing.expectEqualStrings("three", decl.values[0]);
    try testing.expectEqualStrings("four", decl.values[1]);

    const varDecl = tree.root[2].type.variable;
    try testing.expectEqualStrings("val", varDecl.name);
    try testing.expectEqualStrings("En", varDecl.initializer.type.indexer.target.type.identifier);
    try testing.expectEqualStrings("three", varDecl.initializer.type.indexer.index.type.identifier);
}

test "Parse If" {
    const input =
        \\ var value = 0
        \\ if true value = 1
        \\ else if 5 < 1 value = 2
        \\ else value = 3
        \\ const a = if true "true" else "false"
        \\
        \\ if true {
        \\     value = 4  
        \\ } else {
        \\    value = 5    
        \\ }
    ;
    const mod = try parseSource(input);
    defer mod.deinit();
    const file = mod.entry;
    const tree = file.tree;

    var if_stmt = tree.root[1].type.@"if";
    try testing.expect(if_stmt.condition.type.boolean);
    try testing.expect(if_stmt.then_branch[0].type.expression.type.binary.right.type.number == 1);
    try testing.expect(if_stmt.else_branch.?[0].type.@"if".then_branch[0].type.expression.type.binary.right.type.number == 2);
    try testing.expect(if_stmt.else_branch.?[0].type.@"if".else_branch.?[0].type.expression.type.binary.right.type.number == 3);

    const decl = tree.root[2].type.variable;
    try testing.expectEqualStrings("true", decl.initializer.type.@"if".then_value.type.string.value);
    try testing.expectEqualStrings("false", decl.initializer.type.@"if".else_value.type.string.value);

    if_stmt = tree.root[3].type.@"if";

    try testing.expect(if_stmt.condition.type.boolean);
    try testing.expect(if_stmt.then_branch[0].type.expression.type.binary.right.type.number == 4);
    try testing.expect(if_stmt.else_branch.?[0].type.expression.type.binary.right.type.number == 5);
}

test "Parse Call expression" {
    const input =
        \\ add(1, 2 * 3, 4 + 5)
    ;
    const mod = try parseSource(input);
    defer mod.deinit();
    const file = mod.entry;
    const tree = file.tree;

    const call = tree.root[0].type.expression.type.call;
    try testing.expectEqualStrings("add", call.target.type.identifier);
    try testing.expect(call.arguments.len == 3);
    try testing.expect(call.arguments[0].type.number == 1);
    try testing.expect(call.arguments[1].type.binary.operator == .multiply);
    try testing.expect(call.arguments[2].type.binary.right.type.number == 5);
}

test "Parse For loop" {
    const input =
        \\ for list |item| {
        \\ }
        \\ for 0..10 |i| {
        \\ }
        \\ for map |keyValue| {
        \\ }
    ;

    const mod = try parseSource(input);
    defer mod.deinit();
    const file = mod.entry;
    const tree = file.tree;

    var loop = tree.root[0].type.@"for";
    try testing.expectEqualStrings("list", loop.iterator.type.identifier);
    try testing.expectEqualStrings("item", loop.capture);

    loop = tree.root[1].type.@"for";
    try testing.expect(loop.iterator.type.range.left.type.number == 0);
    try testing.expect(loop.iterator.type.range.right.type.number == 10);
    try testing.expectEqualStrings("i", loop.capture);

    loop = tree.root[2].type.@"for";
    try testing.expectEqualStrings("map", loop.iterator.type.identifier);
    try testing.expectEqualStrings("keyValue", loop.capture);
}

test "Parse While loop" {
    const input =
        \\ while x < y { x }"
    ;
    const mod = try parseSource(input);
    defer mod.deinit();
    const file = mod.entry;
    const tree = file.tree;

    const loop = tree.root[0].type.@"while";
    try testing.expect(loop.condition.type.binary.operator == .less_than);
    try testing.expectEqualStrings("x", loop.body[0].type.expression.type.identifier);
}

test "Parse Indexing" {
    const input =
        \\ test.other.third.final
    ;
    const mod = try parseSource(input);
    defer mod.deinit();
    const file = mod.entry;
    const tree = file.tree;

    var idx = tree.root[0].type.expression.type.indexer;
    const values = [_][]const u8{ "final", "third", "other" };
    inline for (values) |v| {
        try testing.expectEqualStrings(v, idx.index.type.identifier);
        if (idx.target.type == .indexer) {
            idx = idx.target.type.indexer;
        } else try testing.expectEqualStrings("test", idx.target.type.identifier);
    }
}

test "Parse Bough" {
    const input =
        \\ === BOUGH {
        \\     :Speaker: "Text goes here" # tagline #tagother#taglast
        \\ }
    ;
    const mod = try parseSource(input);
    defer mod.deinit();
    const file = mod.entry;
    const tree = file.tree;
    const bough = tree.root[0].type.bough;
    try testing.expectEqualStrings(bough.name, "BOUGH");

    const line = bough.body[0].type.dialogue;
    try testing.expectEqualStrings("Speaker", line.speaker.?);
    try testing.expectEqualStrings("Text goes here", line.content.type.string.value);
    try testing.expectEqualStrings("tagline", line.tags[0]);
    try testing.expectEqualStrings("tagother", line.tags[1]);
    try testing.expectEqualStrings("taglast", line.tags[2]);
}

test "Parse No Speaker" {
    const input =
        \\  === BOUGH {
        \\      :: "Text goes here"
        \\  }
    ;
    const mod = try parseSource(input);
    defer mod.deinit();
    const file = mod.entry;
    const tree = file.tree;
    const line = tree.root[0].type.bough.body[0].type.dialogue;
    try testing.expect(line.speaker == null);
    try testing.expectEqualStrings("Text goes here", line.content.type.string.value);
}

test "Parse divert" {
    const input =
        \\  === BOUGH {}
        \\  => BOUGH
    ;
    const mod = try parseSource(input);
    defer mod.deinit();
    const file = mod.entry;
    const tree = file.tree;
    const divert = tree.root[1].type.divert.path;
    try testing.expectEqualStrings("BOUGH", divert[0]);
}

test "Parse Forks" {
    const input =
        \\  === BOUGH {
        \\      fork {
        \\          ~ "choice 1" {
        \\              :Other: "Response"
        \\          }
        \\          ~ "choice 2" => END
        \\      }
        \\  }
        \\  === END {}
    ;
    const mod = try parseSource(input);
    defer mod.deinit();
    const file = mod.entry;
    const tree = file.tree;

    const fork = tree.root[0].type.bough.body[0].type.fork;
    try testing.expect(fork.name == null);

    var choice = fork.body[0].type.choice;
    try testing.expectEqualStrings("choice 1", choice.content.type.string.value);
    const line = choice.body[0].type.dialogue;
    try testing.expectEqualStrings("Other", line.speaker.?);
    try testing.expectEqualStrings("Response", line.content.type.string.value);

    choice = fork.body[1].type.choice;
    try testing.expectEqualStrings("choice 2", choice.content.type.string.value);
    try testing.expect(choice.body[0].type == .divert);
}

test "Parse Divert" {
    const input =
        \\  === BOUGH {
        \\      => INNER
        \\      === INNER {
        \\          :speaker: "Inner"
        \\      }
        \\  }
        \\  === END {}
    ;
    const mod = try parseSource(input);
    defer mod.deinit();
    const file = mod.entry;
    const tree = file.tree;
    const body = tree.root[0].type.bough.body;
    const divert = body[0].type.divert;
    try testing.expectEqualStrings("INNER", divert.path[0]);
    const inner = tree.root[0].type.bough.body[1].type.bough.body[0];
    try testing.expectEqualStrings("Inner", inner.type.dialogue.content.type.string.value);
}

test "Parse Inline Code" {
    const input =
        \\  === BOUGH {
        \\      :Speaker: "{sayHello()}, how are you?"
        \\  }
    ;
    const mod = try parseSource(input);
    defer mod.deinit();
    const file = mod.entry;
    const tree = file.tree;
    const dialogue = tree.root[0].type.bough.body[0].type.dialogue;
    const string = dialogue.content.type.string;
    try testing.expectEqualStrings("sayHello", string.expressions[0].type.call.target.type.identifier);
    try testing.expectEqualStrings("{0}, how are you?", string.value);
}
