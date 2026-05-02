//! IR-driven codegen smoke tests. Each case exercises a path through
//! `Codegen.emit` and confirms it produces a `Bytecode` without erroring.
//! Behavioral coverage lives in `vm.test.zig`; IR-level structural
//! coverage in `ir.test.zig`; diagnostic coverage in `compiler.test.zig`.

const std = @import("std");

const topi = @import("topi");
const ir = topi.ir;
const backend = topi.backend;
const Bytecode = backend.Bytecode;
const Codegen = backend.Codegen;
const builtins = topi.runtime.builtins;
const Module = topi.module.Module;
const File = topi.module.File;

const testing = std.testing;
const allocator = testing.allocator;
const cl = builtins.functions.values().len;

fn newModuleWithSource(source: []const u8) !*Module {
    const mod = try Module.initEmpty(allocator, std.testing.io);
    errdefer mod.deinit();
    const file = try mod.arena.allocator().create(File);
    file.* = .{
        .path = "",
        .name = "",
        .dir_name = "",
        .source = source,
        .module = mod,
    };
    mod.entry = file;
    try mod.includes.putNoClobber(allocator, file.path, file);
    return mod;
}

fn compileViaIr(mod: *Module) !Bytecode {
    try mod.entry.loadSource();
    try mod.entry.buildTree();
    var program = try ir.lower(mod.arena.allocator(), mod);
    defer program.deinit();
    try ir.validate(mod.arena.allocator(), mod, &program);
    return try Codegen.emit(allocator, mod, &program);
}

fn expectCompiles(cases: []const []const u8) !void {
    for (cases) |src| {
        errdefer std.log.warn("case: {s}", .{src});
        var mod = try newModuleWithSource(src);
        defer mod.deinit();
        var bc = try compileViaIr(mod);
        bc.free(allocator);
    }
}

test "Codegen empty program" {
    var mod = try newModuleWithSource("");
    defer mod.deinit();
    var bc = try compileViaIr(mod);
    defer bc.free(allocator);

    // Builtins always populate the head of the constants pool.
    try testing.expectEqual(cl, bc.constants.len);
    try testing.expectEqual(@as(usize, 0), bc.instructions.len);
    try testing.expectEqual(@as(usize, 0), bc.global_symbols.len);
    try testing.expectEqual(@as(usize, 0), bc.locals_count);
}

test "Codegen expressions" {
    try expectCompiles(&.{
        "1 + 2",
        "1 - 2",
        "3 * 4",
        "10 / 2",
        "5 % 2",
        "1 == 2",
        "1 != 2",
        "1 < 2",
        "1 <= 2",
        "1 > 2",
        "1 >= 2",
        "true and false",
        "true or false",
        "!true",
        "-5",
        "true",
        "false",
        "0..10",
    });
}

test "Codegen if" {
    try expectCompiles(&.{
        "if true { 10 }",
        "if true { 10 } else { 20 }",
        "if 1 == 1 { 5 } else { 6 }",
    });
}

test "Codegen while" {
    try expectCompiles(&.{
        "while false { }",
        "while 1 > 2 { }",
    });
}

test "Codegen for" {
    try expectCompiles(&.{
        "for 0..5 |x| { 1 }",
    });
}

test "Codegen switch" {
    try expectCompiles(&.{
        "var n = 1 switch n { 1: 10, 2: 20, else: 30 }",
        "var n = 2 switch n { 1, 2: 10, else: 20 }",
    });
}

test "Codegen text/collections/index" {
    try expectCompiles(&.{
        \\const greeting = "hello"
        ,
        \\const x = 1
        \\const greeting = "hi {x}!"
        ,
        "const xs = List{1, 2, 3}",
        "const s = Set{1, 2, 3}",
        \\const m = Map{"a": 1, "b": 2}
        ,
        "const xs = List{1, 2, 3} const y = xs[0]",
        "const xs = List{1, 2, 3} const n = xs.count",
    });
}

test "Codegen assignment" {
    try expectCompiles(&.{
        "var x = 0 x = 5",
        "var x = 0 x = 1 + 2",
        "var x = 0 x += 5",
        "var x = 1 x -= 1",
        "var x = 2 x *= 3",
        "var x = 6 x /= 2",
        "var x = 7 x %= 2",
        "var xs = List{1, 2, 3} xs[0] = 99",
        "var xs = List{1, 2, 3} xs[0] += 5",
    });
}

test "Codegen function" {
    try expectCompiles(&.{
        "fn five || { return 5 }",
        "fn five || { return 5 } five()",
        "fn add |a, b| { return a + b } add(1, 2)",
        "fn id |x| { return x } id(1) + id(2)",
        "fn fact |n| { if n <= 1 { return 1 } return n * fact(n - 1) }",
    });
}

test "Codegen class" {
    try expectCompiles(&.{
        "class Pt { x = 0 y = 0 }",
        "class Pt { x = 0 y = 0 } var p = new Pt{}",
    });
}

test "Codegen enum" {
    try expectCompiles(&.{
        "enum Color { Red, Green, Blue }",
        "enum Color { Red, Green, Blue } const c = Color.Green",
    });
}

test "Codegen narrative" {
    try expectCompiles(&.{
        \\=== Start @AAAAAAAA-AAAAAAAA {
        \\}
        ,
        \\=== Start @AAAAAAAA-AAAAAAAA {
        \\    => Other
        \\}
        \\=== Other @BBBBBBBB-BBBBBBBB {
        \\    fin
        \\}
        ,
        \\=== Start @AAAAAAAA-AAAAAAAA {
        \\    =>^ Other
        \\}
        \\=== Other @BBBBBBBB-BBBBBBBB {
        \\    fin
        \\}
        ,
        \\=== Start @AAAAAAAA-AAAAAAAA {
        \\    => Other
        \\}
        \\=== Other @BBBBBBBB-BBBBBBBB {
        \\    => Start
        \\}
        ,
    });
}

test "Codegen var_decl" {
    try expectCompiles(&.{
        "const x = 5",
        "var y = 10",
        "const a = 1 + 2",
        "var b = true",
        "const x = 1 const y = 2 const z = x + y",
    });
}

fn constantsCount(src: []const u8) !usize {
    var mod = try newModuleWithSource(src);
    defer mod.deinit();
    var bc = try compileViaIr(mod);
    defer bc.free(allocator);
    return bc.constants.len;
}

test "Codegen dedups number literals" {
    const one = try constantsCount("const a = 5");
    const two = try constantsCount("const a = 5 const b = 5");
    try testing.expectEqual(one, two);
}

test "Codegen dedups string literals" {
    const one = try constantsCount(
        \\const a = "hello"
    );
    const two = try constantsCount(
        \\const a = "hello" const b = "hello"
    );
    try testing.expectEqual(one, two);
}

test "Codegen does not dedup interpolated strings" {
    const one = try constantsCount(
        \\const x = 1
        \\const a = "v={x}"
    );
    const two = try constantsCount(
        \\const x = 1
        \\const a = "v={x}" const b = "v={x}"
    );
    try testing.expectEqual(one + 1, two);
}

test "Codegen dedups speaker names" {
    const one = try constantsCount(
        \\=== B @AAAAAAAA-AAAAAAAA {
        \\    :P: "a"
        \\}
    );
    const two = try constantsCount(
        \\=== B @AAAAAAAA-AAAAAAAA {
        \\    :P: "a"
        \\    :P: "b"
        \\}
    );
    // One added entry for the second line's text Obj (UUID-unique), but the
    // speaker name "P" is name-keyed so it does not grow.
    try testing.expectEqual(one + 1, two);
}
