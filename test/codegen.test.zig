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
const Emitter = backend.emit.Emitter;
const OpCode = backend.OpCode;
const builtins = topi.runtime.builtins;
const Module = topi.module.Module;
const File = topi.module.File;
const Token = topi.frontend.Token;

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

// -- debug_info -----------------------------------------------------------
//
// Markers in `Chunk.debug_markers` are kept as one entry per
// `(file_index, line)` run. `Chunk.debugInfo()`
// flattens them into the on-disk `[]DebugInfo{file, []Range{start,end,line}}`
// the VM uses to attribute an `ip` back to a source line. These tests pin
// both layers.

fn tok(file_index: usize, line: usize) Token {
    return .{
        .token_type = .eof,
        .start = 0,
        .end = 0,
        .line = line,
        .column = 0,
        .file_index = file_index,
    };
}

fn newSingleFileModule() !*Module {
    const mod = try Module.initEmpty(allocator, std.testing.io);
    errdefer mod.deinit();
    const file = try mod.arena.allocator().create(File);
    file.* = .{ .path = "main.topi", .name = "main", .dir_name = "", .source = "", .module = mod };
    try mod.includes.putNoClobber(allocator, file.path, file);
    return mod;
}

fn newMultiFileModule() !*Module {
    const mod = try Module.initEmpty(allocator, std.testing.io);
    errdefer mod.deinit();
    const a = try mod.arena.allocator().create(File);
    a.* = .{ .path = "a.topi", .name = "a", .dir_name = "", .source = "", .module = mod };
    try mod.includes.putNoClobber(allocator, a.path, a);
    const b = try mod.arena.allocator().create(File);
    b.* = .{ .path = "b.topi", .name = "b", .dir_name = "", .source = "", .module = mod };
    try mod.includes.putNoClobber(allocator, b.path, b);
    return mod;
}

test "Emitter merges markers within a (file,line) run" {
    var mod = try newSingleFileModule();
    defer mod.deinit();
    var em = try Emitter.init(allocator, mod);
    defer em.deinit();

    try em.writeOp(.true, tok(0, 1));
    try em.writeOp(.pop, tok(0, 1));
    try em.writeOp(.add, tok(0, 1));

    try testing.expectEqual(@as(usize, 1), em.chunk.debug_markers.items.len);
    try testing.expectEqual(@as(u32, 0), em.chunk.debug_markers.items[0].ip);
    try testing.expectEqual(@as(usize, 3), em.chunk.instructions.items.len);
}

test "Emitter starts a new marker on line change" {
    var mod = try newSingleFileModule();
    defer mod.deinit();
    var em = try Emitter.init(allocator, mod);
    defer em.deinit();

    try em.writeOp(.true, tok(0, 1));
    try em.writeOp(.pop, tok(0, 2));
    try em.writeOp(.add, tok(0, 2));
    try em.writeOp(.not, tok(0, 3));

    const markers = em.chunk.debug_markers.items;
    try testing.expectEqual(@as(usize, 3), markers.len);
    try testing.expectEqual(@as(u32, 0), markers[0].ip);
    try testing.expectEqual(@as(u32, 1), markers[1].ip);
    try testing.expectEqual(@as(u32, 2), markers[1].line);
    try testing.expectEqual(@as(u32, 3), markers[2].ip);
    try testing.expectEqual(@as(u32, 3), markers[2].line);
}

test "Emitter does not push a marker for operand bytes" {
    var mod = try newSingleFileModule();
    defer mod.deinit();
    var em = try Emitter.init(allocator, mod);
    defer em.deinit();

    try em.writeOp(.constant, tok(0, 1));
    _ = try em.writeInt(u32, 42, tok(0, 1));
    try em.writeOp(.constant, tok(0, 1));
    _ = try em.writeInt(u32, 7, tok(0, 1));

    try testing.expectEqual(@as(usize, 1), em.chunk.debug_markers.items.len);
    try testing.expectEqual(@as(usize, 10), em.chunk.instructions.items.len);
}

test "removeLast pops marker only when the op started its own run" {
    var mod = try newSingleFileModule();
    defer mod.deinit();
    var em = try Emitter.init(allocator, mod);
    defer em.deinit();

    // Three ops in a single run — only the first pushed a marker.
    try em.writeOp(.true, tok(0, 1));
    try em.writeOp(.pop, tok(0, 1));
    try em.writeOp(.pop, tok(0, 1));
    try testing.expectEqual(@as(usize, 1), em.chunk.debug_markers.items.len);

    // Continuation removal: marker stays put, instructions truncate.
    try em.removeLast(.pop);
    try testing.expectEqual(@as(usize, 1), em.chunk.debug_markers.items.len);
    try testing.expectEqual(@as(usize, 2), em.chunk.instructions.items.len);

    // Open a new run on a new line; the new op owns the only marker
    // for its run and removeLast must drop it.
    try em.writeOp(.add, tok(0, 2));
    try testing.expectEqual(@as(usize, 2), em.chunk.debug_markers.items.len);
    try em.removeLast(.add);
    try testing.expectEqual(@as(usize, 1), em.chunk.debug_markers.items.len);
    try testing.expectEqual(@as(usize, 2), em.chunk.instructions.items.len);
}

test "debugInfo collapses to one range per run with correct boundaries" {
    var mod = try newSingleFileModule();
    defer mod.deinit();
    var em = try Emitter.init(allocator, mod);
    defer em.deinit();

    // line 1: 1 byte; line 2: 5 bytes (.constant + u32); line 3: 1 byte.
    try em.writeOp(.true, tok(0, 1));
    try em.writeOp(.constant, tok(0, 2));
    _ = try em.writeInt(u32, 0, tok(0, 2));
    try em.writeOp(.pop, tok(0, 3));

    const info = try em.chunk.debugInfo(allocator);
    defer {
        for (info) |*d| d.deinit();
        allocator.free(info);
    }
    try testing.expectEqual(@as(usize, 1), info.len);
    const ranges = info[0].ranges.items;
    try testing.expectEqual(@as(usize, 3), ranges.len);
    try testing.expectEqual(@as(u32, 0), ranges[0].start);
    try testing.expectEqual(@as(u32, 1), ranges[0].end);
    try testing.expectEqual(@as(u32, 1), ranges[0].line);
    try testing.expectEqual(@as(u32, 1), ranges[1].start);
    try testing.expectEqual(@as(u32, 6), ranges[1].end);
    try testing.expectEqual(@as(u32, 2), ranges[1].line);
    try testing.expectEqual(@as(u32, 6), ranges[2].start);
    try testing.expectEqual(@as(u32, 7), ranges[2].end);
    try testing.expectEqual(@as(u32, 3), ranges[2].line);
}

test "debugInfo groups ranges per file across interleaved emits" {
    var mod = try newMultiFileModule();
    defer mod.deinit();
    var em = try Emitter.init(allocator, mod);
    defer em.deinit();

    // a:1, b:1, a:2 — three runs, two distinct files. The second
    // visit to file `a` must reuse the same DebugInfo entry, not
    // create a new one.
    try em.writeOp(.true, tok(0, 1));
    try em.writeOp(.pop, tok(1, 1));
    try em.writeOp(.add, tok(0, 2));

    const info = try em.chunk.debugInfo(allocator);
    defer {
        for (info) |*d| d.deinit();
        allocator.free(info);
    }
    try testing.expectEqual(@as(usize, 2), info.len);
    try testing.expectEqualStrings("a.topi", info[0].file);
    try testing.expectEqualStrings("b.topi", info[1].file);
    try testing.expectEqual(@as(usize, 2), info[0].ranges.items.len);
    try testing.expectEqual(@as(usize, 1), info[1].ranges.items.len);

    // a's ranges remain in emit order: [0,1) line 1, then [2,3) line 2.
    try testing.expectEqual(@as(u32, 0), info[0].ranges.items[0].start);
    try testing.expectEqual(@as(u32, 1), info[0].ranges.items[0].end);
    try testing.expectEqual(@as(u32, 1), info[0].ranges.items[0].line);
    try testing.expectEqual(@as(u32, 2), info[0].ranges.items[1].start);
    try testing.expectEqual(@as(u32, 3), info[0].ranges.items[1].end);
    try testing.expectEqual(@as(u32, 2), info[0].ranges.items[1].line);

    // b's single range covers the middle byte at line 1.
    try testing.expectEqual(@as(u32, 1), info[1].ranges.items[0].start);
    try testing.expectEqual(@as(u32, 2), info[1].ranges.items[0].end);
    try testing.expectEqual(@as(u32, 1), info[1].ranges.items[0].line);
}

test "debugInfo on empty chunk returns no entries" {
    var mod = try newSingleFileModule();
    defer mod.deinit();
    var em = try Emitter.init(allocator, mod);
    defer em.deinit();

    const info = try em.chunk.debugInfo(allocator);
    defer allocator.free(info);
    try testing.expectEqual(@as(usize, 0), info.len);
}

test "Codegen attaches debug_info to top-level program" {
    // Three statements on three lines so we get at least three ranges
    // covering distinct ips.
    var mod = try newModuleWithSource(
        \\const a = 1
        \\const b = 2
        \\const c = a + b
    );
    defer mod.deinit();
    var bc = try compileViaIr(mod);
    defer bc.free(allocator);

    try testing.expect(bc.debug_info.len >= 1);
    var total_ranges: usize = 0;
    for (bc.debug_info) |info| total_ranges += info.ranges.items.len;
    try testing.expect(total_ranges >= 3);
    // Final range must extend to the end of the instruction stream so
    // ip lookups against the trailing bytes (e.g. the implicit `.end`
    // op) still hit a range.
    var max_end: u32 = 0;
    for (bc.debug_info) |info| {
        for (info.ranges.items) |r| {
            if (r.end > max_end) max_end = r.end;
        }
    }
    try testing.expectEqual(@as(u32, @intCast(bc.instructions.len)), max_end);
}

test "Codegen attaches debug_info to function bodies" {
    var mod = try newModuleWithSource(
        \\fn add |a, b| {
        \\    return a + b
        \\}
        \\add(1, 2)
    );
    defer mod.deinit();
    var bc = try compileViaIr(mod);
    defer bc.free(allocator);

    // Find the compiled `add` function in the constants pool.
    var found: ?*const topi.types.Value.Obj = null;
    for (bc.constants) |c| {
        if (c == .obj and c.obj.data == .function) {
            const f = c.obj.data.function;
            if (f.name) |n| if (std.mem.eql(u8, n, "add")) {
                found = c.obj;
            };
        }
    }
    const fn_obj = found orelse return error.TestFailedToFindFunction;
    const f = fn_obj.data.function;
    try testing.expect(f.debug_info.len >= 1);
    var max_end: u32 = 0;
    for (f.debug_info) |info| {
        for (info.ranges.items) |r| {
            if (r.end > max_end) max_end = r.end;
        }
    }
    try testing.expectEqual(@as(u32, @intCast(f.instructions.len)), max_end);
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
