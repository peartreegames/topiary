const std = @import("std");
const topi = @import("topi");

const ir = topi.ir;
const Module = topi.module.Module;
const File = topi.module.File;

const testing = std.testing;
const allocator = testing.allocator;

/// Parse a string of source into a Module with an empty include map and a
/// single file. Caller owns the returned Module and must `deinit` it.
fn parseSource(source: []const u8) !*Module {
    const mod = try Module.initEmpty(allocator, std.testing.io);
    errdefer mod.deinit();
    const file = try mod.arena.allocator().create(File);
    file.* = .{
        .module = mod,
        .path = "_test_",
        .name = "_test_",
        .dir_name = ".",
        .source = source,
    };
    mod.entry = file;
    try mod.includes.putNoClobber(allocator, file.path, file);
    try file.buildTree();
    return mod;
}

fn lowerSource(source: []const u8) !struct { mod: *Module, program: ir.Program } {
    const mod = try parseSource(source);
    errdefer mod.deinit();
    const program = try ir.lower(allocator, mod);
    return .{ .mod = mod, .program = program };
}

test "lower empty program" {
    var result = try lowerSource("");
    defer result.mod.deinit();
    var program = result.program;
    defer program.deinit();

    try testing.expectEqual(@as(usize, 0), program.body.len);
    try testing.expectEqual(@as(usize, 0), program.anchors.count());
}

test "lower bough with fin emits visit" {
    var result = try lowerSource(
        \\=== Intro {
        \\    fin
        \\}
    );
    defer result.mod.deinit();
    var program = result.program;
    defer program.deinit();

    try testing.expectEqual(@as(usize, 1), program.body.len);
    const bough = program.body[0].kind.bough;
    try testing.expectEqualStrings("Intro", bough.name);
    try testing.expectEqualStrings("Intro", bough.anchor.path);
    try testing.expectEqual(ir.AnchorRef.Kind.bough, bough.anchor.kind.?);

    // body = [Visit, fin]
    try testing.expectEqual(@as(usize, 2), bough.body.len);
    try testing.expect(bough.body[0].kind == .visit);
    try testing.expectEqualStrings("Intro", bough.body[0].kind.visit.target.path);
    try testing.expect(bough.body[1].kind == .fin);

    // anchor table populated
    try testing.expectEqual(@as(usize, 1), program.anchors.count());
}

test "lower var declaration sets type hint" {
    var result = try lowerSource("var x = 1");
    defer result.mod.deinit();
    var program = result.program;
    defer program.deinit();

    try testing.expectEqual(@as(usize, 1), program.body.len);
    const v = program.body[0].kind.var_decl;
    try testing.expectEqualStrings("x", v.name);
    try testing.expect(v.slot == .global);
    try testing.expect(v.slot.global.var_type == .number);
}

test "lower forward divert resolves via validation pass" {
    var result = try lowerSource(
        \\=== A {
        \\    => B
        \\}
        \\=== B {
        \\    fin
        \\}
    );
    defer result.mod.deinit();
    var program = result.program;
    defer program.deinit();

    const bough_a = program.body[0].kind.bough;
    // body = [Visit, divert]
    const divert = bough_a.body[1].kind.divert;
    try testing.expectEqualStrings("B", divert.target.path);
    try testing.expectEqual(ir.AnchorRef.Kind.bough, divert.target.kind.?);
}

test "lower unresolved divert emits error" {
    const mod = try parseSource(
        \\=== A {
        \\    => Nowhere
        \\}
    );
    defer mod.deinit();

    var program = try ir.lower(allocator, mod);
    defer program.deinit();

    var found_unknown = false;
    for (mod.errors.list.items) |e| {
        if (std.mem.indexOf(u8, e.fmt, "Unknown name 'Nowhere'") != null) {
            found_unknown = true;
            break;
        }
    }
    try testing.expect(found_unknown);
}

test "lower backup divert produces backup_divert kind" {
    var result = try lowerSource(
        \\=== A {
        \\    =>^ B
        \\}
        \\=== B {
        \\    fin
        \\}
    );
    defer result.mod.deinit();
    var program = result.program;
    defer program.deinit();

    const bough_a = program.body[0].kind.bough;
    try testing.expect(bough_a.body[1].kind == .backup_divert);
}

test "lower interpolated string produces segments" {
    var result = try lowerSource(
        \\var name = "World"
        \\=== Hello {
        \\    :: "Hi {name}!"
        \\}
    );
    defer result.mod.deinit();
    var program = result.program;
    defer program.deinit();

    const bough = program.body[1].kind.bough;
    // body = [Visit, line]
    const line = bough.body[1].kind.line;
    // segments = [.literal "Hi ", .interp <name>, .literal "!"]
    try testing.expectEqual(@as(usize, 3), line.segments.len);
    try testing.expect(line.segments[0] == .literal);
    try testing.expectEqualStrings("Hi ", line.segments[0].literal);
    try testing.expect(line.segments[1] == .interp);
    try testing.expect(line.segments[2] == .literal);
    try testing.expectEqualStrings("!", line.segments[2].literal);

    // The interpolated expression resolves to a global load of "name".
    const interp_expr = line.segments[1].interp;
    try testing.expect(interp_expr.kind == .load_global);
    try testing.expectEqualStrings("name", interp_expr.kind.load_global.name);
}

test "lower for loop defines capture slot" {
    var result = try lowerSource(
        \\var xs = List{1, 2, 3}
        \\=== Loop {
        \\    for xs |x| {
        \\        :: "{x}"
        \\    }
        \\}
    );
    defer result.mod.deinit();
    var program = result.program;
    defer program.deinit();

    const bough = program.body[1].kind.bough;
    // body = [Visit, for]
    const for_stmt = bough.body[1].kind.@"for";
    try testing.expectEqualStrings("x", for_stmt.capture_name);
    try testing.expect(for_stmt.capture_slot == .local);
}

test "lower class produces class stmt with methods" {
    var result = try lowerSource(
        \\class Point {
        \\    x = 0,
        \\    y = 0,
        \\    fn greet || { return "hi" }
        \\}
    );
    defer result.mod.deinit();
    var program = result.program;
    defer program.deinit();

    try testing.expectEqual(@as(usize, 1), program.body.len);
    const class = program.body[0].kind.class;
    try testing.expectEqualStrings("Point", class.name);
    try testing.expectEqual(@as(usize, 2), class.field_names.len);
    try testing.expectEqual(@as(usize, 1), class.methods.len);
    try testing.expectEqual(ir.AnchorRef.Kind.class, class.anchor.kind.?);
}

test "lower dump round-trip does not panic" {
    var result = try lowerSource(
        \\=== Story {
        \\    === Chapter {
        \\        => Sibling
        \\    }
        \\    === Sibling {
        \\        fin
        \\    }
        \\}
    );
    defer result.mod.deinit();
    var program = result.program;
    defer program.deinit();

    var buf: [4096]u8 = undefined;
    var fbw = std.Io.Writer.fixed(&buf);
    try ir.dump(&program, &fbw);
    const out = fbw.buffered();
    try testing.expect(std.mem.indexOf(u8, out, "bough") != null);
    try testing.expect(std.mem.indexOf(u8, out, "Story.Chapter") != null);
}
