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
    try testing.expect(interp_expr.kind == .load);
    try testing.expect(interp_expr.kind.load == .global);
    try testing.expectEqualStrings("name", interp_expr.kind.load.global.name);
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

// ===========================================================================
// Semantic diagnostics — emit-time and post-walk
// ===========================================================================

const CompilerErr = topi.backend.CompilerErr;

fn hasError(mod: *Module, needle: []const u8, severity: CompilerErr.Severity) bool {
    for (mod.errors.list.items) |e| {
        if (e.severity != severity) continue;
        if (std.mem.indexOf(u8, e.fmt, needle) != null) return true;
    }
    return false;
}

test "diagnostic: builtin shadow flags variable named like a builtin" {
    var result = try lowerSource("var rnd = 0");
    defer result.mod.deinit();
    var program = result.program;
    defer program.deinit();
    try testing.expect(hasError(result.mod, "is a builtin function", .err));
}

test "diagnostic: ordinary variable does not trigger builtin shadow" {
    var result = try lowerSource("var something = 0");
    defer result.mod.deinit();
    var program = result.program;
    defer program.deinit();
    try testing.expect(!hasError(result.mod, "is a builtin function", .err));
}

test "diagnostic: assigning to a constant variable errors" {
    var result = try lowerSource(
        \\const x = 0
        \\=== Main {
        \\    x = 1
        \\    fin
        \\}
    );
    defer result.mod.deinit();
    var program = result.program;
    defer program.deinit();
    try testing.expect(hasError(result.mod, "Cannot assign to constant variable 'x'", .err));
}

test "diagnostic: assigning to mutable variable is fine" {
    var result = try lowerSource(
        \\var x = 0
        \\=== Main {
        \\    x = 1
        \\    fin
        \\}
    );
    defer result.mod.deinit();
    var program = result.program;
    defer program.deinit();
    try testing.expect(!hasError(result.mod, "Cannot assign", .err));
}

test "diagnostic: arity mismatch on direct call" {
    var result = try lowerSource(
        \\fn add |a, b| { return a + b }
        \\=== Main {
        \\    var x = add(1)
        \\    fin
        \\}
    );
    defer result.mod.deinit();
    var program = result.program;
    defer program.deinit();
    try testing.expect(hasError(result.mod, "'add' expects 2 argument(s), but got 1", .err));
}

test "diagnostic: matching arity does not error" {
    var result = try lowerSource(
        \\fn add |a, b| { return a + b }
        \\=== Main {
        \\    var x = add(1, 2)
        \\    fin
        \\}
    );
    defer result.mod.deinit();
    var program = result.program;
    defer program.deinit();
    try testing.expect(!hasError(result.mod, "expects", .err));
}

test "diagnostic: instance with unknown field errors" {
    var result = try lowerSource(
        \\class Point { x = 0, y = 0 }
        \\=== Main {
        \\    var p = new Point{ x = 1, z = 9 }
        \\    fin
        \\}
    );
    defer result.mod.deinit();
    var program = result.program;
    defer program.deinit();
    try testing.expect(hasError(result.mod, "Class 'Point' has no field 'z'", .err));
}

test "diagnostic: instance with valid fields is fine" {
    var result = try lowerSource(
        \\class Point { x = 0, y = 0 }
        \\=== Main {
        \\    var p = new Point{ x = 1, y = 2 }
        \\    fin
        \\}
    );
    defer result.mod.deinit();
    var program = result.program;
    defer program.deinit();
    try testing.expect(!hasError(result.mod, "has no field", .err));
}

test "diagnostic: unreachable code after fin warns" {
    var result = try lowerSource(
        \\=== Main {
        \\    fin
        \\    :: "never"
        \\}
    );
    defer result.mod.deinit();
    var program = result.program;
    defer program.deinit();
    try testing.expect(hasError(result.mod, "Unreachable code after 'fin'", .warn));
}

test "diagnostic: bough after divert is not flagged unreachable" {
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
    try testing.expect(!hasError(result.mod, "Unreachable", .warn));
}

test "diagnostic: fork choice without exit warns" {
    var result = try lowerSource(
        \\=== Main {
        \\    fork {
        \\        ~ "Stay" {
        \\            :: "ok"
        \\        }
        \\        ~ "Leave" {
        \\            => Done
        \\        }
        \\    }
        \\}
        \\=== Done { fin }
    );
    defer result.mod.deinit();
    var program = result.program;
    defer program.deinit();
    try testing.expect(hasError(result.mod, "execution will end silently", .warn));
}

test "lower dot-access produces field expr" {
    var result = try lowerSource(
        \\var l = List{1, 2}
        \\var z = l.count
    );
    defer result.mod.deinit();
    var program = result.program;
    defer program.deinit();

    const z_init = program.body[1].kind.var_decl.initializer;
    try testing.expect(z_init.kind == .field);
    try testing.expectEqualStrings("count", z_init.kind.field.name);
}

test "lower bracket-index still produces index expr" {
    var result = try lowerSource(
        \\var l = List{1, 2}
        \\var z = l[0]
    );
    defer result.mod.deinit();
    var program = result.program;
    defer program.deinit();

    const z_init = program.body[1].kind.var_decl.initializer;
    try testing.expect(z_init.kind == .index);
}

test "diagnostic: dot-access on built-in method does not error" {
    var result = try lowerSource(
        \\var l = List{1, 2}
        \\var n = l.has(1)
    );
    defer result.mod.deinit();
    var program = result.program;
    defer program.deinit();
    try testing.expect(!hasError(result.mod, "Unknown name 'has'", .err));
}

test "diagnostic: dot-access unknown class field errors" {
    var result = try lowerSource(
        \\class P { x = 0 }
        \\var p = new P{}
        \\=== Main {
        \\    var z = p.bad
        \\    fin
        \\}
    );
    defer result.mod.deinit();
    var program = result.program;
    defer program.deinit();
    try testing.expect(hasError(result.mod, "Class 'P' does not contain a field 'bad'", .err));
}

test "diagnostic: dot-access valid class field and method does not error" {
    var result = try lowerSource(
        \\class P {
        \\    x = 0,
        \\    fn greet || { return 1 }
        \\}
        \\var p = new P{}
        \\=== Main {
        \\    var a = p.x
        \\    var b = p.greet()
        \\    fin
        \\}
    );
    defer result.mod.deinit();
    var program = result.program;
    defer program.deinit();
    try testing.expect(!hasError(result.mod, "does not contain", .err));
}

test "diagnostic: dot-access unknown collection method errors" {
    var result = try lowerSource(
        \\var l = List{1}
        \\=== Main {
        \\    var z = l.bogus
        \\    fin
        \\}
    );
    defer result.mod.deinit();
    var program = result.program;
    defer program.deinit();
    try testing.expect(hasError(result.mod, "Unknown method 'bogus' on list", .err));
}

test "diagnostic: dot-access unknown string method errors" {
    var result = try lowerSource(
        \\var s = "hello"
        \\=== Main {
        \\    var z = s.bogus
        \\    fin
        \\}
    );
    defer result.mod.deinit();
    var program = result.program;
    defer program.deinit();
    try testing.expect(hasError(result.mod, "Unknown method 'bogus' on string", .err));
}

test "diagnostic: dot-access on number rejects field access" {
    var result = try lowerSource(
        \\var n = 1
        \\=== Main {
        \\    var z = n.foo
        \\    fin
        \\}
    );
    defer result.mod.deinit();
    var program = result.program;
    defer program.deinit();
    try testing.expect(hasError(result.mod, "Cannot access field 'foo' on a number", .err));
}

test "diagnostic: dot-access on unknown type is silent" {
    var result = try lowerSource(
        \\fn g |x| { return x }
        \\=== Main {
        \\    var z = g(1).foo
        \\    fin
        \\}
    );
    defer result.mod.deinit();
    var program = result.program;
    defer program.deinit();
    try testing.expect(!hasError(result.mod, "does not contain", .err));
    try testing.expect(!hasError(result.mod, "Unknown method", .err));
    try testing.expect(!hasError(result.mod, "Cannot access field", .err));
}

test "diagnostic: backup fork choice without exit does not warn" {
    var result = try lowerSource(
        \\=== Main {
        \\    fork^ {
        \\        ~ "Stay" {
        \\            :: "ok"
        \\        }
        \\        ~ "Leave" {
        \\            :: "bye"
        \\        }
        \\    }
        \\}
    );
    defer result.mod.deinit();
    var program = result.program;
    defer program.deinit();
    try testing.expect(!hasError(result.mod, "end silently", .warn));
}
