const std = @import("std");
const topi = @import("topi");

const ir = topi.ir;
const Module = topi.module.Module;
const File = topi.module.File;
const builtins = topi.runtime.builtins;

const testing = std.testing;
const allocator = testing.allocator;

/// Number of anchors lowering pre-registers for builtin functions.
/// Tests that assert anchor counts add this to their expected total.
const builtin_anchors = builtins.functions.kvs.len;

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
    var program = try ir.lower(allocator, mod);
    errdefer program.deinit();
    try ir.validate(allocator, mod, &program);
    return .{ .mod = mod, .program = program };
}

test "lower empty program" {
    var result = try lowerSource("");
    defer result.mod.deinit();
    var program = result.program;
    defer program.deinit();

    try testing.expectEqual(@as(usize, 0), program.body.len);
    try testing.expectEqual(builtin_anchors, program.anchors.count());
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

    // anchor table populated (1 user anchor + builtin pre-registrations)
    try testing.expectEqual(builtin_anchors + 1, program.anchors.count());
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

test "diagnostic: function declaration shadowing a builtin errors" {
    var result = try lowerSource(
        \\fn print || { }
    );
    defer result.mod.deinit();
    var program = result.program;
    defer program.deinit();
    try testing.expect(hasError(result.mod, "'print' is already declared", .err));
}

test "diagnostic: class declaration shadowing a builtin errors" {
    var result = try lowerSource(
        \\class print {}
    );
    defer result.mod.deinit();
    var program = result.program;
    defer program.deinit();
    try testing.expect(hasError(result.mod, "'print' is already declared", .err));
}

test "diagnostic: enum declaration shadowing a builtin errors" {
    var result = try lowerSource(
        \\enum print { A, B }
    );
    defer result.mod.deinit();
    var program = result.program;
    defer program.deinit();
    try testing.expect(hasError(result.mod, "'print' is already declared", .err));
}

test "diagnostic: duplicate class field errors" {
    var result = try lowerSource(
        \\class Foo {
        \\    x = 1,
        \\    x = 2
        \\}
    );
    defer result.mod.deinit();
    var program = result.program;
    defer program.deinit();
    try testing.expect(hasError(result.mod, "'x' is already declared", .err));
}

test "diagnostic: duplicate class method errors" {
    var result = try lowerSource(
        \\class Foo {
        \\    fn bar || { return 1 }
        \\    fn bar || { return 2 }
        \\}
    );
    defer result.mod.deinit();
    var program = result.program;
    defer program.deinit();
    try testing.expect(hasError(result.mod, "'bar' is already declared", .err));
}

test "diagnostic: class field and method with same name errors" {
    var result = try lowerSource(
        \\class Foo {
        \\    bar = 1,
        \\    fn bar || { return 2 }
        \\}
    );
    defer result.mod.deinit();
    var program = result.program;
    defer program.deinit();
    try testing.expect(hasError(result.mod, "'bar' is already declared", .err));
}

test "diagnostic: bare assignment to a builtin errors" {
    var result = try lowerSource(
        \\=== Main {
        \\    print = 1
        \\    fin
        \\}
    );
    defer result.mod.deinit();
    var program = result.program;
    defer program.deinit();
    try testing.expect(hasError(result.mod, "Cannot assign to constant 'print'", .err));
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

test "diagnostic: class field literal defaults are accepted" {
    var result = try lowerSource(
        \\class A {
        \\    n = 0,
        \\    b = true,
        \\    nothing = nil,
        \\    s = "hello",
        \\    items = List{1, 2, 3}
        \\}
    );
    defer result.mod.deinit();
    var program = result.program;
    defer program.deinit();
    try testing.expect(!hasError(result.mod, "Only literal values", .err));
    try testing.expect(!hasError(result.mod, "Interpolated strings", .err));
}

test "diagnostic: class field arithmetic on literals is accepted" {
    var result = try lowerSource(
        \\class A {
        \\    radius = 5,
        \\    diameter = 5 * 2,
        \\    sign = -1,
        \\    flag = !true
        \\}
    );
    defer result.mod.deinit();
    var program = result.program;
    defer program.deinit();
    try testing.expect(!hasError(result.mod, "Only literal values", .err));
}

test "diagnostic: class field enum reference is accepted" {
    var result = try lowerSource(
        \\enum Status { ACTIVE, IDLE }
        \\class A {
        \\    state = Status.ACTIVE
        \\}
    );
    defer result.mod.deinit();
    var program = result.program;
    defer program.deinit();
    try testing.expect(!hasError(result.mod, "Only literal values", .err));
}

test "diagnostic: class field interpolated string errors" {
    var result = try lowerSource(
        \\class A {
        \\    name = "world",
        \\    greeting = "hi {name}"
        \\}
    );
    defer result.mod.deinit();
    var program = result.program;
    defer program.deinit();
    try testing.expect(hasError(result.mod, "Interpolated strings are not allowed as static default values", .err));
}

test "diagnostic: class field with function call errors" {
    var result = try lowerSource(
        \\fn compute || return 5
        \\class A {
        \\    val = compute()
        \\}
    );
    defer result.mod.deinit();
    var program = result.program;
    defer program.deinit();
    try testing.expect(hasError(result.mod, "Only literal values are allowed here", .err));
}

test "diagnostic: class field referencing variable errors" {
    var result = try lowerSource(
        \\var counter = 0
        \\class A {
        \\    seed = counter
        \\}
    );
    defer result.mod.deinit();
    var program = result.program;
    defer program.deinit();
    try testing.expect(hasError(result.mod, "Only literal values are allowed here", .err));
}

test "diagnostic: class field instance constructor is rejected" {
    // `new C{}` isn't statically evaluable — class field defaults must
    // be plain literals or references to existing constants. The IR
    // validator rejects up-front with the same "Only literal values"
    // message used for other non-static forms (previously the AST
    // codegen surfaced this as a confusing NotYetImplemented error).
    var result = try lowerSource(
        \\class Inner { v = 1 }
        \\class Outer {
        \\    nested = new Inner{}
        \\}
    );
    defer result.mod.deinit();
    var program = result.program;
    defer program.deinit();
    try testing.expect(hasError(result.mod, "Only literal values are allowed here", .err));
}

test "diagnostic: class field with non-literal inside list errors" {
    var result = try lowerSource(
        \\fn pick || return 1
        \\class A {
        \\    items = List{1, pick(), 3}
        \\}
    );
    defer result.mod.deinit();
    var program = result.program;
    defer program.deinit();
    try testing.expect(hasError(result.mod, "Only literal values are allowed here", .err));
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

test "diagnostic: enum value access errors on unknown value" {
    var result = try lowerSource(
        \\enum E { A, B }
        \\=== Main {
        \\    var z = E.C
        \\    fin
        \\}
    );
    defer result.mod.deinit();
    var program = result.program;
    defer program.deinit();
    try testing.expect(hasError(result.mod, "Enum 'E' does not contain a value 'C'", .err));
}

test "diagnostic: enum value access on valid value is silent" {
    var result = try lowerSource(
        \\enum E { A, B }
        \\=== Main {
        \\    var z = E.A
        \\    fin
        \\}
    );
    defer result.mod.deinit();
    var program = result.program;
    defer program.deinit();
    try testing.expect(!hasError(result.mod, "does not contain", .err));
}

test "diagnostic: class-as-constant unknown field errors" {
    var result = try lowerSource(
        \\class C { x = 0 }
        \\=== Main {
        \\    var z = C.bogus
        \\    fin
        \\}
    );
    defer result.mod.deinit();
    var program = result.program;
    defer program.deinit();
    try testing.expect(hasError(result.mod, "Class 'C' does not contain a field 'bogus'", .err));
}

test "diagnostic: class-as-constant valid method called directly is silent" {
    var result = try lowerSource(
        \\class C {
        \\    x = 0,
        \\    fn foo || { return 1 }
        \\}
        \\=== Main {
        \\    var z = C.foo()
        \\    fin
        \\}
    );
    defer result.mod.deinit();
    var program = result.program;
    defer program.deinit();
    try testing.expect(!hasError(result.mod, "does not contain", .err));
    try testing.expect(!hasError(result.mod, "Cannot store", .err));
}

test "diagnostic: storing top-level function as value errors" {
    var result = try lowerSource(
        \\fn sum |x, y| return x + y
        \\=== Main {
        \\    var f = sum
        \\    fin
        \\}
    );
    defer result.mod.deinit();
    var program = result.program;
    defer program.deinit();
    try testing.expect(hasError(result.mod, "Cannot store function 'sum' as a value", .err));
}

test "diagnostic: reassigning a variable to a function errors" {
    var result = try lowerSource(
        \\fn sum |x, y| return x + y
        \\fn product |x, y| return x * y
        \\=== Main {
        \\    var f = sum(0, 0)
        \\    f = product
        \\    fin
        \\}
    );
    defer result.mod.deinit();
    var program = result.program;
    defer program.deinit();
    try testing.expect(hasError(result.mod, "Cannot store function 'product' as a value", .err));
}

test "diagnostic: storing class method as value errors" {
    var result = try lowerSource(
        \\class C {
        \\    x = 0,
        \\    fn foo || { return 1 }
        \\}
        \\=== Main {
        \\    var z = C.foo
        \\    fin
        \\}
    );
    defer result.mod.deinit();
    var program = result.program;
    defer program.deinit();
    try testing.expect(hasError(result.mod, "Cannot store method 'foo' as a value", .err));
}

test "diagnostic: storing instance method as value errors" {
    var result = try lowerSource(
        \\class C {
        \\    x = 0,
        \\    fn foo || { return 1 }
        \\}
        \\var c = new C{}
        \\=== Main {
        \\    var z = c.foo
        \\    fin
        \\}
    );
    defer result.mod.deinit();
    var program = result.program;
    defer program.deinit();
    try testing.expect(hasError(result.mod, "Cannot store method 'foo' as a value", .err));
}

test "diagnostic: function as call argument is silent" {
    var result = try lowerSource(
        \\fn double |x| return x * 2
        \\fn apply |f, x| return f(x)
        \\=== Main {
        \\    var n = apply(double, 5)
        \\    fin
        \\}
    );
    defer result.mod.deinit();
    var program = result.program;
    defer program.deinit();
    try testing.expect(!hasError(result.mod, "Cannot store", .err));
}

test "diagnostic: function in list literal errors" {
    var result = try lowerSource(
        \\fn sum |x, y| return x + y
        \\=== Main {
        \\    var fns = List{sum}
        \\    fin
        \\}
    );
    defer result.mod.deinit();
    var program = result.program;
    defer program.deinit();
    try testing.expect(hasError(result.mod, "Cannot store function 'sum' as a value", .err));
}

test "returning a function value is allowed (used for currying)" {
    var result = try lowerSource(
        \\fn sum |x, y| return x + y
        \\fn pick || {
        \\    return sum
        \\}
    );
    defer result.mod.deinit();
    var program = result.program;
    defer program.deinit();
    try testing.expect(!hasError(result.mod, "Cannot store function 'sum' as a value", .err));
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
