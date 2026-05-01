const std = @import("std");
const testing = std.testing;

const topi = @import("topi");
const Value = topi.types.Value;
const ValueType = topi.types.Type;

const Bytecode = topi.backend.Bytecode;

const Vm = topi.runtime.Vm;
const State = topi.runtime.State;

const Module = topi.module.Module;

const compileSource = @import("compiler.test.zig").compileSource;
const initTestVm = @import("vm.test.zig").initTestVm;
const TestRunner = @import("runner.zig").TestRunner;

const allocator = testing.allocator;

// ---------------------------------------------------------------------------
// Exhaustive Value variant round-trip
//
// `Value.serialize` / `Value.deserialize` (src/types/value.zig) must stay in
// lock-step. Classify every `Value` and every `Obj.Data` variant as either
// "constant-eligible" (i.e. participates in serialize/deserialize) or "not".
// Because both switches are exhaustive, adding a new variant to either union
// forces this file to be updated, which in turn forces the author to decide
// whether the new variant needs its own serialize/deserialize prong.
// ---------------------------------------------------------------------------

const ConstantClass = enum { eligible, not_constant };

fn classifyType(t: ValueType) ConstantClass {
    return switch (t) {
        .void, .nil, .bool, .number, .visit, .timestamp, .const_string, .obj => .eligible,
        .range, .map_pair, .enum_value, .ref => .not_constant,
    };
}

fn classifyDataType(d: Value.Obj.DataType) ConstantClass {
    return switch (d) {
        .string,
        .@"enum",
        .list,
        .map,
        .set,
        .function,
        .@"extern",
        .builtin,
        .class,
        .anchor,
        => .eligible,
        .instance => .not_constant,
    };
}

fn roundTrip(value: Value, alloc: std.mem.Allocator) !Value {
    var buffer: std.Io.Writer.Allocating = .init(alloc);
    defer buffer.deinit();
    try value.serialize(&buffer.writer);
    var reader = std.Io.Reader.fixed(buffer.written());
    return Value.deserialize(&reader, alloc);
}

test "Round-trip every constant produced by a representative compile" {
    // Each top-level construct lands at least one new `Obj.DataType` variant
    // in `bytecode.constants`. Builtins are added by the compiler prelude.
    // List/set/map are NOT compile-time constants — they're built at runtime
    // from instructions — so the next test covers them by hand.
    const script =
        \\ === START @A1B2C3D4-E5F6G7H8 {
        \\    :speaker: "Hello" @I9J0K1L2-M3N4O5P6
        \\ }
        \\ enum Color { Red, Blue, Green }
        \\ extern fn ext |x| return x
        \\ class Foo {
        \\     x = 1,
        \\     name = "hello",
        \\ }
        \\ fn add |a, b| { return a + b }
        \\ var n = 42
        \\ var s = "world"
    ;

    var mod = try Module.initEmpty(allocator, std.testing.io);
    defer mod.deinit();
    var bytecode = try compileSource(script, mod);
    defer bytecode.free(allocator);

    var seen_types = std.EnumSet(ValueType).initEmpty();
    var seen_data = std.EnumSet(Value.Obj.DataType).initEmpty();

    for (bytecode.constants) |c| {
        // Fresh arena per constant so the deserialized copy + any owned
        // strings/containers are reclaimed in one shot.
        var arena = std.heap.ArenaAllocator.init(allocator);
        defer arena.deinit();

        // Classification — refuses to compile if a new variant is added to
        // the unions without updating the classifier.
        try testing.expectEqual(ConstantClass.eligible, classifyType(c.tag()));
        seen_types.insert(c.tag());
        if (c == .obj) {
            const tag: Value.Obj.DataType = std.meta.activeTag(c.obj.data);
            try testing.expectEqual(ConstantClass.eligible, classifyDataType(tag));
            seen_data.insert(tag);
        }

        const restored = try roundTrip(c, arena.allocator());
        try testing.expect(c.eql(restored));
    }

    inline for (.{
        ValueType.number,
        ValueType.const_string,
        ValueType.obj,
    }) |t| try testing.expect(seen_types.contains(t));

    // Variants the script is designed to leave in compiled constants.
    inline for (.{
        Value.Obj.DataType.anchor,
        Value.Obj.DataType.@"enum",
        Value.Obj.DataType.class,
        Value.Obj.DataType.function,
        Value.Obj.DataType.builtin,
        Value.Obj.DataType.@"extern",
        Value.Obj.DataType.string,
    }) |d| try testing.expect(seen_data.contains(d));
}

test "Round-trip hand-built list/set/map values" {
    // These container variants don't appear as compile-time constants
    // (`List{...}` etc. compiles to runtime construction opcodes), so
    // exercise them directly to ensure the serialize/deserialize prongs
    // for `.list`, `.set`, and `.map` stay in sync.
    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();
    const a = arena.allocator();

    // list
    {
        var items: std.ArrayList(Value) = .empty;
        try items.appendSlice(a, &.{
            .{ .number = 1 },
            .{ .number = 2 },
            .{ .const_string = try a.dupe(u8, "three") },
        });
        const obj = try a.create(Value.Obj);
        obj.* = .{ .data = .{ .list = items } };
        const v: Value = .{ .obj = obj };

        const restored = try roundTrip(v, a);
        try testing.expect(v.eql(restored));
    }

    // set
    {
        var set: Value.Obj.SetType = .empty;
        try set.put(a, .{ .number = 10 }, {});
        try set.put(a, .{ .number = 20 }, {});
        const obj = try a.create(Value.Obj);
        obj.* = .{ .data = .{ .set = set } };
        const v: Value = .{ .obj = obj };

        const restored = try roundTrip(v, a);
        try testing.expect(v.eql(restored));
    }

    // map
    {
        var map: Value.Obj.MapType = .empty;
        try map.put(a, .{ .const_string = try a.dupe(u8, "key") }, .{ .number = 42 });
        const obj = try a.create(Value.Obj);
        obj.* = .{ .data = .{ .map = map } };
        const v: Value = .{ .obj = obj };

        const restored = try roundTrip(v, a);
        try testing.expect(v.eql(restored));
    }
}

test "Round-trip string with segment table" {
    // Reproduce a compile-time string constant with the layout codegen
    // produces for `"hi {name}!"`: bytes = "hi {0}!", segments =
    // [literal{0,3}, interp:0, literal{6,7}]. After serialize/deserialize
    // the bytes and segment table must survive intact.
    const Segment = topi.types.Segment;
    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();
    const a = arena.allocator();

    const bytes = try a.dupe(u8, "hi {0}!");
    const segments = try a.alloc(Segment, 3);
    segments[0] = .{ .literal = .{ .start = 0, .end = 3 } };
    segments[1] = .{ .interp = 0 };
    segments[2] = .{ .literal = .{ .start = 6, .end = 7 } };

    const obj = try a.create(Value.Obj);
    obj.* = .{ .data = .{ .string = .{ .bytes = bytes, .segments = segments } } };
    const v: Value = .{ .obj = obj };

    const restored = try roundTrip(v, a);
    try testing.expectEqualSlices(u8, bytes, restored.obj.data.string.bytes);
    try testing.expectEqual(@as(usize, 3), restored.obj.data.string.segments.len);
    try testing.expectEqual(Segment{ .literal = .{ .start = 0, .end = 3 } }, restored.obj.data.string.segments[0]);
    try testing.expectEqual(Segment{ .interp = 0 }, restored.obj.data.string.segments[1]);
    try testing.expectEqual(Segment{ .literal = .{ .start = 6, .end = 7 } }, restored.obj.data.string.segments[2]);
}

// ---------------------------------------------------------------------------
// Bytecode round-trip — moved from compiler.test.zig
// ---------------------------------------------------------------------------

test "Compile Serialization" {
    const input =
        \\ var str = "string value"
        \\ const num = 25
        \\ fn fun |x| {
        \\     return x * 2
        \\ }
        \\ var list = List{
        \\    "one", // comment on one
        \\    "two"
        \\ }
        \\ const set = Set{1, 2, 3.3}
        \\ const map = Map{1:2.2, 3: 4.4}
    ;

    var mod = try Module.initEmpty(allocator, std.testing.io);
    defer mod.deinit();
    var bytecode = try compileSource(input, mod);
    defer bytecode.free(allocator);

    // this doesn't need to be a file, but it's nice to sometimes not delete it and inspect it
    const test_io = std.testing.io;
    const cwd = std.Io.Dir.cwd();
    defer cwd.deleteFile(test_io, "tmp.topi.byte") catch {};
    {
        const file = try cwd.createFile(test_io, "tmp.topi.byte", .{});
        defer file.close(test_io);
        var buf: [1024]u8 = undefined;
        var file_writer = file.writer(test_io, &buf);
        _ = try bytecode.serialize(allocator, &file_writer.interface);
    }
    const file = try cwd.openFile(test_io, "tmp.topi.byte", .{});
    defer file.close(test_io);
    var buf: [1024]u8 = undefined;
    var file_reader = file.reader(test_io, &buf);
    const reader = &file_reader.interface;
    var deserialized = try Bytecode.deserialize(allocator, reader);
    defer deserialized.free(allocator);
    try testing.expectEqualSlices(u8, bytecode.instructions, deserialized.instructions);
    for (bytecode.constants, 0..) |constant, i| {
        try testing.expect(constant.eql(deserialized.constants[i]));
    }
}

test "Compile Serialization: class with collection fields" {
    const input =
        \\ class Thing {
        \\     tags = Set{},
        \\     items = List{"a", "b"},
        \\     data = Map{"key": 42}
        \\ }
        \\ var t = new Thing{}
    ;

    var mod = try Module.initEmpty(allocator, std.testing.io);
    defer mod.deinit();
    var bytecode = try compileSource(input, mod);
    defer bytecode.free(allocator);

    const test_io2 = std.testing.io;
    const cwd2 = std.Io.Dir.cwd();
    defer cwd2.deleteFile(test_io2, "tmp_class_coll.topi.byte") catch {};
    {
        const file = try cwd2.createFile(test_io2, "tmp_class_coll.topi.byte", .{});
        defer file.close(test_io2);
        var buf: [1024]u8 = undefined;
        var file_writer = file.writer(test_io2, &buf);
        _ = try bytecode.serialize(allocator, &file_writer.interface);
    }
    const file = try cwd2.openFile(test_io2, "tmp_class_coll.topi.byte", .{});
    defer file.close(test_io2);
    var buf: [1024]u8 = undefined;
    var file_reader = file.reader(test_io2, &buf);
    const reader = &file_reader.interface;
    var deserialized = try Bytecode.deserialize(allocator, reader);
    defer deserialized.free(allocator);
    try testing.expectEqualSlices(u8, bytecode.instructions, deserialized.instructions);
    for (bytecode.constants, 0..) |constant, i| {
        try testing.expect(constant.eql(deserialized.constants[i]));
    }
}

// ---------------------------------------------------------------------------
// State (globals JSON) round-trip — moved from vm.test.zig
// ---------------------------------------------------------------------------

test "Runtime Save and Load State" {
    const test_case =
        \\ var value = 0
        \\ value += 1
        \\ var list = List{}
        \\ var outer = List{list}
        \\ var last = list
        \\ var str = "value"
        \\ list.add(value)
        \\ list[0] = "changed"
        \\ class Test {
        \\    field1 = "one",
        \\    field2 = 2,
        \\ }
        \\ var test = new Test{}
        \\ fn func || return "func"
        \\ enum Enum {
        \\    One,
        \\    Two
        \\ }
        \\ var enumValue = Enum.One
        \\ var set = Set{"set_value"}
    ;
    const alloc = testing.allocator;

    var mod = try Module.initEmpty(allocator, std.testing.io);
    defer mod.deinit();
    var vm = try initTestVm(test_case, mod, false);
    defer vm.deinit();
    defer (@as(*TestRunner, @fieldParentPtr("runner", vm.runner))).deinit();
    defer vm.bytecode.free(alloc);
    try vm.interpret();

    // Serialize initial state after interpret
    var data1: std.Io.Writer.Allocating = .init(alloc);
    defer data1.deinit();
    try State.serialize(&vm, &data1.writer);

    const second_case =
        \\ var value = 10
        \\ value += 5
        \\ var outer = List{}
        \\ var test = "t"
        \\ enum Enum {
        \\    One,
        \\    Two
        \\ }
        \\ var set = Set{"will be overwritten"}
    ;

    var mod2 = try Module.initEmpty(allocator, std.testing.io);
    defer mod2.deinit();
    var vm2 = try initTestVm(second_case, mod2, false);
    defer vm2.deinit();
    defer (@as(*TestRunner, @fieldParentPtr("runner", vm2.runner))).deinit();
    defer vm2.bytecode.free(testing.allocator);
    // Write data from initial state into new vm
    var data_reader = std.Io.Reader.fixed(data1.written());
    try State.deserialize(&vm2, &data_reader);
    try testing.expectEqual(vm2.globals[0].number, 1);
    try testing.expectEqualSlices(u8, vm2.globals[1].obj.data.list.items[0].obj.data.list.items[0].asString() orelse unreachable, "changed");
    try testing.expectEqual(vm2.globals[2].obj.data.instance.fields[1].number, 2);

    // run new vm
    try vm2.interpret();
    try testing.expectEqual(vm2.globals[0].number, 6);

    // serialize new vm state
    var data2 = std.Io.Writer.Allocating.init(alloc);
    defer data2.deinit();
    const size = try State.calculateSize(&vm2);
    try State.serialize(&vm2, &data2.writer);
    try testing.expectEqual(465, size);
}

test "State Visit Counter Round-Trip" {
    const test_case =
        \\ === START @A1B2C3D4-E5F6G7H8 {
        \\    :speaker: "Hello" @I9J0K1L2-M3N4O5P6
        \\ }
    ;
    const alloc = testing.allocator;

    var mod = try Module.initEmpty(allocator, std.testing.io);
    defer mod.deinit();
    var vm = try initTestVm(test_case, mod, false);
    defer vm.deinit();
    defer (@as(*TestRunner, @fieldParentPtr("runner", vm.runner))).deinit();
    defer vm.bytecode.free(alloc);
    try vm.interpret();

    // Find the START visit counter and verify it was incremented
    var start_index: ?usize = null;
    for (vm.bytecode.global_symbols) |sym| {
        if (std.mem.eql(u8, sym.name, "START")) {
            start_index = sym.index;
            break;
        }
    }
    try testing.expect(start_index != null);
    try testing.expectEqual(@as(u32, 1), vm.globals[start_index.?].visit);

    // Serialize
    var data: std.Io.Writer.Allocating = .init(alloc);
    defer data.deinit();
    try State.serialize(&vm, &data.writer);

    // Deserialize into new VM from same script
    var mod2 = try Module.initEmpty(allocator, std.testing.io);
    defer mod2.deinit();
    var vm2 = try initTestVm(test_case, mod2, false);
    defer vm2.deinit();
    defer (@as(*TestRunner, @fieldParentPtr("runner", vm2.runner))).deinit();
    defer vm2.bytecode.free(alloc);

    var reader = std.Io.Reader.fixed(data.written());
    try State.deserialize(&vm2, &reader);

    // Visit counter should be preserved
    var start_index2: ?usize = null;
    for (vm2.bytecode.global_symbols) |sym| {
        if (std.mem.eql(u8, sym.name, "START")) {
            start_index2 = sym.index;
            break;
        }
    }
    try testing.expect(start_index2 != null);
    try testing.expectEqual(@as(u32, 1), vm2.globals[start_index2.?].visit);

    // Run again — visit counter should increment to 2
    try vm2.interpret();
    try testing.expectEqual(@as(u32, 2), vm2.globals[start_index2.?].visit);
}

test "State Object Sharing Preservation" {
    const test_case =
        \\ var list = List{1, 2, 3}
        \\ var alias = list
    ;
    const alloc = testing.allocator;

    var mod = try Module.initEmpty(allocator, std.testing.io);
    defer mod.deinit();
    var vm = try initTestVm(test_case, mod, false);
    defer vm.deinit();
    defer (@as(*TestRunner, @fieldParentPtr("runner", vm.runner))).deinit();
    defer vm.bytecode.free(alloc);
    try vm.interpret();

    // Both globals should reference the same object
    try testing.expectEqual(vm.globals[0].obj.id, vm.globals[1].obj.id);

    // Serialize
    var data: std.Io.Writer.Allocating = .init(alloc);
    defer data.deinit();
    try State.serialize(&vm, &data.writer);

    // Deserialize into new VM
    var mod2 = try Module.initEmpty(allocator, std.testing.io);
    defer mod2.deinit();
    var vm2 = try initTestVm(test_case, mod2, false);
    defer vm2.deinit();
    defer (@as(*TestRunner, @fieldParentPtr("runner", vm2.runner))).deinit();
    defer vm2.bytecode.free(alloc);

    var reader = std.Io.Reader.fixed(data.written());
    try State.deserialize(&vm2, &reader);

    // After deserialization, both globals should still share the same object
    try testing.expectEqual(vm2.globals[0].obj.id, vm2.globals[1].obj.id);
    // Verify the list contents are correct
    try testing.expectEqual(@as(usize, 3), vm2.globals[0].obj.data.list.items.len);
}

test "State Same-Script Round-Trip" {
    const test_case =
        \\ var count = 0
        \\ count += 10
        \\ var name = "hello"
    ;
    const alloc = testing.allocator;

    var mod = try Module.initEmpty(allocator, std.testing.io);
    defer mod.deinit();
    var vm = try initTestVm(test_case, mod, false);
    defer vm.deinit();
    defer (@as(*TestRunner, @fieldParentPtr("runner", vm.runner))).deinit();
    defer vm.bytecode.free(alloc);
    try vm.interpret();

    try testing.expectEqual(@as(f32, 10), vm.globals[0].number);

    // Serialize
    var data: std.Io.Writer.Allocating = .init(alloc);
    defer data.deinit();
    try State.serialize(&vm, &data.writer);

    // Load into new VM compiled from the SAME script
    var mod2 = try Module.initEmpty(allocator, std.testing.io);
    defer mod2.deinit();
    var vm2 = try initTestVm(test_case, mod2, false);
    defer vm2.deinit();
    defer (@as(*TestRunner, @fieldParentPtr("runner", vm2.runner))).deinit();
    defer vm2.bytecode.free(alloc);

    var reader = std.Io.Reader.fixed(data.written());
    try State.deserialize(&vm2, &reader);

    // Loaded state should be present before execution
    try testing.expectEqual(@as(f32, 10), vm2.globals[0].number);

    // Run — decl_global should skip (count is already 10), then count += 10 gives 20
    try vm2.interpret();
    try testing.expectEqual(@as(f32, 20), vm2.globals[0].number);
}

test "State Cross-Module Class Resolution" {
    const script1 =
        \\ class Foo {
        \\     x = 1,
        \\     y = "hello",
        \\ }
        \\ var obj = new Foo{}
    ;
    const script2 =
        \\ class Foo {
        \\     x = 1,
        \\     y = "hello",
        \\ }
        \\ var obj = new Foo{}
    ;
    const alloc = testing.allocator;

    // Run script 1
    var mod1 = try Module.initEmpty(allocator, std.testing.io);
    defer mod1.deinit();
    var vm1 = try initTestVm(script1, mod1, false);
    defer vm1.deinit();
    defer (@as(*TestRunner, @fieldParentPtr("runner", vm1.runner))).deinit();
    defer vm1.bytecode.free(alloc);
    try vm1.interpret();

    // Serialize
    var data: std.Io.Writer.Allocating = .init(alloc);
    defer data.deinit();
    try State.serialize(&vm1, &data.writer);

    // Load into script 2 (which defines the same class)
    var mod2 = try Module.initEmpty(allocator, std.testing.io);
    defer mod2.deinit();
    var vm2 = try initTestVm(script2, mod2, false);
    defer vm2.deinit();
    defer (@as(*TestRunner, @fieldParentPtr("runner", vm2.runner))).deinit();
    defer vm2.bytecode.free(alloc);

    var reader = std.Io.Reader.fixed(data.written());
    try State.deserialize(&vm2, &reader);

    // Instance should be resolved with the module's class (not a stub)
    const instance = vm2.globals[0].obj.data.instance;
    try testing.expectEqual(@as(usize, 2), instance.fields.len);
    try testing.expectEqual(@as(f32, 1), instance.fields[0].number);

    // The base class should be from the module's constants (not a deserialized stub)
    var found_module_class = false;
    for (vm2.bytecode.constants) |c| {
        if (c == .obj and c.obj.data == .class) {
            if (std.mem.eql(u8, c.obj.data.class.name, "Foo")) {
                try testing.expectEqual(c.obj, instance.base);
                found_module_class = true;
                break;
            }
        }
    }
    try testing.expect(found_module_class);
}

test "State Variation Round-Trip: cycle and sequence" {
    // Advance cycle to index 2 and sequence to index 2, save, load into
    // a fresh VM, and verify the next calls resume from the saved position.
    // Use separate lists for each function since they share a content-hash key.
    const script =
        \\ var lc = List{10, 20, 30}
        \\ var r1 = cycle(lc)
        \\ var r2 = cycle(lc)
        \\ var ls = List{100, 200, 300}
        \\ var s1 = sequence(ls)
        \\ var s2 = sequence(ls)
    ;
    const alloc = testing.allocator;

    var mod = try Module.initEmpty(allocator, std.testing.io);
    defer mod.deinit();
    var vm = try initTestVm(script, mod, false);
    defer vm.deinit();
    defer (@as(*TestRunner, @fieldParentPtr("runner", vm.runner))).deinit();
    defer vm.bytecode.free(alloc);
    try vm.interpret();

    // After interpret: cycle at index 2, sequence at index 2
    // r1=10 (cycle 0→1), r2=20 (cycle 1→2)
    // s1=100 (seq 0→1), s2=200 (seq 1→2)
    try testing.expectEqual(@as(f32, 10), vm.globals[1].number);
    try testing.expectEqual(@as(f32, 20), vm.globals[2].number);
    try testing.expectEqual(@as(f32, 100), vm.globals[4].number);
    try testing.expectEqual(@as(f32, 200), vm.globals[5].number);

    // Serialize
    var data: std.Io.Writer.Allocating = .init(alloc);
    defer data.deinit();
    try State.serialize(&vm, &data.writer);

    // Load into fresh VM that calls cycle/sequence once more.
    // Use NEW variable names (next_c, next_s) so decl_global doesn't skip
    // the initialization — the deserialized state has r1/r2/s1/s2 but not
    // next_c/next_s, so their globals start as void and the call executes.
    const script2 =
        \\ var lc = List{10, 20, 30}
        \\ var next_c = cycle(lc)
        \\ var ls = List{100, 200, 300}
        \\ var next_s = sequence(ls)
    ;
    var mod2 = try Module.initEmpty(allocator, std.testing.io);
    defer mod2.deinit();
    var vm2 = try initTestVm(script2, mod2, false);
    defer vm2.deinit();
    defer (@as(*TestRunner, @fieldParentPtr("runner", vm2.runner))).deinit();
    defer vm2.bytecode.free(alloc);

    var reader = std.Io.Reader.fixed(data.written());
    try State.deserialize(&vm2, &reader);
    try vm2.interpret();

    // cycle should resume at index 2 → return 30, advance to 0
    const nc_idx = try vm2.getGlobalsIndex("next_c");
    try testing.expectEqual(@as(f32, 30), vm2.globals[nc_idx].number);
    // sequence should resume at index 2 → return 300, stay at 2
    const ns_idx = try vm2.getGlobalsIndex("next_s");
    try testing.expectEqual(@as(f32, 300), vm2.globals[ns_idx].number);
}

test "State Variation Round-Trip: shuffle preserves order" {
    // Run shuffle once (creates a permutation, advances index to 1),
    // save, load, and verify the next shuffle returns the second element
    // of the same permutation (not a fresh reshuffle).
    const script =
        \\ var l = List{10, 20, 30}
        \\ var r1 = shuffle(l)
    ;
    const alloc = testing.allocator;

    var mod = try Module.initEmpty(allocator, std.testing.io);
    defer mod.deinit();
    var vm = try initTestVm(script, mod, false);
    defer vm.deinit();
    defer (@as(*TestRunner, @fieldParentPtr("runner", vm.runner))).deinit();
    defer vm.bytecode.free(alloc);
    try vm.interpret();

    const first_result = vm.globals[1].number;
    try testing.expect(first_result == 10 or first_result == 20 or first_result == 30);

    var data: std.Io.Writer.Allocating = .init(alloc);
    defer data.deinit();
    try State.serialize(&vm, &data.writer);

    const script2 =
        \\ var l = List{10, 20, 30}
        \\ var next = shuffle(l)
    ;
    var mod2 = try Module.initEmpty(allocator, std.testing.io);
    defer mod2.deinit();
    var vm2 = try initTestVm(script2, mod2, false);
    defer vm2.deinit();
    defer (@as(*TestRunner, @fieldParentPtr("runner", vm2.runner))).deinit();
    defer vm2.bytecode.free(alloc);

    var reader = std.Io.Reader.fixed(data.written());
    try State.deserialize(&vm2, &reader);
    try vm2.interpret();

    const next_idx = try vm2.getGlobalsIndex("next");
    const second_result = vm2.globals[next_idx].number;
    try testing.expect(second_result == 10 or second_result == 20 or second_result == 30);
    try testing.expect(second_result != first_result);
}

test "State Schema Evolution: removed global" {
    // Save state with globals a, b, c. Load into script with only a and c.
    // b should be silently ignored.
    const script1 =
        \\ var a = 1
        \\ var b = 2
        \\ var c = 3
    ;
    const script2 =
        \\ var a = 0
        \\ var c = 0
    ;
    const alloc = testing.allocator;

    var mod = try Module.initEmpty(allocator, std.testing.io);
    defer mod.deinit();
    var vm = try initTestVm(script1, mod, false);
    defer vm.deinit();
    defer (@as(*TestRunner, @fieldParentPtr("runner", vm.runner))).deinit();
    defer vm.bytecode.free(alloc);
    try vm.interpret();

    var data: std.Io.Writer.Allocating = .init(alloc);
    defer data.deinit();
    try State.serialize(&vm, &data.writer);

    var mod2 = try Module.initEmpty(allocator, std.testing.io);
    defer mod2.deinit();
    var vm2 = try initTestVm(script2, mod2, false);
    defer vm2.deinit();
    defer (@as(*TestRunner, @fieldParentPtr("runner", vm2.runner))).deinit();
    defer vm2.bytecode.free(alloc);

    var reader = std.Io.Reader.fixed(data.written());
    try State.deserialize(&vm2, &reader);

    // a should be restored, c should be restored, b is gone
    const a_idx = try vm2.getGlobalsIndex("a");
    const c_idx = try vm2.getGlobalsIndex("c");
    try testing.expectEqual(@as(f32, 1), vm2.globals[a_idx].number);
    try testing.expectEqual(@as(f32, 3), vm2.globals[c_idx].number);
}

test "State Schema Evolution: added global" {
    // Save state with global a. Load into script with a and b (new).
    // a should be restored, b should keep its declaration default.
    const script1 =
        \\ var a = 42
    ;
    const script2 =
        \\ var a = 0
        \\ var b = 99
    ;
    const alloc = testing.allocator;

    var mod = try Module.initEmpty(allocator, std.testing.io);
    defer mod.deinit();
    var vm = try initTestVm(script1, mod, false);
    defer vm.deinit();
    defer (@as(*TestRunner, @fieldParentPtr("runner", vm.runner))).deinit();
    defer vm.bytecode.free(alloc);
    try vm.interpret();

    var data: std.Io.Writer.Allocating = .init(alloc);
    defer data.deinit();
    try State.serialize(&vm, &data.writer);

    var mod2 = try Module.initEmpty(allocator, std.testing.io);
    defer mod2.deinit();
    var vm2 = try initTestVm(script2, mod2, false);
    defer vm2.deinit();
    defer (@as(*TestRunner, @fieldParentPtr("runner", vm2.runner))).deinit();
    defer vm2.bytecode.free(alloc);

    // Load state (only has a), then interpret (sets b=99)
    var reader = std.Io.Reader.fixed(data.written());
    try State.deserialize(&vm2, &reader);

    const a_idx = try vm2.getGlobalsIndex("a");
    try testing.expectEqual(@as(f32, 42), vm2.globals[a_idx].number);

    // Run the script — b gets its default value
    try vm2.interpret();
    const b_idx = try vm2.getGlobalsIndex("b");
    try testing.expectEqual(@as(f32, 99), vm2.globals[b_idx].number);
}

test "State Malformed JSON" {
    // Verify deserialize fails cleanly with invalid JSON input.
    const script =
        \\ var x = 1
    ;
    const alloc = testing.allocator;

    var mod = try Module.initEmpty(allocator, std.testing.io);
    defer mod.deinit();
    var vm = try initTestVm(script, mod, false);
    defer vm.deinit();
    defer (@as(*TestRunner, @fieldParentPtr("runner", vm.runner))).deinit();
    defer vm.bytecode.free(alloc);

    // Empty input
    {
        var reader = std.Io.Reader.fixed("");
        try testing.expectError(error.UnexpectedEndOfInput, State.deserialize(&vm, &reader));
    }

    // Truncated JSON
    {
        var reader = std.Io.Reader.fixed("{\"x\":{\"num");
        try testing.expectError(error.UnexpectedEndOfInput, State.deserialize(&vm, &reader));
    }

    // Not JSON at all
    {
        var reader = std.Io.Reader.fixed("not json");
        const result = State.deserialize(&vm, &reader);
        try testing.expectError(error.SyntaxError, result);
    }
}

test "State version marker round-trip" {
    const script =
        \\ var x = 42
    ;
    const alloc = testing.allocator;

    var mod = try Module.initEmpty(allocator, std.testing.io);
    defer mod.deinit();
    var vm = try initTestVm(script, mod, false);
    defer vm.deinit();
    defer (@as(*TestRunner, @fieldParentPtr("runner", vm.runner))).deinit();
    defer vm.bytecode.free(alloc);
    try vm.interpret();

    // Serialize and verify __version is present
    var data: std.Io.Writer.Allocating = .init(alloc);
    defer data.deinit();
    try State.serialize(&vm, &data.writer);
    const json = data.written();
    try testing.expect(std.mem.indexOf(u8, json, "\"__version\":2") != null);

    // Deserialize works fine with version marker
    var mod2 = try Module.initEmpty(allocator, std.testing.io);
    defer mod2.deinit();
    var vm2 = try initTestVm(script, mod2, false);
    defer vm2.deinit();
    defer (@as(*TestRunner, @fieldParentPtr("runner", vm2.runner))).deinit();
    defer vm2.bytecode.free(alloc);

    var reader = std.Io.Reader.fixed(json);
    try State.deserialize(&vm2, &reader);
    try testing.expectEqual(@as(f32, 42), vm2.globals[0].number);
}

test "State version marker: future version rejected" {
    const script =
        \\ var x = 1
    ;
    const alloc = testing.allocator;

    var mod = try Module.initEmpty(allocator, std.testing.io);
    defer mod.deinit();
    var vm = try initTestVm(script, mod, false);
    defer vm.deinit();
    defer (@as(*TestRunner, @fieldParentPtr("runner", vm.runner))).deinit();
    defer vm.bytecode.free(alloc);

    var reader = std.Io.Reader.fixed("{\"__version\":999,\"x\":{\"number\":1}}");
    try testing.expectError(error.UnsupportedStateVersion, State.deserialize(&vm, &reader));
}

test "State version marker: legacy save without version loads fine" {
    const script =
        \\ var x = 1
    ;
    const alloc = testing.allocator;

    var mod = try Module.initEmpty(allocator, std.testing.io);
    defer mod.deinit();
    var vm = try initTestVm(script, mod, false);
    defer vm.deinit();
    defer (@as(*TestRunner, @fieldParentPtr("runner", vm.runner))).deinit();
    defer vm.bytecode.free(alloc);

    // JSON without __version (simulating old save format)
    var reader = std.Io.Reader.fixed("{\"x\":{\"number\":42.00000}}");
    try State.deserialize(&vm, &reader);
    try testing.expectEqual(@as(f32, 42), vm.globals[0].number);
}

test "State dangling ref returns Void" {
    const script =
        \\ var x = List{1, 2, 3}
    ;
    const alloc = testing.allocator;

    var mod = try Module.initEmpty(allocator, std.testing.io);
    defer mod.deinit();
    var vm = try initTestVm(script, mod, false);
    defer vm.deinit();
    defer (@as(*TestRunner, @fieldParentPtr("runner", vm.runner))).deinit();
    defer vm.bytecode.free(alloc);

    // JSON where x references a UUID that doesn't exist in the root
    var reader = std.Io.Reader.fixed("{\"__version\":1,\"x\":{\"ref\":\"AAAAAAAAAAAAAAAAA\"}}");
    try State.deserialize(&vm, &reader);
    // Dangling ref should resolve to void
    try testing.expectEqual(vm.globals[0], .void);
}
