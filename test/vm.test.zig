const std = @import("std");
const testing = std.testing;


const topi = @import("topi");
const Vm = topi.runtime.Vm;
const Runner = topi.runtime.Runner;
const State = topi.runtime.State;
const fmt = topi.utils.fmt;

const Bytecode = topi.backend.Bytecode;

const Module = topi.module.Module;

const RuntimeErr = topi.runtime.RuntimeErr;

const compileSource = @import("compiler.test.zig").compileSource;
const TestRunner = @import("runner.zig").TestRunner;
const allocator = std.testing.allocator;

fn printBytecode(bytecode: *Bytecode) !void {
    var buffer: [128]u8 = undefined;
    var writer = std.fs.File.stderr().writer(&buffer);
    const stderr = &writer.interface;
    try bytecode.print(stderr);
}

pub fn initTestVm(source: []const u8, mod: *Module, debug: bool) !Vm {
    var bytecode = try compileSource(source, mod);
    errdefer bytecode.free(allocator);
    if (debug) try printBytecode(&bytecode);
    const test_runner = try TestRunner.init(allocator);
    return Vm.init(allocator, bytecode, &test_runner.runner);
}

fn printErr(vm: *Vm) void {
    var buffer: [128]u8 = undefined;
    var writer = std.fs.File.stderr().writer(&buffer);
    const stderr = &writer.interface;
    vm.err.print(stderr);
}

test "Runtime Basics" {
    const test_cases = .{
        .{ .input = "1", .value = 1.0, .type = f32 },
        .{ .input = "2", .value = 2.0, .type = f32 },
        .{ .input = "1 + 2", .value = 3.0, .type = f32 },
        .{ .input = "-12", .value = -12.0, .type = f32 },
        .{ .input = "111 + 222", .value = 333.0, .type = f32 },
        .{ .input = "5 - 2", .value = 3.0, .type = f32 },
        .{ .input = "5 * 2", .value = 10.0, .type = f32 },
        .{ .input = "6 / 2", .value = 3.0, .type = f32 },
        .{ .input = "6 % 5", .value = 1.0, .type = f32 },
        .{ .input = "1 == 1", .value = true, .type = bool },
        .{ .input = "1 != 1", .value = false, .type = bool },
        .{ .input = "1 > 5", .value = false, .type = bool },
        .{ .input = "1 < 5", .value = true, .type = bool },
        .{ .input = "!true", .value = false, .type = bool },
        .{ .input = "!false", .value = true, .type = bool },
        .{ .input = "!!true == true", .value = true, .type = bool },
        .{ .input = "!(1 == 1) != (5 > 10)", .value = false, .type = bool },
    };

    inline for (test_cases) |case| {
        var mod = try Module.initEmpty(allocator);
        defer mod.deinit();
        var vm = try initTestVm(case.input, mod, false);
        defer vm.deinit();
        defer (@as(*TestRunner, @fieldParentPtr("runner", vm.runner))).deinit();
        defer vm.bytecode.free(testing.allocator);
        try vm.interpret();
        switch (case.type) {
            f32 => try testing.expect(case.value == vm.stack.previous().number),
            bool => try testing.expect(case.value == vm.stack.previous().bool),
            else => continue,
        }
    }
}

test "Runtime Conditionals" {
    const test_cases = .{
        .{ .input = "if true { 10 }", .value = 10.0, .type = .number },
        .{ .input = "if true { 10 } else { 20 }", .value = 10.0, .type = .number },
        .{ .input = "if false { 10 } else { 20 }", .value = 20.0, .type = .number },
        .{ .input = "if 1 == 1 { 10 }", .value = 10.0, .type = .number },
        .{ .input = "if false { 10 }", .value = void, .type = .nil },
        .{ .input = "if 1 > 0 or 2 < 1 { 10 }", .value = 10.0, .type = .number },
        .{ .input = "if 1 > 0 and 2 < 1 { 10 }", .value = void, .type = .nil },
        .{ .input = "const ten = if true 10 else 0 ten", .value = 10, .type = .number },
        .{ .input = "const zero = if false 5 else 0 zero", .value = 0.0, .type = .number },
    };

    inline for (test_cases) |case| {
        var mod = try Module.initEmpty(allocator);
        defer mod.deinit();
        var vm = try initTestVm(case.input, mod, false);
        defer vm.deinit();
        defer (@as(*TestRunner, @fieldParentPtr("runner", vm.runner))).deinit();
        defer vm.bytecode.free(testing.allocator);
        try vm.interpret();
        switch (case.type) {
            .number => {
                errdefer std.debug.print("{any} == {any}", .{ case.value, vm.stack.previous().number });
                try testing.expect(case.value == vm.stack.previous().number);
            },
            .nil => try testing.expect(vm.stack.previous().is(.nil)),
            else => continue,
        }
    }
}
test "Runtime Variables" {
    const test_cases = .{
        .{ .input = "var one = 1 one", .value = 1.0 },
        .{ .input = "var one = 1 var two = 2 one + two", .value = 3.0 },
        .{ .input = "var one = 1 var two = one + one one + two", .value = 3.0 },
        .{ .input =
        \\ var five = 1
        \\ five = 5
        , .value = 5.0 },
    };

    inline for (test_cases) |case| {
        var mod = try Module.initEmpty(allocator);
        defer mod.deinit();
        var vm = try initTestVm(case.input, mod, false);
        defer vm.deinit();
        defer (@as(*TestRunner, @fieldParentPtr("runner", vm.runner))).deinit();
        defer vm.bytecode.free(testing.allocator);
        try vm.interpret();
        const value = vm.stack.previous().number;
        try testing.expect(case.value == value);
    }
}

test "Runtime Constant Variables" {
    const test_cases = &[_][]const u8{
        "const one = 1 one = 2",
        "const one = 1 one += 3",
    };

    inline for (test_cases) |case| {
        var mod = try Module.initEmpty(allocator);
        defer mod.deinit();
        const err = initTestVm(case, mod, false);
        try testing.expectError(Vm.Error.CompilerError, err);
    }
}

test "Runtime Strings" {
    const test_cases = .{
        .{ .input = "\"testing\"", .value = "testing" },
        .{ .input = "\"test\" + \"ing\"", .value = "testing" },
        .{ .input = "\"test\" + \"ing\" + \"testing\"", .value = "testingtesting" },
        .{ .input = "\"{123}test\"", .value = "123test" },
        .{ .input = "\"test{123}\"", .value = "test123" },
        .{ .input = "\"{123}te{4 * 5}st{6 + 7}\"", .value = "123te20st13" },
        .{ .input = "\"test{\"test\"}ing", .value = "testtesting" },
        .{ .input = "\"test{\"\\\"test\\\"\"}ing", .value = "test\"test\"ing" },
        .{ .input = "\"test\\\"test\\\"ing", .value = "test\"test\"ing" },
        .{ .input = "\"test\\\ting", .value = "test\ting" },
        .{ .input = "\"test\\\ring", .value = "test\ring" },
        .{ .input = "\"test\\{test\\}ing", .value = "test{test}ing" },
        .{ .input = "\"test{\"quote\\\"test\\\"quote\"}ing", .value = "testquote\"test\"quoteing" },
        .{ .input = "var t = \"test\" t += \"ing\"", .value = "testing" },
    };

    inline for (test_cases) |case| {
        var mod = try Module.initEmpty(allocator);
        errdefer std.log.err("Error on: {s}", .{case.input});
        defer mod.deinit();
        var vm = try initTestVm(case.input, mod, false);
        defer vm.deinit();
        defer (@as(*TestRunner, @fieldParentPtr("runner", vm.runner))).deinit();
        defer vm.bytecode.free(testing.allocator);
        try vm.interpret();
        const str = vm.stack.previous().asString() orelse unreachable;
        try testing.expectEqualStrings(case.value, str);
    }
}

test "Runtime String Methods" {
    // upper
    {
        var mod = try Module.initEmpty(allocator);
        defer mod.deinit();
        var vm = try initTestVm("\"hello\".upper()", mod, false);
        defer vm.deinit();
        defer (@as(*TestRunner, @fieldParentPtr("runner", vm.runner))).deinit();
        defer vm.bytecode.free(testing.allocator);
        vm.interpret() catch |err| { printErr(&vm); return err; };
        try testing.expectEqualStrings("HELLO", vm.stack.previous().asString().?);
    }
    {
        var mod = try Module.initEmpty(allocator);
        defer mod.deinit();
        var vm = try initTestVm("\"Hello World\".upper()", mod, false);
        defer vm.deinit();
        defer (@as(*TestRunner, @fieldParentPtr("runner", vm.runner))).deinit();
        defer vm.bytecode.free(testing.allocator);
        vm.interpret() catch |err| { printErr(&vm); return err; };
        try testing.expectEqualStrings("HELLO WORLD", vm.stack.previous().asString().?);
    }

    // lower
    {
        var mod = try Module.initEmpty(allocator);
        defer mod.deinit();
        var vm = try initTestVm("\"HELLO\".lower()", mod, false);
        defer vm.deinit();
        defer (@as(*TestRunner, @fieldParentPtr("runner", vm.runner))).deinit();
        defer vm.bytecode.free(testing.allocator);
        vm.interpret() catch |err| { printErr(&vm); return err; };
        try testing.expectEqualStrings("hello", vm.stack.previous().asString().?);
    }

    // replace
    {
        var mod = try Module.initEmpty(allocator);
        defer mod.deinit();
        var vm = try initTestVm("\"hello\".replace(\"l\", \"r\")", mod, false);
        defer vm.deinit();
        defer (@as(*TestRunner, @fieldParentPtr("runner", vm.runner))).deinit();
        defer vm.bytecode.free(testing.allocator);
        vm.interpret() catch |err| { printErr(&vm); return err; };
        try testing.expectEqualStrings("herro", vm.stack.previous().asString().?);
    }
    {
        var mod = try Module.initEmpty(allocator);
        defer mod.deinit();
        var vm = try initTestVm("\"hello\".replace(\"x\", \"y\")", mod, false);
        defer vm.deinit();
        defer (@as(*TestRunner, @fieldParentPtr("runner", vm.runner))).deinit();
        defer vm.bytecode.free(testing.allocator);
        vm.interpret() catch |err| { printErr(&vm); return err; };
        try testing.expectEqualStrings("hello", vm.stack.previous().asString().?);
    }
    {
        var mod = try Module.initEmpty(allocator);
        defer mod.deinit();
        var vm = try initTestVm("\"aaa\".replace(\"a\", \"bb\")", mod, false);
        defer vm.deinit();
        defer (@as(*TestRunner, @fieldParentPtr("runner", vm.runner))).deinit();
        defer vm.bytecode.free(testing.allocator);
        vm.interpret() catch |err| { printErr(&vm); return err; };
        try testing.expectEqualStrings("bbbbbb", vm.stack.previous().asString().?);
    }

    // substr (inclusive end)
    {
        var mod = try Module.initEmpty(allocator);
        defer mod.deinit();
        var vm = try initTestVm("\"hello\".substr(1, 3)", mod, false);
        defer vm.deinit();
        defer (@as(*TestRunner, @fieldParentPtr("runner", vm.runner))).deinit();
        defer vm.bytecode.free(testing.allocator);
        vm.interpret() catch |err| { printErr(&vm); return err; };
        try testing.expectEqualStrings("ell", vm.stack.previous().asString().?);
    }
    {
        var mod = try Module.initEmpty(allocator);
        defer mod.deinit();
        var vm = try initTestVm("\"hello\".substr(0, 4)", mod, false);
        defer vm.deinit();
        defer (@as(*TestRunner, @fieldParentPtr("runner", vm.runner))).deinit();
        defer vm.bytecode.free(testing.allocator);
        vm.interpret() catch |err| { printErr(&vm); return err; };
        try testing.expectEqualStrings("hello", vm.stack.previous().asString().?);
    }
    {
        var mod = try Module.initEmpty(allocator);
        defer mod.deinit();
        var vm = try initTestVm("\"hello\".substr(0, 0)", mod, false);
        defer vm.deinit();
        defer (@as(*TestRunner, @fieldParentPtr("runner", vm.runner))).deinit();
        defer vm.bytecode.free(testing.allocator);
        vm.interpret() catch |err| { printErr(&vm); return err; };
        try testing.expectEqualStrings("h", vm.stack.previous().asString().?);
    }

    // trim
    {
        var mod = try Module.initEmpty(allocator);
        defer mod.deinit();
        var vm = try initTestVm("\" hello \".trim()", mod, false);
        defer vm.deinit();
        defer (@as(*TestRunner, @fieldParentPtr("runner", vm.runner))).deinit();
        defer vm.bytecode.free(testing.allocator);
        vm.interpret() catch |err| { printErr(&vm); return err; };
        try testing.expectEqualStrings("hello", vm.stack.previous().asString().?);
    }

    // has on string literal
    {
        var mod = try Module.initEmpty(allocator);
        defer mod.deinit();
        var vm = try initTestVm("\"test\".has(\"tes\")", mod, false);
        defer vm.deinit();
        defer (@as(*TestRunner, @fieldParentPtr("runner", vm.runner))).deinit();
        defer vm.bytecode.free(testing.allocator);
        vm.interpret() catch |err| { printErr(&vm); return err; };
        try testing.expect(vm.stack.previous().bool == true);
    }
    {
        var mod = try Module.initEmpty(allocator);
        defer mod.deinit();
        var vm = try initTestVm("\"test\".has(\"foo\")", mod, false);
        defer vm.deinit();
        defer (@as(*TestRunner, @fieldParentPtr("runner", vm.runner))).deinit();
        defer vm.bytecode.free(testing.allocator);
        vm.interpret() catch |err| { printErr(&vm); return err; };
        try testing.expect(vm.stack.previous().bool == false);
    }

    // split
    {
        var mod = try Module.initEmpty(allocator);
        defer mod.deinit();
        var vm = try initTestVm("\"a,b,c\".split(\",\")", mod, false);
        defer vm.deinit();
        defer (@as(*TestRunner, @fieldParentPtr("runner", vm.runner))).deinit();
        defer vm.bytecode.free(testing.allocator);
        vm.interpret() catch |err| { printErr(&vm); return err; };
        const list = vm.stack.previous().obj.data.list;
        try testing.expectEqual(@as(usize, 3), list.items.len);
        try testing.expectEqualStrings("a", list.items[0].asString().?);
        try testing.expectEqualStrings("b", list.items[1].asString().?);
        try testing.expectEqualStrings("c", list.items[2].asString().?);
    }

    // length on string
    {
        var mod = try Module.initEmpty(allocator);
        defer mod.deinit();
        var vm = try initTestVm("\"hello\".length()", mod, false);
        defer vm.deinit();
        defer (@as(*TestRunner, @fieldParentPtr("runner", vm.runner))).deinit();
        defer vm.bytecode.free(testing.allocator);
        vm.interpret() catch |err| { printErr(&vm); return err; };
        try testing.expect(vm.stack.previous().number == 5.0);
    }

    // var-based string
    {
        var mod = try Module.initEmpty(allocator);
        defer mod.deinit();
        var vm = try initTestVm(
            \\var s = "Hello World"
            \\s.lower()
        , mod, false);
        defer vm.deinit();
        defer (@as(*TestRunner, @fieldParentPtr("runner", vm.runner))).deinit();
        defer vm.bytecode.free(testing.allocator);
        vm.interpret() catch |err| { printErr(&vm); return err; };
        try testing.expectEqualStrings("hello world", vm.stack.previous().asString().?);
    }
}

test "Runtime Weighted" {
    // weighted returns one of the items - just verify it doesn't crash and returns a valid item
    var mod = try Module.initEmpty(allocator);
    defer mod.deinit();
    var vm = try initTestVm("weighted(List{1, 2, 3}, List{1, 1, 1})", mod, false);
    defer vm.deinit();
    defer (@as(*TestRunner, @fieldParentPtr("runner", vm.runner))).deinit();
    defer vm.bytecode.free(testing.allocator);
    vm.interpret() catch |err| {
        printErr(&vm);
        return err;
    };
    const result = vm.stack.previous().number;
    try testing.expect(result == 1.0 or result == 2.0 or result == 3.0);
}

test "Runtime Lists" {
    const test_cases = .{
        .{ .input = "List{}", .value = [_]f32{} },
        .{ .input = "List{1,2,3}", .value = [_]f32{ 1, 2, 3 } },
        .{ .input = "List{1 + 2, 3 * 4, 5 + 6}", .value = [_]f32{ 3, 12, 11 } },
        .{ .input = "var l = List{} l.add(1) l", .value = [_]f32{1} },
        .{ .input = "var l = List{} l.add(1) l.add(2) l", .value = [_]f32{ 1, 2 } },
        .{ .input = "var l = List{} l.add(1) l.add(2) l.remove(1) l", .value = [_]f32{2} },
        .{ .input = "var l = List{1,2,3,4,5} l.remove(3) l", .value = [_]f32{ 1, 2, 4, 5 } },
    };

    inline for (test_cases) |case| {
        var mod = try Module.initEmpty(allocator);
        defer mod.deinit();
        var vm = try initTestVm(case.input, mod, false);
        defer vm.deinit();
        defer (@as(*TestRunner, @fieldParentPtr("runner", vm.runner))).deinit();
        defer vm.bytecode.free(testing.allocator);
        try vm.interpret();
        const prev = vm.stack.previous();
        for (case.value, 0..) |v, i| {
            try testing.expect(v == prev.obj.data.list.items[i].number);
        }
    }
}

test "Runtime Maps" {
    const test_cases = .{
        .{ .input = "Map{}", .keys = [_]f32{}, .values = [_]f32{} },
        .{ .input = "Map{1:2, 3: 4}", .keys = [_]f32{ 1, 3 }, .values = [_]f32{ 2, 4 } },
        .{ .input = "Map{1 + 1: 2 * 2, 3 + 3: 4 * 4}", .keys = [_]f32{ 2, 6 }, .values = [_]f32{ 4, 16 } },
        .{ .input = "var m = Map{1:2} m.add(3, 4) m", .keys = [_]f32{ 1, 3 }, .values = [_]f32{ 2, 4 } },
        .{ .input = "var m = Map{1:2} m.add(3, 4) m.remove(1) m", .keys = [_]f32{3}, .values = [_]f32{4} },
    };

    inline for (test_cases) |case| {
        var mod = try Module.initEmpty(allocator);
        defer mod.deinit();
        var vm = try initTestVm(case.input, mod, false);
        defer vm.deinit();
        defer (@as(*TestRunner, @fieldParentPtr("runner", vm.runner))).deinit();
        defer vm.bytecode.free(testing.allocator);
        try vm.interpret();
        const map = vm.stack.previous().obj.data.map;
        try testing.expect(map.keys().len == case.keys.len);
        if (case.keys.len > 0) {
            for (map.keys(), 0..) |k, i| {
                errdefer std.debug.print("{}:{}", .{ k.number, map.get(k).?.number });
                try testing.expect(case.keys[i] == k.number);
                try testing.expect(case.values[i] == map.get(k).?.number);
            }
        }
    }
}

test "Runtime Sets" {
    const test_cases = .{
        .{ .input = "Set{}", .values = [_]f32{} },
        .{ .input = "Set{1, 2}", .values = [_]f32{ 1, 2 } },
        .{ .input = "Set{1 + 1, 3 + 3}", .values = [_]f32{ 2, 6 } },
        .{ .input = "var s = Set{1} s.add(2) s.add(1) s", .values = [_]f32{ 1, 2 } },
        .{ .input = "var s = Set{1} s.add(2) s.remove(1) s", .values = [_]f32{2} },
    };

    inline for (test_cases) |case| {
        var mod = try Module.initEmpty(allocator);
        defer mod.deinit();
        var vm = try initTestVm(case.input, mod, false);
        defer vm.deinit();
        defer (@as(*TestRunner, @fieldParentPtr("runner", vm.runner))).deinit();
        defer vm.bytecode.free(testing.allocator);
        try vm.interpret();
        const set = vm.stack.previous().obj.data.set;
        try testing.expect(set.keys().len == case.values.len);
        if (case.values.len > 0) {
            for (set.keys(), 0..) |k, i| {
                errdefer std.debug.print("{}", .{k.number});
                try testing.expect(case.values[i] == k.number);
            }
        }
    }
}

test "Runtime Index" {
    const test_cases = .{
        .{ .input = "List{1,2,3}[1]", .value = 2.0 },
        .{ .input = "List{1,2,3}[0 + 2]", .value = 3.0 },
        .{ .input = "List{List{1,2,3}}[0][0]", .value = 1.0 },
        .{ .input = "List{}[0]", .value = null },
        .{ .input = "List{1,2,3}[99]", .value = null },
        .{ .input = "Map{1: 1, 2: 2}[1]", .value = 1.0 },
        .{ .input = "Map{1: 1, 2: 2}[2]", .value = 2.0 },
        .{ .input = "Map{1: 1}[2]", .value = null },
        .{ .input = "Map{}[0]", .value = null },
        .{ .input = "List{1,1,1}.count()", .value = 3.0 },
        .{ .input = "Set{\"one\"}.count()", .value = 1.0 },
        .{ .input = "Map{\"one\": 1 }[\"one\"]", .value = 1.0 },
        .{
            .input =
            \\ const list = List{1,2,3}
            \\ list[0] = 4
            \\ list[0]
            ,
            .value = 4.0,
        },
        .{
            .input =
            \\ const inner = List{1,2,3,4,5}
            \\ const outer = List{inner,6,7,8,9}
            \\ var ref = outer[0]
            \\ ref[0] = 10
            \\ inner[0]
            ,
            .value = 10.0,
        },
        .{
            .input =
            \\ const inner = List{1,2,3,4,5}
            \\ const mid = List{inner, 6,7,8}
            \\ const outer = List{mid,9}
            \\ outer[0][0][4] = 99
            \\ outer[0][0][4]
            ,
            .value = 99.0,
        },
    };

    inline for (test_cases) |case| {
        var mod = try Module.initEmpty(allocator);
        defer mod.deinit();
        var vm = try initTestVm(case.input, mod, false);
        defer vm.deinit();
        defer (@as(*TestRunner, @fieldParentPtr("runner", vm.runner))).deinit();
        defer vm.bytecode.free(testing.allocator);
        try vm.interpret();
        const value = vm.stack.previous();
        switch (@TypeOf(case.value)) {
            comptime_float => try testing.expect(case.value == value.number),
            else => try testing.expect(value == .nil),
        }
    }
}

test "Runtime Functions" {
    const test_cases = .{
        .{ .input =
        \\ fn fifteen || return 5 + 10
        \\ fifteen()
        , .value = 15.0 },
        .{ .input =
        \\ fn one || return 1
        \\ fn two || return 2
        \\ one() + two()
        , .value = 3.0 },
        .{ .input =
        \\ fn a || return 1
        \\ fn b || return a() + 1
        \\ fn c || return b() + 1
        \\ c()
        , .value = 3.0 },
        .{ .input =
        \\ fn exit || { return 99 return 100 }
        \\ exit()
        , .value = 99.0 },
        .{ .input =
        \\ fn cond || { if true return 1 return 0 }
        \\ cond()
        , .value = 1.0 },
        .{ .input =
        \\ fn cond || { if false return 1 return 0 }
        \\ cond()
        , .value = 0.0 },
        .{ .input =
        \\ fn noop || {}
        \\ noop()
        , .value = null },
        .{ .input =
        \\ fn noop || {}
        \\ fn noopop || noop()
        \\ noop()
        \\ noopop()
        , .value = null },
        .{ .input =
        \\ fn one || return 1
        \\ fn curry || return one
        \\ curry()()
        , .value = 1.0 },
        .{ .input =
        \\ fn fib |n| {
        \\   if n < 2 return n
        \\   return fib(n - 1) + fib(n - 2)
        \\ }
        \\ const s = mstime()
        \\ const v = fib(25)
        \\ const e = mstime()
        \\ const duration = e - s
        \\ // print("Start: {s}, End: {e}, Value: {v}, Duration: {duration}ms")
        \\ v
        , .value = 75025.0 },
    };

    inline for (test_cases) |case| {
        var mod = try Module.initEmpty(allocator);
        defer mod.deinit();
        var vm = try initTestVm(case.input, mod, false);
        defer vm.deinit();
        defer (@as(*TestRunner, @fieldParentPtr("runner", vm.runner))).deinit();
        defer vm.bytecode.free(testing.allocator);
        try vm.interpret();
        const value = vm.stack.previous();
        errdefer std.log.err("Err case: {s}\n{} != {}\n", .{ case.input, if (value == .nil) 0 else value.number, case.value });
        switch (@TypeOf(case.value)) {
            comptime_float => try testing.expect(case.value == value.number),
            else => try testing.expect(value == .void),
        }
    }
}

test "Runtime Locals" {
    const test_cases = .{
        .{
            .input =
            \\ fn oneFn || {
            \\     var one = 1
            \\     return one
            \\ }
            \\ oneFn()
            ,
            .value = 1.0,
        },
        .{
            .input =
            \\ fn threeFn || {
            \\     var one = 1
            \\     var two = 2
            \\     return one + two
            \\ }
            \\ threeFn()
            ,
            .value = 3.0,
        },
        .{
            .input =
            \\ fn threeFn || {
            \\     var one = 1
            \\     var two = 2
            \\     return one + two
            \\ }
            \\ fn sevenFn || {
            \\     var three = 3
            \\     var four = 4
            \\     return three + four
            \\ }
            \\ threeFn() + sevenFn()
            ,
            .value = 10.0,
        },
        .{
            .input =
            \\ const five = 5
            \\ fn minusOne || {
            \\     const one = 1
            \\     return five - one
            \\ }
            \\ fn minusTwo || {
            \\     const two = 2
            \\     return five - two
            \\ }
            \\ minusOne() + minusTwo()
            ,
            .value = 7.0,
        },
    };

    inline for (test_cases) |case| {
        var mod = try Module.initEmpty(allocator);
        defer mod.deinit();
        var vm = try initTestVm(case.input, mod, false);
        defer vm.deinit();
        defer (@as(*TestRunner, @fieldParentPtr("runner", vm.runner))).deinit();
        defer vm.bytecode.free(testing.allocator);
        try vm.interpret();
        const value = vm.stack.previous();
        errdefer std.debug.print("{s}:: {any} == {any}", .{ case.input, case.value, value });
        switch (@TypeOf(case.value)) {
            comptime_float => try testing.expect(case.value == value.number),
            else => try testing.expect(value == .nil),
        }
    }
}

test "Runtime Function Arguments" {
    const test_cases = .{
        .{ .input =
        \\ fn ident |a| return a
        \\ ident(4)
        , .value = 4.0 },
        .{ .input =
        \\ fn sum |a, b| return a + b
        \\ sum(1, 2)
        , .value = 3.0 },
        .{ .input =
        \\ fn sum |a, b| {
        \\    const c = a + b
        \\    return c
        \\ }
        \\ sum(1, 2)
        , .value = 3.0 },
        .{ .input =
        \\ fn sum |a, b| {
        \\    const c = a + b
        \\    return c
        \\ }
        \\ sum(1, 2) + sum(3, 4)
        , .value = 10.0 },
        .{ .input =
        \\ const globalNum = 10
        \\ fn sum |a, b| {
        \\     const c = a + b
        \\     return c + globalNum
        \\ }
        \\ sum(5, 5) + globalNum
        , .value = 30.0 },
        .{ .input =
        \\ const globalNum = 10
        \\ fn sum |a, b| {
        \\     const c = a + b
        \\     return c + globalNum
        \\ }
        \\ fn outer || {
        \\    return
        \\    sum(1, 2) +
        \\    sum(3, 4) +
        \\    globalNum
        \\ }
        \\ outer() + globalNum
        , .value = 50.0 },
    };
    inline for (test_cases) |case| {
        var mod = try Module.initEmpty(allocator);
        defer mod.deinit();
        var vm = try initTestVm(case.input, mod, false);
        defer vm.deinit();
        defer (@as(*TestRunner, @fieldParentPtr("runner", vm.runner))).deinit();
        defer vm.bytecode.free(testing.allocator);
        try vm.interpret();
        const value = vm.stack.previous();
        switch (@TypeOf(case.value)) {
            comptime_float => try testing.expect(case.value == value.number),
            else => try testing.expect(value == .nil),
        }
    }
}

test "Runtime Builtin Functions" {
    const test_cases = .{
        .{
            .input = "rnd(1, 10)",
            .type = f32,
        },
        .{
            .input = "rnd01()",
            .type = f32,
        },
    };
    inline for (test_cases) |case| {
        var mod = try Module.initEmpty(allocator);
        defer mod.deinit();
        var vm = try initTestVm(case.input, mod, false);
        defer vm.deinit();
        defer (@as(*TestRunner, @fieldParentPtr("runner", vm.runner))).deinit();
        defer vm.bytecode.free(testing.allocator);
        vm.interpret() catch |err| {
            printErr(&vm);
            return err;
        };
        var buffer: [128]u8 = undefined;
        var writer = std.fs.File.stderr().writer(&buffer);
        const stderr = &writer.interface;
        const value = vm.stack.previous();
        try value.print(stderr, null);
        try stderr.writeAll("\n");
        try testing.expect(value == .number);
    }
}

test "Runtime While and For Loops" {
    const test_cases = .{
        .{ .input =
        \\ var x = 0
        \\ while x < 10 {
        \\     x = x + 1
        \\ }
        \\ x
        , .value = 10 },
        .{ .input =
        \\ var x = 0
        \\ while true {
        \\    if x > 9 break
        \\    x = x + 1
        \\ }
        \\ x
        , .value = 10 },
        .{ .input =
        \\ var x = 0
        \\ while true {
        \\    x = x + 1
        \\    if x < 10 continue
        \\    break
        \\ }
        \\ x
        , .value = 10 },
        .{ .input =
        \\ var x = 0
        \\ while true {
        \\    var y = 1
        \\    x = x + y
        \\    if x < 10 continue
        \\    break
        \\ }
        \\ x
        , .value = 10 },
        .{ .input =
        \\ var x = 0
        \\ while true {
        \\    var y = 1
        \\    while true {
        \\       y += x + 1
        \\       if y < 10 continue
        \\       break;
        \\    }
        \\    x = x + y
        \\    if x < 100 continue
        \\    break
        \\ }
        \\ x
        , .value = 190 },
        .{ .input =
        \\ const list = List{1,2,3,4,5}
        \\ var sum = 0
        \\ for list |item| {
        \\     sum += item
        \\ }
        \\ sum
        , .value = 15 },
        .{ .input =
        \\ const set = Set{1,2,3,3,3}
        \\ var sum = 0
        \\ for set |item| {
        \\     sum += item
        \\ }
        \\ sum
        , .value = 6 },
        .{ .input =
        \\ const map = Map{1:2,3:4,5:6}
        \\ var sum = 0
        \\ for map |kvp| {
        \\     sum += kvp.key + kvp.value
        \\ }
        \\ sum
        , .value = 21 },
        .{ .input =
        \\ var sum = 0
        \\ for 0..10 |i| {
        \\     sum += i
        \\ }
        \\ sum
        , .value = 55 },
        .{ .input =
        \\ const list = List{1,2,3,4,5}
        \\ var sum = 0
        \\ for 0..(list.count() - 1) |i| {
        \\     sum += list[i]
        \\ }
        \\ sum
        , .value = 15 },
    };
    inline for (test_cases) |case| {
        var mod = try Module.initEmpty(allocator);
        defer mod.deinit();
        var vm = try initTestVm(case.input, mod, false);
        defer vm.deinit();
        defer (@as(*TestRunner, @fieldParentPtr("runner", vm.runner))).deinit();
        defer vm.bytecode.free(testing.allocator);
        errdefer std.debug.print("Error Case: {s}", .{case.input});
        vm.interpret() catch |err| {
            printErr(&vm);
            return err;
        };
        const value = vm.stack.previous();
        try testing.expectEqual(value.number, case.value);
    }
}

test "Runtime Class Definition" {
    const input =
        \\ class Test {
        \\    value = 0,
        \\    fn one || return 1
        \\ }
        \\ assert(Test.value == 0, "Test.value == 0")
        \\ assert(Test.one() == 1, "Test.one == 1")
    ;
    var mod = try Module.initEmpty(allocator);
    defer mod.deinit();
    var vm = try initTestVm(input, mod, false);
    defer vm.deinit();
    defer (@as(*TestRunner, @fieldParentPtr("runner", vm.runner))).deinit();
    defer vm.bytecode.free(testing.allocator);
    vm.interpret() catch |err| {
        printErr(&vm);
        return err;
    };
}

test "Runtime Class Error" {
    const tests = .{
        \\ class Test {
        \\    value = 0, // zero
        \\    fn one || return self.value,
        \\ }
        \\ Test.one() != Test.value
    };
    inline for (tests) |input| {
        var mod = try Module.initEmpty(allocator);
        defer mod.deinit();
        var vm = try initTestVm(input, mod, false);
        defer vm.deinit();
        defer (@as(*TestRunner, @fieldParentPtr("runner", vm.runner))).deinit();
        defer vm.bytecode.free(testing.allocator);
        const err = vm.interpret() catch |err| err;
        try std.testing.expectError(error.RuntimeError, err);
    }
}

test "Runtime Class Compile Error" {
    const tests = .{
        .{ .input =
        \\ class Test {
        \\    value = 0
        \\ }
        \\ var test = new Test{
        \\    val = 2
        \\ }
        , .err = Vm.Error.CompilerError },
        .{ .input =
        \\ class Test {
        \\    value = 0
        \\ }
        \\ Test.value = 55
        , .err = Vm.Error.CompilerError },
        .{ .input =
        \\ class Test {
        \\    value = 0
        \\ }
        \\ Test = 55
        , .err = Vm.Error.CompilerError },
        .{ .input =
        \\ var test = new Test{}
        , .err = Vm.Error.CompilerError },
    };
    inline for (tests) |input| {
        var mod = try Module.initEmpty(allocator);
        defer mod.deinit();
        const err = initTestVm(input.input, mod, false);
        try testing.expectError(input.err, err);
    }
}

test "Runtime Instance" {
    const input =
        \\ class Test {
        \\    value = 0,
        \\    list = List{},
        \\    nested = List{}
        \\    fn func || return "func"
        \\    fn incr |i| self.value += i
        \\ }
        \\ assert(Test.value == 0, "Test.value == 0")
        \\ const test = new Test{}
        \\ test.value = 5
        \\ assert(test.value == 5, "test.value == 5")
        \\ test.value += 1
        \\ assert(test.value == 6, "test.value == 6")
        \\ assert(test.func() == "func", "test.func() == \"func\"")
        \\ test.incr(1)
        \\ assert(test.value == 7, "test.value == 7")
        \\ test.list.add(1)
        \\ assert(test.list.count() == 1, "test.list.count() == 1")
        \\ assert(test.list[0] == 1, "test.list[0] == 1")
        \\ test.list[0] = 99
        \\ assert(test.list[0] == 99, "test.list[0] == 99")
        \\ test.nested.add(2)
        \\ assert(test.nested[0] == 2, "test.nested[2] == 2")
        \\ test.list.add(test.nested)
        \\ assert(test.list[1][0] == 2, "test.list[1][0] == 2")
        \\ test.value = Test.value
        \\ assert(test.value == 0, "test.value == 0")
    ;
    var mod = try Module.initEmpty(allocator);
    defer mod.deinit();
    var vm = try initTestVm(input, mod, false);
    defer vm.deinit();
    defer (@as(*TestRunner, @fieldParentPtr("runner", vm.runner))).deinit();
    defer vm.bytecode.free(testing.allocator);
    vm.interpret() catch |err| {
        printErr(&vm);
        return err;
    };
}

test "Runtime Enums" {
    const input =
        \\ enum TimeOfDay {
        \\  Morning,
        \\  Afternoon,
        \\  Evening,
        \\  Night
        \\ }
        \\
        \\ fn timeOfDay |hour| {
        \\  switch hour {
        \\      0..4: return TimeOfDay.Night,
        \\      5..11: return TimeOfDay.Morning,
        \\      12..16: return TimeOfDay.Afternoon,
        \\      16..21: return TimeOfDay.Evening,
        \\      22..24: return TimeOfDay.Night,
        \\  }
        \\ }
        \\
        \\ assert(timeOfDay(5) == TimeOfDay.Morning, "timeOfDay(5) == TimeOfDay.Morning")
        \\ var tod = TimeOfDay.Morning
        \\ assert(tod < TimeOfDay.Evening, "tod < TimeOfDay.Evening");
        \\ enumseq Quest {
        \\    None,
        \\    KnowsOf,
        \\    Started,
        \\    Complete
        \\ }
        \\ var quest = Quest.None
        \\ quest = Quest.Started
        \\ quest = Quest.KnowsOf
        \\ quest = Quest.Complete
        \\ quest = Quest.None
        \\ assert(quest == Quest.Complete, "Quest is not Complete")
    ;

    var mod = try Module.initEmpty(allocator);
    defer mod.deinit();
    var vm = try initTestVm(input, mod, false);
    defer vm.deinit();
    defer (@as(*TestRunner, @fieldParentPtr("runner", vm.runner))).deinit();
    defer vm.bytecode.free(testing.allocator);
    vm.interpret() catch |err| {
        printErr(&vm);
        return err;
    };
}

test "Runtime Boughs" {
    const test_cases = .{
        .{
            .input =
            \\ === START {
            \\    :speaker: "Text goes here" #tag1 #tag2
            \\    :speaker: "More text here"
            \\ }
            ,
            .expected = &[_][]const u8{ "Text goes here", "More text here" },
        },
        .{ .input =
        \\ === START {
        \\    const before = "This is added before"
        \\    const after = "and this is added afterwards"
        \\    :speaker_one: "{before} and then more text here"
        \\    :speaker_two: "Text goes here {after}"
        \\ }
        , .expected = &[_][]const u8{ "This is added before and then more text here", "Text goes here and this is added afterwards" } },
        .{ .input =
        \\ fn repeat |str, count| {
        \\     var result = ""
        \\     while count > 0 {
        \\          result = result + str
        \\          count -= 1
        \\     }
        \\     return result
        \\ }
        \\ === START {
        \\    :speaker_one: "Hello, {repeat("Yo ", 5)}!"
        \\    :speaker_two: "Uh.. hello?"
        \\ }
        , .expected = &[_][]const u8{ "Hello, Yo Yo Yo Yo Yo !", "Uh.. hello?" } },
        .{
            .input =
            \\ === START {
            \\     if true { :speaker: "This is true" }
            \\     else { :speaker: "This is false" }
            \\ }
            ,
            .expected = &[_][]const u8{"This is true"},
        },
        .{
            .input =
            \\ === START {
            \\    if true :speaker: "True text goes here"
            \\    :speaker: "More text here"
            \\    if false {
            \\        :speaker: "False text doesn't appear"
            \\        assert(false, "should not be here")
            \\    }
            \\    :speaker: "Final text here"
            \\ }
            ,
            .expected = &[_][]const u8{ "True text goes here", "More text here", "Final text here" },
        },
        .{
            .input =
            \\ === START {
            \\    :speaker: "Text goes here"
            \\    === INNER {
            \\        :speaker: "Inner text here"
            \\    }
            \\    :speaker: "More goes here"
            \\    => INNER
            \\    assert(false, "should not be here")
            \\ }
            ,
            .expected = &[_][]const u8{ "Text goes here", "More goes here", "Inner text here" },
        },
        .{
            .input =
            \\ === START {
            \\    var str = "string"
            \\    :speaker: "Text goes here"
            \\    === OUTER {
            \\        var num = 50
            \\        :speaker: "Outer text here doesn't happen"
            \\        === INNER {
            \\            num += 5
            \\            str += " gnirts"
            \\            :speaker: "Inner and final text here {str} {num}"
            \\        }
            \\    }
            \\    :speaker: "More goes here"
            \\    => OUTER.INNER
            \\    assert(false, "should not be here")
            \\ }
            ,
            .expected = &[_][]const u8{ "Text goes here", "More goes here", "Inner and final text here string gnirts 55 " },
        },
        .{
            .input =
            \\ var count = 0
            \\ === START {
            \\    :speaker: "Text goes here"
            \\    === QS {
            \\        :speaker: "Start fork"
            \\        fork QUESTION {
            \\            ~ "One" #test => ANSWERS.ONE
            \\            ~ "Two" => ANSWERS.TWO
            \\        }
            \\    }
            \\    :speaker: "More goes here"
            \\    => QS
            \\    === ANSWERS {
            \\        === ONE {
            \\            :speaker: "One"
            \\            count += 1
            \\            if count < 3 => QS.QUESTION
            \\        }
            \\        === TWO {
            \\            :speaker: "Two"
            \\            count += 1
            \\            if count < 3 => QS.QUESTION
            \\        }
            \\    }
            \\ }
            ,
            .choices = &[_]usize{ 0, 0, 1 },
            .expected = &[_][]const u8{ "Text goes here", "More goes here", "Start fork", "One", "One", "Two" },
        },
    };

    inline for (test_cases) |case| {
        var mod = try Module.initEmpty(allocator);
        defer mod.deinit();
        var vm = try initTestVm(case.input, mod, false);
        defer vm.deinit();
        const test_runner: *TestRunner = @fieldParentPtr("runner", vm.runner);
        defer test_runner.deinit();
        defer vm.bytecode.free(testing.allocator);

        if (@hasField(@TypeOf(case), "choices")) {
            try test_runner.choices_to_make.appendSlice(allocator, case.choices);
        }

        vm.interpret() catch |err| {
            std.debug.print("{s}\n", .{case.input});

            try printBytecode(&vm.bytecode);
            printErr(&vm);
            return err;
        };

        try test_runner.expectOutput(case.expected);
    }
}

test "Runtime Bough Nested Starts with Backups" {
    const input =
        \\ === START {
        \\    =>^ OUTER
        \\    var val = "value"
        \\    === OUTER {
        \\        fork^ {
        \\           ~ "One" { "One" }
        \\           ~ "Two" { "Two" }
        \\        }
        \\        :Speaker: "Outer"
        \\        === INNER {
        \\            :Speaker: "Inner"
        \\        }
        \\        :Speaker: "After Inner"
        \\    }
        \\    :Speaker: "After Outer"
        \\ }
        \\ :Speaker: "After Start"
    ;
    const expected = &[_][]const u8{"Inner"};

    var mod = try Module.initEmpty(allocator);
    defer mod.deinit();
    var vm = try initTestVm(input, mod, false);
    defer vm.deinit();
    const test_runner: *TestRunner = @fieldParentPtr("runner", vm.runner);
    defer test_runner.deinit();
    defer vm.bytecode.free(testing.allocator);
    try vm.start("START.OUTER.INNER");
    while (vm.can_continue) {
        vm.run() catch |err| {
            printErr(&vm);
            return err;
        };
    }
    try test_runner.expectOutput(expected);
}

test "Runtime Bough Loops" {
    const test_cases = .{
        .{
            .input =
            \\ var i = 0
            \\ var str = ""
            \\ === START {
            \\     while i < 5 {
            \\         i += 1
            \\         str = "{i}"
            \\         :Speaker: "Testing {str}"
            \\     }
            \\ i
            \\ }
            ,
            .value = 5,
        },
    };
    inline for (test_cases) |case| {
        var mod = try Module.initEmpty(allocator);
        defer mod.deinit();
        var vm = try initTestVm(case.input, mod, false);
        defer vm.deinit();
        defer (@as(*TestRunner, @fieldParentPtr("runner", vm.runner))).deinit();
        defer vm.bytecode.free(testing.allocator);
        vm.interpret() catch |err| {
            printErr(&vm);
            return err;
        };
        const value = vm.stack.previous();
        try testing.expectEqual(value.number, case.value);
    }
}

test "Runtime Bough Functions" {
    const test_cases = .{
        .{
            .input =
            \\ === START {
            \\   fn t || return "test"
            \\   :Speaker: "This is a {t()}"
            \\ }
            ,
            .expected = &[_][]const u8{"This is a test"},
        },
        .{
            .input =
            \\ === START {
            \\   fn sum |x, y| return x + y
            \\   :Speaker: "1 + 7 equals {sum(1, 7)}"
            \\ }
            ,
            .expected = &[_][]const u8{"1 + 7 equals 8"},
        },
        .{
            .input =
            \\ === START {
            \\   fn sum |x, y| return x + y
            \\   => INNER
            \\   === INNER {
            \\     :Speaker: "2 + 7 equals {sum(2, 7)}"
            \\   }
            \\ }
            ,
            .expected = &[_][]const u8{"2 + 7 equals 9"},
        },
        .{
            .input =
            \\ const t = "test"
            \\ === START {
            \\   const value = 10
            \\   fn sum |x, y| return x + y + value
            \\   => INNER
            \\   === INNER {
            \\     :Speaker: "3 + 7 + {value} equals {sum(3, 7)}"
            \\     :Speaker: "Testing 123 {t} 123"
            \\   }
            \\ }
            ,
            .expected = &[_][]const u8{ "3 + 7 + 10 equals 20", "Testing 123 test 123" },
        },
        .{
            .input =
            \\ === START {
            \\   fn greet |time| {
            \\     switch time {
            \\       5..11: return "Morning",
            \\       12..16: return "Afternoon",
            \\       17..21: return "Evening",
            \\       else: return "Hello"
            \\     }
            \\   }
            \\   => GREETING
            \\   === GREETING {
            \\     :Speaker: "{greet(1)}"
            \\     :Speaker: "{greet(5)}"
            \\     :Speaker: "{greet(10)}"
            \\     :Speaker: "{greet(13)}"
            \\     :Speaker: "{greet(21)}"
            \\     :Speaker: "{greet(24)}"
            \\   }
            \\ }
            ,
            .expected = &[_][]const u8{ "Hello", "Morning", "Morning", "Afternoon", "Evening", "Hello" },
        },
    };

    inline for (test_cases) |case| {
        var mod = try Module.initEmpty(allocator);
        defer mod.deinit();
        var vm = try initTestVm(case.input, mod, false);
        defer vm.deinit();
        const test_runner: *TestRunner = @fieldParentPtr("runner", vm.runner);
        defer test_runner.deinit();
        defer vm.bytecode.free(testing.allocator);
        vm.interpret() catch |err| {
            printErr(&vm);
            return err;
        };
        try test_runner.expectOutput(case.expected);
    }
}

test "Runtime Forks" {
    const test_cases = .{
        .{
            .input =
            \\ === START {
            \\    :speaker: "Question"
            \\    fork {
            \\        ~ "Answer one" #one #of #many #tags #here {
            \\            :speaker: "You chose one"
            \\        }
            \\        ~ "Answer two" #two {
            \\            :speaker: "You chose two"
            \\        }
            \\    }
            \\ }
            ,
            .expected = &[_][]const u8{"Question","You chose one"},
        },
        .{
            .input =
            \\ === START {
            \\    :speaker: "Question"
            \\    var count = 0
            \\    fork NAMED {
            \\        ~ "Answer one" {
            \\            :speaker: "You chose one"
            \\            if count < 2 {
            \\                count += 1
            \\                => NAMED
            \\            }
            \\            else => DONE
            \\        }
            \\    }
            \\ }
            \\ === DONE {
            \\    :speaker: "Done"
            \\ }
            ,
            .expected = &[_][]const u8{"Question","You chose one", "You chose one", "You chose one", "Done"},
        },
        .{
            .input =
            \\ === START {
            \\    fork^ {
            \\        ~ "_0" {
            \\            fork^ {
            \\              ~ "_0_0" {
            \\                 :speaker: "_0_0"
            \\              }
            \\              ~ "_0_1" {
            \\                 :speaker: "_0_1"
            \\              }
            \\            }
            \\        }
            \\        ~ "_1" {
            \\            fork^ {
            \\              ~ "_1_0" {
            \\                 :speaker: "_1_0"
            \\              }
            \\              ~ "_1_1" {
            \\                 :speaker: "_1_1"
            \\              }
            \\            }
            \\        }
            \\    }
            \\    :: "Done"
            \\ }
            ,
            .expected = &[_][]const u8{"_0_0","Done"},
        }
    };

    inline for (test_cases) |case| {
        var mod = try Module.initEmpty(allocator);
        defer mod.deinit();
        var vm = try initTestVm(case.input, mod, false);
        defer vm.deinit();
        const test_runner: *TestRunner = @fieldParentPtr("runner", vm.runner);
        defer test_runner.deinit();
        defer vm.bytecode.free(testing.allocator);
        vm.interpret() catch |err| {
            printErr(&vm);
            return err;
        };
        test_runner.expectOutput(case.expected) catch |err| {
            std.debug.print("Output: {f}", .{fmt.array("{s}", test_runner.output.items)});
            return err;
        };
    }
}

test "Runtime Visits" {
    const test_cases = .{
        .{
            .input =
            \\ === START {
            \\     :speaker: "Question"
            \\    fork^ NAMED {
            \\        ~* ONE "Answer one" {
            \\            :speaker: "You chose one"
            \\            => NAMED
            \\        }
            \\        ~ TWO "Answer two" {
            \\            :speaker: "You chose two"
            \\        }
            \\    }
            \\ assert(START == 1, "START == 1")
            \\ assert(START.NAMED == 2, "START.NAMED == 2")
            \\ assert(START.NAMED.ONE == 1, "START.NAMED.ONE == 1")
            \\ assert(START.NAMED.TWO == 1, "START.NAMED.TWO == 1")
            \\ // binary operations
            \\ assert(START - 1 == 0, "START - 1 == 0")
            \\ assert(START * 2 == 2, "START * 2 == 2")
            \\ assert(START / 4 == 0.25, "START / 4 == 0.25")
            \\ assert(START.NAMED % 2 == 0, "START.NAMED % 2 == 0")
            \\ }
            ,
            .expected = &[_][]const u8{"Question", "You chose one", "You chose two" }
        },
        .{
            .input =
            \\ === START {
            \\     :speaker: "Question"
            \\    fork^ ANON {
            \\        ~* ONE "Answer one" {
            \\            :speaker: "You chose one"
            \\        }
            \\        ~ TWO "Answer two" {
            \\            :speaker: "You chose two"
            \\        }
            \\    }
            \\ assert(START == 1, "START == 1")
            \\ assert(START.ANON.ONE == 1, "START.ANON.ONE == 1")
            \\ assert(START.ANON.TWO == 0, "START.ANON.TWO == 0")
            \\ }
            ,
            .expected = &[_][]const u8{"Question", "You chose one" }
        },
        .{
            .input =
            \\ === START {
            \\     :speaker: "Starting"
            \\    === INNER {
            \\        :speaker: "Inside question"
            \\        fork^ ANON {
            \\            ~* ONE "Answer one" {
            \\                :speaker: "You chose one"
            \\            }
            \\            ~ TWO "Answer two" {
            \\                :speaker: "You chose two"
            \\            }
            \\        }
            \\    }
            \\ =>^ INNER
            \\ assert(START == 1, "START == 1")
            \\ assert(START.INNER == 1, "START.INNER == 1")
            \\ assert(START.INNER.ANON.ONE == 1, "START.INNER.ANON.ONE == 1")
            \\ assert(START.INNER.ANON.TWO == 0, "START.INNER.ANON.TWO == 0")
            \\ }
            ,
            .expected = &[_][]const u8{"Starting", "Inside question", "You chose one" }
        },
        .{
            .input =
            \\ === START {
            \\     :speaker: "Starting"
            \\     fork NAMED {
            \\         ~ ONE "Answer one" {
            \\             :speaker: "You chose one"
            \\             assert(ONE == 1, "ONE == 1")
            \\             assert(NAMED.TWO == 0, "NAMED.TWO == 0")
            \\             => TWO
            \\         }
            \\         ~ TWO "Answer two" {
            \\             :speaker: "You chose two"
            \\             assert(NAMED.ONE == 1, "NAMED.ONE == 1")
            \\             assert(TWO == 1, "TWO == 1")
            \\         }
            \\     }
            \\ }
            ,
            .expected = &[_][]const u8{"Starting", "You chose one", "You chose two" }
        },
    };

    inline for (test_cases) |case| {
        var mod = try Module.initEmpty(allocator);
        defer mod.deinit();
        var vm = try initTestVm(case.input, mod, false);
        defer vm.deinit();
        const test_runner: *TestRunner = @fieldParentPtr("runner", vm.runner);
        defer test_runner.deinit();
        defer vm.bytecode.free(testing.allocator);
        vm.interpret() catch |err| {
            std.debug.print("{s}", .{ case.input });
            try printBytecode(&vm.bytecode);
            printErr(&vm);
            return err;
        };
        test_runner.expectOutput(case.expected) catch |err| {
            std.debug.print("Output: {f}", .{fmt.array("{s}", test_runner.output.items)});
            return err;
        };
    }
}

test "Runtime Jump Backups" {
    const test_cases = .{
        .{
            .input =
            \\ === START {
            \\     :speaker: "Question"
            \\     =>^ MIDDLE
            \\     :speaker: "Continue here after divert"
            \\ }
            \\ === MIDDLE {
            \\     :speaker: "Done"
            \\ }
            ,
            .expected = &[_][]const u8{"Question", "Done", "Continue here after divert"}
        },
        .{
            .input =
            \\ === START {
            \\     :speaker: "Question"
            \\    fork^ {
            \\        ~ "Answer one" {
            \\            :speaker: "You chose one"
            \\        }
            \\        ~ "Answer two" {
            \\            :speaker: "You chose two"
            \\        }
            \\    }
            \\    :speaker: "Continue here after fork"
            \\ }
            ,
            .expected = &[_][]const u8{"Question", "You chose one", "Continue here after fork"}
        },
        .{
            .input =
            \\ var done = false
            \\ === START {
            \\     :speaker: "Question"
            \\    var count = 0
            \\    fork^ NAMED {
            \\        ~ "Answer one" {
            \\            :speaker: "You chose one"
            \\            if count == 0 {
            \\                count += 1
            \\                =>^ NAMED
            \\            }
            \\            else :speaker: "Else branch"
            \\        }
            \\    }
            \\    => DONE
            \\ }
            \\
            \\ === DONE {
            \\     if done == false {
            \\        done = true
            \\        :speaker: "Not done yet"
            \\     } else :speaker: "Done"
            \\ }
            ,
            .expected = &[_][]const u8{"Question", "You chose one", "You chose one", "Else branch", "Not done yet", "Done"}
        },
    };

    inline for (test_cases) |case| {
        var mod = try Module.initEmpty(allocator);
        defer mod.deinit();
        var vm = try initTestVm(case.input, mod, false);
        defer vm.deinit();
        const test_runner: *TestRunner = @fieldParentPtr("runner", vm.runner);
        defer test_runner.deinit();
        defer vm.bytecode.free(testing.allocator);
        vm.interpret() catch |err| {
            std.debug.print("{s}", .{ case.input });
            try printBytecode(&vm.bytecode);
            printErr(&vm);
            return err;
        };

        test_runner.expectOutput(case.expected) catch |err| {
            std.debug.print("Output: {f}", .{fmt.array("{s}", test_runner.output.items)});
            return err;
        };
    }
}

test "Runtime Jump Code" {
    const test_cases = .{
        .{
            .input =
            \\ === START {
            \\    var firstValue = 0
            \\    :: "First: {firstValue}"
            \\    => INNER
            \\    === INNER {
            \\        var secondValue = 0
            \\        firstValue += 1
            \\        :: "First: {firstValue}, Second: {secondValue}"
            \\        if INNER == 1 => START
            \\        else :: "Done"
            \\    }
            \\ }
            ,
            .expected = &[_][]const u8{"First: 0", "First: 1, Second: 0", "First: 0", "First: 1, Second: 0", "Done"}
        },
        .{
            .input =
            \\ === START {
            \\    var firstValue = 0
            \\    :: "First: {firstValue}"
            \\    === INNER {
            \\        var secondValue = 0
            \\        firstValue += 1
            \\        :: "First: {firstValue}, Second: {secondValue}"
            \\        if INNER == 1 => START
            \\        else if INNER == 2 => INNER
            \\        else :: "Done"
            \\    }
            \\    => INNER
            \\ }
            ,
            .expected = &[_][]const u8{"First: 0", "First: 1, Second: 0", "First: 0", "First: 1, Second: 0", "First: 2, Second: 0", "Done"}
        },
        .{
            .input =
            \\ === START {
            \\    if true {
            \\        :: "Testing fin"
            \\        fin
            \\    }
            \\    :: "Fin did not work!"
            \\ }
            ,
            .expected = &[_][]const u8{"Testing fin"}
        },
        .{
            .input =
            \\ === START {
            \\    =>^ INNER
            \\    =>^ INNER
            \\    === INNER {
            \\        if INNER == 1 :: "Fin executed correctly"
            \\        if INNER == 2 fin
            \\    }
            \\    :: "Fin did not work!"
            \\ }
            ,
            .expected = &[_][]const u8{"Fin executed correctly"}
        },
    };

    inline for (test_cases) |case| {
        var mod = try Module.initEmpty(allocator);
        defer mod.deinit();
        var vm = try initTestVm(case.input, mod, false);
        defer vm.deinit();
        const test_runner: *TestRunner = @fieldParentPtr("runner", vm.runner);
        defer test_runner.deinit();
        defer vm.bytecode.free(testing.allocator);
        vm.interpret() catch |err| {
            printErr(&vm);
            return err;
        };
        test_runner.expectOutput(case.expected) catch |err| {
            std.debug.print("{s}\n", .{case.input});
            std.debug.print("Output: {f}\n", .{fmt.array("{s}", test_runner.output.items)});
            return err;
        };
    }
}

test "Runtime Circular References" {
    const input =
        \\ var one = List{}
        \\ var two = List{"two"}
        \\ one.add(two)
        \\ two.add(one)
        \\ :: "{one[0][0]}"
    ;
    var mod = try Module.initEmpty(allocator);
    defer mod.deinit();
    var vm = try initTestVm(input, mod, false);
    defer vm.deinit();
    const test_runner: *TestRunner = @fieldParentPtr("runner", vm.runner);
    defer test_runner.deinit();
    defer vm.bytecode.free(testing.allocator);
    vm.interpret() catch |err| {
        printErr(&vm);
        return err;
    };
    try test_runner.expectOutput(&[_][]const u8 {"two"});
}

test "Runtime Switch" {
    const test_cases = .{
        .{
            .input =
            \\ var i = -1
            \\ switch 1 {
            \\     0: i = 0,
            \\     1: i = 1
            \\ }
            \\ assert(i == 1, "{i} == 1");
            \\ i
            ,
            .value = 1.0,
        },
        .{
            .input =
            \\ var i = -1
            \\ switch -1 {
            \\     0: i = 0,
            \\     1: i = 1
            \\ }
            \\ assert(i == -1, "{i} == -1");
            \\ i
            ,
            .value = -1.0,
        },
        .{
            .input =
            \\ var i = -1
            \\ switch 1 {
            \\     0: i = 0,
            \\     1: i = 1,
            \\ }
            \\ assert(i == 1, "{i} == 1");
            \\ i
            ,
            .value = 1.0,
        },
        .{
            .input =
            \\ var i = -1
            \\ switch 1 {
            \\     0: i = 0,
            \\     1: i = 1,
            \\     else: i = 5
            \\ }
            \\ assert(i == 1, "{i} == 1");
            \\ i
            ,
            .value = 1.0,
        },
        .{
            .input =
            \\ var i = -1
            \\ switch 10 {
            \\     0: i = 0,
            \\     1: i = 1,
            \\     else: i = 5
            \\ }
            \\ assert(i == 5, "{i} == 5");
            \\ i
            ,
            .value = 5.0,
        },
        .{
            .input =
            \\ var i = -1
            \\ switch 4 {
            \\     0,1,2,3: i = 0,
            \\     4,5,6,7: i = 4,
            \\     else: i = 5
            \\ }
            \\ assert(i == 4, "{i} == 4");
            \\ i
            ,
            .value = 4.0,
        },
        .{
            .input =
            \\ var i = -1
            \\ switch 4 {
            \\     0..3: i = 0,
            \\     4..7: i = 4,
            \\     else: i = 5
            \\ }
            \\ assert(i == 4, "{i} == 4");
            \\ i
            ,
            .value = 4.0,
        },
        .{
            .input =
            \\ var i = -1
            \\ switch "test" {
            \\     "one": i = 0,
            \\     "two": i = 4,
            \\     "test": i = 5
            \\ }
            \\ assert(i == 5, "{i} == 5");
            \\ i
            ,
            .value = 5.0,
        },
    };
    inline for (test_cases) |case| {
        var mod = try Module.initEmpty(allocator);
        defer mod.deinit();
        var vm = try initTestVm(case.input, mod, false);
        defer vm.deinit();
        defer (@as(*TestRunner, @fieldParentPtr("runner", vm.runner))).deinit();
        defer vm.bytecode.free(testing.allocator);
        vm.interpret() catch |err| {
            printErr(&vm);
            return err;
        };
    }
}

test "Runtime Externs and Subscribers" {
    const test_cases = .{
        .{ .input =
        \\ var value = 1
        \\ value = 5
        , .value = 5.0 },
    };

    inline for (test_cases) |case| {
        var mod = try Module.initEmpty(allocator);
        defer mod.deinit();
        var vm = try initTestVm(case.input, mod, false);
        defer vm.deinit();
        const test_runner: *TestRunner = @fieldParentPtr("runner", vm.runner);
        defer test_runner.deinit();
        defer vm.bytecode.free(testing.allocator);
        _ = try vm.subscribeToValueChange("value");
        try vm.interpret();
        _ = vm.unsubscribeToValueChange("value");
        try testing.expect(case.value == vm.stack.previous().number);
        try testing.expect(case.value == test_runner.values_changed.pop().?.number);
    }
}

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

    var mod = try Module.initEmpty(allocator);
    defer mod.deinit();
    var vm = try initTestVm(test_case, mod, false);
    defer vm.deinit();
    defer (@as(*TestRunner, @fieldParentPtr("runner", vm.runner))).deinit();
    defer vm.bytecode.free(alloc);
    try vm.interpret();

    // Serialize initial state after interpret
    var data1: std.io.Writer.Allocating = .init(alloc);
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

    var mod2 = try Module.initEmpty(allocator);
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

    var mod = try Module.initEmpty(allocator);
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
    var mod2 = try Module.initEmpty(allocator);
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

    var mod = try Module.initEmpty(allocator);
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
    var mod2 = try Module.initEmpty(allocator);
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

    var mod = try Module.initEmpty(allocator);
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
    var mod2 = try Module.initEmpty(allocator);
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
    var mod1 = try Module.initEmpty(allocator);
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
    var mod2 = try Module.initEmpty(allocator);
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

    var mod = try Module.initEmpty(allocator);
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
    var data: std.io.Writer.Allocating = .init(alloc);
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
    var mod2 = try Module.initEmpty(allocator);
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

    var mod = try Module.initEmpty(allocator);
    defer mod.deinit();
    var vm = try initTestVm(script, mod, false);
    defer vm.deinit();
    defer (@as(*TestRunner, @fieldParentPtr("runner", vm.runner))).deinit();
    defer vm.bytecode.free(alloc);
    try vm.interpret();

    const first_result = vm.globals[1].number;
    try testing.expect(first_result == 10 or first_result == 20 or first_result == 30);

    var data: std.io.Writer.Allocating = .init(alloc);
    defer data.deinit();
    try State.serialize(&vm, &data.writer);

    const script2 =
        \\ var l = List{10, 20, 30}
        \\ var next = shuffle(l)
    ;
    var mod2 = try Module.initEmpty(allocator);
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

    var mod = try Module.initEmpty(allocator);
    defer mod.deinit();
    var vm = try initTestVm(script1, mod, false);
    defer vm.deinit();
    defer (@as(*TestRunner, @fieldParentPtr("runner", vm.runner))).deinit();
    defer vm.bytecode.free(alloc);
    try vm.interpret();

    var data: std.io.Writer.Allocating = .init(alloc);
    defer data.deinit();
    try State.serialize(&vm, &data.writer);

    var mod2 = try Module.initEmpty(allocator);
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

    var mod = try Module.initEmpty(allocator);
    defer mod.deinit();
    var vm = try initTestVm(script1, mod, false);
    defer vm.deinit();
    defer (@as(*TestRunner, @fieldParentPtr("runner", vm.runner))).deinit();
    defer vm.bytecode.free(alloc);
    try vm.interpret();

    var data: std.io.Writer.Allocating = .init(alloc);
    defer data.deinit();
    try State.serialize(&vm, &data.writer);

    var mod2 = try Module.initEmpty(allocator);
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

    var mod = try Module.initEmpty(allocator);
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

test "Runtime Includes" {
    const main_contents =
        \\ include "./test1.topi"
        \\ include "./test2.topi" // will be skipped in compilation, but good to do
        \\ var m = Test.Main
        \\ assert(m == Test.Main, "m == Test.Main")
        \\ assert(t1 == Test.One, "t1 == Test.One")
        \\ assert(t2 == Test.Two, "t2 == Test.Two")
    ;
    const test1_contents =
        \\ include "./test2.topi"
        \\ var t1 = Test.One
    ;
    const test2_contents =
        \\ enum Test {
        \\   Main,
        \\   One,
        \\   Two
        \\ }
        \\ var t2 = Test.Two
    ;

    const cwd = std.fs.cwd();
    const main_file = try cwd.createFile("main.topi", .{ .read = true });
    const test1_file = try cwd.createFile("test1.topi", .{ .read = true });
    const test2_file = try cwd.createFile("test2.topi", .{ .read = true });
    defer cwd.deleteFile("main.topi") catch {};
    defer cwd.deleteFile("test1.topi") catch {};
    defer cwd.deleteFile("test2.topi") catch {};

    try main_file.writeAll(main_contents);
    try test1_file.writeAll(test1_contents);
    try test2_file.writeAll(test2_contents);
    main_file.close();
    test1_file.close();
    test2_file.close();

    var buffer: [128]u8 = undefined;
    var writer = std.fs.File.stderr().writer(&buffer);
    const err_writer = &writer.interface;
    const entry_path = try std.fs.cwd().realpathAlloc(std.testing.allocator, "main.topi");
    defer std.testing.allocator.free(entry_path);
    var mod = try Module.init(std.testing.allocator, entry_path);
    defer mod.deinit();

    const bytecode = mod.generateBytecode(std.testing.allocator) catch |err| {
        try mod.writeErrors(err_writer);
        return err;
    };
    defer bytecode.free(std.testing.allocator);

    var test_runner = try TestRunner.init(allocator);
    var vm = try Vm.init(std.testing.allocator, bytecode, &test_runner.runner);
    defer vm.deinit();
    defer test_runner.deinit();
    vm.interpret() catch |err| {
        printErr(&vm);
        return err;
    };
}

test "Runtime Error Trace with Function Name" {
    var mod = try Module.initEmpty(allocator);
    defer mod.deinit();

    var vm = try initTestVm(
        \\ fn add |a, b| {
        \\     return a + b
        \\ }
        \\ add(1, "hello")
    , mod, false);
    defer vm.deinit();
    defer (@as(*TestRunner, @fieldParentPtr("runner", vm.runner))).deinit();
    defer vm.bytecode.free(testing.allocator);

    const result = vm.interpret();
    try testing.expectError(error.RuntimeError, result);
    try testing.expect(vm.err.trace.items.len > 0);

    const trace = vm.err.trace.items[0];
    try testing.expectEqualStrings("add", trace.function_name.?);
    try testing.expect(trace.line > 0);
    try testing.expect(trace.file != null);
}

test "Runtime Error Trace with Nested Function Names" {
    var mod = try Module.initEmpty(allocator);
    defer mod.deinit();

    var vm = try initTestVm(
        \\ fn inner |x| {
        \\     return x + "bad"
        \\ }
        \\ fn outer || {
        \\     return inner(1)
        \\ }
        \\ outer()
    , mod, false);
    defer vm.deinit();
    defer (@as(*TestRunner, @fieldParentPtr("runner", vm.runner))).deinit();
    defer vm.bytecode.free(testing.allocator);

    const result = vm.interpret();
    try testing.expectError(error.RuntimeError, result);
    try testing.expect(vm.err.trace.items.len >= 2);

    // innermost frame first
    try testing.expectEqualStrings("inner", vm.err.trace.items[0].function_name.?);
    try testing.expectEqualStrings("outer", vm.err.trace.items[1].function_name.?);
}

test "Runtime Error Trace without Function Name" {
    var mod = try Module.initEmpty(allocator);
    defer mod.deinit();

    // Error at top-level scope — no function name
    var vm = try initTestVm(
        \\ var x = 1 + "bad"
    , mod, false);
    defer vm.deinit();
    defer (@as(*TestRunner, @fieldParentPtr("runner", vm.runner))).deinit();
    defer vm.bytecode.free(testing.allocator);

    const result = vm.interpret();
    try testing.expectError(error.RuntimeError, result);
    // Top-level errors may have no trace entries since frame 0 is skipped
}

test "Runtime Cycle" {
    var mod = try Module.initEmpty(allocator);
    defer mod.deinit();
    var vm = try initTestVm(
        \\var l = List{1, 2, 3}
        \\var r1 = cycle(l)
        \\var r2 = cycle(l)
        \\var r3 = cycle(l)
        \\var r4 = cycle(l)
        \\var r5 = cycle(l)
    , mod, false);
    defer vm.deinit();
    defer (@as(*TestRunner, @fieldParentPtr("runner", vm.runner))).deinit();
    defer vm.bytecode.free(testing.allocator);
    vm.interpret() catch |err| {
        printErr(&vm);
        return err;
    };
    // cycle wraps: 1, 2, 3, 1, 2
    try testing.expectEqual(@as(f32, 1), vm.globals[1].number); // r1
    try testing.expectEqual(@as(f32, 2), vm.globals[2].number); // r2
    try testing.expectEqual(@as(f32, 3), vm.globals[3].number); // r3
    try testing.expectEqual(@as(f32, 1), vm.globals[4].number); // r4
    try testing.expectEqual(@as(f32, 2), vm.globals[5].number); // r5
}

test "Runtime Sequence" {
    var mod = try Module.initEmpty(allocator);
    defer mod.deinit();
    var vm = try initTestVm(
        \\var l = List{1, 2, 3}
        \\var r1 = sequence(l)
        \\var r2 = sequence(l)
        \\var r3 = sequence(l)
        \\var r4 = sequence(l)
        \\var r5 = sequence(l)
    , mod, false);
    defer vm.deinit();
    defer (@as(*TestRunner, @fieldParentPtr("runner", vm.runner))).deinit();
    defer vm.bytecode.free(testing.allocator);
    vm.interpret() catch |err| {
        printErr(&vm);
        return err;
    };
    // sequence sticks on last: 1, 2, 3, 3, 3
    try testing.expectEqual(@as(f32, 1), vm.globals[1].number); // r1
    try testing.expectEqual(@as(f32, 2), vm.globals[2].number); // r2
    try testing.expectEqual(@as(f32, 3), vm.globals[3].number); // r3
    try testing.expectEqual(@as(f32, 3), vm.globals[4].number); // r4
    try testing.expectEqual(@as(f32, 3), vm.globals[5].number); // r5
}

test "Runtime Shuffle" {
    var mod = try Module.initEmpty(allocator);
    defer mod.deinit();
    var vm = try initTestVm(
        \\var l = List{1, 2, 3}
        \\var r1 = shuffle(l)
        \\var r2 = shuffle(l)
        \\var r3 = shuffle(l)
    , mod, false);
    defer vm.deinit();
    defer (@as(*TestRunner, @fieldParentPtr("runner", vm.runner))).deinit();
    defer vm.bytecode.free(testing.allocator);
    vm.interpret() catch |err| {
        printErr(&vm);
        return err;
    };
    // shuffle returns all 3 elements in some order
    const r1 = vm.globals[1].number;
    const r2 = vm.globals[2].number;
    const r3 = vm.globals[3].number;
    // each result should be 1, 2, or 3
    try testing.expect(r1 == 1 or r1 == 2 or r1 == 3);
    try testing.expect(r2 == 1 or r2 == 2 or r2 == 3);
    try testing.expect(r3 == 1 or r3 == 2 or r3 == 3);
    // all three should be different (complete permutation)
    try testing.expect(r1 != r2 and r2 != r3 and r1 != r3);
}

test "Runtime Random" {
    var mod = try Module.initEmpty(allocator);
    defer mod.deinit();
    var vm = try initTestVm(
        \\var l = List{1, 2, 3}
        \\var r1 = random(l)
        \\var r2 = random(l)
    , mod, false);
    defer vm.deinit();
    defer (@as(*TestRunner, @fieldParentPtr("runner", vm.runner))).deinit();
    defer vm.bytecode.free(testing.allocator);
    vm.interpret() catch |err| {
        printErr(&vm);
        return err;
    };
    // random returns a valid list element
    const r1 = vm.globals[1].number;
    const r2 = vm.globals[2].number;
    try testing.expect(r1 == 1 or r1 == 2 or r1 == 3);
    try testing.expect(r2 == 1 or r2 == 2 or r2 == 3);
}

test "Runtime Cycle Empty List" {
    var mod = try Module.initEmpty(allocator);
    defer mod.deinit();
    var vm = try initTestVm("cycle(List{})", mod, false);
    defer vm.deinit();
    defer (@as(*TestRunner, @fieldParentPtr("runner", vm.runner))).deinit();
    defer vm.bytecode.free(testing.allocator);
    vm.interpret() catch |err| {
        printErr(&vm);
        return err;
    };
    try testing.expect(vm.stack.previous() == .nil);
}

// ---------------------------------------------------------------------------
// GC stress tests. These force the collector to run on (nearly) every
// allocation so that any missing roots or unrooted intermediates surface as
// use-after-free corruption rather than latent bugs. Each test targets a
// specific risk area identified in the GC audit.
// ---------------------------------------------------------------------------

fn gcStressVm(source: []const u8, mod: *Module) !Vm {
    var vm = try initTestVm(source, mod, false);
    vm.gc.setThreshold(0);
    return vm;
}

test "GC: iterator holds list while body allocates" {
    // The for loop holds `items` only via the iterator (vm.iterators). Each
    // iteration of the loop body allocates a new string via split_method,
    // which forces a collection. If the iterator were not a GC root, the
    // list would be swept and the iterator would dereference freed memory.
    const source =
        \\ var items = List{"alpha,beta", "gamma,delta", "epsilon,zeta"}
        \\ var total = 0
        \\ for items |s| {
        \\     var parts = s.split(",")
        \\     total = total + parts.count()
        \\ }
    ;
    var mod = try Module.initEmpty(allocator);
    defer mod.deinit();
    var vm = try gcStressVm(source, mod);
    defer vm.deinit();
    defer (@as(*TestRunner, @fieldParentPtr("runner", vm.runner))).deinit();
    defer vm.bytecode.free(testing.allocator);
    vm.interpret() catch |err| {
        printErr(&vm);
        return err;
    };
    const total_idx = try vm.getGlobalsIndex("total");
    try testing.expect(vm.globals[total_idx].number == 6);
}

test "GC: split produces many strings under collection pressure" {
    // split_method appends intermediate GC-allocated strings to a local
    // ArrayList (not a GC root) before wrapping it in a list. With the
    // threshold at 0, every gc.create triggers a full mark-sweep, so any
    // missing push-protection would free earlier strings.
    const source =
        \\ var parts = "a,b,c,d,e,f,g,h,i,j".split(",")
        \\ var joined = ""
        \\ for parts |p| {
        \\     joined = joined + p
        \\ }
    ;
    var mod = try Module.initEmpty(allocator);
    defer mod.deinit();
    var vm = try gcStressVm(source, mod);
    defer vm.deinit();
    defer (@as(*TestRunner, @fieldParentPtr("runner", vm.runner))).deinit();
    defer vm.bytecode.free(testing.allocator);
    vm.interpret() catch |err| {
        printErr(&vm);
        return err;
    };
    const joined_idx = try vm.getGlobalsIndex("joined");
    const joined = vm.globals[joined_idx].obj.data.string;
    try testing.expectEqualStrings("abcdefghij", joined);
}

test "GC: nested list construction under pressure" {
    // Building a list-of-lists inside the VM exercises both the list opcode
    // handlers and the GC's handling of nested obj references. Verifies the
    // expression-evaluation stack keeps children rooted while the parent is
    // being built.
    const source =
        \\ var grid = List{List{1,2,3}, List{4,5,6}, List{7,8,9}}
        \\ var total = 0
        \\ for grid |row| {
        \\     for row |cell| {
        \\         total = total + cell
        \\     }
        \\ }
    ;
    var mod = try Module.initEmpty(allocator);
    defer mod.deinit();
    var vm = try gcStressVm(source, mod);
    defer vm.deinit();
    defer (@as(*TestRunner, @fieldParentPtr("runner", vm.runner))).deinit();
    defer vm.bytecode.free(testing.allocator);
    vm.interpret() catch |err| {
        printErr(&vm);
        return err;
    };
    const total_idx = try vm.getGlobalsIndex("total");
    try testing.expect(vm.globals[total_idx].number == 45);
}

test "GC: state round-trip with nested containers under pressure" {
    // state.deserialize rebuilds nested containers via recursion. The
    // ArrayList/HashMap builders are not GC roots, so children could be
    // collected mid-build if intermediates are not push-protected.
    const source =
        \\ var data = List{List{"a","b"}, List{"c","d","e"}, Map{1:List{"f"}}}
    ;
    var mod = try Module.initEmpty(allocator);
    defer mod.deinit();

    // First VM: serialize, then tear down cleanly.
    var buf: [8192]u8 = undefined;
    const serialized_len = blk: {
        var vm1 = try initTestVm(source, mod, false);
        defer vm1.deinit();
        defer (@as(*TestRunner, @fieldParentPtr("runner", vm1.runner))).deinit();
        defer vm1.bytecode.free(testing.allocator);
        try vm1.interpret();

        var writer = std.Io.Writer.fixed(&buf);
        try State.serialize(&vm1, &writer);
        break :blk writer.end;
    };

    // Second VM: deserialize under extreme GC pressure, then verify the
    // tree is still intact.
    var mod2 = try Module.initEmpty(allocator);
    defer mod2.deinit();
    var vm2 = try initTestVm(source, mod2, false);
    defer vm2.deinit();
    defer (@as(*TestRunner, @fieldParentPtr("runner", vm2.runner))).deinit();
    defer vm2.bytecode.free(testing.allocator);

    vm2.gc.setThreshold(0);
    var reader = std.Io.Reader.fixed(buf[0..serialized_len]);
    try State.deserialize(&vm2, &reader);

    // Force one more collection by interpreting — if any of the restored
    // children were collected during deserialize, this will crash or
    // produce wrong data.
    try vm2.interpret();

    const data_idx = try vm2.getGlobalsIndex("data");
    const outer = vm2.globals[data_idx].obj.data.list;
    try testing.expect(outer.items.len == 3);
    const first = outer.items[0].obj.data.list;
    try testing.expect(first.items.len == 2);
    try testing.expectEqualStrings("a", first.items[0].obj.data.string);
    try testing.expectEqualStrings("b", first.items[1].obj.data.string);
    const second = outer.items[1].obj.data.list;
    try testing.expect(second.items.len == 3);
    try testing.expectEqualStrings("e", second.items[2].obj.data.string);
}

test "GC: class instance with default list field under pressure" {
    // createInstance clones the class's default field values. Each clone of
    // a container allocates a new GC object; with threshold 0, every clone
    // triggers mark-sweep, so earlier clones must be push-protected.
    const source =
        \\ class Thing {
        \\     items = List{1,2,3}
        \\     pair = Map{10:20}
        \\ }
        \\ var t = new Thing{}
    ;
    var mod = try Module.initEmpty(allocator);
    defer mod.deinit();
    var vm = try gcStressVm(source, mod);
    defer vm.deinit();
    defer (@as(*TestRunner, @fieldParentPtr("runner", vm.runner))).deinit();
    defer vm.bytecode.free(testing.allocator);
    vm.interpret() catch |err| {
        printErr(&vm);
        return err;
    };
    const t_idx = try vm.getGlobalsIndex("t");
    const inst = vm.globals[t_idx].obj.data.instance;
    // fields[0] = items list, fields[1] = pair map (order matches declaration)
    const items_list = inst.fields[0].obj.data.list;
    try testing.expect(items_list.items.len == 3);
    try testing.expect(items_list.items[0].number == 1);
    try testing.expect(items_list.items[2].number == 3);
    const pair_map = inst.fields[1].obj.data.map;
    try testing.expect(pair_map.keys().len == 1);
    try testing.expect(pair_map.keys()[0].number == 10);
}

test "GC: class default string containers do not leak compile-time items" {
    // Regression: evaluateLiteral allocates heap Value.Objs for each string
    // (and nested container) inside List/Set/Map class-field defaults. On
    // teardown, Class.deinit -> destroyStatic must recursively free those
    // inner items. testing.allocator fails the test on leaks.
    const source =
        \\ class Thing {
        \\     names = List{"a","b","c"},
        \\     tags = Set{"x","y"},
        \\     lookup = Map{"k":"v"}
        \\ }
        \\ var t = new Thing{}
    ;
    var mod = try Module.initEmpty(allocator);
    defer mod.deinit();
    var vm = try initTestVm(source, mod, false);
    defer vm.deinit();
    defer (@as(*TestRunner, @fieldParentPtr("runner", vm.runner))).deinit();
    defer vm.bytecode.free(testing.allocator);
    vm.interpret() catch |err| {
        printErr(&vm);
        return err;
    };
    const t_idx = try vm.getGlobalsIndex("t");
    const inst = vm.globals[t_idx].obj.data.instance;
    const names = inst.fields[0].obj.data.list;
    try testing.expect(names.items.len == 3);
    try testing.expectEqualStrings("a", names.items[0].obj.data.string);
    try testing.expectEqualStrings("c", names.items[2].obj.data.string);
    const tags = inst.fields[1].obj.data.set;
    try testing.expect(tags.keys().len == 2);
    const lookup = inst.fields[2].obj.data.map;
    try testing.expect(lookup.keys().len == 1);
    try testing.expectEqualStrings("k", lookup.keys()[0].obj.data.string);
    try testing.expectEqualStrings("v", lookup.values()[0].obj.data.string);
}

test "GC: circular instance references under pressure" {
    // Two instances reference each other (a.other = b, b.other = a).
    // Under threshold 0, the mark algorithm must handle the cycle via its
    // is_marked early-exit without looping or sweeping reachable objects.
    const source =
        \\ class Node {
        \\     other = nil,
        \\     value = 0,
        \\ }
        \\ var a = new Node{}
        \\ var b = new Node{}
        \\ a.value = 1
        \\ b.value = 2
        \\ a.other = b
        \\ b.other = a
    ;
    var mod = try Module.initEmpty(allocator);
    defer mod.deinit();
    var vm = try gcStressVm(source, mod);
    defer vm.deinit();
    defer (@as(*TestRunner, @fieldParentPtr("runner", vm.runner))).deinit();
    defer vm.bytecode.free(testing.allocator);
    vm.interpret() catch |err| {
        printErr(&vm);
        return err;
    };
    const a_idx = try vm.getGlobalsIndex("a");
    const b_idx = try vm.getGlobalsIndex("b");
    const a_inst = vm.globals[a_idx].obj.data.instance;
    const b_inst = vm.globals[b_idx].obj.data.instance;
    // a.value == 1, b.value == 2
    try testing.expectEqual(@as(f32, 1), a_inst.fields[1].number);
    try testing.expectEqual(@as(f32, 2), b_inst.fields[1].number);
    // a.other is b, b.other is a (circular)
    try testing.expectEqual(vm.globals[b_idx].obj.id, a_inst.fields[0].obj.id);
    try testing.expectEqual(vm.globals[a_idx].obj.id, b_inst.fields[0].obj.id);
}

test "GC: string method chaining under pressure" {
    // Each string method allocates a new GC string. Under threshold 0,
    // intermediates must survive on the stack until consumed by the next
    // method call.
    const source =
        \\ var s1 = "hello".upper()
        \\ var s2 = "WORLD".lower()
        \\ var s3 = "abcabc".replace("a", "x")
        \\ var s4 = "hello world".upper().replace("HELLO", "HI")
    ;
    var mod = try Module.initEmpty(allocator);
    defer mod.deinit();
    var vm = try gcStressVm(source, mod);
    defer vm.deinit();
    defer (@as(*TestRunner, @fieldParentPtr("runner", vm.runner))).deinit();
    defer vm.bytecode.free(testing.allocator);
    vm.interpret() catch |err| {
        printErr(&vm);
        return err;
    };
    const s1_idx = try vm.getGlobalsIndex("s1");
    const s2_idx = try vm.getGlobalsIndex("s2");
    const s3_idx = try vm.getGlobalsIndex("s3");
    const s4_idx = try vm.getGlobalsIndex("s4");
    try testing.expectEqualStrings("HELLO", vm.globals[s1_idx].obj.data.string);
    try testing.expectEqualStrings("world", vm.globals[s2_idx].obj.data.string);
    try testing.expectEqualStrings("xbcxbc", vm.globals[s3_idx].obj.data.string);
    try testing.expectEqualStrings("HI WORLD", vm.globals[s4_idx].obj.data.string);
}

test "GC: map construction with dynamic keys under pressure" {
    // Build a map by inserting string keys produced by split() and string
    // values produced by upper(). Both key and value are freshly allocated
    // GC objects; under threshold 0, neither can be collected before the
    // map insertion completes.
    const source =
        \\ var parts = "a,b,c".split(",")
        \\ var m = Map{}
        \\ for parts |p| {
        \\     m[p] = p.upper()
        \\ }
    ;
    var mod = try Module.initEmpty(allocator);
    defer mod.deinit();
    var vm = try gcStressVm(source, mod);
    defer vm.deinit();
    defer (@as(*TestRunner, @fieldParentPtr("runner", vm.runner))).deinit();
    defer vm.bytecode.free(testing.allocator);
    vm.interpret() catch |err| {
        printErr(&vm);
        return err;
    };
    const m_idx = try vm.getGlobalsIndex("m");
    const map = vm.globals[m_idx].obj.data.map;
    try testing.expectEqual(@as(usize, 3), map.keys().len);
}

test "typeOf builtin: primitives" {
    const test_cases = .{
        .{ .input = "var r = typeOf(1)", .expected = "number" },
        .{ .input = "var r = typeOf(true)", .expected = "bool" },
        .{ .input = "var r = typeOf(\"hello\")", .expected = "string" },
    };
    inline for (test_cases) |tc| {
        var mod = try Module.initEmpty(allocator);
        defer mod.deinit();
        var vm = try initTestVm(tc.input, mod, false);
        defer vm.deinit();
        defer (@as(*TestRunner, @fieldParentPtr("runner", vm.runner))).deinit();
        defer vm.bytecode.free(testing.allocator);
        try vm.interpret();
        const r_idx = try vm.getGlobalsIndex("r");
        try testing.expectEqualStrings(tc.expected, vm.globals[r_idx].const_string);
    }
}

test "typeOf builtin: collections" {
    const test_cases = .{
        .{ .input = "var r = typeOf(List{1, 2})", .expected = "list" },
        .{ .input = "var r = typeOf(Map{\"a\": 1})", .expected = "map" },
        .{ .input = "var r = typeOf(Set{1, 2})", .expected = "set" },
    };
    inline for (test_cases) |tc| {
        var mod = try Module.initEmpty(allocator);
        defer mod.deinit();
        var vm = try initTestVm(tc.input, mod, false);
        defer vm.deinit();
        defer (@as(*TestRunner, @fieldParentPtr("runner", vm.runner))).deinit();
        defer vm.bytecode.free(testing.allocator);
        try vm.interpret();
        const r_idx = try vm.getGlobalsIndex("r");
        try testing.expectEqualStrings(tc.expected, vm.globals[r_idx].const_string);
    }
}

test "typeOf builtin: instance returns class name" {
    const script =
        \\ class Player {
        \\    name = "default",
        \\ }
        \\ var p = new Player{}
        \\ var r = typeOf(p)
    ;
    var mod = try Module.initEmpty(allocator);
    defer mod.deinit();
    var vm = try initTestVm(script, mod, false);
    defer vm.deinit();
    defer (@as(*TestRunner, @fieldParentPtr("runner", vm.runner))).deinit();
    defer vm.bytecode.free(testing.allocator);
    try vm.interpret();
    const r_idx = try vm.getGlobalsIndex("r");
    try testing.expectEqualStrings("Player", vm.globals[r_idx].const_string);
}

test "typeOf builtin: function" {
    const script =
        \\ fn add |x, y| return x + y
        \\ var r = typeOf(add)
    ;
    var mod = try Module.initEmpty(allocator);
    defer mod.deinit();
    var vm = try initTestVm(script, mod, false);
    defer vm.deinit();
    defer (@as(*TestRunner, @fieldParentPtr("runner", vm.runner))).deinit();
    defer vm.bytecode.free(testing.allocator);
    try vm.interpret();
    const r_idx = try vm.getGlobalsIndex("r");
    try testing.expectEqualStrings("function", vm.globals[r_idx].const_string);
}

test "State version marker round-trip" {
    const script =
        \\ var x = 42
    ;
    const alloc = testing.allocator;

    var mod = try Module.initEmpty(allocator);
    defer mod.deinit();
    var vm = try initTestVm(script, mod, false);
    defer vm.deinit();
    defer (@as(*TestRunner, @fieldParentPtr("runner", vm.runner))).deinit();
    defer vm.bytecode.free(alloc);
    try vm.interpret();

    // Serialize and verify __version is present
    var data: std.io.Writer.Allocating = .init(alloc);
    defer data.deinit();
    try State.serialize(&vm, &data.writer);
    const json = data.written();
    try testing.expect(std.mem.indexOf(u8, json, "\"__version\":2") != null);

    // Deserialize works fine with version marker
    var mod2 = try Module.initEmpty(allocator);
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

    var mod = try Module.initEmpty(allocator);
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

    var mod = try Module.initEmpty(allocator);
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

    var mod = try Module.initEmpty(allocator);
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
