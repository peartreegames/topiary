const std = @import("std");
const testing = std.testing;


const topi = @import("topi");
const Vm = topi.runtime.Vm;
const Runner = topi.runtime.Runner;
const State = topi.runtime.State;
const fmt = topi.utils.fmt;

const Bytecode = topi.backend.Bytecode;

const Module = topi.module.Module;

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
        // .{ .input = "\"test\".has(\"tes\")", .value = true },
        // .{ .input = "\"test\".has(\"foo\")", .value = false },
        // .{ .input = "\"testing\".has(\"tin\")", .value = true },
        // .{ .input = "\"testing\".has(\"tester\")", .value = false },
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
        \\    value = 0,
        \\    fn one || return 1
        \\ }
        \\ var test = new Test{}
        \\ test.val = 55
        ,
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
            \\    fork^ {
            \\        ~* ONE "Answer one" {
            \\            :speaker: "You chose one"
            \\        }
            \\        ~ TWO "Answer two" {
            \\            :speaker: "You chose two"
            \\        }
            \\    }
            \\ assert(START == 1, "START == 1")
            \\ assert(START._0.ONE == 1, "START._0.ONE == 1")
            \\ assert(START._0.TWO == 0, "START._0.TWO == 0")
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
            \\        fork^ {
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
            \\ assert(START.INNER._0.ONE == 1, "START.INNER._0.ONE == 1")
            \\ assert(START.INNER._0.TWO == 0, "START.INNER._0.TWO == 0")
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
        _ = vm.unusbscribeToValueChange("value");
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
    try testing.expectEqual(572, size);
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
