const std = @import("std");
const Vm = @import("vm.zig").Vm;
const testing = std.testing;
const parser = @import("parser.zig");
const Scope = @import("scope.zig").Scope;
const compiler = @import("compiler.zig");
const Value = @import("values.zig").Value;
const Errors = @import("compiler-error.zig").CompilerErrors;
const compileSource = @import("compiler.test.zig").compileSource;
const State = @import("state.zig").State;
const runners = @import("runner.zig");
const module = @import("module.zig");
const Runner = runners.Runner;
const Line = runners.Line;
const Choice = runners.Choice;

const Compiler = compiler.Compiler;
const Module = module.Module;
const allocator = std.testing.allocator;

pub const TestRunner = struct {
    runner: Runner,

    pub fn init() TestRunner {
        return .{
            .runner = .{
                .onLineFn = TestRunner.onLine,
                .onChoicesFn = TestRunner.onChoices,
            },
        };
    }

    pub fn onLine(_: *Runner, vm: *Vm, dialogue: Line) void {
        if (dialogue.speaker) |speaker| {
            std.debug.print("{s}: ", .{speaker});
        }
        std.debug.print("{s} ", .{dialogue.content});
        for (dialogue.tags) |tag| {
            std.debug.print("#{s} ", .{tag});
        }
        std.debug.print("    ID:{s}\n", .{dialogue.id});
        vm.selectContinue();
    }

    pub fn onChoices(_: *Runner, vm: *Vm, choices: []Choice) void {
        for (choices, 0..) |choice, i| {
            std.debug.print("[{d}] {s} ", .{ i, choice.content });
            for (choice.tags) |tag| std.debug.print("#{s} ", .{tag});
            std.debug.print("    ID:{s}\n", .{choice.id});
        }

        var rnd = std.rand.DefaultPrng.init(std.crypto.random.int(u64));
        const index = rnd.random().intRangeAtMost(usize, 0, choices.len - 1);
        vm.selectChoice(index) catch |err| {
            std.debug.print("Error: {}", .{err});
        };
    }
};

var test_runner = TestRunner.init();
pub fn initTestVm(source: []const u8, mod: *Module, debug: bool) !Vm {
    const errWriter = std.io.getStdErr().writer();
    var bytecode = compileSource(source, mod) catch |err| {
        try mod.writeErrors(errWriter);
        return err;
    };
    errdefer bytecode.free(allocator);
    if (debug) {
        bytecode.print(std.debug);
    }
    return Vm.init(allocator, bytecode, &test_runner.runner);
}

test "Basics" {
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
        var mod = Module.create(allocator);
        defer mod.deinit();
        defer mod.entry.source_loaded = false;
        var vm = try initTestVm(case.input, &mod, false);
        defer vm.deinit();
        defer vm.bytecode.free(testing.allocator);
        try vm.interpret();
        switch (case.type) {
            f32 => try testing.expect(case.value == vm.stack.previous().number),
            bool => try testing.expect(case.value == vm.stack.previous().bool),
            else => continue,
        }
    }
}

test "Conditionals" {
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
        var mod = Module.create(allocator);
        defer mod.deinit();
        defer mod.entry.source_loaded = false;
        var vm = try initTestVm(case.input, &mod, false);
        defer vm.deinit();
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
test "Variables" {
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
        var mod = Module.create(allocator);
        defer mod.deinit();
        defer mod.entry.source_loaded = false;
        var vm = try initTestVm(case.input, &mod, false);
        defer vm.deinit();
        defer vm.bytecode.free(testing.allocator);
        try vm.interpret();
        const value = vm.stack.previous().number;
        try testing.expect(case.value == value);
    }
}

test "Constant Variables" {
    const test_cases = &[_][]const u8{
        "const one = 1 one = 2",
        "const one = 1 one += 3",
    };

    inline for (test_cases) |case| {
        var mod = Module.create(allocator);
        defer mod.deinit();
        defer mod.entry.source_loaded = false;
        const err = initTestVm(case, &mod, false);
        try testing.expectError(Vm.Error.CompilerError, err);
    }
}

test "Strings" {
    const test_cases = .{
        .{ .input = "\"testing\"", .value = "testing" },
        .{ .input = "\"test\" + \"ing\"", .value = "testing" },
        .{ .input = "\"test\" + \"ing\" + \"testing\"", .value = "testingtesting" },
        .{ .input = "\"{123}test\"", .value = "123test" },
        .{ .input = "\"test{123}\"", .value = "test123" },
        .{ .input = "\"{123}te{4 * 5}st{6 + 7}\"", .value = "123te20st13" },
        .{ .input = "\"test{\"test\"}ing", .value = "testtesting" },
        .{ .input = "\"test{\"\"\"test\"\"\"}ing", .value = "test\"test\"ing" },
        .{ .input = "\"test{\"quote\"\"test\"\"quote\"}ing", .value = "testquote\"test\"quoteing" },
        // .{ .input = "\"test\".has(\"tes\")", .value = true },
        // .{ .input = "\"test\".has(\"foo\")", .value = false },
        // .{ .input = "\"testing\".has(\"tin\")", .value = true },
        // .{ .input = "\"testing\".has(\"tester\")", .value = false },
    };

    inline for (test_cases) |case| {
        var mod = Module.create(allocator);
        errdefer std.log.err("Error on: {s}", .{case.input});
        defer mod.deinit();
        defer mod.entry.source_loaded = false;
        var vm = try initTestVm(case.input, &mod, false);
        defer vm.deinit();
        defer vm.bytecode.free(testing.allocator);
        try vm.interpret();
        const str = vm.stack.previous().obj.data.string;
        try testing.expectEqualStrings(case.value, str[0..(str.len - 1)]);
    }
}

test "Lists" {
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
        var mod = Module.create(allocator);
        defer mod.deinit();
        defer mod.entry.source_loaded = false;
        var vm = try initTestVm(case.input, &mod, false);
        defer vm.deinit();
        defer vm.bytecode.free(testing.allocator);
        try vm.interpret();
        const prev = vm.stack.previous();
        for (case.value, 0..) |v, i| {
            try testing.expect(v == prev.obj.data.list.items[i].number);
        }
    }
}

test "Maps" {
    const test_cases = .{
        .{ .input = "Map{}", .keys = [_]f32{}, .values = [_]f32{} },
        .{ .input = "Map{1:2, 3: 4}", .keys = [_]f32{ 1, 3 }, .values = [_]f32{ 2, 4 } },
        .{ .input = "Map{1 + 1: 2 * 2, 3 + 3: 4 * 4}", .keys = [_]f32{ 2, 6 }, .values = [_]f32{ 4, 16 } },
        .{ .input = "var m = Map{1:2} m.add(3, 4) m", .keys = [_]f32{ 1, 3 }, .values = [_]f32{ 2, 4 } },
        .{ .input = "var m = Map{1:2} m.add(3, 4) m.remove(1) m", .keys = [_]f32{3}, .values = [_]f32{4} },
    };

    inline for (test_cases) |case| {
        var mod = Module.create(allocator);
        defer mod.deinit();
        defer mod.entry.source_loaded = false;
        var vm = try initTestVm(case.input, &mod, false);
        defer vm.deinit();
        defer vm.bytecode.free(testing.allocator);
        try vm.interpret();
        const map = vm.stack.previous().obj.data.map;
        try testing.expect(map.keys().len == case.keys.len);
        if (case.keys.len > 0) {
            for (map.keys(), 0..) |k, i| {
                errdefer std.log.warn("{}:{}", .{ k.number, map.get(k).?.number });
                try testing.expect(case.keys[i] == k.number);
                try testing.expect(case.values[i] == map.get(k).?.number);
            }
        }
    }
}

test "Sets" {
    const test_cases = .{
        .{ .input = "Set{}", .values = [_]f32{} },
        .{ .input = "Set{1, 2}", .values = [_]f32{ 1, 2 } },
        .{ .input = "Set{1 + 1, 3 + 3}", .values = [_]f32{ 2, 6 } },
        .{ .input = "var s = Set{1} s.add(2) s.add(1) s", .values = [_]f32{ 1, 2 } },
        .{ .input = "var s = Set{1} s.add(2) s.remove(1) s", .values = [_]f32{2} },
    };

    inline for (test_cases) |case| {
        var mod = Module.create(allocator);
        defer mod.deinit();
        defer mod.entry.source_loaded = false;
        var vm = try initTestVm(case.input, &mod, false);
        defer vm.deinit();
        defer vm.bytecode.free(testing.allocator);
        try vm.interpret();
        const set = vm.stack.previous().obj.data.set;
        try testing.expect(set.keys().len == case.values.len);
        if (case.values.len > 0) {
            for (set.keys(), 0..) |k, i| {
                errdefer std.log.warn("{}", .{k.number});
                try testing.expect(case.values[i] == k.number);
            }
        }
    }
}

test "Index" {
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
            \\ print(outer)
            \\ outer[0][0][4]
            ,
            .value = 99.0,
        },
    };

    inline for (test_cases) |case| {
        var mod = Module.create(allocator);
        defer mod.deinit();
        defer mod.entry.source_loaded = false;
        var vm = try initTestVm(case.input, &mod, false);
        defer vm.deinit();
        defer vm.bytecode.free(testing.allocator);
        try vm.interpret();
        const value = vm.stack.previous();
        switch (@TypeOf(case.value)) {
            comptime_float => try testing.expect(case.value == value.number),
            else => try testing.expect(value == .nil),
        }
    }
}

test "Functions" {
    const test_cases = .{
        .{ .input = 
        \\ const fifteen = || return 5 + 10
        \\ fifteen()
        , .value = 15.0 },
        .{ .input = 
        \\ const one = || return 1
        \\ const two = || return 2
        \\ one() + two()
        , .value = 3.0 },
        .{ .input = 
        \\ const a = || return 1
        \\ const b = || return a() + 1
        \\ const c = || return b() + 1
        \\ c()
        , .value = 3.0 },
        .{ .input = 
        \\ (|| return 33)()
        , .value = 33.0 },
        .{ .input = 
        \\ const exit = || { return 99 return 100 }
        \\ exit()
        , .value = 99.0 },
        .{ .input = 
        \\ const cond = || { if (true) return 1 return 0 }
        \\ cond()
        , .value = 1.0 },
        .{ .input = 
        \\ const cond = || { if (false) return 1 return 0 }
        \\ cond()
        , .value = 0.0 },
        .{ .input = 
        \\ const noop = || {}
        \\ noop()
        , .value = null },
        .{ .input = 
        \\ const noop = || {}
        \\ const noopop = || noop()
        \\ noop()
        \\ noopop()
        , .value = null },
        .{ .input = 
        \\ const one = || return 1
        \\ const curry = || return one
        \\ curry()()
        , .value = 1.0 },
    };

    inline for (test_cases) |case| {
        var mod = Module.create(allocator);
        defer mod.deinit();
        defer mod.entry.source_loaded = false;
        var vm = try initTestVm(case.input, &mod, false);
        defer vm.deinit();
        defer vm.bytecode.free(testing.allocator);
        try vm.interpret();
        const value = vm.stack.previous();
        switch (@TypeOf(case.value)) {
            comptime_float => try testing.expect(case.value == value.number),
            else => try testing.expect(value == .void),
        }
    }
}

test "Locals" {
    const test_cases = .{
        .{
            .input =
            \\ const oneFn = || {
            \\     var one = 1
            \\     return one
            \\ }
            \\ oneFn()
            ,
            .value = 1.0,
        },
        .{
            .input =
            \\ const threeFn = || {
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
            \\ const threeFn = || {
            \\     var one = 1
            \\     var two = 2
            \\     return one + two
            \\ }
            \\ const sevenFn = || {
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
            \\ const minusOne = || {
            \\     const one = 1
            \\     return five - one
            \\ }
            \\ const minusTwo = || {
            \\     const two = 2
            \\     return five - two
            \\ }
            \\ minusOne() + minusTwo()
            ,
            .value = 7.0,
        },
    };

    inline for (test_cases) |case| {
        var mod = Module.create(allocator);
        defer mod.deinit();
        defer mod.entry.source_loaded = false;
        var vm = try initTestVm(case.input, &mod, false);
        defer vm.deinit();
        defer vm.bytecode.free(testing.allocator);
        try vm.interpret();
        const value = vm.stack.previous();
        errdefer std.log.warn("{s}:: {any} == {any}", .{ case.input, case.value, value });
        switch (@TypeOf(case.value)) {
            comptime_float => try testing.expect(case.value == value.number),
            else => try testing.expect(value == .nil),
        }
    }
}

test "Function Arguments" {
    const test_cases = .{
        .{ .input = 
        \\ const ident = |a| return a
        \\ ident(4)
        , .value = 4.0 },
        .{ .input = 
        \\ const sum = |a, b| return a + b
        \\ sum(1, 2)
        , .value = 3.0 },
        .{ .input = 
        \\ const sum = |a, b| {
        \\    const c = a + b
        \\    return c
        \\ }
        \\ sum(1, 2)
        , .value = 3.0 },
        .{ .input = 
        \\ const sum = |a, b| {
        \\    const c = a + b
        \\    return c
        \\ }
        \\ sum(1, 2) + sum(3, 4)
        , .value = 10.0 },
        .{ .input = 
        \\ const globalNum = 10
        \\ const sum = |a, b| {
        \\     const c = a + b
        \\     return c + globalNum
        \\ }
        \\ sum(5, 5) + globalNum
        , .value = 30.0 },
        .{ .input = 
        \\ const globalNum = 10
        \\ const sum = |a, b| {
        \\     const c = a + b
        \\     return c + globalNum
        \\ }
        \\ const outer = || {
        \\    return
        \\    sum(1, 2) +
        \\    sum(3, 4) +
        \\    globalNum
        \\ }
        \\ outer() + globalNum
        , .value = 50.0 },
    };
    inline for (test_cases) |case| {
        var mod = Module.create(allocator);
        defer mod.deinit();
        defer mod.entry.source_loaded = false;
        var vm = try initTestVm(case.input, &mod, false);
        defer vm.deinit();
        defer vm.bytecode.free(testing.allocator);
        try vm.interpret();
        const value = vm.stack.previous();
        switch (@TypeOf(case.value)) {
            comptime_float => try testing.expect(case.value == value.number),
            else => try testing.expect(value == .nil),
        }
    }
}

test "Builtin Functions" {
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
        var mod = Module.create(allocator);
        defer mod.deinit();
        defer mod.entry.source_loaded = false;
        var vm = try initTestVm(case.input, &mod, false);
        defer vm.deinit();
        defer vm.bytecode.free(testing.allocator);
        vm.interpret() catch |err| {
            vm.err.print(std.io.getStdErr().writer());
            return err;
        };
        const value = vm.stack.previous();
        try testing.expect(value == .number);
    }
}

test "Closures" {
    const test_cases = .{
        .{
            .input =
            \\ const newClosure = |a| {
            \\     return || return a
            \\ }
            \\ const closure = newClosure(99)
            \\ closure()
            ,
            .value = 99.0,
        },
        .{
            .input =
            \\ const newAdder = |a, b| {
            \\     return |c| return a + b + c
            \\ }
            \\ const adder = newAdder(1, 2)
            \\ adder(7)
            ,
            .value = 10.0,
        },
        .{
            .input =
            \\ const newAdder = |a, b| {
            \\     const c = a + b
            \\     return |d| return c + d
            \\ }
            \\ const adder = newAdder(1, 2)
            \\ adder(10)
            ,
            .value = 13.0,
        },
        .{
            .input =
            \\ const countDown = |x| {
            \\     if x == 0 return 0
            \\     else return countDown(x - 1)
            \\ }
            \\ const wrapper = || return countDown(2)
            \\ wrapper()
            ,
            .value = 0.0,
        },
        .{
            .input =
            \\ const wrapper = || {
            \\     const countDown = |x| {
            \\         if x == 0 return 22
            \\         else return countDown(x - 1)
            \\     }
            \\     return countDown(2)
            \\ }
            \\ wrapper()
            ,
            .value = 22.0,
        },
    };
    inline for (test_cases) |case| {
        var mod = Module.create(allocator);
        defer mod.deinit();
        defer mod.entry.source_loaded = false;
        var vm = try initTestVm(case.input, &mod, false);
        defer vm.deinit();
        defer vm.bytecode.free(testing.allocator);
        try vm.interpret();
        const value = vm.stack.previous();
        try testing.expectEqual(value.number, case.value);
    }
}

test "Loops" {
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
        var mod = Module.create(allocator);
        defer mod.deinit();
        defer mod.entry.source_loaded = false;
        var vm = try initTestVm(case.input, &mod, false);
        defer vm.deinit();
        defer vm.bytecode.free(testing.allocator);
        vm.interpret() catch |err| {
            vm.err.print(std.io.getStdErr().writer());
            return err;
        };
        const value = vm.stack.previous();
        value.print(std.debug, vm.bytecode.constants);
        try testing.expectEqual(value.number, case.value);
    }
}

test "Classes" {
    const input =
        \\ class Test = {
        \\    value = 0
        \\ }
    ;
    var mod = Module.create(allocator);
    defer mod.deinit();
    defer mod.entry.source_loaded = false;
    var vm = try initTestVm(input, &mod, false);
    defer vm.deinit();
    defer vm.bytecode.free(testing.allocator);
    try vm.interpret();
    const value = vm.stack.previous();
    try testing.expect(value.obj.data == .class);
    try testing.expectEqualStrings("Test", value.obj.data.class.name);
}

test "Instance" {
    const input =
        \\ class Test = {
        \\    value = 0,
        \\    fn = || return "func",
        \\    incr = |i| self.value += i,
        \\    list = List{},
        \\    nested = List{}
        \\ }
        \\ const test = new Test{}
        \\ test.value = 5
        \\ print(test)
        \\ print(test.value)
        \\ test.value += 1
        \\ print(test.value)
        \\ print(test.fn())
        \\ test.incr(1)
        \\ print(test.value)
        \\ test.list.add(1)
        \\ print(test.list)
        \\ test.list[0] = 99
        \\ print(test.list)
        \\ test.nested.add(2)
        \\ test.list.add(test.nested)
        \\ print(test.list)
        \\ print(test.list[1][0])
    ;
    var mod = Module.create(allocator);
    defer mod.deinit();
    defer mod.entry.source_loaded = false;
    var vm = try initTestVm(input, &mod, false);
    defer vm.deinit();
    defer vm.bytecode.free(testing.allocator);
    vm.interpret() catch |err| {
        vm.err.print(std.io.getStdErr().writer());
        return err;
    };
}

test "Enums" {
    const input =
        \\ enum TimeOfDay = {
        \\  Morning,
        \\  Afternoon,
        \\  Evening,
        \\  Night
        \\ }
        \\
        \\ const timeOfDay = |hour| {
        \\  switch (hour) {
        \\      0..4: return TimeOfDay.Night,
        \\      5..11: return TimeOfDay.Morning,
        \\      12..16: return TimeOfDay.Afternoon,
        \\      16..21: return TimeOfDay.Evening,
        \\      22..24: return TimeOfDay.Night,
        \\  }
        \\ }
        \\
        \\ print(timeOfDay(5))
        \\
    ;

    var mod = Module.create(allocator);
    defer mod.deinit();
    defer mod.entry.source_loaded = false;
    var vm = try initTestVm(input, &mod, false);
    defer vm.deinit();
    defer vm.bytecode.free(testing.allocator);
    try vm.interpret();
}

test "Boughs" {
    const test_cases = .{
        .{ .input = 
        \\ === START {
        \\    :speaker: "Text goes here" #tag1 #tag2
        \\    :speaker: "More text here"
        \\ }
        },
        .{ .input = 
        \\ === START {
        \\    const before = "This is added before"
        \\    const after = "and this is added afterwards"
        \\    :speaker_one: "{before} and then more text here"
        \\    :speaker_two: "Text goes here {after}"
        \\ }
        },
        .{ .input = 
        \\ const repeat = |str, count| {
        \\     var result = ""
        \\     while count > 0 {
        \\          result = result + str
        \\          count -= 1
        \\          print(count)
        \\     }
        \\     return result
        \\ }
        \\ === START {
        \\    :speaker_one: "Hello, {repeat("Yo ", 5)}!"
        \\    :speaker_two: "Uh.. hello?"
        \\ }
        },
        .{ .input = 
        \\ === START {
        \\     if true { :speaker: "This is true" }
        \\     else { :speaker: "This is false" }
        \\ }
        },
        .{ .input = 
        \\ === START {
        \\    if true :speaker: "True text goes here"
        \\    :speaker: "More text here"
        \\    if false :speaker: "False text doesn't appear"
        \\    :speaker: "Final text here"
        \\ }
        },
        .{ .input = 
        \\ === START {
        \\    :speaker: "Text goes here"
        \\    === INNER {
        \\        :speaker: "Inner text here"
        \\    }
        \\    :speaker: "More goes here"
        \\    => INNER
        \\    :speaker: "Final goes here" // should not be printed
        \\ }
        },
        .{ .input = 
        \\ === START {
        \\    :speaker: "Text goes here"
        \\    === OUTER {
        \\        :speaker: "Outer text here doesn't happen"
        \\        === INNER {
        \\            :speaker: "Inner and final text here"
        \\        }
        \\    }
        \\    :speaker: "More goes here"
        \\    => OUTER.INNER
        \\    :speaker: "Text doesn't appear here" // should not be printed
        \\ }
        },
    };

    inline for (test_cases) |case| {
        var mod = Module.create(allocator);
        defer mod.deinit();
        defer mod.entry.source_loaded = false;
        std.debug.print("\n======\n", .{});
        var vm = try initTestVm(case.input, &mod, false);
        defer vm.deinit();
        defer vm.bytecode.free(testing.allocator);
        try vm.interpret();
    }
}

test "Bough Loops" {
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
        var mod = Module.create(allocator);
        defer mod.deinit();
        defer mod.entry.source_loaded = false;
        var vm = try initTestVm(case.input, &mod, false);
        defer vm.deinit();
        defer vm.bytecode.free(testing.allocator);
        vm.interpret() catch |err| {
            vm.err.print(std.io.getStdErr().writer());
            return err;
        };
        const value = vm.stack.previous();
        try testing.expectEqual(value.number, case.value);
    }
}

test "Bough Functions" {
    const test_cases = .{
        .{
            .input =
            \\ === START {
            \\   const t = || return "test"
            \\   :Speaker: "This is a {t()}"
            \\ }
            ,
        },
        .{
            .input =
            \\ === START {
            \\   const sum = |x, y| return x + y
            \\   :Speaker: "1 + 7 equals {sum(1, 7)}"
            \\ }
            ,
        },
        .{
            .input =
            \\ === START {
            \\   const sum = |x, y| return x + y
            \\   => INNER
            \\   === INNER {
            \\     :Speaker: "2 + 7 equals {sum(2, 7)}"
            \\   }
            \\ }
            ,
        },
        .{
            .input =
            \\ const t = "test"
            \\ === START {
            \\   const value = 10
            \\   const sum = |x, y| return x + y + value
            \\   => INNER
            \\   === INNER {
            \\     :Speaker: "3 + 7 + {value} equals {sum(3, 7)}"
            \\     :Speaker: "Testing 123 {t} 123"
            \\   }
            \\ }
            ,
        },
        .{
            .input =
            \\ === START {
            \\   const greet = |time| {
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
        },
    };

    inline for (test_cases) |case| {
        std.debug.print("\n======\n", .{});
        var mod = Module.create(allocator);
        defer mod.deinit();
        defer mod.entry.source_loaded = false;
        var vm = try initTestVm(case.input, &mod, false);
        defer vm.deinit();
        defer vm.bytecode.free(testing.allocator);
        try vm.interpret();
    }
}

test "Forks" {
    const test_cases = .{
        .{
            .input =
            \\ === START {
            \\     :speaker: "Question"
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
        },
        .{
            .input =
            \\ === START {
            \\     :speaker: "Question"
            \\    var count = 0
            \\    fork NAMED {
            \\        ~ "Answer one" {
            \\            :speaker: "You chose one"
            \\            if count < 5 {
            \\                count += 1
            \\                => NAMED
            \\            }
            \\            => DONE
            \\        }
            \\    }
            \\ }
            \\ === DONE {
            \\     :speaker: "Done"
            \\ }
            ,
        },
    };

    inline for (test_cases) |case| {
        std.debug.print("\n======\n", .{});
        var mod = Module.create(allocator);
        defer mod.deinit();
        defer mod.entry.source_loaded = false;
        var vm = try initTestVm(case.input, &mod, false);
        defer vm.deinit();
        defer vm.bytecode.free(testing.allocator);
        try vm.interpret();
    }
}

test "Visits" {
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
            \\ print("START: {START}")
            \\ print("START.NAMED: {START.NAMED}")
            \\ print("START.NAMED.ONE: {START.NAMED.ONE}")
            \\ print("START.NAMED.TWO: {START.NAMED.TWO}")
            \\ }
            ,
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
            \\ print("START: {START}")
            \\ print("START._0.ONE: {START._0.ONE}")
            \\ print("START._0.TWO: {START._0.TWO}")
            \\ }
            ,
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
            \\ print(START)
            \\ print(START.INNER)
            \\ print(START.INNER._0.ONE)
            \\ print(START.INNER._0.TWO)
            \\ }
            ,
        },
        .{
            .input =
            \\ === START {
            \\     :speaker: "Starting"
            \\     fork NAMED {
            \\         ~ ONE "Answer one" {
            \\             :speaker: "You chose one"
            \\             print("ONE={ONE}")
            \\             print("TWO={NAMED.TWO}")
            \\         }
            \\         ~ TWO "Answer two" {
            \\             :speaker: "You chose two"
            \\             print("ONE={NAMED.ONE}")
            \\             print("TWO={TWO}")
            \\         }
            \\     }
            \\ }
            ,
        },
    };

    inline for (test_cases) |case| {
        var mod = Module.create(allocator);
        defer mod.deinit();
        defer mod.entry.source_loaded = false;
        std.debug.print("\n======\n", .{});
        var vm = try initTestVm(case.input, &mod, false);
        defer vm.deinit();
        defer vm.bytecode.free(testing.allocator);
        try vm.interpret();
    }
}

test "Jump Backups" {
    const test_cases = .{
        .{
            .input =
            \\ === START {
            \\     :speaker: "Question"
            \\     => MIDDLE^
            \\     :speaker: "Continue here after divert"
            \\ }
            \\ === MIDDLE {
            \\     :speaker: "Done"
            \\ }
            ,
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
        },
        .{
            .input =
            \\ === START {
            \\     :speaker: "Question"
            \\    var count = 0
            \\    fork NAMED {
            \\        ~ "Answer one" {
            \\            :speaker: "You chose one"
            \\            if count == 0 {
            \\                count += 1
            \\                => NAMED^
            \\            }
            \\            else :speaker: "Else branch"
            \\        }
            \\    }
            \\    => DONE
            \\ }
            \\ var done = false
            \\ === DONE {
            \\     if done == false {
            \\        done = true
            \\        :speaker: "Not done yet"
            \\     } else :speaker: "Done"
            \\ }
            ,
        },
    };

    inline for (test_cases) |case| {
        std.debug.print("\n======\n", .{});
        var mod = Module.create(allocator);
        defer mod.deinit();
        defer mod.entry.source_loaded = false;
        var vm = try initTestVm(case.input, &mod, false);
        defer vm.deinit();
        defer vm.bytecode.free(testing.allocator);
        try vm.interpret();
    }
}

test "Jump Code" {
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
            \\        if (INNER == 1) => START
            \\        else :: "Done"
            \\    }
            \\ }
            ,
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
            \\        if (INNER == 1) => START
            \\        else if (INNER == 2) => INNER
            \\        else :: "Done"
            \\    }
            \\ }
            ,
        },
    };

    inline for (test_cases) |case| {
        std.debug.print("\n======\n", .{});
        var mod = Module.create(allocator);
        defer mod.deinit();
        defer mod.entry.source_loaded = false;
        var vm = try initTestVm(case.input, &mod, false);
        defer vm.deinit();
        defer vm.bytecode.free(testing.allocator);
        vm.interpret() catch |err| {
            vm.err.print(std.io.getStdErr().writer());
            return err;
        };
    }
}

test "Switch" {
    const test_cases = .{
        .{
            .input =
            \\ var i = -1
            \\ switch 1 {
            \\     0: i = 0,
            \\     1: i = 1
            \\ }
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
            \\ i
            ,
            .value = 5.0,
        },
    };
    inline for (test_cases) |case| {
        var mod = Module.create(allocator);
        defer mod.deinit();
        defer mod.entry.source_loaded = false;
        var vm = try initTestVm(case.input, &mod, false);
        defer vm.deinit();
        defer vm.bytecode.free(testing.allocator);
        try vm.interpret();
    }
}

test "Externs and Subscribers" {
    const test_cases = .{
        .{ .input = 
        \\ extern const value = 1
        \\ value
        , .value = 2.0 },
        .{ .input = 
        \\ extern var value = 1
        \\ value = 5
        , .value = 5.0 },
    };

    inline for (test_cases) |case| {
        var mod = Module.create(allocator);
        defer mod.deinit();
        defer mod.entry.source_loaded = false;
        var vm = try initTestVm(case.input, &mod, false);
        defer vm.deinit();
        defer vm.bytecode.free(testing.allocator);
        const Listener = struct {
            pub fn onChange(_: usize, value: Value) void {
                std.debug.print("\nListener::{}\n", .{value});
            }
        };
        try vm.setExtern("value", .{ .number = 2 });
        try vm.subscribeCallback("value", Listener.onChange);
        try vm.interpret();
        try testing.expect(case.value == vm.stack.previous().number);
    }
}

test "Save and Load State" {
    const test_case =
        \\ var value = 0
        \\ value += 1
        \\ var list = List{}
        \\ var outer = List{list}
        \\ var last = list
        \\ var str = "value"
        \\ list.add(value)
        \\ list[0] = "changed"
        \\ class Test = {
        \\    field1 = "one",
        \\    field2 = 2,
        \\ }
        \\ var test = new Test{}
        \\ var func = || return "func"
    ;
    const alloc = testing.allocator;

    var mod = Module.create(allocator);
    defer mod.deinit();
    defer mod.entry.source_loaded = false;
    var vm = try initTestVm(test_case, &mod, false);
    defer vm.deinit();
    defer vm.bytecode.free(testing.allocator);
    try vm.interpret();

    var data = std.ArrayList(u8).init(alloc);
    defer data.deinit();
    try State.serialize(&vm, data.writer());
    std.debug.print("\n{s}\n", .{data.items});

    const second_case =
        \\ var value = 10
        \\ value += 5
        \\ var outer = List{}
        \\ var test = "t"
    ;

    var mod2 = Module.create(allocator);
    defer mod2.deinit();
    defer mod2.entry.source_loaded = false;
    var vm2 = try initTestVm(second_case, &mod2, false);
    defer vm2.deinit();
    defer vm2.bytecode.free(testing.allocator);
    try State.deserialize(&vm2, data.items);
    // try vm2.loadState(&save);
    try testing.expectEqual(vm2.globals[0].number, 1);
    try testing.expectEqualSlices(u8, vm2.globals[1].obj.data.list.items[0].obj.data.list.items[0].obj.data.string, "changed");
    try testing.expectEqual(vm2.globals[2].obj.data.instance.fields[1].number, 2);
    try vm2.interpret();
    try testing.expectEqual(vm2.globals[0].number, 6);

    std.log.warn("CALC SIZE: {}, ACTUAL SIZE: {}", .{ try State.calculateSize(&vm), data.items.len });
}
