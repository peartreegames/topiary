const std = @import("std");
const Vm = @import("./vm.zig").Vm;
const testing = std.testing;
const parser = @import("./parser.zig");
const Scope = @import("./scope.zig").Scope;
const compiler = @import("./compiler.zig");
const Value = @import("./values.zig").Value;
const Errors = @import("./compiler-error.zig").CompilerErrors;
const StateMap = @import("./state.zig").StateMap;
const runners = @import("./runner.zig");
const Runner = runners.Runner;
const Dialogue = runners.Dialogue;
const Choice = runners.Choice;

const Compiler = compiler.Compiler;
const compileSource = compiler.compileSource;

pub const TestRunner = struct {
    runner: Runner,

    pub fn init() TestRunner {
        return .{
            .runner = .{
                .onDialogueFn = TestRunner.onDialogue,
                .onChoicesFn = TestRunner.onChoices,
            },
        };
    }

    pub fn onDialogue(_: *Runner, vm: *Vm, dialogue: Dialogue) void {
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
pub fn initTestVm(source: []const u8, debug: bool) !Vm {
    var alloc = std.testing.allocator;
    var errors = Errors.init(alloc);
    defer errors.deinit();
    var bytecode = compileSource(alloc, source, &errors) catch |err| {
        errors.write(source, std.io.getStdErr().writer()) catch {};
        return err;
    };
    errdefer bytecode.free(alloc);
    if (debug) {
        bytecode.print(std.debug);
    }
    var vm = try Vm.init(alloc, bytecode, &test_runner.runner);
    return vm;
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
        var vm = try initTestVm(case.input, false);
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
        var vm = try initTestVm(case.input, false);
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
        var vm = try initTestVm(case.input, false);
        defer vm.deinit();
        defer vm.bytecode.free(testing.allocator);
        try vm.interpret();
        var value = vm.stack.previous().number;
        try testing.expect(case.value == value);
    }
}

test "Constant Variables" {
    const test_cases = &[_][]const u8{
        "const one = 1 one = 2",
        "const one = 1 one += 3",
    };

    inline for (test_cases) |case| {
        var err = initTestVm(case, false);
        try testing.expect(Vm.Error.CompilerError == err);
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
        .{ .input = "\"test\".has(\"tes\")", .value = true },
        .{ .input = "\"test\".has(\"foo\")", .value = false },
        .{ .input = "\"testing\".has(\"tin\")", .value = true },
        .{ .input = "\"testing\".has(\"tester\")", .value = false },
    };

    inline for (test_cases) |case| {
        var vm = try initTestVm(case.input, false);
        defer vm.deinit();
        defer vm.bytecode.free(testing.allocator);
        try vm.interpret();
        switch (@TypeOf(case.value)) {
            []const u8 => try testing.expectEqualStrings(case.value, vm.stack.previous().obj.data.string),
            bool => try testing.expect(case.value == vm.stack.previous().bool),
            else => {},
        }
    }
}

test "Lists" {
    const test_cases = .{
        .{ .input = "[]", .value = [_]f32{} },
        .{ .input = "[1,2,3]", .value = [_]f32{ 1, 2, 3 } },
        .{ .input = "[1 + 2, 3 * 4, 5 + 6]", .value = [_]f32{ 3, 12, 11 } },
        .{ .input = "var l = [] l.add(1) l", .value = [_]f32{1} },
        .{ .input = "var l = [] l.add(1) l.add(2) l", .value = [_]f32{ 1, 2 } },
        .{ .input = "var l = [] l.add(1) l.add(2) l.remove(1) l", .value = [_]f32{2} },
        .{ .input = "var l = [1,2,3,4,5] l.remove(3) l", .value = [_]f32{ 1, 2, 4, 5 } },
    };

    inline for (test_cases) |case| {
        var vm = try initTestVm(case.input, false);
        defer vm.deinit();
        defer vm.bytecode.free(testing.allocator);
        try vm.interpret();
        var prev = vm.stack.previous();
        for (case.value, 0..) |v, i| {
            try testing.expect(v == prev.obj.data.list.items[i].number);
        }
    }
}

test "Maps" {
    const test_cases = .{
        .{ .input = "({:})", .keys = [_]f32{}, .values = [_]f32{} },
        .{ .input = "({1:2, 3: 4})", .keys = [_]f32{ 1, 3 }, .values = [_]f32{ 2, 4 } },
        .{ .input = "({1 + 1: 2 * 2, 3 + 3: 4 * 4})", .keys = [_]f32{ 2, 6 }, .values = [_]f32{ 4, 16 } },
        .{ .input = "var m = {1:2} m.add(3, 4) m", .keys = [_]f32{ 1, 3 }, .values = [_]f32{ 2, 4 } },
        .{ .input = "var m = {1:2} m.add(3, 4) m.remove(1) m", .keys = [_]f32{3}, .values = [_]f32{4} },
    };

    inline for (test_cases) |case| {
        var vm = try initTestVm(case.input, false);
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
        .{ .input = "({})", .values = [_]f32{} },
        .{ .input = "({1, 2})", .values = [_]f32{ 1, 2 } },
        .{ .input = "({1 + 1, 3 + 3})", .values = [_]f32{ 2, 6 } },
        .{ .input = "var s = {1} s.add(2) s.add(1) s", .values = [_]f32{ 1, 2 } },
        .{ .input = "var s = {1} s.add(2) s.remove(1) s", .values = [_]f32{2} },
    };

    inline for (test_cases) |case| {
        var vm = try initTestVm(case.input, false);
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
        .{ .input = "[1,2,3][1]", .value = 2.0 },
        .{ .input = "[1,2,3][0 + 2]", .value = 3.0 },
        .{ .input = "[[1,2,3]][0][0]", .value = 1.0 },
        .{ .input = "[][0]", .value = null },
        .{ .input = "[1,2,3][99]", .value = null },
        .{ .input = "({1: 1, 2: 2})[1]", .value = 1.0 },
        .{ .input = "({1: 1, 2: 2})[2]", .value = 2.0 },
        .{ .input = "({1: 1})[2]", .value = null },
        .{ .input = "({:})[0]", .value = null },
        .{ .input = "[1,1,1].count()", .value = 3.0 },
        .{ .input = "({\"one\"}).count()", .value = 1.0 },
        .{ .input = "({\"one\": 1 })[\"one\"]", .value = 1.0 },
    };

    inline for (test_cases) |case| {
        var vm = try initTestVm(case.input, false);
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
        var vm = try initTestVm(case.input, false);
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
        var vm = try initTestVm(case.input, false);
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
        var vm = try initTestVm(case.input, false);
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
        var vm = try initTestVm(case.input, false);
        defer vm.deinit();
        defer vm.bytecode.free(testing.allocator);
        vm.interpret() catch |err| {
            vm.err.print(std.debug);
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
        var vm = try initTestVm(case.input, false);
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
        \\ const list = [1,2,3,4,5]
        \\ var sum = 0
        \\ for list |item| {
        \\     sum += item
        \\ }
        \\ sum
        , .value = 15 },
        .{ .input = 
        \\ const set = {1,2,3,3,3}
        \\ var sum = 0
        \\ for set |item| {
        \\     sum += item
        \\ }
        \\ sum
        , .value = 6 },
        .{ .input = 
        \\ const map = {1:2,3:4,5:6}
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
        \\ const list = [1,2,3,4,5]
        \\ var sum = 0
        \\ for 0..(list.count() - 1) |i| {
        \\     sum += list[i]
        \\ }
        \\ sum
        , .value = 15 },
    };
    inline for (test_cases) |case| {
        var vm = try initTestVm(case.input, false);
        defer vm.deinit();
        defer vm.bytecode.free(testing.allocator);
        vm.interpret() catch |err| {
            vm.err.print(std.debug);
            return err;
        };
        const value = vm.stack.previous();
        try testing.expectEqual(value.number, case.value);
    }
}

test "Classes" {
    const input =
        \\ class Test {
        \\    value = 0
        \\ }
    ;
    var vm = try initTestVm(input, false);
    defer vm.deinit();
    defer vm.bytecode.free(testing.allocator);
    try vm.interpret();
    var value = vm.stack.previous();
    try testing.expect(value.obj.data == .class);
    try testing.expectEqualStrings("Test", value.obj.data.class.name);
}

test "Instance" {
    const input =
        \\ class Test {
        \\    value = 0,
        \\    fn = || return "func",
        \\    incr = |i| self.value += i
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
    ;
    var vm = try initTestVm(input, false);
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
        \\ => START
        },
        .{ .input = 
        \\ === START {
        \\    const before = "This is added before"
        \\    const after = "and this is added afterwards"
        \\    :speaker_one: "{before} and then more text here"
        \\    :speaker_two: "Text goes here {after}"
        \\ }
        \\ => START
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
        \\ => START
        },
        .{ .input = 
        \\ === START {
        \\     if true { :speaker: "This is true" }
        \\     else { :speaker: "This is false" }
        \\ }
        \\ => START
        },
        .{ .input = 
        \\ === START {
        \\    if true :speaker: "True text goes here"
        \\    :speaker: "More text here"
        \\    if false :speaker: "False text doesn't appear"
        \\    :speaker: "Final text here"
        \\ }
        \\ => START
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
        \\ => START
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
        \\ => START
        },
    };

    inline for (test_cases) |case| {
        std.debug.print("\n======\n", .{});
        var vm = try initTestVm(case.input, false);
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
            \\ }
            \\ => START^
            \\ i
            ,
            .value = 5,
        },
    };
    inline for (test_cases) |case| {
        var vm = try initTestVm(case.input, false);
        defer vm.deinit();
        defer vm.bytecode.free(testing.allocator);
        vm.interpret() catch |err| {
            vm.err.print(std.debug);
            return err;
        };
        const value = vm.stack.previous();
        try testing.expectEqual(value.number, case.value);
    }
}

test "Forks" {
    const test_cases = .{
        .{
            .input =
            \\ === START {
            \\     :speaker: "Question"
            \\    fork {
            \\        ~ "Answer one" {
            \\            :speaker: "You chose one"
            \\        }
            \\        ~ "Answer two" {
            \\            :speaker: "You chose two"
            \\        }
            \\    }
            \\ }
            \\ => START
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
            \\ => START
            \\ === DONE {
            \\     :speaker: "Done"
            \\ }
            ,
        },
    };

    inline for (test_cases) |case| {
        std.debug.print("\n======\n", .{});
        var vm = try initTestVm(case.input, false);
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
            \\ }
            \\ => START^
            \\ print("START: {START}")
            \\ print("START.NAMED: {START.NAMED}")
            \\ print("START.NAMED.ONE: {START.NAMED.ONE}")
            \\ print("START.NAMED.TWO: {START.NAMED.TWO}")
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
            \\ }
            \\ => START^
            \\ print("START: {START}")
            \\ print("START._0.ONE: {START._0.ONE}")
            \\ print("START._0.TWO: {START._0.TWO}")
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
            \\ }
            \\ => START.INNER^
            \\ print(START)
            \\ print(START.INNER)
            \\ print(START.INNER._0.ONE)
            \\ print(START.INNER._0.TWO)
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
            \\ => START
            ,
        },
    };

    inline for (test_cases) |case| {
        std.debug.print("\n======\n", .{});
        var vm = try initTestVm(case.input, false);
        defer vm.deinit();
        defer vm.bytecode.free(testing.allocator);
        try vm.interpret();
    }
}

test "Jump Backups" {
    const test_cases = .{
        .{
            .input =
            \\ => START
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
            \\ => START
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
            \\ => START
            ,
        },
    };

    inline for (test_cases) |case| {
        std.debug.print("\n======\n", .{});
        var vm = try initTestVm(case.input, false);
        defer vm.deinit();
        defer vm.bytecode.free(testing.allocator);
        try vm.interpret();
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
        var vm = try initTestVm(case.input, false);
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
        var vm = try initTestVm(case.input, false);
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
    ;
    var alloc = testing.allocator;

    var vm = try initTestVm(test_case, false);
    defer vm.deinit();
    defer vm.bytecode.free(testing.allocator);
    try vm.interpret();

    var save = StateMap.init(alloc);
    defer save.deinit();
    try vm.saveState(&save);

    const second_case =
        \\ var value = 10
        \\ value += 5
    ;

    var vm2 = try initTestVm(second_case, false);
    defer vm2.deinit();
    defer vm2.bytecode.free(testing.allocator);
    try vm2.loadState(&save);
    try testing.expectEqual(vm2.globals[0].number, 1);

    try vm2.interpret();

    try testing.expectEqual(vm2.globals[0].number, 6);
}
