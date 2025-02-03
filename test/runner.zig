const std = @import("std");

const topi = @import("topi");
const Vm = topi.runtime.Vm;
const Runner = topi.runtime.Runner;
const Line = topi.runtime.Line;
const Choice = topi.runtime.Choice;

const Value = topi.types.Value;

pub const TestRunner = struct {
    runner: Runner,

    pub fn init() TestRunner {
        return .{
            .runner = .{
                .on_line = TestRunner.onLine,
                .on_choices = TestRunner.onChoices,
                .on_value_changed = TestRunner.onValueChanged,
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

    pub fn onValueChanged(_: *Runner, _: *Vm, name: []const u8, value: Value) void {
        std.debug.print("Value Changed Callback: {s}\n", .{name});
        value.print(std.io.getStdErr().writer(), null) catch unreachable;
    }
};
