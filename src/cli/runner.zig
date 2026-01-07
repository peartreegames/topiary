const std = @import("std");
const print = @import("main.zig").print;

const topi = @import("topi");
const Vm = topi.runtime.Vm;
const Runner = topi.runtime.Runner;
const Line = topi.runtime.Line;
const Choice = topi.runtime.Choice;

const Value = topi.types.Value;

pub const CliRunner = struct {
    runner: Runner,
    is_auto: bool,

    pub fn init(is_auto: bool) CliRunner {
        return .{
            .is_auto = is_auto,
            .runner = .{
                .on_line = onLine,
                .on_choices = onChoices,
                .on_value_changed = onValueChanged,
            },
        };
    }

    pub fn onLine(runner: *Runner, vm: *Vm, dialogue: Line) void {
        const self: *CliRunner = @fieldParentPtr("runner", runner);
        print(":", .{}) catch {};
        if (dialogue.speaker) |speaker| {
            print("{s}", .{speaker}) catch {};
        }
        print(": ", .{}) catch {};
        print("{s}", .{dialogue.content}) catch {};
        if (self.is_auto) {
            print("\n", .{}) catch {};
            vm.selectContinue();
        } else {
            var stdin_buffer: [2]u8 = undefined;
            var stdin_reader = std.fs.File.stdin().reader(&stdin_buffer);
            const stdin = &stdin_reader.interface;
            while (stdin.takeDelimiterExclusive('\n')) |_| {
                vm.selectContinue();
                break;
            } else |_| {
                vm.selectContinue();
            }
        }
    }

    pub fn onChoices(_: *Runner, vm: *Vm, choices: []Choice) void {
        var index: ?usize = null;
        while (index == null) {
            var stdin_buffer: [8]u8 = undefined;
            var stdin_reader = std.fs.File.stdin().reader(&stdin_buffer);
            const stdin = &stdin_reader.interface;
            for (choices, 0..) |choice, i| {
                print("[{d}] {s}\n", .{ i, choice.content }) catch {};
            }

            while (stdin.takeDelimiterExclusive('\n')) |line| {
                index = std.fmt.parseInt(usize, line, 10) catch null;
                if (index == null or index.? >= choices.len) {
                    index = null;
                    print("Invalid value '{s}'.\n", .{line}) catch {};
                }
                break;
            } else |err| switch (err) {
                error.EndOfStream => {},
                error.StreamTooLong => {
                    print("Input too long\n", .{}) catch {};
                },
                error.ReadFailed => {
                    print("Read failed\n", .{}) catch {};
                }
            }
        }
        vm.selectChoice(index.?) catch |err| print("Error: {}", .{err}) catch {};
    }

    pub fn onValueChanged(_: *Runner, _: *Vm, _: []const u8, _: Value) void {}
};

pub const AutoTestRunner = struct {
    runner: Runner,
    rnd: std.Random.Xoshiro256,

    pub fn init() AutoTestRunner {
        return .{
            .rnd = std.Random.DefaultPrng.init(std.crypto.random.int(u64)),
            .runner = .{
                .on_line = onLine,
                .on_choices = onChoices,
                .on_value_changed = onValueChanged,
            },
        };
    }

    pub fn onLine(_: *Runner, vm: *Vm, _: Line) void {
        vm.selectContinue();
    }

    pub fn onChoices(runner: *Runner, vm: *Vm, choices: []Choice) void {
        var auto: *AutoTestRunner = @fieldParentPtr("runner", runner);
        const index = auto.rnd.random().intRangeAtMost(usize, 0, choices.len - 1);
        vm.selectChoice(index) catch |err| {
            std.debug.print("Error: {}", .{err});
        };
    }

    pub fn onValueChanged(_: *Runner, _: *Vm, _: []const u8, _: Value) void {}
};
