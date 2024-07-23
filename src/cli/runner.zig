const std = @import("std");

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

    pub fn print(_: *CliRunner, comptime msg: []const u8, args: anytype) void {
        const stdout = std.io.getStdOut().writer();
        stdout.print(msg, args) catch {
            std.debug.print("Could not print message", .{});
        };
    }

    pub fn onLine(runner: *Runner, vm: *Vm, dialogue: Line) void {
        const stdin = std.io.getStdIn().reader();
        const self: *CliRunner = @fieldParentPtr("runner", runner);
        self.print(":", .{});
        if (dialogue.speaker) |speaker| {
            self.print("{s}", .{speaker});
        }
        self.print(": ", .{});
        self.print("{s}", .{dialogue.content});
        if (self.is_auto) {
            self.print("\n", .{});
            vm.selectContinue();
        } else {
            var buf: [2]u8 = undefined;
            if (stdin.readUntilDelimiterOrEof(&buf, '\n') catch &buf) |_| {
                vm.selectContinue();
            }
        }
    }

    pub fn onChoices(runner: *Runner, vm: *Vm, choices: []Choice) void {
        const stdin = std.io.getStdIn().reader();
        const stderr = std.io.getStdErr().writer();
        const self: *CliRunner = @fieldParentPtr("runner", runner);
        var index: ?usize = null;
        while (index == null) {
            for (choices, 0..) |choice, i| {
                self.print("[{d}] {s}\n", .{ i, choice.content });
            }
            var buf: [10]u8 = undefined;
            if (stdin.readUntilDelimiterOrEof(&buf, '\n') catch &buf) |user_input| {
                const input = std.mem.trim(u8, user_input, "\r\n");
                index = std.fmt.parseInt(usize, input, 10) catch |err| blk: {
                    stderr.print("Invalid value: {}.\n", .{err}) catch {};
                    break :blk null;
                };
                if (index != null and index.? >= choices.len) {
                    index = null;
                    stderr.print("Invalid value.\n", .{}) catch {};
                }
            }
        }
        vm.selectChoice(index.?) catch |err| self.print("Error: {}", .{err});
    }

    pub fn onValueChanged(_: *Runner, _: *Vm, _: []const u8, _: Value) void {}
};

pub const AutoTestRunner = struct {
    runner: Runner,
    rnd: std.rand.Xoshiro256,

    pub fn init() AutoTestRunner {
        return .{
            .rnd = std.rand.DefaultPrng.init(std.crypto.random.int(u64)),
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
