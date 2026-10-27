const std = @import("std");
const print = @import("main.zig").print;
const io = @import("main.zig").io;

const topi = @import("topi");
const Vm = topi.runtime.Vm;
const Runner = topi.runtime.Runner;
const Line = topi.runtime.Line;
const Choice = topi.runtime.Choice;

const Value = topi.types.Value;

pub const CliRunner = struct {
    runner: Runner,
    is_auto: bool,
    json: bool,
    scripted_choices: []const usize,
    next_scripted: usize = 0,
    stop_reason: ?StopReason = null,

    pub const StopReason = enum { awaiting_choice, invalid_choice };

    pub fn init(is_auto: bool, json: bool, scripted_choices: []const usize) CliRunner {
        return .{
            .is_auto = is_auto or json,
            .json = json,
            .scripted_choices = scripted_choices,
            .runner = .{
                .on_line = onLine,
                .on_choices = onChoices,
                .on_value_changed = onValueChanged,
            },
        };
    }

    pub fn onLine(runner: *Runner, vm: *Vm, dialogue: *const Line) void {
        const self: *CliRunner = @fieldParentPtr("runner", runner);
        if (self.json) {
            emitJson(.{ .type = "line", .speaker = dialogue.speaker, .content = dialogue.content, .tags = dialogue.tags });
            vm.selectContinue();
            return;
        }
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
            var stdin_reader = std.Io.File.stdin().reader(io, &stdin_buffer);
            const stdin = &stdin_reader.interface;
            while (stdin.takeDelimiterExclusive('\n')) |_| {
                vm.selectContinue();
                break;
            } else |_| {
                vm.selectContinue();
            }
        }
    }

    pub fn onChoices(runner: *Runner, vm: *Vm, choices: []Choice) void {
        const self: *CliRunner = @fieldParentPtr("runner", runner);
        if (self.next_scripted < self.scripted_choices.len) {
            const scripted = self.scripted_choices[self.next_scripted];
            self.next_scripted += 1;
            if (self.json) {
                emitJson(.{ .type = "choices", .choices = choices, .selected = scripted });
            } else {
                for (choices, 0..) |choice, i| print("[{d}] {s}\n", .{ i, choice.content }) catch {};
                print("> {d}\n", .{scripted}) catch {};
            }
            vm.selectChoice(scripted) catch return self.stop(vm, .invalid_choice);
            return;
        }
        if (self.json) {
            emitJson(.{ .type = "choices", .choices = choices, .selected = null });
            return self.stop(vm, .awaiting_choice);
        }
        var index: ?usize = null;
        while (index == null) {
            var stdin_buffer: [8]u8 = undefined;
            var stdin_reader = std.Io.File.stdin().reader(io, &stdin_buffer);
            const stdin = &stdin_reader.interface;
            for (choices, 0..) |choice, i| {
                print("[{d}] {s}\n", .{ i, choice.content }) catch {};
            }
            if (vm.canRewind()) print("[b] back  ", .{}) catch {};
            if (vm.canRedo()) print("[f] forward  ", .{}) catch {};
            if (vm.canRewind() or vm.canRedo()) print("\n", .{}) catch {};

            while (stdin.takeDelimiterExclusive('\n')) |line| {
                const trimmed = std.mem.trim(u8, line, " \r\n\t");
                if (vm.canRewind() and (std.mem.eql(u8, trimmed, "b") or std.mem.eql(u8, trimmed, "B"))) {
                    vm.rewind() catch |err| {
                        print("Rewind failed: {}\n", .{err}) catch {};
                        break;
                    };
                    return;
                }
                if (vm.canRedo() and (std.mem.eql(u8, trimmed, "f") or std.mem.eql(u8, trimmed, "F"))) {
                    vm.redo() catch |err| {
                        print("Redo failed: {}\n", .{err}) catch {};
                        break;
                    };
                    return;
                }
                index = std.fmt.parseInt(usize, trimmed, 10) catch null;
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
                },
            }
        }
        vm.selectChoice(index.?) catch |err| print("Error: {}", .{err}) catch {};
    }

    pub fn onValueChanged(_: *Runner, _: *Vm, _: []const u8, _: Value) void {}

    fn stop(self: *CliRunner, vm: *Vm, reason: StopReason) void {
        self.stop_reason = reason;
        vm.can_continue = false;
    }
};

pub fn emitJson(value: anytype) void {
    var buffer: [256]u8 = undefined;
    var writer = std.Io.File.stdout().writerStreaming(io, &buffer);
    const out = &writer.interface;
    std.json.Stringify.value(value, .{}, out) catch {};
    out.writeByte('\n') catch {};
    out.flush() catch {};
}

pub const AutoTestRunner = struct {
    runner: Runner,
    rnd: std.Random.Xoshiro256,

    pub fn init(seed: u64) AutoTestRunner {
        return .{
            .rnd = std.Random.DefaultPrng.init(seed),
            .runner = .{
                .on_line = onLine,
                .on_choices = onChoices,
                .on_value_changed = onValueChanged,
            },
        };
    }

    pub fn onLine(_: *Runner, vm: *Vm, _: *const Line) void {
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
