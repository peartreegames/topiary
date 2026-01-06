const std = @import("std");

const topi = @import("topi");
const Vm = topi.runtime.Vm;
const Runner = topi.runtime.Runner;
const Line = topi.runtime.Line;
const Choice = topi.runtime.Choice;

const Value = topi.types.Value;

pub const TestRunner = struct {
    runner: Runner,
    allocator: std.mem.Allocator,
    output: std.ArrayList([]const u8) = .empty,
    choices_to_make: std.ArrayList(usize) = .empty,
    choice_index: usize = 0,
    values_changed: std.ArrayList(Value) = .empty,

    pub fn init(allocator: std.mem.Allocator) !*TestRunner {
        const test_runner = try allocator.create(TestRunner);
        test_runner.* = .{
            .allocator = allocator,
            .runner = .{
                .on_line = TestRunner.onLine,
                .on_choices = TestRunner.onChoices,
                .on_value_changed = TestRunner.onValueChanged,
            },
        };
        return test_runner;
    }

    pub fn deinit(self: *TestRunner) void {
        self.clear();
        self.output.deinit(self.allocator);
        self.choices_to_make.deinit(self.allocator);
        self.allocator.destroy(self);
        self.values_changed.deinit(self.allocator);
    }

    pub fn clear(self: *TestRunner) void {
        for (self.output.items) |item| {
            self.allocator.free(item);
        }
        self.output.clearRetainingCapacity();
    }

    pub fn expectOutput(self: *TestRunner, comptime expected: []const []const u8) !void {
        try std.testing.expectEqual(expected.len, self.output.items.len);
        for (expected, 0..) |expected_line, i| {
            try std.testing.expectEqualStrings(std.mem.trim(u8, expected_line, " \n\r\t"), self.output.items[i]);
        }
    }

    pub fn onLine(_: *Runner, vm: *Vm, dialogue: Line) void {
        const test_runner: *TestRunner = @fieldParentPtr("runner", vm.runner);
        const content = test_runner.allocator.dupe(u8, dialogue.content) catch unreachable;
        test_runner.output.append(test_runner.allocator, content) catch unreachable;
        vm.selectContinue();
    }

    pub fn onChoices(_: *Runner, vm: *Vm, choices: []Choice) void {
        const self: *TestRunner = @fieldParentPtr( "runner", vm.runner);

        var selection = if (self.choice_index < self.choices_to_make.items.len) blk: {
            const idx = self.choices_to_make.items[self.choice_index];
            self.choice_index += 1;
            break :blk idx;
        } else 0;
        if (selection >= choices.len) selection = 0;

        vm.selectChoice(selection) catch |err| {
            std.debug.print("Error selecting choice: {}\n", .{err});
        };
    }

    pub fn onValueChanged(_: *Runner, vm: *Vm, _: []const u8, value: Value) void {
        const test_runner: *TestRunner = @fieldParentPtr("runner", vm.runner);
        test_runner.values_changed.append(test_runner.allocator, value) catch unreachable;
    }
};
