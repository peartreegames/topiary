const Vm = @import("vm.zig").Vm;
const Value = @import("../types/index.zig").Value;

pub const Line = struct {
    speaker: ?[]const u8,
    content: []const u8,
    tags: [][]const u8,
};

pub const Choice = struct {
    content: []const u8,
    tags: [][]const u8,
    visit_count: u32,
    ip: u32,
};

pub const Runner = struct {
    on_line: OnLine,
    on_choices: OnChoices,
    on_value_changed: OnValueChanged,

    pub const OnLine = *const fn (runner: *Runner, vm: *Vm, dialogue: Line) void;
    pub const OnChoices = *const fn (runner: *Runner, vm: *Vm, choices: []Choice) void;
    pub const OnValueChanged = *const fn (runner: *Runner, vm: *Vm, name: []const u8, value: Value) void;

    pub fn onLine(runner: *Runner, vm: *Vm, dialogue: Line) void {
        runner.on_line(runner, vm, dialogue);
    }

    pub fn onChoices(runner: *Runner, vm: *Vm, choices: []Choice) void {
        runner.on_choices(runner, vm, choices);
    }

    pub fn onValueChanged(runner: *Runner, vm: *Vm, name: []const u8, value: Value) void {
        runner.on_value_changed(runner, vm, name, value);
    }
};
