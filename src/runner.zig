const Vm = @import("./vm.zig").Vm;
const UUID = @import("./utils/uuid.zig").UUID;
const ID = UUID.ID;

pub const Line = struct {
    speaker: ?[]const u8,
    content: []const u8,
    tags: [][]const u8,
    id: ID,
};

pub const Choice = struct {
    content: []const u8,
    tags: [][]const u8,
    visit_count: u32,
    ip: u32,
    id: ID,
};

pub const Runner = struct {
    onLineFn: OnLine,
    onChoicesFn: OnChoices,

    pub const OnLine = *const fn (runner: *Runner, vm: *Vm, dialogue: Line) void;
    pub const OnChoices = *const fn (runner: *Runner, vm: *Vm, choices: []Choice) void;

    pub fn onLine(runner: *Runner, vm: *Vm, dialogue: Line) void {
        runner.onLineFn(runner, vm, dialogue);
    }

    pub fn onChoices(runner: *Runner, vm: *Vm, choices: []Choice) void {
        runner.onChoicesFn(runner, vm, choices);
    }
};
