const Vm = @import("./vm.zig").Vm;
const UUID = @import("./utils/uuid.zig").UUID;
const ID = UUID.ID;

pub const Dialogue = struct {
    speaker: ?[]const u8,
    content: []const u8,
    tags: [][]const u8,
    id: ID,
};

pub const Choice = struct {
    content: []const u8,
    count: usize,
    ip: u32,
    id: ID,
};

pub const Runner = struct {
    onDialogueFn: OnDialogue,
    onChoicesFn: OnChoices,

    pub const OnDialogue = *const fn (runner: *Runner, vm: *Vm, dialogue: Dialogue) void;
    pub const OnChoices = *const fn (runner: *Runner, vm: *Vm, choices: []Choice) void;

    pub fn onDialogue(runner: *Runner, vm: *Vm, dialogue: Dialogue) void {
        runner.onDialogueFn(runner, vm, dialogue);
    }

    pub fn onChoices(runner: *Runner, vm: *Vm, choices: []Choice) void {
        runner.onChoicesFn(runner, vm, choices);
    }
};
