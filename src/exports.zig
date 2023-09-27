const std = @import("std");
const Vm = @import("vm.zig").Vm;
const Value = @import("./values.zig").Value;
const Errors = @import("error.zig").Errors;
const compileSource = @import("compiler.zig").compileSource;
const ByteCode = @import("bytecode.zig").ByteCode;
const runners = @import("./runner.zig");
const Runner = runners.Runner;
const Dialogue = runners.Dialogue;
const Choice = runners.Choice;

// const alloc = std.testing.allocator;
const alloc = std.heap.page_allocator;

const ExternDialogue = struct {
    content: [*c]const u8,
    speaker: [*c]const u8,
    tags: [*][*c]const u8,
    tags_length: u8,
};

const ExternChoice = struct {
    content: [*c]const u8,
    count: u8,
    ip: u32,
};

const OnExternDialogue = *const fn (vm_ptr: usize, dialogue: *ExternDialogue) void;
const OnExternChoices = *const fn (vm_ptr: usize, choices: [*]*ExternChoice, choices_len: u8) void;

export fn compile(path_ptr: [*]const u8, length: usize) void {
    var arena = std.heap.ArenaAllocator.init(alloc);
    defer arena.deinit();

    var path = path_ptr[0..length];
    const source_file = std.fs.cwd().openFile(path, .{}) catch @panic("Could not open file for compiler");
    defer source_file.close();

    var output_file_name = std.fmt.allocPrint(arena.allocator(), "{s}b", .{path}) catch @panic("Could not allocate new file name string.");
    const output_file = std.fs.cwd().createFile(output_file_name, .{ .read = true }) catch @panic("Could not create new file.");
    var writer = output_file.writer();
    defer output_file.close();

    var content_alloc = arena.allocator();
    var source = source_file.reader().readAllAlloc(content_alloc, 10_000) catch @panic("Could not read file.");
    defer content_alloc.free(source);

    var errors = Errors.init(arena.allocator());
    defer errors.deinit();

    var bytecode = compileSource(arena.allocator(), source, &errors) catch @panic("Could not compile source");
    bytecode.serialize(writer) catch @panic("Could not serialize bytecode.");
}

export fn run(vm_ptr: usize) void {
    var vm: *Vm = @ptrFromInt(vm_ptr);
    vm.interpret() catch @panic("Unable to run vm");
}

export fn selectContinue(vm_ptr: usize) void {
    var vm: *Vm = @ptrFromInt(vm_ptr);
    vm.selectContinue();
}

export fn selectChoice(vm_ptr: usize, index: usize) void {
    var vm: *Vm = @ptrFromInt(vm_ptr);
    vm.selectChoice(index) catch @panic("Invalid choice");
}

export fn getVariable(vm_ptr: usize, name: [*c]const u8) Value {
    var vm: *Vm = @ptrFromInt(vm_ptr);
    return vm.getExtern(name[0..]);
}

export fn createVm(path_ptr: [*c]const u8, path_len: usize, on_dialogue_ptr: usize, on_choice_ptr: usize) usize {
    const on_dialogue: OnExternDialogue = @ptrFromInt(on_dialogue_ptr);
    const on_choices: OnExternChoices = @ptrFromInt(on_choice_ptr);

    var path = path_ptr[0..path_len];
    const bytecode_file = std.fs.cwd().openFile(path, .{}) catch @panic("Could not open file for vm");
    defer bytecode_file.close();

    var bytecode = ByteCode.deserialize(alloc, bytecode_file.reader()) catch @panic("Could not deserialize bytecode");
    var errors = alloc.create(Errors) catch @panic("Could not allocate errors");
    errors.* = Errors.init(alloc);
    var extern_runner = alloc.create(ExternRunner) catch @panic("Could not allocate runner");
    extern_runner.* = ExternRunner.init(alloc, on_dialogue, on_choices);
    var vm = alloc.create(Vm) catch @panic("Could not allocate vm");
    vm.* = Vm.init(alloc, bytecode, &extern_runner.runner, errors) catch @panic("Could not initialize Vm");
    return @intFromPtr(vm);
}

export fn destroyVm(vm_ptr: usize) void {
    var vm: *Vm = @ptrFromInt(vm_ptr);
    vm.err.deinit();
    alloc.destroy(vm.err);
    vm.bytecode.free(alloc);
    vm.deinit();
    var runner = @fieldParentPtr(ExternRunner, "runner", vm.runner);
    alloc.destroy(runner);
    alloc.destroy(vm);
}

const ExternRunner = struct {
    onExternDialogue: OnExternDialogue,
    onExternChoices: OnExternChoices,
    allocator: std.mem.Allocator,

    runner: Runner,
    tags: [255][*c]const u8,
    dialogue: ExternDialogue = undefined,

    pub fn init(allocator: std.mem.Allocator, on_dialogue: OnExternDialogue, on_choices: OnExternChoices) ExternRunner {
        return .{
            .allocator = allocator,
            .onExternDialogue = on_dialogue,
            .onExternChoices = on_choices,
            .tags = [_][*c]const u8{0} ** 255,
            .runner = .{
                .onDialogueFn = onDialogue,
                .onChoicesFn = onChoices,
            },
        };
    }

    pub fn onDialogue(runner: *Runner, vm: *Vm, dialogue: Dialogue) void {
        var self = @fieldParentPtr(ExternRunner, "runner", runner);

        var i: usize = 0;
        while (i < dialogue.tags.len) : (i += 1) {
            self.tags[i] = dialogue.tags[i].ptr;
        }
        self.dialogue = .{
            .content = dialogue.content.ptr,
            .speaker = if (dialogue.speaker) |s| s.ptr else "",
            .tags = &self.tags,
            .tags_length = @intCast(dialogue.tags.len),
        };
        self.onExternDialogue(@intFromPtr(vm), &self.dialogue);
    }

    pub fn onChoices(runner: *Runner, vm: *Vm, choices: []Choice) void {
        var self = @fieldParentPtr(ExternRunner, "runner", runner);
        var i: usize = 0;
        var result = self.allocator.alloc(*ExternChoice, choices.len) catch @panic("Could not allocate choices");
        while (i < choices.len) : (i += 1) {
            var choice = self.allocator.create(ExternChoice) catch @panic("Could not create Choice");
            choice.* = .{
                .content = choices[i].content.ptr,
                .count = @intCast(choices[i].count),
                .ip = choices[i].ip,
            };
            result[i] = choice;
        }
        self.onExternChoices(@intFromPtr(vm), result.ptr, @intCast(result.len));
        for (result) |choice| self.allocator.destroy(choice);
        self.allocator.free(result);
    }
};

const TestRunner = struct {
    pub fn onDialogue(vm_ptr: usize, dialogue: *ExternDialogue) void {
        std.debug.print("{s}: {s}\n", .{
            dialogue.speaker,
            dialogue.content,
        });
        selectContinue(vm_ptr);
    }

    pub fn onChoices(vm_ptr: usize, choices: [*]*ExternChoice, choices_len: u8) void {
        for (choices, 0..choices_len) |choice, i| {
            std.debug.print("[{d}] {s}\n", .{ i, choice.content });
        }
        selectChoice(vm_ptr, 0);
    }
};

test "Create and Destroy Vm" {
    const text =
        \\ === START {
        \\     :: "A person approaches."
        \\     :Stranger: "Hey there."
        \\     fork^ {
        \\         ~ "Greet them." {
        \\             :Drew: "Oh, uh, nice to meet you. My name is Drew."
        \\             :Drew: "Sorry, I thought you were someone I knew."
        \\             :Drew: "I'd love to stay and chat, but this is just a short demo."
        \\         }
        \\         ~ "Say nothing." {
        \\             :: "The person acts as though they were addressing someone else."
        \\         }
        \\     }
        \\     :: "They walk away..."
        \\ }
        \\
        \\ => START
    ;

    const path = "./test.topi";
    const file = try std.fs.cwd().createFile(path, .{});
    defer file.close();

    try file.writeAll(text);
    compile(path, path.len);
    var compiled_path = "./test.topib";
    const on_dialogue: OnExternDialogue = TestRunner.onDialogue;
    const on_choices: OnExternChoices = TestRunner.onChoices;
    var vm_ptr = createVm(compiled_path, compiled_path.len, @intFromPtr(on_dialogue), @intFromPtr(on_choices));

    run(vm_ptr);
    destroyVm(vm_ptr);
    try std.fs.cwd().deleteFile(path);
    try std.fs.cwd().deleteFile(compiled_path);
}
