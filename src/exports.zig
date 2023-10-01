const std = @import("std");
const Vm = @import("./vm.zig").Vm;
const values = @import("./values.zig");
const Value = @import("./values.zig").Value;
const ExportValue = @import("./export-value.zig").ExportValue;
const Errors = @import("./error.zig").Errors;
const compileSource = @import("./compiler.zig").compileSource;
const ByteCode = @import("./bytecode.zig").ByteCode;
const runners = @import("./runner.zig");
const Subscriber = @import("./subscriber.zig").Subscriber;
const Runner = runners.Runner;
const Dialogue = runners.Dialogue;
const Choice = runners.Choice;

// const alloc = std.testing.allocator;
const alloc = std.heap.page_allocator;

const ExportDialogue = extern struct {
    content: [*c]const u8,
    speaker: [*c]const u8,
    tags: [*][*c]const u8,
    tags_length: u8,
};

const ExportChoice = extern struct {
    content: [*c]const u8,
    count: u8,
    ip: u32,
};

const OnExportValueChanged = *const fn (value: *ExportValue) void;
const OnExportDialogue = *const fn (vm_ptr: usize, dialogue: *ExportDialogue) void;
const OnExportChoices = *const fn (vm_ptr: usize, choices: [*]*ExportChoice, choices_len: u8) void;

export fn compile(source_ptr: [*c]const u8, length: usize, out_ptr: [*c]u8, max: usize) void {
    var arena = std.heap.ArenaAllocator.init(alloc);
    defer arena.deinit();

    var errors = Errors.init(arena.allocator());
    defer errors.deinit();

    var bytecode = compileSource(
        arena.allocator(),
        source_ptr[0..length],
        &errors,
    ) catch @panic("Could not compile source");

    var fbs = std.io.fixedBufferStream(out_ptr[0..max]);
    var writer = fbs.writer();
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

export fn tryGetValue(vm_ptr: usize, name_ptr: [*c]const u8, name_length: usize, out: *ExportValue) callconv(.C) bool {
    var vm: *Vm = @ptrFromInt(vm_ptr);

    var index = vm.getGlobalsIndex(name_ptr[0..name_length]) catch {
        out.* = ExportValue.Nil;
        return false;
    };
    out.* = ExportValue.fromValue(vm.globals[index], alloc);
    return true;
}

export fn destroyValue(value: *ExportValue) void {
    value.deinit(alloc);
}

const ExportCallback = struct {
    callback: OnExportValueChanged,

    pub fn init(callback: OnExportValueChanged) ExportCallback {
        return .{
            .callback = callback,
        };
    }

    pub fn onValueChanged(context_ptr: usize, value: Value) void {
        var self: *ExportCallback = @ptrFromInt(context_ptr);
        var exp = alloc.create(ExportValue) catch @panic("Could not allocate ExportValue");
        exp.* = ExportValue.fromValue(value, alloc);
        self.callback(exp);
        alloc.destroy(exp);
    }

    pub fn onUnsubscribe(context_ptr: usize, allocator: std.mem.Allocator) void {
        var self: *ExportCallback = @ptrFromInt(context_ptr);
        allocator.destroy(self);
    }
};

export fn subscribe(vm_ptr: usize, name_ptr: [*c]const u8, name_length: usize, callback_ptr: usize) void {
    var vm: *Vm = @ptrFromInt(vm_ptr);

    const name = name_ptr[0..name_length];
    var export_callback = vm.allocator.create(ExportCallback) catch @panic("Could not allocate ExportCallback");
    export_callback.* = ExportCallback.init(@ptrFromInt(callback_ptr));

    vm.subscribeDelegate(
        name,
        .{
            .onUnsubscribe = &ExportCallback.onUnsubscribe,
            .context_ptr = @intFromPtr(export_callback),
            .callback = &ExportCallback.onValueChanged,
        },
    ) catch @panic("Could not subscribe to variable");
}

export fn unsubscribe(vm_ptr: usize, name_ptr: [*c]const u8, name_length: usize, callback_ptr: usize) void {
    var vm: *Vm = @ptrFromInt(vm_ptr);

    const name = name_ptr[0..name_length];
    var callback: OnExportValueChanged = @ptrFromInt(callback_ptr);
    const export_callback = @fieldParentPtr(ExportCallback, "callback", &callback);

    vm.unsubscribeDelegate(name, .{
        .context_ptr = @intFromPtr(export_callback),
        .callback = &ExportCallback.onValueChanged,
    }) catch @panic("Could not unsubscribe from variable");
}

export fn createVm(source_ptr: [*c]const u8, source_len: usize, on_dialogue_ptr: usize, on_choice_ptr: usize) usize {
    const on_dialogue: OnExportDialogue = @ptrFromInt(on_dialogue_ptr);
    const on_choices: OnExportChoices = @ptrFromInt(on_choice_ptr);

    var fbs = std.io.fixedBufferStream(source_ptr[0..source_len]);
    var bytecode = ByteCode.deserialize(alloc, fbs.reader()) catch @panic("Could not deserialize bytecode");
    var errors = alloc.create(Errors) catch @panic("Could not allocate errors");
    errors.* = Errors.init(alloc);

    var extern_runner = alloc.create(ExportRunner) catch @panic("Could not allocate runner");
    extern_runner.* = ExportRunner.init(alloc, on_dialogue, on_choices);

    var vm = alloc.create(Vm) catch @panic("Could not allocate vm");
    vm.* = Vm.init(alloc, bytecode, &extern_runner.runner, errors) catch @panic("Could not initialize Vm");

    return @intFromPtr(vm);
}

export fn destroyVm(vm_ptr: usize) void {
    var vm: *Vm = @ptrFromInt(vm_ptr);
    vm.err.deinit();
    alloc.destroy(vm.err);
    vm.deinit();
    var runner = @fieldParentPtr(ExportRunner, "runner", vm.runner);
    alloc.destroy(runner);
    alloc.destroy(vm);
}

const ExportRunner = struct {
    onExportDialogue: OnExportDialogue,
    onExportChoices: OnExportChoices,
    allocator: std.mem.Allocator,

    runner: Runner,
    tags: [255][*c]const u8,
    dialogue: ExportDialogue = undefined,

    pub fn init(allocator: std.mem.Allocator, on_dialogue: OnExportDialogue, on_choices: OnExportChoices) ExportRunner {
        return .{
            .allocator = allocator,
            .onExportDialogue = on_dialogue,
            .onExportChoices = on_choices,
            .tags = [_][*c]const u8{0} ** 255,
            .runner = .{
                .onDialogueFn = onDialogue,
                .onChoicesFn = onChoices,
            },
        };
    }

    pub fn onDialogue(runner: *Runner, vm: *Vm, dialogue: Dialogue) void {
        var self = @fieldParentPtr(ExportRunner, "runner", runner);

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
        self.onExportDialogue(@intFromPtr(vm), &self.dialogue);
    }

    pub fn onChoices(runner: *Runner, vm: *Vm, choices: []Choice) void {
        var self = @fieldParentPtr(ExportRunner, "runner", runner);
        var i: usize = 0;
        var result = self.allocator.alloc(*ExportChoice, choices.len) catch @panic("Could not allocate choices");
        while (i < choices.len) : (i += 1) {
            var choice = self.allocator.create(ExportChoice) catch @panic("Could not create Choice");
            choice.* = .{
                .content = choices[i].content.ptr,
                .count = @intCast(choices[i].count),
                .ip = choices[i].ip,
            };
            result[i] = choice;
        }
        self.onExportChoices(@intFromPtr(vm), result.ptr, @intCast(result.len));
        for (result) |choice| self.allocator.destroy(choice);
        self.allocator.free(result);
    }
};

const TestRunner = struct {
    pub fn onDialogue(vm_ptr: usize, dialogue: *ExportDialogue) void {
        std.debug.print("{s}: {s}\n", .{
            dialogue.speaker,
            dialogue.content,
        });
        selectContinue(vm_ptr);
    }

    pub fn onChoices(vm_ptr: usize, choices: [*]*ExportChoice, choices_len: u8) void {
        for (choices, 0..choices_len) |choice, i| {
            std.debug.print("[{d}] {s}\n", .{ i, choice.content });
        }
        selectChoice(vm_ptr, 0);
    }
};

fn testSubscriber(value: ExportValue) void {
    std.debug.print("ExportSubscriber: {s}\n", .{value.data.string});
}

test "Create and Destroy Vm" {
    const text =
        \\ var value = "test 123"
        \\ var list = [1,2,3,4]
        \\ === START {
        \\     :: "A person approaches."
        \\     :Stranger: "Hey there."
        \\     value = "321 test"
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

    var buf: [4096]u8 = undefined;
    compile(text.ptr, text.len, &buf, buf.len);
    const on_dialogue: OnExportDialogue = TestRunner.onDialogue;
    const on_choices: OnExportChoices = TestRunner.onChoices;
    var vm_ptr = createVm(&buf, buf.len, @intFromPtr(on_dialogue), @intFromPtr(on_choices));

    var val_name = "value";
    subscribe(
        vm_ptr,
        val_name,
        val_name.len,
        @intFromPtr(&testSubscriber),
    );
    run(vm_ptr);
    unsubscribe(
        vm_ptr,
        val_name,
        val_name.len,
        @intFromPtr(&testSubscriber),
    );

    var list_name = "list";
    var out: ExportValue = undefined;
    if (tryGetValue(
        vm_ptr,
        list_name,
        list_name.len,
        &out,
    )) {
       std.debug.print("GETVARIALBE:: {any}\n", .{out.data.list.items[0..out.data.list.count]});
       destroyValue(&out);
    }
    destroyVm(vm_ptr);
}
