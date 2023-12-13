const std = @import("std");
const Vm = @import("./vm.zig").Vm;
const values = @import("./values.zig");
const Value = @import("./values.zig").Value;
const ExportValue = @import("./export-value.zig").ExportValue;
const Errors = @import("./compiler-error.zig").CompilerErrors;
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
    visit_count: u32,
    ip: u32,
};

const OnExportValueChanged = *const fn (value: *ExportValue) void;
const OnExportDialogue = *const fn (vm_ptr: usize, dialogue: *ExportDialogue) void;
const OnExportChoices = *const fn (vm_ptr: usize, choices: [*]ExportChoice, choices_len: u8) void;
const OutputLogFn = *const fn (msg: [*c]const u8) void;
var output_log: ?OutputLogFn = null;

const Severity = enum(u8) {
    info,
    warn,
    err,
};

var severity_log: Severity = .err;

fn log(msg: [*c]const u8, severity: Severity) void {
    if (@intFromEnum(severity) < @intFromEnum(severity_log)) return;
    if (output_log) |l| l(msg);
}

export fn setLogger(logger_ptr: usize, severity_value: u8) void {
    severity_log = @enumFromInt(severity_value);
    output_log = @ptrFromInt(logger_ptr);
}

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
    const writer = fbs.writer();
    bytecode.serialize(writer) catch @panic("Could not serialize bytecode.");
}

export fn start(vm_ptr: usize) void {
    log("Starting VM", .info);
    var vm: *Vm = @ptrFromInt(vm_ptr);
    vm.start() catch log("Could not start vm");
}

export fn run(vm_ptr: usize, error_line: *c_int, error_message: [*c]u8, capacity: usize) void {
    log("Running VM", .info);
    var vm: *Vm = @ptrFromInt(vm_ptr);
    vm.run() catch {
        error_line.* = @intCast(vm.err.line);
        if (vm.err.msg) |msg| {
            const len = @min(msg.len, capacity - 1);
            std.mem.copy(u8, error_message[0..len], msg[0..len]);
            if (len < capacity) error_message[len] = 0;
        }
    };
}

export fn canContinue(vm_ptr: usize) bool {
    const vm: *Vm = @ptrFromInt(vm_ptr);
    return vm.can_continue;
}

export fn isWaiting(vm_ptr: usize) bool {
    const vm: *Vm = @ptrFromInt(vm_ptr);
    return vm.is_waiting;
}

export fn selectContinue(vm_ptr: usize) void {
    log("Selecting continue", .info);
    var vm: *Vm = @ptrFromInt(vm_ptr);
    vm.selectContinue();
}

export fn selectChoice(vm_ptr: usize, index: usize) void {
    log("Selecting choice", .info);
    var vm: *Vm = @ptrFromInt(vm_ptr);
    vm.selectChoice(index) catch log("Invalid choice", .err);
}

export fn tryGetValue(vm_ptr: usize, name_ptr: [*c]const u8, name_length: usize, out: *ExportValue) callconv(.C) bool {
    var vm: *Vm = @ptrFromInt(vm_ptr);

    const index = vm.getGlobalsIndex(name_ptr[0..name_length]) catch {
        out.* = ExportValue.Nil;
        return false;
    };
    out.* = ExportValue.fromValue(vm.globals[index], alloc);
    return true;
}

export fn setExternString(vm_ptr: usize, name_ptr: [*c]const u8, name_length: usize, value_ptr: [*c]const u8, value_length: usize) void {
    var vm: *Vm = @ptrFromInt(vm_ptr);
    const name = name_ptr[0..name_length];
    const value = value_ptr[0..value_length];
    const str = vm.gc.create(vm, .{ .string = vm.allocator.dupe(u8, value) catch @panic("Cannot allocate memory for string.") }) catch @panic("Unable to create gc value");
    vm.setExtern(name, str) catch @panic("Invalid operation");
}

export fn setExternNumber(vm_ptr: usize, name_ptr: [*c]const u8, name_length: usize, value: f32) void {
    var vm: *Vm = @ptrFromInt(vm_ptr);
    const name = name_ptr[0..name_length];
    vm.setExtern(name, .{ .number = value }) catch @panic("Invalid operation");
}

export fn setExternBool(vm_ptr: usize, name_ptr: [*c]const u8, name_length: usize, value: bool) void {
    var vm: *Vm = @ptrFromInt(vm_ptr);
    const name = name_ptr[0..name_length];
    vm.setExtern(name, if (value) values.True else values.False) catch @panic("Invalid operation");
}

export fn setExternNil(vm_ptr: usize, name_ptr: [*c]const u8, name_length: usize) void {
    var vm: *Vm = @ptrFromInt(vm_ptr);
    const name = name_ptr[0..name_length];
    vm.setExtern(name, values.Nil) catch @panic("Invalid operation");
}

export fn setExternFunc(vm_ptr: usize, name_ptr: [*]const u8, name_length: usize, value_ptr: usize, arity: u8) void {
    var vm: *Vm = @ptrFromInt(vm_ptr);
    const name = name_ptr[0..name_length];
    const wrapper = alloc.create(ExportFunction) catch @panic("Could not allocate ExportFunction");
    wrapper.* = ExportFunction.init(@as(ExportFunction.Delegate, @ptrFromInt(value_ptr)));
    const val = vm.gc.create(vm, .{ .ext_function = .{ .arity = arity, .backing = ExportFunction.call, .context_ptr = @intFromPtr(wrapper) } }) catch @panic("Could not create value");
    vm.setExtern(name, val) catch @panic("Invalid operation");
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
        const exp = alloc.create(ExportValue) catch {
            log("Could not allocate ExportValue", .err);
            return;
        };
        exp.* = ExportValue.fromValue(value, alloc);
        self.callback(exp);
        alloc.destroy(exp);
    }

    pub fn onUnsubscribe(context_ptr: usize, allocator: std.mem.Allocator) void {
        const self: *ExportCallback = @ptrFromInt(context_ptr);
        allocator.destroy(self);
    }
};

const ExportFunction = struct {
    func: Delegate,

    pub const Delegate = *const fn (args: [*c]ExportValue, args_len: u8) ExportValue;

    pub fn init(func: Delegate) ExportFunction {
        return .{ .func = func };
    }

    pub fn call(context_ptr: usize, args: []Value) Value {
        var self: *ExportFunction = @ptrFromInt(context_ptr);
        var exp_args = alloc.alloc(ExportValue, args.len) catch {
            log("Could not allocate args", .err);
            return values.Nil;
        };
        var i: usize = 0;
        while (i < args.len) : (i += 1) {
            exp_args[i] = ExportValue.fromValue(args[i], alloc);
        }
        var v = self.func(exp_args.ptr, @intCast(exp_args.len));
        return v.toValue();
    }
};

export fn subscribe(vm_ptr: usize, name_ptr: [*c]const u8, name_length: usize, callback_ptr: usize) void {
    var vm: *Vm = @ptrFromInt(vm_ptr);

    const name = name_ptr[0..name_length];
    const export_callback = vm.allocator.create(ExportCallback) catch @panic("Could not allocate ExportCallback");
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

    log("Creating VM", .info);
    var fbs = std.io.fixedBufferStream(source_ptr[0..source_len]);
    const bytecode = ByteCode.deserialize(alloc, fbs.reader()) catch {
        log("Could not deserialize bytecode", .err);
        return 0;
    };

    var extern_runner = alloc.create(ExportRunner) catch {
        log("Could not allocate runner", .err);
        return 0;
    };
    extern_runner.* = ExportRunner.init(alloc, on_dialogue, on_choices);

    const vm = alloc.create(Vm) catch {
        log("Could not allocate vm", .err);
        return 0;
    };
    vm.* = Vm.init(alloc, bytecode, &extern_runner.runner) catch {
        log("Could not initialize Vm", .err);
        return 0;
    };
    return @intFromPtr(vm);
}

export fn destroyVm(vm_ptr: usize) void {
    log("Destroying VM", .info);
    var vm: *Vm = @ptrFromInt(vm_ptr);
    const runner = @fieldParentPtr(ExportRunner, "runner", vm.runner);
    alloc.destroy(runner);
    vm.deinit();
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
        var result = self.allocator.alloc(ExportChoice, choices.len) catch {
            log("Could not allocate choices", .err);
            return;
        };
        while (i < choices.len) : (i += 1) {
            result[i] = .{
                .content = choices[i].content.ptr,
                .visit_count = @intCast(choices[i].visit_count),
                .ip = choices[i].ip,
            };
        }
        self.onExportChoices(@intFromPtr(vm), result.ptr, @intCast(result.len));
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

    pub fn onChoices(vm_ptr: usize, choices: [*]ExportChoice, choices_len: u8) void {
        for (choices, 0..choices_len) |choice, i| {
            std.debug.print("[{d}] {s}\n", .{ i, choice.content });
        }
        selectChoice(vm_ptr, 0);
    }

    pub fn log(msg: [*c]const u8) void {
        std.debug.print("[debug] {s}\n", .{msg});
    }
};

fn testSubscriber(value: ExportValue) void {
    std.debug.print("ExportSubscriber: {s}\n", .{value.data.string});
}

test "Create and Destroy Vm" {
    const text =
        \\ var value = "test 123"
        \\ var list = [1,2,3,4]
        \\ var set = {"some", "string", "values"}
        \\ var map = {0: 0.0001, 1: 1.1111, 2: 2.222 }
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

    output_log = TestRunner.log;
    var buf: [4096]u8 = undefined;
    compile(text.ptr, text.len, &buf, buf.len);
    const on_dialogue: OnExportDialogue = TestRunner.onDialogue;
    const on_choices: OnExportChoices = TestRunner.onChoices;
    const vm_ptr = createVm(&buf, buf.len, @intFromPtr(on_dialogue), @intFromPtr(on_choices));

    const val_name = "value";
    subscribe(
        vm_ptr,
        val_name,
        val_name.len,
        @intFromPtr(&testSubscriber),
    );
    var err_line: c_int = 0;
    var err_msg: [2048]u8 = undefined;
    start(vm_ptr);
    while (canContinue(vm_ptr)) {
        run(vm_ptr, &err_line, &err_msg, 2048);
        if (err_line != 0) {
            std.log.warn("Error line {}: {s}", .{ err_line, err_msg });
            break;
        }
    }
    unsubscribe(
        vm_ptr,
        val_name,
        val_name.len,
        @intFromPtr(&testSubscriber),
    );

    const list_name = "list";
    var list_value: ExportValue = undefined;
    if (tryGetValue(
        vm_ptr,
        list_name,
        list_name.len,
        &list_value,
    )) {
        std.debug.print("List: {}\n", .{list_value.data.list});
        for (list_value.data.list.items[0..list_value.data.list.count]) |item| {
            std.debug.print("List Item: {d}\n", .{item.data.number});
        }
        destroyValue(&list_value);
    }

    const set_name = "set";
    var set_value: ExportValue = undefined;
    if (tryGetValue(
        vm_ptr,
        set_name,
        set_name.len,
        &set_value,
    )) {
        for (set_value.data.list.items[0..set_value.data.list.count]) |item| {
            std.debug.print("Set Item: {s}\n", .{item.data.string});
        }
        destroyValue(&set_value);
    }

    const map_name = "map";
    var map_value: ExportValue = undefined;
    if (tryGetValue(
        vm_ptr,
        map_name,
        map_name.len,
        &map_value,
    )) {
        for (map_value.data.list.items[0 .. map_value.data.list.count * 2]) |item| {
            std.debug.print("Map Item: {d}\n", .{item.data.number});
        }
        destroyValue(&map_value);
    }
    destroyVm(vm_ptr);
}
