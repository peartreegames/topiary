const std = @import("std");
const Vm = @import("./vm.zig").Vm;
const values = @import("./values.zig");
const Value = @import("./values.zig").Value;
const ExportValue = @import("./export-value.zig").ExportValue;
const Errors = @import("./compiler-error.zig").CompilerErrors;
const Compiler = @import("./compiler.zig").Compiler;
const compileSource = @import("compiler.test.zig").compileSource;
const Bytecode = @import("./bytecode.zig").Bytecode;
const runners = @import("./runner.zig");
const Subscriber = @import("./subscriber.zig").Subscriber;
const Module = @import("module.zig").Module;
const File = @import("module.zig").File;
const Runner = runners.Runner;
const Dialogue = runners.Dialogue;
const Choice = runners.Choice;

const alloc = std.testing.allocator;
// var alloc = std.heap.page_allocator;
var debug_log: ?OnExportDebugLog = null;
var debug_severity: Severity = .err;

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

const OnExportValueChanged = *const fn (value: ExportValue) void;
const OnExportDialogue = *const fn (vm_ptr: usize, dialogue: *ExportDialogue) void;
const OnExportChoices = *const fn (vm_ptr: usize, choices: [*]ExportChoice, choices_len: u8) void;

const OnExportDebugLog = *const fn (msg: [*c]const u8, severity: Severity) void;
const Severity = enum(u8) {
    debug,
    info,
    warn,
    err,
};

fn log(comptime msg: []const u8, args: anytype, severity: Severity) void {
    if (@intFromEnum(severity) < @intFromEnum(debug_severity)) return;
    if (debug_log) |l| {
        var buf: [2048]u8 = undefined;
        const fmt = std.fmt.bufPrintZ(&buf, msg, args) catch msg;
        l(fmt.ptr, severity);
    }
}

export fn setDebugLog(logger_ptr: usize) void {
    debug_log = @ptrFromInt(logger_ptr);
}

export fn setDebugSeverity(severity: u8) void {
    debug_severity = @enumFromInt(severity);
}

export fn compile(path_ptr: [*c]const u8, path_length: usize, out_ptr: [*c]u8, max: usize) void {
    var arena = std.heap.ArenaAllocator.init(alloc);
    defer arena.deinit();
    var comp_alloc = arena.allocator();

    const file_path = path_ptr[0..path_length];
    const full_path = std.fs.cwd().realpathAlloc(comp_alloc, file_path) catch |err| {
        log("Could not get full path for '{s}': {s}", .{ file_path, @errorName(err) }, .err);
        return;
    };
    var mod = Module{
        .allocator = comp_alloc,
        .entry = undefined,
        .includes = std.StringArrayHashMap(*File).init(comp_alloc),
    };
    const file = comp_alloc.create(File) catch |err| {
        log("Could not allocate Module File: {s}", .{@errorName(err)}, .err);
        return;
    };
    file.* = File.create(full_path, &mod) catch |err| {
        log("Could not create Module File: {s}", .{@errorName(err)}, .err);
        return;
    };
    mod.entry = file;
    mod.includes.putNoClobber(file.path, file) catch unreachable;
    defer mod.deinit();
    file.loadSource(comp_alloc) catch |err| {
        log("Could not load file source: {s}", .{@errorName(err)}, .err);
        return;
    };

    var buf: [4096]u8 = undefined;
    var err_fbs = std.io.fixedBufferStream(&buf);

    file.buildTree(comp_alloc) catch |err| {
        mod.writeErrors(err_fbs.writer()) catch |e| {
            log("Could not write errors to log message. Something is very wrong. {s}", .{@errorName(e)}, .err);
            return;
        };
        log("Could not parse file '{s}': {s}\n{s}", .{ file_path, @errorName(err), buf[0..] }, .err);
        return;
    };

    var compiler = Compiler.init(comp_alloc) catch |err| {
        log("Could not create compiler: {s}", .{@errorName(err)}, .err);
        return;
    };
    defer compiler.deinit();

    compiler.compile(&mod) catch |err| {
        mod.writeErrors(err_fbs.writer()) catch |e| {
            log("Could not write errors to log message. Something is very wrong. {s}", .{@errorName(e)}, .err);
            return;
        };
        log("Could not compile file '{s}': {s}\n{s}", .{ file_path, @errorName(err), buf[0..] }, .err);
        return;
    };
    var bytecode = compiler.bytecode() catch |err| {
        log("Could not create bytecode: {s}", .{@errorName(err)}, .err);
        return;
    };

    var fbs = std.io.fixedBufferStream(out_ptr[0..max]);
    const writer = fbs.writer();
    bytecode.serialize(writer) catch |err| log("Could not serialize bytecode: {s}", .{@errorName(err)}, .err);
}

export fn start(vm_ptr: usize) void {
    log("Starting VM", .{}, .info);
    var vm: *Vm = @ptrFromInt(vm_ptr);
    for (vm.bytecode.global_symbols) |sym| {
        if (sym.is_extern and vm.globals[sym.index] == .void) {
            log("Export \"{s}\" has not been set", .{sym.name}, .warn);
        }
    }
    vm.start() catch |err| log("Could not start vm: {any}", .{err}, .err);
}

export fn run(vm_ptr: usize) void {
    log("Running VM", .{}, .info);
    var vm: *Vm = @ptrFromInt(vm_ptr);
    vm.run() catch {
        if (vm.err.msg) |msg| {
            log("Error Line {d}: {s}", .{ vm.err.line, msg }, .err);
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
    log("Selecting continue", .{}, .info);
    var vm: *Vm = @ptrFromInt(vm_ptr);
    vm.selectContinue();
}

export fn selectChoice(vm_ptr: usize, index: usize) void {
    log("Selecting choice", .{}, .info);
    var vm: *Vm = @ptrFromInt(vm_ptr);
    vm.selectChoice(index) catch log("Invalid choice", .{}, .err);
}

export fn tryGetValue(vm_ptr: usize, name_ptr: [*c]const u8, name_length: usize, out: *ExportValue) bool {
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
    const dupe = vm.allocator.dupe(u8, value) catch |err| {
        log("Could not allocate memory for string \"{s}\": {s}", .{ name, @errorName(err) }, .err);
        return;
    };
    const str = vm.gc.create(vm, .{ .string = dupe }) catch |err| {
        log("Could not allocate GC value \"{s}\": {s}", .{ name, @errorName(err) }, .err);
        return;
    };
    vm.setExtern(name, str) catch |err| {
        log("Could not set Export value \"{s}\": {s}", .{ name, @errorName(err) }, .err);
    };
}

export fn setExternNumber(vm_ptr: usize, name_ptr: [*c]const u8, name_length: usize, value: f32) void {
    var vm: *Vm = @ptrFromInt(vm_ptr);
    const name = name_ptr[0..name_length];
    vm.setExtern(name, .{ .number = value }) catch |err| {
        log("Could not set Export value \"{s}\": {s}", .{ name, @errorName(err) }, .err);
    };
}

export fn setExternBool(vm_ptr: usize, name_ptr: [*c]const u8, name_length: usize, value: bool) void {
    var vm: *Vm = @ptrFromInt(vm_ptr);
    const name = name_ptr[0..name_length];
    vm.setExtern(name, if (value) values.True else values.False) catch |err| {
        log("Could not set Export value \"{s}\": {s}", .{ name, @errorName(err) }, .err);
    };
}

export fn setExternNil(vm_ptr: usize, name_ptr: [*c]const u8, name_length: usize) void {
    var vm: *Vm = @ptrFromInt(vm_ptr);
    const name = name_ptr[0..name_length];
    vm.setExtern(name, values.Nil) catch |err| {
        log("Could not set Export value \"{s}\": {s}", .{ name, @errorName(err) }, .err);
    };
}

export fn setExternFunc(vm_ptr: usize, name_ptr: [*c]const u8, name_length: usize, value_ptr: usize, arity: u8) void {
    var vm: *Vm = @ptrFromInt(vm_ptr);
    const name = name_ptr[0..name_length];
    log("Setting extern function \"{s}\"", .{name}, .info);
    const wrapper = alloc.create(ExportFunction) catch |err| {
        log("Could not allocate ExportFunction '{s}': {s}", .{ name, @errorName(err) }, .err);
        return;
    };
    wrapper.* = ExportFunction.create(@as(ExportFunction.Delegate, @ptrFromInt(value_ptr)));
    const val = vm.gc.create(vm, .{ .ext_function = .{ .arity = arity, .backing = ExportFunction.call, .context_ptr = @intFromPtr(wrapper) } }) catch |err| {
        log("Could not create function value '{s}': {s}", .{ name, @errorName(err) }, .err);
        return;
    };
    vm.setExtern(name, val) catch |err| {
        log("Could not set Export value '{s}': {s}", .{ name, @errorName(err) }, .err);
    };
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
        const ext = ExportValue.fromValue(value, alloc);
        self.callback(ext);
    }

    pub fn onUnsubscribe(context_ptr: usize, allocator: std.mem.Allocator) void {
        const self: *ExportCallback = @ptrFromInt(context_ptr);
        allocator.destroy(self);
    }
};

pub const ExportFunction = struct {
    func: Delegate,

    pub const Delegate = *const fn (args: [*c]ExportValue, args_len: u8) callconv(.C) ExportValue;

    pub fn create(func: Delegate) ExportFunction {
        return .{ .func = func };
    }

    pub fn call(context_ptr: usize, args: []Value) Value {
        var self: *ExportFunction = @ptrFromInt(context_ptr);
        var arena = std.heap.ArenaAllocator.init(alloc);
        const arenaAlloc = arena.allocator();
        defer arena.deinit();
        var exp_args = arenaAlloc.alloc(ExportValue, args.len) catch |err| {
            log("Could not allocate args: {s}", .{@errorName(err)}, .err);
            return values.Nil;
        };
        var i: usize = 0;
        while (i < args.len) : (i += 1) {
            exp_args[i] = ExportValue.fromValue(args[i], alloc);
        }
        log("Calling ExportFunction", .{}, .info);
        var v = self.func(exp_args.ptr, @intCast(exp_args.len));
        return v.toValue();
    }
};

export fn subscribe(vm_ptr: usize, name_ptr: [*c]const u8, name_length: usize, callback_ptr: usize) void {
    var vm: *Vm = @ptrFromInt(vm_ptr);

    const name = name_ptr[0..name_length];
    const extern_callback = vm.allocator.create(ExportCallback) catch {
        log("Could not allocate ExportCallback", .{}, .err);
        return;
    };
    extern_callback.* = ExportCallback.init(@ptrFromInt(callback_ptr));

    vm.subscribeDelegate(
        name,
        .{
            .onUnsubscribe = &ExportCallback.onUnsubscribe,
            .context_ptr = @intFromPtr(extern_callback),
            .callback = &ExportCallback.onValueChanged,
        },
    ) catch |err| {
        log("Could not subscribe to variable '{s}': {s}", .{ name, @errorName(err) }, .warn);
        return;
    };
}

export fn unsubscribe(vm_ptr: usize, name_ptr: [*c]const u8, name_length: usize, callback_ptr: usize) void {
    var vm: *Vm = @ptrFromInt(vm_ptr);

    const name = name_ptr[0..name_length];
    var callback: OnExportValueChanged = @ptrFromInt(callback_ptr);
    const extern_callback = @fieldParentPtr(ExportCallback, "callback", &callback);

    vm.unsubscribeDelegate(name, .{
        .context_ptr = @intFromPtr(extern_callback),
        .callback = &ExportCallback.onValueChanged,
    }) catch |err| {
        log("Could not unsubscribe from variable '{s}': {s}", .{ name, @errorName(err) }, .warn);
        return;
    };
}

export fn createVm(source_ptr: [*c]const u8, source_len: usize, on_dialogue_ptr: usize, on_choice_ptr: usize) usize {
    const on_dialogue: OnExportDialogue = @ptrFromInt(on_dialogue_ptr);
    const on_choices: OnExportChoices = @ptrFromInt(on_choice_ptr);

    var fbs = std.io.fixedBufferStream(source_ptr[0..source_len]);
    log("Deserializing bytecode", .{}, .info);
    const bytecode = Bytecode.deserialize(alloc, fbs.reader()) catch {
        log("Could not deserialize bytecode", .{}, .err);
        return 0;
    };

    log("Creating ExportRunner", .{}, .info);
    var extern_runner = alloc.create(ExportRunner) catch {
        log("Could not allocate runner", .{}, .err);
        return 0;
    };
    log("Initializing ExportRunner", .{}, .info);
    extern_runner.* = ExportRunner.init(alloc, on_dialogue, on_choices);

    log("Creating VM", .{}, .info);
    const vm = alloc.create(Vm) catch {
        log("Could not allocate vm", .{}, .err);
        return 0;
    };
    log("Initializing VM", .{}, .info);
    vm.* = Vm.init(alloc, bytecode, &extern_runner.runner) catch {
        log("Could not initialize Vm", .{}, .err);
        return 0;
    };
    return @intFromPtr(vm);
}

export fn destroyVm(vm_ptr: usize) void {
    log("Destroying VM", .{}, .info);
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
        log("OnDialoguePtr: {d}", .{@intFromPtr(self.onExportDialogue)}, .debug);
        self.onExportDialogue(@intFromPtr(vm), &self.dialogue);
    }

    pub fn onChoices(runner: *Runner, vm: *Vm, choices: []Choice) void {
        var self = @fieldParentPtr(ExportRunner, "runner", runner);
        var i: usize = 0;
        var result = self.allocator.alloc(ExportChoice, choices.len) catch {
            log("Could not allocate choices", .{}, .err);
            return;
        };
        while (i < choices.len) : (i += 1) {
            result[i] = .{
                .content = choices[i].content.ptr,
                .visit_count = @intCast(choices[i].visit_count),
                .ip = choices[i].ip,
            };
        }

        log("OnChoicesPtr: {d}", .{@intFromPtr(self.onExportChoices)}, .debug);
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

    pub fn log(msg: [*c]const u8, severity: Severity) void {
        std.debug.print("[{s}] {s}\n", .{ @tagName(severity), msg });
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

    debug_log = TestRunner.log;
    debug_severity = .info;
    defer debug_log = null;
    defer debug_severity = .err;

    var buf: [4096]u8 = undefined;
    const file = try std.fs.cwd().createFile("tmp.topi", .{ .read = true });
    defer std.fs.cwd().deleteFile("tmp.topi") catch |err| {
        std.log.warn("Could not delete file: {}", .{err});
    };
    defer file.close();
    try file.writer().writeAll(text);

    try file.seekTo(0);
    compile("tmp.topi", "tmp.topi".len, &buf, buf.len);
    const on_dialogue: OnExportDialogue = TestRunner.onDialogue;
    const on_choices: OnExportChoices = TestRunner.onChoices;

    const vm_ptr = createVm(&buf, buf.len, @intFromPtr(on_dialogue), @intFromPtr(on_choices));
    const vm: *Vm = @ptrFromInt(vm_ptr);
    vm.bytecode.print(std.debug);
    std.debug.print("\n=====\n", .{});
    const val_name = "value";
    subscribe(
        vm_ptr,
        val_name,
        val_name.len,
        @intFromPtr(&testSubscriber),
    );
    start(vm_ptr);
    while (canContinue(vm_ptr)) {
        run(vm_ptr);
        if (vm.err.msg) |msg| {
            std.log.warn("Error: {s}", .{msg});
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
    vm.bytecode.free(alloc);
    destroyVm(vm_ptr);
}
