const std = @import("std");
const Vm = @import("vm.zig").Vm;
const values = @import("values.zig");
const Value = @import("values.zig").Value;
const ExportValue = @import("export-value.zig").ExportValue;
const Errors = @import("compiler-error.zig").CompilerErrors;
const Compiler = @import("compiler.zig").Compiler;
const compileSource = @import("compiler.test.zig").compileSource;
const Bytecode = @import("bytecode.zig").Bytecode;
const runners = @import("runner.zig");
const Module = @import("module.zig").Module;
const File = @import("module.zig").File;
const Runner = runners.Runner;
const Line = runners.Line;
const Choice = runners.Choice;
const State = @import("./state.zig").State;

// const alloc = std.testing.allocator;
var alloc = std.heap.page_allocator;
var debug_log: ?OnExportDebugLog = null;
var debug_severity: Severity = .err;
var value_changed_callback: ?OnExportValueChanged = null;

const ExportLine = extern struct {
    content: [*c]const u8,
    content_length: usize,
    speaker: [*c]const u8,
    speaker_length: usize,
    tags: [*][*c]const u8,
    tags_length: u8,
};

const ExportChoice = extern struct {
    content: [*c]const u8,
    content_length: usize,
    tags: [*][*c]const u8,
    tags_length: u8,
    visit_count: u32,
    ip: u32,
};

const OnExportValueChanged = *const fn (name: [*c]const u8, value: ExportValue) void;
const OnExportLine = *const fn (vm_ptr: usize, dialogue: *ExportLine) void;
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
        const fmt = std.fmt.allocPrint(alloc, msg, args) catch |err| blk: {
            std.log.err("Error fmt: {}", .{err});
            break :blk msg;
        };
        defer alloc.free(fmt);
        l(fmt.ptr, severity);
    }
}

/// Sets the global debug logger instance with
/// a pointer to a logger instance.
///
///   Warning: Be careful when passing the logger pointer. Incorrect usage can
///   lead to unexpected behavior. Also ensure to manage the life cycle of the
///   logger instance properly to avoid memory leaks and dangling pointer issues.
export fn setDebugLog(logger_ptr: usize) void {
    debug_log = @ptrFromInt(logger_ptr);
}

/// Sets the global debug severity
export fn setDebugSeverity(severity: u8) void {
    debug_severity = @enumFromInt(severity);
}

/// Used to pre-calculate the size required
/// for a compiled topi module
export fn calculateCompileSize(path_ptr: [*c]const u8, path_length: usize) usize {
    log("Calculating Compile size", .{}, .debug);
    var counter = std.io.countingWriter(std.io.null_writer);
    writeBytecode(path_ptr, path_length, counter.writer());
    return counter.bytes_written;
}

/// Compiles the given path to the byte array
/// Can use `calculateCompileSize` to get the max required, or pass a larger than expected size
export fn compile(path_ptr: [*c]const u8, path_length: usize, out_ptr: [*c]u8, max: usize) usize {
    var fbs = std.io.fixedBufferStream(out_ptr[0..max]);
    const writer = fbs.writer();
    writeBytecode(path_ptr, path_length, writer);
    return fbs.pos;
}

fn writeBytecode(path_ptr: [*]const u8, path_length: usize, writer: anytype) void {
    var arena = std.heap.ArenaAllocator.init(alloc);
    defer arena.deinit();
    const comp_alloc = arena.allocator();

    const full_path = alloc.dupe(u8, path_ptr[0..path_length]) catch |err| {
        log("Could not allocate file full_path: {s}", .{@errorName(err)}, .err);
        return;
    };
    defer alloc.free(full_path);
    var mod = Module{
        .arena = arena,
        .allocator = comp_alloc,
        .entry = undefined,
        .includes = std.StringHashMap(*File).init(comp_alloc),
    };
    const file = File.create(&mod, full_path) catch |err| {
        log("Could not create Module File: {s}", .{@errorName(err)}, .err);
        return;
    };
    mod.entry = file;
    mod.includes.putNoClobber(file.path, file) catch unreachable;
    defer mod.deinit();
    file.loadSource() catch |err| {
        log("Could not load file source: {s}", .{@errorName(err)}, .err);
        return;
    };

    var output_log = std.ArrayList(u8).init(alloc);
    defer output_log.deinit();
    const output_writer = output_log.writer();

    file.buildTree() catch |err| {
        mod.writeErrors(output_writer) catch |e| {
            log("Could not write errors to log message. Something is very wrong. {s}", .{@errorName(e)}, .err);
            return;
        };
        log("Could not parse file '{s}': {s}\n{s}", .{ full_path, @errorName(err), output_log.items }, .err);
        return;
    };

    var compiler = Compiler.init(comp_alloc) catch |err| {
        log("Could not create compiler: {s}", .{@errorName(err)}, .err);
        return;
    };
    defer compiler.deinit();

    compiler.compile(&mod) catch |err| {
        mod.writeErrors(output_writer) catch |e| {
            log("Could not write errors to log message. Something is very wrong. {s}", .{@errorName(e)}, .err);
            return;
        };
        log("Could not compile file '{s}': {s}\n{s}", .{ full_path, @errorName(err), output_log.items }, .err);
        return;
    };
    var bytecode = compiler.bytecode() catch |err| {
        log("Could not create bytecode: {s}", .{@errorName(err)}, .err);
        return;
    };

    bytecode.serialize(writer) catch |err| {
        log("Could not serialize bytecode: {s}", .{@errorName(err)}, .err);
        return;
    };
}

/// Start of the vm dialogue
export fn start(vm_ptr: usize, path_ptr: [*]const u8, path_len: usize) void {
    log("Starting VM", .{}, .info);
    var vm: *Vm = @ptrFromInt(vm_ptr);
    for (vm.bytecode.global_symbols) |sym| {
        if (sym.is_extern and vm.globals[sym.index] == .void) {
            log("Export \"{s}\" has not been set", .{sym.name}, .warn);
        }
    }
    const path = if (path_len > 0) path_ptr[0..path_len] else if (vm.bytecode.boughs.len > 0) vm.bytecode.boughs[0].name else {
        log("Topi file does not have a start bough", .{}, .err);
        return;
    };
    vm.start(path) catch |err| log("Could not start vm: {any}", .{err}, .err);
}

export fn run(vm_ptr: usize) void {
    log("Running VM", .{}, .info);
    var vm: *Vm = @ptrFromInt(vm_ptr);
    vm.run() catch {
        if (vm.err.msg) |msg| {
            log("Error at line {}: {s}", .{ vm.err.line, msg }, .err);
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

fn exportValueChangedCallback(name: []const u8, value: Value) void {
    log("ValueChangedCallback: {s}", .{name}, .debug);
    if (value_changed_callback) |cb| {
        cb(name.ptr, ExportValue.fromValue(value, alloc));
    }
}

export fn setSubscriberCallback(vm_ptr: usize, callback_ptr: usize) void {
    value_changed_callback = @ptrFromInt(callback_ptr);
    var vm: *Vm = @ptrFromInt(vm_ptr);
    vm.value_subscriber_callback = &exportValueChangedCallback;
}

export fn subscribe(vm_ptr: usize, name_ptr: [*c]const u8, name_length: usize) bool {
    var vm: *Vm = @ptrFromInt(vm_ptr);

    const name = name_ptr[0..name_length];
    return vm.subscribeToValueChange(name) catch |err| {
        log("Could not subscribe to variable '{s}': {s}", .{ name, @errorName(err) }, .warn);
        return false;
    };
}

export fn unsubscribe(vm_ptr: usize, name_ptr: [*c]const u8, name_length: usize) bool {
    var vm: *Vm = @ptrFromInt(vm_ptr);
    const name = name_ptr[0..name_length];
    return vm.unusbscribeToValueChange(name);
}

export fn createVm(source_ptr: [*c]const u8, source_len: usize, on_dialogue_ptr: usize, on_choice_ptr: usize) usize {
    const on_dialogue: OnExportLine = @ptrFromInt(on_dialogue_ptr);
    const on_choices: OnExportChoices = @ptrFromInt(on_choice_ptr);

    var fbs = std.io.fixedBufferStream(source_ptr[0..source_len]);
    log("Deserializing bytecode", .{}, .info);
    const bytecode = Bytecode.deserialize(alloc, fbs.reader()) catch |err| {
        log("Could not deserialize bytecode: {s}", .{@errorName(err)}, .err);
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
    const runner: *ExportRunner = @fieldParentPtr("runner", vm.runner);
    alloc.destroy(runner);
    vm.deinit();
    alloc.destroy(vm);
}

export fn calculateStateSize(vm_ptr: usize) usize {
    const vm: *Vm = @ptrFromInt(vm_ptr);
    log("Calculating State size", .{}, .debug);
    return State.calculateSize(vm) catch 0;
}

export fn saveState(vm_ptr: usize, out_ptr: [*c]u8, max: usize) usize {
    const vm: *Vm = @ptrFromInt(vm_ptr);
    var fbs = std.io.fixedBufferStream(out_ptr[0..max]);
    State.serialize(vm, fbs.writer()) catch |err| {
        log("Could not serialize state: {s}", .{@errorName(err)}, .err);
        return 0;
    };
    return fbs.pos;
}

export fn loadState(vm_ptr: usize, json_str: [*]const u8, json_len: usize) void {
    const vm: *Vm = @ptrFromInt(vm_ptr);
    State.deserialize(vm, json_str[0..json_len]) catch |err| {
        log("Could not deserialize data: {s}", .{@errorName(err)}, .err);
    };
}

const ExportRunner = struct {
    onExportLine: OnExportLine,
    onExportChoices: OnExportChoices,
    allocator: std.mem.Allocator,

    runner: Runner,
    tags: [512][*c]const u8,
    dialogue: ExportLine = undefined,

    pub fn init(allocator: std.mem.Allocator, on_dialogue: OnExportLine, on_choices: OnExportChoices) ExportRunner {
        return .{
            .allocator = allocator,
            .onExportLine = on_dialogue,
            .onExportChoices = on_choices,
            .tags = [_][*c]const u8{0} ** 512,
            .runner = .{
                .onLineFn = onLine,
                .onChoicesFn = onChoices,
            },
        };
    }

    pub fn onLine(runner: *Runner, vm: *Vm, dialogue: Line) void {
        var self: *ExportRunner = @fieldParentPtr("runner", runner);

        var i: usize = 0;
        while (i < dialogue.tags.len) : (i += 1) {
            self.tags[i] = dialogue.tags[i].ptr;
        }

        self.dialogue = .{
            .content = dialogue.content.ptr,
            .content_length = dialogue.content.len,
            .speaker = if (dialogue.speaker) |s| s.ptr else "",
            .speaker_length = @intCast(if (dialogue.speaker) |s| s.len else 0),
            .tags = &self.tags,
            .tags_length = @intCast(dialogue.tags.len),
        };
        log("OnLinePtr: {d}", .{@intFromPtr(self.onExportLine)}, .debug);
        self.onExportLine(@intFromPtr(vm), &self.dialogue);
    }

    pub fn onChoices(runner: *Runner, vm: *Vm, choices: []Choice) void {
        var self: *ExportRunner = @fieldParentPtr("runner", runner);
        var i: usize = 0;
        var result = self.allocator.alloc(ExportChoice, choices.len) catch {
            log("Could not allocate choices", .{}, .err);
            return;
        };
        var t_count: usize = 0;
        while (i < choices.len) : (i += 1) {
            var t: usize = 0;
            const t_start = t_count;
            while (t < choices[i].tags.len) : (t += 1) {
                self.tags[t_count + t] = choices[i].tags[t].ptr;
            }
            t_count += t;

            result[i] = .{
                .content = choices[i].content.ptr,
                .content_length = choices[i].content.len,
                .tags = self.tags[t_start..t_count].ptr,
                .tags_length = @intCast(choices[i].tags.len),
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
    pub fn onLine(vm_ptr: usize, dialogue: *ExportLine) void {
        _ = dialogue;
        selectContinue(vm_ptr);
    }

    pub fn onChoices(vm_ptr: usize, choices: [*]ExportChoice, choices_len: u8) void {
        _ = choices;
        _ = choices_len;
        selectChoice(vm_ptr, 0);
    }

    pub fn log(msg: [*c]const u8, severity: Severity) void {
        std.debug.print("[{s}] {s}\n", .{ @tagName(severity), msg });
    }
};

fn testSubscriber(name: [*c]const u8, value: ExportValue) void {
    std.log.warn("testSubscribe: {s}", .{name});
    std.testing.expectEqualSlices(u8, "321 test", value.data.string[0..8]) catch {};
}

test "Create and Destroy Vm" {
    const text =
        \\ var value = "test 123"
        \\ var list = List{1,2,3,4}
        \\ var set = Set{"some", "string", "values"}
        \\ var map = Map{0: 0.0001, 1: 1.1111, 2: 2.222 }
        \\ === START {
        \\     :: "A person approaches." #starting
        \\     :Stranger: "Hey there."
        \\     value = "321 test"
        \\     fork^ {
        \\         ~ "Greet them." #lots #of #tags #here {
        \\             :Drew: "Oh, uh, nice to meet you. My name is Drew."
        \\             :Drew: "Sorry, I thought you were someone I knew."
        \\             :Drew: "I'd love to stay and chat, but this is just a short demo."
        \\         }
        \\         ~ "Say nothing." #one #here {
        \\             :: "The person acts as though they were addressing someone else."
        \\         }
        \\     }
        \\     :: "They walk away..."
        \\ }
        \\
    ;

    debug_log = TestRunner.log;
    defer debug_log = null;

    const file = try std.fs.cwd().createFile("tmp.topi", .{ .read = true });
    defer std.fs.cwd().deleteFile("tmp.topi") catch {};
    defer file.close();
    try file.writer().writeAll(text);

    try file.seekTo(0);
    const buf = try std.testing.allocator.alloc(u8, 4096);
    defer std.testing.allocator.free(buf);
    const dir_path = try std.fs.cwd().realpathAlloc(std.testing.allocator, ".");
    defer std.testing.allocator.free(dir_path);
    const path = try std.fs.path.resolve(std.testing.allocator, &.{ dir_path, "tmp.topi" });
    defer std.testing.allocator.free(path);

    const compile_size = calculateCompileSize(path.ptr, path.len);
    try std.testing.expectEqual(compile(path.ptr, path.len, buf.ptr, buf.len), compile_size);

    const on_dialogue: OnExportLine = TestRunner.onLine;
    const on_choices: OnExportChoices = TestRunner.onChoices;

    const vm_ptr = createVm(buf.ptr, buf.len, @intFromPtr(on_dialogue), @intFromPtr(on_choices));
    const vm: *Vm = @ptrFromInt(vm_ptr);
    setSubscriberCallback(vm_ptr, @intFromPtr(&testSubscriber));

    defer destroyVm(vm_ptr);
    defer vm.bytecode.free(alloc);
    const val_name = "value";
    _ = subscribe(vm_ptr, val_name, val_name.len);
    start(vm_ptr, "", 0);
    while (canContinue(vm_ptr)) {
        run(vm_ptr);
        if (vm.err.msg) |msg| {
            std.log.warn("Error: {s}", .{msg});
            break;
        }
    }
    _ = unsubscribe(vm_ptr, val_name, val_name.len);

    const list_name = "list";
    var list_value: ExportValue = undefined;
    if (tryGetValue(
        vm_ptr,
        list_name,
        list_name.len,
        &list_value,
    )) {
        const list = list_value.data.list.items;
        try std.testing.expectEqual(1, list[0].data.number);
        try std.testing.expectEqual(2, list[1].data.number);
        try std.testing.expectEqual(3, list[2].data.number);
        try std.testing.expectEqual(4, list[3].data.number);
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
        const set = set_value.data.list.items;
        try std.testing.expectEqualSlices(u8, "some", set[0].data.string[0..4]);
        try std.testing.expectEqualSlices(u8, "string", set[1].data.string[0..6]);
        try std.testing.expectEqualSlices(u8, "values", set[2].data.string[0..6]);
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
        const map = map_value.data.list.items;
        try std.testing.expectEqual(0, map[0].data.number);
        try std.testing.expectEqual(0.0001, map[1].data.number);
        try std.testing.expectEqual(1, map[2].data.number);
        try std.testing.expectEqual(1.1111, map[3].data.number);
        try std.testing.expectEqual(2, map[4].data.number);
        try std.testing.expectEqual(2.222, map[5].data.number);
        destroyValue(&map_value);
    }
}
