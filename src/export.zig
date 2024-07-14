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
var gpa = std.heap.GeneralPurposeAllocator(.{}){};
var alloc = gpa.allocator();
var debug_log: ?OnExportDebugLog = null;
var debug_severity: Severity = .err;
var value_changed_callback: ?OnExportValueChanged = null;

const ExportLine = extern struct {
    content: [*c]const u8,
    content_len: usize,
    speaker: [*c]const u8,
    speaker_len: usize,
    tags: [*][*c]const u8,
    tags_length: u8,
};

const ExportChoice = extern struct {
    content: [*]const u8,
    content_len: usize,
    tags: [*][*c]const u8,
    tags_length: u8,
    visit_count: u32,
    ip: u32,
};

const OnExportValueChanged = *const fn (vm_ptr: usize, name_ptr: [*c]const u8, name_len: usize, value: ExportValue) callconv(.C) void;
const OnExportLine = *const fn (vm_ptr: usize, dialogue: *ExportLine) callconv(.C) void;
const OnExportChoices = *const fn (vm_ptr: usize, choices: [*]ExportChoice, choices_len: u8) callconv(.C) void;

const OnExportDebugLog = *const fn (msg: [*:0]const u8, severity: Severity) callconv(.C) void;
const Severity = enum(u8) {
    debug,
    info,
    warn,
    err,
};

fn log(comptime msg: []const u8, args: anytype, severity: Severity) void {
    if (@intFromEnum(severity) < @intFromEnum(debug_severity)) return;
    if (debug_log) |l| {
        const fmt = std.fmt.allocPrintZ(alloc, msg, args) catch |err| blk: {
            std.log.err("Error fmt: {}", .{err});
            break :blk msg;
        };
        defer alloc.free(fmt[0 .. fmt.len - 1]);
        l(@ptrCast(fmt), severity);
    }
}

/// Sets the global debug logger instance with
/// a pointer to a logger instance.
export fn setDebugLog(logger_ptr: usize) callconv(.C) void {
    debug_log = @ptrFromInt(logger_ptr);
}

/// Sets the global debug severity
export fn setDebugSeverity(severity: u8) callconv(.C) void {
    debug_severity = @enumFromInt(severity);
}

/// Used to pre-calculate the size required
/// for a compiled topi module
export fn calculateCompileSize(path_ptr: [*:0]const u8) callconv(.C) usize {
    log("Calculating Compile size", .{}, .debug);
    var counter = std.io.countingWriter(std.io.null_writer);
    writeBytecode(std.mem.sliceTo(path_ptr, 0), counter.writer());
    return counter.bytes_written;
}

/// Compiles the given path to the byte array
/// Can use `calculateCompileSize` to get the max required, or pass a larger than expected size
export fn compile(path_ptr: [*:0]const u8, out_ptr: [*]u8, max: usize) callconv(.C) usize {
    var fbs = std.io.fixedBufferStream(out_ptr[0..max]);
    const writer = fbs.writer();
    writeBytecode(std.mem.sliceTo(path_ptr, 0), writer);
    return fbs.pos;
}

fn writeBytecode(path: []const u8, writer: anytype) void {
    var arena = std.heap.ArenaAllocator.init(alloc);
    defer arena.deinit();
    const comp_alloc = arena.allocator();

    const full_path = alloc.dupe(u8, path) catch |err| {
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
export fn start(vm_ptr: usize, path_ptr: [*:0]const u8) callconv(.C) void {
    log("Starting VM", .{}, .info);
    var vm: *Vm = @ptrFromInt(vm_ptr);
    for (vm.bytecode.global_symbols) |sym| {
        if (sym.is_extern and vm.globals[sym.index] == .void) {
            log("Export \"{s}\" has not been set", .{sym.name}, .warn);
        }
    }

    const path = if (path_ptr[0] != 0) std.mem.sliceTo(path_ptr, 0) else if (vm.bytecode.boughs.len > 0) vm.bytecode.boughs[0].name else {
        log("Topi file does not have a start bough", .{}, .err);
        return;
    };
    vm.start(path) catch |err| log("Could not start vm: {any}", .{err}, .err);
}

export fn run(vm_ptr: usize) callconv(.C) void {
    log("Running Vm", .{}, .debug);
    var vm: *Vm = @ptrFromInt(vm_ptr);
    vm.run() catch |err| {
        log("Vm Error: {s}", .{@errorName(err)}, .err);
        if (vm.err.msg) |msg| {
            log("Error at line {}: {s}", .{ vm.err.line, msg }, .err);
        }
    };
}

export fn canContinue(vm_ptr: usize) callconv(.C) bool {
    const vm: *Vm = @ptrFromInt(vm_ptr);
    return vm.can_continue;
}

export fn isWaiting(vm_ptr: usize) callconv(.C) bool {
    const vm: *Vm = @ptrFromInt(vm_ptr);
    return vm.is_waiting;
}

export fn selectContinue(vm_ptr: usize) callconv(.C) void {
    log("Selecting continue", .{}, .info);
    var vm: *Vm = @ptrFromInt(vm_ptr);
    vm.selectContinue();
}

export fn selectChoice(vm_ptr: usize, index: usize) callconv(.C) void {
    log("Selecting choice", .{}, .info);
    var vm: *Vm = @ptrFromInt(vm_ptr);
    vm.selectChoice(index) catch log("Invalid choice", .{}, .err);
}

export fn setExtern(vm_ptr: usize, name_ptr: [*:0]const u8, exp_value: ExportValue) callconv(.C) void {
    var vm: *Vm = @ptrFromInt(vm_ptr);
    const name = std.mem.sliceTo(name_ptr, 0);
    const value = exp_value.toValue(vm) catch |err| {
        log("Could not create Value \"{s}\": {s}", .{ name, @errorName(err) }, .err);
        return;
    };

    vm.setExtern(name, value) catch |err| {
        log("Could not set Export value \"{s}\": {s}", .{ name, @errorName(err) }, .err);
    };
}

export fn setExternFunc(vm_ptr: usize, name_ptr: [*:0]const u8, value_ptr: usize, arity: u8) callconv(.C) void {
    var vm: *Vm = @ptrFromInt(vm_ptr);
    const name = std.mem.sliceTo(name_ptr, 0);
    log("Setting extern function \"{s}\"", .{name}, .info);
    const wrapper = alloc.create(ExportFunction) catch |err| {
        log("Could not allocate ExportFunction '{s}': {s}", .{ name, @errorName(err) }, .err);
        return;
    };
    wrapper.* = ExportFunction.create(vm, @as(ExportFunction.Delegate, @ptrFromInt(value_ptr)));
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
    vm: *Vm,

    pub const Delegate = *const fn (vm_ptr: usize, args: [*c]ExportValue, args_len: u8) callconv(.C) ExportValue;

    pub fn create(vm: *Vm, func: Delegate) ExportFunction {
        return .{
            .func = func,
            .vm = vm,
        };
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
            exp_args[i] = ExportValue.fromValue(args[i], arenaAlloc);
        }
        log("Calling ExportFunction", .{}, .info);
        var v = self.func(@intFromPtr(self.vm), exp_args.ptr, @intCast(exp_args.len));
        return v.toValue(self.vm) catch |err| {
            log("Could not return value of extern function, returning null: {s}", .{@errorName(err)}, .err);
            return values.Nil;
        };
    }
};

fn exportValueChangedCallback(vm: *Vm, name: []const u8, value: Value) void {
    log("ValueChangedCallback: {s}", .{name}, .debug);
    if (value_changed_callback) |cb| {
        const export_value = ExportValue.fromValue(value, alloc);
        defer export_value.deinit(alloc);
        cb(@intFromPtr(vm), name, export_value);
    }
}

export fn subscribe(vm_ptr: usize, name_ptr: [*:0]const u8) callconv(.C) bool {
    var vm: *Vm = @ptrFromInt(vm_ptr);

    const name = std.mem.sliceTo(name_ptr, 0);
    return vm.subscribeToValueChange(name) catch |err| {
        log("Could not subscribe to variable '{s}': {s}", .{ name, @errorName(err) }, .warn);
        return false;
    };
}

export fn unsubscribe(vm_ptr: usize, name_ptr: [*:0]const u8) callconv(.C) bool {
    var vm: *Vm = @ptrFromInt(vm_ptr);
    const name = std.mem.sliceTo(name_ptr, 0);
    return vm.unusbscribeToValueChange(name);
}

export fn createVm(source_ptr: [*c]const u8, source_len: usize, on_dialogue_ptr: usize, on_choice_ptr: usize, on_value_changed_ptr: usize) callconv(.C) usize {
    const on_dialogue: OnExportLine = @ptrFromInt(on_dialogue_ptr);
    const on_choices: OnExportChoices = @ptrFromInt(on_choice_ptr);
    const on_value_changed: OnExportValueChanged = @ptrFromInt(on_value_changed_ptr);

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
    extern_runner.* = ExportRunner.init(alloc, on_dialogue, on_choices, on_value_changed);

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

export fn saveState(vm_ptr: usize, out_ptr: [*:0]u8, max: usize) usize {
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
    on_export_line: OnExportLine,
    on_export_choices: OnExportChoices,
    allocator: std.mem.Allocator,
    on_export_value_changed: OnExportValueChanged,

    runner: Runner,
    tags: [512][*c]const u8,
    dialogue: ExportLine = undefined,

    pub fn init(allocator: std.mem.Allocator, on_line: OnExportLine, on_choices: OnExportChoices, on_value_changed: OnExportValueChanged) ExportRunner {
        return .{
            .allocator = allocator,
            .on_export_line = on_line,
            .on_export_choices = on_choices,
            .on_export_value_changed = on_value_changed,
            .tags = [_][*c]const u8{0} ** 512,
            .runner = .{
                .on_line = onLine,
                .on_choices = onChoices,
                .on_value_changed = onValueChanged,
            },
        };
    }

    pub fn onLine(runner: *Runner, vm: *Vm, dialogue: Line) void {
        var self: *ExportRunner = @fieldParentPtr("runner", runner);

        var i: usize = 0;
        while (i < dialogue.tags.len) : (i += 1) {
            self.tags[i] = dialogue.tags[i].ptr;
        }

        const content = dialogue.content;
        const speaker = dialogue.speaker;
        self.dialogue = .{
            .content = content.ptr,
            .content_len = content.len,
            .speaker = if (speaker) |s| s.ptr else "",
            .speaker_len = if (speaker) |s| s.len else 0,
            .tags = &self.tags,
            .tags_length = @intCast(dialogue.tags.len),
        };
        log("Line: {s}", .{dialogue.content}, .debug);
        self.on_export_line(@intFromPtr(vm), &self.dialogue);
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
                const tag = choices[i].tags[t];
                self.tags[t_count + t] = tag.ptr;
            }
            t_count += t;

            log("Choice: {s}", .{choices[i].content}, .debug);
            const content = choices[i].content;
            result[i] = .{
                .content = content.ptr,
                .content_len = content.len,
                .tags = self.tags[t_start..t_count].ptr,
                .tags_length = @intCast(choices[i].tags.len),
                .visit_count = @intCast(choices[i].visit_count),
                .ip = choices[i].ip,
            };
        }

        self.on_export_choices(@intFromPtr(vm), result.ptr, @intCast(result.len));
        self.allocator.free(result);
    }

    pub fn onValueChanged(runner: *Runner, vm: *Vm, name: []const u8, value: Value) void {
        var self: *ExportRunner = @fieldParentPtr("runner", runner);
        const exp_value = ExportValue.fromValue(value, vm.allocator);
        defer exp_value.deinit(vm.allocator);
        self.on_export_value_changed(@intFromPtr(vm), name.ptr, name.len, exp_value);
    }
};

const TestRunner = struct {
    pub fn onLine(vm_ptr: usize, dialogue: *ExportLine) callconv(.C) void {
        _ = dialogue;
        selectContinue(vm_ptr);
    }

    pub fn onChoices(vm_ptr: usize, choices: [*]ExportChoice, choices_len: u8) callconv(.C) void {
        _ = choices;
        _ = choices_len;
        selectChoice(vm_ptr, 0);
    }

    pub fn onValueChanged(_: usize, name_ptr: [*c]const u8, name_len: usize, value: ExportValue) callconv(.C) void {
        std.debug.print("onValueChanged: {s} = ", .{name_ptr[0..name_len]});
        value.print(std.debug);
        std.debug.print("\n", .{});
    }

    pub fn log(msg: [*:0]const u8, severity: Severity) callconv(.C) void {
        std.debug.print("[{s}] {s}\n", .{ @tagName(severity), msg });
    }
};

test "Create and Destroy Vm" {
    const text =
        \\ extern var value = "test 123"
        \\ extern var list = List{}
        \\ extern var set = Set{}
        \\ extern var map = Map{}
        \\ === START {
        \\     :: "A person approaches." #starting
        \\     :Stranger: "Hey there."
        \\     print(value)
        \\     print(list)
        \\     print(set)
        \\     print(map)
        \\     list.add(3)
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
    const path = try std.fs.path.resolve(std.testing.allocator, &.{ dir_path, "tmp.topi" ++ "\x00" });
    defer std.testing.allocator.free(path);
    const path_ptr: [*:0]const u8 = path[0 .. path.len - 1 :0];
    const calc_size = calculateCompileSize(path_ptr);
    const compile_size = compile(path_ptr, buf.ptr, buf.len);
    try std.testing.expectEqual(compile_size, calc_size);

    const on_dialogue: OnExportLine = TestRunner.onLine;
    const on_choices: OnExportChoices = TestRunner.onChoices;
    const on_value_changed: OnExportValueChanged = TestRunner.onValueChanged;

    const vm_ptr = createVm(
        buf.ptr,
        buf.len,
        @intFromPtr(on_dialogue),
        @intFromPtr(on_choices),
        @intFromPtr(on_value_changed),
    );
    const vm: *Vm = @ptrFromInt(vm_ptr);

    defer destroyVm(vm_ptr);
    // defer vm.bytecode.free(std.testing.allocator);

    var list_value = [2]ExportValue{
        .{ .tag = .number, .data = .{ .number = 1 } },
        .{ .tag = .number, .data = .{ .number = 2 } },
    };
    const list = ExportValue{ .tag = .list, .data = .{
        .list = .{
            .items = &list_value,
            .count = 2,
        },
    } };
    var set_value = [2]ExportValue{
        .{ .tag = .string, .data = .{ .string = "some".ptr } },
        .{ .tag = .string, .data = .{ .string = "value".ptr } },
    };
    const set = ExportValue{
        .tag = .set,
        .data = .{ .list = .{ .items = &set_value, .count = 2 } },
    };
    var map_value = [4]ExportValue{
        .{ .tag = .number, .data = .{ .number = 0 } },
        .{ .tag = .number, .data = .{ .number = 0.0001 } },
        .{ .tag = .number, .data = .{ .number = 1 } },
        .{ .tag = .number, .data = .{ .number = 1.1111 } },
    };
    const map = ExportValue{
        .tag = .map,
        .data = .{ .list = .{ .items = &map_value, .count = 4 } },
    };
    setExtern(vm_ptr, "list", list);
    setExtern(vm_ptr, "set", set);
    setExtern(vm_ptr, "map", map);

    const list_name = "list";
    _ = subscribe(vm_ptr, list_name);

    start(vm_ptr, "");
    while (canContinue(vm_ptr)) {
        run(vm_ptr);
        if (vm.err.msg) |msg| {
            std.log.warn("Error: {s}", .{msg});
            break;
        }
    }
    _ = unsubscribe(vm_ptr, list_name);
}
