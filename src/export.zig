const std = @import("std");
const Vm = @import("vm.zig").Vm;
const values = @import("values.zig");
const Value = @import("values.zig").Value;
const ExportValue = @import("export-value.zig").ExportValue;
const Errors = @import("compiler-error.zig").CompilerErrors;
const Compiler = @import("compiler.zig").Compiler;
const compileSource = @import("compiler.test.zig").compileSource;
const Bytecode = @import("bytecode.zig").Bytecode;
const Module = @import("module.zig").Module;
const File = @import("module.zig").File;
const State = @import("state.zig").State;
const er = @import("export-runner.zig");
const ExportLogger = er.ExportLogger;
const ExportRunner = er.ExportRunner;
const ExportFunction = er.ExportFunction;
const ExportString = er.ExportString;
const ExportLine = er.ExportLine;
const ExportChoice = er.ExportChoice;

var gpa = std.heap.GeneralPurposeAllocator(.{}){};
var alloc = gpa.allocator();

/// Used to pre-calculate the size required
/// for a compiled topi module
export fn calculateCompileSize(path_ptr: [*:0]const u8, log_ptr: usize, log_severity: u8) callconv(.C) usize {
    const logger = ExportLogger{ .on_log = @ptrFromInt(log_ptr), .severity = @enumFromInt(log_severity), .allocator = alloc };
    logger.log("Calculating Compile size", .{}, .debug);
    var counter = std.io.countingWriter(std.io.null_writer);
    writeBytecode(std.mem.sliceTo(path_ptr, 0), counter.writer(), logger);
    return counter.bytes_written;
}

/// Compiles the given path to the byte array
/// Can use `calculateCompileSize` to get the max required, or pass a larger than expected size
export fn compile(path_ptr: [*:0]const u8, out_ptr: [*]u8, max: usize, log_ptr: usize, log_severity: u8) callconv(.C) usize {
    const logger = ExportLogger{ .on_log = @ptrFromInt(log_ptr), .severity = @enumFromInt(log_severity), .allocator = alloc };
    var fbs = std.io.fixedBufferStream(out_ptr[0..max]);
    const writer = fbs.writer();
    writeBytecode(std.mem.sliceTo(path_ptr, 0), writer, logger);
    return fbs.pos;
}

fn writeBytecode(path: []const u8, writer: anytype, logger: ExportLogger) void {
    var arena = std.heap.ArenaAllocator.init(alloc);
    defer arena.deinit();
    const comp_alloc = arena.allocator();

    const full_path = alloc.dupe(u8, path) catch |err| {
        logger.log("Could not allocate file full_path: {s}", .{@errorName(err)}, .err);
        return;
    };
    defer alloc.free(full_path);
    var mod = Module{
        .arena = arena,
        .allocator = comp_alloc,
        .entry = undefined,
        .includes = std.StringArrayHashMap(*File).init(comp_alloc),
    };
    const file = File.create(&mod, full_path) catch |err| {
        logger.log("Could not create Module File: {s}", .{@errorName(err)}, .err);
        return;
    };
    mod.entry = file;
    mod.includes.putNoClobber(file.path, file) catch unreachable;
    defer mod.deinit();
    file.loadSource() catch |err| {
        logger.log("Could not load file source: {s}", .{@errorName(err)}, .err);
        return;
    };

    var output_log = std.ArrayList(u8).init(alloc);
    defer output_log.deinit();
    const output_writer = output_log.writer();

    file.buildTree() catch |err| {
        mod.writeErrors(output_writer) catch |e| {
            logger.log("Could not write errors to log message. Something is very wrong. {s}", .{@errorName(e)}, .err);
            return;
        };
        logger.log("Could not parse file '{s}': {s}\n{s}", .{ full_path, @errorName(err), output_log.items }, .err);
        return;
    };

    var compiler = Compiler.init(comp_alloc, &mod) catch |err| {
        logger.log("Could not create compiler: {s}", .{@errorName(err)}, .err);
        return;
    };
    defer compiler.deinit();

    compiler.compile() catch |err| {
        mod.writeErrors(output_writer) catch |e| {
            logger.log("Could not write errors to log message. Something is very wrong. {s}", .{@errorName(e)}, .err);
            return;
        };
        logger.log("Could not compile file '{s}': {s}\n{s}", .{ full_path, @errorName(err), output_log.items }, .err);
        return;
    };
    var bytecode = compiler.bytecode() catch |err| {
        logger.log("Could not create bytecode: {s}", .{@errorName(err)}, .err);
        return;
    };

    bytecode.serialize(writer) catch |err| {
        logger.log("Could not serialize bytecode: {s}", .{@errorName(err)}, .err);
        return;
    };
}

/// Start of the vm dialogue
export fn start(vm_ptr: usize, path_ptr: [*:0]const u8) callconv(.C) void {
    var vm: *Vm = @ptrFromInt(vm_ptr);
    const runner: *ExportRunner = @fieldParentPtr("runner", vm.runner);
    const logger = runner.logger;
    for (vm.bytecode.global_symbols) |sym| {
        if (sym.is_extern and vm.globals[sym.index] == .void) {
            logger.log("Export \"{s}\" has not been set", .{sym.name}, .warn);
        }
    }

    const path = if (path_ptr[0] != 0) std.mem.sliceTo(path_ptr, 0) else if (vm.bytecode.boughs.len > 0) vm.bytecode.boughs[0].name else {
        logger.log("Topi file does not have a start bough", .{}, .err);
        return;
    };
    vm.start(path) catch |err| logger.log("Could not start vm: {any}", .{err}, .err);
}

export fn run(vm_ptr: usize) callconv(.C) void {
    var vm: *Vm = @ptrFromInt(vm_ptr);
    vm.run() catch |err| {
        const runner: *ExportRunner = @fieldParentPtr("runner", vm.runner);
        runner.logger.log("Vm Error: {s}", .{@errorName(err)}, .err);
        if (vm.err.msg) |msg| {
            runner.logger.log("Error at line {}: {s}", .{ vm.err.line, msg }, .err);
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
    var vm: *Vm = @ptrFromInt(vm_ptr);
    const runner: *ExportRunner = @fieldParentPtr("runner", vm.runner);
    runner.logger.log("Selecting continue", .{}, .debug);
    vm.selectContinue();
}

export fn selectChoice(vm_ptr: usize, index: usize) callconv(.C) void {
    var vm: *Vm = @ptrFromInt(vm_ptr);
    const runner: *ExportRunner = @fieldParentPtr("runner", vm.runner);
    runner.logger.log("Selecting choice {}", .{index}, .debug);
    vm.selectChoice(index) catch runner.logger.log("Invalid choice", .{}, .err);
}

export fn setExtern(vm_ptr: usize, name_ptr: [*:0]const u8, exp_value: ExportValue, free_ptr: usize) callconv(.C) void {
    var vm: *Vm = @ptrFromInt(vm_ptr);
    const runner: *ExportRunner = @fieldParentPtr("runner", vm.runner);
    const logger = runner.logger;
    const name = std.mem.sliceTo(name_ptr, 0);
    logger.log("Setting {s} to {s}", .{ name, @tagName(exp_value.tag) }, .debug);
    const value = exp_value.toValue(vm, @ptrFromInt(free_ptr)) catch |err| {
        logger.log("Could not create Value \"{s}\": {s}", .{ name, @errorName(err) }, .err);
        return;
    };

    vm.setExtern(name, value) catch |err| {
        logger.log("Could not set Export value \"{s}\": {s}", .{ name, @errorName(err) }, .err);
    };
}

export fn setExternFunc(vm_ptr: usize, name_ptr: [*:0]const u8, value_ptr: usize, arity: u8, free_ptr: usize) callconv(.C) void {
    var vm: *Vm = @ptrFromInt(vm_ptr);
    const runner: *ExportRunner = @fieldParentPtr("runner", vm.runner);
    const logger = runner.logger;
    const name = std.mem.sliceTo(name_ptr, 0);
    logger.log("Setting extern function \"{s}\"", .{name}, .info);
    const wrapper = alloc.create(ExportFunction) catch |err| {
        logger.log("Could not allocate ExportFunction '{s}': {s}", .{ name, @errorName(err) }, .err);
        return;
    };
    wrapper.* = ExportFunction.create(vm, @ptrFromInt(value_ptr), @ptrFromInt(free_ptr));
    const val = vm.gc.create(vm, .{ .ext_function = .{ .arity = arity, .backing = ExportFunction.call, .context_ptr = @intFromPtr(wrapper) } }) catch |err| {
        logger.log("Could not create function value '{s}': {s}", .{ name, @errorName(err) }, .err);
        return;
    };
    vm.setExtern(name, val) catch |err| {
        logger.log("Could not set Export value '{s}': {s}", .{ name, @errorName(err) }, .err);
    };
}

export fn destroyValue(value: *ExportValue) void {
    value.deinit(alloc);
}

export fn subscribe(vm_ptr: usize, name_ptr: [*:0]const u8) callconv(.C) bool {
    var vm: *Vm = @ptrFromInt(vm_ptr);

    const name = std.mem.sliceTo(name_ptr, 0);
    return vm.subscribeToValueChange(name) catch |err| {
        const runner: *ExportRunner = @fieldParentPtr("runner", vm.runner);
        runner.logger.log("Could not subscribe to variable '{s}': {s}", .{ name, @errorName(err) }, .warn);
        return false;
    };
}

export fn unsubscribe(vm_ptr: usize, name_ptr: [*:0]const u8) callconv(.C) bool {
    var vm: *Vm = @ptrFromInt(vm_ptr);
    const name = std.mem.sliceTo(name_ptr, 0);
    return vm.unusbscribeToValueChange(name);
}

export fn createVm(source_ptr: [*c]const u8, source_len: usize, on_dialogue_ptr: usize, on_choice_ptr: usize, on_value_changed_ptr: usize, log_ptr: usize, log_severity: u8) callconv(.C) usize {
    const on_dialogue: ExportRunner.OnLine = @ptrFromInt(on_dialogue_ptr);
    const on_choices: ExportRunner.OnChoices = @ptrFromInt(on_choice_ptr);
    const on_value_changed: ExportRunner.OnValueChanged = @ptrFromInt(on_value_changed_ptr);
    const logger = ExportLogger{ .on_log = @ptrFromInt(log_ptr), .severity = @enumFromInt(log_severity), .allocator = alloc };

    var fbs = std.io.fixedBufferStream(source_ptr[0..source_len]);
    logger.log("Deserializing bytecode", .{}, .info);
    const bytecode = Bytecode.deserialize(alloc, fbs.reader()) catch |err| {
        logger.log("Could not deserialize bytecode: {s}", .{@errorName(err)}, .err);
        return 0;
    };

    logger.log("Creating ExportRunner", .{}, .info);
    var extern_runner = alloc.create(ExportRunner) catch {
        logger.log("Could not allocate runner", .{}, .err);
        return 0;
    };
    logger.log("Initializing ExportRunner", .{}, .info);
    extern_runner.* = ExportRunner.init(alloc, on_dialogue, on_choices, on_value_changed, logger);

    logger.log("Creating VM", .{}, .info);
    const vm = alloc.create(Vm) catch {
        logger.log("Could not allocate vm", .{}, .err);
        return 0;
    };
    logger.log("Initializing VM", .{}, .info);
    vm.* = Vm.init(alloc, bytecode, &extern_runner.runner) catch {
        logger.log("Could not initialize Vm", .{}, .err);
        return 0;
    };
    return @intFromPtr(vm);
}

export fn destroyVm(vm_ptr: usize) void {
    var vm: *Vm = @ptrFromInt(vm_ptr);
    const runner: *ExportRunner = @fieldParentPtr("runner", vm.runner);
    runner.logger.log("Destroying VM", .{}, .info);
    alloc.destroy(runner);
    vm.deinit();
    alloc.destroy(vm);
}

export fn calculateStateSize(vm_ptr: usize) usize {
    const vm: *Vm = @ptrFromInt(vm_ptr);
    const runner: *ExportRunner = @fieldParentPtr("runner", vm.runner);
    runner.logger.log("Calculating State size", .{}, .debug);
    return State.calculateSize(vm) catch 0;
}

export fn saveState(vm_ptr: usize, out_ptr: [*:0]u8, max: usize) usize {
    const vm: *Vm = @ptrFromInt(vm_ptr);
    var fbs = std.io.fixedBufferStream(out_ptr[0..max]);
    State.serialize(vm, fbs.writer()) catch |err| {
        const runner: *ExportRunner = @fieldParentPtr("runner", vm.runner);
        runner.logger.log("Could not serialize state: {s}", .{@errorName(err)}, .err);
        return 0;
    };
    return fbs.pos;
}

export fn loadState(vm_ptr: usize, json_str: [*]const u8, json_len: usize) void {
    const vm: *Vm = @ptrFromInt(vm_ptr);
    State.deserialize(vm, json_str[0..json_len]) catch |err| {
        const runner: *ExportRunner = @fieldParentPtr("runner", vm.runner);
        runner.logger.log("Could not deserialize data: {s}", .{@errorName(err)}, .err);
    };
}

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

    pub fn log(msg: ExportString, severity: ExportLogger.Severity) callconv(.C) void {
        std.debug.print("[{s}] {s}\n", .{ @tagName(severity), msg.ptr[0..msg.len] });
    }
    pub fn free(ptr: usize) void {
        std.debug.print("free memory at: {d}\n", .{ptr});
    }
};

test "Create and Destroy Vm" {
    const text =
        \\ extern var value = "test 123"
        \\ extern var list = List{}
        \\ extern var set = Set{}
        \\ extern var map = Map{}
        \\ enum Enum = {
        \\     One
        \\     Two
        \\ }
        \\ extern var enum_value = Enum.One
        \\ === START {
        \\     :: "A person approaches." #starting
        \\     :Stranger: "Hey there."
        \\     print(value)
        \\     print(list)
        \\     print(set)
        \\     print(map)
        \\     print(enum_value)
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

    const file = try std.fs.cwd().createFile("tmp.topi", .{ .read = true });
    defer std.fs.cwd().deleteFile("tmp.topi") catch {};
    defer file.close();
    try file.writer().writeAll(text);

    try file.seekTo(0);
    const buf = try std.testing.allocator.alloc(u8, 8192);
    defer std.testing.allocator.free(buf);
    const dir_path = try std.fs.cwd().realpathAlloc(std.testing.allocator, ".");
    defer std.testing.allocator.free(dir_path);
    const path = try std.fs.path.resolve(std.testing.allocator, &.{ dir_path, "tmp.topi" ++ "\x00" });
    defer std.testing.allocator.free(path);
    const path_ptr: [*:0]const u8 = path[0 .. path.len - 1 :0];
    const calc_size = calculateCompileSize(path_ptr, @intFromPtr(&TestRunner.log), @intFromEnum(ExportLogger.Severity.debug));
    const compile_size = compile(path_ptr, buf.ptr, buf.len, @intFromPtr(&TestRunner.onValueChanged), @intFromEnum(ExportLogger.Severity.debug));
    try std.testing.expectEqual(compile_size, calc_size);

    const vm_ptr = createVm(
        buf.ptr,
        buf.len,
        @intFromPtr(&TestRunner.onLine),
        @intFromPtr(&TestRunner.onChoices),
        @intFromPtr(&TestRunner.onValueChanged),
        @intFromPtr(&TestRunner.log),
        @intFromEnum(ExportLogger.Severity.debug),
    );
    const vm: *Vm = @ptrFromInt(vm_ptr);

    defer destroyVm(vm_ptr);
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
        .{ .tag = .string, .data = .{ .string = .{ .ptr = "some".ptr, .len = 4 } } },
        .{ .tag = .string, .data = .{ .string = .{ .ptr = "value".ptr, .len = 5 } } },
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

    const enum_value = ExportValue{
        .tag = .@"enum",
        .data = .{ .@"enum" = .{
            .name = .{ .ptr = "Enum".ptr, .len = 4 },
            .value = .{ .ptr = "Two".ptr, .len = 3 },
        } },
    };
    const free_ptr = @intFromPtr(&TestRunner.free);
    setExtern(vm_ptr, "list", list, free_ptr);
    setExtern(vm_ptr, "set", set, free_ptr);
    setExtern(vm_ptr, "map", map, free_ptr);
    setExtern(vm_ptr, "enum_value", enum_value, free_ptr);

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
