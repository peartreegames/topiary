const std = @import("std");
const topi = @import("topi");
const Compiler = topi.backend.Compiler;
const CompilerErrors = topi.backend.CompilerErrors;
const Bytecode = topi.backend.Bytecode;

const Vm = topi.runtime.Vm;
const State = topi.runtime.State;
const Extern = topi.runtime.Extern;

const Module = topi.module.Module;
const File = topi.module.File;

const export_runner = @import("runner.zig");
const ExportLogger = export_runner.ExportLogger;
const ExportRunner = export_runner.ExportRunner;
const ExportFunction = export_runner.ExportFunction;
const ExportString = export_runner.ExportString;
const ExportLine = export_runner.ExportLine;
const ExportChoice = export_runner.ExportChoice;

var gpa: std.heap.DebugAllocator(.{}) = .init;
var alloc = gpa.allocator();

/// Used to pre-calculate the size required
/// for a compiled topi module
pub export fn calculateCompileSize(path_ptr: [*:0]const u8, log_ptr: usize, log_severity: u8) callconv(.c) usize {
    const logger = ExportLogger{ .on_log = @ptrFromInt(log_ptr), .severity = @enumFromInt(log_severity), .allocator = alloc };

    const mod = createModule(std.mem.sliceTo(path_ptr, 0), logger);
    if (mod == null) return 0;
    defer mod.?.deinit();

    var bytecode = createBytecode(mod.?, logger);
    if (bytecode == null) return 0;

    var buf: [1024]u8 = undefined;
    var counter = std.Io.Writer.Discarding.init(&buf);
    const count = bytecode.?.serialize(alloc, &counter.writer) catch |err| {
        logger.log("Could not calculate size {s}", .{@errorName(err)}, .err);
        return 0;
    };
    logger.log("Calculated Compile size {d}", .{count}, .debug);
    return count;
}

/// Compiles the given path to the byte array
/// Can use `calculateCompileSize` to get the max required, or pass a larger than expected size
pub export fn compile(path_ptr: [*:0]const u8, out_ptr: [*]u8, max: usize, log_ptr: usize, log_severity: u8) callconv(.c) usize {
    const logger = ExportLogger{ .on_log = @ptrFromInt(log_ptr), .severity = @enumFromInt(log_severity), .allocator = alloc };
    var fixed = std.io.Writer.fixed(out_ptr[0..max]);
    const size = writeBytecode(std.mem.sliceTo(path_ptr, 0), &fixed, logger);
    logger.log("Compiled size {d} / {d}", .{ size, max }, .debug);
    return size;
}

fn createModule(path: []const u8, logger: ExportLogger) ?*Module {
    const full_path = alloc.dupe(u8, path) catch |err| {
        logger.log("Could not allocate file full_path: {s}", .{@errorName(err)}, .err);
        return null;
    };
    defer alloc.free(full_path);

    var arena = std.heap.ArenaAllocator.init(alloc);
    const arena_alloc = arena.allocator();
    var mod = alloc.create(Module) catch |err| {
        logger.log("Could not allocate module: {s}", .{@errorName(err)}, .err);
        return null;
    };
    mod.* = Module{
        .arena = arena,
        .allocator = alloc,
        .entry = undefined,
        .includes = std.StringArrayHashMap(*File).init(arena_alloc),
    };
    const file = File.create(mod, full_path) catch |err| {
        logger.log("Could not create Module File: {s}", .{@errorName(err)}, .err);
        return null;
    };
    mod.entry = file;
    mod.includes.putNoClobber(file.path, file) catch unreachable;

    mod.entry.loadSource() catch |err| {
        logger.log("Could not load file source: {s}", .{@errorName(err)}, .err);
        return null;
    };

    mod.entry.buildTree() catch |err| {
        var output_log: std.Io.Writer.Allocating = .init(mod.allocator);
        defer output_log.deinit();
        const output_writer = &output_log.writer;
        mod.writeErrors(output_writer) catch |e| {
            logger.log("Could not write errors to log message. Something is very wrong. {s}", .{@errorName(e)}, .err);
            return null;
        };
        logger.log("Could not parse file '{s}': {s}\n{s}", .{ mod.entry.path, @errorName(err), output_log.written() }, .err);
        return null;
    };
    return mod;
}

fn createBytecode(mod: *Module, logger: ExportLogger) ?Bytecode {
    var compiler = Compiler.init(mod.allocator, &mod.*) catch |err| {
        logger.log("Could not create compiler: {s}", .{@errorName(err)}, .err);
        return null;
    };
    defer compiler.deinit();

    compiler.compile() catch |err| {
        var output_log: std.Io.Writer.Allocating = .init(mod.allocator);
        defer output_log.deinit();
        const output_writer = &output_log.writer;

        mod.writeErrors(output_writer) catch |e| {
            logger.log("Could not write errors to log message. Something is very wrong. {s}", .{@errorName(e)}, .err);
            return null;
        };
        logger.log("Could not compile file '{s}': {s}\n{s}", .{ mod.entry.path, @errorName(err), output_log.written() }, .err);
        return null;
    };

    return compiler.bytecode() catch |err| {
        logger.log("Could not create bytecode: {s}", .{@errorName(err)}, .err);
        return null;
    };
}

fn writeBytecode(path: []const u8, writer: *std.io.Writer, logger: ExportLogger) usize {
    const mod = createModule(path, logger);
    if (mod == null) return 0;
    defer mod.?.deinit();

    var bytecode = createBytecode(mod.?, logger);
    if (bytecode == null) return 0;

    return bytecode.?.serialize(alloc, writer) catch |err| {
        logger.log("Could not serialize bytecode: {s}", .{@errorName(err)}, .err);
        return 0;
    };
}

/// Start of the vm dialogue
pub export fn start(vm_ptr: usize, path_ptr: [*:0]const u8) callconv(.c) void {
    var vm: *Vm = @ptrFromInt(vm_ptr);
    const runner: *ExportRunner = @fieldParentPtr("runner", vm.runner);
    const logger = runner.logger;
    for (vm.bytecode.constants) |c| {
        if (c != .obj or c.obj.data != .@"extern") continue;
        if (c.obj.data.@"extern".context_ptr == null)
            logger.log("Extern '{s}' has not been set", .{c.obj.data.@"extern".name}, .warn);
    }

    const path = if (path_ptr[0] != 0) std.mem.sliceTo(path_ptr, 0) else for (vm.bytecode.constants) |c| {
        if (c == .obj and c.obj.data == .anchor) break c.obj.data.anchor.name;
    } else {
        logger.log("Topi file does not have a start bough", .{}, .err);
        return;
    };
    vm.start(path) catch |err| logger.log("Could not start vm: {any}", .{err}, .err);
}

pub export fn run(vm_ptr: usize) callconv(.c) void {
    var vm: *Vm = @ptrFromInt(vm_ptr);
    vm.run() catch |err| {
        const runner: *ExportRunner = @fieldParentPtr("runner", vm.runner);
        runner.logger.log("Vm Error: {s}", .{@errorName(err)}, .err);
        if (vm.err.msg) |msg| {
            runner.logger.log("Error at line {}: {s}", .{ vm.err.line, msg }, .err);
        }
    };
}

pub export fn canContinue(vm_ptr: usize) callconv(.c) bool {
    const vm: *Vm = @ptrFromInt(vm_ptr);
    return vm.can_continue;
}

pub export fn isWaiting(vm_ptr: usize) callconv(.c) bool {
    const vm: *Vm = @ptrFromInt(vm_ptr);
    return vm.is_waiting;
}

pub export fn selectContinue(vm_ptr: usize) callconv(.c) void {
    var vm: *Vm = @ptrFromInt(vm_ptr);
    const runner: *ExportRunner = @fieldParentPtr("runner", vm.runner);
    runner.logger.log("Selecting continue", .{}, .debug);
    vm.selectContinue();
}

pub export fn selectChoice(vm_ptr: usize, index: usize) callconv(.c) void {
    var vm: *Vm = @ptrFromInt(vm_ptr);
    const runner: *ExportRunner = @fieldParentPtr("runner", vm.runner);
    runner.logger.log("Selecting choice {}", .{index}, .debug);
    vm.selectChoice(index) catch runner.logger.log("Invalid choice", .{}, .err);
}

pub export fn setExternFunc(vm_ptr: usize, name_ptr: [*:0]const u8, value_ptr: usize, arity: u8, free_ptr: usize) callconv(.c) void {
    var vm: *Vm = @ptrFromInt(vm_ptr);
    const runner: *ExportRunner = @fieldParentPtr("runner", vm.runner);
    const logger = runner.logger;
    const name = std.mem.sliceTo(name_ptr, 0);
    logger.log("Setting extern function \"{s}\"", .{name}, .info);
    const wrapper = alloc.create(ExportFunction) catch |err| {
        logger.log("Could not allocate ExportFunction '{s}': {t}", .{ name, err }, .err);
        return;
    };
    wrapper.* = ExportFunction.create(vm, @ptrFromInt(value_ptr), @ptrFromInt(free_ptr));
    vm.setExtern(name, arity, @intFromPtr(wrapper), ExportFunction.call, ExportFunction.destroy) catch |err| {
        logger.log("Could not set extern fn '{s}': {t}", .{ name, err }, .err);
    };
}

pub export fn subscribe(vm_ptr: usize, name_ptr: [*:0]const u8) callconv(.c) bool {
    var vm: *Vm = @ptrFromInt(vm_ptr);

    const name = std.mem.sliceTo(name_ptr, 0);
    return vm.subscribeToValueChange(name) catch |err| {
        const runner: *ExportRunner = @fieldParentPtr("runner", vm.runner);
        runner.logger.log("Could not subscribe to variable '{s}': {s}", .{ name, @errorName(err) }, .warn);
        return false;
    };
}

pub export fn unsubscribe(vm_ptr: usize, name_ptr: [*:0]const u8) callconv(.c) bool {
    var vm: *Vm = @ptrFromInt(vm_ptr);
    const name = std.mem.sliceTo(name_ptr, 0);
    return vm.unusbscribeToValueChange(name);
}

pub export fn createVm(source_ptr: [*c]const u8, source_len: usize, on_dialogue_ptr: usize, on_choice_ptr: usize, on_value_changed_ptr: usize, log_ptr: usize, log_severity: u8) callconv(.c) usize {
    const on_dialogue: ExportRunner.OnLine = @ptrFromInt(on_dialogue_ptr);
    const on_choices: ExportRunner.OnChoices = @ptrFromInt(on_choice_ptr);
    const on_value_changed: ExportRunner.OnValueChanged = @ptrFromInt(on_value_changed_ptr);
    const logger = ExportLogger{ .on_log = @ptrFromInt(log_ptr), .severity = @enumFromInt(log_severity), .allocator = alloc };

    var fixed = std.Io.Reader.fixed(source_ptr[0..source_len]);
    logger.log("Deserializing bytecode", .{}, .info);
    const bytecode = Bytecode.deserialize(alloc, &fixed) catch |err| {
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

    logger.log("Creating Vm", .{}, .info);
    const vm = alloc.create(Vm) catch {
        logger.log("Could not allocate vm", .{}, .err);
        return 0;
    };
    logger.log("Initializing Vm, globals: {}", .{bytecode.global_symbols.len}, .info);
    vm.* = Vm.init(alloc, bytecode, &extern_runner.runner) catch {
        logger.log("Could not initialize Vm", .{}, .err);
        return 0;
    };
    return @intFromPtr(vm);
}

pub export fn destroyVm(vm_ptr: usize) void {
    var vm: *Vm = @ptrFromInt(vm_ptr);
    const runner: *ExportRunner = @fieldParentPtr("runner", vm.runner);
    runner.logger.log("Destroying Vm", .{}, .info);
    alloc.destroy(runner);
    vm.deinit();
    alloc.destroy(vm);
}

pub export fn calculateStateSize(vm_ptr: usize) usize {
    const vm: *Vm = @ptrFromInt(vm_ptr);
    const runner: *ExportRunner = @fieldParentPtr("runner", vm.runner);
    runner.logger.log("Calculating State size", .{}, .debug);
    return State.calculateSize(vm) catch 0;
}

pub export fn saveStateFile(vm_ptr: usize, path_ptr: [*:0]u8) void {
    const vm: *Vm = @ptrFromInt(vm_ptr);
    const path = std.mem.sliceTo(path_ptr, 0);
    var file = std.fs.openFileAbsolute(path, .{}) catch |err| {
        const runner: *ExportRunner = @fieldParentPtr("runner", vm.runner);
        runner.logger.log("Could not open file: {s}", .{@errorName(err)}, .err);
        return;
    };
    defer file.close();
    var buf: [1024]u8 = undefined;
    var file_writer = file.writer(&buf);
    const writer = &file_writer.interface;
    State.serialize(vm, writer) catch |err| {
        const runner: *ExportRunner = @fieldParentPtr("runner", vm.runner);
        runner.logger.log("Could not serialize state: {s}", .{@errorName(err)}, .err);
    };
}

pub export fn saveState(vm_ptr: usize, out_ptr: [*:0]u8, max: usize) usize {
    const vm: *Vm = @ptrFromInt(vm_ptr);
    var fbs = std.Io.Writer.fixed(out_ptr[0..max]);
    State.serialize(vm, &fbs) catch |err| {
        const runner: *ExportRunner = @fieldParentPtr("runner", vm.runner);
        runner.logger.log("Could not serialize state: {s}", .{@errorName(err)}, .err);
        return 0;
    };
    return fbs.end;
}

pub export fn loadStateFile(vm_ptr: usize, path_ptr: [*:0]u8) void {
    const vm: *Vm = @ptrFromInt(vm_ptr);
    const path = std.mem.sliceTo(path_ptr, 0);
    var file = std.fs.openFileAbsolute(path, .{}) catch |err| {
        const runner: *ExportRunner = @fieldParentPtr("runner", vm.runner);
        runner.logger.log("Could not load file: {s}", .{@errorName(err)}, .err);
        return;
    };
    defer file.close();
    var buf: [1024]u8 = undefined;
    var reader = file.reader(&buf);
    const read = &reader.interface;
    State.deserialize(vm, read) catch |err| {
        const runner: *ExportRunner = @fieldParentPtr("runner", vm.runner);
        runner.logger.log("Could not deserialize data: {s}", .{@errorName(err)}, .err);
    };
}

pub export fn loadState(vm_ptr: usize, json_str: [*]const u8, json_len: usize) void {
    const vm: *Vm = @ptrFromInt(vm_ptr);
    var fbs = std.Io.Reader.fixed(json_str[0..json_len]);
    State.deserialize(vm, &fbs) catch |err| {
        const runner: *ExportRunner = @fieldParentPtr("runner", vm.runner);
        runner.logger.log("Could not deserialize data: {s}", .{@errorName(err)}, .err);
    };
}
