const std = @import("std");
const topi = @import("topi");
const Compiler = topi.backend.Compiler;
const CompilerErrors = topi.backend.CompilerErrors;
const Bytecode = topi.backend.Bytecode;

const Vm = topi.runtime.Vm;
const State = topi.runtime.State;

const Module = topi.module.Module;
const File = topi.module.File;

const ExportValue = @import("value.zig").ExportValue;
const export_runner = @import("runner.zig");
const ExportLogger = export_runner.ExportLogger;
const ExportRunner = export_runner.ExportRunner;
const ExportFunction = export_runner.ExportFunction;
const ExportString = export_runner.ExportString;
const ExportLine = export_runner.ExportLine;
const ExportChoice = export_runner.ExportChoice;

var gpa = std.heap.GeneralPurposeAllocator(.{}){};
var alloc = gpa.allocator();

/// Used to pre-calculate the size required
/// for a compiled topi module
pub export fn calculateCompileSize(path_ptr: [*:0]const u8, log_ptr: usize, log_severity: u8) callconv(.C) usize {
    const logger = ExportLogger{ .on_log = @ptrFromInt(log_ptr), .severity = @enumFromInt(log_severity), .allocator = alloc };
    var counter = std.io.countingWriter(std.io.null_writer);
    writeBytecode(std.mem.sliceTo(path_ptr, 0), &counter, logger);
    logger.log("Calculated Compile size {d}", .{counter.bytes_written}, .debug);
    return counter.bytes_written;
}

/// Compiles the given path to the byte array
/// Can use `calculateCompileSize` to get the max required, or pass a larger than expected size
pub export fn compile(path_ptr: [*:0]const u8, out_ptr: [*]u8, max: usize, log_ptr: usize, log_severity: u8) callconv(.C) usize {
    const logger = ExportLogger{ .on_log = @ptrFromInt(log_ptr), .severity = @enumFromInt(log_severity), .allocator = alloc };
    var fbs = std.io.fixedBufferStream(out_ptr[0..max]);
    writeBytecode(std.mem.sliceTo(path_ptr, 0), &fbs, logger);
    logger.log("Compiled size {d} / {d}", .{ fbs.pos, max }, .debug);
    return fbs.pos;
}

fn writeBytecode(path: []const u8, seekable: anytype, logger: ExportLogger) void {
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

    bytecode.serialize(seekable) catch |err| {
        logger.log("Could not serialize bytecode: {s}", .{@errorName(err)}, .err);
        return;
    };
}

/// Start of the vm dialogue
pub export fn start(vm_ptr: usize, path_ptr: [*:0]const u8) callconv(.C) void {
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

pub export fn run(vm_ptr: usize) callconv(.C) void {
    var vm: *Vm = @ptrFromInt(vm_ptr);
    vm.run() catch |err| {
        const runner: *ExportRunner = @fieldParentPtr("runner", vm.runner);
        runner.logger.log("Vm Error: {s}", .{@errorName(err)}, .err);
        if (vm.err.msg) |msg| {
            runner.logger.log("Error at line {}: {s}", .{ vm.err.line, msg }, .err);
        }
    };
}

pub export fn canContinue(vm_ptr: usize) callconv(.C) bool {
    const vm: *Vm = @ptrFromInt(vm_ptr);
    return vm.can_continue;
}

pub export fn isWaiting(vm_ptr: usize) callconv(.C) bool {
    const vm: *Vm = @ptrFromInt(vm_ptr);
    return vm.is_waiting;
}

pub export fn selectContinue(vm_ptr: usize) callconv(.C) void {
    var vm: *Vm = @ptrFromInt(vm_ptr);
    const runner: *ExportRunner = @fieldParentPtr("runner", vm.runner);
    runner.logger.log("Selecting continue", .{}, .debug);
    vm.selectContinue();
}

pub export fn selectChoice(vm_ptr: usize, index: usize) callconv(.C) void {
    var vm: *Vm = @ptrFromInt(vm_ptr);
    const runner: *ExportRunner = @fieldParentPtr("runner", vm.runner);
    runner.logger.log("Selecting choice {}", .{index}, .debug);
    vm.selectChoice(index) catch runner.logger.log("Invalid choice", .{}, .err);
}

pub export fn setExtern(vm_ptr: usize, name_ptr: [*:0]const u8, exp_value: ExportValue, free_ptr: usize) callconv(.C) void {
    var vm: *Vm = @ptrFromInt(vm_ptr);
    const runner: *ExportRunner = @fieldParentPtr("runner", vm.runner);
    const logger = runner.logger;
    const name = std.mem.sliceTo(name_ptr, 0);
    const value = exp_value.toValue(vm, @ptrFromInt(free_ptr)) catch |err| {
        logger.log("Could not create Value \"{s}\": {s}", .{ name, @errorName(err) }, .err);
        return;
    };
    logger.log("Setting {s} to {s}", .{ name, value }, .debug);

    vm.setExtern(name, value) catch |err| {
        logger.log("Could not set Export value \"{s}\": {s}", .{ name, @errorName(err) }, .err);
    };
}

pub export fn setExternFunc(vm_ptr: usize, name_ptr: [*:0]const u8, value_ptr: usize, arity: u8, free_ptr: usize) callconv(.C) void {
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
    const val = vm.gc.create(vm, .{ .ext_function = .{
        .arity = arity,
        .backing = ExportFunction.call,
        .context_ptr = @intFromPtr(wrapper),
        .destroy = ExportFunction.destroy,
    } }) catch |err| {
        logger.log("Could not create function value '{s}': {s}", .{ name, @errorName(err) }, .err);
        return;
    };
    vm.setExtern(name, val) catch |err| {
        logger.log("Could not set Export value '{s}': {s}", .{ name, @errorName(err) }, .err);
    };
}

pub export fn subscribe(vm_ptr: usize, name_ptr: [*:0]const u8) callconv(.C) bool {
    var vm: *Vm = @ptrFromInt(vm_ptr);

    const name = std.mem.sliceTo(name_ptr, 0);
    return vm.subscribeToValueChange(name) catch |err| {
        const runner: *ExportRunner = @fieldParentPtr("runner", vm.runner);
        runner.logger.log("Could not subscribe to variable '{s}': {s}", .{ name, @errorName(err) }, .warn);
        return false;
    };
}

pub export fn unsubscribe(vm_ptr: usize, name_ptr: [*:0]const u8) callconv(.C) bool {
    var vm: *Vm = @ptrFromInt(vm_ptr);
    const name = std.mem.sliceTo(name_ptr, 0);
    return vm.unusbscribeToValueChange(name);
}

pub export fn createVm(source_ptr: [*c]const u8, source_len: usize, on_dialogue_ptr: usize, on_choice_ptr: usize, on_value_changed_ptr: usize, log_ptr: usize, log_severity: u8) callconv(.C) usize {
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

pub export fn destroyVm(vm_ptr: usize) void {
    var vm: *Vm = @ptrFromInt(vm_ptr);
    const runner: *ExportRunner = @fieldParentPtr("runner", vm.runner);
    runner.logger.log("Destroying VM", .{}, .info);
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
    State.serialize(vm, file.writer()) catch |err| {
        const runner: *ExportRunner = @fieldParentPtr("runner", vm.runner);
        runner.logger.log("Could not serialize state: {s}", .{@errorName(err)}, .err);
    };
}

pub export fn saveState(vm_ptr: usize, out_ptr: [*:0]u8, max: usize) usize {
    const vm: *Vm = @ptrFromInt(vm_ptr);
    var fbs = std.io.fixedBufferStream(out_ptr[0..max]);
    State.serialize(vm, fbs.writer()) catch |err| {
        const runner: *ExportRunner = @fieldParentPtr("runner", vm.runner);
        runner.logger.log("Could not serialize state: {s}", .{@errorName(err)}, .err);
        return 0;
    };
    return fbs.pos;
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
    State.deserialize(vm, file.reader()) catch |err| {
        const runner: *ExportRunner = @fieldParentPtr("runner", vm.runner);
        runner.logger.log("Could not deserialize data: {s}", .{@errorName(err)}, .err);
    };
}

pub export fn loadState(vm_ptr: usize, json_str: [*]const u8, json_len: usize) void {
    const vm: *Vm = @ptrFromInt(vm_ptr);
    var fbs = std.io.fixedBufferStream(json_str[0..json_len]);
    State.deserialize(vm, fbs.reader()) catch |err| {
        const runner: *ExportRunner = @fieldParentPtr("runner", vm.runner);
        runner.logger.log("Could not deserialize data: {s}", .{@errorName(err)}, .err);
    };
}
