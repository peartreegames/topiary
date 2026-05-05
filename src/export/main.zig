const std = @import("std");
const topi = @import("topi");
const Bytecode = topi.backend.Bytecode;

const Vm = topi.runtime.Vm;
const State = topi.runtime.State;
const Extern = topi.runtime.Extern;

const Module = topi.module.Module;
const File = topi.module.File;
const Formatter = topi.frontend.Formatter;

const export_runner = @import("runner.zig");
const ExportLogger = export_runner.ExportLogger;
const ExportRunner = export_runner.ExportRunner;
const ExportFunction = export_runner.ExportFunction;
const ExportString = export_runner.ExportString;
const ExportLine = export_runner.ExportLine;
const ExportChoice = export_runner.ExportChoice;

var alloc = std.heap.smp_allocator;
const io = std.Io.Threaded.global_single_threaded.io();

/// Override the default panic handler so the host can log a final message
/// before the process aborts. After invoking the registered handler (if any),
/// chains to `defaultPanic` which dumps a trace and aborts.
pub const Panic = std.debug.FullPanic(panicImpl);

const PanicHandler = *const fn (msg_ptr: [*]const u8, msg_len: usize) callconv(.c) void;
var registered_panic: ?PanicHandler = null;

fn panicImpl(msg: []const u8, first_trace_addr: ?usize) noreturn {
    if (registered_panic) |h| h(msg.ptr, msg.len);
    std.debug.defaultPanic(msg, first_trace_addr);
}

/// Register a process-wide panic handler. Pass null to clear.
/// The handler runs in undefined-state context: it must not allocate,
/// must not re-enter Topi, and should do the minimum (log + return).
pub export fn setPanicHandler(handler_ptr: ?*const anyopaque) void {
    registered_panic = if (handler_ptr) |p| @ptrCast(@alignCast(p)) else null;
}

/// Used to pre-calculate the size required
/// for a compiled topi module
pub export fn calculateCompileSize(path_ptr: [*:0]const u8, log_ptr: *const anyopaque, log_severity: u8) usize {
    const logger = ExportLogger{
        .on_log = @ptrCast(@alignCast(log_ptr)),
        .severity = @enumFromInt(log_severity),
        .allocator = alloc,
    };

    var buf: [1024]u8 = undefined;
    var counter = std.Io.Writer.Discarding.init(&buf);
    const count = writeBytecode(std.mem.sliceTo(path_ptr, 0), &counter.writer, logger);
    if (count > 0) logger.log("Calculated Compile size {d}", .{count}, .debug);
    return count;
}

/// Compiles the given path to the byte array
/// Can use `calculateCompileSize` to get the max required, or pass a larger than expected size
pub export fn compile(path_ptr: [*:0]const u8, out_ptr: [*]u8, max: usize, log_ptr: *const anyopaque, log_severity: u8) usize {
    const logger = ExportLogger{
        .on_log = @ptrCast(@alignCast(log_ptr)),
        .severity = @enumFromInt(log_severity),
        .allocator = alloc,
    };
    var fixed = std.Io.Writer.fixed(out_ptr[0..max]);
    const size = writeBytecode(std.mem.sliceTo(path_ptr, 0), &fixed, logger);
    logger.log("Compiled size {d} / {d}", .{ size, max }, .debug);
    return size;
}

fn createModule(path: []const u8, logger: ExportLogger, allocator: std.mem.Allocator) ?*Module {
    var mod = Module.init(allocator, io, path) catch |err| {
        logger.log("Could not allocate module: {s}", .{@errorName(err)}, .err);
        return null;
    };

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
        logger.log("Could not read .topi file '{s}': {s}\n{s}", .{ mod.entry.path, @errorName(err), output_log.written() }, .err);
        return null;
    };
    return mod;
}

fn createBytecode(mod: *Module, logger: ExportLogger, allocator: std.mem.Allocator) ?Bytecode {
    return mod.generateBytecode(allocator) catch |err| {
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
}

// Compile-then-discard: arena batches all transient allocations
// (module, parser, lowering, codegen, Value.Obj constants, bytecode
// arrays). Only the serialized bytes escape via the writer.
fn writeBytecode(path: []const u8, writer: *std.Io.Writer, logger: ExportLogger) usize {
    var arena = std.heap.ArenaAllocator.init(alloc);
    defer arena.deinit();
    const aa = arena.allocator();

    const mod = createModule(path, logger, aa) orelse return 0;
    var bytecode = createBytecode(mod, logger, aa) orelse return 0;

    return bytecode.serialize(aa, writer) catch |err| {
        logger.log("Could not write compiled bytecode: {s}", .{@errorName(err)}, .err);
        return 0;
    };
}

/// Start of the vm dialogue
pub export fn start(vm_ptr: *anyopaque, path_ptr: [*:0]const u8) void {
    var vm: *Vm = @ptrCast(@alignCast(vm_ptr));
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

pub export fn run(vm_ptr: *anyopaque) void {
    var vm: *Vm = @ptrCast(@alignCast(vm_ptr));
    vm.run() catch {
        const runner: *ExportRunner = @fieldParentPtr("runner", vm.runner);
        var printer = std.Io.Writer.Allocating.init(alloc);
        defer printer.deinit();
        vm.err.print(&printer.writer);
        runner.logger.log("{s}", .{printer.written()}, .err);
    };
}

pub export fn canContinue(vm_ptr: *anyopaque) bool {
    const vm: *Vm = @ptrCast(@alignCast(vm_ptr));
    return vm.can_continue;
}

pub export fn isWaiting(vm_ptr: *anyopaque) bool {
    const vm: *Vm = @ptrCast(@alignCast(vm_ptr));
    return vm.is_waiting;
}

pub export fn selectContinue(vm_ptr: *anyopaque) void {
    var vm: *Vm = @ptrCast(@alignCast(vm_ptr));
    const runner: *ExportRunner = @fieldParentPtr("runner", vm.runner);
    runner.logger.log("Selecting continue", .{}, .debug);
    vm.selectContinue();
}

pub export fn selectChoice(vm_ptr: *anyopaque, index: usize) void {
    var vm: *Vm = @ptrCast(@alignCast(vm_ptr));
    const runner: *ExportRunner = @fieldParentPtr("runner", vm.runner);
    runner.logger.log("Selecting choice {}", .{index}, .debug);
    vm.selectChoice(index) catch runner.logger.log("Invalid choice", .{}, .err);
}

pub export fn setExternFunc(vm_ptr: *anyopaque, name_ptr: [*:0]const u8, value_ptr: *const anyopaque, arity: u8, user_data: usize, free_ptr: *const anyopaque) void {
    var vm: *Vm = @ptrCast(@alignCast(vm_ptr));
    const runner: *ExportRunner = @fieldParentPtr("runner", vm.runner);
    const logger = runner.logger;
    const name = std.mem.sliceTo(name_ptr, 0);
    logger.log("Setting extern function \"{s}\"", .{name}, .info);
    const wrapper = vm.alloc.create(ExportFunction) catch |err| {
        logger.log("Could not allocate ExportFunction '{s}': {s}", .{ name, @errorName(err) }, .err);
        return;
    };
    wrapper.* = ExportFunction.create(vm, @ptrCast(@alignCast(value_ptr)), user_data, @ptrCast(@alignCast(free_ptr)));
    vm.setExtern(name, arity, wrapper, ExportFunction.call, ExportFunction.destroy) catch |err| {
        logger.log("Could not set extern fn '{s}': {s}", .{ name, @errorName(err) }, .err);
        vm.alloc.destroy(wrapper);
    };
}

pub export fn subscribe(vm_ptr: *anyopaque, name_ptr: [*:0]const u8) bool {
    var vm: *Vm = @ptrCast(@alignCast(vm_ptr));

    const name = std.mem.sliceTo(name_ptr, 0);
    return vm.subscribeToValueChange(name) catch |err| {
        const runner: *ExportRunner = @fieldParentPtr("runner", vm.runner);
        runner.logger.log("Could not subscribe to variable '{s}': {s}", .{ name, @errorName(err) }, .warn);
        return false;
    };
}

pub export fn unsubscribe(vm_ptr: *anyopaque, name_ptr: [*:0]const u8) bool {
    var vm: *Vm = @ptrCast(@alignCast(vm_ptr));
    const name = std.mem.sliceTo(name_ptr, 0);
    return vm.unsubscribeToValueChange(name);
}

/// Creates a VM from serialized bytecode. Returns null on failure.
/// The returned VM is single-threaded: all export functions must be called from
/// the same thread. String pointers in callbacks (onLine, onChoices, onValueChanged)
/// are valid only for the duration of the callback — the host must copy them to persist.
pub export fn createVm(
    source_ptr: [*]const u8,
    source_len: usize,
    on_dialogue_ptr: *const anyopaque,
    on_choice_ptr: *const anyopaque,
    on_value_changed_ptr: *const anyopaque,
    log_ptr: *const anyopaque,
    log_severity: u8,
)?*anyopaque {
    const on_dialogue: ExportRunner.OnLine = @ptrCast(@alignCast(on_dialogue_ptr));
    const on_choices: ExportRunner.OnChoices = @ptrCast(@alignCast(on_choice_ptr));
    const on_value_changed: ExportRunner.OnValueChanged = @ptrCast(@alignCast(on_value_changed_ptr));
    const logger = ExportLogger{ .on_log = @ptrCast(@alignCast(log_ptr)), .severity = @enumFromInt(log_severity), .allocator = alloc };

    var fixed = std.Io.Reader.fixed(source_ptr[0..source_len]);
    logger.log("Deserializing bytecode", .{}, .info);
    const bytecode = Bytecode.deserialize(alloc, &fixed) catch |err| {
        logger.log("Could not deserialize bytecode: {s}", .{@errorName(err)}, .err);
        return null;
    };

    logger.log("Creating ExportRunner", .{}, .info);
    const extern_runner = alloc.create(ExportRunner) catch {
        logger.log("Could not allocate runner", .{}, .err);
        bytecode.free(alloc);
        return null;
    };
    logger.log("Initializing ExportRunner", .{}, .info);
    extern_runner.* = ExportRunner.init(alloc, on_dialogue, on_choices, on_value_changed, logger);

    logger.log("Creating Vm", .{}, .info);
    const vm = alloc.create(Vm) catch {
        logger.log("Could not allocate vm", .{}, .err);
        alloc.destroy(extern_runner);
        bytecode.free(alloc);
        return null;
    };
    logger.log("Starting dialogue runtime, globals: {}", .{bytecode.global_symbols.len}, .info);
    vm.* = Vm.init(alloc, io, &bytecode, &extern_runner.runner) catch {
        logger.log("Could not start the dialogue runtime", .{}, .err);
        alloc.destroy(vm);
        alloc.destroy(extern_runner);
        bytecode.free(alloc);
        return null;
    };
    return @ptrCast(vm);
}

/// Destroys the VM and frees all associated memory. Safe to call at any point,
/// including while the VM is paused mid-dialogue waiting for input.
pub export fn destroyVm(vm_ptr: *anyopaque) void {
    var vm: *Vm = @ptrCast(@alignCast(vm_ptr));
    const runner: *ExportRunner = @fieldParentPtr("runner", vm.runner);
    runner.logger.log("Destroying Vm", .{}, .info);
    vm.deinit();
    alloc.destroy(runner);
    alloc.destroy(vm);
}

pub export fn calculateStateSize(vm_ptr: *anyopaque) usize {
    const vm: *Vm = @ptrCast(@alignCast(vm_ptr));
    const runner: *ExportRunner = @fieldParentPtr("runner", vm.runner);
    runner.logger.log("Calculating State size", .{}, .debug);
    return State.calculateSize(vm) catch 0;
}

pub export fn saveStateFile(vm_ptr: *anyopaque, path_ptr: [*:0]const u8) bool {
    const vm: *Vm = @ptrCast(@alignCast(vm_ptr));
    const path = std.mem.sliceTo(path_ptr, 0);
    var file = std.Io.Dir.createFileAbsolute(io, path, .{}) catch |err| {
        const runner: *ExportRunner = @fieldParentPtr("runner", vm.runner);
        runner.logger.log("Could not create file: {s}", .{@errorName(err)}, .err);
        return false;
    };
    defer file.close(io);
    var buf: [1024]u8 = undefined;
    var file_writer = file.writer(io, &buf);
    const writer = &file_writer.interface;
    State.serialize(vm, writer) catch |err| {
        const runner: *ExportRunner = @fieldParentPtr("runner", vm.runner);
        runner.logger.log("Could not serialize state: {s}", .{@errorName(err)}, .err);
        return false;
    };
    return true;
}

pub export fn saveState(vm_ptr: *anyopaque, out_ptr: [*]u8, max: usize) usize {
    const vm: *Vm = @ptrCast(@alignCast(vm_ptr));
    var fbs = std.Io.Writer.fixed(out_ptr[0..max]);
    State.serialize(vm, &fbs) catch |err| {
        const runner: *ExportRunner = @fieldParentPtr("runner", vm.runner);
        runner.logger.log("Could not serialize state: {s}", .{@errorName(err)}, .err);
        return 0;
    };
    return fbs.end;
}

pub export fn loadStateFile(vm_ptr: *anyopaque, path_ptr: [*:0]const u8) bool {
    const vm: *Vm = @ptrCast(@alignCast(vm_ptr));
    const path = std.mem.sliceTo(path_ptr, 0);
    var file = std.Io.Dir.openFileAbsolute(io, path, .{}) catch |err| {
        const runner: *ExportRunner = @fieldParentPtr("runner", vm.runner);
        runner.logger.log("Could not load file: {s}", .{@errorName(err)}, .err);
        return false;
    };
    defer file.close(io);
    var buf: [1024]u8 = undefined;
    var reader = file.reader(io, &buf);
    const read = &reader.interface;
    State.deserialize(vm, read) catch |err| {
        const runner: *ExportRunner = @fieldParentPtr("runner", vm.runner);
        runner.logger.log("Could not deserialize data: {s}", .{@errorName(err)}, .err);
        return false;
    };
    return true;
}

pub export fn loadState(vm_ptr: *anyopaque, json_str: [*]const u8, json_len: usize) bool {
    const vm: *Vm = @ptrCast(@alignCast(vm_ptr));
    var fbs = std.Io.Reader.fixed(json_str[0..json_len]);
    State.deserialize(vm, &fbs) catch |err| {
        const runner: *ExportRunner = @fieldParentPtr("runner", vm.runner);
        runner.logger.log("Could not deserialize data: {s}", .{@errorName(err)}, .err);
        return false;
    };
    return true;
}

pub export fn setLocaleFile(vm_ptr: *anyopaque, path_ptr: [*:0]const u8) bool {
    var vm: *Vm = @ptrCast(@alignCast(vm_ptr));
    const path = std.mem.sliceTo(path_ptr, 0);
    vm.setLocale(path) catch |err| {
        const runner: *ExportRunner = @fieldParentPtr("runner", vm.runner);
        runner.logger.log("Could not set locale '{s}': {s}", .{ path, @errorName(err) }, .err);
        return false;
    };
    return true;
}

pub export fn setLocale(vm_ptr: *anyopaque, data_ptr: [*]const u8, data_len: usize) bool {
    var vm: *Vm = @ptrCast(@alignCast(vm_ptr));
    vm.setLocaleFromBuffer("mem", data_ptr[0..data_len]) catch |err| {
        const runner: *ExportRunner = @fieldParentPtr("runner", vm.runner);
        runner.logger.log("Could not set locale from buffer: {s}", .{@errorName(err)}, .err);
        return false;
    };
    return true;
}

pub export fn removeLocale(vm_ptr: *anyopaque) void {
    var vm: *Vm = @ptrCast(@alignCast(vm_ptr));
    vm.removeLocale();
}

pub export fn calculateFormatSize(path_ptr: [*:0]const u8, indent_width: u8, log_ptr: *const anyopaque, log_severity: u8) usize {
    const logger = ExportLogger{
        .on_log = @ptrCast(@alignCast(log_ptr)),
        .severity = @enumFromInt(log_severity),
        .allocator = alloc,
    };
    const result = formatInternal(std.mem.sliceTo(path_ptr, 0), if (indent_width == 0) 4 else indent_width, logger);
    if (result) |formatted| {
        defer alloc.free(formatted);
        return formatted.len;
    } else return 0;
}

pub export fn format(path_ptr: [*:0]const u8, out_ptr: [*]u8, max: usize, indent_width: u8, log_ptr: *const anyopaque, log_severity: u8) usize {
    const logger = ExportLogger{
        .on_log = @ptrCast(@alignCast(log_ptr)),
        .severity = @enumFromInt(log_severity),
        .allocator = alloc,
    };
    const result = formatInternal(std.mem.sliceTo(path_ptr, 0), if (indent_width == 0) 4 else indent_width, logger);
    if (result) |formatted| {
        defer alloc.free(formatted);
        const len = @min(formatted.len, max);
        @memcpy(out_ptr[0..len], formatted[0..len]);
        return len;
    } else return 0;
}

fn formatInternal(path: []const u8, indent_width: u8, logger: ExportLogger) ?[]const u8 {
    var mod = Module.init(alloc, io, path) catch |err| {
        logger.log("Could not create module: {s}", .{@errorName(err)}, .err);
        return null;
    };
    defer mod.deinit();

    mod.resolveIncludes() catch |err| {
        logger.log("Could not resolve includes: {s}", .{@errorName(err)}, .err);
        return null;
    };

    mod.entry.buildTree() catch |err| {
        logger.log("Could not read .topi file: {s}", .{@errorName(err)}, .err);
        return null;
    };

    const source = mod.entry.source orelse return null;
    const tree = mod.entry.tree orelse return null;
    return Formatter.format(source, tree, alloc, indent_width, false) catch |err| {
        logger.log("Could not format: {s}", .{@errorName(err)}, .err);
        return null;
    };
}
