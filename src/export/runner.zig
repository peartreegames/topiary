const std = @import("std");

const topi = @import("topi");
const Vm = topi.runtime.Vm;
const Runner = topi.runtime.Runner;
const Line = topi.runtime.Line;
const Choice = topi.runtime.Choice;

const Value = topi.types.Value;
const Nil = topi.types.Nil;

const ExportValue = @import("value.zig").ExportValue;
pub const ExportFunctionDelegate = *const fn (args: []Value) Value;

pub const ExportString = extern struct {
    ptr: [*c]const u8,
    len: usize,
};

pub const ExportLine = extern struct {
    content: ExportString,
    speaker: ExportString,
    tags: [*c]ExportString,
    tags_length: u8,
};

pub const ExportChoice = extern struct {
    content: ExportString,
    tags: [*c]ExportString,
    tags_length: u8,
    visit_count: u32,
    ip: u32,
};

pub const ExportLogger = struct {
    on_log: OnLog,
    severity: Severity,
    allocator: std.mem.Allocator,

    pub const OnLog = *const fn (msg: ExportString, severity: Severity) callconv(.C) void;
    pub const Severity = enum(u8) {
        debug,
        info,
        warn,
        err,
    };

    pub fn log(self: ExportLogger, comptime msg: []const u8, args: anytype, severity: Severity) void {
        if (@intFromEnum(severity) < @intFromEnum(self.severity)) return;
        const fmt = std.fmt.allocPrint(self.allocator, msg, args) catch |err| {
            std.log.err("Error fmt: {}", .{err});
            self.on_log(.{ .ptr = msg.ptr, .len = msg.len }, severity);
            return;
        };
        defer self.allocator.free(fmt);
        self.on_log(.{ .ptr = fmt.ptr, .len = fmt.len }, severity);
    }
};

pub const ExportFunction = struct {
    func: Delegate,
    free: Free,
    vm: *Vm,

    pub const Free = *const fn (ptr: usize) callconv(.C) void;
    pub const Delegate = *const fn (vm_ptr: usize, args: [*c]ExportValue, args_len: u8) callconv(.C) ExportValue;

    pub fn create(vm: *Vm, func: Delegate, free: Free) ExportFunction {
        return .{
            .func = func,
            .free = free,
            .vm = vm,
        };
    }

    pub fn call(context_ptr: usize, args: []Value) Value {
        var self: *ExportFunction = @ptrFromInt(context_ptr);
        var arena = std.heap.ArenaAllocator.init(self.vm.allocator);
        const arenaAlloc = arena.allocator();
        defer arena.deinit();
        const runner: *ExportRunner = @fieldParentPtr("runner", self.vm.runner);
        const logger = runner.logger;
        var exp_args = arenaAlloc.alloc(ExportValue, args.len) catch |err| {
            logger.log("Could not allocate args: {s}", .{@errorName(err)}, .err);
            return Nil;
        };
        var i: usize = 0;
        while (i < args.len) : (i += 1) {
            exp_args[i] = ExportValue.fromValue(args[i], arenaAlloc);
        }
        logger.log("Calling ExportFunction", .{}, .info);
        var v = self.func(@intFromPtr(self.vm), exp_args.ptr, @intCast(exp_args.len));
        return v.toValue(self.vm, self.free) catch |err| {
            logger.log("Could not return value of extern function, returning null: {s}", .{@errorName(err)}, .err);
            return Nil;
        };
    }
};

pub const ExportRunner = struct {
    allocator: std.mem.Allocator,
    on_line: OnLine,
    on_choices: OnChoices,
    on_value_changed: OnValueChanged,
    logger: ExportLogger,

    runner: Runner,
    tags: [512]ExportString,
    dialogue: ExportLine = undefined,

    pub const OnValueChanged = *const fn (vm_ptr: usize, name_ptr: [*c]const u8, name_len: usize, value: ExportValue) callconv(.C) void;
    pub const OnLine = *const fn (vm_ptr: usize, dialogue: *ExportLine) callconv(.C) void;
    pub const OnChoices = *const fn (vm_ptr: usize, choices: [*]ExportChoice, choices_len: u8) callconv(.C) void;

    pub fn init(allocator: std.mem.Allocator, on_line: OnLine, on_choices: OnChoices, on_value_changed: OnValueChanged, logger: ExportLogger) ExportRunner {
        return .{
            .allocator = allocator,
            .on_line = on_line,
            .on_choices = on_choices,
            .on_value_changed = on_value_changed,
            .logger = logger,
            .tags = [_]ExportString{.{ .ptr = 0, .len = 0 }} ** 512,
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
            const tag = dialogue.tags[i];
            self.tags[i] = .{ .ptr = tag.ptr, .len = tag.len };
        }

        self.dialogue = .{
            .content = .{ .ptr = dialogue.content.ptr, .len = dialogue.content.len },
            .speaker = if (dialogue.speaker) |s| .{ .ptr = s.ptr, .len = s.len } else .{ .ptr = "".ptr, .len = 0 },
            .tags = &self.tags,
            .tags_length = @intCast(dialogue.tags.len),
        };
        self.logger.log("Line:{s}: {s}", .{ dialogue.speaker orelse "", dialogue.content }, .debug);
        self.on_line(@intFromPtr(vm), &self.dialogue);
    }

    pub fn onChoices(runner: *Runner, vm: *Vm, choices: []Choice) void {
        var self: *ExportRunner = @fieldParentPtr("runner", runner);
        var i: usize = 0;
        var result = self.allocator.alloc(ExportChoice, choices.len) catch {
            self.logger.log("Could not allocate choices", .{}, .err);
            return;
        };
        var t_count: usize = 0;
        while (i < choices.len) : (i += 1) {
            var t: usize = 0;
            const t_start = t_count;
            while (t < choices[i].tags.len) : (t += 1) {
                const tag = choices[i].tags[t];
                self.tags[t_count + t] = .{ .ptr = tag.ptr, .len = tag.len };
            }
            t_count += t;

            self.logger.log("Choice: {s}", .{choices[i].content}, .debug);
            const content = choices[i].content;
            result[i] = .{
                .content = .{ .ptr = content.ptr, .len = content.len },
                .tags = self.tags[t_start..t_count].ptr,
                .tags_length = @intCast(choices[i].tags.len),
                .visit_count = @intCast(choices[i].visit_count),
                .ip = choices[i].ip,
            };
        }

        self.on_choices(@intFromPtr(vm), result.ptr, @intCast(result.len));
        self.allocator.free(result);
    }

    pub fn onValueChanged(runner: *Runner, vm: *Vm, name: []const u8, value: Value) void {
        var self: *ExportRunner = @fieldParentPtr("runner", runner);
        const exp_value = ExportValue.fromValue(value, vm.allocator);
        defer exp_value.deinit(vm.allocator);
        self.on_value_changed(@intFromPtr(vm), name.ptr, name.len, exp_value);
    }
};
