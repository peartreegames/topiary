const std = @import("std");
const Vm = @import("vm.zig").Vm;
const Scope = @import("scope.zig").Scope;
const Compiler = @import("compiler.zig").Compiler;
const Errors = @import("compiler-error.zig").CompilerErrors;
const module = @import("module.zig");
const runners = @import("runner.zig");

const Runner = runners.Runner;
const Dialogue = runners.Dialogue;
const Choice = runners.Choice;

const File = module.File;
const Module = module.Module;

fn usage(comptime msg: []const u8) !void {
    var out = std.io.getStdErr().writer();
    if (!std.mem.eql(u8, msg, "")) {
        try out.print(msg, .{});
        try out.print("\n", .{});
    }
    try out.print("topi - command line topiary processor\n", .{});
    try out.print("Usage:\n", .{});
    try out.print("        topi [-v | --version] [-h | --help] <command> <file> [flags]\n", .{});
    try out.print("\n", .{});
    try out.print("Commands:\n", .{});
    try out.print("        topi run <file> [start_bough] [--verbose]\n", .{});
    try out.print("        topi auto <file> <count> [-verbose]\n", .{});
    try out.print("        topi compile <file> <output_file|--dry|-d> [--verbose]\n", .{});
    try out.print("\n", .{});
    try out.print("Flags:\n", .{});
    try out.print("        --version, -v: Output current version\n", .{});
    try out.print("        --verbose: Output debug logs\n", .{});
    try out.print("        --dry, -d: Compile only\n", .{});
}

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    var args = try std.process.argsWithAllocator(arena.allocator());
    _ = args.skip();

    const maybe_cmd = args.next();
    if (maybe_cmd == null) return usage("");
    const cmd = maybe_cmd.?;

    const is_version = std.mem.eql(u8, cmd, "-v") or std.mem.eql(u8, cmd, "--version");
    if (is_version) return usage("");
    const is_help = std.mem.eql(u8, cmd, "-h") or std.mem.eql(u8, cmd, "--help");
    if (is_help) return usage("");

    const is_run = std.mem.eql(u8, cmd, "run");
    const is_auto = std.mem.eql(u8, cmd, "auto");
    const is_compile = std.mem.eql(u8, cmd, "compile");
    if (!is_run and !is_auto and !is_compile) return usage("Unknown command");

    const maybe_file_path = args.next();
    if (maybe_file_path == null) {
        return usage("No file argument provided.\n");
    }
    const file_path = maybe_file_path.?;

    var out_path: ?[]const u8 = null;
    var bough_path: ?[]const u8 = null;
    var auto_count: usize = 0;
    var is_dry = false;
    var is_verbose = false;

    if (is_run) {
        bough_path = args.next();
        if (bough_path) |b| {
            if (std.mem.eql(u8, b, "--verbose")) {
                bough_path = null;
                is_verbose = true;
            }
        }
    }
    if (is_compile) {
        out_path = args.next();
        if (out_path == null) return usage("Compile requires an output path or --dry flag.");
        is_dry = std.mem.eql(u8, out_path.?, "--dry") or std.mem.eql(u8, out_path.?, "-d");
    }

    if (is_auto) {
        const maybe_count = args.next();
        if (maybe_count == null) return usage("Auto requires a play count.\n");
        auto_count = std.fmt.parseInt(u64, maybe_count.?, 10) catch {
            return usage("Invalid auto count specified");
        };
    }
    const flag = args.next();
    if (flag) |f| {
        if (std.mem.eql(u8, f, "--verbose")) {
            is_verbose = true;
        }
    }

    const allocator = arena.allocator();
    const full_path = std.fs.cwd().realpathAlloc(allocator, file_path) catch |err| {
        try std.io.getStdErr().writer().print("Could not find file at {s}", .{file_path});
        if (is_verbose) return err;
        return;
    };
    var mod = Module.init(allocator, full_path) catch |err| {
        if (is_verbose) return err;
        return;
    };
    var bytecode = mod.generateBytecode() catch |err| {
        if (is_verbose) return err;
        return;
    };
    defer bytecode.free(allocator);
    mod.deinit();

    if (is_dry) {
        var out = std.io.getStdOut().writer();
        try out.writeAll("Success");
        return;
    }

    if (is_compile) {
        const file = try std.fs.cwd().createFile(out_path.?, .{});
        defer file.close();
        const writer = file.writer();
        bytecode.serialize(writer) catch |err| {
            if (is_verbose) return err;
            return;
        };
        return;
    }

    const vm_alloc = arena.allocator();
    if (is_auto) {
        var i: usize = 0;
        var visit_counts = std.StringArrayHashMap(u64).init(vm_alloc);
        defer visit_counts.deinit();
        while (i < auto_count) : (i += 1) {
            var auto_runner = AutoTestRunner.init();
            var vm = try Vm.init(vm_alloc, bytecode, &auto_runner.runner);
            vm.interpret() catch {
                vm.err.print(std.io.getStdErr().writer());
                continue;
            };
            defer vm.deinit();
            for (vm.globals, 0..) |g, idx| {
                if (g == .visit) {
                    const name = bytecode.global_symbols[idx].name;
                    const cur = try visit_counts.getOrPutValue(name, 0);
                    try visit_counts.put(name, cur.value_ptr.* + g.visit);
                } else break;
                // all visits are first so we can break
            }
        }
        var it = visit_counts.iterator();
        while (it.next()) |entry| {
            std.debug.print("{s} = {}\n", .{ entry.key_ptr.*, entry.value_ptr.* });
        }
        return;
    }

    var cli_runner = CliRunner.init();
    var vm = Vm.init(vm_alloc, bytecode, &cli_runner.runner) catch |err| {
        try std.io.getStdErr().writeAll("Could not initialize Vm");
        if (is_verbose) return err;
        return;
    };

    try vm.start(bough_path orelse vm.bytecode.boughs[0].name);
    while (vm.can_continue) {
        vm.run() catch {
            vm.err.print(std.io.getStdErr().writer());
            break;
        };
    }
}

fn getFilePath(allocator: std.mem.Allocator) !?[]const u8 {
    var args = try std.process.argsWithAllocator(allocator);
    _ = args.skip();
    return args.next();
}

const CliRunner = struct {
    runner: Runner,

    pub fn init() CliRunner {
        return .{
            .runner = .{
                .onDialogueFn = onDialogue,
                .onChoicesFn = onChoices,
            },
        };
    }

    pub fn print(_: *CliRunner, comptime msg: []const u8, args: anytype) void {
        const stdout = std.io.getStdOut().writer();
        stdout.print(msg, args) catch {
            std.debug.print("Could not print message", .{});
        };
    }

    pub fn onDialogue(runner: *Runner, vm: *Vm, dialogue: Dialogue) void {
        const stdin = std.io.getStdIn().reader();
        const self = @fieldParentPtr(CliRunner, "runner", runner);
        self.print(":", .{});
        if (dialogue.speaker) |speaker| {
            self.print("{s}", .{speaker});
        }
        self.print(": ", .{});
        self.print("{s}", .{dialogue.content});
        var buf: [2]u8 = undefined;
        if (stdin.readUntilDelimiterOrEof(&buf, '\n') catch &buf) |_| {
            vm.selectContinue();
        }
    }

    pub fn onChoices(runner: *Runner, vm: *Vm, choices: []Choice) void {
        const stdin = std.io.getStdIn().reader();
        const stderr = std.io.getStdErr().writer();
        const self = @fieldParentPtr(CliRunner, "runner", runner);
        var index: ?usize = null;
        while (index == null) {
            for (choices, 0..) |choice, i| {
                self.print("[{d}] {s}\n", .{ i, choice.content });
            }
            var buf: [10]u8 = undefined;
            if (stdin.readUntilDelimiterOrEof(&buf, '\n') catch &buf) |user_input| {
                const input = std.mem.trim(u8, user_input, "\r\n");
                index = std.fmt.parseInt(usize, input, 10) catch |err| blk: {
                    stderr.print("Invalid value: {}.\n", .{err}) catch {};
                    break :blk null;
                };
                if (index != null and index.? >= choices.len) {
                    index = null;
                    stderr.print("Invalid value.\n", .{}) catch {};
                }
            }
        }
        vm.selectChoice(index.?) catch |err| self.print("Error: {}", .{err});
    }
};

const AutoTestRunner = struct {
    runner: Runner,
    rnd: std.rand.Xoshiro256,

    pub fn init() AutoTestRunner {
        return .{
            .rnd = std.rand.DefaultPrng.init(std.crypto.random.int(u64)),
            .runner = .{
                .onDialogueFn = AutoTestRunner.onDialogue,
                .onChoicesFn = AutoTestRunner.onChoices,
            },
        };
    }

    pub fn onDialogue(_: *Runner, vm: *Vm, _: Dialogue) void {
        vm.selectContinue();
    }

    pub fn onChoices(runner: *Runner, vm: *Vm, choices: []Choice) void {
        var auto = @fieldParentPtr(AutoTestRunner, "runner", runner);
        const index = auto.rnd.random().intRangeAtMost(usize, 0, choices.len - 1);
        vm.selectChoice(index) catch |err| {
            std.debug.print("Error: {}", .{err});
        };
    }
};
