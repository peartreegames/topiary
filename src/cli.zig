const std = @import("std");
const Vm = @import("./vm.zig").Vm;
const Scope = @import("./scope.zig").Scope;
const Compiler = @import("./compiler.zig").Compiler;
const Errors = @import("./compiler-error.zig").CompilerErrors;
const module = @import("module.zig");
const runners = @import("./runner.zig");

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
    } else {
        try out.print("topi - command line topiary processor\n", .{});
        try out.print("Usage:\n", .{});
        try out.print("\n", .{});
    }
    try out.print("        topi <file> [--auto|-a <count>] [--compile|-c <output_file>]\n", .{});
}

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    var args = try std.process.argsWithAllocator(arena.allocator());
    _ = args.skip();

    const file_path = args.next();
    if (file_path == null) {
        try usage("No file argument provided.\n");
        return;
    }
    if (std.mem.eql(u8, file_path.?, "-h") or std.mem.eql(u8, file_path.?, "--help")) {
        try usage("");
        return;
    }
    const maybe_flag = args.next();
    var is_compile = false;
    var out_path: ?[]const u8 = null;
    var is_auto = false;
    var auto_count: usize = 0;
    if (maybe_flag) |flag| {
        if (std.mem.eql(u8, flag, "-c") or std.mem.eql(u8, flag, "--compile")) {
            const maybe_out_file = args.next();
            if (maybe_out_file == null) {
                try usage("Compile requires an output file.\n");
                return;
            }
            is_compile = true;
            out_path = maybe_out_file.?;
        }
        if (std.mem.eql(u8, flag, "-a") or std.mem.eql(u8, flag, "--auto")) {
            const maybe_auto_count = args.next();
            if (maybe_auto_count == null) {
                try usage("Auto requires a play count.\n");
                return;
            }
            is_auto = true;
            auto_count = try std.fmt.parseInt(u64, maybe_auto_count.?, 10);
        }
    }

    const allocator = arena.allocator();
    const full_path = try std.fs.cwd().realpathAlloc(allocator, file_path.?);
    var mod = try Module.init(allocator, full_path);
    var bytecode = try mod.generateBytecode();
    defer bytecode.free(allocator);
    mod.deinit();

    if (is_compile) {
        const file = try std.fs.cwd().createFile(out_path.?, .{});
        defer file.close();
        const writer = file.writer();
        try bytecode.serialize(writer);
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
                vm.err.print(std.debug);
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
    var vm = try Vm.init(vm_alloc, bytecode, &cli_runner.runner);

    vm.interpret() catch {
        vm.err.print(std.debug);
    };
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

    pub fn onDialogue(_: *Runner, vm: *Vm, dialogue: Dialogue) void {
        if (dialogue.speaker) |speaker| {
            std.debug.print("{s}: ", .{speaker});
        }
        const stdin = std.io.getStdIn().reader();
        std.debug.print("{s}", .{dialogue.content});
        var buf: [2]u8 = undefined;
        if (stdin.readUntilDelimiterOrEof(&buf, '\n') catch &buf) |_| {
            vm.selectContinue();
        }
    }

    pub fn onChoices(_: *Runner, vm: *Vm, choices: []Choice) void {
        const stdin = std.io.getStdIn().reader();
        var index: ?usize = null;
        while (index == null) {
            for (choices, 0..) |choice, i| {
                std.debug.print("[{d}] {s}\n", .{ i, choice.content });
            }
            var buf: [10]u8 = undefined;
            if (stdin.readUntilDelimiterOrEof(&buf, '\n') catch &buf) |user_input| {
                const input = std.mem.trim(u8, user_input, "\r\n");
                index = std.fmt.parseInt(usize, input, 10) catch |err| blk: {
                    std.debug.print("Invliad value: {}.\n", .{err});
                    break :blk null;
                };
                if (index != null and index.? >= choices.len) {
                    index = null;
                    std.debug.print("Invalid value.\n", .{});
                }
            }
        }
        vm.selectChoice(index.?) catch |err| std.debug.print("Error: {}", .{err});
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
