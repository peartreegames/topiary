const std = @import("std");
const Vm = @import("./vm.zig").Vm;
const parseFile = @import("./parser.zig").parseFile;
const Scope = @import("./scope.zig").Scope;
const Compiler = @import("./compiler.zig").Compiler;
const Errors = @import("./compiler-error.zig").CompilerErrors;
const runners = @import("./runner.zig");

const Runner = runners.Runner;
const Dialogue = runners.Dialogue;
const Choice = runners.Choice;

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    var args = try std.process.argsWithAllocator(arena.allocator());
    _ = args.skip();
    const file_path = args.next();
    if (file_path == null) {
        std.log.warn("No file argument provided.", .{});
        return;
    }

    var allocator = arena.allocator();
    const vm_alloc = arena.allocator();
    var err = Errors.init(vm_alloc);
    errdefer err.deinit();

    const dir = try std.fs.cwd().openDir(std.fs.path.dirname(file_path.?).?, .{});
    const file_name = std.fs.path.basename(file_path.?);
    var tree = parseFile(allocator, dir, file_name, &err) catch return;
    defer tree.deinit();
    defer allocator.free(tree.source);

    var compiler = try Compiler.init(allocator, &err);
    defer compiler.deinit();

    compiler.compile(tree) catch |e| {
        try err.write(tree.source, std.io.getStdErr().writer());
        return e;
    };
    const bytecode = try compiler.bytecode();

    const is_maybe_auto = args.next();
    if (is_maybe_auto) |is_auto| {
        if (std.mem.eql(u8, is_auto, "--auto")) {
            const count = try std.fmt.parseInt(u64, args.next().?, 10);
            var i: usize = 0;
            var visit_counts = std.StringArrayHashMap(u64).init(vm_alloc);
            defer visit_counts.deinit();
            while (i < count) : (i += 1) {
                var auto_runner = AutoTestRunner.init();
                var vm = try Vm.init(vm_alloc, bytecode, &auto_runner.runner);
                vm.interpret() catch {
                    try err.write(tree.source, std.io.getStdErr().writer());
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
        }
        return;
    }

    var cli_runner = CliRunner.init();
    var vm = try Vm.init(vm_alloc, bytecode, &cli_runner.runner);

    vm.interpret() catch {
        try err.write(tree.source, std.io.getStdErr().writer());
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
