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
    const file_path = try getFilePath(arena.allocator());
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
