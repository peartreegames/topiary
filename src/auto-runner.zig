const std = @import("std");
const Vm = @import("./vm.zig").Vm;
const parseFile = @import("./parser.zig").parseFile;
const Scope = @import("./scope.zig").Scope;
const Compiler = @import("./compiler.zig").Compiler;
const Errors = @import("./error.zig").Errors;
const runners = @import("./runner.zig");

const Runner = runners.Runner;
const Dialogue = runners.Dialogue;
const Choice = runners.Choice;

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    var args = try std.process.argsWithAllocator(arena.allocator());
    _ = args.skip();
    var file_path = args.next();
    if (file_path == null) {
        std.log.warn("No file argument provided.", .{});
        return;
    }
    var count = try std.fmt.parseInt(u64, args.next().?, 10);
    var allocator = arena.allocator();
    var vm_alloc = arena.allocator();
    var err = Errors.init(vm_alloc);
    errdefer err.deinit();

    var dir = try std.fs.cwd().openDir(std.fs.path.dirname(file_path.?).?, .{});
    var file_name = std.fs.path.basename(file_path.?);
    var tree = parseFile(allocator, dir, file_name, &err) catch return;
    defer tree.deinit();
    defer allocator.free(tree.source);

    var compiler = try Compiler.init(allocator, &err);
    defer compiler.deinit();

    compiler.compile(tree) catch |e| {
        try err.write(tree.source, std.io.getStdErr().writer());
        return e;
    };
    var bytecode = try compiler.bytecode();

    var i: usize = 0;
    var visit_counts = std.StringHashMap(u64).init(vm_alloc);
    defer visit_counts.deinit();
    while (i < count) : (i += 1) {
        var auto_runner = AutoTestRunner.init();
        var vm = try Vm.init(vm_alloc, bytecode, &auto_runner.runner, &err);
        vm.interpret() catch {
            try err.write(tree.source, std.io.getStdErr().writer());
            continue;
        };
        defer vm.deinit();
        for (vm.globals, 0..) |g, idx| {
            if (g == .visit) {
                const name = bytecode.global_symbols[idx].name;
                var cur = try visit_counts.getOrPutValue(name, 0);
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

pub const AutoTestRunner = struct {
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
