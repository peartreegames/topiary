const std = @import("std");
const _vm = @import("./vm.zig");
const parse = @import("./parser.zig").parse;
const Errors = @import("./error.zig").Errors;

const Vm = _vm.Vm;
const Dialogue = _vm.Dialogue;
const Choice = _vm.Choice;

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    var file_path = try getFilePath(arena.allocator());
    if (file_path == null) {
        std.log.warn("No file argument provided.", .{});
        return;
    }
    const file = std.fs.cwd().openFile(file_path.?, .{}) catch |e| {
        return std.log.err("Could not open file: {s}, {}", .{ file_path.?, e });
    };
    defer file.close();

    var content_alloc = arena.allocator();
    var contents = try file.reader().readAllAlloc(content_alloc, 10_000);
    defer content_alloc.free(contents);

    var vm_alloc = arena.allocator();

    var vm = try Vm.init(vm_alloc, CliRunner);
    vm.interpretSource(contents) catch {
        try vm.err.write(contents, std.io.getStdErr().writer());
    };
}

fn getFilePath(allocator: std.mem.Allocator) !?[]const u8 {
    var args = try std.process.argsWithAllocator(allocator);
    _ = args.skip();
    return args.next();
}

const CliRunner = struct {
    pub fn on_dialogue(vm: *Vm, dialogue: Dialogue) void {
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

    pub fn on_choices(vm: *Vm, choices: []Choice) void {
        const stdin = std.io.getStdIn().reader();
        var index: ?usize = null;
        while (index == null) {
            for (choices, 0..) |choice, i| {
                std.debug.print("[{d}] {s}\n", .{ i, choice.content });
            }
            var buf: [10]u8 = undefined;
            if (stdin.readUntilDelimiterOrEof(&buf, '\n') catch &buf) |user_input| {
                var input = std.mem.trim(u8, user_input, "\r\n");
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
