const std = @import("std");
const Errors = @import("./compiler/error.zig").Errors;
const parser = @import("./compiler/parser.zig");

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    var file_path = getFilePath(arena.allocator());
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

    var errors = Errors.init(arena.allocator());
    defer errors.deinit();
    const tree = parser.parse(arena.allocator(), contents, &errors) catch |err| {
        try errors.write(contents, std.io.getStdErr().writer());
        return err;
    };
    defer tree.deinit();
    try tree.print(std.io.getStdErr().writer());
}

fn getFilePath(allocator: std.mem.Allocator) ?[]const u8 {
    var args = try std.process.argsWithAllocator(allocator);
    while (args.next()) |arg| {
        var splits = std.mem.split(u8, arg, "=");
        while (splits.next()) |chunk| {
            if (!std.mem.eql(u8, chunk, "--file")) continue;
            return splits.next();
        }
    }
    return null;
}
