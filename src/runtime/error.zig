const std = @import("std");

pub const RuntimeErr = struct {
    msg: ?[]const u8 = null,
    trace: std.ArrayList(Trace) = .empty,

    pub const Trace = struct {
        line: u32 = 0,
        file: ?[]const u8 = null,
        function_name: ?[]const u8 = null,
    };

    pub fn deinit(self: *RuntimeErr, allocator: std.mem.Allocator) void {
        if (self.msg) |m| allocator.free(m);
        self.trace.deinit(allocator);
    }

    pub fn print(self: @This(), writer: *std.Io.Writer) void {
        writer.print("Error: {?s}\n", .{self.msg}) catch {};
        for (self.trace.items) |t| {
            writer.print("  at {?s}:{d}", .{ t.file, t.line }) catch {};
            if (t.function_name) |name| {
                writer.print(" (in {s})", .{name}) catch {};
            }
            writer.print("\n", .{}) catch {};
        }
        writer.flush() catch {};
    }
};
