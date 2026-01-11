const std = @import("std");
const Token = @import("../frontend/index.zig").Token;

pub const CompilerErr = struct {
    file_path: []const u8,
    fmt: []const u8,
    severity: Severity,
    token: Token,

    const Severity = enum {
        info,
        warn,
        err,
    };

    pub fn write(self: CompilerErr, source: []const u8, writer: *std.Io.Writer) !void {
        const color_prefix = switch (self.severity) {
            .err => "\x1b[0;31m",
            .info => "\x1b[0;37m",
            .warn => "\x1b[0;36m",
        };
        try writer.print("{s}error: \x1b[0m{s}\n", .{ color_prefix, self.fmt });
        var start = self.token.start;
        var end = self.token.end;
        const line = self.token.line;
        const column = self.token.column;

        if (start == end) end += 1;
        start = @min(start, source.len - 1);
        end = @min(end, source.len);

        try writer.print("file {s}, line {}\n", .{ self.file_path, line });
        try writer.print("======\n", .{});

        var lines = std.mem.splitSequence(u8, source, "\n");
        var lineNumber: usize = 1;
        while (lines.next()) |l| : (lineNumber += 1) {
            if (lineNumber < line) continue;
            var lineStart: usize = 0;
            if (lineNumber == 1) lineStart = if (std.mem.startsWith(u8, l, "\xEF\xBB\xBF")) 3 else @as(usize, 0);
            try writer.print("{s}\n", .{l[lineStart..]});
            const offset_col: u8 = if (lineNumber == 1) 1 else 2;
            var count = @max(column - offset_col, 0);
            while (count > 0) : (count -= 1) {
                try writer.writeByte(' ');
            }
            try writer.print("{s}", .{color_prefix});
            count = end - start;
            while (count > 0) : (count -= 1) {
                try writer.writeByte('~');
            }
            try writer.writeAll("\n\x1b[0m");
            break;
        }
        try writer.print("======\n", .{});
    }
};

pub const CompilerErrors = struct {
    list: std.ArrayList(CompilerErr),
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) CompilerErrors {
        return .{ .list = .empty, .allocator = allocator };
    }

    pub fn add(self: *CompilerErrors, file_path: []const u8, comptime fmt: []const u8, token: Token, severity: CompilerErr.Severity, args: anytype) !void {
        const msg = try std.fmt.allocPrint(self.allocator, fmt, args);
        try self.list.append(self.allocator, .{
            .file_path = file_path,
            .fmt = msg,
            .severity = severity,
            .token = token,
        });
    }

    pub fn deinit(self: *CompilerErrors) void {
        for (self.list.items) |err| {
            self.allocator.free(err.fmt);
        }
        self.list.deinit(self.allocator);
    }
};
