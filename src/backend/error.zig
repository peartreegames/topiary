const std = @import("std");
const Token = @import("../frontend/index.zig").Token;

pub const CompilerErr = struct {
    fmt: []const u8,
    severity: Severity,
    token: Token,

    const Severity = enum {
        info,
        warn,
        err,
    };
};

pub const CompilerErrors = struct {
    list: std.ArrayList(CompilerErr),
    allocator: std.mem.Allocator,

    // used for interpolatedExpressions
    offset_pos: usize = 0,
    offset_line: usize = 0,
    offset_col: usize = 0,

    pub fn init(allocator: std.mem.Allocator) CompilerErrors {
        return .{ .list = std.ArrayList(CompilerErr).init(allocator), .allocator = allocator };
    }

    pub fn add(self: *CompilerErrors, comptime fmt: []const u8, token: Token, severity: CompilerErr.Severity, args: anytype) !void {
        const msg = try std.fmt.allocPrint(self.allocator, fmt, args);
        try self.list.append(.{
            .fmt = msg,
            .severity = severity,
            .token = token,
        });
    }
    pub fn deinit(self: *CompilerErrors) void {
        for (self.list.items) |err| {
            self.allocator.free(err.fmt);
        }
        self.list.deinit();
    }

    pub fn write(self: *CompilerErrors, file_path: []const u8, source: []const u8, writer: anytype) !void {
        while (self.list.popOrNull()) |err| {
            defer self.allocator.free(err.fmt);
            const color_prefix = switch (err.severity) {
                .err => "\x1b[0;31m",
                .info => "\x1b[0;37m",
                .warn => "\x1b[0;36m",
            };
            try writer.print("{s}error: \x1b[0m{s}\n", .{ color_prefix, err.fmt });

            var start = err.token.start + self.offset_pos;
            var end = err.token.end + self.offset_pos;
            const line = err.token.line + self.offset_line;
            const column = err.token.column + self.offset_col;

            if (start == end) end += 1;
            start = @min(start, source.len - 1);
            end = @min(end, source.len);

            try writer.print("file: {s}\ntype: {s}, line: {}, column_start: {}, column_end: {}, source_start: {}, source_end: {}\n", .{ file_path, Token.toString(err.token.token_type), line, column, column + end - start, start, end });

            var lines = std.mem.splitSequence(u8, source, "\n");
            var lineNumber: usize = 1;
            while (lines.next()) |l| : (lineNumber += 1) {
                if (lineNumber < line) continue;
                var lineStart: usize = 0;
                if (lineNumber == 1) lineStart = if (std.mem.startsWith(u8, l, "\xEF\xBB\xBF")) 3 else @as(usize, 0);
                try writer.print("{s}\n", .{l[lineStart..]});
                const offset_col: u8 = if (lineNumber == 1) 1 else 2;
                try writer.writeByteNTimes(' ', @max(column - offset_col, 0));
                try writer.print("{s}", .{color_prefix});
                try writer.writeByteNTimes('~', end - start);
                try writer.writeAll("\n\x1b[0m");
                break;
            }
        }
    }
};
