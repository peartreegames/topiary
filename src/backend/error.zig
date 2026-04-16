const std = @import("std");
const Token = @import("../frontend/index.zig").Token;

pub const CompilerErr = struct {
    file_path: []const u8,
    fmt: []const u8,
    severity: Severity,
    token: Token,
    end_token: ?Token = null,
    note: ?[]const u8 = null,
    suggestion: ?[]const u8 = null,

    const Severity = enum {
        info,
        warn,
        err,
    };

    pub fn write(self: CompilerErr, source: []const u8, writer: *std.Io.Writer) !void {
        const color_prefix = switch (self.severity) {
            .err => "\x1b[0;31m",
            .info => "\x1b[0;37m",
            .warn => "\x1b[0;33m",
        };
        const help_prefix = "\x1b[0;36m";
        const label = switch (self.severity) {
            .err => "error",
            .warn => "warning",
            .info => "info",
        };
        try writer.print("{s}{s}: \x1b[0m{s}\n", .{ color_prefix, label, self.fmt });
        var start = self.token.start;
        var end = self.token.end;
        if (self.end_token) |et| {
            if (et.file_index == self.token.file_index and et.line == self.token.line and et.end > end) {
                end = et.end;
            }
        }
        const line = self.token.line;
        const column = self.token.column;

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
            const offset_col: u8 = 1;
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

        if (self.suggestion) |s| {
            try writer.print("{s}help:\x1b[0m {s}\n", .{ help_prefix, s });
        }
        if (self.note) |n| {
            try writer.print("{s}note:\x1b[0m {s}\n", .{ help_prefix, n });
        }
    }
};

pub const CompilerErrors = struct {
    list: std.ArrayList(CompilerErr),
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) CompilerErrors {
        return .{ .list = .empty, .allocator = allocator };
    }

    pub fn add(self: *CompilerErrors, file_path: []const u8, comptime fmt: []const u8, token: Token, severity: CompilerErr.Severity, args: anytype) !void {
        try self.list.ensureUnusedCapacity(self.allocator, 1);
        const msg = try std.fmt.allocPrint(self.allocator, fmt, args);
        self.list.appendAssumeCapacity(.{
            .file_path = file_path,
            .fmt = msg,
            .severity = severity,
            .token = token,
        });
    }

    pub fn addSpan(
        self: *CompilerErrors,
        file_path: []const u8,
        comptime fmt: []const u8,
        start_token: Token,
        end_token: ?Token,
        severity: CompilerErr.Severity,
        args: anytype,
    ) !void {
        try self.list.ensureUnusedCapacity(self.allocator, 1);
        const msg = try std.fmt.allocPrint(self.allocator, fmt, args);
        self.list.appendAssumeCapacity(.{
            .file_path = file_path,
            .fmt = msg,
            .severity = severity,
            .token = start_token,
            .end_token = end_token,
        });
    }

    pub fn addWithHelp(
        self: *CompilerErrors,
        file_path: []const u8,
        comptime fmt: []const u8,
        token: Token,
        severity: CompilerErr.Severity,
        args: anytype,
        suggestion: ?[]const u8,
        note: ?[]const u8,
    ) !void {
        // Take ownership of caller-provided suggestion/note: they are freed
        // alongside `fmt` in `deinit`, so if the append fails we must free
        // them here to avoid leaking.
        errdefer {
            if (suggestion) |s| self.allocator.free(s);
            if (note) |n| self.allocator.free(n);
        }
        try self.list.ensureUnusedCapacity(self.allocator, 1);
        const msg = try std.fmt.allocPrint(self.allocator, fmt, args);
        self.list.appendAssumeCapacity(.{
            .file_path = file_path,
            .fmt = msg,
            .severity = severity,
            .token = token,
            .suggestion = suggestion,
            .note = note,
        });
    }

    pub fn addSpanWithHelp(
        self: *CompilerErrors,
        file_path: []const u8,
        comptime fmt: []const u8,
        start_token: Token,
        end_token: ?Token,
        severity: CompilerErr.Severity,
        args: anytype,
        suggestion: ?[]const u8,
        note: ?[]const u8,
    ) !void {
        errdefer {
            if (suggestion) |s| self.allocator.free(s);
            if (note) |n| self.allocator.free(n);
        }
        try self.list.ensureUnusedCapacity(self.allocator, 1);
        const msg = try std.fmt.allocPrint(self.allocator, fmt, args);
        self.list.appendAssumeCapacity(.{
            .file_path = file_path,
            .fmt = msg,
            .severity = severity,
            .token = start_token,
            .end_token = end_token,
            .suggestion = suggestion,
            .note = note,
        });
    }

    pub fn deinit(self: *CompilerErrors) void {
        for (self.list.items) |err| {
            self.allocator.free(err.fmt);
            if (err.suggestion) |s| self.allocator.free(s);
            if (err.note) |n| self.allocator.free(n);
        }
        self.list.deinit(self.allocator);
    }
};
