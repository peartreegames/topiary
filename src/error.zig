const std = @import("std");
const tok = @import("token.zig");
const Token = tok.Token;

pub const Error = struct {
    fmt: []const u8,
    severity: Severity,
    token: Token,

    const Severity = enum {
        info,
        warn,
        err,
    };
};

pub const Errors = struct {
    list: std.ArrayListUnmanaged(Error),
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) Errors {
        return .{ .list = std.ArrayListUnmanaged(Error){}, .allocator = allocator };
    }

    pub fn add(self: *Errors, comptime fmt: []const u8, token: Token, severity: Error.Severity, args: anytype) !void {
        const msg = try std.fmt.allocPrint(self.allocator, fmt, args);
        errdefer self.allocator.free(msg);

        return self.list.append(self.allocator, .{
            .fmt = msg,
            .severity = severity,
            .token = token,
        });
    }
    pub fn deinit(self: *Errors) void {
        for (self.list.items) |err| {
            self.allocator.free(err.fmt);
        }
        self.list.deinit(self.allocator);
        self.* = undefined;
    }

    pub fn write(self: *Errors, source: []const u8, writer: anytype) !void {
        while (self.list.popOrNull()) |err| {
            defer self.allocator.free(err.fmt);
            const color_prefix = switch (err.severity) {
                .err => "\x1b[0;31m",
                .info => "\x1b[0;37m",
                .warn => "\x1b[0;36m",
            };
            try writer.print("{s}error: \x1b[0m{s}\n", .{ color_prefix, err.fmt });

            const start = @min(err.token.start, source.len - 1);
            const end = @min(err.token.end, source.len);

            try writer.print("type: {s}, line: {d}, column: {d}\n", .{ tok.toString(err.token.token_type), err.token.line, err.token.column });
            try writer.print("{s}\n", .{source[start..end]});
            try writer.writeByteNTimes('~', end - start);
            try writer.writeAll("\x1b[0;35m^\n\x1b[0m");
        }
    }
};
