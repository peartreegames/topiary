const std = @import("std");

pub fn array(
    comptime fmt: []const u8,
    s: anytype,
) struct {
    data: @TypeOf(s),
    pub fn format(
        self: @This(),
        w: *std.Io.Writer,
    ) std.Io.Writer.Error!void {
        for (self.data, 0..) |elem, i| {
            try w.print(fmt, .{elem});
            if (i != self.data.len - 1)
                try w.writeAll(" ");
        }
    }
} {
    return .{ .data = s };
}
