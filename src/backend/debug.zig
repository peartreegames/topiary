const std = @import("std");

pub const DebugInfo = struct {
    allocator: std.mem.Allocator,
    file: []const u8,
    ranges: std.ArrayList(Range),

    pub const Range = struct {
        start: u32,
        end: u32,
        line: u32,
    };

    pub fn init(allocator: std.mem.Allocator, file: []const u8) DebugInfo {
        return .{
            .allocator = allocator,
            .file = file,
            .ranges = .empty,
        };
    }

    pub fn deinit(self: *DebugInfo) void {
        self.allocator.free(self.file);
        self.ranges.deinit(self.allocator);
    }

    pub fn serialize(self: *const DebugInfo, writer: *std.Io.Writer) !void {
        try writer.writeInt(u16, @intCast(self.file.len), .little);
        try writer.writeAll(self.file);
        try writer.writeInt(u16, @intCast(self.ranges.items.len), .little);
        for (self.ranges.items) |range| {
            try writer.writeInt(u32, range.start, .little);
            try writer.writeInt(u32, range.end, .little);
            try writer.writeInt(u32, range.line, .little);
        }
    }

    pub fn deserialize(reader: *std.Io.Reader, allocator: std.mem.Allocator) !DebugInfo {
        const file_len = try reader.takeInt(u16, .little);
        const file_buf = try allocator.alloc(u8, file_len);
        try reader.readSliceAll(file_buf);
        var ranges_len = try reader.takeInt(u16, .little);
        var ranges = try std.ArrayList(Range).initCapacity(allocator, ranges_len);
        while (ranges_len > 0) : (ranges_len -= 1) {
            const start = try reader.takeInt(u32, .little);
            const end = try reader.takeInt(u32, .little);
            const line = try reader.takeInt(u32, .little);
            ranges.appendAssumeCapacity(.{
                .start = start,
                .end = end,
                .line = line,
            });
        }
        return .{
            .allocator = allocator,
            .file = file_buf,
            .ranges = ranges,
        };
    }
};
