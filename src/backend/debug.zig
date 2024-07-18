const std = @import("std");

pub const DebugInfo = struct {
    file: []const u8,
    ranges: std.ArrayList(Range),

    pub const Range = struct {
        start: u32,
        end: u32,
        line: u32,
    };

    pub fn init(allocator: std.mem.Allocator, file: []const u8) DebugInfo {
        return .{
            .file = file,
            .ranges = std.ArrayList(Range).init(allocator),
        };
    }

    pub fn deinit(self: *const DebugInfo, allocator: std.mem.Allocator) void {
        allocator.free(self.file);
        self.ranges.deinit();
    }

    pub fn serialize(self: *const DebugInfo, writer: anytype) !void {
        try writer.writeInt(u16, @intCast(self.file.len), .little);
        try writer.writeAll(self.file);
        try writer.writeInt(u16, @intCast(self.ranges.items.len), .little);
        for (self.ranges.items) |range| {
            try writer.writeInt(u32, range.start, .little);
            try writer.writeInt(u32, range.end, .little);
            try writer.writeInt(u32, range.line, .little);
        }
    }

    pub fn deserialize(reader: anytype, allocator: std.mem.Allocator) !DebugInfo {
        const file_len = try reader.readInt(u16, .little);
        const file_buf = try allocator.alloc(u8, file_len);
        try reader.readNoEof(file_buf);
        var ranges_len = try reader.readInt(u16, .little);
        var ranges = try std.ArrayList(Range).initCapacity(allocator, ranges_len);
        while (ranges_len > 0) : (ranges_len -= 1) {
            const start = try reader.readInt(u32, .little);
            const end = try reader.readInt(u32, .little);
            const line = try reader.readInt(u32, .little);
            ranges.appendAssumeCapacity(.{
                .start = start,
                .end = end,
                .line = line,
            });
        }
        return .{
            .file = file_buf,
            .ranges = ranges,
        };
    }
};
