pub const RuntimeErr = struct {
    line: u32 = 0,
    file: ?[]const u8 = null,
    msg: ?[]const u8 = null,
    pub fn print(self: @This(), writer: anytype) void {
        writer.print("Error at \"{?s}\" line {}: {?s}\n", .{ self.file, self.line, self.msg }) catch {};
    }
};
