const std = @import("std");

var rnd: ?std.rand.DefaultPrng = null;
pub const UUID = struct {
    pub const ID = [Size]u8;
    pub const Empty: ID = [_]u8{ '0', '0', '0', '0', '0', '0', '0', '0', '-', '0', '0', '0', '0', '0', '0', '0', '0' };

    const Self = @This();
    const Size: usize = 17;
    const chars: []const u8 = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ";

    pub fn new() ID {
        if (rnd == null) rnd = std.rand.DefaultPrng.init(@intCast(std.time.milliTimestamp()));
        return create(rnd.?.random().int(u64));
    }

    pub fn create(seed: u64) ID {
        var r = std.rand.DefaultPrng.init(seed);
        var id: ID = undefined;

        var i: usize = 0;
        while (i < Size) : (i += 1) {
            const res: u8 = r.random().uintLessThanBiased(u8, chars.len);
            id[i] = chars[res];
        }

        id[8] = '-';
        return id;
    }

    pub fn fromString(str: []const u8) ID {
        var id: ID = undefined;
        var i: usize = 0;
        while (i < Size) : (i += 1) {
            id[i] = str[i];
        }
        return id;
    }
};
