const std = @import("std");

pub const UUID = struct {
    pub const ID = [Size]u8;
    pub const Empty: ID = [_]u8{ '0', '0', '0', '0', '0', '0', '-', '0', '0', '0', '0', '0', '0' };

    const Self = @This();
    const Size: usize = 13;
    const chars: []const u8 = "23456789ABCDEFGHJKMNPQRSTUVWXYZ";

    pub fn new() ID {
        return create(@intCast(std.time.milliTimestamp()));
    }

    pub fn create(seed: u64) ID {
        var r = std.rand.DefaultPrng.init(seed);
        var id: ID = undefined;

        var i: usize = 0;
        while (i < Size) : (i += 1) {
            var res: u8 = r.random().uintLessThanBiased(u8, chars.len);
            id[i] = chars[res];
        }

        id[6] = '-';
        return id;
    }
};
