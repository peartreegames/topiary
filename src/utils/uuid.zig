const std = @import("std");

var rnd: ?std.Random.DefaultPrng = null;
pub const UUID = struct {
    pub const ID = [Size]u8;
    pub const Empty: ID = [_]u8{ '0', '0', '0', '0', '0', '0', '0', '0', '-', '0', '0', '0', '0', '0', '0', '0', '0' };

    const Self = @This();
    pub const Size: usize = 17;
    const chars: []const u8 = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ";

    pub fn new() ID {
        if (rnd == null) rnd = std.Random.DefaultPrng.init(@intCast(std.time.milliTimestamp()));
        return create(rnd.?.random().int(u64));
    }

    pub fn create(seed: u64) ID {
        var r = std.Random.DefaultPrng.init(seed);
        var id: ID = undefined;

        var i: usize = 0;
        while (i < Size) : (i += 1) {
            const res: u8 = r.random().uintLessThanBiased(u8, chars.len);
            id[i] = chars[res];
        }

        id[8] = '-';
        return id;
    }

    pub fn isEmpty(id: ID) bool {
        return std.mem.eql(u8, &id, &UUID.Empty);
    }

    pub fn fromString(str: []const u8) ID {
        if (str.len != Size) return Empty;
        if (str[8] != '-') return Empty;
        for (str, 0..) |c, i| {
            if (i == 8) continue;
            if (std.mem.indexOfScalar(u8, chars, c) == null) return Empty;
        }
        var id: ID = undefined;
        @memcpy(&id, str[0..Size]);
        return id;
    }

    pub fn fromStringHash(str: []const u8) ID {
        if (str.len == 0) return Empty;
        const hash = std.hash.Wyhash.hash(0, str);
        return create(hash);
    }
};
