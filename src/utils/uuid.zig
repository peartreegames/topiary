const std = @import("std");

const chars: []const u8 = "0123456789ABCDEF";

pub const ID = [UUID.Size]u8;

pub const UUID = struct {
    const Self = @This();
    pub const Size: usize = 36;
    pub const Empty: ID = undefined;
    id: ID,

    pub fn create() !ID {
        return UUID.new(@intCast(std.time.milliTimestamp()));
    }
    pub fn new(seed: u64) !ID {
        var r = std.rand.DefaultPrng.init(seed);
        var uu = try std.heap.page_allocator.create(Self);

        var i: usize = 0;
        while (i < 36) : (i += 1) {
            var res: u8 = r.random().uintLessThanBiased(u8, 16);
            uu.id[i] = chars[res];
        }

        uu.id[8] = '-';
        uu.id[13] = '-';
        uu.id[14] = '4';
        uu.id[18] = '-';
        uu.id[23] = '-';

        return uu.id;
    }
};
