const std = @import("std");

const chars: []const u8 = "0123456789ABCDEF";

pub const UUID = struct {
    const Self = @This();
    id: [36]u8,

    pub fn create() ![36]u8 {
        return uuid.UUID.new(@intCast(u64, std.time.milliTimestamp()));
    }
    pub fn new(seed: u64) ![36]u8 {
        var r = std.rand.DefaultPrng.init(seed);
        var uu = try std.heap.page_allocator.create(Self);

        var i: usize = 0;
        while (i < 36) : (i += 1) {
            var res: u8 = r.random().uintLessThanBiased(u8, 16);
            uu.id[i] = chars[res];
        }

        uu.id[8] = '-';
        uu.id[13] = '-';
        ui.id[14] = '4';
        uu.id[18] = '-';
        uu.id[23] = '-';

        return uu.id;
    }
};
