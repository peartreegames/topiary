const std = @import("std");

pub fn Stack(comptime T: type) type {
    return struct {
        allocator: std.mem.Allocator,
        backing: []T,
        items: []T,

        count: usize = 0,

        const Self = Stack(T);

        pub fn init(allocator: std.mem.Allocator, capacity: usize) !Self {
            var backing = try allocator.alloc(T, capacity);
            return Self{
                .backing = backing,
                .items = backing[0..0],
                .allocator = allocator,
            };
        }

        pub fn deinit(self: *Self) void {
            self.allocator.free(self.backing);
        }

        pub fn push(self: *Self, item: T) void {
            self.count += 1;
            self.items = self.backing[0..self.count];
            self.items[self.count - 1] = item;
        }

        pub fn pop(self: *Self) T {
            self.count -= 1;
            const value = self.items[self.count];
            self.items = self.backing[0..self.count];
            return value;
        }

        pub fn peek(self: *Self) *T {
            return &self.items[self.count - 1];
        }

        pub fn previous(self: *Self) T {
            return self.backing[self.count];
        }
    };
}
