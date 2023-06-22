const std = @import("std");
const Vm = @import("./vm.zig").Vm;
const Value = @import("./values.zig").Value;

pub const Gc = struct {
    vm: *Vm,
    allocator: std.mem.Allocator,
    bytes_allocated: usize,
    next_collection: usize,
    stack: ?*Value,

    const default_collection: usize = 1024 * 1024;
    const heap_grow_factor = 1.8;

    pub fn init(vm: *Vm, allocator: std.mem.Allocator) Gc {
        return .{
            .vm = vm,
            .allocator = allocator,
            .bytes_allocated = 0,
            .next_collection = default_collection,
            .stack = null,
        };
    }

    pub fn deinit(self: *Gc) void {
        self.collect();
    }

    pub fn create(self: *Gc, comptime T: type, value: T) !*Value {
        const obj = try self.allocator.create(T);
        obj.* = value;
        self.stack = &obj.base;

        self.bytes_allocated += @sizeOf(T);
        if (self.bytes_allocator > self.next_collection) {
            self.mark(&obj.base);
            self.collect();
        }
        return &obj.base;
    }

    fn mark(self: *Gc, value: *Value) void {
        _ = self;
        if (value.is_marked) return;
        value.is_marked = true;
    }

    fn markAll(self: *Gc) void {
        for (self.vm.stack.items) |value| {
            self.mark(value);
        }
    }

    fn sweep(self: *Gc) void {
        if (self.stack == null) return;

        var valuePtr = &(self.stack);
        while (valuePtr.*) |value| {
            if (!value.is_marked) {
                var unmarked = value;
                valuePtr.* = value.next;
                self.allocator.destroy(unmarked);
                continue;
            }
            value.is_marked = false;
            valuePtr = &(value.next);
        }
    }

    fn collect(self: *Gc) void {
        self.markAll();
        self.sweep();
        self.bytes_allocated = 0;
    }
};
