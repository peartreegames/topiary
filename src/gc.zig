const std = @import("std");
const Vm = @import("./vm.zig").Vm;
const Value = @import("./values.zig").Value;

pub const Object = struct {
    is_marked: bool = false,
    next: ?*Object = null,
    value: Value,
};

pub const Gc = struct {
    allocator: std.mem.Allocator,
    bytes_allocated: usize,
    next_collection: usize,
    stack: ?*Object,

    const default_collection: usize = 1024 * 1024;

    pub fn init(allocator: std.mem.Allocator) Gc {
        return .{
            .allocator = allocator,
            .bytes_allocated = 0,
            .next_collection = default_collection,
            .stack = null,
        };
    }

    pub fn deinit(self: *Gc) void {
        while (self.stack) |obj| {
            const next = obj.next;
            obj.value.destroy(self.allocator);
            self.allocator.destroy(obj);
            self.stack = next;
        }
        self.* = undefined;
    }

    pub fn create(self: *Gc, vm: *Vm, value: Value) !*Object {
        var obj = try self.allocator.create(Object);
        obj.* = .{
            .value = value,
            .next = self.stack,
        };
        self.stack = obj;

        self.bytes_allocated += @sizeOf(Value);
        if (self.bytes_allocated > self.next_collection) {
            mark(obj);
            self.collect(vm);
        }
        return obj;
    }

    fn mark(value: *Object) void {
        if (value.is_marked) return;
        value.is_marked = true;
    }

    fn markAll(vm: *Vm) void {
        for (vm.stack.items) |item| mark(item);
        for (vm.globals.items) |item| mark(item);
    }

    fn sweep(self: *Gc) void {
        if (self.stack == null) return;

        var valuePtr = self.stack;
        while (valuePtr) |value| {
            if (!value.is_marked) {
                var unmarked = value;
                valuePtr = value.next;
                switch (unmarked.value) {
                    .string => |s| self.allocator.free(s),
                    else => {},
                }
                self.allocator.destroy(unmarked);
                continue;
            }
            value.is_marked = false;
            valuePtr = value.next;
        }
    }

    fn collect(self: *Gc, vm: *Vm) void {
        markAll(vm);
        self.sweep();
        self.bytes_allocated = 0;
    }
};
