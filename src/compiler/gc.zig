const std = @import("std");
const Allocator = std.mem.Allocator;
const Value = @import("./values.zig").Value;

const root = @import("root");

pub const Gc = struct {
    const DEFAULT_GC_SIZE = 1024 * 1024;

    gpa: *Allocator,
    stack: ?*Value,
    newly_allocator: usize,

    pub fn init(allocator: *Allocator) Gc {
        return .{ .gpa = allocator, .stack = null, .newly_allocated = 0 };
    }

    pub fn newValue(self: *Gc, comptime T: type, val: T) !*Value {
        const typed = try self.gpa.create(T);

        typed.* = val;
        typed.base.next = self.stack;

        self.stack = &typed.base;
        self.newly_allocated += @sizeOf(T);
        if (self.newly_allocated >= self.trigger_size) {
            //also mark currently created so it doesn't get sweeped instantly
            self.mark(&typed.base);
            self.markAndSweep();
        }

        return &typed.base;
    }

    pub fn mark(self: *Gc, val: *Value) void {
        if (val.is_marked) return;

        val.is_marked = true;
        switch (val.type) {
            .iterable => self.mark(val.toIterable().value),
            .list => for (val.toList().value.items) |item| self.mark(item),
            .map => {
                for (val.toMap().value.items()) |entry| {
                    self.mark(entry.key);
                    self.mark(entry.value);
                }
            },
            else => {},
        }
    }

    pub fn sweep(self: *Gc) void {
        if (self.stack == null) return;

        var prev: ?*Value = null;
        var next: ?*Value = self.stack;
        while (next) |val| {
            next = val.next;
            if (!val.is_marked) {
                if (prev) |p| p.next = next else self.stack = next;
                val.destroy(self.gpa);
            } else {
                val.is_marked = false;
                prev = val;
            }
        }
    }

    fn markAndSweep(self: *Gc) void {
        // for (self.vm.globals.items) |global| self.mark(global);
        // for (self.vm.call_stack.items) |cs| if (cs.fp) |func| self.mark(func);
        // for (self.vm.locals.items) |local| self.mark(local);
        // for (self.vm.stack[0..self.vm.sp]) |stack| self.mark(stack);
        // for (self.vm.libs.items()) |lib_entry| self.mark(lib_entry.value);

        self.sweep();
        self.newly_allocated = 0;
    }

    pub fn deinit(self: *Gc) void {
        while (self.stack) |next| {
            const temp = next.next;
            next.destroy(self.gpa);
            self.stack = temp;
        }
        self.* = undefined;
    }
};
