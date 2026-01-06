const std = @import("std");
const Vm = @import("vm.zig").Vm;
const Value = @import("../types/index.zig").Value;
const UUID = @import("../utils/index.zig").UUID;

const Obj = Value.Obj;

pub const Gc = struct {
    allocator: std.mem.Allocator,
    allocated: usize,
    threshold: usize,
    stack: ?*Obj,

    const default_collection: usize = 1024 * 1024;
    const growth_factor = 1.5;

    pub fn init(allocator: std.mem.Allocator) Gc {
        return .{
            .allocator = allocator,
            .allocated = 0,
            .threshold = default_collection,
            .stack = null,
        };
    }

    pub fn deinit(self: *Gc) void {
        while (self.stack) |obj| {
            const next = obj.next;
            obj.destroy(self.allocator);
            self.stack = next;
        }
        self.* = undefined;
    }

    /// root_ctx must have `fn root() []const []Value` function
    pub fn create(self: *Gc, root_ctx: anytype, data: Obj.Data) !Value {
        const obj = try self.allocator.create(Obj);
        obj.* = .{
            .id = UUID.new(),
            .data = data,
            .next = self.stack,
        };
        self.stack = obj;
        self.allocated += @sizeOf(Obj);
        if (self.allocated > self.threshold) {
            mark(obj);
            self.collect(root_ctx);
        }
        return .{
            .obj = obj,
        };
    }

    fn mark(obj: *Obj) void {
        if (obj.is_marked) return;
        obj.is_marked = true;
    }

    fn markAll(root_ctx: anytype) void {
        for (root_ctx.roots()) |list| {
            for (list) |item| {
                if (item == .obj) mark(item.obj);
            }
        }
    }

    fn sweep(self: *Gc) void {
        self.allocated = 0;
        if (self.stack == null) return;

        var objPtr = self.stack;
        while (objPtr) |obj| {
            if (!obj.is_marked) {
                const unmarked = obj;
                objPtr = obj.next;
                unmarked.destroy(self.allocator);
                continue;
            }
            obj.is_marked = false;
            objPtr = obj.next;
        }
    }

    fn collect(self: *Gc, root_ctx: anytype) void {
        markAll(root_ctx);
        self.sweep();
        self.threshold = @intFromFloat(@as(f64, @floatFromInt(self.allocated)) * growth_factor);
        if (self.threshold < default_collection) self.threshold = default_collection;
    }
};
