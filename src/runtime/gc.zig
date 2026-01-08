const std = @import("std");
const Vm = @import("vm.zig").Vm;
const Value = @import("../types/index.zig").Value;
const UUID = @import("../utils/index.zig").UUID;

const Obj = Value.Obj;
pub const GcObj = struct {
    next: ?*GcObj = null,
    obj: Obj,
    is_marked: bool = false,
};

pub const Gc = struct {
    allocator: std.mem.Allocator,
    allocated: usize,
    threshold: usize,
    stack: ?*GcObj,

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
        while (self.stack) |gc_obj| {
            const next = gc_obj.next;
            gc_obj.obj.deinit(self.allocator);
            self.allocator.destroy(gc_obj);
            self.stack = next;
        }
        self.* = undefined;
    }

    /// root_ctx must have `fn root() []const []Value` function
    pub fn create(self: *Gc, root_ctx: anytype, data: Obj.Data) !Value {
        const gc_obj = try self.allocator.create(GcObj);
        gc_obj.* = .{
            .obj = .{
                .id = UUID.new(),
                .data = data
            },
            .next = self.stack,
        };
        self.stack = gc_obj;
        self.allocated += @sizeOf(GcObj);
        if (self.allocated > self.threshold) {
            self.collect(root_ctx);
        }
        return .{
            .obj = &gc_obj.obj,
        };
    }

    fn mark(obj: *GcObj) void {
        if (obj.is_marked) return;
        obj.is_marked = true;
    }

    fn markAll(root_ctx: anytype) void {
        for (root_ctx.roots()) |list| {
            for (list) |item| {
                if (item == .obj) mark(@fieldParentPtr("obj", item.obj));
            }
        }
    }

    fn sweep(self: *Gc) void {
        if (self.stack == null) return;

        var prev: ?*GcObj = null;
        var current = self.stack;
        while (current) |obj| {
            if (!obj.is_marked) {
                const unmarked = obj;
                current = obj.next;
                if (prev) |p| p.next = current else self.stack = current;
                unmarked.obj.deinit(self.allocator);
                self.allocator.destroy(unmarked);
                self.allocated -= @sizeOf(GcObj);
                continue;
            }
            obj.is_marked = false;
            prev = current;
            current = obj.next;
        }
    }

    fn collect(self: *Gc, root_ctx: anytype) void {
        markAll(root_ctx);
        self.sweep();
        self.threshold = @intFromFloat(@as(f64, @floatFromInt(self.allocated)) * growth_factor);
        if (self.threshold < default_collection) self.threshold = default_collection;
    }
};
