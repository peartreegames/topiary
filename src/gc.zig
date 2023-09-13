const std = @import("std");
const Vm = @import("./vm.zig").Vm;
const Value = @import("./values.zig").Value;

const Obj = Value.Obj;

pub const Gc = struct {
    allocator: std.mem.Allocator,
    bytes_allocated: usize,
    next_collection: usize,
    stack: ?*Obj,

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
            Obj.destroy(self.allocator, obj);
            self.stack = next;
        }
        self.* = undefined;
    }

    /// root_ctx must have `fn root() []const []Value` function
    pub fn create(self: *Gc, root_ctx: anytype, data: Obj.Data) !Value {
        var obj = try self.allocator.create(Obj);
        obj.* = .{
            .data = data,
            .next = self.stack,
        };
        self.stack = obj;
        self.bytes_allocated += @sizeOf(Obj);
        if (self.bytes_allocated > self.next_collection) {
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
        if (self.stack == null) return;

        var objPtr = self.stack;
        while (objPtr) |obj| {
            if (!obj.is_marked) {
                var unmarked = obj;
                objPtr = obj.next;
                Obj.destroy(self.allocator, unmarked);
                continue;
            }
            obj.is_marked = false;
            objPtr = obj.next;
        }
    }

    fn collect(self: *Gc, root_ctx: anytype) void {
        markAll(root_ctx);
        self.sweep();
        self.bytes_allocated = 0;
    }
};
