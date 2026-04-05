const std = @import("std");
const Vm = @import("vm.zig").Vm;
const Value = @import("../types/index.zig").Value;
const UUID = @import("../utils/index.zig").UUID;

const Obj = Value.Obj;
const Class = @import("../types/index.zig").Class;
const DebugInfo = @import("../backend/index.zig").DebugInfo;

pub const GcObj = struct {
    next: ?*GcObj = null,
    obj: Obj,
    data_size: usize = 0,
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

    /// Set the collection threshold. Intended for tests that need to force
    /// frequent collections; production code should leave the default alone.
    pub fn setThreshold(self: *Gc, threshold: usize) void {
        self.threshold = threshold;
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

    /// root_ctx must have a `fn markRoots(self: *@This()) void` method that
    /// calls `Gc.markValue` on every reachable root.
    pub fn create(self: *Gc, root_ctx: anytype, data: Obj.Data) !Value {
        if (self.allocated > self.threshold) {
            self.collect(root_ctx);
        }
        const size = dataSize(data);
        const gc_obj = try self.allocator.create(GcObj);
        gc_obj.* = .{
            .obj = .{
                .id = UUID.new(),
                .data = data,
            },
            .next = self.stack,
            .data_size = size,
        };
        self.stack = gc_obj;
        self.allocated += @sizeOf(GcObj) + size;
        return .{
            .obj = &gc_obj.obj,
        };
    }

    fn dataSize(data: Obj.Data) usize {
        return switch (data) {
            .string => |s| s.len,
            .list => |l| l.capacity * @sizeOf(Value),
            .map => |m| (m.capacity() + 1) * (@sizeOf(Value) * 2 + @sizeOf(u32)),
            .set => |s| (s.capacity() + 1) * (@sizeOf(Value) + @sizeOf(u32)),
            .class => |c| (c.fields.len + c.methods.len) * @sizeOf(Class.Member),
            .instance => |inst| inst.fields.len * @sizeOf(Value),
            .@"enum" => |e| e.name.len + e.values.len * @sizeOf([]const u8),
            .function => |f| f.instructions.len + f.debug_info.len * @sizeOf(DebugInfo) + (if (f.name) |n| n.len else 0),
            .anchor => |a| a.name.len,
            .@"extern", .builtin => 0,
        };
    }

    pub fn markValue(val: Value) void {
        if (val == .obj) mark(@fieldParentPtr("obj", val.obj));
    }

    fn mark(gc_obj: *GcObj) void {
        if (gc_obj.is_marked) return;
        gc_obj.is_marked = true;
        switch (gc_obj.obj.data) {
            .list => |l| for (l.items) |item| markValue(item),
            .map => |m| {
                for (m.keys()) |k| markValue(k);
                for (m.values()) |v| markValue(v);
            },
            .set => |s| for (s.keys()) |k| markValue(k),
            .class => |c| {
                for (c.fields) |f| markValue(f.value);
                for (c.methods) |m| markValue(m.value);
            },
            .instance => |inst| {
                mark(@fieldParentPtr("obj", inst.base));
                for (inst.fields) |f| markValue(f);
            },
            .string, .@"enum", .function, .@"extern", .builtin, .anchor => {},
        }
    }

    fn markAll(root_ctx: anytype) void {
        root_ctx.markRoots();
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
                self.allocated -= @sizeOf(GcObj) + unmarked.data_size;
                unmarked.obj.deinit(self.allocator);
                self.allocator.destroy(unmarked);
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
