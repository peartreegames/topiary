const std = @import("std");

const topi = @import("topi");
const Value = topi.types.Value;
const TrueValue = topi.types.True;
const FalseValue = topi.types.False;
const NilValue = topi.types.Nil;
const Vm = topi.runtime.Vm;

const runner = @import("runner.zig");
const ExportFunction = runner.ExportFunction;
const ExportString = runner.ExportString;

const Tag = enum(u8) {
    nil,
    bool,
    number,
    string,
    list,
    set,
    map,
    @"enum",
};

pub const ExportValue = extern struct {
    tag: Tag,
    data: extern union {
        nil: void,
        bool: bool,
        number: f32,
        string: ExportString,
        list: extern struct {
            items: [*c]ExportValue,
            count: u16,
        },
        @"enum": extern struct {
            name: ExportString,
            value: ExportString,
        },
    },

    pub const Nil: ExportValue = .{ .tag = Tag.nil, .data = .{ .nil = {} } };
    pub const True: ExportValue = .{ .tag = Tag.bool, .data = .{ .bool = true } };
    pub const False: ExportValue = .{ .tag = Tag.bool, .data = .{ .bool = false } };

    pub fn fromValue(value: Value, allocator: std.mem.Allocator) ExportValue {
        return switch (value) {
            .bool => |b| if (b) True else False,
            .number => |n| .{ .tag = Tag.number, .data = .{ .number = n } },
            .enum_value => |e| .{ .tag = Tag.@"enum", .data = .{ .@"enum" = .{
                .name = .{ .ptr = e.base.data.@"enum".name.ptr, .len = e.base.data.@"enum".name.len },
                .value = .{ .ptr = e.base.data.@"enum".values[e.index].ptr, .len = e.base.data.@"enum".values[e.index].len },
            } } },
            .obj => |o| switch (o.data) {
                .string => |s| .{ .tag = Tag.string, .data = .{ .string = .{ .ptr = s.ptr, .len = s.len } } },
                .list => |l| blk: {
                    var list = allocator.alloc(ExportValue, l.items.len) catch @panic("Could not allocate list items");
                    var i: usize = 0;
                    while (i < l.items.len) : (i += 1) {
                        list[i] = fromValue(l.items[i], allocator);
                    }
                    break :blk .{ .tag = Tag.list, .data = .{ .list = .{ .items = list.ptr, .count = @intCast(list.len) } } };
                },
                .set => |s| blk: {
                    var list = allocator.alloc(ExportValue, s.count()) catch @panic("Could not allocate list items");
                    for (s.keys(), 0..) |key, i| {
                        list[i] = fromValue(key, allocator);
                    }
                    break :blk .{ .tag = Tag.set, .data = .{ .list = .{ .items = list.ptr, .count = @intCast(list.len) } } };
                },
                .map => |m| blk: {
                    const count = m.count();
                    var list = allocator.alloc(ExportValue, count * 2) catch @panic("Could not allocate list items");
                    var it = m.iterator();
                    var i: usize = 0;
                    while (it.next()) |kvp| {
                        list[i] = fromValue(kvp.key_ptr.*, allocator);
                        list[i + 1] = fromValue(kvp.value_ptr.*, allocator);
                        i += 2;
                    }
                    break :blk .{ .tag = Tag.map, .data = .{ .list = .{ .items = list.ptr, .count = @intCast(count) } } };
                },
                else => Nil,
            },
            else => Nil,
        };
    }

    pub fn toValue(self: *const ExportValue, vm: *Vm, free: ExportFunction.Free) !Value {
        return switch (self.tag) {
            .nil => NilValue,
            .bool => if (self.data.bool) TrueValue else FalseValue,
            .number => .{ .number = self.data.number },
            .@"enum" => {
                const e = self.data.@"enum";
                const name = try vm.alloc.dupe(u8, e.name.ptr[0..e.name.len]);
                free(@intFromPtr(e.name.ptr));
                errdefer vm.alloc.free(name);
                const value = try vm.alloc.dupe(u8, e.value.ptr[0..e.value.len]);
                free(@intFromPtr(e.value.ptr));
                errdefer vm.alloc.free(value);

                for (vm.bytecode.constants) |c| {
                    if (c == .obj and c.obj.data == .@"enum" and std.mem.eql(u8, name, c.obj.data.@"enum".name)) {
                        const base = c.obj.data.@"enum";
                        for (base.values, 0..) |v, i| {
                            if (std.mem.eql(u8, v, value)) {
                                return .{ .enum_value = .{ .base = c.obj, .index = @intCast(i) } };
                            }
                        }
                        return error.ValueNotFound;
                    }
                }
                return error.EnumNotFound;
            },
            .string => {
                const str = try vm.alloc.dupe(u8, self.data.string.ptr[0..self.data.string.len]);
                free(@intFromPtr(self.data.string.ptr));
                return vm.gc.create(vm, .{ .string = str });
            },
            .list => {
                var list: std.ArrayList(Value) = .empty;
                for (0..self.data.list.count) |i| {
                    const item: *ExportValue = @ptrCast(&self.data.list.items[i]);
                    try list.append(vm.alloc, try item.toValue(vm, free));
                }
                free(@intFromPtr(self.data.list.items));
                return vm.gc.create(vm, .{ .list = list });
            },
            .set => {
                var set = Value.Obj.SetType.empty;
                const length = self.data.list.count;
                var i: usize = 0;
                while (i < length) : (i += 1) {
                    const item: *ExportValue = @ptrCast(&self.data.list.items[i]);
                    const value = try item.toValue(vm, free);
                    try set.putContext(vm.alloc, value, {}, Value.adapter);
                }

                free(@intFromPtr(self.data.list.items));
                return vm.gc.create(vm, .{ .set = set });
            },
            .map => {
                var map = Value.Obj.MapType.empty;
                const length = self.data.list.count;
                var i: usize = 0;
                while (i < length) : (i += 2) {
                    const key: *ExportValue = @ptrCast(&self.data.list.items[i]);
                    const value: *ExportValue = @ptrCast(&self.data.list.items[i + 1]);
                    try map.put(vm.alloc, try key.toValue(vm, free), try value.toValue(vm, free));
                }
                free(@intFromPtr(self.data.list.items));
                return vm.gc.create(vm, .{ .map = map });
            },
        };
    }
    pub fn print(self: ExportValue, writer: *std.Io.Writer) !void {
        switch (self.tag) {
            .nil => try writer.print("nil", .{}),
            .bool => try writer.print("{}", .{self.data.bool}),
            .number => try writer.print("{d:.5}", .{self.data.number}),
            .@"enum" => try writer.print("{s}.{s}", .{ self.data.@"enum".name.ptr[0..self.data.@"enum".name.len], self.data.@"enum".value.ptr[0..self.data.@"enum".value.len] }),
            .string => try writer.print("{s}", .{self.data.string.ptr[0..self.data.string.len]}),
            .list => {
                try writer.print("List{{", .{});
                for (0..self.data.list.count) |i| {
                    try self.data.list.items[i].print(writer);
                    if (i < self.data.list.count - 1) try writer.print(", ", .{});
                }
                try writer.print("}}", .{});
            },
            .set => {
                try writer.print("Set{{", .{});
                for (0..self.data.list.count) |i| {
                    try self.data.list.items[i].print(writer);
                    if (i < self.data.list.count - 1) try writer.print(", ", .{});
                }
                try writer.print("}}", .{});
            },
            .map => {
                try writer.print("Map{{", .{});
                var i: usize = 0;
                while (i < self.data.list.count) : (i += 2) {
                    try self.data.list.items[i].print(writer);
                    try writer.print(":", .{});
                    try self.data.list.items[i + 1].print(writer);
                    if (i < self.data.list.count - 2) try writer.print(", ", .{});
                }
                try writer.print("}}", .{});
            },
        }
    }

    pub fn deinit(self: *const ExportValue, allocator: std.mem.Allocator) void {
        switch (self.tag) {
            .list, .set, .map => {
                var count = self.data.list.count;
                if (self.tag == .map) count *= 2;
                for (self.data.list.items[0..count]) |item| {
                    item.deinit(allocator);
                }
                allocator.free(self.data.list.items[0..count]);
            },
            else => {},
        }
    }
};
