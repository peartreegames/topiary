const std = @import("std");
const Value = @import("./values.zig").Value;
const OpCode = @import("./opcode.zig").OpCode;

pub const ExternList = struct {
    allocator: std.mem.Allocator,
    externs: std.ArrayList(Extern),
    names: std.StringHashMap(usize),
    indexes: std.AutoHashMap(global_id_size, usize), // globals index <> externs index

    const global_id_size = OpCode.Size(.set_global);
    pub fn init(allocator: std.mem.Allocator) ExternList {
        return .{
            .allocator = allocator,
            .externs = std.ArrayList(Extern).init(allocator),
            .names = std.StringHashMap(usize).init(allocator),
            .indexes = std.AutoHashMap(global_id_size, usize).init(allocator),
        };
    }

    pub fn deinit(self: *ExternList) void {
        for (self.externs.items) |e| e.subscribers.deinit();
        self.externs.deinit();
        self.names.deinit();
        self.indexes.deinit();
    }

    pub fn getByName(self: *ExternList, name: []const u8) ?*Extern {
        var index = self.names.get(name);
        if (index) |i| return &self.externs.items[i];
        return null;
    }

    pub fn getByIndex(self: *ExternList, globals_index: global_id_size) ?*Extern {
        var index = self.indexes.get(globals_index);
        if (index) |i| return &self.externs.items[i];
        return null;
    }

    pub fn append(self: *ExternList, name: []const u8, globals_index: global_id_size) !void {
        var externs_index = self.externs.items.len;
        try self.externs.append(.{
            .allocator = self.allocator,
            .globals_index = globals_index,
            .subscribers = std.ArrayList(Extern.OnValueChanged).init(self.allocator),
        });
        try self.names.put(name, externs_index);
        try self.indexes.put(globals_index, externs_index);
    }
};

pub const Extern = struct {
    allocator: std.mem.Allocator,
    globals_index: u16,
    subscribers: std.ArrayList(OnValueChanged),

    pub const OnValueChanged = *const fn (value: Value) void;

    pub fn subscribe(self: *Extern, callback: OnValueChanged) !void {
        try self.subscribers.append(callback);
    }

    pub fn unsubscribe(self: *Extern, callback: OnValueChanged) void {
        _ = self.subscribers.swapRemove(callback);
    }

    pub fn set(self: *Extern, globals: *std.ArrayList(Value), value: Value) void {
        if (globals.items[self.globals_index].eql(value)) return;
        globals.items[self.globals_index] = value;

        for (self.subscribers.items) |sub| {
            sub(value);
        }
    }

    pub fn setWithoutNotify(self: *Extern, globals: *std.ArrayList(Value), value: Value) void {
        globals.items[self.globals_index] = value;
    }
};
