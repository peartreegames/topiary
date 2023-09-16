const std = @import("std");
const Value = @import("./values.zig").Value;
const OpCode = @import("./opcode.zig").OpCode;

pub const Extern = struct {
    subscribers: ?std.ArrayList(OnValueChanged),

    pub const OnValueChanged = *const fn (value: Value) void;
    pub fn init(allocator: std.mem.Allocator, is_extern: bool) Extern {
        return .{
            .subscribers = if (is_extern) std.ArrayList(Extern.OnValueChanged).init(allocator) else null,
        };
    }

    pub fn deinit(self: *Extern) void {
        if (self.subscribers) |*subs| subs.deinit();
    }

    pub fn isExtern(self: *Extern) bool {
        return self.subscribers != null;
    }

    pub fn subscribe(self: *Extern, callback: OnValueChanged) !void {
        if (self.subscribers) |*subs| try subs.*.append(callback);
    }

    pub fn unsubscribe(self: *Extern, callback: OnValueChanged) void {
        if (self.subscribers) |*subs| {
            for (subs.items, 0..) |sub, i| {
                if (sub != callback) continue;
                _ = subs.*.swapRemove(i);
                break;
            }
        }
    }

    pub fn invoke(self: *Extern, value: Value) void {
        if (self.subscribers) |subs| {
            for (subs.items) |sub| sub(value);
        }
    }
};
