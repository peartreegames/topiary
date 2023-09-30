const std = @import("std");
const Value = @import("./values.zig").Value;
const OpCode = @import("./opcode.zig").OpCode;

pub const Subscriber = struct {
    subscribers: ?std.ArrayList(Delegate) = null,
    allocator: std.mem.Allocator,

    pub const Delegate = struct {
        callback: OnValueChanged,
        /// Address of the containing context
        context_ptr: usize = 0,
        /// Used for any deallocation that may be needed
        onUnsubscribe: *const fn (context_ptr: usize, allocator: std.mem.Allocator) void = &Delegate.NoOp,

        pub fn NoOp(_: usize, _: std.mem.Allocator) void {}
    };

    pub const OnValueChanged = *const fn (context_ptr: usize, value: Value) void;
    pub fn init(allocator: std.mem.Allocator) Subscriber {
        return .{
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *Subscriber) void {
        if (self.subscribers) |*subs| subs.deinit();
    }

    pub fn hasSubscribers(self: *Subscriber) bool {
        return self.subscribers != null;
    }

    pub fn subscribe(self: *Subscriber, delegate: Delegate) !void {
        if (self.subscribers == null) self.subscribers = std.ArrayList(Delegate).init(self.allocator);
        if (self.subscribers) |*subs| try subs.*.append(delegate);
    }

    pub fn unsubscribe(self: *Subscriber, delegate: Delegate) void {
        if (self.subscribers) |*subs| {
            for (subs.items, 0..) |sub, i| {
                if (sub.context_ptr != delegate.context_ptr and @intFromPtr(sub.callback) != @intFromPtr(delegate.callback)) continue;
                if (sub.onUnsubscribe != &Delegate.NoOp) {
                    sub.onUnsubscribe(sub.context_ptr, self.allocator);
                }
                _ = subs.*.swapRemove(i);
                break;
            }
            if (subs.items.len == 0) {
                subs.deinit();
                self.subscribers = null;
            }
        }
    }

    pub fn invoke(self: *Subscriber, value: Value) void {
        if (self.subscribers) |subs| {
            for (subs.items) |sub| sub.callback(sub.context_ptr, value);
        }
    }
};
