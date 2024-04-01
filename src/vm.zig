const std = @import("std");
const Compiler = @import("./compiler.zig").Compiler;
const Bytecode = @import("./bytecode.zig").Bytecode;
const parser = @import("./parser.zig");
const OpCode = @import("./opcode.zig").OpCode;
const values = @import("./values.zig");
const Stack = @import("./structures/stack.zig").Stack;
const Gc = @import("./gc.zig").Gc;
const Token = @import("./token.zig").Token;
const Frame = @import("./frame.zig").Frame;
const Class = @import("./class.zig").Class;
const builtins = @import("./builtins.zig");
const Rnd = @import("./builtins.zig").Rnd;
const Subscriber = @import("./subscriber.zig").Subscriber;
const StateMap = @import("./state.zig").StateMap;
const runners = @import("./runner.zig");
const Runner = runners.Runner;
const Dialogue = runners.Dialogue;
const Choice = runners.Choice;

test {
    _ = @import("./vm.test.zig");
    std.testing.refAllDeclsRecursive(@This());
}

const stack_size = 4096;
const frame_size = 255;
const iterator_size = 255;
const globals_size = 65535;
const Value = values.Value;
const Iterator = values.Iterator;
const adapter = values.adapter;

const input_buf: [2]u8 = undefined;

const InterpretResult = union(enum) {
    Completed,
    Paused,
};

pub const RuntimeErr = struct {
    line: u32 = 0,
    msg: ?[]const u8 = null,
    pub fn print(self: @This(), writer: anytype) void {
        if (self.msg) |m| {
            writer.print("Error at line {}: {s}\n", .{ self.line, m }) catch {};
        }
    }
};

pub const Vm = struct {
    allocator: std.mem.Allocator,
    frames: Stack(Frame),
    err: RuntimeErr,
    gc: Gc,
    globals: []Value,
    subscribers: []Subscriber,

    stack: Stack(Value),
    /// Current iterators to allow easy nesting
    iterators: Stack(Iterator),
    /// List of positions to jump back to using `^`
    jump_backups: std.ArrayList(OpCode.Size(.jump)),
    /// Used to ensure preceeding code is executed before arriving at Bough
    jump_requests: std.ArrayList(OpCode.Size(.jump)),

    bytecode: Bytecode,
    /// Current instruction position
    ip: usize = 0,
    debug: bool = false,

    /// Used to cache the choices
    choices_list: std.ArrayList(Choice),
    /// Used to send to the on_choices method
    current_choices: []Choice = undefined,
    /// Determines if the vm is waiting on input
    /// hopefully will remove with async?
    is_waiting: bool = false,
    can_continue: bool = false,
    runner: *Runner,

    pub const Error = error{
        RuntimeError,
        BoughNotFound,
        InvalidChoice,
        Uninitialized,
    } || Compiler.Error;

    pub fn init(allocator: std.mem.Allocator, bytecode: Bytecode, runner: anytype) !Vm {
        var globals = try allocator.alloc(Value, bytecode.global_symbols.len);
        var subs = try allocator.alloc(Subscriber, bytecode.global_symbols.len);

        var i: usize = 0;
        while (i < bytecode.global_symbols.len) : (i += 1) {
            globals[i] = .void;
            subs[i] = Subscriber.init(allocator);
        }

        const root_frame = try allocator.create(Value.Obj.Data);
        const root_closure = try allocator.create(Value.Obj);
        root_frame.* = .{
            .function = .{
                .instructions = bytecode.instructions,
                .lines = bytecode.token_lines,
                .locals_count = 0,
                .arity = 0,
            },
        };
        root_closure.* = .{
            .data = .{
                .closure = .{
                    .data = root_frame,
                    .free_values = &[_]Value{},
                },
            },
        };

        var vm = Vm{
            .allocator = allocator,
            .bytecode = bytecode,
            .frames = try Stack(Frame).init(allocator, frame_size),
            .err = .{},
            .globals = globals,
            .subscribers = subs,
            .runner = runner,
            .gc = Gc.init(allocator),
            .stack = try Stack(Value).init(allocator, stack_size),
            .iterators = try Stack(Iterator).init(allocator, iterator_size),
            .jump_backups = std.ArrayList(OpCode.Size(.jump)).init(allocator),
            .jump_requests = std.ArrayList(OpCode.Size(.jump)).init(allocator),
            .choices_list = std.ArrayList(Choice).init(allocator),
        };

        vm.stack.resize(bytecode.locals_count);
        vm.frames.push(try Frame.create(root_closure, 0, 0));
        return vm;
    }

    pub fn deinit(self: *Vm) void {
        self.allocator.destroy(self.currentFrame().cl.data.closure.data);
        Value.Obj.destroy(self.allocator, self.currentFrame().cl);
        self.choices_list.deinit();
        self.stack.deinit();
        self.iterators.deinit();
        self.jump_backups.deinit();
        self.jump_requests.deinit();
        self.frames.deinit();
        self.gc.deinit();
        for (self.subscribers) |*sub| sub.deinit();
        self.allocator.free(self.subscribers);
        self.allocator.free(self.globals);
    }

    /// Add the current state to a StateMap
    // pub fn serializeState(self: *Vm) ![]const u8 {
    //     var map = StateMap.init(self.allocator);
    //     for (self.bytecode.global_symbols) |s| {
    //         if (s.is_extern or self.globals[s.index] == .void) continue;
    //         const value = self.globals[s.index];
    //         if (value == .visit and value.visit == 0) continue;
    //         try map.put(s.name, self.globals[s.index]);
    //     }
    // }

    /// Load the StateMap into the globals list
    // pub fn loadState(self: *Vm, json_string: []const u8) void {
    //     for (self.bytecode.global_symbols) |s| {
    //         const value = state.get(s.name);
    //         if (value) |v| self.globals[s.index] = v;
    //     }
    // }

    pub fn roots(self: *Vm) []const []Value {
        return &([_][]Value{ self.globals, self.stack.backing });
    }

    fn currentFrame(self: *Vm) *Frame {
        return self.frames.peek();
    }

    pub fn selectContinue(self: *Vm) void {
        self.is_waiting = false;
    }

    pub fn selectChoice(self: *Vm, index: usize) Error!void {
        if (index < 0 or index >= self.current_choices.len) {
            return Error.InvalidChoice;
        }

        const choice = self.current_choices[index];
        self.currentFrame().ip = choice.ip;
        self.is_waiting = false;
        for (self.current_choices) |c| {
            self.allocator.free(c.tags);
        }
        self.allocator.free(self.current_choices);
    }

    pub fn getGlobalsIndex(self: *Vm, name: []const u8) !usize {
        for (self.bytecode.global_symbols) |s| {
            if (!std.mem.eql(u8, name, s.name)) continue;
            return s.index;
        }
        return error.SymbolNotFound;
    }

    fn getExternIndex(self: *Vm, name: []const u8) !usize {
        const index = try self.getGlobalsIndex(name);
        if (!self.bytecode.global_symbols[index].is_extern) return error.IllegalOperation;
        return index;
    }

    pub fn setExtern(self: *Vm, name: []const u8, value: Value) !void {
        const index = try self.getExternIndex(name);
        self.globals[index] = value;
    }

    pub fn getExtern(self: *Vm, name: []const u8) !Value {
        const index = self.getExternIndex(name) catch |err| {
            if (err == Error.IllegalOperation) return err;
            return values.Nil;
        };
        return self.globals[index];
    }

    pub fn subscribeCallback(self: *Vm, name: []const u8, callback: Subscriber.OnValueChanged) !void {
        try self.subscribers[try self.getGlobalsIndex(name)].subscribe(.{ .callback = callback });
    }

    pub fn subscribeDelegate(self: *Vm, name: []const u8, delegate: Subscriber.Delegate) !void {
        try self.subscribers[try self.getGlobalsIndex(name)].subscribe(delegate);
    }

    pub fn unsubscribeCallback(self: *Vm, name: []const u8, callback: Subscriber.OnValueChanged) !void {
        self.subscribers[try self.getGlobalsIndex(name)].unsubscribe(.{ .callback = callback });
    }

    pub fn unsubscribeDelegate(self: *Vm, name: []const u8, delegate: Subscriber.Delegate) !void {
        self.subscribers[try self.getGlobalsIndex(name)].unsubscribe(delegate);
    }

    pub fn interpret(self: *Vm) !void {
        const path = if (self.bytecode.boughs.len == 0) null else self.bytecode.boughs[0].name;
        try self.start(path);
        while (self.can_continue) {
            try self.run();
        }
        // if (self.stack.count > self.bytecode.locals_count) {
        //     var count = self.stack.count - self.bytecode.locals_count;
        //     std.log.warn("Completed run but still had {} items on stack.", .{count});
        //     self.stack.print(std.debug, count);
        // }
    }

    pub fn start(self: *Vm, bough_path: ?[]const u8) !void {
        self.can_continue = true;
        self.stack.resize(self.bytecode.locals_count);
        while (self.frames.count > 1) {
            _ = self.frames.pop();
        }
        self.currentFrame().ip = 0;
        if (bough_path) |path| {
            var split_it = std.mem.split(u8, path, ".");
            var path_parts = std.ArrayList([]const u8).init(self.allocator);
            defer path_parts.deinit();
            while (split_it.next()) |split| {
                try path_parts.append(split);
                const current_path = try std.mem.join(self.allocator, ".", path_parts.items);
                defer self.allocator.free(current_path);
                var found = false;
                for (self.bytecode.boughs) |b| {
                    if (std.mem.eql(u8, current_path, b.name)) {
                        try self.jump_requests.append(b.ip);
                        found = true;
                        break;
                    }
                }
                if (!found) {
                    self.err.msg = "Could not find starting path";
                    self.err.line = 0;
                    return Error.BoughNotFound;
                }
            }
        }
        std.mem.reverse(OpCode.Size(.jump), self.jump_requests.items);
    }

    fn fail(self: *Vm, comptime msg: []const u8, args: anytype) !void {
        self.err.msg = try std.fmt.allocPrint(self.allocator, msg, args);
        self.err.line = self.currentFrame().cl.data.closure.data.function.lines[self.currentFrame().ip];
        return Error.RuntimeError;
    }

    fn readByte(self: *Vm) u8 {
        var frame = self.currentFrame();
        const byte = frame.instructions()[frame.ip];
        frame.ip += 1;
        return byte;
    }

    fn readInt(self: *Vm, comptime T: type) T {
        var frame = self.currentFrame();
        if (T == u8) {
            return self.readByte();
        } else {
            const result = std.mem.readVarInt(T, frame.instructions()[frame.ip..(frame.ip + @sizeOf(T))], .little);
            frame.ip += @sizeOf(T);
            return result;
        }
    }

    fn end(self: *Vm) void {
        self.can_continue = false;
    }

    pub fn run(self: *Vm) !void {
        if (!self.can_continue or self.is_waiting) return;
        while (self.ip < self.currentFrame().instructions().len) : (self.ip = self.currentFrame().ip) {
            const instruction = self.readByte();
            if (instruction == 170) {
                self.end();
                return;
            }
            const op: OpCode = @enumFromInt(instruction);
            const is_in_jump = self.jump_requests.items.len > 0;

            switch (op) {
                .constant => {
                    const index = self.readInt(OpCode.Size(.constant));
                    const value = self.bytecode.constants[index];
                    try self.push(value);
                },
                .pop => _ = self.pop(),
                .add => {
                    const right = self.pop();
                    const left = self.pop();
                    if (@intFromEnum(right) != @intFromEnum(left)) {
                        return self.fail("Cannot add types {s} and {s}", .{ left.typeName(), right.typeName() });
                    }
                    switch (right) {
                        .number => try self.push(.{ .number = right.number + left.number }),
                        .obj => |o| {
                            switch (o.data) {
                                .string => |s| try self.pushAlloc(.{ .string = try std.mem.concat(self.allocator, u8, &.{ left.obj.data.string, s }) }),
                                else => return self.fail("Cannot add types {s} and {s}", .{ left.typeName(), right.typeName() }),
                            }
                        },
                        else => return self.fail("Cannot add types {s} and {s}", .{ left.typeName(), right.typeName() }),
                    }
                },
                .subtract, .multiply, .divide, .modulus => try self.binaryNumberOp(op),
                .equal, .not_equal, .greater_than => try self.comparisonOp(op),
                .true => try self.push(values.True),
                .false => try self.push(values.False),
                .nil => try self.push(values.Nil),
                .negate => try self.push(.{ .number = -self.pop().number }),
                .not => {
                    const value = self.pop();
                    switch (value) {
                        .bool => |b| try self.push(if (b) values.False else values.True),
                        .nil => try self.push(values.True),
                        .number => |n| try self.push(if (@abs(n) < 0.00001) values.False else values.True),
                        else => try self.push(values.False),
                    }
                },
                .@"or" => {
                    const right = self.pop();
                    const left = self.pop();
                    if (right != .bool or left != .bool) {
                        return self.fail("Conditionals must be of type bool not types {s} and {s}", .{ left.typeName(), right.typeName() });
                    }
                    try self.push(.{ .bool = right.bool or left.bool });
                },
                .@"and" => {
                    const right = self.pop();
                    const left = self.pop();
                    if (right != .bool or left != .bool) {
                        return self.fail("Conditionals must be of type bool not types {s} and {s}", .{ left.typeName(), right.typeName() });
                    }
                    try self.push(.{ .bool = right.bool and left.bool });
                },
                .jump => {
                    const dest = self.readInt(OpCode.Size(.jump));
                    if (dest > self.currentFrame().instructions().len) break;
                    self.currentFrame().ip = dest;
                },
                .jump_if_false => {
                    const dest = self.readInt(OpCode.Size(.jump_if_false));
                    var condition = self.pop();
                    if (!try condition.isTruthy()) {
                        self.currentFrame().ip = dest;
                    }
                },
                .prong => {
                    const dest = self.readInt(OpCode.Size(.jump));
                    const values_count = self.readInt(u8);
                    const capture = self.stack.items[self.stack.count - values_count - 1];
                    var i: usize = 0;
                    var match = false;
                    while (i < values_count) : (i += 1) {
                        const value = self.pop();
                        if (!self.prongIsMatch(capture, value)) continue;
                        match = true;
                        self.stack.count -= values_count - i - 1;
                        break;
                    }

                    if (match or values_count == 0) {
                        self.currentFrame().ip = dest;
                    }
                },
                .decl_global => {
                    const index = self.readInt(OpCode.Size(.get_global));
                    if (index > globals_size) return self.fail("Globals index {} is out of bounds of max size", .{index});
                    const value = self.pop();
                    // global already set from loaded state
                    if (self.globals[index] != .void) {
                        continue;
                    }
                    self.globals[index] = value;
                },
                .set_global => {
                    const index = self.readInt(OpCode.Size(.set_global));
                    if (index > globals_size) return self.fail("Globals index {} is out of bounds of max size", .{index});

                    if (index >= self.globals.len) return self.fail("Globals index {} is out of bounds of current size {}", .{ index, self.globals.len });
                    const value = self.pop();
                    self.globals[index] = value;
                    self.subscribers[index].invoke(value);
                },
                .get_global => {
                    const index = self.readInt(OpCode.Size(.get_global));
                    const value = self.globals[index];
                    try self.push(value);
                },
                .set_local => {
                    const index = self.readInt(OpCode.Size(.set_local));
                    const frame = self.currentFrame();
                    self.stack.items[frame.bp + index] = self.pop();
                },
                .get_local => {
                    const index = self.readInt(OpCode.Size(.get_local));
                    const frame = self.currentFrame();
                    const value = self.stack.items[frame.bp + index];
                    try self.push(value);
                },
                .set_property => {
                    const field_value = self.pop();
                    const instance_value = self.pop();
                    const new_value = self.pop();

                    switch (instance_value.obj.data) {
                        .list => |l| {
                            const n: usize = @intFromFloat(field_value.number);
                            if (n >= l.items.len) return self.fail("Index {d} out of bounds for list of length {d}", .{ n, l.items.len });
                            l.items[n] = new_value;
                        },
                        .map => {
                            var m = instance_value.obj.data.map;
                            try m.put(field_value, new_value);
                        },
                        .instance => {
                            var i = instance_value.obj.data.instance;
                            const field_name = field_value.obj.data.string;
                            if (!i.fields.contains(field_name))
                                return self.fail("Instance of {s} does not contain {s}", .{ i.class.name, field_name });
                            try i.fields.put(field_name, new_value);
                        },
                        // todo add string indexing
                        else => return self.fail("Cannot index {s} into type {s}", .{ @tagName(field_value), @tagName(instance_value.obj.data) }),
                    }
                },
                .get_builtin => {
                    const index = self.readInt(OpCode.Size(.get_builtin));
                    const value = builtins.builtins[index].value;
                    try self.push(value.*);
                },
                .get_free => {
                    const index = self.readInt(OpCode.Size(.get_free));
                    const obj = self.currentFrame().cl;
                    try self.push(obj.data.closure.free_values[index]);
                },
                .set_free => {
                    const index = self.readInt(OpCode.Size(.get_free));
                    var obj = self.currentFrame().cl;
                    obj.data.closure.free_values[index] = self.pop();
                },
                .string => {
                    const index = self.readInt(OpCode.Size(.constant));
                    const value = self.bytecode.constants[index];
                    var count = self.readInt(u8);
                    var args = try std.ArrayList(Value).initCapacity(self.allocator, count);
                    defer args.deinit();
                    while (count > 0) : (count -= 1) {
                        try args.append(self.pop());
                    }
                    std.mem.reverse(Value, args.items);
                    var list = std.ArrayList(u8).init(self.allocator);
                    defer list.deinit();
                    var writer = list.writer();
                    // index
                    var i: usize = 0;
                    var a: usize = 0;
                    // start
                    var s: usize = 0;
                    const str = value.obj.*.data.string;
                    while (i < str.len) : (i += 1) {
                        const c = str[i];
                        if (c == '{') {
                            try writer.writeAll(str[s..i]);
                            switch (args.items[a]) {
                                .number => |n| try writer.print("{d:.5}", .{n}),
                                .bool => |b| try writer.writeAll(if (b) "true" else "false"),
                                .obj => |o| try writer.writeAll(o.data.string),
                                .visit => |v| try std.fmt.formatIntValue(v, "", .{}, list.writer()),
                                else => return self.fail("Unsupported interpolated type {s} for {s}", .{ args.items[a].typeName(), str }),
                            }
                            i += 1;
                            s = i + 1;
                            a += 1;
                        }
                    }
                    try writer.writeAll(str[s..]);
                    try list.append(0);
                    try self.pushAlloc(.{ .string = try list.toOwnedSlice() });
                },
                .list => {
                    var count = self.readInt(OpCode.Size(.list));
                    var list = try std.ArrayList(Value).initCapacity(self.allocator, count);
                    while (count > 0) : (count -= 1) {
                        try list.append(self.pop());
                    }
                    std.mem.reverse(Value, list.items);
                    try self.pushAlloc(.{ .list = list });
                },
                .map => {
                    var count = self.readInt(OpCode.Size(.map));
                    var map = Value.Obj.MapType.initContext(self.allocator, adapter);
                    while (count > 0) : (count -= 1) {
                        const value = self.pop();
                        const key = self.pop();
                        try map.put(key, value);
                    }
                    map.sort(adapter);
                    try self.pushAlloc(.{ .map = map });
                },
                .set => {
                    var count = self.readInt(OpCode.Size(.set));
                    var set = Value.Obj.SetType.initContext(self.allocator, adapter);
                    while (count > 0) : (count -= 1) {
                        try set.put(self.pop(), {});
                    }
                    set.sort(adapter);
                    try self.pushAlloc(.{ .set = set });
                },
                .iter_start => {
                    var value = self.pop();
                    self.iterators.push(.{
                        .value = value,
                        .index = 0,
                    });
                    if (value.len() > 0) try self.push(value.getAtIndex(0));
                },
                .iter_next => {
                    const iter = self.iterators.peek().*;
                    var value = iter.value;
                    if (iter.index >= value.len()) {
                        try self.push(values.Nil);
                        try self.push(values.False);
                    } else {
                        try self.push(value.getAtIndex(iter.index));
                        try self.push(values.True);
                    }
                    self.iterators.peek().index += 1;
                },
                .iter_end => {
                    _ = self.iterators.pop();
                },
                .class => {
                    const value = self.pop();
                    var count = self.readInt(OpCode.Size(.class));
                    var fields = std.ArrayList(Class.Field).init(self.allocator);
                    errdefer fields.deinit();
                    while (count > 0) : (count -= 1) {
                        const name = self.pop();
                        const field_name = name.obj.data.string;
                        const field_value = self.pop();
                        try fields.append(.{
                            .name = field_name,
                            .value = field_value,
                        });
                    }

                    std.mem.reverse(Class.Field, fields.items);
                    const class = try Class.init(self.allocator, value.obj.data.string, try fields.toOwnedSlice());
                    try self.pushAlloc(.{ .class = class });
                },
                .instance => {
                    const value = self.pop();
                    var class = value.obj.data.class;

                    var count = self.readInt(OpCode.Size(.class));
                    var fields = std.ArrayList(Class.Field).init(self.allocator);
                    defer fields.deinit();
                    while (count > 0) : (count -= 1) {
                        const name = self.pop();
                        const str_name = name.obj.data.string;
                        const field = self.pop();
                        try fields.append(.{
                            .name = str_name,
                            .value = field,
                        });
                    }
                    std.mem.reverse(Class.Field, fields.items);
                    const instance = try class.createInstance(try fields.toOwnedSlice());
                    try self.pushAlloc(.{ .instance = instance });
                },
                .range => {
                    const left = self.pop();
                    const right = self.pop();
                    try self.push(.{
                        .range = .{
                            .start = @as(i32, @intFromFloat(left.number)),
                            .end = @as(i32, @intFromFloat(right.number)),
                        },
                    });
                },
                .index => {
                    const index = self.pop();
                    const target = self.pop();
                    switch (target) {
                        .obj => |o| switch (o.data) {
                            // TODO: Will want to clean this up.
                            // Also the fails here should be caught in the compiler
                            // but that's for another day
                            .string => {
                                if (index == .obj and index.obj.data == .string) {
                                    const name = index.obj.data.string;
                                    if (std.mem.eql(u8, name, "has")) {
                                        try self.push(builtins.Has.value);
                                        try self.push(target);
                                    } else if (std.mem.eql(u8, name, "count")) {
                                        try self.push(.{ .number = @as(f32, @floatFromInt(o.data.string.len)) });
                                    } else return self.fail("Unknown method \"{s}\" on string. Only \"count\", \"has\" are allowed.", .{index.obj.data.string});
                                }
                            },
                            .list => |l| {
                                if (index == .obj and index.obj.data == .string) {
                                    const name = index.obj.data.string;
                                    if (std.mem.eql(u8, name, "count")) {
                                        try self.push(builtins.Count.value);
                                        try self.push(target);
                                    } else if (std.mem.eql(u8, name, "add")) {
                                        try self.push(builtins.Add.value);
                                        try self.push(target);
                                    } else if (std.mem.eql(u8, name, "remove")) {
                                        try self.push(builtins.Remove.value);
                                        try self.push(target);
                                    } else if (std.mem.eql(u8, name, "has")) {
                                        try self.push(builtins.Has.value);
                                        try self.push(target);
                                    } else if (std.mem.eql(u8, name, "clear")) {
                                        try self.push(builtins.Clear.value);
                                        try self.push(target);
                                    } else return self.fail("Unknown method \"{s}\" on list. Only \"count\", \"add\", \"remove\", \"has\", or \"clear\" are allowed.", .{index.obj.data.string});
                                } else if (index == .number) {
                                    const i = @as(u32, @intFromFloat(index.number));
                                    if (i < 0 or i >= l.items.len) {
                                        try self.push(values.Nil);
                                    } else try self.push(l.items[i]);
                                } else try self.push(values.Nil);
                            },
                            .map => |m| {
                                if (m.get(index)) |v| {
                                    try self.push(v);
                                } else if (index == .obj and index.obj.data == .string) {
                                    const name = index.obj.data.string;
                                    if (std.mem.eql(u8, name, "count")) {
                                        try self.push(builtins.Count.value);
                                        try self.push(target);
                                    } else if (std.mem.eql(u8, name, "add")) {
                                        try self.push(builtins.AddMap.value);
                                        try self.push(target);
                                    } else if (std.mem.eql(u8, name, "remove")) {
                                        try self.push(builtins.Remove.value);
                                        try self.push(target);
                                    } else if (std.mem.eql(u8, name, "has")) {
                                        try self.push(builtins.Has.value);
                                        try self.push(target);
                                    } else if (std.mem.eql(u8, name, "clear")) {
                                        try self.push(builtins.Clear.value);
                                        try self.push(target);
                                    } else return self.fail("Unknown method \"{s}\" on map. Only \"count\", \"add\", \"remove\", \"has\", or \"clear\" are allowed.", .{index.obj.data.string});
                                } else try self.push(values.Nil);
                            },
                            .set => {
                                if (index != .obj)
                                    return self.fail("Can only query set methods by string name, not {s}", .{@tagName(index)});
                                if (index.obj.data != .string)
                                    return self.fail("Can only query set methods by string name, not {s}", .{@tagName(index.obj.data)});
                                const name = index.obj.data.string;
                                if (std.mem.eql(u8, name, "count")) {
                                    try self.push(builtins.Count.value);
                                    try self.push(target);
                                } else if (std.mem.eql(u8, name, "add")) {
                                    try self.push(builtins.Add.value);
                                    try self.push(target);
                                } else if (std.mem.eql(u8, name, "remove")) {
                                    try self.push(builtins.Remove.value);
                                    try self.push(target);
                                } else if (std.mem.eql(u8, name, "has")) {
                                    try self.push(builtins.Has.value);
                                    try self.push(target);
                                } else if (std.mem.eql(u8, name, "clear")) {
                                    try self.push(builtins.Clear.value);
                                    try self.push(target);
                                } else return self.fail("Unknown method \"{s}\" on set. Only \"count\", \"add\", \"remove\", \"has\", or \"clear\" are allowed.", .{index.obj.data.string});
                            },
                            .instance => |i| {
                                if (index != .obj)
                                    return self.fail("Can only query instance fields by string name, not {s}", .{@tagName(index)});
                                if (index.obj.data != .string)
                                    return self.fail("Can only query instance fields by string name, not {s}", .{@tagName(index.obj.data)});
                                if (i.fields.get(index.obj.data.string)) |field| {
                                    try self.push(field);
                                    if (field == .obj and field.obj.data == .closure) {
                                        try self.push(target);
                                    }
                                } else return self.fail("Unknown field \"{s}\" on instance of {s}.", .{ index.obj.data.string, i.class.name });
                            },
                            .@"enum" => |e| {
                                if (index != .obj)
                                    return self.fail("Can only query instance fields by string name, not {s}", .{@tagName(index)});
                                if (index.obj.data != .string)
                                    return self.fail("Can only query instance fields by string name, not {s}", .{@tagName(index.obj.data)});
                                var found = false;
                                for (e.values, 0..) |name, i| {
                                    if (std.mem.eql(u8, name, index.obj.data.string)) {
                                        try self.push(.{ .enum_value = .{ .index = @intCast(i), .base = &target } });
                                        found = true;
                                    }
                                }
                                if (!found) return self.fail("Unknown value \"{s}\" on enum {s}", .{ index.obj.data.string, e.name });
                            },
                            else => return self.fail("Unknown target type {s} to index. Only lists, maps, sets, or instances can be indexed.", .{@tagName(target)}),
                        },
                        .map_pair => |mp| {
                            if (index != .obj)
                                return self.fail("Unknown index key \"{s}\" on map key/value pair. Only \"key\" or \"value\" are allowed.", .{@tagName(index)});
                            if (index.obj.data != .string)
                                return self.fail("Unknown index key \"{s}\" on map key/value pair. Only \"key\" or \"value\" are allowed.", .{@tagName(index.obj.data)});
                            const name = index.obj.data.string;
                            if (std.mem.eql(u8, name, "key")) {
                                try self.push(mp.key.*);
                            } else if (std.mem.eql(u8, name, "value")) {
                                try self.push(mp.value.*);
                            } else return self.fail("Unknown index key \"{s}\" on map key/value pair. Only \"key\" or \"value\" are allowed.", .{@tagName(index)});
                        },
                        else => {
                            return self.fail("Invalid index \"{s}\" on target type \"{s}\"", .{ @tagName(index), @tagName(target) });
                        },
                    }
                },
                .dialogue => {
                    const has_speaker = self.readInt(u8) == 1;
                    var speaker: ?[]const u8 = null;
                    if (has_speaker) {
                        const speaker_value = self.pop();
                        speaker = speaker_value.obj.data.string;
                    }

                    const dialogue_value = self.pop();

                    const tag_count = self.readInt(u8);
                    var tags = try self.allocator.alloc([]const u8, tag_count);
                    defer self.allocator.free(tags);
                    var i: usize = 0;
                    while (i < tag_count) : (i += 1) {
                        const tag_value = self.pop();
                        tags[tag_count - i - 1] = tag_value.obj.data.string;
                    }
                    const id_index = self.readInt(OpCode.Size(.constant));
                    if (is_in_jump) continue;
                    self.is_waiting = true;
                    self.runner.onDialogue(self, .{
                        .content = dialogue_value.obj.data.string,
                        .speaker = speaker,
                        .tags = tags,
                        .id = self.bytecode.uuids[id_index],
                    });
                    return;
                },
                .call => {
                    const arg_count = self.readInt(OpCode.Size(.call));
                    const value = self.stack.items[self.stack.count - 1 - arg_count];
                    if (value != .obj)
                        return self.fail("Cannot call non-function type {s}", .{@tagName(value.obj.data)});
                    switch (value.obj.data) {
                        .closure => |c| {
                            const f = c.data.function;
                            if (f.arity != arg_count) {
                                return self.fail(
                                    "Function expected {} arguments, but found {}",
                                    .{ f.arity, arg_count },
                                );
                            }
                            const frame = try Frame.create(value.obj, 0, self.stack.count - arg_count);
                            self.frames.push(frame);
                            self.stack.count = frame.bp + f.locals_count;
                        },
                        .builtin => |b| {
                            if (b.arity != arg_count)
                                return self.fail(
                                    "Builtin Function expected {} arguments, but found {}",
                                    .{ b.arity, arg_count },
                                );
                            const result = b.backing(&self.gc, self.stack.items[self.stack.count - arg_count .. self.stack.count]);
                            self.stack.count -= arg_count + 1;
                            try self.push(result);
                        },
                        .ext_function => |e| {
                            if (e.arity != arg_count)
                                return self.fail(
                                    "Extern Function expected {} arguments, but found {}",
                                    .{ e.arity, arg_count },
                                );
                            const result = e.backing(e.context_ptr, self.stack.items[self.stack.count - arg_count .. self.stack.count]);
                            self.stack.count -= arg_count + 1;
                            try self.push(result);
                        },
                        else => {
                            return self.fail("Cannot call non-function type {s}", .{@tagName(value.obj.data)});
                        },
                    }
                },
                .closure => {
                    const index = self.readInt(OpCode.Size(.constant));
                    const count = self.readInt(u8);
                    var value = self.bytecode.constants[index];
                    var free_values = try self.allocator.alloc(Value, count);
                    for (0..count) |i| {
                        free_values[i] = self.stack.items[self.stack.count - count + i];
                    }
                    const closure = try self.gc.create(self, .{
                        .closure = .{
                            .data = &value.obj.data,
                            .free_values = free_values,
                        },
                    });
                    const reset_count = self.stack.count - count;
                    self.stack.count = reset_count;
                    try self.push(closure);
                },
                .current_closure => {
                    const current = self.currentFrame().cl;
                    try self.push(.{ .obj = current });
                },
                .return_value => {
                    const value = self.pop();
                    const frame = self.frames.pop();
                    self.stack.count = frame.bp - 1;
                    try self.push(value);
                },
                .return_void => {
                    const frame = self.frames.pop();
                    self.stack.count = frame.bp - 1;
                    try self.push(values.Void);
                },
                .fork => {
                    if (is_in_jump) continue;
                    self.is_waiting = true;
                    self.current_choices = try self.choices_list.toOwnedSlice();
                    self.runner.onChoices(self, self.current_choices);
                    self.choices_list.clearRetainingCapacity();
                    return;
                },
                .choice => {
                    const ip = self.readInt(OpCode.Size(.jump));
                    const is_unique = self.readInt(u8) == 1;
                    const id_index = self.readInt(OpCode.Size(.constant));
                    const visit_index = self.readInt(OpCode.Size(.get_global));
                    const visit_count = self.globals[visit_index].visit;
                    const content = self.pop().obj.data.string;

                    const tag_count = self.readInt(u8);
                    var tags = try self.allocator.alloc([]const u8, tag_count);
                    var i: usize = 0;
                    while (i < tag_count) : (i += 1) {
                        const tag_value = self.pop();
                        tags[tag_count - i - 1] = tag_value.obj.data.string;
                    }
                    if (visit_count > 0 and is_unique) continue;

                    try self.choices_list.append(.{
                        .content = content,
                        .tags = tags,
                        .visit_count = visit_count,
                        .ip = ip,
                        .id = self.bytecode.uuids[id_index],
                    });
                },
                .visit => {
                    const index = self.readInt(OpCode.Size(.get_global));
                    self.globals[index].visit += 1;
                },
                .divert => {
                    // already in a jump request, continue
                    if (is_in_jump) {
                        self.currentFrame().ip = self.jump_requests.pop();
                        continue;
                    }
                    var count = self.readInt(u8);
                    while (count > 0) : (count -= 1) {
                        const dest = self.readInt(OpCode.Size(.divert));
                        const len = self.currentFrame().instructions().len;
                        if (dest > len) return self.fail("Divert {d} is out of range {d}", .{ dest, len });
                        try self.jump_requests.append(dest);
                    }
                    if (self.jump_requests.items.len > 0) {
                        self.currentFrame().ip = self.jump_requests.pop();
                    }
                },
                .backup => {
                    const ip = self.readInt(OpCode.Size(.backup));
                    if (self.jump_backups.items.len > 0 and self.jump_backups.getLast() == ip) {
                        _ = self.pop();
                        continue;
                    }
                    try self.jump_backups.append(ip);
                },
                .fin => {
                    if (is_in_jump) {
                        self.currentFrame().ip = self.jump_requests.pop();
                        continue;
                    }
                    if (self.jump_backups.items.len > 0) {
                        self.currentFrame().ip = self.jump_backups.pop();
                        continue;
                    }
                    self.end();
                    break;
                },
            }
        }
        self.end();
    }

    fn binaryNumberOp(self: *Vm, op: OpCode) !void {
        const right = self.pop().number;
        const left = self.pop().number;
        const total = switch (op) {
            .subtract => left - right,
            .multiply => left * right,
            .divide => left / right,
            .modulus => @mod(left, right),
            else => return self.fail("Unknown binary operator '{s}'", .{@tagName(op)}),
        };
        try self.push(.{ .number = total });
    }

    fn comparisonOp(self: *Vm, op: OpCode) !void {
        var right = self.pop();
        var left = self.pop();
        if (right == .visit) right = .{ .number = @floatFromInt(right.visit) };
        if (left == .visit) left = .{ .number = @floatFromInt(left.visit) };

        if (@intFromEnum(right) != @intFromEnum(left)) {
            return self.fail("Cannot compare mismatched types '{s}' and '{s}'", .{ @tagName(left), @tagName(right) });
        }
        switch (op) {
            .equal => try self.push(.{ .bool = right.eql(left) }),
            .not_equal => try self.push(.{ .bool = !right.eql(left) }),
            .greater_than => try self.push(.{ .bool = left.number > right.number }),
            else => return self.fail("Unknown comparison operator '{s}'", .{@tagName(op)}),
        }
    }

    fn prongIsMatch(self: *Vm, capture: Value, case: Value) bool {
        _ = self;
        return switch (case) {
            .range => |r| @as(i32, @intFromFloat(capture.number)) >= r.start and @as(i32, @intFromFloat(capture.number)) <= r.end,
            else => capture.eql(case),
        };
    }

    fn push(self: *Vm, value: Value) !void {
        errdefer value.print(std.debug, self.bytecode.constants);
        if (self.stack.items.len >= stack_size) return error.OutOfMemory;
        self.stack.push(value);
    }

    fn pushAlloc(self: *Vm, data: Value.Obj.Data) !void {
        self.stack.push(try self.gc.create(self, data));
    }

    fn pop(self: *Vm) Value {
        return self.stack.pop();
    }

    pub fn print(self: *Vm, writer: anytype) void {
        writer.print("==STACK==\n", .{});
        for (self.stack.items) |*item| {
            writer.print("[", .{});
            item.print(writer);
            writer.print("]\n", .{});
        }
        writer.print("\n", .{});
    }
};
