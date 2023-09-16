const std = @import("std");
const Compiler = @import("./compiler.zig").Compiler;
const compileSource = @import("./compiler.zig").compileSource;
const ByteCode = @import("./bytecode.zig").ByteCode;
const Errors = @import("./error.zig").Errors;
const parser = @import("./parser.zig");
const OpCode = @import("./opcode.zig").OpCode;
const values = @import("./values.zig");
const Stack = @import("./stack.zig").Stack;
const Gc = @import("./gc.zig").Gc;
const Token = @import("./token.zig").Token;
const Frame = @import("./frame.zig").Frame;
const Class = @import("./class.zig").Class;
const builtins = @import("./builtins.zig");
const Rnd = @import("./builtins.zig").Rnd;
const DebugToken = @import("./debug.zig").DebugToken;
const Extern = @import("./extern.zig").Extern;

test {
    _ = @import("./vm.test.zig");
    std.testing.refAllDeclsRecursive(@This());
}

const stack_size = 4096;
const frame_size = 1023;
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

pub const Dialogue = struct {
    speaker: ?[]const u8,
    content: []const u8,
    tags: [][]const u8,
};

pub const Choice = struct {
    content: []const u8,
    count: usize,
    ip: u16,
};

pub const Vm = struct {
    allocator: std.mem.Allocator,
    frames: Stack(Frame),
    err: *Errors,
    gc: Gc,
    globals: std.ArrayList(Value),
    externs: std.ArrayList(Extern),

    stack: Stack(Value),
    /// Current iterators to allow easy nesting
    iterators: Stack(Iterator),
    /// List of positions to jump back to using `^`
    jump_backups: std.ArrayList(OpCode.Size(.jump)),

    bytecode: ByteCode,
    /// Current instruction position
    ip: usize = 0,
    debug: bool = false,

    on_dialogue: OnDialogue,
    on_choices: OnChoices,
    /// Used to cache the choices
    choices_list: std.ArrayList(Choice),
    /// Used to send to the on_choices method
    current_choices: []Choice = undefined,
    /// Determines if the vm is waiting on input
    is_waiting: bool = false,

    pub const OnDialogue = *const fn (vm: *Vm, dialogue: Dialogue) void;
    pub const OnChoices = *const fn (vm: *Vm, choices: []Choice) void;

    pub const Error = error{
        RuntimeError,
        InvalidChoice,
        Uninitialized,
    } || Compiler.Error;

    pub fn init(allocator: std.mem.Allocator, bytecode: ByteCode, runner: anytype, errors: *Errors) !Vm {
        if (!std.meta.trait.hasFunctions(runner, .{ "on_dialogue", "on_choices" }))
            return error.RuntimeError;

        var globals = try std.ArrayList(Value).initCapacity(allocator, bytecode.global_symbols.len);
        var externs = try std.ArrayList(Extern).initCapacity(allocator, bytecode.global_symbols.len);
        try globals.resize(bytecode.global_symbols.len);
        try externs.resize(bytecode.global_symbols.len);

        for (bytecode.global_symbols, 0..) |sym, i| {
            globals.items[i] = .void;
            externs.items[i] = Extern.init(allocator, sym.is_extern);
        }
        return .{
            .allocator = allocator,
            .bytecode = bytecode,
            .err = errors,
            .frames = try Stack(Frame).init(allocator, frame_size),
            .globals = globals,
            .externs = externs,
            .gc = Gc.init(allocator),
            .stack = try Stack(Value).init(allocator, stack_size),
            .iterators = try Stack(Iterator).init(allocator, iterator_size),
            .jump_backups = std.ArrayList(OpCode.Size(.jump)).init(allocator),
            .on_dialogue = runner.on_dialogue,
            .on_choices = runner.on_choices,
            .choices_list = std.ArrayList(Choice).init(allocator),
        };
    }

    pub fn deinit(self: *Vm) void {
        self.choices_list.deinit();
        self.stack.deinit();
        self.iterators.deinit();
        self.jump_backups.deinit();
        self.globals.deinit();
        self.frames.deinit();
        self.gc.deinit();
        for (self.externs.items) |*ext| ext.*.deinit();
        self.externs.deinit();
        self.bytecode.free(self.allocator);
    }

    pub fn roots(self: *Vm) []const []Value {
        return &([_][]Value{ self.globals.items, self.stack.backing });
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
        var choice = self.current_choices[index];
        self.currentFrame().ip = choice.ip;
        self.is_waiting = false;
        self.allocator.free(self.current_choices);
    }

    fn getExternIndex(self: *Vm, name: []const u8) !usize {
        for (self.bytecode.global_symbols) |s| {
            if (!std.mem.eql(u8, name, s.name)) continue;
            if (!s.is_extern) return error.IllegalOperation;
            return s.index;
        }
        return error.SymbolNotFound;
    }

    pub fn setExternNumber(self: *Vm, name: []const u8, value: f32) !void {
        return self.setExtern(try self.getExternIndex(name), f32, value);
    }

    pub fn setExternBool(self: *Vm, name: []const u8, value: bool) !void {
        return self.setExtern(try self.getExternIndex(name), bool, value);
    }

    pub fn setExternNil(self: *Vm, name: []const u8) !void {
        return self.setExtern(try self.getExternIndex(name), @TypeOf(null), null);
    }

    pub fn setExtern(self: *Vm, index: usize, comptime T: type, value: T) !void {
        self.globals.items[index] = Value.createFrom(T, value);
    }

    pub fn subscribe(self: *Vm, name: []const u8, callback: Extern.OnValueChanged) !void {
        try self.externs.items[try self.getExternIndex(name)].subscribe(callback);
    }

    pub fn unsubscribe(self: *Vm, name: []const u8, callback: Extern.OnValueChanged) !void {
        self.externs.items[try self.getExternIndex(name)].unsubscribe(callback);
    }

    pub fn interpret(self: *Vm) !void {
        var root_frame = try self.allocator.create(Value.Obj.Data);
        var root_closure = try self.allocator.create(Value.Obj);
        defer self.allocator.destroy(root_frame);
        defer self.allocator.destroy(root_closure);
        root_frame.* = .{
            .function = .{
                .instructions = self.bytecode.instructions,
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
        self.stack.resize(self.bytecode.locals_count);
        self.frames.push(try Frame.create(root_closure, 0, 0));

        try self.run();
        if (self.stack.count > self.bytecode.locals_count) {
            std.log.warn("Completed run but still had {} items on stack.", .{self.stack.count});
        }
    }

    fn fail(self: *Vm, comptime msg: []const u8, args: anytype) !void {
        var token = DebugToken.get(self.bytecode.tokens, self.ip) orelse undefined;
        try self.err.add(msg, token, .err, args);
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
        const result = std.mem.readIntSliceBig(T, frame.instructions()[frame.ip..(frame.ip + @sizeOf(T))]);
        frame.ip += @sizeOf(T);
        return result;
    }

    fn run(self: *Vm) !void {
        while (self.ip < self.currentFrame().instructions().len) : (self.ip = self.currentFrame().ip) {
            if (self.is_waiting) continue;
            const instruction = self.readByte();
            const op: OpCode = @enumFromInt(instruction);
            switch (op) {
                .constant => {
                    var index = self.readInt(u16);
                    var value = self.bytecode.constants[index];
                    try self.push(value);
                },
                .pop => _ = self.pop(),
                .add => {
                    const right = self.pop();
                    const left = self.pop();
                    if (@intFromEnum(right) != @intFromEnum(left)) {
                        std.log.warn("Cannot add types {} and {}", .{ left, right });
                        return error.RuntimeError;
                    }
                    switch (right) {
                        .number => try self.push(.{ .number = right.number + left.number }),
                        .obj => |o| {
                            switch (o.data) {
                                .string => |s| try self.pushAlloc(.{ .string = try std.mem.concat(self.allocator, u8, &.{ left.obj.data.string, s }) }),
                                else => return error.RuntimeError,
                            }
                        },
                        else => return error.RuntimeError,
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
                        else => try self.push(values.False),
                    }
                },
                .@"or" => {
                    const right = self.pop();
                    const left = self.pop();
                    if (right != .bool or left != .bool) {
                        return error.RuntimeError;
                    }
                    try self.push(.{ .bool = right.bool or left.bool });
                },
                .@"and" => {
                    const right = self.pop();
                    const left = self.pop();
                    if (right != .bool or left != .bool) {
                        return error.RuntimeError;
                    }
                    try self.push(.{ .bool = right.bool and left.bool });
                },
                .jump => {
                    var dest = self.readInt(OpCode.Size(.jump));
                    if (dest > self.currentFrame().instructions().len) break;
                    self.currentFrame().ip = dest;
                },
                .jump_if_false => {
                    var dest = self.readInt(OpCode.Size(.jump_if_false));
                    var condition = self.pop();
                    if (!try condition.isTruthy()) {
                        self.currentFrame().ip = dest;
                    }
                },
                .prong => {
                    var dest = self.readInt(OpCode.Size(.jump));
                    var values_count = self.readInt(u8);
                    var capture = self.stack.items[self.stack.count - values_count - 1];
                    var i: usize = 0;
                    var match = false;
                    while (i < values_count) : (i += 1) {
                        var value = self.pop();
                        if (!self.prongIsMatch(capture, value)) continue;
                        match = true;
                        self.stack.count -= values_count - i - 1;
                        break;
                    }

                    if (match) {
                        self.currentFrame().ip = dest;
                    }
                },
                .decl_global => {
                    const index = self.readInt(OpCode.Size(.get_global));
                    if (index > globals_size) return Error.RuntimeError;
                    // global already set from loaded state
                    var value = self.pop();
                    if (self.globals.items[index] != .void) continue;

                    self.globals.items[index] = value;
                },
                .set_global => {
                    const index = self.readInt(OpCode.Size(.set_global));
                    if (index > globals_size) return Error.RuntimeError;
                    if (index >= self.globals.items.len) return Error.RuntimeError;
                    const value = self.pop();
                    self.globals.items[index] = value;
                    var ext = self.externs.items[index];
                    if (ext.isExtern()) ext.invoke(value);
                },
                .get_global => {
                    const index = self.readInt(OpCode.Size(.get_global));
                    const value = self.globals.items[index];
                    try self.push(value);
                },
                .set_local => {
                    const index = self.readInt(OpCode.Size(.set_local));
                    var frame = self.currentFrame();
                    self.stack.items[frame.bp + index] = self.pop();
                },
                .get_local => {
                    const index = self.readInt(OpCode.Size(.get_local));
                    var frame = self.currentFrame();
                    var value = self.stack.items[frame.bp + index];
                    try self.push(value);
                },
                .set_property => {
                    var field_name_value = self.pop();
                    var instance_value = self.pop();
                    var new_value = self.pop();
                    var field_name = field_name_value.obj.data.string;
                    var instance = instance_value.obj.data.instance;
                    if (!instance.fields.contains(field_name)) return error.RuntimeError;
                    try instance.fields.put(field_name, new_value);
                },
                .get_builtin => {
                    const index = self.readInt(OpCode.Size(.get_builtin));
                    var value = builtins.builtins[index].value;
                    try self.push(value.*);
                },
                .get_free => {
                    const index = self.readInt(OpCode.Size(.get_free));
                    var obj = self.currentFrame().cl;
                    try self.push(obj.data.closure.free_values[index]);
                },
                .set_free => {
                    const index = self.readInt(OpCode.Size(.get_free));
                    var obj = self.currentFrame().cl;
                    obj.data.closure.free_values[index] = self.pop();
                },
                .string => {
                    var index = self.readInt(u16);
                    var value = self.bytecode.constants[index];
                    var count = self.readInt(u8);
                    var args = try std.ArrayList(Value).initCapacity(self.allocator, count);
                    defer args.deinit();
                    while (count > 0) : (count -= 1) {
                        try args.append(self.pop());
                    }
                    std.mem.reverse(Value, args.items);
                    var buf: [1028]u8 = undefined;
                    var fbs = std.io.fixedBufferStream(&buf);
                    var writer = fbs.writer();
                    var i: usize = 0;
                    var a: usize = 0;
                    var start: usize = 0;
                    const str = value.obj.*.data.string;
                    while (i < str.len) : (i += 1) {
                        var c = str[i];
                        if (c == '{') {
                            try writer.writeAll(str[start..i]);
                            switch (args.items[a]) {
                                .number => |n| try std.fmt.formatFloatDecimal(n, std.fmt.FormatOptions{}, fbs.writer()),
                                .bool => |b| try writer.writeAll(if (b) "true" else "false"),
                                .obj => |o| try writer.writeAll(o.data.string),
                                else => return error.RuntimeError,
                            }
                            i += 1;
                            start = i + 1;
                            a += 1;
                        }
                    }
                    try writer.writeAll(str[start..]);
                    try self.pushAlloc(.{ .string = try self.allocator.dupe(u8, fbs.getWritten()) });
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
                    var iter = self.iterators.peek().*;
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
                    var value = self.pop();
                    var count = self.readInt(OpCode.Size(.class));
                    var fields = std.ArrayList(Class.Field).init(self.allocator);
                    errdefer fields.deinit();
                    while (count > 0) : (count -= 1) {
                        var name = self.pop();
                        var field_name = name.obj.data.string;
                        var field_value = self.pop();
                        try fields.append(.{
                            .name = field_name,
                            .value = field_value,
                        });
                    }

                    std.mem.reverse(Class.Field, fields.items);
                    var class = try Class.init(self.allocator, value.obj.data.string, try fields.toOwnedSlice());
                    try self.pushAlloc(.{ .class = class });
                },
                .instance => {
                    var value = self.pop();
                    var class = value.obj.data.class;

                    var count = self.readInt(OpCode.Size(.class));
                    var fields = std.ArrayList(Class.Field).init(self.allocator);
                    defer fields.deinit();
                    while (count > 0) : (count -= 1) {
                        var name = self.pop();
                        var str_name = name.obj.data.string;
                        var field = self.pop();
                        try fields.append(.{
                            .name = str_name,
                            .value = field,
                        });
                    }
                    std.mem.reverse(Class.Field, fields.items);
                    var instance = try class.createInstance(try fields.toOwnedSlice());
                    try self.pushAlloc(.{ .instance = instance });
                },
                .range => {
                    var left = self.pop();
                    var right = self.pop();
                    try self.push(.{
                        .range = .{
                            .start = @as(i32, @intFromFloat(left.number)),
                            .end = @as(i32, @intFromFloat(right.number)),
                        },
                    });
                },
                .index => {
                    const index = self.pop();
                    var target = self.pop();
                    switch (target) {
                        .obj => |o| switch (o.data) {
                            // TODO: Will want to clean this up.
                            // Also the fails here should be caught in the compiler
                            // but that's for another day
                            .string => {
                                if (index == .obj and index.obj.data == .string) {
                                    var name = index.obj.data.string;
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
                                    var name = index.obj.data.string;
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
                                    var name = index.obj.data.string;
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
                                var name = index.obj.data.string;
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
                            else => return self.fail("Unknown target type {s} to index. Only lists, maps, sets, or instances can be indexed.", .{@tagName(target)}),
                        },
                        .map_pair => |mp| {
                            if (index != .obj)
                                return self.fail("Unknown index key \"{s}\" on map key/value pair. Only \"key\" or \"value\" are allowed.", .{@tagName(index)});
                            if (index.obj.data != .string)
                                return self.fail("Unknown index key \"{s}\" on map key/value pair. Only \"key\" or \"value\" are allowed.", .{@tagName(index.obj.data)});
                            var name = index.obj.data.string;
                            if (std.mem.eql(u8, name, "key")) {
                                try self.push(mp.key.*);
                            } else if (std.mem.eql(u8, name, "value")) {
                                try self.push(mp.value.*);
                            } else return self.fail("Unknown index key \"{s}\" on map key/value pair. Only \"key\" or \"value\" are allowed.", .{@tagName(index)});
                        },
                        else => unreachable,
                    }
                },
                .dialogue => {
                    self.is_waiting = true;
                    var has_speaker = self.readInt(u8) == 1;
                    var speaker: ?[]const u8 = null;
                    if (has_speaker) {
                        var speaker_value = self.pop();
                        speaker = speaker_value.obj.data.string;
                    }

                    var dialogue_value = self.pop();

                    var tag_count = self.readInt(u8);
                    var tags = try std.ArrayList([]const u8).initCapacity(self.allocator, tag_count);
                    var i: usize = 0;
                    while (i < tag_count) : (i += 1) {
                        const tag_value = self.pop();
                        tags.items[tag_count - i] = tag_value.obj.data.string;
                    }
                    var result = Dialogue{
                        .content = dialogue_value.obj.data.string,
                        .speaker = speaker,
                        .tags = try tags.toOwnedSlice(),
                    };
                    self.on_dialogue(self, result);
                },
                .call => {
                    const arg_count = self.readInt(OpCode.Size(.call));
                    var value = self.stack.items[self.stack.count - 1 - arg_count];
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
                            var frame = try Frame.create(value.obj, 0, self.stack.count - arg_count);
                            self.frames.push(frame);
                            self.stack.count = frame.bp + f.locals_count;
                        },
                        .builtin => |b| {
                            if (b.arity != arg_count)
                                return self.fail(
                                    "Builtin Function expected {} arguments, but found {}",
                                    .{ b.arity, arg_count },
                                );
                            var result = b.backing(&self.gc, self.stack.items[self.stack.count - arg_count .. self.stack.count]);
                            self.stack.count -= arg_count + 1;
                            try self.push(result);
                        },
                        else => {
                            return self.fail("Cannot call non-function type {s}", .{@tagName(value.obj.data)});
                        },
                    }
                },
                .closure => {
                    const index = self.readInt(u16);
                    const count = self.readInt(u8);
                    var value = self.bytecode.constants[index];
                    var free_values = try self.allocator.alloc(Value, count);
                    for (0..count) |i| {
                        free_values[i] = self.stack.items[self.stack.count - count + i];
                    }
                    var closure = try self.gc.create(self, .{
                        .closure = .{
                            .data = &value.obj.data,
                            .free_values = free_values,
                        },
                    });
                    var reset_count = self.stack.count - count;
                    self.stack.count = reset_count;
                    try self.push(closure);
                },
                .current_closure => {
                    var current = self.currentFrame().cl;
                    try self.push(.{ .obj = current });
                },
                .return_value => {
                    var value = self.pop();
                    var frame = self.frames.pop();
                    self.stack.count = frame.bp - 1;
                    try self.push(value);
                },
                .return_void => {
                    var frame = self.frames.pop();
                    self.stack.count = frame.bp - 1;
                    try self.push(values.Void);
                },
                .fork => {
                    self.is_waiting = true;
                    self.current_choices = try self.choices_list.toOwnedSlice();
                    self.on_choices(self, self.current_choices);
                    self.choices_list.clearRetainingCapacity();
                },
                .choice => {
                    const ip = self.readInt(OpCode.Size(.choice));
                    try self.choices_list.append(.{
                        .content = self.pop().obj.data.string,
                        .count = 0,
                        .ip = ip,
                    });
                },
                .backup => {
                    const ip = self.readInt(OpCode.Size(.backup));
                    try self.jump_backups.append(ip);
                },
                .fin => {
                    if (self.jump_backups.items.len > 0) {
                        self.currentFrame().ip = self.jump_backups.pop();
                        continue;
                    }
                    break;
                },
            }
        }
    }

    fn binaryNumberOp(self: *Vm, op: OpCode) !void {
        const right = self.pop().number;
        const left = self.pop().number;
        const total = switch (op) {
            .subtract => left - right,
            .multiply => left * right,
            .divide => left / right,
            .modulus => @mod(left, right),
            else => return self.fail("Unknown binary operator {s}", .{@tagName(op)}),
        };
        try self.push(.{ .number = total });
    }

    fn comparisonOp(self: *Vm, op: OpCode) !void {
        const right = self.pop();
        const left = self.pop();
        if (@intFromEnum(right) != @intFromEnum(left)) {
            return self.fail("Cannot compare mismatched types {s} and {s}", .{ @tagName(.left), @tagName(.right) });
        }
        switch (op) {
            .equal => try self.push(.{ .bool = right.eql(left) }),
            .not_equal => try self.push(.{ .bool = !right.eql(left) }),
            .greater_than => try self.push(.{ .bool = left.number > right.number }),
            else => return self.fail("Unknown comparison operator {s}", .{@tagName(op)}),
        }
    }

    fn prongIsMatch(self: *Vm, capture: Value, case: Value) bool {
        _ = self;
        return switch (case) {
            .range => |r| @as(i32, @intFromFloat(capture.number)) >= r.start or @as(i32, @intFromFloat(capture.number)) <= r.end,
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
