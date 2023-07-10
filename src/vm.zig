const std = @import("std");
const compiler = @import("./compiler.zig");
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
const builtins = @import("./builtins.zig").builtins;
const Rnd = @import("./builtins.zig").Rnd;
const DebugToken = @import("./debug.zig").DebugToken;

const testing = std.testing;
const stack_size = 2047;
const frame_size = 1023;
const globals_size = 65535;
const Value = values.Value;
const adapter = values.adapter;
const input_buf: [2]u8 = undefined;

const InterpretError = error{
    CompileError,
    RuntimeError,
    InvalidChoice,
};

const InterpretResult = union(enum) {
    Completed,
    Paused,
};

const Dialogue = struct {
    speaker: ?[]const u8,
    content: []const u8,
    tags: [][]const u8,
};

const Choice = struct {
    content: []const u8,
    count: usize,
    ip: u16,
};

pub const Vm = struct {
    allocator: std.mem.Allocator,
    frames: Stack(Frame),
    errors: Errors,
    gc: Gc,
    globals: std.ArrayList(Value),
    stack: Stack(Value),

    bytecode: ByteCode = undefined,
    ip: usize = 0,
    debug: bool = false,

    on_dialogue: OnDialogue,
    on_choices: OnChoices,
    choices_list: std.ArrayList(Choice),
    current_choices: []Choice = undefined,
    is_waiting: bool = false,

    pub const OnDialogue = *const fn (vm: *Vm, dialogue: Dialogue) void;
    pub const OnChoices = *const fn (vm: *Vm, choices: []Choice) void;

    pub fn init(allocator: std.mem.Allocator, runner: anytype) !Vm {
        if (!std.meta.trait.hasFunctions(runner, .{ "on_dialogue", "on_choices" }))
            return error.RuntimeError;
        return .{
            .allocator = allocator,
            .errors = Errors.init(allocator),
            .frames = try Stack(Frame).init(allocator, frame_size),
            .globals = try std.ArrayList(Value).initCapacity(allocator, 1024),
            .gc = Gc.init(allocator),
            .stack = try Stack(Value).init(allocator, stack_size),
            .on_dialogue = runner.on_dialogue,
            .on_choices = runner.on_choices,
            .choices_list = std.ArrayList(Choice).init(allocator),
        };
    }

    pub fn deinit(self: *Vm) void {
        self.choices_list.deinit();
        self.stack.deinit();
        self.globals.deinit();
        self.frames.deinit();
        self.errors.deinit();
        self.gc.deinit();
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

    pub fn selectChoice(self: *Vm, index: usize) InterpretError!void {
        if (index < 0 or index >= self.current_choices.len) {
            std.log.warn("CHOICE:{}", .{index});
            return InterpretError.InvalidChoice;
        }
        var choice = self.current_choices[index];
        self.currentFrame().ip = choice.ip;
        self.is_waiting = false;
        self.allocator.free(self.current_choices);
    }

    pub fn interpret(self: *Vm, bytecode: ByteCode) !void {
        self.bytecode = bytecode;
        var root_frame = try self.allocator.create(Value.Obj.Data);
        var root_closure = try self.allocator.create(Value.Obj);
        defer self.allocator.destroy(root_frame);
        defer self.allocator.destroy(root_closure);
        root_frame.* = .{
            .function = .{
                .instructions = bytecode.instructions,
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
        self.frames.push(try Frame.create(root_closure, 0, 0));
        try self.run();
    }

    pub fn interpretSource(self: *Vm, source: []const u8) !void {
        var bytecode = compiler.compileSource(self.allocator, source, &self.errors) catch |err| {
            try self.errors.write(source, std.io.getStdErr().writer());
            return err;
        };
        if (self.debug) {
            bytecode.print(std.debug);
        }

        try self.interpret(bytecode);
    }

    fn fail(self: *Vm, comptime msg: []const u8, token: Token, args: anytype) !void {
        try self.errors.add(msg, token, .err, args);

        return error.RuntimeError;
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
            const op = @enumFromInt(OpCode, instruction);
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
                .set_global => {
                    const index = self.readInt(OpCode.Size(.set_global));
                    if (index >= self.globals.items.len) try self.globals.resize(@intFromFloat(usize, @floatFromInt(f32, index + 1) * @as(f32, 2.0)));
                    self.globals.items[index] = self.pop();
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
                    try self.push(self.stack.items[frame.bp + index]);
                },
                .set_property => {
                    var new_value = self.pop();
                    var field_name_value = self.pop();
                    var instance_value = self.pop();

                    var field_name = field_name_value.obj.data.string;
                    var instance = instance_value.obj.data.instance;
                    if (!instance.fields.contains(field_name)) return error.RuntimeError;
                    try instance.fields.put(field_name, new_value);
                },
                .get_builtin => {
                    const index = self.readInt(OpCode.Size(.get_builtin));
                    var value = builtins[index].value;
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
                .class => {
                    var value = self.pop();
                    var count = self.readInt(OpCode.Size(.class));
                    var fields = std.ArrayList(Class.Field).init(self.allocator);
                    errdefer fields.deinit();
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
                .index => {
                    const index = self.pop();
                    var target = self.pop();
                    if (target != .obj) return error.RuntimeError;
                    switch (target.obj.data) {
                        .list => |l| {
                            if (index != .number) return error.RuntimeError;
                            const i = @intFromFloat(u32, index.number);
                            if (i < 0 or i >= l.items.len) {
                                try self.push(values.Nil);
                            } else try self.push(l.items[i]);
                        },
                        .map => |m| {
                            if (m.get(index)) |v| {
                                try self.push(v);
                            } else try self.push(values.Nil);
                        },
                        .instance => |i| {
                            if (i.fields.get(index.obj.data.string)) |field| {
                                try self.push(field);
                            } else return error.RuntimeError;
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
                    try self.push(values.Nil);
                    self.on_dialogue(self, result);
                },
                .call => {
                    const arg_count = self.readInt(OpCode.Size(.call));
                    var value = self.stack.items[self.stack.count - 1 - arg_count];
                    if (value != .obj) return error.RuntimeError;
                    switch (value.obj.data) {
                        .closure => |c| {
                            const f = c.data.function;
                            if (f.arity != arg_count) return error.RuntimeError;
                            var frame = try Frame.create(value.obj, 0, self.stack.count - arg_count);
                            self.frames.push(frame);
                            self.stack.count = frame.bp + f.locals_count;
                        },
                        .builtin => |b| {
                            if (b.arity != arg_count)
                                return self.fail(
                                    "Function expected {} arguments, but found {}",
                                    DebugToken.get(self.bytecode.tokens, self.ip) orelse undefined,
                                    .{ b.arity, arg_count },
                                );
                            var result = b.backing(&self.gc, self.stack.items[self.stack.count - arg_count .. self.stack.count]);
                            try self.push(result);
                        },
                        else => return error.RuntimeError,
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

                    self.stack.count = self.stack.count - count + 1;
                    try self.push(closure);
                },
                .current_closure => {
                    var current = self.currentFrame().cl;
                    try self.push(.{ .obj = current });
                },
                .return_value => {
                    var value = self.pop();
                    var frame = self.frames.pop();
                    _ = self.pop();
                    self.stack.count = frame.bp - 1;
                    try self.push(value);
                },
                .return_void => {
                    var frame = self.frames.pop();
                    // _ = self.pop();
                    self.stack.count = frame.bp - 1;
                    try self.push(values.Nil);
                },
                .fork => {
                    self.is_waiting = true;
                    self.current_choices = try self.choices_list.toOwnedSlice();
                    self.on_choices(self, self.current_choices);
                    self.choices_list.clearRetainingCapacity();
                },
                .choice => {
                    const ip = self.readInt(u16);
                    try self.choices_list.append(.{
                        .content = self.pop().obj.data.string,
                        .count = 0,
                        .ip = ip,
                    });
                },
                .fin => break,
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
            else => return error.UnknownOperator,
        };
        try self.push(.{ .number = total });
    }

    fn comparisonOp(self: *Vm, op: OpCode) !void {
        const right = self.pop();
        const left = self.pop();
        if (@intFromEnum(right) != @intFromEnum(left)) return error.RuntimeError;
        switch (op) {
            .equal => try self.push(.{ .bool = right.eql(left) }),
            .not_equal => try self.push(.{ .bool = !right.eql(left) }),
            .greater_than => try self.push(.{ .bool = left.number > right.number }),
            else => return error.UnknownOperator,
        }
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

const TestRunner = struct {
    pub fn on_dialogue(vm: *Vm, dialogue: Dialogue) void {
        if (dialogue.speaker) |speaker| {
            std.debug.print("{s}: ", .{speaker});
        }
        std.debug.print("{s}\n", .{dialogue.content});
        vm.selectContinue();
    }

    pub fn on_choices(vm: *Vm, choices: []Choice) void {
        std.debug.print("---CHOICE---\n", .{});
        for (choices, 0..) |choice, i| {
            std.debug.print("[{d}] {s}\n", .{ i, choice.content });
        }

        var rnd = std.rand.DefaultPrng.init(std.crypto.random.int(u64));
        const index = rnd.random().intRangeAtMost(usize, 0, choices.len - 1);
        vm.selectChoice(index) catch |err| {
            std.debug.print("Error: {}", .{err});
        };
    }
};

test "Basics" {
    const test_cases = .{
        .{ .input = "1", .value = 1.0, .type = f32 },
        .{ .input = "2", .value = 2.0, .type = f32 },
        .{ .input = "1 + 2", .value = 3.0, .type = f32 },
        .{ .input = "-12", .value = -12.0, .type = f32 },
        .{ .input = "111 + 222", .value = 333.0, .type = f32 },
        .{ .input = "5 - 2", .value = 3.0, .type = f32 },
        .{ .input = "5 * 2", .value = 10.0, .type = f32 },
        .{ .input = "6 / 2", .value = 3.0, .type = f32 },
        .{ .input = "6 % 5", .value = 1.0, .type = f32 },
        .{ .input = "1 == 1", .value = true, .type = bool },
        .{ .input = "1 != 1", .value = false, .type = bool },
        .{ .input = "1 > 5", .value = false, .type = bool },
        .{ .input = "1 < 5", .value = true, .type = bool },
        .{ .input = "!true", .value = false, .type = bool },
        .{ .input = "!false", .value = true, .type = bool },
        .{ .input = "!!true == true", .value = true, .type = bool },
        .{ .input = "!(1 == 1) != (5 > 10)", .value = false, .type = bool },
    };

    inline for (test_cases) |case| {
        var vm = try Vm.init(testing.allocator, TestRunner);
        defer vm.deinit();
        try vm.interpretSource(case.input);
        switch (case.type) {
            f32 => try testing.expect(case.value == vm.stack.previous().number),
            bool => try testing.expect(case.value == vm.stack.previous().bool),
            else => continue,
        }
    }
}

test "Conditionals" {
    const test_cases = .{
        .{ .input = "if true { 10 }", .value = 10.0, .type = .number },
        .{ .input = "if true { 10 } else { 20 }", .value = 10.0, .type = .number },
        .{ .input = "if false { 10 } else { 20 }", .value = 20.0, .type = .number },
        .{ .input = "if 1 == 1 { 10 }", .value = 10.0, .type = .number },
        .{ .input = "if false { 10 }", .value = void, .type = .nil },
        .{ .input = "if 1 > 0 or 2 < 1 { 10 }", .value = 10.0, .type = .number },
        .{ .input = "if 1 > 0 and 2 < 1 { 10 }", .value = void, .type = .nil },
    };

    inline for (test_cases) |case| {
        var vm = try Vm.init(testing.allocator, TestRunner);
        defer vm.deinit();
        try vm.interpretSource(case.input);
        switch (case.type) {
            .number => try testing.expect(case.value == vm.stack.previous().number),
            .nil => try testing.expect(vm.stack.previous().is(.nil)),
            else => continue,
        }
    }
}
test "Variables" {
    const test_cases = .{
        .{ .input = "var one = 1 one", .value = 1.0 },
        .{ .input = "var one = 1 var two = 2 one + two", .value = 3.0 },
        .{ .input = "var one = 1 var two = one + one one + two", .value = 3.0 },
        .{ .input = 
        \\ var five = 1
        \\ five = 5
        , .value = 5.0 },
    };

    inline for (test_cases) |case| {
        var vm = try Vm.init(testing.allocator, TestRunner);
        defer vm.deinit();
        try vm.interpretSource(case.input);
        var value = vm.stack.previous().number;
        errdefer std.log.warn("\n{s}\n ==== {}", .{ case.input, value });
        try testing.expect(case.value == value);
    }
}

test "Strings" {
    const test_cases = .{
        .{ .input = "\"testing\"", .value = "testing" },
        .{ .input = "\"test\" + \"ing\"", .value = "testing" },
        .{ .input = "\"test\" + \"ing\" + \"testing\"", .value = "testingtesting" },
        .{ .input = "\"{123}test\"", .value = "123test" },
        .{ .input = "\"test{123}\"", .value = "test123" },
        .{ .input = "\"{123}te{4 * 5}st{6 + 7}\"", .value = "123te20st13" },
    };

    inline for (test_cases) |case| {
        errdefer std.log.warn("{s}", .{case.input});
        var vm = try Vm.init(testing.allocator, TestRunner);
        // vm.debug = true;
        defer vm.deinit();
        try vm.interpretSource(case.input);
        try testing.expectEqualStrings(case.value, vm.stack.previous().obj.data.string);
    }
}

test "Lists" {
    const test_cases = .{
        .{ .input = "[]", .value = [_]f32{} },
        .{ .input = "[1,2,3]", .value = [_]f32{ 1, 2, 3 } },
        .{ .input = "[1 + 2, 3 * 4, 5 + 6]", .value = [_]f32{ 3, 12, 11 } },
    };

    inline for (test_cases) |case| {
        var vm = try Vm.init(testing.allocator, TestRunner);
        // vm.debug = true;
        defer vm.deinit();
        try vm.interpretSource(case.input);
        for (case.value, 0..) |v, i| {
            try testing.expect(v == vm.stack.previous().obj.data.list.items[i].number);
        }
    }
}

test "Maps" {
    const test_cases = .{
        .{ .input = "({:})", .keys = [_]f32{}, .values = [_]f32{} },
        .{ .input = "({1:2, 3: 4})", .keys = [_]f32{ 1, 3 }, .values = [_]f32{ 2, 4 } },
        .{ .input = "({1 + 1: 2 * 2, 3 + 3: 4 * 4})", .keys = [_]f32{ 2, 6 }, .values = [_]f32{ 4, 16 } },
    };

    inline for (test_cases) |case| {
        var vm = try Vm.init(testing.allocator, TestRunner);
        // vm.debug = true;
        defer vm.deinit();
        try vm.interpretSource(case.input);
        const map = vm.stack.previous().obj.data.map;
        try testing.expect(map.keys().len == case.keys.len);
        if (case.keys.len > 0) {
            for (map.keys(), 0..) |k, i| {
                errdefer std.log.warn("{}:{}", .{ k.number, map.get(k).?.number });
                try testing.expect(case.keys[i] == k.number);
                try testing.expect(case.values[i] == map.get(k).?.number);
            }
        }
    }
}

test "Sets" {
    const test_cases = .{
        .{ .input = "({})", .values = [_]f32{} },
        .{ .input = "({1, 2})", .values = [_]f32{ 1, 2 } },
        .{ .input = "({1 + 1, 3 + 3})", .values = [_]f32{ 2, 6 } },
    };

    inline for (test_cases) |case| {
        var vm = try Vm.init(testing.allocator, TestRunner);
        // vm.debug = true;
        defer vm.deinit();
        try vm.interpretSource(case.input);
        const set = vm.stack.previous().obj.data.set;
        try testing.expect(set.keys().len == case.values.len);
        if (case.values.len > 0) {
            for (set.keys(), 0..) |k, i| {
                errdefer std.log.warn("{}", .{k.number});
                try testing.expect(case.values[i] == k.number);
            }
        }
    }
}

test "Index" {
    const test_cases = .{
        .{ .input = "[1,2,3][1]", .value = 2.0 },
        .{ .input = "[1,2,3][0 + 2]", .value = 3.0 },
        .{ .input = "[[1,2,3]][0][0]", .value = 1.0 },
        .{ .input = "[][0]", .value = null },
        .{ .input = "[1,2,3][99]", .value = null },
        .{ .input = "({1: 1, 2: 2})[1]", .value = 1.0 },
        .{ .input = "({1: 1, 2: 2})[2]", .value = 2.0 },
        .{ .input = "({1: 1})[2]", .value = null },
        .{ .input = "({:})[0]", .value = null },
    };

    inline for (test_cases) |case| {
        var vm = try Vm.init(testing.allocator, TestRunner);
        // vm.debug = true;
        defer vm.deinit();
        try vm.interpretSource(case.input);
        const value = vm.stack.previous();
        errdefer std.log.warn("{s}--{}", .{ case.input, @TypeOf(case.value) });
        switch (@TypeOf(case.value)) {
            comptime_float => try testing.expect(case.value == value.number),
            else => try testing.expect(value == .nil),
        }
    }
}

test "Functions" {
    const test_cases = .{
        .{ .input = 
        \\ const fifteen = || return 5 + 10
        \\ fifteen()    
        , .value = 15.0 },
        .{ .input = 
        \\ const one = || return 1
        \\ const two = || return 2
        \\ one() + two() 
        , .value = 3.0 },
        .{ .input = 
        \\ const a = || return 1
        \\ const b = || return a() + 1
        \\ const c = || return b() + 1
        \\ c() 
        , .value = 3.0 },
        .{ .input = 
        \\ (|| return 33)()
        , .value = 33.0 },
        .{ .input = 
        \\ const exit = || { return 99 return 100 }
        \\ exit()
        , .value = 99.0 },
        .{ .input = 
        \\ const cond = || { if (true) return 1 return 0 }
        \\ cond()
        , .value = 1.0 },
        .{ .input = 
        \\ const cond = || { if (false) return 1 return 0 }
        \\ cond()
        , .value = 0.0 },
        .{ .input = 
        \\ const noop = || {}
        \\ noop()
        , .value = null },
        .{ .input = 
        \\ const noop = || {}
        \\ const noopop = || noop()
        \\ noop()
        \\ noopop()
        , .value = null },
        .{ .input = 
        \\ const one = || return 1
        \\ const curry = || return one
        \\ curry()()
        , .value = 1.0 },
    };

    inline for (test_cases) |case| {
        var vm = try Vm.init(testing.allocator, TestRunner);
        // vm.debug = true;
        defer vm.deinit();
        try vm.interpretSource(case.input);
        const value = vm.stack.previous();
        switch (@TypeOf(case.value)) {
            comptime_float => try testing.expect(case.value == value.number),
            else => try testing.expect(value == .nil),
        }
    }
}

test "Locals" {
    const test_cases = .{
        .{
            .input =
            \\ const oneFn = || {
            \\     var one = 1
            \\     return one      
            \\ }
            \\ oneFn()
            ,
            .value = 1.0,
        },
        .{
            .input =
            \\ const threeFn = || {
            \\     var one = 1
            \\     var two = 2
            \\     return one + two     
            \\ }
            \\ threeFn()
            ,
            .value = 3.0,
        },
        .{
            .input =
            \\ const threeFn = || {
            \\     var one = 1
            \\     var two = 2
            \\     return one + two     
            \\ }
            \\ const sevenFn = || {
            \\     var three = 3
            \\     var four = 4    
            \\     return three + four
            \\ }
            \\ threeFn() + sevenFn()
            ,
            .value = 10.0,
        },
        .{
            .input =
            \\ const five = 5
            \\ const minusOne = || {
            \\     const one = 1
            \\     return five - one
            \\ }
            \\ const minusTwo = || {
            \\     const two = 2
            \\     return five - two    
            \\ }
            \\ minusOne() + minusTwo()
            ,
            .value = 7.0,
        },
    };

    inline for (test_cases) |case| {
        var vm = try Vm.init(testing.allocator, TestRunner);
        // vm.debug = true;
        defer vm.deinit();
        try vm.interpretSource(case.input);
        const value = vm.stack.previous();
        errdefer std.log.warn("{s}:: {any} == {any}", .{ case.input, case.value, value });
        switch (@TypeOf(case.value)) {
            comptime_float => try testing.expect(case.value == value.number),
            else => try testing.expect(value == .nil),
        }
    }
}

test "Function Arguments" {
    const test_cases = .{
        .{ .input = 
        \\ const ident = |a| return a
        \\ ident(4)
        , .value = 4.0 },
        .{ .input = 
        \\ const sum = |a, b| return a + b
        \\ sum(1, 2)
        , .value = 3.0 },
        .{ .input = 
        \\ const sum = |a, b| { 
        \\    const c = a + b
        \\    return c
        \\ }       
        \\ sum(1, 2)
        , .value = 3.0 },
        .{ .input = 
        \\ const sum = |a, b| { 
        \\    const c = a + b
        \\    return c
        \\ }
        \\ sum(1, 2) + sum(3, 4)
        , .value = 10.0 },
        .{ .input = 
        \\ const globalNum = 10
        \\ const sum = |a, b| {
        \\     const c = a + b
        \\     return c + globalNum
        \\ }
        \\ sum(5, 5) + globalNum
        , .value = 30.0 },
        .{ .input = 
        \\ const globalNum = 10
        \\ const sum = |a, b| {
        \\     const c = a + b
        \\     return c + globalNum
        \\ }
        \\ const outer = || { 
        \\    return 
        \\    sum(1, 2) + 
        \\    sum(3, 4) + 
        \\    globalNum 
        \\ }
        \\ outer() + globalNum
        , .value = 50.0 },
    };
    inline for (test_cases) |case| {
        errdefer std.log.warn("\n======\n{s}\n======\n", .{case.input});
        var vm = try Vm.init(testing.allocator, TestRunner);
        // vm.debug = true;
        defer vm.deinit();
        try vm.interpretSource(case.input);
        const value = vm.stack.previous();
        switch (@TypeOf(case.value)) {
            comptime_float => try testing.expect(case.value == value.number),
            else => try testing.expect(value == .nil),
        }
    }
}

test "Builtin Functions" {
    const test_cases = .{ .{
        .input = "rnd(1, 10)",
        .type = f32,
    }, .{
        .input = "rnd01()",
        .type = f32,
    } };
    inline for (test_cases) |case| {
        errdefer std.log.warn("\n======\n{s}\n======\n", .{case.input});
        var vm = try Vm.init(testing.allocator, TestRunner);
        // vm.debug = true;
        defer vm.deinit();
        vm.interpretSource(case.input) catch |err| {
            try vm.errors.write(case.input, std.io.getStdErr().writer());
            return err;
        };
        const value = vm.stack.previous();
        try testing.expect(value == .number);
    }
}

test "Closures" {
    const test_cases = .{
        .{
            .input =
            \\ const newClosure = |a| {
            \\     return || return a 
            \\ }
            \\ const closure = newClosure(99)
            \\ closure()
            ,
            .value = 99.0,
        },
        .{
            .input =
            \\ const newAdder = |a, b| {
            \\     return |c| return a + b + c 
            \\ }
            \\ const adder = newAdder(1, 2)
            \\ adder(7)
            ,
            .value = 10.0,
        },
        .{
            .input =
            \\ const newAdder = |a, b| {
            \\     const c = a + b
            \\     return |d| return c + d
            \\ }
            \\ const adder = newAdder(1, 2)
            \\ adder(10)
            ,
            .value = 13.0,
        },
        .{
            .input =
            \\ const countDown = |x| {
            \\     if x == 0 return 0
            \\     else return countDown(x - 1)
            \\ }
            \\ const wrapper = || return countDown(2)
            \\ wrapper()
            ,
            .value = 0.0,
        },
        .{
            .input =
            \\ const wrapper = || {
            \\     const countDown = |x| {
            \\         if x == 0 return 22
            \\         else return countDown(x - 1)
            \\     }
            \\     return countDown(2)
            \\ }
            \\ wrapper()
            ,
            .value = 22.0,
        },
    };
    inline for (test_cases) |case| {
        errdefer std.log.warn("\n======\n{s}\n======\n", .{case.input});
        var vm = try Vm.init(testing.allocator, TestRunner);
        // vm.debug = true;
        defer vm.deinit();
        try vm.interpretSource(case.input);
        const value = vm.stack.previous();
        try testing.expectEqual(value.number, case.value);
    }
}

test "Loops" {
    const test_cases = .{
        .{ .input = 
        \\ var x = 0
        \\ while x < 10 {
        \\     x = x + 1
        \\ }
        \\ x
        , .value = 10 },
        .{ .input = 
        \\ var x = 0
        \\ while true {
        \\    if x > 9 break
        \\    x = x + 1
        \\ }
        \\ x
        , .value = 10 },
        .{ .input = 
        \\ var x = 0
        \\ while true {
        \\    x = x + 1
        \\    if x < 10 continue
        \\    break
        \\ }
        \\ x
        , .value = 10 },
        // .{ .input =
        // \\ const list = [1,2,3,4,5]
        // \\ var sum = 0
        // \\ for list |item| {
        // \\     sum = sum + item
        // \\ }
        // \\ sum
        // , .value = 15 },
    };
    inline for (test_cases) |case| {
        errdefer std.log.warn("\n======\n{s}\n======\n", .{case.input});
        var vm = try Vm.init(testing.allocator, TestRunner);
        // vm.debug = true;
        defer vm.deinit();
        try vm.interpretSource(case.input);
        const value = vm.stack.previous();
        try testing.expectEqual(value.number, case.value);
    }
}

test "Classes" {
    const input =
        \\ class Test {
        \\    value = 0        
        \\ }
    ;
    errdefer std.log.warn("\n======\n{s}\n======\n", .{input});
    var vm = try Vm.init(testing.allocator, TestRunner);
    // vm.debug = true;
    std.debug.print("\n======\n", .{});
    defer vm.deinit();
    try vm.interpretSource(input);
    var value = vm.stack.previous();
    try testing.expect(value.obj.data == .class);
    try testing.expectEqualStrings("Test", value.obj.data.class.name);
}

test "Instance" {
    const input =
        \\ class Test {
        \\    value = 0        
        \\ }
        \\ const test = new Test{}
        \\ print(test)
        \\ print(test.value)
        \\ test.value = 5
        \\ print(test.value)
    ;
    errdefer std.log.warn("\n======\n{s}\n======\n", .{input});
    var vm = try Vm.init(testing.allocator, TestRunner);
    vm.debug = true;
    std.debug.print("\n======\n", .{});
    try vm.interpretSource(input);
    defer vm.deinit();
    // var value = vm.stack.previous();
    // _ = value;
    // try testing.expect(value.obj.data == .instance);
    // try testing.expect(value.obj.data.instance.fields.get("value").?.number == 0);
}

test "Boughs" {
    const test_cases = .{
        .{ .input = 
        \\ === START {
        \\    :speaker: "Text goes here"
        \\    :speaker: "More text here"
        \\ }
        \\ => START
        },
        .{ .input = 
        \\ === START {
        \\    const before = "This is added before"
        \\    const after = "and this is added afterwards"
        \\    :speaker_one: "{before} and then more text here"
        \\    :speaker_two: "Text goes here {after}"
        \\ }
        \\ => START
        },
        .{ .input = 
        \\ const repeat = |str, count| {
        \\     var result = ""
        \\     while count > 0 {
        \\          result = result + str 
        \\          count -= 1
        \\          print(count)
        \\     }
        \\     return result
        \\ }
        \\ === START {
        \\    :speaker_one: "Hello, {repeat("Yo ", 5)}!"
        \\    :speaker_two: "Uh.. hello?"
        \\ }
        \\ => START
        },
        .{ .input = 
        \\ === START {
        \\    if true :speaker: "True text goes here" 
        \\    :speaker: "More text here"
        \\    if false :speaker: "False text doesn't appear"
        \\    :speaker: "Final text here"
        \\ }
        \\ => START
        },
        .{ .input = 
        \\ === START {
        \\    :speaker: "Text goes here"
        \\    === INNER {
        \\        :speaker: "Inner text here"
        \\    }
        \\    :speaker: "More goes here"
        \\    => INNER
        \\    :speaker: "Final goes here" // should not be printed
        \\ }
        \\ => START
        },
        .{ .input = 
        \\ === START {
        \\    :speaker: "Text goes here"
        \\    === OUTER {
        \\        :speaker: "Outer text here doesn't happen"
        \\        === INNER {
        \\            :speaker: "Inner and final text here"
        \\        }
        \\    }
        \\    :speaker: "More goes here"
        \\    => OUTER.INNER
        \\    :speaker: "Text doesn't appear here" // should not be printed
        \\ }
        \\ => START
        },
    };

    inline for (test_cases) |case| {
        errdefer std.log.warn("\n======\n{s}\n======\n", .{case.input});
        var vm = try Vm.init(testing.allocator, TestRunner);
        // vm.debug = true;
        defer vm.deinit();
        std.debug.print("\n======\n", .{});
        try vm.interpretSource(case.input);
    }
}

test "Forks" {
    const test_cases = .{
        .{
            .input =
            \\ === START {
            \\     :speaker: "Question"
            \\    fork {
            \\        ~ "Answer one" {
            \\            :speaker: "You chose one"
            \\        }
            \\        ~ "Answer two" {      
            \\            :speaker: "You chose two"
            \\        }
            \\    }
            \\ }
            \\ => START
            ,
        },
        .{
            .input =
            \\ === START {
            \\     :speaker: "Question"
            \\    var count = 0
            \\    fork NAMED {
            \\        ~ "Answer one" {
            \\            :speaker: "You chose one"
            \\            if count < 5 {
            \\                count += 1
            \\                => NAMED
            \\            }
            \\            => DONE
            \\        }
            \\    }
            \\ }
            \\ => START
            \\ === DONE {
            \\     :speaker: "Done"
            \\ }
            ,
        },
    };

    inline for (test_cases) |case| {
        errdefer std.log.warn("\n======\n{s}\n======\n", .{case.input});
        var vm = try Vm.init(testing.allocator, TestRunner);
        // vm.debug = true;
        std.debug.print("\n======\n", .{});
        defer vm.deinit();
        try vm.interpretSource(case.input);
    }
}
