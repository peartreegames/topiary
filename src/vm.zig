const std = @import("std");
const Compiler = @import("./compiler.zig").Compiler;
const ByteCode = @import("./compiler.zig").ByteCode;
const Errors = @import("./error.zig").Errors;
const parser = @import("./parser.zig");
const OpCode = @import("./opcode.zig").OpCode;
const values = @import("./values.zig");
const Stack = @import("./stack.zig").Stack;
const Gc = @import("./gc.zig").Gc;
const Token = @import("./token.zig").Token;

const testing = std.testing;
const stack_size = 2048;
const globals_size = 65536;
const Value = values.Value;
const adapter = values.adapter;

const InterpretError = error{
    CompileError,
    RuntimeError,
};

const InterpretResult = union(enum) {
    Completed,
    Paused,
};

pub const Vm = struct {
    allocator: std.mem.Allocator,
    gc: Gc,
    globals: std.ArrayList(Value),
    stack: Stack(Value),

    bytecode: ByteCode = undefined,
    ip: usize = 0,
    debug: bool = false,

    pub fn init(allocator: std.mem.Allocator) !Vm {
        return .{
            .allocator = allocator,
            .stack = try Stack(Value).init(allocator, stack_size),
            .globals = try std.ArrayList(Value).initCapacity(allocator, 1024),
            .gc = Gc.init(allocator),
        };
    }

    pub fn deinit(self: *Vm) void {
        self.stack.deinit();
        self.globals.deinit();
        self.bytecode.free(testing.allocator);
        self.gc.deinit();
    }

    pub fn roots(self: *Vm) []const []Value {
        return &([_][]Value{ self.globals.items, self.stack.items });
    }

    pub fn interpret(self: *Vm, bytecode: ByteCode) !void {
        self.bytecode = bytecode;
        try self.run();
    }

    pub fn interpretSource(self: *Vm, source: []const u8) !void {
        var errors = Errors.init(self.allocator);
        defer errors.deinit();
        const tree = parser.parse(self.allocator, source, &errors) catch |err| {
            try errors.write(source, std.io.getStdErr().writer());
            return err;
        };
        defer tree.deinit();
        var compiler = Compiler.init(self.allocator);
        defer compiler.deinit();
        try compiler.compile(tree);

        var bytecode = try compiler.bytecode();
        if (self.debug) {
            bytecode.print(std.debug);
        }

        try self.interpret(bytecode);
    }

    fn fail(self: *Vm, comptime msg: []const u8, token: Token) !void {
        _ = token;
        _ = msg;
        _ = self;
        // try self.errors.add()
        return error.RuntimeError;
    }

    fn readByte(self: *Vm) u8 {
        const byte = self.bytecode.instructions[self.ip];
        self.ip += 1;
        return byte;
    }

    fn readInt(self: *Vm, comptime T: type) T {
        const result = std.mem.readIntSliceBig(T, self.bytecode.instructions[self.ip..(self.ip + @sizeOf(T))]);
        self.ip += @sizeOf(T);
        return result;
    }

    fn run(self: *Vm) !void {
        while (self.ip < self.bytecode.instructions.len) {
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
                    if (@TypeOf(right) != @TypeOf(left)) return error.RuntimeError;
                    switch (right) {
                        .number => try self.push(.{ .number = right.number + left.number }),
                        .obj => |o| {
                            switch (o.data) {
                                .string => |s| try self.pushAlloc(.{ .string = try std.mem.concat(self.allocator, u8, &.{ left.obj.*.data.string, s }) }),
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
                .jump => {
                    var dest = self.readInt(u16);
                    self.ip = dest;
                },
                .jump_if_false => {
                    var dest = self.readInt(u16);
                    var condition = self.pop();
                    if (!try condition.isTruthy()) {
                        self.ip = dest;
                    }
                },
                .set_global => {
                    const index = self.readInt(u16);
                    if (index >= self.globals.items.len) try self.globals.resize(@intFromFloat(usize, @floatFromInt(f32, index + 1) * @as(f32, 2.0)));
                    self.globals.items[index] = self.pop();
                },
                .get_global => {
                    const index = self.readInt(u16);
                    const value = self.globals.items[index];
                    try self.push(value);
                },
                .string => {
                    var index = self.readInt(u16);
                    var value = self.bytecode.constants[index];
                    var count = self.readInt(u16);
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
                    var count = self.readInt(u16);
                    var list = try std.ArrayList(Value).initCapacity(self.allocator, count);
                    while (count > 0) : (count -= 1) {
                        try list.append(self.pop());
                    }
                    std.mem.reverse(Value, list.items);
                    try self.pushAlloc(.{ .list = list });
                },
                .map => {
                    var count = self.readInt(u16);
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
                    var count = self.readInt(u16);
                    var set = Value.Obj.SetType.initContext(self.allocator, adapter);
                    while (count > 0) : (count -= 1) {
                        try set.put(self.pop(), {});
                    }
                    set.sort(adapter);
                    try self.pushAlloc(.{ .set = set });
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
                        else => unreachable,
                    }
                },
                .@"return" => break,
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
        var vm = try Vm.init(testing.allocator);
        defer vm.deinit();
        try vm.interpretSource(case.input);
        switch (case.type) {
            f32 => try testing.expect(case.value == vm.stack.last().?.number),
            bool => try testing.expect(case.value == vm.stack.last().?.bool),
            else => continue,
        }
    }
}

test "Conditionals" {
    const test_cases = .{
        .{ .input = "if (true) { 10 }", .value = 10.0, .type = .number },
        .{ .input = "if (true) { 10 } else { 20 }", .value = 10.0, .type = .number },
        .{ .input = "if (false) { 10 } else { 20 }", .value = 20.0, .type = .number },
        .{ .input = "if (1 == 1) { 10 }", .value = 10.0, .type = .number },
        .{ .input = "if (false) { 10 }", .value = void, .type = .nil },
    };

    inline for (test_cases) |case| {
        var vm = try Vm.init(testing.allocator);
        defer vm.deinit();
        try vm.interpretSource(case.input);
        switch (case.type) {
            .number => try testing.expect(case.value == vm.stack.last().?.number),
            .nil => try testing.expect(vm.stack.last().?.is(.nil)),
            else => continue,
        }
    }
}
test "Variables" {
    const test_cases = .{
        .{ .input = "var one = 1 one", .value = 1.0, .type = .number },
        .{ .input = "var one = 1 var two = 2 one + two", .value = 3.0, .type = .number },
        .{ .input = "var one = 1 var two = one + one one + two", .value = 3.0, .type = .number },
    };

    inline for (test_cases) |case| {
        var vm = try Vm.init(testing.allocator);
        defer vm.deinit();
        try vm.interpretSource(case.input);
        try testing.expect(case.value == vm.stack.last().?.number);
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
        var vm = try Vm.init(testing.allocator);
        // vm.debug = true;
        defer vm.deinit();
        try vm.interpretSource(case.input);
        try testing.expectEqualStrings(case.value, vm.stack.last().?.obj.data.string);
    }
}

test "Lists" {
    const test_cases = .{
        .{ .input = "[]", .value = [_]f32{} },
        .{ .input = "[1,2,3]", .value = [_]f32{ 1, 2, 3 } },
        .{ .input = "[1 + 2, 3 * 4, 5 + 6]", .value = [_]f32{ 3, 12, 11 } },
    };

    inline for (test_cases) |case| {
        var vm = try Vm.init(testing.allocator);
        // vm.debug = true;
        defer vm.deinit();
        try vm.interpretSource(case.input);
        for (case.value, 0..) |v, i| {
            try testing.expect(v == vm.stack.last().?.obj.data.list.items[i].number);
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
        var vm = try Vm.init(testing.allocator);
        // vm.debug = true;
        defer vm.deinit();
        try vm.interpretSource(case.input);
        const map = vm.stack.last().?.obj.data.map;
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
        var vm = try Vm.init(testing.allocator);
        // vm.debug = true;
        defer vm.deinit();
        try vm.interpretSource(case.input);
        const set = vm.stack.last().?.obj.data.set;
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
        var vm = try Vm.init(testing.allocator);
        vm.debug = true;
        defer vm.deinit();
        try vm.interpretSource(case.input);
        const value = vm.stack.last().?;
        errdefer std.log.warn("{s}--{}", .{ case.input, @TypeOf(case.value) });
        errdefer value.print(std.debug);
        switch (@TypeOf(case.value)) {
            comptime_float => try testing.expect(case.value == value.number),
            else => try testing.expect(value == .nil),
        }
    }
}
