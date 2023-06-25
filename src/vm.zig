const std = @import("std");
const Compiler = @import("./compiler.zig").Compiler;
const ByteCode = @import("./compiler.zig").ByteCode;
const Errors = @import("./error.zig").Errors;
const parser = @import("./parser.zig");
const OpCode = @import("./opcode.zig").OpCode;
const values = @import("./values.zig");
const Stack = @import("./stack.zig").Stack;
const Gc = @import("./gc.zig").Gc;
const Object = @import("./gc.zig").Object;
const Token = @import("./token.zig").Token;

const testing = std.testing;
const stack_size = 2048;
const globals_size = 65536;
const Value = values.Value;

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
    globals: std.ArrayList(*Object),
    stack: Stack(*Object),

    bytecode: ByteCode = undefined,
    ip: usize = 0,
    debug: bool = false,

    pub fn init(allocator: std.mem.Allocator) !Vm {
        return .{
            .allocator = allocator,
            .stack = try Stack(*Object).init(allocator, stack_size),
            .globals = try std.ArrayList(*Object).initCapacity(allocator, 1024),
            .gc = Gc.init(allocator),
        };
    }

    pub fn deinit(self: *Vm) void {
        self.stack.deinit();
        self.globals.deinit();
        self.bytecode.free(testing.allocator);
        self.gc.deinit();
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
            Compiler.print(bytecode, null, std.debug);
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
                    switch (value) {
                        .string => |s| try self.push(.{ .string = try self.allocator.dupe(u8, s) }),
                        else => try self.push(value),
                    }
                },
                .pop => _ = self.pop(),
                .add => {
                    const right = self.pop().*.value;
                    const left = self.pop().*.value;
                    if (@TypeOf(right) != @TypeOf(left)) return error.RuntimeError;
                    switch (right) {
                        .number => try self.push(.{ .number = right.number + left.number }),
                        .string => {
                            try self.push(.{ .string = try std.mem.concat(self.allocator, u8, &.{ left.string, right.string }) });
                        },
                        else => return error.RuntimeError,
                    }
                },
                .subtract, .multiply, .divide, .modulus => try self.binaryNumberOp(op),
                .equal, .not_equal, .greater_than => try self.comparisonOp(op),
                .true => try self.push(values.True),
                .false => try self.push(values.False),
                .nil => try self.push(values.Nil),
                .negate => try self.push(.{ .number = -self.pop().*.value.number }),
                .not => {
                    const value = self.pop().*.value;
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
                    var condition = self.pop().*.value;
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
                    try self.push(self.globals.items[index].*.value);
                },
                .@"return" => break,
            }
        }
    }

    fn binaryNumberOp(self: *Vm, op: OpCode) !void {
        const right = self.pop().*.value.number;
        const left = self.pop().*.value.number;
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
        const right = self.pop().*.value;
        const left = self.pop().*.value;
        if (@intFromEnum(right) != @intFromEnum(left)) return error.RuntimeError;
        switch (op) {
            .equal => try self.push(.{ .bool = right.eql(left) }),
            .not_equal => try self.push(.{ .bool = !right.eql(left) }),
            .greater_than => try self.push(.{ .bool = left.number > right.number }),
            else => return error.UnknownOperator,
        }
    }

    fn push(self: *Vm, value: Value) !void {
        self.stack.push(try self.gc.create(self, value));
    }

    fn pop(self: *Vm) *Object {
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
            f32 => try testing.expect(case.value == vm.stack.last().?.*.value.number),
            bool => try testing.expect(case.value == vm.stack.last().?.*.value.bool),
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
            .number => try testing.expect(case.value == vm.stack.last().?.*.value.number),
            .nil => try testing.expect(vm.stack.last().?.*.value.is(.nil)),
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
        try testing.expect(case.value == vm.stack.last().?.*.value.number);
    }
}

test "Strings" {
    const test_cases = .{
        .{ .input = "\"testing\"", .value = "testing" },
        .{ .input = "\"test\" + \"ing\"", .value = "testing" },
        .{ .input = "\"test\" + \"ing\" + \"testing\"", .value = "testingtesting" },
    };

    inline for (test_cases) |case| {
        var vm = try Vm.init(testing.allocator);
        // vm.debug = true;
        defer vm.deinit();
        try vm.interpretSource(case.input);
        try testing.expectEqualStrings(case.value, vm.stack.last().?.*.value.string);
    }
}
