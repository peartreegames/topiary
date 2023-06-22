const std = @import("std");
const Compiler = @import("./compiler/compiler.zig").Compiler;
const ByteCode = @import("./compiler/compiler.zig").ByteCode;
const Errors = @import("./compiler/error.zig").Errors;
const parser = @import("./compiler/parser.zig");
const OpCode = @import("./compiler/opcode.zig").OpCode;
const values = @import("./compiler/values.zig");
const Stack = @import("./stack.zig").Stack;

const testing = std.testing;
const stack_size = 2048;
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
    bytecode: ByteCode = undefined,
    ip: usize = 0,
    stack: Stack(Value),
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) !Vm {
        return .{
            .allocator = allocator,
            .stack = try Stack(Value).init(allocator, stack_size),
        };
    }

    pub fn deinit(self: *Vm) void {
        self.stack.deinit();
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
        defer bytecode.free(testing.allocator);
        Compiler.print(bytecode, null, std.debug);

        try self.interpret(bytecode);
    }

    fn readByte(self: *Vm) u8 {
        const byte = self.bytecode.instructions[self.ip];
        self.ip += 1;
        return byte;
    }

    fn run(self: *Vm) !void {
        while (self.ip < self.bytecode.instructions.len) {
            const instruction = self.readByte();
            const op = @intToEnum(OpCode, instruction);
            switch (op) {
                .constant => {
                    var index = std.mem.readIntSliceBig(u16, self.bytecode.instructions[self.ip..(self.ip + 2)]);
                    var value = self.bytecode.constants[index];
                    self.ip += 2;
                    self.push(Value.create(f32, value.number));
                },
                .pop => _ = self.pop(),
                .add => {
                    const right = self.pop();
                    const left = self.pop();
                    if (@TypeOf(right) != @TypeOf(left)) return error.RuntimeError;
                    switch (right) {
                        .number => self.push(Value.create(f32, right.number + left.number)),
                        // .string => self.push(Value.create([]const u8, right.string ++ left.string)),
                        else => return error.RuntimeError,
                    }
                },
                .subtract, .multiply, .divide, .modulus => try self.binaryNumberOp(op),
                .equal, .not_equal, .greater_than => try self.comparisonOp(op),
                .true => self.push(values.True),
                .false => self.push(values.False),
                .nil => self.push(values.Nil),
                .negate => self.push(Value.create(f32, -self.pop().number)),
                .not => {
                    const value = self.pop().bool;
                    if (value) {
                        self.push(values.False);
                    } else if (!value) {
                        self.push(values.True);
                    } else self.push(values.False);
                },
                .jump => {
                    var dest = std.mem.readIntSliceBig(u16, self.bytecode.instructions[self.ip..(self.ip + 2)]);
                    self.ip = dest;
                },
                .jump_if_false => {
                    var dest = std.mem.readIntSliceBig(u16, self.bytecode.instructions[self.ip..(self.ip + 2)]);
                    self.ip += 2;
                    var condition = self.pop();
                    if (!condition.bool) {
                        self.ip = dest;
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
        self.push(Value.create(f32, total));
    }

    fn comparisonOp(self: *Vm, op: OpCode) !void {
        const right = self.pop();
        const left = self.pop();
        if (@enumToInt(right) != @enumToInt(left)) return error.RuntimeError;
        switch (op) {
            .equal => self.push(Value.create(bool, right.equals(left))),
            .not_equal => self.push(Value.create(bool, !right.equals(left))),
            .greater_than => self.push(Value.create(bool, left.number > right.number)),
            else => return error.UnknownOperator,
        }
    }

    fn push(self: *Vm, value: Value) void {
        self.stack.push(value);
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
        .{ .input = "if (true) { 10 }", .value = 10.0, .type = f32 },
        .{ .input = "if (true) { 10 } else { 20 }", .value = 10.0, .type = f32 },
        .{ .input = "if (false) { 10 } else { 20 }", .value = 20.0, .type = f32 },
        .{ .input = "if (1 == 1) { 10 }", .value = 10.0, .type = f32 },
        .{ .input = "if (false) { 10 }", .value = null, .type = null },
    };

    inline for (test_cases) |case| {
        var vm = try Vm.init(testing.allocator);
        defer vm.deinit();
        try vm.interpretSource(case.input);
        switch (case.type) {
            f32 => try testing.expect(case.value == vm.stack.last().?.number),
            null => try testing.expect(case.value == vm.stack.last().?.nil),
            else => continue,
        }
    }
}
