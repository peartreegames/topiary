const std = @import("std");
const Compiler = @import("./compiler/compiler.zig").Compiler;
const ByteCode = @import("./compiler/compiler.zig").ByteCode;
const Errors = @import("./compiler/error.zig").Errors;
const parser = @import("./compiler/parser.zig");
const OpCode = @import("./compiler/opcode.zig").OpCode;
const values = @import("./values.zig");
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
                    // if (!std.mem.eql(u8, @tagName(right), @tagName(left))) return error.RuntimeError;
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
                .negate => self.push(Value.create(f32, -self.pop().number)),
                .@"return" => break,
            }
        }
    }

    fn binaryNumberOp(self: *Vm, op: OpCode) !void {
        const right = self.pop().number;
        const left = self.pop().number;
        if (@TypeOf(right) != @TypeOf(left)) return error.RuntimeError;
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
        if (@TypeOf(right) != @TypeOf(left)) return error.RuntimeError;
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
    };

    inline for (test_cases) |case| {
        const input = case.input;
        var allocator = testing.allocator;
        var errors = Errors.init(allocator);
        defer errors.deinit();
        const tree = parser.parse(allocator, input, &errors) catch |err| {
            try errors.write(input, std.io.getStdErr().writer());
            return err;
        };
        defer tree.deinit();
        var compiler = Compiler.init(allocator);
        defer compiler.deinit();
        try compiler.compile(tree);
        // compiler.print(std.debug);

        var bytecode = try compiler.bytecode();
        defer bytecode.free(testing.allocator);
        // bytecode.print(std.debug);

        var vm = try Vm.init(allocator);
        defer vm.deinit();
        try vm.interpret(bytecode);
        // std.debug.print("{} == {}\n", .{ case.value, vm.stack.last().? });
        switch (case.type) {
            f32 => try testing.expect(case.value == vm.stack.last().?.number),
            bool => try testing.expect(case.value == vm.stack.last().?.bool),
            else => continue,
        }
    }
}
