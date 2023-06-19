const std = @import("std");
const Compiler = @import("./compiler/compiler.zig").Compiler;
const ByteCode = @import("./compiler/compiler.zig").ByteCode;
const Errors = @import("./compiler/errors.zig").Errors;
const parser = @import("./compiler/parser.zig");
const OpCode = @import("./compiler/opcode.zig").OpCode;
const Value = @import("./values.zig").Value;

const testing = std.testing;
const stack_size = 2048;

const InterpretError = error{
    CompileError,
    RuntimeError,
};

const InterpretResult = union(enum) {
    Completed,
    Paused,
};

pub const Vm = struct {
    bytecode: ByteCode,
    stack: [stack_size]Value,
    ip: *u8,
    allocator: std.mem.Allocator,

    pub fn interpret(self: *Vm, bytecode: ByteCode) InterpretError!void {
        self.bytecode = bytecode;
        self.ip = &bytecode[0];
        self.run();
    }

    fn run(self: *Vm) InterpretError!void {
        while (true) : (self.ip += 1) {
            const op = @ptrCast(OpCode, self.ip);
            switch (op) {
                .constant => {
                    var i = @ptrToInt(self.ip);
                    var index = std.mem.readIntSliceBig(u16, self.bytecode.instructions[i..(i + 2)]);
                    self.ip += 2;
                    std.debug.print("{d:0>4}\n", .{index});
                },
                else => break,
            }
        }
    }
};

test "Basics" {
    const input = "1";

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
    try compiler.print(std.debug);

    var bytecode = try compiler.bytecode();
    defer bytecode.free(testing.allocator);
}
