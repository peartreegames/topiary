const std = @import("std");
const Value = @import("values.zig").Value;
const OpCode = @import("opcode.zig").OpCode;
const Errors = @import("compiler-error.zig").CompilerErrors;
const ValueType = @import("values.zig").Type;
const String = @import("values.zig").String;
const Scope = @import("scope.zig").Scope;
const Symbol = @import("scope.zig").Symbol;
const builtins = @import("builtins.zig").builtins;
const Bytecode = @import("bytecode.zig").Bytecode;
const JumpTree = @import("structures/jump-tree.zig").JumpTree;
const VisitTree = @import("structures/visit-tree.zig").VisitTree;
const Enum = @import("enum.zig").Enum;
const UUID = @import("utils/uuid.zig").UUID;
const compiler = @import("compiler.zig");
const module = @import("module.zig");
const parseSource = @import("parser.test.zig").parseSource;
const initial_constants = compiler.initial_constants;

const testing = std.testing;
const allocator = testing.allocator;
const cl = initial_constants.len;
const Module = module.Module;
const Compiler = compiler.Compiler;

pub fn compileSource(source: []const u8, mod: *Module) !Bytecode {
    const errWriter = std.io.getStdIn().writer();
    parseSource(source, mod) catch |err| {
        try mod.writeErrors(errWriter);
        return err;
    };

    var comp = try Compiler.init(allocator);
    defer comp.deinit();

    try comp.compile(mod);
    return comp.bytecode();
}

test "Basic Compile" {
    const test_cases = .{
        .{
            .input = "1 + 2",
            .constants = [_]Value{ .{ .number = 1 }, .{ .number = 2 } },
            .instructions = [_]u8{
                @intFromEnum(OpCode.constant),
                0 + cl,
                0,
                0,
                0,
                @intFromEnum(OpCode.constant),
                1 + cl,
                0,
                0,
                0,
                @intFromEnum(OpCode.add),
                @intFromEnum(OpCode.pop),
            },
        },
    };

    const errWriter = std.io.getStdIn().writer();
    inline for (test_cases) |case| {
        var mod = Module.create(allocator);
        defer mod.deinit();
        defer mod.entry.source_loaded = false;
        var bytecode = compileSource(case.input, &mod) catch |err| {
            try mod.writeErrors(errWriter);
            return err;
        };
        defer bytecode.free(allocator);
        for (case.instructions, 0..) |instruction, i| {
            errdefer std.log.warn("Error on {}", .{i});
            try testing.expectEqual(instruction, bytecode.instructions[i]);
        }
        for (case.constants, 0..) |constant, i| {
            switch (constant) {
                .number => |n| try testing.expectEqual(n, bytecode.constants[i + cl].number),
                else => continue,
            }
        }
    }
}

test "Conditionals Compile" {
    const test_cases = .{
        .{
            .input = "if true { 10 } 333",
            .constants = [_]Value{ .{ .number = 10 }, .{ .number = 333 } },
            .instructions = [_]u8{
                @intFromEnum(OpCode.true),
                @intFromEnum(OpCode.jump_if_false),
                17,
                0,
                0,
                0,
                @intFromEnum(OpCode.constant),
                0 + cl,
                0,
                0,
                0,
                @intFromEnum(OpCode.pop),
                @intFromEnum(OpCode.jump),
                19,
                0,
                0,
                0,
                @intFromEnum(OpCode.nil),
                @intFromEnum(OpCode.pop),
                @intFromEnum(OpCode.constant),
                1 + cl,
                0,
                0,
                0,
                @intFromEnum(OpCode.pop),
            },
        },
        .{
            .input = "if true { 10 } else { 20 } 333",
            .constants = [_]Value{ .{ .number = 10 }, .{ .number = 20 }, .{ .number = 333 } },
            .instructions = [_]u8{
                @intFromEnum(OpCode.true),
                @intFromEnum(OpCode.jump_if_false),
                17,
                0,
                0,
                0,
                @intFromEnum(OpCode.constant),
                0 + cl,
                0,
                0,
                0,
                @intFromEnum(OpCode.pop),
                @intFromEnum(OpCode.jump),
                23,
                0,
                0,
                0,
                @intFromEnum(OpCode.constant),
                1 + cl,
                0,
                0,
                0,
                @intFromEnum(OpCode.pop),
                @intFromEnum(OpCode.constant),
                2 + cl,
                0,
                0,
                0,
                @intFromEnum(OpCode.pop),
            },
        },
    };

    const errWriter = std.io.getStdIn().writer();
    inline for (test_cases) |case| {
        var mod = Module.create(allocator);
        defer mod.deinit();
        defer mod.entry.source_loaded = false;
        var bytecode = compileSource(case.input, &mod) catch |err| {
            try mod.writeErrors(errWriter);
            return err;
        };
        defer bytecode.free(allocator);
        for (case.instructions, 0..) |instruction, i| {
            errdefer std.log.warn("Error on instruction:{}", .{i});
            try testing.expectEqual(instruction, bytecode.instructions[i]);
        }
        for (case.constants, 0..) |constant, i| {
            switch (constant) {
                .number => |n| try testing.expectEqual(n, bytecode.constants[i + cl].number),
                else => continue,
            }
        }
    }
}

test "Variables" {
    const test_cases = .{
        .{
            .input =
            \\ var one = 1
            \\ var two = 2
            ,
            .constants = [_]Value{ .{ .number = 1 }, .{ .number = 2 } },
            .instructions = [_]u8{
                @intFromEnum(OpCode.constant),
                0 + cl,
                0,
                0,
                0,
                @intFromEnum(OpCode.decl_global),
                0,
                0,
                0,
                0,
                @intFromEnum(OpCode.constant),
                1 + cl,
                0,
                0,
                0,
                @intFromEnum(OpCode.decl_global),
                1,
                0,
                0,
                0,
            },
        },
        .{
            .input =
            \\ var one = 1
            \\ var two = one
            \\ two
            ,
            .constants = [_]Value{.{ .number = 1 }},
            .instructions = [_]u8{
                @intFromEnum(OpCode.constant),
                0 + cl,
                0,
                0,
                0,
                @intFromEnum(OpCode.decl_global),
                0,
                0,
                0,
                0,
                @intFromEnum(OpCode.get_global),
                0,
                0,
                0,
                0,
                @intFromEnum(OpCode.decl_global),
                1,
                0,
                0,
                0,
                @intFromEnum(OpCode.get_global),
                1,
                0,
                0,
                0,
                @intFromEnum(OpCode.pop),
            },
        },
    };

    const errWriter = std.io.getStdIn().writer();
    inline for (test_cases) |case| {
        var mod = Module.create(allocator);
        defer mod.deinit();
        defer mod.entry.source_loaded = false;
        var bytecode = compileSource(case.input, &mod) catch |err| {
            try mod.writeErrors(errWriter);
            return err;
        };
        defer bytecode.free(allocator);
        for (case.instructions, 0..) |instruction, i| {
            errdefer std.log.warn("{s} -- {}", .{ case.input, i });
            try testing.expectEqual(instruction, bytecode.instructions[i]);
        }
        for (case.constants, 0..) |constant, i| {
            switch (constant) {
                .number => |n| try testing.expectEqual(n, bytecode.constants[i + cl].number),
                else => continue,
            }
        }
    }
}

test "Strings" {
    const test_cases = .{
        .{
            .input = "\"testing\"",
            .constants = [_][]const u8{"testing"},
            .instructions = [_]u8{
                @intFromEnum(OpCode.string),
                0 + cl,
                0,
                0,
                0,
                0,
                @intFromEnum(OpCode.pop),
            },
        },
        .{
            .input = "\"test\" + \"ing\"",
            .constants = [_][]const u8{ "test", "ing" },
            .instructions = [_]u8{
                @intFromEnum(OpCode.string),
                0 + cl,
                0,
                0,
                0,
                0,
                @intFromEnum(OpCode.string),
                1 + cl,
                0,
                0,
                0,
                0,
                @intFromEnum(OpCode.add),
                @intFromEnum(OpCode.pop),
            },
        },
    };

    const errWriter = std.io.getStdIn().writer();
    inline for (test_cases) |case| {
        var mod = Module.create(allocator);
        defer mod.deinit();
        defer mod.entry.source_loaded = false;
        var bytecode = compileSource(case.input, &mod) catch |err| {
            try mod.writeErrors(errWriter);
            return err;
        };
        defer bytecode.free(allocator);
        for (case.instructions, 0..) |instruction, i| {
            errdefer std.log.warn("{}", .{i});
            try testing.expectEqual(instruction, bytecode.instructions[i]);
        }
        for (case.constants, 0..) |constant, i| {
            try testing.expectEqualStrings(constant, bytecode.constants[i + cl].obj.data.string);
        }
    }
}

test "Lists" {
    const test_cases = .{
        .{
            .input = "List{}",
            .constants = [_]f32{},
            .instructions = [_]u8{
                @intFromEnum(OpCode.list),
                0,
                0,
                @intFromEnum(OpCode.pop),
            },
        },
        .{
            .input = "List{1,2,3}",
            .constants = [_]f32{ 1, 2, 3 },
            .instructions = [_]u8{
                @intFromEnum(OpCode.constant),
                0 + cl,
                0,
                0,
                0,
                @intFromEnum(OpCode.constant),
                1 + cl,
                0,
                0,
                0,
                @intFromEnum(OpCode.constant),
                2 + cl,
                0,
                0,
                0,
                @intFromEnum(OpCode.list),
                3,
                0,
                @intFromEnum(OpCode.pop),
            },
        },
        .{
            .input = "List{1 + 2, 3 - 4, 5 * 6}",
            .constants = [_]f32{ 1, 2, 3, 4, 5, 6 },
            .instructions = [_]u8{
                @intFromEnum(OpCode.constant),
                0 + cl,
                0,
                0,
                0,
                @intFromEnum(OpCode.constant),
                1 + cl,
                0,
                0,
                0,
                @intFromEnum(OpCode.add),
                @intFromEnum(OpCode.constant),
                2 + cl,
                0,
                0,
                0,
                @intFromEnum(OpCode.constant),
                3 + cl,
                0,
                0,
                0,
                @intFromEnum(OpCode.subtract),
                @intFromEnum(OpCode.constant),
                4 + cl,
                0,
                0,
                0,
                @intFromEnum(OpCode.constant),
                5 + cl,
                0,
                0,
                0,
                @intFromEnum(OpCode.multiply),
                @intFromEnum(OpCode.list),
                3,
                0,
                @intFromEnum(OpCode.pop),
            },
        },
    };

    const errWriter = std.io.getStdIn().writer();
    inline for (test_cases) |case| {
        var mod = Module.create(allocator);
        defer mod.deinit();
        defer mod.entry.source_loaded = false;
        var bytecode = compileSource(case.input, &mod) catch |err| {
            try mod.writeErrors(errWriter);
            return err;
        };
        defer bytecode.free(allocator);
        for (case.instructions, 0..) |instruction, i| {
            errdefer std.log.warn("{}", .{i});
            try testing.expectEqual(instruction, bytecode.instructions[i]);
        }

        for (case.constants, 0..) |constant, i| {
            try testing.expect(constant == bytecode.constants[i + cl].number);
        }
    }
}

test "Maps and Sets" {
    // {} denotes a group as well as a map/set,
    // wrap it in a () to force group expression statements
    const test_cases = .{
        .{
            .input = "Map{}",
            .constants = [_]f32{},
            .instructions = [_]u8{
                @intFromEnum(OpCode.map),
                0,
                0,
                @intFromEnum(OpCode.pop),
            },
        },
        .{
            .input = "Map{1: 2, 3: 4, 5: 6}",
            .constants = [_]f32{ 1, 2, 3, 4, 5, 6 },
            .instructions = [_]u8{
                @intFromEnum(OpCode.constant),
                0 + cl,
                0,
                0,
                0,
                @intFromEnum(OpCode.constant),
                1 + cl,
                0,
                0,
                0,
                @intFromEnum(OpCode.constant),
                2 + cl,
                0,
                0,
                0,
                @intFromEnum(OpCode.constant),
                3 + cl,
                0,
                0,
                0,
                @intFromEnum(OpCode.constant),
                4 + cl,
                0,
                0,
                0,
                @intFromEnum(OpCode.constant),
                5 + cl,
                0,
                0,
                0,
                @intFromEnum(OpCode.map),
                3,
                0,
                @intFromEnum(OpCode.pop),
            },
        },
        .{
            .input = "Map{1: 2 + 3, 4: 5 * 6}",
            .constants = [_]f32{ 1, 2, 3, 4, 5, 6 },
            .instructions = [_]u8{
                @intFromEnum(OpCode.constant),
                0 + cl,
                0,
                0,
                0,
                @intFromEnum(OpCode.constant),
                1 + cl,
                0,
                0,
                0,
                @intFromEnum(OpCode.constant),
                2 + cl,
                0,
                0,
                0,
                @intFromEnum(OpCode.add),
                @intFromEnum(OpCode.constant),
                3 + cl,
                0,
                0,
                0,
                @intFromEnum(OpCode.constant),
                4 + cl,
                0,
                0,
                0,
                @intFromEnum(OpCode.constant),
                5 + cl,
                0,
                0,
                0,
                @intFromEnum(OpCode.multiply),
                @intFromEnum(OpCode.map),
                2,
                0,
                @intFromEnum(OpCode.pop),
            },
        },
        .{
            .input = "Set{}",
            .constants = [_]f32{},
            .instructions = [_]u8{
                @intFromEnum(OpCode.set),
                0,
                0,
                @intFromEnum(OpCode.pop),
            },
        },
        .{
            .input = "Set{1, 2, 3}",
            .constants = [_]f32{ 1, 2, 3 },
            .instructions = [_]u8{
                @intFromEnum(OpCode.constant),
                0 + cl,
                0,
                0,
                0,
                @intFromEnum(OpCode.constant),
                1 + cl,
                0,
                0,
                0,
                @intFromEnum(OpCode.constant),
                2 + cl,
                0,
                0,
                0,
                @intFromEnum(OpCode.set),
                3,
                0,
                @intFromEnum(OpCode.pop),
            },
        },
        .{
            .input = "Set{1 + 2, 3 * 4, 5 - 6}",
            .constants = [_]f32{ 1, 2, 3, 4, 5, 6 },
            .instructions = [_]u8{
                @intFromEnum(OpCode.constant),
                0 + cl,
                0,
                0,
                0,
                @intFromEnum(OpCode.constant),
                1 + cl,
                0,
                0,
                0,
                @intFromEnum(OpCode.add),
                @intFromEnum(OpCode.constant),
                2 + cl,
                0,
                0,
                0,
                @intFromEnum(OpCode.constant),
                3 + cl,
                0,
                0,
                0,
                @intFromEnum(OpCode.multiply),
                @intFromEnum(OpCode.constant),
                4 + cl,
                0,
                0,
                0,
                @intFromEnum(OpCode.constant),
                5 + cl,
                0,
                0,
                0,
                @intFromEnum(OpCode.subtract),
                @intFromEnum(OpCode.set),
                3,
                0,
                @intFromEnum(OpCode.pop),
            },
        },
    };

    const errWriter = std.io.getStdIn().writer();
    inline for (test_cases) |case| {
        var mod = Module.create(allocator);
        defer mod.deinit();
        defer mod.entry.source_loaded = false;
        var bytecode = compileSource(case.input, &mod) catch |err| {
            try mod.writeErrors(errWriter);
            return err;
        };
        defer bytecode.free(allocator);
        for (case.instructions, 0..) |instruction, i| {
            errdefer std.log.warn("Error on: {} \n{s}", .{ i, case.input });
            try testing.expectEqual(instruction, bytecode.instructions[i]);
        }

        for (case.constants, 0..) |constant, i| {
            try testing.expect(constant == bytecode.constants[i + cl].number);
        }
    }
}

test "Index" {
    const test_cases = .{
        .{
            .input = "List{1,2,3}[1 + 1]",
            .constants = [_]f32{ 1, 2, 3, 1, 1 },
            .instructions = [_]u8{
                @intFromEnum(OpCode.constant),
                0 + cl,
                0,
                0,
                0,
                @intFromEnum(OpCode.constant),
                1 + cl,
                0,
                0,
                0,
                @intFromEnum(OpCode.constant),
                2 + cl,
                0,
                0,
                0,
                @intFromEnum(OpCode.list),
                3,
                0,
                @intFromEnum(OpCode.constant),
                3 + cl,
                0,
                0,
                0,
                @intFromEnum(OpCode.constant),
                4 + cl,
                0,
                0,
                0,
                @intFromEnum(OpCode.add),
                @intFromEnum(OpCode.index),
                @intFromEnum(OpCode.pop),
            },
        },
        .{
            .input = "Map{1: 2}[2 - 1]",
            .constants = [_]f32{ 1, 2, 2, 1 },
            .instructions = [_]u8{
                @intFromEnum(OpCode.constant),
                0 + cl,
                0,
                0,
                0,
                @intFromEnum(OpCode.constant),
                1 + cl,
                0,
                0,
                0,
                @intFromEnum(OpCode.map),
                1,
                0,
                @intFromEnum(OpCode.constant),
                2 + cl,
                0,
                0,
                0,
                @intFromEnum(OpCode.constant),
                3 + cl,
                0,
                0,
                0,
                @intFromEnum(OpCode.subtract),
                @intFromEnum(OpCode.index),
                @intFromEnum(OpCode.pop),
            },
        },
    };

    const errWriter = std.io.getStdIn().writer();
    inline for (test_cases) |case| {
        var mod = Module.create(allocator);
        defer mod.deinit();
        defer mod.entry.source_loaded = false;
        var bytecode = compileSource(case.input, &mod) catch |err| {
            try mod.writeErrors(errWriter);
            return err;
        };
        defer bytecode.free(allocator);
        for (case.instructions, 0..) |instruction, i| {
            errdefer std.log.warn("Error on: {}", .{i});
            try testing.expectEqual(instruction, bytecode.instructions[i]);
        }

        for (case.constants, 0..) |constant, i| {
            try testing.expect(constant == bytecode.constants[i + cl].number);
        }
    }
}

test "Functions" {
    const test_cases = .{
        .{
            .input = "|| return 5 + 10",
            .instructions = [_]u8{
                @intFromEnum(OpCode.closure),
                2 + cl,
                0,
                0,
                0,
                0,
                @intFromEnum(OpCode.pop),
            },
            .constants = [_]f32{ 5, 10 },
            .functions = [_][]const u8{
                &[_]u8{},
                &[_]u8{},
                &[_]u8{
                    @intFromEnum(OpCode.constant),
                    0 + cl,
                    0,
                    0,
                    0,
                    @intFromEnum(OpCode.constant),
                    1 + cl,
                    0,
                    0,
                    0,
                    @intFromEnum(OpCode.add),
                    @intFromEnum(OpCode.return_value),
                },
            },
        },
        .{
            .input = "|| 5 + 10",
            .instructions = [_]u8{
                @intFromEnum(OpCode.closure),
                2 + cl,
                0,
                0,
                0,
                0,
                @intFromEnum(OpCode.pop),
            },
            .constants = [_]f32{ 5, 10 },
            .functions = [_][]const u8{
                &[_]u8{},
                &[_]u8{},
                &[_]u8{
                    @intFromEnum(OpCode.constant),
                    0 + cl,
                    0,
                    0,
                    0,
                    @intFromEnum(OpCode.constant),
                    1 + cl,
                    0,
                    0,
                    0,
                    @intFromEnum(OpCode.add),
                    @intFromEnum(OpCode.pop),
                    @intFromEnum(OpCode.return_void),
                },
            },
        },
        .{
            .input = "|| { 5 + 10 return void }",
            .instructions = [_]u8{
                @intFromEnum(OpCode.closure),
                2 + cl,
                0,
                0,
                0,
                0,
                @intFromEnum(OpCode.pop),
            },
            .constants = [_]f32{ 5, 10 },
            .functions = [_][]const u8{
                &[_]u8{},
                &[_]u8{},
                &[_]u8{
                    @intFromEnum(OpCode.constant),
                    0 + cl,
                    0,
                    0,
                    0,
                    @intFromEnum(OpCode.constant),
                    1 + cl,
                    0,
                    0,
                    0,
                    @intFromEnum(OpCode.add),
                    @intFromEnum(OpCode.pop),
                    @intFromEnum(OpCode.return_void),
                },
            },
        },
        .{
            .input = "|| {}",
            .instructions = [_]u8{
                @intFromEnum(OpCode.closure),
                0 + cl,
                0,
                0,
                0,
                0,
                @intFromEnum(OpCode.pop),
            },
            .constants = [_]f32{ 0, 0, 0 },
            .functions = [_][]const u8{
                &[_]u8{
                    @intFromEnum(OpCode.return_void),
                },
            },
        },
        .{
            .input = "|| { return 5 }()",
            .instructions = [_]u8{
                @intFromEnum(OpCode.closure),
                1 + cl,
                0,
                0,
                0,
                0,
                @intFromEnum(OpCode.call),
                0,
                @intFromEnum(OpCode.pop),
            },
            .constants = [_]f32{5},
            .functions = [_][]const u8{
                &[_]u8{},
                &[_]u8{
                    @intFromEnum(OpCode.constant),
                    0 + cl,
                    0,
                    0,
                    0,
                    @intFromEnum(OpCode.return_value),
                },
            },
        },
        .{
            .input =
            \\ const five = || return 5
            \\ five()
            ,
            .instructions = [_]u8{
                @intFromEnum(OpCode.closure),
                1 + cl,
                0,
                0,
                0,
                0,
                @intFromEnum(OpCode.decl_global),
                0,
                0,
                0,
                0,
                @intFromEnum(OpCode.get_global),
                0,
                0,
                0,
                0,
                @intFromEnum(OpCode.call),
                0,
                @intFromEnum(OpCode.pop),
            },
            .constants = [_]f32{5},
            .functions = [_][]const u8{
                &[_]u8{},
                &[_]u8{
                    @intFromEnum(OpCode.constant),
                    0 + cl,
                    0,
                    0,
                    0,
                    @intFromEnum(OpCode.return_value),
                },
            },
        },
        .{
            .input =
            \\ const two = || return 2
            \\ two() + two()
            ,
            .instructions = [_]u8{
                @intFromEnum(OpCode.closure),
                1 + cl,
                0,
                0,
                0,
                0,
                @intFromEnum(OpCode.decl_global),
                0,
                0,
                0,
                0,
                @intFromEnum(OpCode.get_global),
                0,
                0,
                0,
                0,
                @intFromEnum(OpCode.call),
                0,
                @intFromEnum(OpCode.get_global),
                0,
                0,
                0,
                0,
                @intFromEnum(OpCode.call),
                0,
                @intFromEnum(OpCode.add),
                @intFromEnum(OpCode.pop),
            },
            .constants = [_]f32{2},
            .functions = [_][]const u8{
                &[_]u8{},
                &[_]u8{
                    @intFromEnum(OpCode.constant),
                    0 + cl,
                    0,
                    0,
                    0,
                    @intFromEnum(OpCode.return_value),
                },
            },
        },
        .{
            .input =
            \\ const value = |a| return a
            \\ value(1) + value(1)
            ,
            .instructions = [_]u8{
                @intFromEnum(OpCode.closure),
                0 + cl,
                0,
                0,
                0,
                0,
                @intFromEnum(OpCode.decl_global),
                0,
                0,
                0,
                0,
                @intFromEnum(OpCode.get_global),
                0,
                0,
                0,
                0,
                @intFromEnum(OpCode.constant),
                1 + cl,
                0,
                0,
                0,
                @intFromEnum(OpCode.call),
                1,
                @intFromEnum(OpCode.get_global),
                0,
                0,
                0,
                0,
                @intFromEnum(OpCode.constant),
                2 + cl,
                0,
                0,
                0,
                @intFromEnum(OpCode.call),
                1,
                @intFromEnum(OpCode.add),
                @intFromEnum(OpCode.pop),
            },
            .constants = [_]f32{ 0, 1, 1 },
            .functions = [_][]const u8{
                &[_]u8{
                    @intFromEnum(OpCode.get_local),    0, 0,
                    @intFromEnum(OpCode.return_value),
                },
            },
        },
        .{
            .input =
            \\ const oneArg = |a| { return a }
            \\ oneArg(24)
            ,
            .instructions = [_]u8{
                @intFromEnum(OpCode.closure),
                0 + cl,
                0,
                0,
                0,
                0,
                @intFromEnum(OpCode.decl_global),
                0,
                0,
                0,
                0,
                @intFromEnum(OpCode.get_global),
                0,
                0,
                0,
                0,
                @intFromEnum(OpCode.constant),
                1 + cl,
                0,
                0,
                0,
                @intFromEnum(OpCode.call),
                1,
                @intFromEnum(OpCode.pop),
            },
            .constants = [_]f32{ 0, 24 },
            .functions = [_][]const u8{
                &[_]u8{
                    @intFromEnum(OpCode.get_local),    0, 0,
                    @intFromEnum(OpCode.return_value),
                },
            },
        },
        .{
            .input =
            \\ const multiArg = |a, b, c| { a b return c }
            \\ multiArg(24,25,26)
            ,
            .instructions = [_]u8{
                @intFromEnum(OpCode.closure),
                0 + cl,
                0,
                0,
                0,
                0,
                @intFromEnum(OpCode.decl_global),
                0,
                0,
                0,
                0,
                @intFromEnum(OpCode.get_global),
                0,
                0,
                0,
                0,
                @intFromEnum(OpCode.constant),
                1 + cl,
                0,
                0,
                0,
                @intFromEnum(OpCode.constant),
                2 + cl,
                0,
                0,
                0,
                @intFromEnum(OpCode.constant),
                3 + cl,
                0,
                0,
                0,
                @intFromEnum(OpCode.call),
                3,
                @intFromEnum(OpCode.pop),
            },
            .constants = [_]f32{ 0, 24, 25, 26 },
            .functions = [_][]const u8{
                &[_]u8{
                    @intFromEnum(OpCode.get_local),
                    0,
                    0,
                    @intFromEnum(OpCode.pop),
                    @intFromEnum(OpCode.get_local),
                    1,
                    0,
                    @intFromEnum(OpCode.pop),
                    @intFromEnum(OpCode.get_local),
                    2,
                    0,
                    @intFromEnum(OpCode.return_value),
                },
            },
        },
    };

    const errWriter = std.io.getStdIn().writer();
    inline for (test_cases) |case| {
        errdefer std.log.warn("{s}", .{case.input});
        var mod = Module.create(allocator);
        defer mod.deinit();
        defer mod.entry.source_loaded = false;
        var bytecode = compileSource(case.input, &mod) catch |err| {
            try mod.writeErrors(errWriter);
            return err;
        };
        defer bytecode.free(allocator);
        errdefer bytecode.print(std.debug);
        for (case.instructions, 0..) |instruction, i| {
            errdefer std.log.warn("Error on: {}\n {s}", .{ i, case.input });
            try testing.expectEqual(instruction, bytecode.instructions[i]);
        }
        for (bytecode.constants[cl..], 0..) |constant, i| {
            switch (constant) {
                .number => |n| try testing.expect(case.constants[i] == n),
                .obj => |o| try testing.expectEqualSlices(u8, case.functions[i], o.data.function.instructions),
                else => unreachable,
            }
        }
    }
}

test "Locals" {
    const test_cases = .{
        .{
            .input =
            \\ const num = 5
            \\ || return num
            ,
            .instructions = [_]u8{
                @intFromEnum(OpCode.constant),
                0 + cl,
                0,
                0,
                0,
                @intFromEnum(OpCode.decl_global),
                0,
                0,
                0,
                0,
                @intFromEnum(OpCode.closure),
                1 + cl,
                0,
                0,
                0,
                0,
                @intFromEnum(OpCode.pop),
            },
            .constants = [_]f32{5},
            .functions = [_][]const u8{
                &[_]u8{},
                &[_]u8{
                    @intFromEnum(OpCode.get_global),
                    0,
                    0,
                    0,
                    0,
                    @intFromEnum(OpCode.return_value),
                },
            },
        },
        .{
            .input =
            \\ || {
            \\     const num = 5
            \\     return num 
            \\ }
            ,
            .instructions = [_]u8{
                @intFromEnum(OpCode.closure),
                1 + cl,
                0,
                0,
                0,
                0,
                @intFromEnum(OpCode.pop),
            },
            .constants = [_]f32{5},
            .functions = [_][]const u8{
                &[_]u8{},
                &[_]u8{
                    @intFromEnum(OpCode.constant),
                    0 + cl,
                    0,
                    0,
                    0,
                    @intFromEnum(OpCode.set_local),
                    0,
                    0,
                    @intFromEnum(OpCode.get_local),
                    0,
                    0,
                    @intFromEnum(OpCode.return_value),
                },
            },
        },
        .{
            .input =
            \\ || {
            \\    const a = 5
            \\    const b = 7
            \\    return a + b
            \\ }
            ,
            .instructions = [_]u8{
                @intFromEnum(OpCode.closure),
                2 + cl,
                0,
                0,
                0,
                0,
                @intFromEnum(OpCode.pop),
            },
            .constants = [_]f32{ 5, 7 },
            .functions = [_][]const u8{
                &[_]u8{},
                &[_]u8{},
                &[_]u8{
                    @intFromEnum(OpCode.constant),
                    0 + cl,
                    0,
                    0,
                    0,
                    @intFromEnum(OpCode.set_local),
                    0,
                    0,
                    @intFromEnum(OpCode.constant),
                    1 + cl,
                    0,
                    0,
                    0,
                    @intFromEnum(OpCode.set_local),
                    1,
                    0,
                    @intFromEnum(OpCode.get_local),
                    0,
                    0,
                    @intFromEnum(OpCode.get_local),
                    1,
                    0,
                    @intFromEnum(OpCode.add),
                    @intFromEnum(OpCode.return_value),
                },
            },
        },
    };

    const errWriter = std.io.getStdIn().writer();
    inline for (test_cases) |case| {
        errdefer std.log.warn("{s}", .{case.input});
        var mod = Module.create(allocator);
        defer mod.deinit();
        defer mod.entry.source_loaded = false;
        var bytecode = compileSource(case.input, &mod) catch |err| {
            try mod.writeErrors(errWriter);
            return err;
        };
        defer bytecode.free(allocator);
        for (case.instructions, 0..) |instruction, i| {
            errdefer std.log.warn("Error on: {}", .{i});
            try testing.expectEqual(instruction, bytecode.instructions[i]);
        }
        for (bytecode.constants[cl..], 0..) |constant, i| {
            switch (constant) {
                .number => |n| try testing.expect(case.constants[i] == n),
                .obj => |o| try testing.expectEqualSlices(u8, case.functions[i], o.data.function.instructions),
                else => unreachable,
            }
        }
    }
}

test "Builtin Functions" {
    const test_cases = .{
        .{
            .input =
            \\ rnd(1, 10)
            ,
            .instructions = [_]u8{
                @intFromEnum(OpCode.get_builtin),
                0,
                @intFromEnum(OpCode.constant),
                0 + cl,
                0,
                0,
                0,
                @intFromEnum(OpCode.constant),
                1 + cl,
                0,
                0,
                0,
                @intFromEnum(OpCode.call),
                2,
                @intFromEnum(OpCode.pop),
            },
            .constants = [_]f32{ 1, 10 },
        },
        .{
            .input = "rnd01()",
            .instructions = [_]u8{
                @intFromEnum(OpCode.get_builtin),
                1,
                @intFromEnum(OpCode.call),
                0,
                @intFromEnum(OpCode.pop),
            },
            .constants = [_]f32{0},
        },
    };

    const errWriter = std.io.getStdIn().writer();
    inline for (test_cases) |case| {
        errdefer std.log.warn("{s}", .{case.input});
        var mod = Module.create(allocator);
        defer mod.deinit();
        defer mod.entry.source_loaded = false;
        var bytecode = compileSource(case.input, &mod) catch |err| {
            try mod.writeErrors(errWriter);
            return err;
        };
        defer bytecode.free(allocator);
        errdefer bytecode.print(std.debug);
        for (case.instructions, 0..) |instruction, i| {
            errdefer std.log.warn("Error on: {}", .{i});
            try testing.expectEqual(instruction, bytecode.instructions[i]);
        }
        for (bytecode.constants[cl..], 0..) |constant, i| {
            try testing.expect(case.constants[i] == constant.number);
        }
    }
}

test "Closures" {
    const test_cases = .{
        .{
            .input = "|a| { return |b| { return a + b } }",
            .instructions = [_]u8{
                @intFromEnum(OpCode.closure),
                1 + cl,
                0,
                0,
                0,
                0,
                @intFromEnum(OpCode.pop),
            },
            .constants = [_]f32{0},
            .functions = [_][]const u8{
                &[_]u8{
                    @intFromEnum(OpCode.get_free),
                    0,
                    @intFromEnum(OpCode.get_local),
                    0,
                    0,
                    @intFromEnum(OpCode.add),
                    @intFromEnum(OpCode.return_value),
                },
                &[_]u8{
                    @intFromEnum(OpCode.get_local),
                    0,
                    0,
                    @intFromEnum(OpCode.closure),
                    0 + cl,
                    0,
                    0,
                    0,
                    1,
                    @intFromEnum(OpCode.return_value),
                },
            },
        },
        .{
            .input =
            \\ |a| {
            \\     return |b| { 
            \\        return |c| return a + b + c 
            \\     }
            \\ }
            ,
            .instructions = [_]u8{
                @intFromEnum(OpCode.closure),
                2 + cl,
                0,
                0,
                0,
                0,
                @intFromEnum(OpCode.pop),
            },
            .constants = [_]f32{0},
            .functions = [_][]const u8{
                &[_]u8{
                    @intFromEnum(OpCode.get_free),
                    0,
                    @intFromEnum(OpCode.get_free),
                    1,
                    @intFromEnum(OpCode.add),
                    @intFromEnum(OpCode.get_local),
                    0,
                    0,
                    @intFromEnum(OpCode.add),
                    @intFromEnum(OpCode.return_value),
                },
                &[_]u8{
                    @intFromEnum(OpCode.get_free),
                    0,
                    @intFromEnum(OpCode.get_local),
                    0,
                    0,
                    @intFromEnum(OpCode.closure),
                    0 + cl,
                    0,
                    0,
                    0,
                    2,
                    @intFromEnum(OpCode.return_value),
                },
                &[_]u8{
                    @intFromEnum(OpCode.get_local),
                    0,
                    0,
                    @intFromEnum(OpCode.closure),
                    1 + cl,
                    0,
                    0,
                    0,
                    1,
                    @intFromEnum(OpCode.return_value),
                },
            },
        },
        .{
            .input =
            \\ const globalNum = 55
            \\ || {
            \\     const a = 66
            \\     return || {
            \\        const b = 77 
            \\        return || {
            \\            const c = 88 
            \\            return globalNum + a + b + c
            \\        }
            \\     } 
            \\ }
            ,
            .instructions = [_]u8{
                @intFromEnum(OpCode.constant),
                0 + cl,
                0,
                0,
                0,
                @intFromEnum(OpCode.decl_global),
                0,
                0,
                0,
                0,
                @intFromEnum(OpCode.closure),
                6 + cl,
                0,
                0,
                0,
                0,
                @intFromEnum(OpCode.pop),
            },
            .constants = [_]f32{ 55, 66, 77, 88 },
            .functions = [_][]const u8{
                &[_]u8{},
                &[_]u8{},
                &[_]u8{},
                &[_]u8{},
                &[_]u8{
                    @intFromEnum(OpCode.constant),
                    3 + cl,
                    0,
                    0,
                    0,
                    @intFromEnum(OpCode.set_local),
                    0,
                    0,
                    @intFromEnum(OpCode.get_global),
                    0,
                    0,
                    0,
                    0,
                    @intFromEnum(OpCode.get_free),
                    0,
                    @intFromEnum(OpCode.add),
                    @intFromEnum(OpCode.get_free),
                    1,
                    @intFromEnum(OpCode.add),
                    @intFromEnum(OpCode.get_local),
                    0,
                    0,
                    @intFromEnum(OpCode.add),
                    @intFromEnum(OpCode.return_value),
                },
                &[_]u8{
                    @intFromEnum(OpCode.constant),
                    2 + cl,
                    0,
                    0,
                    0,
                    @intFromEnum(OpCode.set_local),
                    0,
                    0,
                    @intFromEnum(OpCode.get_free),
                    0,
                    @intFromEnum(OpCode.get_local),
                    0,
                    0,
                    @intFromEnum(OpCode.closure),
                    4 + cl,
                    0,
                    0,
                    0,
                    2,
                    @intFromEnum(OpCode.return_value),
                },
                &[_]u8{
                    @intFromEnum(OpCode.constant),
                    1 + cl,
                    0,
                    0,
                    0,
                    @intFromEnum(OpCode.set_local),
                    0,
                    0,
                    @intFromEnum(OpCode.get_local),
                    0,
                    0,
                    @intFromEnum(OpCode.closure),
                    5 + cl,
                    0,
                    0,
                    0,
                    1,
                    @intFromEnum(OpCode.return_value),
                },
            },
        },
        .{
            .input =
            \\ const countDown =|x| return countDown(x - 1)
            \\ countDown(1)
            ,
            .instructions = [_]u8{
                @intFromEnum(OpCode.closure),
                1 + cl,
                0,
                0,
                0,
                0,
                @intFromEnum(OpCode.decl_global),
                0,
                0,
                0,
                0,
                @intFromEnum(OpCode.get_global),
                0,
                0,
                0,
                0,
                @intFromEnum(OpCode.constant),
                2 + cl,
                0,
                0,
                0,
                @intFromEnum(OpCode.call),
                1,
                @intFromEnum(OpCode.pop),
            },
            .constants = [_]f32{ 1, 0, 1 },
            .functions = [_][]const u8{
                &[_]u8{},
                &[_]u8{
                    @intFromEnum(OpCode.current_closure),
                    @intFromEnum(OpCode.get_local),
                    0,
                    0,
                    @intFromEnum(OpCode.constant),
                    0 + cl,
                    0,
                    0,
                    0,
                    @intFromEnum(OpCode.subtract),
                    @intFromEnum(OpCode.call),
                    1,
                    @intFromEnum(OpCode.return_value),
                },
            },
        },
    };

    const errWriter = std.io.getStdIn().writer();
    inline for (test_cases) |case| {
        errdefer std.log.warn("{s}", .{case.input});
        var mod = Module.create(allocator);
        defer mod.deinit();
        defer mod.entry.source_loaded = false;
        var bytecode = compileSource(case.input, &mod) catch |err| {
            try mod.writeErrors(errWriter);
            return err;
        };
        defer bytecode.free(allocator);
        errdefer bytecode.print(std.debug);
        for (case.instructions, 0..) |instruction, i| {
            errdefer std.log.warn("Error on: {}", .{i});
            try testing.expectEqual(instruction, bytecode.instructions[i]);
        }
        for (bytecode.constants[cl..], 0..) |constant, i| {
            switch (constant) {
                .number => |n| try testing.expect(case.constants[i] == n),
                .obj => |o| try testing.expectEqualSlices(u8, case.functions[i], o.data.function.instructions),
                else => unreachable,
            }
        }
    }
}

test "Classes" {
    const TestValue = union(enum(u4)) {
        number: f32,
        string: []const u8,
    };
    const test_cases = .{
        .{
            .input =
            \\ class Test = {
            \\     value = 0    
            \\ }
            \\ new Test{}.value
            \\
            ,
            .instructions = [_]u8{
                @intFromEnum(OpCode.constant),
                0 + cl,
                0,
                0,
                0,
                @intFromEnum(OpCode.constant),
                1 + cl,
                0,
                0,
                0,
                @intFromEnum(OpCode.constant),
                2 + cl,
                0,
                0,
                0,
                @intFromEnum(OpCode.class),
                1,
                @intFromEnum(OpCode.decl_global),
                0,
                0,
                0,
                0,
                @intFromEnum(OpCode.get_global),
                0,
                0,
                0,
                0,
                @intFromEnum(OpCode.instance),
            },
            .constants = [_]TestValue{
                .{ .number = 0.0 },
                .{ .string = "value" },
                .{ .string = "Test" },
            },
        },
    };

    const errWriter = std.io.getStdIn().writer();
    inline for (test_cases) |case| {
        errdefer std.log.warn("{s}", .{case.input});
        var mod = Module.create(allocator);
        defer mod.deinit();
        defer mod.entry.source_loaded = false;
        var bytecode = compileSource(case.input, &mod) catch |err| {
            try mod.writeErrors(errWriter);
            return err;
        };
        defer bytecode.free(allocator);
        errdefer bytecode.print(std.debug);
        for (case.instructions, 0..) |instruction, i| {
            errdefer std.log.warn("Error on: {}", .{i});
            try testing.expectEqual(instruction, bytecode.instructions[i]);
        }
        for (case.constants, 0..) |constant, i| {
            switch (constant) {
                .number => |n| try testing.expect(n == bytecode.constants[i + cl].number),
                .string => |s| try testing.expectEqualStrings(s, bytecode.constants[i + cl].obj.data.string),
            }
        }
    }
}

test "Global Jump Error" {
    const input =
        \\ === START {}
        \\ => START
    ;

    var mod = Module.create(allocator);
    defer mod.deinit();
    defer mod.entry.source_loaded = false;
    const err = compileSource(input, &mod);
    const errWriter = std.io.getStdErr().writer();
    try mod.writeErrors(errWriter);

    try testing.expect(Compiler.Error.CompilerError == err);
}

test "Serialize" {
    const input =
        \\ var str = "string value"
        \\ const num = 25
        \\ const fun = |x| {
        \\     return x * 2
        \\ }
        \\ var list = List{"one", "two"}
        \\ const set = Set{1, 2, 3.3}
        \\ const map = Map{1:2.2, 3: 4.4}
    ;

    const errWriter = std.io.getStdIn().writer();
    errdefer std.log.warn("{s}", .{input});
    var mod = Module.create(allocator);
    defer mod.deinit();
    defer mod.entry.source_loaded = false;
    var bytecode = compileSource(input, &mod) catch |err| {
        try mod.writeErrors(errWriter);
        return err;
    };
    defer bytecode.free(allocator);

    // this doesn't need to be a file, but it's nice to sometimes not delete it and inspect it
    const file = try std.fs.cwd().createFile("tmp.topib", .{ .read = true });
    defer std.fs.cwd().deleteFile("tmp.topib") catch {};
    defer file.close();
    try bytecode.serialize(file.writer());

    try file.seekTo(0);
    var deserialized = try Bytecode.deserialize(allocator, file.reader());
    defer deserialized.free(allocator);
    try testing.expectEqualSlices(u8, bytecode.instructions, deserialized.instructions);
    for (bytecode.constants, 0..) |constant, i| {
        try testing.expect(constant.eql(deserialized.constants[i]));
    }
}
