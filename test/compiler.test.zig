const std = @import("std");

const topi = @import("topi");
const Value = topi.types.Value;
const ValueType = topi.types.Type;
const String = topi.types.String;
const Function = topi.types.Function;

const Errors = topi.backend.CompilerErrors;
const Bytecode = topi.backend.Bytecode;
const Compiler = topi.backend.Compiler;
const OpCode = topi.backend.OpCode;
const Scope = topi.backend.Scope;
const Symbol = topi.backend.Symbol;
const builtins = topi.runtime.builtins;

const parseSource = @import("parser.test.zig").parseSource;

const Module = topi.module.Module;
const File = topi.module.File;

const testing = std.testing;
const allocator = testing.allocator;
const cl = builtins.values().len;

pub fn compileSource(source: []const u8, mod: *Module) !Bytecode {
    const file = try mod.arena.allocator().create(File);
    file.* = .{
        .path = "",
        .name = "",
        .dir_name = "",
        .source = source,
        .module = mod,
    };
    mod.entry = file;
    try mod.includes.putNoClobber(file.path, file);
    return mod.generateBytecode(allocator);
}

test "Compile Basic" {
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

    inline for (test_cases) |case| {
        var mod = try Module.initEmpty(allocator);
        defer mod.deinit();
        var bytecode = try compileSource(case.input, mod);
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

test "Compile Conditionals" {
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

    inline for (test_cases) |case| {
        var mod = try Module.initEmpty(allocator);
        defer mod.deinit();
        var bytecode = try compileSource(case.input, mod);
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

test "Compile Variables" {
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

    inline for (test_cases) |case| {
        var mod = try Module.initEmpty(allocator);
        defer mod.deinit();
        var bytecode = try compileSource(case.input, mod);
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

test "Compile Strings" {
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

    inline for (test_cases) |case| {
        var mod = try Module.initEmpty(allocator);
        defer mod.deinit();
        var bytecode = try compileSource(case.input, mod);
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

test "Compile Lists" {
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

    inline for (test_cases) |case| {
        var mod = try Module.initEmpty(allocator);
        defer mod.deinit();
        var bytecode = try compileSource(case.input, mod);
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

test "Compile Maps and Sets" {
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

    inline for (test_cases) |case| {
        var mod = try Module.initEmpty(allocator);
        defer mod.deinit();
        var bytecode = try compileSource(case.input, mod);
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

test "Compile Index" {
    const test_cases = .{
        .{
            .input = "List{1,2,3}[1 + 1]",
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
                @intFromEnum(OpCode.constant),
                0 + cl,
                0,
                0,
                0,
                @intFromEnum(OpCode.constant),
                0 + cl,
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
            .constants = [_]f32{ 1, 2 },
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
                1 + cl,
                0,
                0,
                0,
                @intFromEnum(OpCode.constant),
                0 + cl,
                0,
                0,
                0,
                @intFromEnum(OpCode.subtract),
                @intFromEnum(OpCode.index),
                @intFromEnum(OpCode.pop),
            },
        },
    };

    inline for (test_cases) |case| {
        var mod = try Module.initEmpty(allocator);
        defer mod.deinit();
        var bytecode = try compileSource(case.input, mod);
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

test "Compile Functions" {
    var five = try allocator.create(Value.Obj);
    five.data = .{ .function = .{
        .locals_count = 0,
        .arity = 0,
        .debug_info = undefined,
        .instructions = &[_]u8{
            @intFromEnum(OpCode.constant),
            1 + cl,
            0,
            0,
            0,
            @intFromEnum(OpCode.return_value),
        },
    } };
    defer allocator.destroy(five);

    var two = try allocator.create(Value.Obj);
    two.data = .{ .function = .{
        .locals_count = 0,
        .arity = 0,
        .debug_info = undefined,
        .instructions = &[_]u8{
            @intFromEnum(OpCode.constant),
            1 + cl,
            0,
            0,
            0,
            @intFromEnum(OpCode.return_value),
        },
    } };
    defer allocator.destroy(two);

    var value = try allocator.create(Value.Obj);
    value.data = .{ .function = .{
        .debug_info = undefined,
        .locals_count = 0,
        .arity = 1,
        .instructions = &[_]u8{
            @intFromEnum(OpCode.get_local),    0, 0,
            @intFromEnum(OpCode.return_value),
        },
    } };
    defer allocator.destroy(value);

    var multi_arg = try allocator.create(Value.Obj);
    multi_arg.data = .{ .function = .{
        .debug_info = undefined,
        .locals_count = 0,
        .arity = 1,
        .instructions = &[_]u8{
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
    } };
    defer allocator.destroy(multi_arg);

    const test_cases = .{
        .{
            .input =
            \\ fn five || { return 5 }
            \\ five()
            ,
            .instructions = [_]u8{
                @intFromEnum(OpCode.constant),
                0 + cl,
                0,
                0,
                0,
                @intFromEnum(OpCode.call),
                0,
                @intFromEnum(OpCode.pop),
            },
            .constants = [_]Value{ .{ .obj = five }, .{ .number = 5 } },
        },
        .{
            .input =
            \\ fn two || { return 2 }
            \\ two() + two()
            ,
            .instructions = [_]u8{
                @intFromEnum(OpCode.constant),
                0 + cl,
                0,
                0,
                0,
                @intFromEnum(OpCode.call),
                0,
                @intFromEnum(OpCode.constant),
                0 + cl,
                0,
                0,
                0,
                @intFromEnum(OpCode.call),
                0,
                @intFromEnum(OpCode.add),
                @intFromEnum(OpCode.pop),
            },
            .constants = [_]Value{ .{ .obj = two }, .{ .number = 2 } },
        },
        .{
            .input =
            \\ fn value |a| { return a }
            \\ value(1) + value(1)
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
                @intFromEnum(OpCode.call),
                1,
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
                1,
                @intFromEnum(OpCode.add),
                @intFromEnum(OpCode.pop),
            },
            .constants = [_]Value{.{ .obj = value }, .{ .number = 1 }},
        },
        .{
            .input =
            \\ fn multiArg |a, b, c| { a b return c }
            \\ multiArg(24, 25, 26)
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
                @intFromEnum(OpCode.constant),
                3 + cl,
                0,
                0,
                0,
                @intFromEnum(OpCode.call),
                3,
                @intFromEnum(OpCode.pop),
            },
            .constants = [_]Value{ .{ .obj = multi_arg }, .{ .number = 24 }, .{ .number = 25 }, .{ .number = 26 } },
        },
    };

    inline for (test_cases) |case| {
        errdefer std.log.warn("{s}", .{case.input});
        var mod = try Module.initEmpty(allocator);
        defer mod.deinit();
        var bytecode = try compileSource(case.input, mod);
        defer bytecode.free(allocator);
        for (case.instructions, 0..) |instruction, i| {
            errdefer {
                std.log.warn("Error on: {}\n {s}", .{ i, case.input });
                var errbuf: [128]u8 = undefined;
                var stderr_writer = std.fs.File.stderr().writer(&errbuf);
                const stderr = &stderr_writer.interface;
                bytecode.print(stderr) catch unreachable;
            }
            try testing.expectEqual(instruction, bytecode.instructions[i]);
        }
        for (bytecode.constants[cl..], 0..) |constant, i| {
            switch (constant) {
                .number => |n| try testing.expect(case.constants[i].number == n),
                .obj => |o| try testing.expectEqualSlices(u8, case.constants[i].obj.data.function.instructions, o.data.function.instructions),
                else => unreachable,
            }
        }
    }
}

test "Compile Builtin Functions" {
    const test_cases = .{
        .{
            .input =
            \\ rnd(1, 10)
            ,
            .instructions = [_]u8{
                @intFromEnum(OpCode.constant),
                0,
                0,
                0,
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
                @intFromEnum(OpCode.constant),
                2,
                0,
                0,
                0,
                @intFromEnum(OpCode.call),
                0,
                @intFromEnum(OpCode.pop),
            },
            .constants = [_]f32{0},
        },
    };

    inline for (test_cases) |case| {
        errdefer std.log.warn("{s}", .{case.input});
        var mod = try Module.initEmpty(allocator);
        defer mod.deinit();
        var bytecode = try compileSource(case.input, mod);
        defer bytecode.free(allocator);
        for (case.instructions, 0..) |instruction, i| {
            errdefer std.log.warn("Error on: {}", .{i});
            try testing.expectEqual(instruction, bytecode.instructions[i]);
        }
        for (bytecode.constants[cl..], 0..) |constant, i| {
            try testing.expect(case.constants[i] == constant.number);
        }
    }
}

test "Compile Classes" {
    const TestValue = union(enum(u4)) {
        number: f32,
        string: []const u8,
        class: []const u8,
    };
    const test_cases = .{
        .{
            .input =
            \\ class Test {
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
                @intFromEnum(OpCode.instance),
                0,
                @intFromEnum(OpCode.constant),
                1 + cl,
                0,
                0,
                0,
                @intFromEnum(OpCode.index),
                1,
            },
            .constants = [_]TestValue{
                .{ .class = "Test" },
                .{ .string = "value" },
            },
        },
    };

    inline for (test_cases) |case| {
        errdefer std.log.warn("{s}", .{case.input});
        var mod = try Module.initEmpty(allocator);
        defer mod.deinit();
        var bytecode = try compileSource(case.input, mod);
        defer bytecode.free(allocator);
        for (case.instructions, 0..) |instruction, i| {
            errdefer std.log.warn("Error on: {}", .{i});
            try testing.expectEqual(instruction, bytecode.instructions[i]);
        }
        for (case.constants, 0..) |constant, i| {
            switch (constant) {
                .number => |n| try testing.expect(n == bytecode.constants[i + cl].number),
                .string => |s| try testing.expectEqualStrings(s, bytecode.constants[i + cl].const_string),
                .class => |c| try testing.expectEqualStrings(c, bytecode.constants[i + cl].obj.data.class.name),
            }
        }
    }
}

test "Compile Enums Error" {
    const tests = .{
        \\ enum TimeOfDay {
        \\  Morning,
        \\  Afternoon,
        \\  Evening,
        \\  Night
        \\ }
        \\
        \\ var time = TimeOfDay.morn
        ,
        \\ enum TimeOfDay {
        \\  Morning,
        \\  Afternoon,
        \\  Evening,
        \\  Night
        \\ }
        \\
        \\ TimeOfDay.Morning = 5
        ,
        \\ enum TimeOfDay {
        \\  Morning,
        \\ }
        \\
        \\ TimeOfDay = 5
    };

    inline for (tests) |input| {
        var mod = try Module.initEmpty(allocator);
        defer mod.deinit();
        const err = compileSource(input, mod);
        try testing.expectError(error.CompilerError, err);
    }
}

test "Compile Serialization" {
    const input =
        \\ var str = "string value"
        \\ const num = 25
        \\ fn fun |x| {
        \\     return x * 2
        \\ }
        \\ var list = List{
        \\    "one", // comment on one
        \\    "two"
        \\ }
        \\ const set = Set{1, 2, 3.3}
        \\ const map = Map{1:2.2, 3: 4.4}
    ;

    var mod = try Module.initEmpty(allocator);
    defer mod.deinit();
    var bytecode = try compileSource(input, mod);
    defer bytecode.free(allocator);

    // this doesn't need to be a file, but it's nice to sometimes not delete it and inspect it
    var file = try std.fs.cwd().createFile("tmp.topi.byte", .{ .read = true });
    defer std.fs.cwd().deleteFile("tmp.topi.byte") catch {};
    defer file.close();
    var buf: [1024]u8 = undefined;
    var file_writer = file.writer(&buf);
    _ = try bytecode.serialize(allocator, &file_writer.interface);
    std.log.info("Serialized bytecode size: {}", .{(try file.stat()).size});

    try file.seekTo(0);
    var file_reader = file.reader(&buf);
    const reader = &file_reader.interface;
    var deserialized = try Bytecode.deserialize(allocator, reader);
    defer deserialized.free(allocator);
    try testing.expectEqualSlices(u8, bytecode.instructions, deserialized.instructions);
    for (bytecode.constants, 0..) |constant, i| {
        try testing.expect(constant.eql(deserialized.constants[i]));
    }
}
