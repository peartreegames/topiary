const std = @import("std");

const topi = @import("topi");
const Vm = topi.runtime.Vm;

const exp = @import("export");
const main = exp.main;
const ExportValue = exp.ExportValue;
const ExportLogger = exp.ExportLogger;
const ExportRunner = exp.ExportRunner;
const ExportFunction = exp.ExportFunction;
const ExportString = exp.ExportString;
const ExportLine = exp.ExportLine;
const ExportChoice = exp.ExportChoice;

const TestRunner = struct {
    pub fn onLine(vm_ptr: usize, dialogue: *ExportLine) callconv(.c) void {
        _ = dialogue;
        main.selectContinue(vm_ptr);
    }

    pub fn onChoices(vm_ptr: usize, choices: [*]ExportChoice, choices_len: u8) callconv(.c) void {
        _ = choices;
        _ = choices_len;
        main.selectChoice(vm_ptr, 0);
    }

    pub fn onValueChanged(_: usize, name_ptr: [*c]const u8, name_len: usize, value: ExportValue) callconv(.c) void {
        var buffer: [1024]u8 = undefined;
        var writer = std.fs.File.stderr().writer(&buffer);
        const stderr = &writer.interface;
        stderr.print("onValueChanged: {s} = ", .{name_ptr[0..name_len]}) catch {};
        value.print(stderr) catch {};
        stderr.print("\n", .{}) catch {};
    }

    pub fn log(msg: ExportString, severity: ExportLogger.Severity) callconv(.c) void {
        std.debug.print("[{t}] {s}\n", .{ severity, msg.ptr[0..msg.len] });
    }

    pub fn free(ptr: usize) void {
        std.debug.print("test export free memory at: {d}\n", .{ptr});
    }

    // *const fn (vm_ptr: usize, args: [*c]ExportValue, args_len: u8) callconv(.c) ExportValue;
    pub fn sum(_: usize, args: [*c]ExportValue, _: u8) callconv(.c) ExportValue {
        const arg1 = args[0].data.number;
        const arg2 = args[1].data.number;
        std.debug.print("extern sum {d} + {d} = {d}\n", .{ arg1, arg2, arg1 + arg2 });
        return .{ .tag = .number, .data = .{ .number = arg1 + arg2 } };
    }
};

test "Export Create and Destroy Vm" {
    const text =
        \\ extern var value = "test 123"
        \\ extern var list = List{}
        \\ extern var set = Set{}
        \\ extern var map = Map{}
        \\ enum Enum {
        \\     One,
        \\     Two
        \\ }
        \\ extern var enum_value = Enum.One
        \\ fn sum |x, y| return x + y
        \\ === START {
        \\     :: "A person approaches." #starting
        \\     :Stranger: "Hey there."
        \\     list.add(3)
        \\     fork^ {
        \\         ~ "Greet them." #lots #of #tags #here {
        \\             :Drew: "Oh, uh, nice to meet you. My name is Drew."
        \\             :Drew: "Sorry, I thought you were someone I knew."
        \\             :Drew: "I'd love to stay and chat, but this is just a short demo."
        \\         }
        \\         ~ "Say nothing." #one #here {
        \\             :: "The person acts as though they were addressing someone else."
        \\         }
        \\     }
        \\     :: "They walk away..."
        \\ }
        \\
    ;

    const file = try std.fs.cwd().createFile("tmp.topi", .{ .read = false });
    defer std.fs.cwd().deleteFile("tmp.topi") catch {};
    defer file.close();
    var file_buf: [1024]u8 = undefined;
    var file_writer = file.writer(&file_buf);
    const file_write = &file_writer.interface;
    try file_write.writeAll(text);
    try file_write.flush();

    try file.seekTo(0);
    const dir_path = try std.fs.cwd().realpathAlloc(std.testing.allocator, ".");
    defer std.testing.allocator.free(dir_path);
    const path = try std.fs.path.resolve(std.testing.allocator, &.{ dir_path, "tmp.topi" ++ "\x00" });
    defer std.testing.allocator.free(path);
    const path_ptr: [*:0]const u8 = path[0 .. path.len - 1 :0];
    const calc_size = main.calculateCompileSize(path_ptr, @intFromPtr(&TestRunner.log), @intFromEnum(ExportLogger.Severity.debug));
    const buf = try std.testing.allocator.alloc(u8, calc_size);
    defer std.testing.allocator.free(buf);
    const compile_size = main.compile(path_ptr, buf.ptr, buf.len, @intFromPtr(&TestRunner.log), @intFromEnum(ExportLogger.Severity.debug));
    try std.testing.expectEqual(compile_size, calc_size);

    const vm_ptr = main.createVm(
        buf.ptr,
        buf.len,
        @intFromPtr(&TestRunner.onLine),
        @intFromPtr(&TestRunner.onChoices),
        @intFromPtr(&TestRunner.onValueChanged),
        @intFromPtr(&TestRunner.log),
        @intFromEnum(ExportLogger.Severity.debug),
    );
    const vm: *Vm = @ptrFromInt(vm_ptr);

    defer main.destroyVm(vm_ptr);
    var list_value = [2]ExportValue{
        .{ .tag = .number, .data = .{ .number = 1 } },
        .{ .tag = .number, .data = .{ .number = 2 } },
    };
    const list = ExportValue{ .tag = .list, .data = .{
        .list = .{
            .items = &list_value,
            .count = 2,
        },
    } };

    const someStr = try std.testing.allocator.dupe(u8, "some");
    defer std.testing.allocator.free(someStr);
    const valueStr = try std.testing.allocator.dupe(u8, "value");
    defer std.testing.allocator.free(valueStr);
    var set_value = [2]ExportValue{
        .{ .tag = .string, .data = .{ .string = .{ .ptr = someStr.ptr, .len = someStr.len } } },
        .{ .tag = .string, .data = .{ .string = .{ .ptr = valueStr.ptr, .len = valueStr.len } } },
    };
    const set = ExportValue{
        .tag = .set,
        .data = .{ .list = .{ .items = &set_value, .count = 2 } },
    };
    var map_value = [4]ExportValue{
        .{ .tag = .number, .data = .{ .number = 0 } },
        .{ .tag = .number, .data = .{ .number = 0.0001 } },
        .{ .tag = .number, .data = .{ .number = 1 } },
        .{ .tag = .number, .data = .{ .number = 1.1111 } },
    };
    const map = ExportValue{
        .tag = .map,
        .data = .{ .list = .{ .items = &map_value, .count = 4 } },
    };

    const enumStr = try std.testing.allocator.dupe(u8, "Enum");
    defer std.testing.allocator.free(enumStr);
    const twoStr = try std.testing.allocator.dupe(u8, "Two");
    defer std.testing.allocator.free(twoStr);
    const enum_value = ExportValue{
        .tag = .@"enum",
        .data = .{ .@"enum" = .{
            .name = .{ .ptr = enumStr.ptr, .len = enumStr.len },
            .value = .{ .ptr = twoStr.ptr, .len = twoStr.len },
        } },
    };
    const free_ptr = @intFromPtr(&TestRunner.free);
    main.setExtern(vm_ptr, "list", list, free_ptr);
    main.setExtern(vm_ptr, "set", set, free_ptr);
    main.setExtern(vm_ptr, "map", map, free_ptr);
    main.setExtern(vm_ptr, "enum_value", enum_value, free_ptr);
    main.setExternFunc(vm_ptr, "sum", @intFromPtr(&TestRunner.sum), 2, free_ptr);

    const list_name = "list";
    _ = main.subscribe(vm_ptr, list_name);

    main.start(vm_ptr, "");
    while (main.canContinue(vm_ptr)) {
        main.run(vm_ptr);
        if (vm.err.msg) |msg| {
            std.log.warn("Error: {s}", .{msg});
            break;
        }
    }
    _ = main.unsubscribe(vm_ptr, list_name);
}
