const std = @import("std");

const topi = @import("topi");
const Vm = topi.runtime.Vm;

const ExportValue = @import("value.zig").ExportValue;
const export_runner = @import("runner.zig");
const ExportLogger = export_runner.ExportLogger;
const ExportRunner = export_runner.ExportRunner;
const ExportFunction = export_runner.ExportFunction;
const ExportString = export_runner.ExportString;
const ExportLine = export_runner.ExportLine;
const ExportChoice = export_runner.ExportChoice;

const main = @import("main.zig");

const TestRunner = struct {
    pub fn onLine(vm_ptr: usize, dialogue: *ExportLine) callconv(.C) void {
        _ = dialogue;
        main.selectContinue(vm_ptr);
    }

    pub fn onChoices(vm_ptr: usize, choices: [*]ExportChoice, choices_len: u8) callconv(.C) void {
        _ = choices;
        _ = choices_len;
        main.selectChoice(vm_ptr, 0);
    }

    pub fn onValueChanged(_: usize, name_ptr: [*c]const u8, name_len: usize, value: ExportValue) callconv(.C) void {
        std.debug.print("onValueChanged: {s} = ", .{name_ptr[0..name_len]});
        value.print(std.debug);
        std.debug.print("\n", .{});
    }

    pub fn log(msg: ExportString, severity: ExportLogger.Severity) callconv(.C) void {
        std.debug.print("[{s}] {s}\n", .{ @tagName(severity), msg.ptr[0..msg.len] });
    }

    pub fn free(ptr: usize) void {
        std.debug.print("free memory at: {d}\n", .{ptr});
    }

    // *const fn (vm_ptr: usize, args: [*c]ExportValue, args_len: u8) callconv(.C) ExportValue;
    pub fn sum(_: usize, args: [*c]ExportValue, _: u8) callconv(.C) ExportValue {
        std.debug.print("extern sum\n", .{});
        return .{ .tag = .number, .data = .{ .number = args[0].data.number + args[1].data.number } };
    }
};

test "Create and Destroy Vm" {
    const text =
        \\ extern var value = "test 123"
        \\ extern var list = List{}
        \\ extern var set = Set{}
        \\ extern var map = Map{}
        \\ enum Enum = {
        \\     One
        \\     Two
        \\ }
        \\ extern var enum_value = Enum.One
        \\ extern var sum = |x, y| return x + y
        \\ === START {
        \\     :: "A person approaches." #starting
        \\     :Stranger: "Hey there."
        \\     print(value)
        \\     print(list)
        \\     print(set)
        \\     print(map)
        \\     print(enum_value)
        \\     list.add(3)
        \\     print(sum(11, 11))
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

    const file = try std.fs.cwd().createFile("tmp.topi", .{ .read = true });
    defer std.fs.cwd().deleteFile("tmp.topi") catch {};
    defer file.close();
    try file.writer().writeAll(text);

    try file.seekTo(0);
    const buf = try std.testing.allocator.alloc(u8, 8192);
    defer std.testing.allocator.free(buf);
    const dir_path = try std.fs.cwd().realpathAlloc(std.testing.allocator, ".");
    defer std.testing.allocator.free(dir_path);
    const path = try std.fs.path.resolve(std.testing.allocator, &.{ dir_path, "tmp.topi" ++ "\x00" });
    defer std.testing.allocator.free(path);
    const path_ptr: [*:0]const u8 = path[0 .. path.len - 1 :0];
    const calc_size = main.calculateCompileSize(path_ptr, @intFromPtr(&TestRunner.log), @intFromEnum(ExportLogger.Severity.debug));
    const compile_size = main.compile(path_ptr, buf.ptr, buf.len, @intFromPtr(&TestRunner.onValueChanged), @intFromEnum(ExportLogger.Severity.debug));
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
    var set_value = [2]ExportValue{
        .{ .tag = .string, .data = .{ .string = .{ .ptr = "some".ptr, .len = 4 } } },
        .{ .tag = .string, .data = .{ .string = .{ .ptr = "value".ptr, .len = 5 } } },
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

    const enum_value = ExportValue{
        .tag = .@"enum",
        .data = .{ .@"enum" = .{
            .name = .{ .ptr = "Enum".ptr, .len = 4 },
            .value = .{ .ptr = "Two".ptr, .len = 3 },
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
