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

const allocator = std.testing.allocator;

var output: std.ArrayList([]const u8) = .empty;
const TestRunner = struct {
    pub fn onLine(vm_ptr: usize, dialogue: *ExportLine) callconv(.c) void {
        output.append(allocator, allocator.dupe(u8, dialogue.content.ptr[0..dialogue.content.len]) catch unreachable) catch unreachable;
        main.selectContinue(vm_ptr);
    }

    pub fn onChoices(vm_ptr: usize, choices: [*]ExportChoice, choices_len: u8) callconv(.c) void {
        _ = choices;
        _ = choices_len;
        main.selectChoice(vm_ptr, 0);
    }

    pub fn onValueChanged(_: usize, name_ptr: [*c]const u8, name_len: usize, _: ExportValue) callconv(.c) void {
        output.append(allocator, std.fmt.allocPrint(allocator, "onValueChanged: {s}", .{name_ptr[0..name_len]}) catch unreachable) catch unreachable;
    }

    pub fn log(msg: ExportString, severity: ExportLogger.Severity) callconv(.c) void {
        _ = msg;
        _ = severity;
    }

    pub fn free(ptr: usize) void {
        _ = ptr;
    }

    // *const fn (vm_ptr: usize, args: [*c]ExportValue, args_len: u8) callconv(.c) ExportValue;
    pub fn sum(_: usize, args: [*c]ExportValue, _: u8) callconv(.c) ExportValue {
        const arg1 = args[0].data.number;
        const arg2 = args[1].data.number;
        output.append(allocator, std.fmt.allocPrint(allocator, "extern sum {d} + {d} = {d}", .{ arg1, arg2, arg1 + arg2 }) catch unreachable) catch unreachable;
        return .{ .tag = .number, .data = .{ .number = arg1 + arg2 } };
    }
};

test "Export Create and Destroy Vm" {
    const text =
        \\ var value = "test 123"
        \\ var list = List{}
        \\ var set = Set{}
        \\ var map = Map{}
        \\ enum Enum {
        \\     One,
        \\     Two
        \\ }
        \\ var enum_value = Enum.One
        \\ extern fn sum |x, y| return x + y
        \\ === START {
        \\     :: "A person approaches." #starting
        \\     :Stranger: "Hey there. {sum(5, 4)}"
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
    defer {
        for (output.items) |o| {
            allocator.free(o);
        }
        output.deinit(allocator);
    }

    const file = try std.fs.cwd().createFile("tmp.topi", .{ .read = false });
    defer std.fs.cwd().deleteFile("tmp.topi") catch {};
    defer file.close();
    var file_buf: [1024]u8 = undefined;
    var file_writer = file.writer(&file_buf);
    const file_write = &file_writer.interface;
    try file_write.writeAll(text);
    try file_write.flush();

    try file.seekTo(0);
    const dir_path = try std.fs.cwd().realpathAlloc(allocator, ".");
    defer allocator.free(dir_path);
    const path = try std.fs.path.resolve(allocator, &.{ dir_path, "tmp.topi" ++ "\x00" });
    defer allocator.free(path);
    const path_ptr: [*:0]const u8 = path[0 .. path.len - 1 :0];
    const calc_size = main.calculateCompileSize(path_ptr, @intFromPtr(&TestRunner.log), @intFromEnum(ExportLogger.Severity.debug));
    const buf = try allocator.alloc(u8, calc_size);
    defer allocator.free(buf);
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
    const free_ptr = @intFromPtr(&TestRunner.free);
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

    const expected = &[_][]const u8{
        "A person approaches.",
        "extern sum 5 + 4 = 9",
        "Hey there. 9",
        "onValueChanged: list",
        "Oh, uh, nice to meet you. My name is Drew.",
        "Sorry, I thought you were someone I knew.",
        "I'd love to stay and chat, but this is just a short demo.",
        "They walk away...",
    };
    try std.testing.expectEqual(expected.len, output.items.len);
    for (expected, 0..) |e, i| {
        try std.testing.expectEqualSlices(u8, e, output.items[i]);
    }
}
