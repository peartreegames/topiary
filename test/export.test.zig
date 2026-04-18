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

fn clearOutput() void {
    for (output.items) |o| {
        allocator.free(o);
    }
    output.clearRetainingCapacity();
}

/// Helper: write source to a temp file, compile via the export API, return
/// the bytecode buffer (caller owns).
fn compileSource(text: []const u8) ![]u8 {
    const test_io = std.testing.io;
    const cwd = std.Io.Dir.cwd();
    const file = try cwd.createFile(test_io, "tmp_export.topi", .{});
    defer file.close(test_io);
    var file_buf: [1024]u8 = undefined;
    var file_writer = file.writer(test_io, &file_buf);
    const file_write = &file_writer.interface;
    try file_write.writeAll(text);
    try file_write.flush();

    const dir_path = try cwd.realPathFileAlloc(test_io, ".", allocator);
    defer allocator.free(dir_path);
    const path = try std.fs.path.resolve(allocator, &.{ dir_path, "tmp_export.topi" ++ "\x00" });
    defer allocator.free(path);
    const path_ptr: [*:0]const u8 = path[0 .. path.len - 1 :0];
    const calc_size = main.calculateCompileSize(path_ptr, @ptrCast(&TestRunner.log), @intFromEnum(ExportLogger.Severity.debug));
    if (calc_size == 0) return error.CompileFailed;
    const buf = try allocator.alloc(u8, calc_size);
    errdefer allocator.free(buf);
    const compile_size = main.compile(path_ptr, buf.ptr, buf.len, @ptrCast(&TestRunner.log), @intFromEnum(ExportLogger.Severity.debug));
    try std.testing.expectEqual(compile_size, calc_size);
    return buf;
}

const TestRunner = struct {
    pub fn onLine(vm_ptr: *anyopaque, dialogue: *ExportLine) callconv(.c) void {
        output.append(allocator, allocator.dupe(u8, dialogue.content.ptr[0..dialogue.content.len]) catch unreachable) catch unreachable;
        main.selectContinue(vm_ptr);
    }

    pub fn onChoices(vm_ptr: *anyopaque, choices: [*]ExportChoice, choices_len: u8) callconv(.c) void {
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

    var free_called: bool = false;

    pub fn free(ptr: usize) callconv(.c) void {
        if (ptr == 0) return;
        free_called = true;
        std.c.free(@ptrFromInt(ptr));
    }

    // *const fn (vm_ptr: *anyopaque, args: [*c]ExportValue, args_len: u8) callconv(.c) ExportValue;
    pub fn sum(_: usize, args: [*c]ExportValue, _: u8) callconv(.c) ExportValue {
        const arg1 = args[0].data.number;
        const arg2 = args[1].data.number;
        output.append(allocator, std.fmt.allocPrint(allocator, "extern sum {d} + {d} = {d}", .{ arg1, arg2, arg1 + arg2 }) catch unreachable) catch unreachable;
        return .{ .tag = .number, .data = .{ .number = arg1 + arg2 } };
    }

    /// Extern function that returns a string ExportValue allocated with C malloc.
    /// Tests the string ownership path: toValue dupes the string then calls free.
    pub fn greet(_: usize, _: [*c]ExportValue, _: u8) callconv(.c) ExportValue {
        const greeting = "hello from C";
        const ptr: [*]u8 = @ptrCast(std.c.malloc(greeting.len) orelse unreachable);
        @memcpy(ptr[0..greeting.len], greeting);
        return .{ .tag = .string, .data = .{ .string = .{ .ptr = ptr, .len = greeting.len } } };
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
        clearOutput();
        output.deinit(allocator);
    }

    const buf = try compileSource(text);
    defer allocator.free(buf);
    defer std.Io.Dir.cwd().deleteFile(std.testing.io, "tmp_export.topi") catch {};

    const vm_ptr = main.createVm(
        buf.ptr,
        buf.len,
        @ptrCast(&TestRunner.onLine),
        @ptrCast(&TestRunner.onChoices),
        @ptrCast(&TestRunner.onValueChanged),
        @ptrCast(&TestRunner.log),
        @intFromEnum(ExportLogger.Severity.debug),
    ) orelse return error.InternalError;
    const vm: *Vm = @ptrCast(@alignCast(vm_ptr));

    defer main.destroyVm(vm_ptr);
    const free_ptr: *const anyopaque = @ptrCast(&TestRunner.free);
    main.setExternFunc(vm_ptr, "sum", @ptrCast(&TestRunner.sum), 2, free_ptr);

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

test "Export State Save and Load" {
    const text =
        \\ var count = 0
        \\ count += 10
        \\ var name = "hello"
        \\ === START {
        \\     :: "count is {count}"
        \\ }
    ;
    output = .empty;
    defer {
        clearOutput();
        output.deinit(allocator);
    }

    const buf = try compileSource(text);
    defer allocator.free(buf);
    defer std.Io.Dir.cwd().deleteFile(std.testing.io, "tmp_export.topi") catch {};

    // Create first VM, run to completion
    const vm1_ptr = main.createVm(
        buf.ptr,
        buf.len,
        @ptrCast(&TestRunner.onLine),
        @ptrCast(&TestRunner.onChoices),
        @ptrCast(&TestRunner.onValueChanged),
        @ptrCast(&TestRunner.log),
        @intFromEnum(ExportLogger.Severity.debug),
    ) orelse return error.InternalError;
    defer main.destroyVm(vm1_ptr);

    main.start(vm1_ptr, "");
    const vm1: *Vm = @ptrCast(@alignCast(vm1_ptr));
    while (main.canContinue(vm1_ptr)) {
        main.run(vm1_ptr);
        if (vm1.err.msg) |_| break;
    }
    try std.testing.expectEqual(@as(usize, 1), output.items.len);
    try std.testing.expectEqualSlices(u8, "count is 10", output.items[0]);

    // Save state via FFI
    const state_size = main.calculateStateSize(vm1_ptr);
    try std.testing.expect(state_size > 0);
    const state_buf = try allocator.allocSentinel(u8, state_size, 0);
    defer allocator.free(state_buf);
    const saved_size = main.saveState(vm1_ptr, state_buf.ptr, state_buf.len);
    try std.testing.expect(saved_size > 0);

    // Create second VM from same bytecode, load state
    clearOutput();
    const vm2_ptr = main.createVm(
        buf.ptr,
        buf.len,
        @ptrCast(&TestRunner.onLine),
        @ptrCast(&TestRunner.onChoices),
        @ptrCast(&TestRunner.onValueChanged),
        @ptrCast(&TestRunner.log),
        @intFromEnum(ExportLogger.Severity.debug),
    ) orelse return error.InternalError;
    defer main.destroyVm(vm2_ptr);

    const loaded = main.loadState(vm2_ptr, state_buf.ptr, saved_size);
    try std.testing.expect(loaded);

    // Run second VM — should produce "count is 10" (restored state + re-exec)
    main.start(vm2_ptr, "");
    const vm2: *Vm = @ptrCast(@alignCast(vm2_ptr));
    while (main.canContinue(vm2_ptr)) {
        main.run(vm2_ptr);
        if (vm2.err.msg) |_| break;
    }

    // After interpret, count = restored(10) then += 10 again = 20
    try std.testing.expectEqual(@as(usize, 1), output.items.len);
    try std.testing.expectEqualSlices(u8, "count is 20", output.items[0]);
}

test "Export Invalid Bytecode Returns Null" {
    // Garbage bytes should cause createVm to return null, not crash.
    const garbage = [_]u8{ 0xDE, 0xAD, 0xBE, 0xEF } ** 16;
    const result = main.createVm(
        &garbage,
        garbage.len,
        @ptrCast(&TestRunner.onLine),
        @ptrCast(&TestRunner.onChoices),
        @ptrCast(&TestRunner.onValueChanged),
        @ptrCast(&TestRunner.log),
        @intFromEnum(ExportLogger.Severity.debug),
    );
    try std.testing.expect(result == null);

    // Empty input
    const empty = [_]u8{0};
    const result2 = main.createVm(
        &empty,
        0,
        @ptrCast(&TestRunner.onLine),
        @ptrCast(&TestRunner.onChoices),
        @ptrCast(&TestRunner.onValueChanged),
        @ptrCast(&TestRunner.log),
        @intFromEnum(ExportLogger.Severity.debug),
    );
    try std.testing.expect(result2 == null);
}

test "Export Subscribe and Unsubscribe Lifecycle" {
    const text =
        \\ var count = 0
        \\ === START {
        \\     count = 1
        \\     :: "line1"
        \\     count = 2
        \\     :: "line2"
        \\     count = 3
        \\     :: "line3"
        \\ }
    ;
    output = .empty;
    defer {
        clearOutput();
        output.deinit(allocator);
    }

    const buf = try compileSource(text);
    defer allocator.free(buf);
    defer std.Io.Dir.cwd().deleteFile(std.testing.io, "tmp_export.topi") catch {};

    const vm_ptr = main.createVm(
        buf.ptr,
        buf.len,
        @ptrCast(&TestRunner.onLine),
        @ptrCast(&TestRunner.onChoices),
        @ptrCast(&TestRunner.onValueChanged),
        @ptrCast(&TestRunner.log),
        @intFromEnum(ExportLogger.Severity.debug),
    ) orelse return error.InternalError;
    defer main.destroyVm(vm_ptr);

    // Subscribe to count
    const subscribed = main.subscribe(vm_ptr, "count");
    try std.testing.expect(subscribed);

    // Subscribing to a non-existent variable should return false
    const bad_sub = main.subscribe(vm_ptr, "nonexistent");
    try std.testing.expect(!bad_sub);

    main.start(vm_ptr, "");
    const vm: *Vm = @ptrCast(@alignCast(vm_ptr));
    while (main.canContinue(vm_ptr)) {
        main.run(vm_ptr);
        if (vm.err.msg) |_| break;
    }

    // Should have onValueChanged callbacks interleaved with lines
    // count=1, "line1", count=2, "line2", count=3, "line3"
    var value_changed_count: usize = 0;
    for (output.items) |item| {
        if (std.mem.startsWith(u8, item, "onValueChanged")) {
            value_changed_count += 1;
        }
    }
    try std.testing.expectEqual(@as(usize, 3), value_changed_count);

    // Unsubscribe
    const unsub = main.unsubscribe(vm_ptr, "count");
    try std.testing.expect(unsub);

    // Unsubscribing again should return false
    const unsub2 = main.unsubscribe(vm_ptr, "count");
    try std.testing.expect(!unsub2);
}

test "Export Extern Function Returning String" {
    const text =
        \\ extern fn greet || return "default"
        \\ === START {
        \\     :: "{greet()}"
        \\ }
    ;
    output = .empty;
    defer {
        clearOutput();
        output.deinit(allocator);
    }

    const buf = try compileSource(text);
    defer allocator.free(buf);
    defer std.Io.Dir.cwd().deleteFile(std.testing.io, "tmp_export.topi") catch {};

    const vm_ptr = main.createVm(
        buf.ptr,
        buf.len,
        @ptrCast(&TestRunner.onLine),
        @ptrCast(&TestRunner.onChoices),
        @ptrCast(&TestRunner.onValueChanged),
        @ptrCast(&TestRunner.log),
        @intFromEnum(ExportLogger.Severity.debug),
    ) orelse return error.InternalError;
    defer main.destroyVm(vm_ptr);

    TestRunner.free_called = false;
    const free_ptr: *const anyopaque = @ptrCast(&TestRunner.free);
    main.setExternFunc(vm_ptr, "greet", @ptrCast(&TestRunner.greet), 0, free_ptr);

    main.start(vm_ptr, "");
    const vm: *Vm = @ptrCast(@alignCast(vm_ptr));
    while (main.canContinue(vm_ptr)) {
        main.run(vm_ptr);
        if (vm.err.msg) |_| break;
    }

    try std.testing.expectEqual(@as(usize, 1), output.items.len);
    try std.testing.expectEqualSlices(u8, "hello from C", output.items[0]);
    // Verify the free callback was invoked to release the C-allocated string
    try std.testing.expect(TestRunner.free_called);
}
