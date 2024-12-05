const std = @import("std");

const topi = @import("topi");
const Vm = topi.runtime.Vm;
const Runner = topi.runtime.Runner;
const State = topi.runtime.State;

const Value = topi.types.Value;

const Scope = topi.backend.Scope;
const Compiler = topi.backend.Compiler;
const Errors = topi.backend.CompilerErrors;

const Locale = topi.locale.Locale;

const version = @import("build").version;

const File = topi.module.File;
const Module = topi.module.Module;

const runner = @import("runner.zig");
const CliRunner = runner.CliRunner;
const AutoTestRunner = runner.AutoTestRunner;

fn usage(comptime msg: []const u8) !void {
    var out = std.io.getStdErr().writer();
    if (!std.mem.eql(u8, msg, "")) {
        try out.print(msg, .{});
        try out.print("\n", .{});
    }
    try out.print("topi - command line topiary processor\n", .{});
    try out.print("Usage:\n", .{});
    try out.print("        topi <command> <file> [flags]\n", .{});
    try out.print("\n", .{});
    try out.print("Commands:\n", .{});
    try out.print("        topi version\n", .{});
    try out.print("        topi run <file> [start_bough] [--auto|-a] [--lang language_key] [--verbose]\n", .{});
    try out.print("        topi test <file> <count> [--quiet] [--verbose]\n", .{});
    try out.print("        topi compile <file> <output_file|--dry|-d> [--loc] [--verbose]\n", .{});
    try out.print("        topi loc validate <file> [--verbose]\n", .{});
    try out.print("        topi loc export <file> <output_file|--dry|-d> [--verbose]\n", .{});
    try out.print("\n", .{});
    try out.print("Flags:\n", .{});
    try out.print("        --verbose: Output debug logs\n", .{});
    try out.print("        --auto, -a: Automatically continue to next line\n", .{});
    try out.print("        --lang: Localization language key\n", .{});
    try out.print("        --loc: Include localization in compiled bytecode\n", .{});
    try out.print("        --dry, -d: Dry-run only\n", .{});
}

fn checkVerbose(err: anytype) !void {
    var args = try std.process.argsWithAllocator(std.heap.page_allocator);
    defer args.deinit();
    while (args.next()) |arg| {
        if (std.mem.eql(u8, arg, "--verbose")) return err;
    }
    return;
}

fn checkFlag(flag: []const u8) !bool {
    var args = try std.process.argsWithAllocator(std.heap.page_allocator);
    defer args.deinit();
    while (args.next()) |arg| {
        if (std.mem.eql(u8, arg, flag)) return true;
    }
    return false;
}

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    var args = try std.process.argsWithAllocator(arena.allocator());
    _ = args.skip();

    const maybe_cmd = args.next();
    if (maybe_cmd == null) return usage("");
    const cmd = maybe_cmd.?;

    const is_version = std.mem.eql(u8, cmd, "version");
    if (is_version) {
        const out = std.io.getStdOut();
        try out.writeAll(version);
        try out.writeAll("\n");
        return;
    }

    const is_run = std.mem.eql(u8, cmd, "run");
    const is_test = std.mem.eql(u8, cmd, "test");
    const is_compile = std.mem.eql(u8, cmd, "compile");
    const is_loc = std.mem.eql(u8, cmd, "loc");
    if (!is_run and !is_test and !is_compile and !is_loc) return usage("Unknown command");

    var bough_path: ?[]const u8 = null;
    var out_path: ?[]const u8 = null;
    var loc_key: ?[]const u8 = null;
    var is_dry = false;
    var is_export = false;
    var is_validate = false;
    var is_auto = false;

    if (is_loc) {
        const maybe_sub = args.next();
        if (maybe_sub) |sub| {
            is_export = std.mem.eql(u8, sub, "export");
            is_validate = std.mem.eql(u8, sub, "validate");
        }
        if (!is_export and !is_validate) return usage("loc requires one of 'validate' or 'export' command");
    }

    const maybe_file_path = args.next();
    if (maybe_file_path == null) return usage("File path missing");
    if (maybe_file_path == null) {
        return usage("No file argument provided.");
    }
    const file_path = maybe_file_path.?;

    if (is_compile or is_loc) {
        const maybe_arg = args.next();
        if (maybe_arg == null) return usage("out_path or --dry required");
        if (std.mem.eql(u8, maybe_arg.?, "--dry") or std.mem.eql(u8, maybe_arg.?, "-d")) {
            is_dry = true;
        } else out_path = maybe_arg;
    }

    if (is_run) {
        while (args.next()) |arg| {
            if (std.mem.eql(u8, arg, "--verbose")) continue;
            if (std.mem.eql(u8, arg, "--auto") or std.mem.eql(u8, arg, "-a")) {
                is_auto = true;
                continue;
            }
            if (std.mem.eql(u8, arg, "--lang")) {
                loc_key = args.next();
                continue;
            }
            bough_path = arg;
        }
    }

    var auto_count: usize = 0;
    const allocator = arena.allocator();
    const full_path = std.fs.cwd().realpathAlloc(allocator, file_path) catch |err| {
        try std.io.getStdErr().writer().print("Could not find file at {s}", .{file_path});
        return checkVerbose(err);
    };

    var mod = Module.init(allocator, full_path) catch |err| {
        return checkVerbose(err);
    };
    errdefer mod.deinit();
    mod.use_loc = try checkFlag("--loc");

    if (is_loc) {
        if (is_validate) {
            const validated = Locale.validateFileAtPath(full_path, allocator) catch |err| {
                return checkVerbose(err);
            };
            defer allocator.free(validated);
            if (is_dry) {
                try std.io.getStdOut().writeAll(validated);
                return;
            } else {
                const new_file = try std.fs.createFileAbsolute(full_path, .{});
                defer new_file.close();
                try new_file.writeAll(validated);
                return;
            }
        }
        if (is_export) {
            if (is_dry) {
                Locale.exportFileAtPath(full_path, std.io.getStdOut().writer(), allocator) catch |err| {
                    return checkVerbose(err);
                };
            } else {
                const dir = std.fs.cwd();
                if (std.fs.path.dirname(out_path.?)) |dir_name| {
                    try dir.makePath(dir_name);
                }
                const file = try dir.createFile(out_path.?, .{});
                defer file.close();
                Locale.exportFileAtPath(full_path, file.writer(), allocator) catch |err| {
                    return checkVerbose(err);
                };
            }
        }
        return;
    }

    var bytecode = mod.generateBytecode(allocator) catch |err| {
        return checkVerbose(err);
    };
    mod.deinit();
    defer bytecode.free(allocator);

    if (is_compile) {
        if (is_dry) {
            var out = std.io.getStdOut().writer();
            try out.writeAll("Success\n");
            return;
        }
        const dir = std.fs.cwd();
        if (std.fs.path.dirname(out_path.?)) |dir_name| {
            try dir.makePath(dir_name);
        }
        var file = try dir.createFile(out_path.?, .{});
        defer file.close();
        bytecode.serialize(&file) catch |err| {
            return checkVerbose(err);
        };
        return;
    }

    const vm_alloc = arena.allocator();
    if (is_test) {
        const maybe_count = args.next();
        if (maybe_count == null) return usage("Test requires a play count.");
        auto_count = std.fmt.parseInt(u64, maybe_count.?, 10) catch {
            return usage("Invalid test count specified");
        };

        var i: usize = 0;
        const is_quiet = try checkFlag("--quiet");
        var visit_counts = std.StringArrayHashMap(u64).init(vm_alloc);
        defer visit_counts.deinit();
        while (i < auto_count) : (i += 1) {
            var auto_runner = AutoTestRunner.init();
            var vm = try Vm.init(vm_alloc, bytecode, &auto_runner.runner);
            vm.interpret() catch {
                vm.err.print(std.io.getStdErr().writer());
                return;
            };
            defer vm.deinit();
            if (is_quiet) continue;
            for (vm.globals, 0..) |g, idx| {
                if (g == .visit) {
                    const name = bytecode.global_symbols[idx].name;
                    const cur = try visit_counts.getOrPutValue(name, 0);
                    try visit_counts.put(name, cur.value_ptr.* + g.visit);
                } else break;
                // all visits are first so we can break
            }
        }
        const out = std.io.getStdOut().writer();
        if (is_quiet) {
            try out.writeAll("Success\n");
        }
        var it = visit_counts.iterator();
        while (it.next()) |entry| {
            try out.print("{s} = {}\n", .{ entry.key_ptr.*, entry.value_ptr.* });
        }
        return;
    }

    if (is_run) {
        var cli_runner = CliRunner.init(is_auto);
        var vm = Vm.init(vm_alloc, bytecode, &cli_runner.runner) catch |err| {
            try std.io.getStdErr().writeAll("Could not initialize Vm");
            return checkVerbose(err);
        };
        vm.loc_key = loc_key;

        try vm.start(bough_path orelse vm.bytecode.boughs[0].name);
        try vm.setLocale(loc_key);
        while (vm.can_continue) {
            vm.run() catch {
                vm.err.print(std.io.getStdErr().writer());
                break;
            };
        }
    }
}

fn getFilePath(allocator: std.mem.Allocator) !?[]const u8 {
    var args = try std.process.argsWithAllocator(allocator);
    _ = args.skip();
    return args.next();
}
