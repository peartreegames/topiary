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

const Command = enum {
    run,
    compile,
    localize,
    @"test",
    version,
};

fn usage(comptime msg: []const u8) !void {
    var out = std.io.getStdErr().writer();
    try out.print("topi - command line topiary processor\n", .{});
    try out.print("Usage:\n", .{});
    try out.print("   topi version                  Print version\n", .{});
    try out.print("   topi run <file>               Run dialogue in terminal\n", .{});
    try out.print("       -a, --auto                Automatically continue to the next line\n", .{});
    try out.print("       -b, --bough <name>        Starting bough\n", .{});
    try out.print("       -k, --language-key <key>  Localization language key\n", .{});
    try out.print("       -l, --load <file>         Read save from file on start\n", .{});
    try out.print("       -s, --save <file>         Write save to file on end\n", .{});
    try out.print("       -v, --verbose\n", .{});
    try out.print("   topi test <file> <count>      Run dialogue <count> times, selecting random choices\n", .{});
    try out.print("       -q, --quiet               Do not output visit tree on end\n", .{});
    try out.print("       -v, --verbose\n", .{});
    try out.print("   topi compile <file>           Compile dialogue to bytecode\n", .{});
    try out.print("       -d, --dry                 Do not write to file on end\n", .{});
    try out.print("       -o, --output <file>       Write to file on end\n", .{});
    try out.print("       -l, --localize            Include localization in compiled bytecode\n", .{});
    try out.print("       -v, --verbose\n", .{});
    try out.print("   topi loc validate <file>      Validate dialogue localization ids\n", .{});
    try out.print("       -v, --verbose\n", .{});
    try out.print("   topi loc export <file>        Export dialogue localization to csv\n", .{});
    try out.print("       -d, --dry                 Do not output to file on end\n", .{});
    try out.print("       -o, --output <file>       Write to file on end\n", .{});
    try out.print("       -v, --verbose\n", .{});
    try out.print("\n", .{});
    if (!std.mem.eql(u8, msg, "")) {
        try out.print("Error: ", .{});
        try out.print(msg, .{});
        try out.print("\n", .{});
    }
    return error.InvalidArguments;
}

const RunArgs = struct {
    auto: bool = false,
    file: ?[]const u8 = null,
    bough: ?[]const u8 = null,
    language: ?[]const u8 = null,
    save: ?[]const u8 = null,
    load: ?[]const u8 = null,
    verbose: bool = false,

    fn init(self: *RunArgs, iter: *std.process.ArgIterator) !void {
        while (iter.next()) |arg| {
            if (std.mem.eql(u8, arg, "-a") or std.mem.eql(u8, arg, "--auto")) {
                self.auto = true;
            } else if (std.mem.eql(u8, arg, "-b") or std.mem.eql(u8, arg, "--bough")) {
                self.bough = iter.next();
            } else if (std.mem.eql(u8, arg, "-s") or std.mem.eql(u8, arg, "--save")) {
                self.save = iter.next();
            } else if (std.mem.eql(u8, arg, "-l") or std.mem.eql(u8, arg, "--load")) {
                self.load = iter.next();
            } else if (std.mem.eql(u8, arg, "-k") or std.mem.eql(u8, arg, "--language-key")) {
                self.language = iter.next();
            } else if (std.mem.eql(u8, arg, "-v") or std.mem.eql(u8, arg, "--verbose")) {
                self.verbose = true;
            } else self.file = arg;
        }
        if (self.file == null) return usage("Missing file");
    }
};

const CompileArgs = struct {
    file: ?[]const u8 = null,
    output: ?[]const u8 = null,
    dry: bool = false,
    loc: bool = false,
    verbose: bool = false,

    fn init(self: *CompileArgs, iter: *std.process.ArgIterator) !void {
        while (iter.next()) |arg| {
            if (std.mem.eql(u8, arg, "-o") or std.mem.eql(u8, arg, "--output")) {
                self.output = iter.next();
            } else if (std.mem.eql(u8, arg, "-d") or std.mem.eql(u8, arg, "--dry")) {
                self.dry = true;
            } else if (std.mem.eql(u8, arg, "-l") or std.mem.eql(u8, arg, "--localize")) {
                self.loc = true;
            } else if (std.mem.eql(u8, arg, "-v") or std.mem.eql(u8, arg, "--verbose")) {
                self.verbose = true;
            } else self.file = arg;
        }
        if (self.file == null) return usage("Missing file");
    }
};

const LocalizeArgs = struct {
    file: ?[]const u8 = null,
    command: LocCommand = .@"export",
    output: ?[]const u8 = null,
    dry: bool = false,
    verbose: bool = false,
    const LocCommand = enum {
        @"export",
        validate,
    };
    fn init(self: *LocalizeArgs, iter: *std.process.ArgIterator) !void {
        const cmd = std.meta.stringToEnum(LocCommand, iter.next().?);
        if (cmd == null) return usage("Missing export or validate command");
        self.command = cmd.?;
        while (iter.next()) |arg| {
            if (std.mem.eql(u8, arg, "-o") or std.mem.eql(u8, arg, "--output")) {
                self.output = iter.next();
            } else if (std.mem.eql(u8, arg, "-d") or std.mem.eql(u8, arg, "--dry")) {
                self.dry = true;
            } else if (std.mem.eql(u8, arg, "-v") or std.mem.eql(u8, arg, "--verbose")) {
                self.verbose = true;
            } else self.file = arg;
        }
        if (self.file == null) return usage("Missing file");
    }
};

const TestArgs = struct {
    file: ?[]const u8 = null,
    count: usize = 0,
    quiet: bool = false,
    verbose: bool = false,
    fn init(self: *TestArgs, iter: *std.process.ArgIterator) !void {
        self.count = try std.fmt.parseInt(usize, iter.next().?, 10);
        while (iter.next()) |arg| {
            if (std.mem.eql(u8, arg, "-q") or std.mem.eql(u8, arg, "--quiet")) {
                self.quiet = true;
            } else if (std.mem.eql(u8, arg, "-v") or std.mem.eql(u8, arg, "--verbose")) {
                self.verbose = true;
            } else self.file = arg;
        }
        if (self.file == null) return usage("Missing file");
    }
};

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    var iter = try std.process.argsWithAllocator(arena.allocator());
    _ = iter.skip();

    const maybe_cmd = iter.next();
    if (maybe_cmd == null) return usage("");
    const cmd = std.meta.stringToEnum(Command, maybe_cmd.?);
    if (cmd == null) return usage("Missing command");

    switch (cmd.?) {
        .version => {
            const out = std.io.getStdOut();
            try out.writeAll(version);
            try out.writeAll("\n");
        },
        .run => {
            var args = RunArgs{};
            args.init(&iter) catch |err| {
                return if (args.verbose) err else {};
            };
            try runCommand(args, arena.allocator());
        },
        .compile => {
            var args = CompileArgs{};
            args.init(&iter) catch |err| {
                return if (args.verbose) err else {};
            };
            try compileCommand(args, arena.allocator());
        },
        .@"test" => {
            var args = TestArgs{};
            args.init(&iter) catch |err| {
                return if (args.verbose) err else {};
            };
            try testCommand(args, arena.allocator());
        },
        .localize => {
            var args = LocalizeArgs{};
            args.init(&iter) catch |err| {
                return if (args.verbose) err else {};
            };
            try localizeCommand(args, arena.allocator());
        },
    }
}

fn createModule(alloc: std.mem.Allocator, args: anytype) !*Module {
    const full_path = std.fs.cwd().realpathAlloc(alloc, args.file.?) catch |err| {
        try std.io.getStdErr().writer().print("Could not find file at {s}", .{args.file.?});
        return err;
    };
    return Module.init(alloc, full_path);
}

fn runCommand(args: RunArgs, alloc: std.mem.Allocator) !void {
    var mod = createModule(alloc, args) catch |err| {
        try std.io.getStdErr().writeAll("Could not create module");
        return if (args.verbose) err else {};
    };
    errdefer mod.deinit();

    var bytecode = mod.generateBytecode(alloc) catch |err| {
        try std.io.getStdErr().writeAll("Could not generate bytecode");
        return if (args.verbose) err else {};
    };
    mod.deinit();
    defer bytecode.free(alloc);

    var cli_runner = CliRunner.init(args.auto);
    var vm = Vm.init(alloc, bytecode, &cli_runner.runner) catch |err| {
        try std.io.getStdErr().writeAll("Could not initialize Vm");
        return if (args.verbose) err else {};
    };

    if (args.load) |file_path| {
        const file = try if (std.fs.path.isAbsolute(file_path)) std.fs.openFileAbsolute(file_path, .{}) else std.fs.cwd().openFile(file_path, .{});
        defer file.close();
        State.deserialize(&vm, file.reader()) catch |err| {
            try std.io.getStdErr().writeAll("Could not load state\n");
            return if (args.verbose) err else {};
        };
    }

    try vm.start(args.bough orelse vm.bytecode.boughs[0].name);
    try vm.setLocale(args.language);
    while (vm.can_continue) {
        vm.run() catch {
            vm.err.print(std.io.getStdErr().writer());
            break;
        };
    }

    if (args.save) |file_path| {
        const dir = std.fs.cwd();
        if (std.fs.path.dirname(file_path)) |dir_name| {
            try dir.makePath(dir_name);
        }
        var file = try dir.createFile(file_path, .{});
        defer file.close();
        State.serialize(&vm, file.writer()) catch |err| {
            try std.io.getStdErr().writeAll("Could not save state\n");
            return if (args.verbose) err else {};
        };
    }
}

fn compileCommand(args: CompileArgs, alloc: std.mem.Allocator) !void {
    var mod = createModule(alloc, args) catch |err| {
        try std.io.getStdErr().writeAll("Could not create module");
        return if (args.verbose) err else {};
    };
    errdefer mod.deinit();
    mod.use_loc = args.loc;
    var bytecode = mod.generateBytecode(alloc) catch |err| {
        try std.io.getStdErr().writeAll("Could not generate bytecode");
        return if (args.verbose) err else {};
    };
    mod.deinit();
    defer bytecode.free(alloc);

    if (args.dry) {
        var out = std.io.getStdOut().writer();
        try out.writeAll("Success\n");
        return;
    }
    const dir = std.fs.cwd();
    if (std.fs.path.dirname(args.output.?)) |dir_name| {
        try dir.makePath(dir_name);
    }
    var file = try dir.createFile(args.output.?, .{});
    defer file.close();
    bytecode.serialize(&file) catch |err| {
        try std.io.getStdErr().writeAll("Could not serialize bytecode");
        return if (args.verbose) err else {};
    };
}

fn testCommand(args: TestArgs, alloc: std.mem.Allocator) !void {
    var mod = createModule(alloc, args) catch |err| {
        return if (args.verbose) err else {};
    };
    errdefer mod.deinit();

    var bytecode = mod.generateBytecode(alloc) catch |err| {
        return if (args.verbose) err else {};
    };
    mod.deinit();
    defer bytecode.free(alloc);

    var visit_counts = std.StringArrayHashMap(u64).init(alloc);
    defer visit_counts.deinit();
    var i: usize = 0;
    while (i < args.count) : (i += 1) {
        var auto_runner = AutoTestRunner.init();
        var vm = try Vm.init(alloc, bytecode, &auto_runner.runner);
        defer vm.deinit();
        vm.interpret() catch {
            vm.err.print(std.io.getStdErr().writer());
            return;
        };
        if (args.quiet) continue;
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
    if (args.quiet) {
        try out.writeAll("Success\n");
    }
    var it = visit_counts.iterator();
    while (it.next()) |entry| {
        try out.print("{s} = {}\n", .{ entry.key_ptr.*, entry.value_ptr.* });
    }
}

fn localizeCommand(args: LocalizeArgs, alloc: std.mem.Allocator) !void {
    const full_path = std.fs.cwd().realpathAlloc(alloc, args.file.?) catch |err| {
        try std.io.getStdErr().writer().print("Could not find file at {s}", .{args.file.?});
        return if (args.verbose) err else {};
    };
    switch (args.command) {
        .@"export" => {
            if (args.dry) {
                Locale.exportFileAtPath(full_path, std.io.getStdOut().writer(), alloc) catch |err| {
                    if (args.verbose) return err;
                    return;
                };
            } else {
                const dir = std.fs.cwd();
                if (std.fs.path.dirname(args.output.?)) |dir_name| {
                    try dir.makePath(dir_name);
                }
                const file = try dir.createFile(args.output.?, .{});
                defer file.close();
                Locale.exportFileAtPath(full_path, file.writer(), alloc) catch |err| {
                    if (args.verbose) return err;
                    return;
                };
            }
        },
        .validate => {
            const validated = Locale.validateFileAtPath(full_path, alloc) catch |err| {
                if (args.verbose) return err;
                return;
            };
            defer alloc.free(validated);
            if (args.dry) {
                try std.io.getStdOut().writeAll(validated);
            } else {
                const new_file = try std.fs.createFileAbsolute(full_path, .{});
                defer new_file.close();
                try new_file.writeAll(validated);
            }
        },
    }
}
