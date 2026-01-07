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
    try print("topi - command line topiary processor\n", .{});
    try print("Usage:\n", .{});
    try print("   topi version                  Print version\n", .{});
    try print("   topi run <file>               Run dialogue in terminal\n", .{});
    try print("       -a, --auto                Automatically continue to the next line\n", .{});
    try print("       -b, --bough <name>        Starting bough\n", .{});
    try print("       -k, --language-key <key>  Localization language key\n", .{});
    try print("       -l, --load <file>         Read save from file on start\n", .{});
    try print("       -s, --save <file>         Write save to file on end\n", .{});
    try print("       -v, --verbose\n", .{});
    try print("   topi test <file> <count>      Run dialogue <count> times, selecting random choices\n", .{});
    try print("       -q, --quiet               Do not output visit tree on end\n", .{});
    try print("       -v, --verbose\n", .{});
    try print("   topi compile <file>           Compile dialogue to bytecode\n", .{});
    try print("       -d, --dry                 Do not write to file on end\n", .{});
    try print("       -o, --output <file>       Write to file on end\n", .{});
    try print("       -l, --localize            Include localization in compiled bytecode\n", .{});
    try print("       -v, --verbose\n", .{});
    try print("   topi loc validate <file>      Validate dialogue localization ids\n", .{});
    try print("       -d, --dry                 Do not write to file on end\n", .{});
    try print("       -v, --verbose\n", .{});
    try print("   topi loc export <file>        Export dialogue localization to csv\n", .{});
    try print("       -d, --dry                 Do not write to file on end\n", .{});
    try print("       -o, --output <file>       Write to file on end\n", .{});
    try print("       -v, --verbose\n", .{});
    try print("\n", .{});
    if (!std.mem.eql(u8, msg, "")) {
        try print("Error: ", .{});
        try print(msg, .{});
        try print("\n", .{});
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

pub fn print(comptime msg: []const u8, args: anytype) !void {
    var buffer: [128]u8 = undefined;
    var writer = std.fs.File.stdout().writer(&buffer);
    const stdout = &writer.interface;
    stdout.print(msg, args) catch {
        std.debug.print("Could not print message", .{});
    };
    try stdout.flush();
}

pub fn main() !void {
    var da: std.heap.DebugAllocator(.{}) = .init;
    var arena = std.heap.ArenaAllocator.init(da.allocator());
    defer arena.deinit();
    var iter = try std.process.argsWithAllocator(arena.allocator());
    _ = iter.skip();

    const maybe_cmd = iter.next();
    if (maybe_cmd == null) return usage("");
    const cmd = std.meta.stringToEnum(Command, maybe_cmd.?);
    if (cmd == null) return usage("Missing command");

    switch (cmd.?) {
        .version => {
            try print("{s}\n", .{version});
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
        try print("Could not find file at {s}\n", .{args.file.?});
        return err;
    };
    return Module.init(alloc, full_path);
}

fn runCommand(args: RunArgs, alloc: std.mem.Allocator) !void {
    var mod = createModule(alloc, args) catch |err| {
        try print("Could not create module\n", .{});
        return if (args.verbose) err else {};
    };
    errdefer mod.deinit();

    var bytecode = mod.generateBytecode(alloc) catch |err| {
        try print("Could not generate bytecode\n", .{});
        return if (args.verbose) err else {};
    };
    mod.deinit();
    defer bytecode.free(alloc);

    var cli_runner = CliRunner.init(args.auto);
    var vm = Vm.init(alloc, bytecode, &cli_runner.runner) catch |err| {
        try print("Could not initialize Vm\n", .{});
        return if (args.verbose) err else {};
    };

    if (args.load) |file_path| {
        const file = try if (std.fs.path.isAbsolute(file_path)) std.fs.openFileAbsolute(file_path, .{}) else std.fs.cwd().openFile(file_path, .{});
        var buf: [128]u8 = undefined;
        var reader = file.reader(&buf);
        const read = &reader.interface;
        defer file.close();
        State.deserialize(&vm, read) catch |err| {
            try print("Could not load state\n", .{});
            return if (args.verbose) err else {};
        };
    }

    try vm.start(args.bough);
    try vm.setLocale(args.language);
    while (vm.can_continue) {
        vm.run() catch {
            var buffer: [128]u8 = undefined;
            var writer = std.fs.File.stdout().writer(&buffer);
            const stdout = &writer.interface;
            vm.err.print(stdout);
            break;
        };
    }

    if (args.save) |file_path| {
        const dir = std.fs.cwd();
        if (std.fs.path.dirname(file_path)) |dir_name| {
            try dir.makePath(dir_name);
        }
        var file = try dir.createFile(file_path, .{});
        var buf : [128]u8 = undefined;
        var file_writer = file.writer(&buf);
        const writer = &file_writer.interface;
        defer file.close();
        State.serialize(&vm, writer) catch |err| {
            try print("Could not save state\n", .{});
            return if (args.verbose) err else {};
        };
    }
}

fn compileCommand(args: CompileArgs, alloc: std.mem.Allocator) !void {
    var mod = createModule(alloc, args) catch |err| {
        try print("Could not create module", .{});
        return if (args.verbose) err else {};
    };
    errdefer mod.deinit();
    mod.use_loc = args.loc;
    var bytecode = mod.generateBytecode(alloc) catch |err| {
        try print("Could not generate bytecode", .{});
        return if (args.verbose) err else {};
    };
    mod.deinit();
    defer bytecode.free(alloc);

    if (args.dry) {
        try print("Success\n", .{});
        return;
    }
    const dir = std.fs.cwd();
    if (std.fs.path.dirname(args.output.?)) |dir_name| {
        try dir.makePath(dir_name);
    }
    var file = try dir.createFile(args.output.?, .{});
    defer file.close();
    var buf: [128]u8 = undefined;
    var file_writer = file.writer(&buf);
    _ = bytecode.serialize(alloc, &file_writer.interface) catch |err| {
        try print("Could not serialize bytecode\n", .{});
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
            var buffer: [128]u8 = undefined;
            var writer = std.fs.File.stdout().writer(&buffer);
            const stdout = &writer.interface;
            vm.err.print(stdout);
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
    if (args.quiet) {
        try print("Success\n", .{});
    }
    var it = visit_counts.iterator();
    while (it.next()) |entry| {
        try print("{s} = {}\n", .{ entry.key_ptr.*, entry.value_ptr.* });
    }
}

fn localizeCommand(args: LocalizeArgs, alloc: std.mem.Allocator) !void {
    const full_path = std.fs.cwd().realpathAlloc(alloc, args.file.?) catch |err| {
        try print("Could not find file at {s}", .{args.file.?});
        return if (args.verbose) err else {};
    };
    switch (args.command) {
        .@"export" => {
            if (args.dry) {
                var buffer: [128]u8 = undefined;
                var writer = std.fs.File.stdout().writer(&buffer);
                const stdout = &writer.interface;
                Locale.exportFileAtPath(full_path, stdout, alloc) catch |err| {
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

                var buffer: [128]u8 = undefined;
                var file_writer = file.writer(&buffer);
                const writer = &file_writer.interface;
                Locale.exportFileAtPath(full_path, writer, alloc) catch |err| {
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
                try print("{s}\n", .{validated});
            } else {
                const new_file = try std.fs.createFileAbsolute(full_path, .{});
                defer new_file.close();
                try new_file.writeAll(validated);
            }
        },
    }
}
