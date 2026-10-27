const std = @import("std");

const topi = @import("topi");
const Vm = topi.runtime.Vm;
const Runner = topi.runtime.Runner;
const State = topi.runtime.State;

const Value = topi.types.Value;

const Locale = topi.locale.Locale;
const Stamp = topi.stamp.Stamp;
const Formatter = topi.frontend.Formatter;

const version = @import("build").version;

const File = topi.module.File;
const Module = topi.module.Module;

const runner = @import("runner.zig");
const CliRunner = runner.CliRunner;
const AutoTestRunner = runner.AutoTestRunner;
const emitJson = runner.emitJson;

pub const io = std.Io.Threaded.global_single_threaded.io();

const Command = enum {
    run,
    compile,
    fmt,
    loc,
    @"test",
    version,
};

fn usage(comptime msg: []const u8) !void {
    try print("topi - command line topiary processor\n", .{});
    try print("Usage:\n", .{});
    try print("   topi version                  Print version\n", .{});
    try print("   topi run <file>               Run dialogue in terminal\n", .{});
    try print("       -a, --auto                    Automatically continue to the next line\n", .{});
    try print("       -b, --bough <name>            Starting bough\n", .{});
    try print("       -c, --choices <i,j,...>       Take these choice indices in order, then prompt\n", .{});
    try print("       -j, --json                    JSON Lines transcript; stops at the first unscripted choice\n", .{});
    try print("       -k, --locale-key-file <file>  Localization key file\n", .{});
    try print("       -l, --load <file>             Read save from file on start\n", .{});
    try print("       -r, --rewind                  Enable rewind/redo at choice prompts (debug)\n", .{});
    try print("       -s, --save <file>             Write save to file on end\n", .{});
    try print("           --seed <n>                Seed the random builtins\n", .{});
    try print("       -v, --verbose\n", .{});
    try print("   topi test <file> <count>      Run dialogue <count> times, selecting random choices\n", .{});
    try print("       -b, --bough <name>            Starting bough\n", .{});
    try print("       -j, --json                    JSON visit counts and never-visited names\n", .{});
    try print("       -q, --quiet                   Do not output visit tree on end\n", .{});
    try print("           --seed <n>                Seed choice selection and the random builtins\n", .{});
    try print("       -v, --verbose\n", .{});
    try print("   topi fmt <file>               Format source file\n", .{});
    try print("       -d, --dry                     Print to stdout instead of writing\n", .{});
    try print("       -i, --indent <n>              Spaces per indent level (default: 4)\n", .{});
    try print("       --stamp                       Also insert @UUID tokens for unstamped nodes\n", .{});
    try print("       --stamp-only                  Insert @UUID tokens without reformatting\n", .{});
    try print("       -v, --verbose\n", .{});
    try print("   topi compile <file>           Compile dialogue to bytecode\n", .{});
    try print("       -d, --dry                     Do not write to file on end\n", .{});
    try print("       -j, --json                    JSON Lines diagnostics\n", .{});
    try print("       -o, --output <file>           Write to file on end\n", .{});
    try print("       -t, --time                   Print compile phase timings\n", .{});
    try print("       -v, --verbose\n", .{});
    try print("   topi loc validate <file>      Check for missing localization ids\n", .{});
    try print("       -d, --dry                     Do not write to file on end\n", .{});
    try print("       -v, --verbose\n", .{});
    try print("   topi loc export <file>        Export dialogue localization to csv\n", .{});
    try print("       -d, --dry                     Do not write to file on end\n", .{});
    try print("       -l, --lang <lang>             Base language column name (default: en)\n", .{});
    try print("           --no-merge                Overwrite existing csv instead of merging\n", .{});
    try print("       -o, --output <file>           Write to file on end\n", .{});
    try print("       -v, --verbose\n", .{});
    try print("   topi loc generate <file>      Export dialogue csv to topil files\n", .{});
    try print("       -d, --dry                     Do not write to file on end\n", .{});
    try print("       -f, --folder <folder>         Folder to output files\n", .{});
    try print("       -k, --locale-key              Generate only a specific localization key\n", .{});
    try print("       -v, --verbose\n", .{});
    try print("\n", .{});
    if (!std.mem.eql(u8, msg, "")) {
        try print("Error: ", .{});
        try print(msg, .{});
        try print("\n", .{});
    }
}

const RunArgs = struct {
    auto: bool = false,
    file: ?[]const u8 = null,
    bough: ?[]const u8 = null,
    locale_key_file: ?[]const u8 = null,
    save: ?[]const u8 = null,
    load: ?[]const u8 = null,
    verbose: bool = false,
    rewind: bool = false,
    choices: ?[]const u8 = null,
    json: bool = false,
    seed: ?u64 = null,

    fn init(self: *RunArgs, iter: *std.process.Args.Iterator) !void {
        while (iter.next()) |arg| {
            if (std.mem.eql(u8, arg, "-a") or std.mem.eql(u8, arg, "--auto")) {
                self.auto = true;
            } else if (std.mem.eql(u8, arg, "-c") or std.mem.eql(u8, arg, "--choices")) {
                self.choices = iter.next();
            } else if (std.mem.eql(u8, arg, "-j") or std.mem.eql(u8, arg, "--json")) {
                self.json = true;
            } else if (std.mem.eql(u8, arg, "--seed")) {
                self.seed = try parseSeed(iter.next());
            } else if (std.mem.eql(u8, arg, "-b") or std.mem.eql(u8, arg, "--bough")) {
                self.bough = iter.next();
            } else if (std.mem.eql(u8, arg, "-s") or std.mem.eql(u8, arg, "--save")) {
                self.save = iter.next();
            } else if (std.mem.eql(u8, arg, "-l") or std.mem.eql(u8, arg, "--load")) {
                self.load = iter.next();
            } else if (std.mem.eql(u8, arg, "-k") or std.mem.eql(u8, arg, "--locale-key-file")) {
                self.locale_key_file = iter.next();
            } else if (std.mem.eql(u8, arg, "-r") or std.mem.eql(u8, arg, "--rewind")) {
                self.rewind = true;
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
    time: bool = false,
    json: bool = false,
    verbose: bool = false,

    fn init(self: *CompileArgs, iter: *std.process.Args.Iterator) !void {
        while (iter.next()) |arg| {
            if (std.mem.eql(u8, arg, "-o") or std.mem.eql(u8, arg, "--output")) {
                self.output = iter.next();
            } else if (std.mem.eql(u8, arg, "-j") or std.mem.eql(u8, arg, "--json")) {
                self.json = true;
            } else if (std.mem.eql(u8, arg, "-d") or std.mem.eql(u8, arg, "--dry")) {
                self.dry = true;
            } else if (std.mem.eql(u8, arg, "-t") or std.mem.eql(u8, arg, "--time")) {
                self.time = true;
            } else if (std.mem.eql(u8, arg, "-v") or std.mem.eql(u8, arg, "--verbose")) {
                self.verbose = true;
            } else self.file = arg;
        }
        if (self.file == null) return usage("Missing file");
    }
};

const LocalizeArgs = struct {
    file: ?[]const u8 = null,
    command: LocCommand = undefined,
    folder: ?[]const u8 = null,
    lang: []const u8 = "en",
    locale_key: ?[]const u8 = null,
    output: ?[]const u8 = null,
    dry: bool = false,
    no_merge: bool = false,
    verbose: bool = false,
    const LocCommand = enum {
        @"export",
        validate,
        generate,
    };
    fn init(self: *LocalizeArgs, iter: *std.process.Args.Iterator) !void {
        const cmd = std.meta.stringToEnum(LocCommand, iter.next().?);
        if (cmd == null) return usage("Missing 'export', 'validate', or 'generate' command");
        self.command = cmd.?;
        while (iter.next()) |arg| {
            if (std.mem.eql(u8, arg, "-o") or std.mem.eql(u8, arg, "--output")) {
                self.output = iter.next();
            } else if (std.mem.eql(u8, arg, "-f") or std.mem.eql(u8, arg, "--folder")) {
                self.folder = iter.next();
            } else if (std.mem.eql(u8, arg, "-l") or std.mem.eql(u8, arg, "--lang")) {
                self.lang = iter.next() orelse "en";
            } else if (std.mem.eql(u8, arg, "-k") or std.mem.eql(u8, arg, "--locale-key")) {
                self.locale_key = iter.next();
            } else if (std.mem.eql(u8, arg, "-d") or std.mem.eql(u8, arg, "--dry")) {
                self.dry = true;
            } else if (std.mem.eql(u8, arg, "--no-merge")) {
                self.no_merge = true;
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
    bough: ?[]const u8 = null,
    quiet: bool = false,
    json: bool = false,
    seed: ?u64 = null,
    verbose: bool = false,
    fn init(self: *TestArgs, iter: *std.process.Args.Iterator) !void {
        while (iter.next()) |arg| {
            if (std.mem.eql(u8, arg, "-q") or std.mem.eql(u8, arg, "--quiet")) {
                self.quiet = true;
            } else if (std.mem.eql(u8, arg, "-j") or std.mem.eql(u8, arg, "--json")) {
                self.json = true;
            } else if (std.mem.eql(u8, arg, "--seed")) {
                self.seed = try parseSeed(iter.next());
            } else if (std.mem.eql(u8, arg, "-b") or std.mem.eql(u8, arg, "--bough")) {
                self.bough = iter.next();
            } else if (std.mem.eql(u8, arg, "-v") or std.mem.eql(u8, arg, "--verbose")) {
                self.verbose = true;
            } else if (std.fmt.parseInt(usize, arg, 10) catch null) |count| {
                self.count = count;
            } else self.file = arg;
        }
        if (self.file == null) return usage("Missing file");
    }
};

const FmtArgs = struct {
    file: ?[]const u8 = null,
    indent: usize = 4,
    dry: bool = false,
    stamp: bool = false,
    stamp_only: bool = false,
    verbose: bool = false,
    fn init(self: *FmtArgs, iter: *std.process.Args.Iterator) !void {
        while (iter.next()) |arg| {
            if (std.mem.eql(u8, arg, "-d") or std.mem.eql(u8, arg, "--dry")) {
                self.dry = true;
            } else if (std.mem.eql(u8, arg, "-i") or std.mem.eql(u8, arg, "--indent")) {
                if (iter.next()) |n| {
                    self.indent = std.fmt.parseInt(usize, n, 10) catch 4;
                }
            } else if (std.mem.eql(u8, arg, "--stamp")) {
                self.stamp = true;
            } else if (std.mem.eql(u8, arg, "--stamp-only")) {
                self.stamp_only = true;
            } else if (std.mem.eql(u8, arg, "-v") or std.mem.eql(u8, arg, "--verbose")) {
                self.verbose = true;
            } else self.file = arg;
        }
        if (self.file == null) return usage("Missing file");
    }
};

fn parseSeed(arg: ?[]const u8) !u64 {
    const text = arg orelse {
        try usage("Missing --seed value");
        return error.InvalidArgs;
    };
    return std.fmt.parseInt(u64, text, 10) catch {
        try usage("--seed must be a non-negative integer");
        return error.InvalidArgs;
    };
}

fn parseChoices(alloc: std.mem.Allocator, list: ?[]const u8) ![]const usize {
    const text = list orelse return &.{};
    var indices: std.ArrayList(usize) = .empty;
    var it = std.mem.tokenizeAny(u8, text, ", ");
    while (it.next()) |part| try indices.append(alloc, try std.fmt.parseInt(usize, part, 10));
    return indices.items;
}

fn seedBuiltins(seed: ?u64) void {
    if (seed) |s| topi.runtime.builtins.restoreRng(std.Random.DefaultPrng.init(s));
}

fn emitDiagnostics(mod: *Module) void {
    for (mod.errors.list.items) |e| emitJson(.{
        .type = "diagnostic",
        .severity = e.severity,
        .file = e.file_path,
        .line = e.token.line,
        .column = e.token.column,
        .message = e.fmt,
        .note = e.note,
        .suggestion = e.suggestion,
    });
}

fn emitRunEnd(vm: *Vm, alloc: std.mem.Allocator, status: []const u8, run_err: ?anyerror) !void {
    var buffer: [256]u8 = undefined;
    var writer = std.Io.File.stdout().writerStreaming(io, &buffer);
    var s: std.json.Stringify = .{ .writer = &writer.interface };
    try s.beginObject();
    try s.objectField("type");
    try s.write("end");
    try s.objectField("status");
    try s.write(status);
    if (run_err) |e| {
        try s.objectField("error");
        try s.write(.{
            .name = @errorName(e),
            .message = vm.err.msg,
            .trace = vm.err.trace.items,
            .suggestion = vm.err.suggestion,
            .note = vm.err.note,
        });
    }
    try s.objectField("visits");
    try s.beginObject();
    for (vm.bytecode.global_symbols) |sym| {
        const value = vm.globals[sym.index];
        if (value != .visit or value.visit == 0) continue;
        try s.objectField(sym.name);
        try s.write(value.visit);
    }
    try s.endObject();
    try s.objectField("globals");
    try s.beginObject();
    for (vm.bytecode.global_symbols) |sym| {
        const value = vm.globals[sym.index];
        if (!sym.is_mutable or value == .visit or value == .void) continue;
        try s.objectField(sym.name);
        try writeValueJson(&s, value, alloc);
    }
    try s.endObject();
    try s.endObject();
    try writer.interface.writeByte('\n');
    try writer.interface.flush();
}

fn writeValueJson(s: *std.json.Stringify, value: Value, alloc: std.mem.Allocator) !void {
    switch (value) {
        .bool => |b| return s.write(b),
        .nil => return s.write(null),
        .number => |n| if (std.math.isFinite(n)) return s.print("{d}", .{n}),
        else => if (value.asString()) |str| return s.write(str),
    }
    var text: std.Io.Writer.Allocating = .init(alloc);
    defer text.deinit();
    var seen: std.array_hash_map.Auto(topi.utils.UUID.ID, void) = .empty;
    defer seen.deinit(alloc);
    value.print(&text.writer, alloc, &seen) catch return s.write("?");
    try s.write(text.written());
}

pub fn print(comptime msg: []const u8, args: anytype) !void {
    var buffer: [128]u8 = undefined;
    var writer = std.Io.File.stdout().writerStreaming(io, &buffer);
    const stdout = &writer.interface;
    stdout.print(msg, args) catch {
        std.debug.print("Could not print message", .{});
    };
    try stdout.flush();
}

pub fn writeErrors(mod: *Module) !void {
    var buffer: [128]u8 = undefined;
    var writer = std.Io.File.stdout().writerStreaming(io, &buffer);
    const stdout = &writer.interface;
    try mod.writeErrors(stdout, true);
}

fn writeOutput(full_path: []const u8, content: []const u8, dry: bool) !void {
    if (dry) {
        var buffer: [128]u8 = undefined;
        var writer = std.Io.File.stdout().writerStreaming(io, &buffer);
        const stdout = &writer.interface;
        try stdout.writeAll(content);
        try stdout.flush();
    } else {
        const new_file = try std.Io.Dir.createFileAbsolute(io, full_path, .{});
        defer new_file.close(io);
        try new_file.writeStreamingAll(io, content);
    }
}

pub fn main(init: std.process.Init.Minimal) !void {
    const alloc = std.heap.smp_allocator;
    var arena = std.heap.ArenaAllocator.init(alloc);
    defer arena.deinit();
    var iter = try std.process.Args.Iterator.initAllocator(init.args, arena.allocator());
    defer iter.deinit();
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
        .fmt => {
            var args = FmtArgs{};
            args.init(&iter) catch |err| {
                return if (args.verbose) err else {};
            };
            try fmtCommand(args, arena.allocator());
        },
        .@"test" => {
            var args = TestArgs{};
            args.init(&iter) catch |err| {
                return if (args.verbose) err else {};
            };
            try testCommand(args, arena.allocator());
        },
        .loc => {
            var args = LocalizeArgs{};
            args.init(&iter) catch |err| {
                return if (args.verbose) err else {};
            };
            try localizeCommand(args, arena.allocator());
        },
    }
}

fn createModule(alloc: std.mem.Allocator, args: anytype) !*Module {
    const full_path = std.Io.Dir.cwd().realPathFileAlloc(io, args.file.?, alloc) catch |err| {
        try print("Could not find file at {s}\n", .{args.file.?});
        return err;
    };
    return Module.init(alloc, io, full_path);
}

fn runCommand(args: RunArgs, alloc: std.mem.Allocator) !void {
    const scripted = parseChoices(alloc, args.choices) catch return usage("--choices expects indices like 1,0,2");
    var mod = createModule(alloc, args) catch |err| {
        try print("Could not create module: {s}\n", .{@errorName(err)});
        return if (args.verbose) err else {};
    };
    defer mod.deinit();

    var bytecode = mod.generateBytecode(alloc) catch |err| {
        if (args.json) {
            emitDiagnostics(mod);
            emitJson(.{ .type = "end", .status = "compile_error" });
            return if (args.verbose) err else {};
        }
        try print("Could not create bytecode: {s}\n", .{@errorName(err)});
        try writeErrors(mod);
        return if (args.verbose) err else {};
    };
    if (args.json) emitDiagnostics(mod) else try writeErrors(mod);
    defer bytecode.free(alloc);
    seedBuiltins(args.seed);

    var cli_runner = CliRunner.init(args.auto, args.json, scripted);
    var vm = Vm.init(alloc, io, &bytecode, &cli_runner.runner) catch |err| {
        try print("Could not start the dialogue runtime\n", .{});
        return if (args.verbose) err else {};
    };
    if (args.rewind) vm.history_capacity = 16;

    if (args.load) |file_path| {
        const file = try if (std.fs.path.isAbsolute(file_path)) std.Io.Dir.openFileAbsolute(io, file_path, .{}) else std.Io.Dir.cwd().openFile(io, file_path, .{});
        var buf: [128]u8 = undefined;
        var reader = file.reader(io, &buf);
        const read = &reader.interface;
        defer file.close(io);
        State.deserialize(&vm, read) catch |err| {
            try print("Could not load state\n", .{});
            return if (args.verbose) err else {};
        };
    }

    vm.start(args.bough) catch |err| {
        if (args.json) {
            try emitRunEnd(&vm, alloc, "error", err);
            return if (args.verbose) err else {};
        }
        var buffer: [128]u8 = undefined;
        var writer = std.Io.File.stdout().writerStreaming(io, &buffer);
        const stdout = &writer.interface;
        vm.err.print(stdout);
        return if (args.verbose) err else {};
    };
    if (args.locale_key_file) |file| {
        try vm.setLocale(file);
    }
    var run_err: ?anyerror = null;
    while (vm.can_continue) {
        vm.run() catch |err| {
            run_err = err;
            if (args.json) break;
            var buffer: [128]u8 = undefined;
            var writer = std.Io.File.stdout().writerStreaming(io, &buffer);
            const stdout = &writer.interface;
            vm.err.print(stdout);
            break;
        };
    }
    if (args.json) {
        const status = if (run_err != null) "error" else if (cli_runner.stop_reason) |r| @tagName(r) else "finished";
        try emitRunEnd(&vm, alloc, status, run_err);
    } else if (cli_runner.stop_reason == .invalid_choice) {
        try print("Invalid scripted choice {d}\n", .{scripted[cli_runner.next_scripted - 1]});
    }

    if (args.save) |file_path| {
        const dir = std.Io.Dir.cwd();
        if (std.fs.path.dirname(file_path)) |dir_name| {
            try dir.createDirPath(io, dir_name);
        }
        var file = try dir.createFile(io, file_path, .{});
        var buf: [128]u8 = undefined;
        var file_writer = file.writer(io, &buf);
        const writer = &file_writer.interface;
        defer file.close(io);
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
    defer mod.deinit();
    var bytecode = mod.generateBytecode(alloc) catch |err| {
        if (args.json) {
            emitDiagnostics(mod);
            emitJson(.{ .type = "result", .ok = false });
            return if (args.verbose) err else {};
        }
        try print("Could not generate bytecode", .{});
        try writeErrors(mod);
        return if (args.verbose) err else {};
    };
    if (args.json) emitDiagnostics(mod) else try writeErrors(mod);
    if (args.time and !args.json) try printCompileTimings(args.file.?, mod.timings);
    defer bytecode.free(alloc);

    if (args.json) emitJson(.{ .type = "result", .ok = true });
    if (args.dry) {
        if (!args.json) try print("Success\n", .{});
        return;
    }
    // --time alone (without -o) is a benchmark mode: timings already printed, nothing to write.
    if (args.output == null) return;
    const dir = std.Io.Dir.cwd();
    if (std.fs.path.dirname(args.output.?)) |dir_name| {
        try dir.createDirPath(io, dir_name);
    }
    var file = try dir.createFile(io, args.output.?, .{});
    defer file.close(io);
    var buf: [128]u8 = undefined;
    var file_writer = file.writer(io, &buf);
    _ = bytecode.serialize(alloc, &file_writer.interface) catch |err| {
        try print("Could not write compiled bytecode to file\n", .{});
        return if (args.verbose) err else {};
    };
}

fn printCompileTimings(file_path: []const u8, t: Module.Timings) !void {
    const ns_per_ms: f64 = 1_000_000.0;
    const total_f = @as(f64, @floatFromInt(t.total_ns));
    const total_ms = total_f / ns_per_ms;
    const resolve_ms = @as(f64, @floatFromInt(t.resolve_includes_ns)) / ns_per_ms;
    const parse_ms = @as(f64, @floatFromInt(t.parse_ns)) / ns_per_ms;
    const compile_ms = @as(f64, @floatFromInt(t.compile_ns)) / ns_per_ms;
    const resolve_pct = if (total_f > 0) (@as(f64, @floatFromInt(t.resolve_includes_ns)) * 100.0 / total_f) else 0.0;
    const parse_pct = if (total_f > 0) (@as(f64, @floatFromInt(t.parse_ns)) * 100.0 / total_f) else 0.0;
    const compile_pct = if (total_f > 0) (@as(f64, @floatFromInt(t.compile_ns)) * 100.0 / total_f) else 0.0;

    try print("topi compile timings ({s})\n", .{std.fs.path.basename(file_path)});
    try print("  files       {d}\n", .{t.file_count});
    try print("  source      {d} bytes\n", .{t.source_bytes});
    try print("  resolve     {d:.2} ms   ({d:.0}%)\n", .{ resolve_ms, resolve_pct });
    try print("  parse       {d:.2} ms   ({d:.0}%)\n", .{ parse_ms, parse_pct });
    try print("  compile     {d:.2} ms   ({d:.0}%)\n", .{ compile_ms, compile_pct });
    try print("  ---------------------\n", .{});
    try print("  total       {d:.2} ms\n", .{total_ms});
}

fn fmtCommand(args: FmtArgs, alloc: std.mem.Allocator) !void {
    const full_path = std.Io.Dir.cwd().realPathFileAlloc(io, args.file.?, alloc) catch |err| {
        try print("Could not find file at {s}\n", .{args.file.?});
        return if (args.verbose) err else {};
    };

    if (args.stamp_only) {
        const stamped = Stamp.stampFileAtPath(full_path, alloc) catch |err| {
            try print("Could not stamp file\n", .{});
            return if (args.verbose) err else {};
        };
        defer alloc.free(stamped);
        try writeOutput(full_path, stamped, args.dry);
        return;
    }

    var mod = Module.init(alloc, io, full_path) catch |err| {
        try print("Could not create module\n", .{});
        return if (args.verbose) err else {};
    };
    defer mod.deinit();
    mod.resolveIncludes() catch |err| {
        try print("Could not resolve includes\n", .{});
        return if (args.verbose) err else {};
    };
    mod.entry.buildTree() catch |err| {
        try print("Could not read .topi file\n", .{});
        try writeErrors(mod);
        return if (args.verbose) err else {};
    };

    const source = mod.entry.source orelse return;
    const tree = mod.entry.tree orelse return;
    const formatted = Formatter.format(source, tree, alloc, args.indent, args.stamp) catch |err| {
        try print("Could not format file\n", .{});
        return if (args.verbose) err else {};
    };
    defer alloc.free(formatted);
    try writeOutput(full_path, formatted, args.dry);
}

fn testCommand(args: TestArgs, alloc: std.mem.Allocator) !void {
    var mod = createModule(alloc, args) catch |err| {
        try print("Could not create module: {s}\n", .{@errorName(err)});
        return if (args.verbose) err else {};
    };
    defer mod.deinit();

    var bytecode = mod.generateBytecode(alloc) catch |err| {
        if (args.json) {
            emitDiagnostics(mod);
            emitJson(.{ .type = "end", .status = "compile_error" });
            return if (args.verbose) err else {};
        }
        try print("Could not create bytecode: {s}\n", .{@errorName(err)});
        try writeErrors(mod);
        return if (args.verbose) err else {};
    };
    if (args.json) emitDiagnostics(mod) else try writeErrors(mod);
    defer bytecode.free(alloc);
    seedBuiltins(args.seed);
    const base_seed = args.seed orelse blk: {
        var seed_bytes: [8]u8 = undefined;
        io.random(&seed_bytes);
        break :blk std.mem.readInt(u64, &seed_bytes, .little);
    };

    var visit_counts: std.array_hash_map.String(u64) = .empty;
    defer visit_counts.deinit(alloc);
    var i: usize = 0;
    while (i < args.count) : (i += 1) {
        var auto_runner = AutoTestRunner.init(base_seed +% i);
        var vm = try Vm.init(alloc, io, &bytecode, &auto_runner.runner);
        defer vm.deinit();
        try vm.start(args.bough);
        while (vm.can_continue) {
            vm.run() catch |e| {
                if (args.json) {
                    try emitRunEnd(&vm, alloc, "error", e);
                    return;
                }
                var buffer: [128]u8 = undefined;
                var writer = std.Io.File.stdout().writerStreaming(io, &buffer);
                const stdout = &writer.interface;
                if (vm.err.msg == null) {
                    stdout.print("Error: {s}\n", .{@errorName(e)}) catch {};
                    stdout.flush() catch {};
                } else {
                    vm.err.print(stdout);
                }
                return;
            };
        }
        if (args.quiet and !args.json) continue;
        for (vm.globals, 0..) |g, idx| {
            if (g == .visit) {
                const name = bytecode.global_symbols[idx].name;
                const cur = try visit_counts.getOrPutValue(alloc, name, 0);
                try visit_counts.put(alloc, name, cur.value_ptr.* + g.visit);
            } else break;
            // all visits are first so we can break
        }
    }
    if (args.json) return emitTestResult(args.count, base_seed, &visit_counts);
    if (args.quiet) {
        try print("Success\n", .{});
    }
    var it = visit_counts.iterator();
    while (it.next()) |entry| {
        try print("{s} = {}\n", .{ entry.key_ptr.*, entry.value_ptr.* });
    }
}

fn emitTestResult(runs: usize, seed: u64, visit_counts: *const std.array_hash_map.String(u64)) !void {
    var buffer: [256]u8 = undefined;
    var writer = std.Io.File.stdout().writerStreaming(io, &buffer);
    var s: std.json.Stringify = .{ .writer = &writer.interface };
    try s.beginObject();
    try s.objectField("type");
    try s.write("result");
    try s.objectField("runs");
    try s.write(runs);
    try s.objectField("seed");
    try s.write(seed);
    try s.objectField("visits");
    try s.beginObject();
    var it = visit_counts.iterator();
    while (it.next()) |entry| {
        try s.objectField(entry.key_ptr.*);
        try s.write(entry.value_ptr.*);
    }
    try s.endObject();
    try s.objectField("unvisited");
    try s.beginArray();
    it = visit_counts.iterator();
    while (it.next()) |entry| if (entry.value_ptr.* == 0) try s.write(entry.key_ptr.*);
    try s.endArray();
    try s.endObject();
    try writer.interface.writeByte('\n');
    try writer.interface.flush();
}

fn localizeCommand(args: LocalizeArgs, alloc: std.mem.Allocator) !void {
    const full_path = std.Io.Dir.cwd().realPathFileAlloc(io, args.file.?, alloc) catch |err| {
        try print("Could not find file at {s}\n", .{args.file.?});
        return if (args.verbose) err else {};
    };
    switch (args.command) {
        .generate => {
            const folder = args.folder orelse ".";
            if (!args.dry) {
                try std.Io.Dir.cwd().createDirPath(io, folder);
            }
            var buffer: [128]u8 = undefined;
            var w = std.Io.File.stdout().writerStreaming(io, &buffer);
            const stdout = &w.interface;
            var gen_result = Locale.generateFromModule(
                alloc,
                full_path,
                folder,
                args.locale_key,
                args.dry,
                stdout,
            ) catch |err| {
                try print("Error bundling localization: {s}\n", .{@errorName(err)});
                if (args.verbose) return err;
                return;
            };
            defer gen_result.deinit();

            // Report warnings
            for (gen_result.missing_csv_files.items) |path| {
                try print("Warning: no CSV found for {s} (run `topi loc export` on it)\n", .{path});
            }
            if (gen_result.missing_uuids.items.len > 0) {
                try print("Warning: {d} UUID(s) in source but not in CSV (re-export needed)\n", .{gen_result.missing_uuids.items.len});
            }
            if (gen_result.extra_uuids.items.len > 0) {
                try print("Warning: {d} UUID(s) in CSV but not in source (stale entries)\n", .{gen_result.extra_uuids.items.len});
            }
            if (gen_result.blank_translations.items.len > 0) {
                try print("Warning: {d} blank translation(s) in CSV\n", .{gen_result.blank_translations.items.len});
            }

            if (args.dry) {
                try print("Successfully generated localization.", .{});
            } else try print("Successfully generated localization into {s}\n", .{folder});
        },
        .@"export" => {
            var mod = Module.init(alloc, io, full_path) catch |err| {
                try print("Could not create module: {s}\n", .{@errorName(err)});
                return if (args.verbose) err else {};
            };
            defer mod.deinit();
            mod.resolveIncludes() catch |err| {
                try writeErrors(mod);
                return if (args.verbose) err else {};
            };
            mod.entry.buildTree() catch |err| {
                try writeErrors(mod);
                return if (args.verbose) err else {};
            };

            if (args.dry) {
                var buffer: [128]u8 = undefined;
                var writer = std.Io.File.stdout().writerStreaming(io, &buffer);
                const stdout = &writer.interface;
                Locale.exportFile(mod.entry, stdout, args.lang, alloc) catch |err| {
                    if (err == error.MissingStamp) {
                        try print("Export failed: dialogue(s) missing @UUID stamps. Run `topi fmt --stamp-only {s}` first.\n", .{args.file.?});
                    } else if (args.verbose) return err;
                    return;
                };
            } else {
                if (args.output == null) {
                    try print("Error: export requires -o <file> or --dry\n", .{});
                    return;
                }
                const dir = std.Io.Dir.cwd();
                if (std.fs.path.dirname(args.output.?)) |dir_name| {
                    try dir.createDirPath(io, dir_name);
                }

                // Read existing CSV before truncating for merge (unless --no-merge)
                const existing_csv: ?[]const u8 = if (args.no_merge) null else blk: {
                    const existing = dir.openFile(io, args.output.?, .{}) catch break :blk null;
                    defer existing.close(io);
                    var rbuf: [1024]u8 = undefined;
                    var rdr = existing.reader(io, &rbuf);
                    break :blk rdr.interface.allocRemaining(alloc, .unlimited) catch null;
                };
                defer if (existing_csv) |csv| alloc.free(csv);

                const file = try dir.createFile(io, args.output.?, .{});
                defer file.close(io);

                var buffer: [1028]u8 = undefined;
                var file_writer = file.writer(io, &buffer);
                const writer = &file_writer.interface;
                if (existing_csv) |csv| {
                    Locale.exportFileWithMerge(mod.entry, writer, args.lang, csv, alloc) catch |err| {
                        if (err == error.MissingStamp) {
                            try print("Export failed: dialogue(s) missing @UUID stamps. Run `topi fmt --stamp-only {s}` first.\n", .{args.file.?});
                        } else if (args.verbose) return err;
                        return;
                    };
                } else {
                    Locale.exportFile(mod.entry, writer, args.lang, alloc) catch |err| {
                        if (err == error.MissingStamp) {
                            try print("Export failed: dialogue(s) missing @UUID stamps. Run `topi fmt --stamp-only {s}` first.\n", .{args.file.?});
                        } else if (args.verbose) return err;
                        return;
                    };
                }
                try writer.flush();
            }
        },
        .validate => {
            var mod = Module.init(alloc, io, full_path) catch |err| {
                try print("Could not create module: {s}\n", .{@errorName(err)});
                return if (args.verbose) err else {};
            };
            defer mod.deinit();
            mod.resolveIncludes() catch |err| {
                try writeErrors(mod);
                return if (args.verbose) err else {};
            };
            mod.entry.buildTree() catch |err| {
                try writeErrors(mod);
                return if (args.verbose) err else {};
            };
            const missing = Locale.checkFile(mod.entry) catch |err| {
                if (args.verbose) return err;
                return;
            };
            if (missing == 0) {
                try print("All localization IDs present.\n", .{});
            } else {
                try print("{d} missing localization ID(s). Run `topi fmt --stamp-only {s}` to add them.\n", .{ missing, args.file.? });
            }
        },
    }
}
