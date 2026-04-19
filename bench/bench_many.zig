// Many-modules compile benchmark.
//
// Compiles every .topi file in the given fixture directory as its own
// independent module, first sequentially then in parallel across
// std.Thread.getCpuCount() workers, and reports wall-clock timings.
//
// Usage: bench-many <fixture-dir>

const std = @import("std");
const topi = @import("topi");
const Module = topi.module.Module;

const io = std.Io.Threaded.global_single_threaded.io();

pub fn main(init: std.process.Init.Minimal) !void {
    const alloc = std.heap.smp_allocator;

    var args_iter = std.process.Args.Iterator.init(init.args);
    _ = args_iter.skip();
    const dir_path = args_iter.next() orelse {
        std.debug.print("usage: bench-many <fixture-dir>\n", .{});
        return error.MissingArgument;
    };

    const paths = try collectTopiFiles(alloc, dir_path);
    defer {
        for (paths) |p| alloc.free(p);
        alloc.free(paths);
    }

    if (paths.len == 0) {
        std.debug.print("bench-many: no .topi files found in {s}\n", .{dir_path});
        return error.NoFiles;
    }

    const cpu_count = std.Thread.getCpuCount() catch 1;
    std.debug.print("bench-many: {d} files, {d} CPUs\n\n", .{ paths.len, cpu_count });

    // Warm-up: compile each file once to prime caches. Not timed.
    for (paths) |p| try compileOne(alloc, p);

    const seq_ns = try runSequential(alloc, paths);
    printResult("sequential", seq_ns, paths.len, 1);

    const par_ns = try runParallel(alloc, paths, cpu_count);
    printResult("parallel  ", par_ns, paths.len, cpu_count);

    const speedup = @as(f64, @floatFromInt(seq_ns)) / @as(f64, @floatFromInt(par_ns));
    std.debug.print("\nspeedup: {d:.2}x on {d} threads\n", .{ speedup, cpu_count });
}

fn collectTopiFiles(alloc: std.mem.Allocator, dir_path: []const u8) ![][]const u8 {
    var dir = try std.Io.Dir.cwd().openDir(io, dir_path, .{ .iterate = true });
    defer dir.close(io);

    const abs_dir = try std.Io.Dir.cwd().realPathFileAlloc(io, dir_path, alloc);
    defer alloc.free(abs_dir);

    var list: std.ArrayList([]const u8) = .empty;
    errdefer {
        for (list.items) |p| alloc.free(p);
        list.deinit(alloc);
    }

    var iter = dir.iterate();
    while (try iter.next(io)) |entry| {
        if (entry.kind != .file) continue;
        if (!std.mem.endsWith(u8, entry.name, ".topi")) continue;
        const joined = try std.fs.path.join(alloc, &.{ abs_dir, entry.name });
        try list.append(alloc, joined);
    }

    std.mem.sort([]const u8, list.items, {}, lessThanStr);
    return try list.toOwnedSlice(alloc);
}

fn lessThanStr(_: void, a: []const u8, b: []const u8) bool {
    return std.mem.order(u8, a, b) == .lt;
}

fn compileOne(alloc: std.mem.Allocator, path: []const u8) !void {
    var mod = try Module.init(alloc, io, path);
    defer mod.deinit();
    var bytecode = try mod.generateBytecode(alloc);
    defer bytecode.free(alloc);
}

fn runSequential(alloc: std.mem.Allocator, paths: []const []const u8) !u64 {
    const start = std.Io.Timestamp.now(io, .awake).nanoseconds;
    for (paths) |p| try compileOne(alloc, p);
    const end = std.Io.Timestamp.now(io, .awake).nanoseconds;
    return @intCast(end - start);
}

const WorkerCtx = struct {
    alloc: std.mem.Allocator,
    paths: []const []const u8,
    next: *std.atomic.Value(usize),
    err: *std.atomic.Value(u8), // 0 = ok, 1 = some worker failed
};

fn worker(ctx: *WorkerCtx) void {
    while (true) {
        const idx = ctx.next.fetchAdd(1, .monotonic);
        if (idx >= ctx.paths.len) return;
        compileOne(ctx.alloc, ctx.paths[idx]) catch {
            _ = ctx.err.swap(1, .monotonic);
            return;
        };
    }
}

fn runParallel(alloc: std.mem.Allocator, paths: []const []const u8, n_threads: usize) !u64 {
    var next: std.atomic.Value(usize) = .init(0);
    var err_flag: std.atomic.Value(u8) = .init(0);

    var ctx: WorkerCtx = .{
        .alloc = alloc,
        .paths = paths,
        .next = &next,
        .err = &err_flag,
    };

    const threads = try alloc.alloc(std.Thread, n_threads);
    defer alloc.free(threads);

    const start = std.Io.Timestamp.now(io, .awake).nanoseconds;
    for (threads) |*t| {
        t.* = try std.Thread.spawn(.{}, worker, .{&ctx});
    }
    for (threads) |t| t.join();
    const end = std.Io.Timestamp.now(io, .awake).nanoseconds;

    if (err_flag.load(.monotonic) != 0) return error.ParallelCompileFailed;
    return @intCast(end - start);
}

fn printResult(label: []const u8, ns: u64, file_count: usize, workers: usize) void {
    const ms = @as(f64, @floatFromInt(ns)) / 1_000_000.0;
    const per_file_us = (@as(f64, @floatFromInt(ns)) / 1000.0) / @as(f64, @floatFromInt(file_count));
    std.debug.print(
        "{s}: {d:.2} ms total  ({d:.1} us/file, {d} workers)\n",
        .{ label, ms, per_file_us, workers },
    );
}
