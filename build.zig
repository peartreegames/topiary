const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});
    const name = b.option([]const u8, "name", "Name of the output file") orelse "topi";

    const version = getVersion(b) catch |err| {
        std.log.err("Could not get version: {}", .{err});
        return;
    };
    const build_options = b.addOptions();
    build_options.addOption([]const u8, "version", version);

    const topi = b.addModule("topi", .{
        .root_source_file = b.path("src/topi.zig"),
    });

    _ = b.addModule("topi_docs", .{
        .root_source_file = b.path("docs/docs.zig"),
    });

    const topi_export = b.addModule("export", .{
        .root_source_file = b.path("src/export/index.zig"),
    });
    topi_export.addImport("topi", topi);

    const topilib = b.addLibrary(.{
        .name = name,
        .linkage = .dynamic,
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/export/main.zig"),
            .target = target,
            .optimize = optimize,
        }),
    });
    topilib.root_module.addImport("topi", topi);

    const art = b.addInstallArtifact(topilib, .{ .dest_dir = .{ .override = .lib } });
    b.getInstallStep().dependOn(&art.step);

    const exe = b.addExecutable(.{
        .name = name,
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/cli/main.zig"),
            .target = b.graph.host,
            .optimize = optimize,
        }),
    });
    exe.root_module.addOptions("build", build_options);
    exe.root_module.addImport("topi", topi);

    b.installArtifact(exe);

    const run_cmd = b.addRunArtifact(exe);

    run_cmd.step.dependOn(b.getInstallStep());
    if (b.args) |args| {
        run_cmd.addArgs(args);
    }

    const run_step = b.step("run", "Run the app");
    run_step.dependOn(&run_cmd.step);

    const filter = b.option([]const []const u8, "filter", "Filter strings for tests") orelse &[_][]const u8{};

    const tests = b.addTest(.{
        .filters = filter,
        .root_module = b.createModule(.{
            .root_source_file = b.path("test/index.zig"),
            .target = target,
            .optimize = optimize,
        }),
    });

    tests.root_module.addImport("topi", topi);
    tests.root_module.addImport("export", topi_export);
    const run_tests = b.addRunArtifact(tests);

    const test_step = b.step("test", "Run unit tests");
    test_step.dependOn(&run_tests.step);

    // Run with: `zig build fuzz --fuzz`
    // The top-level `--fuzz` flag tells the build system to enable continuous
    // fuzzing for tests that call `std.testing.fuzz`.
    const fuzz_step = b.step("fuzz", "Run fuzz tests (add --fuzz to enable continuous fuzzing)");
    fuzz_step.dependOn(&run_tests.step);

    // --- Benchmark fixture + run ---
    // `zig build bench-gen` writes 1 entry + 20 chapter .topi files into bench/fixtures/.
    // `zig build bench` runs that and then compiles the fixture with --time.
    const bench_gen_exe = b.addExecutable(.{
        .name = "bench-gen",
        .root_module = b.createModule(.{
            .root_source_file = b.path("bench/gen.zig"),
            .target = b.graph.host,
            .optimize = .Debug,
        }),
    });
    const run_bench_gen = b.addRunArtifact(bench_gen_exe);
    run_bench_gen.addArg("bench/fixtures/");

    const bench_gen_step = b.step("bench-gen", "Generate benchmark fixture (entry + 20 chapters)");
    bench_gen_step.dependOn(&run_bench_gen.step);

    const run_bench = b.addRunArtifact(exe);
    run_bench.step.dependOn(&run_bench_gen.step);
    run_bench.addArgs(&.{ "compile", "bench/fixtures/entry.topi", "--time" });

    const bench_step = b.step("bench", "Generate fixture and run topi compile --time on it");
    bench_step.dependOn(&run_bench.step);
}

fn getVersion(b: *std.Build) ![]const u8 {
    const zon = @import("build.zig.zon");
    return b.allocator.dupe(u8, zon.version);
}
