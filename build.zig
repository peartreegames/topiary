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

    const topi_export = b.addModule("export", .{
        .root_source_file = b.path("src/export/index.zig"),
    });
    topi_export.addImport("topi", topi);

    const topilib = b.addLibrary(.{
        .name = name,
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
}

fn getVersion(b: *std.Build) ![]const u8 {
    var tree = try std.zig.Ast.parse(b.allocator, @embedFile("build.zig.zon"), .zon);
    defer tree.deinit(b.allocator);
    const version_str = tree.tokenSlice(tree.nodes.items(.main_token)[2]);
    return b.allocator.dupe(u8, version_str[1 .. version_str.len - 1]);
}
