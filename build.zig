const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const version = getVersion(b) catch |err| {
        std.log.err("Could not get version: {}", .{err});
        return;
    };
    const build_options = b.addOptions();
    build_options.addOption([]const u8, "version", version);

     _ = b.addModule("topi", .{
         .root_source_file = b.path("src/topi.zig"),
     });

    const topilib = b.addSharedLibrary(.{
        .name = "topi",
        .root_source_file = b.path("src/export.zig"),
        .target = target,
        .optimize = optimize,
    });

    const art = b.addInstallArtifact(topilib, .{ .dest_dir = .{ .override = .lib } });
    b.getInstallStep().dependOn(&art.step);

    const exe = b.addExecutable(.{
        .name = "topi",
        .root_source_file = b.path("src/cli.zig"),
        .target = target,
        .optimize = optimize,
    });
    exe.root_module.addOptions("build", build_options);
    b.installArtifact(exe);

    const run_cmd = b.addRunArtifact(exe);

    run_cmd.step.dependOn(b.getInstallStep());
    if (b.args) |args| {
        run_cmd.addArgs(args);
    }

    const run_step = b.step("run", "Run the app");
    run_step.dependOn(&run_cmd.step);

    const tests = b.addTest(.{
        .root_source_file = b.path("src/vm.zig"),
        .target = target,
        .optimize = optimize,
    });

    const test_step = b.step("test", "Run unit tests");
    test_step.dependOn(&tests.step);
}

fn getVersion(b: *std.Build) ![]const u8 {
    var tree = try std.zig.Ast.parse(b.allocator, @embedFile("build.zig.zon"), .zon);
    defer tree.deinit(b.allocator);
    const version_str = tree.tokenSlice(tree.nodes.items(.main_token)[2]);
    return b.allocator.dupe(u8, version_str[1 .. version_str.len - 1]);
}
