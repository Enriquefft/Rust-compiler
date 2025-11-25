const std = @import("std");

// Although this function looks imperative, note that its job is to
// declaratively construct a build graph that will be executed by an external
// runner.
pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const main_module = b.createModule(.{
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
    });

    const exe = b.addExecutable(.{
        .name = "rust-compiler",
        .root_module = main_module,
    });
    b.installArtifact(exe);

    const run_cmd = b.addRunArtifact(exe);
    if (b.args) |args| run_cmd.addArgs(args);

    const run_step = b.step("run", "Run the rust-compiler");
    run_step.dependOn(&run_cmd.step);

    const main_test_module = b.createModule(.{
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
    });

    const main_tests = b.addTest(.{
        .root_module = main_test_module,
    });

    const all_tests_module = b.createModule(.{
        .root_source_file = b.path("src/all_tests.zig"),
        .target = target,
        .optimize = optimize,
    });

    const all_module_tests = b.addTest(.{
        .root_module = all_tests_module,
    });

    const test_step = b.step("test", "Run unit tests and golden tests");
    test_step.dependOn(&main_tests.step);
    test_step.dependOn(&all_module_tests.step);
    // TODO: attach golden file runners once tests/ fixtures land per architecture roadmap.

    // TODO: add documentation/auxiliary targets (e.g., docs, lint) when modules exist.
}
