const std = @import("std");
const driver = @import("driver.zig");

// End-to-end integration test that compiles a sample program,
// links the generated assembly into a binary, executes it, and
// asserts the output matches the println! expectation.
test "println end-to-end emits hello" {
    const allocator = std.testing.allocator;

    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();

    const tmp_path = try tmp.dir.realpathAlloc(allocator, ".");
    defer allocator.free(tmp_path);

    const asm_path = try std.fs.path.join(allocator, &.{ tmp_path, "hello.s" });
    defer allocator.free(asm_path);

    const bin_path = try std.fs.path.join(allocator, &.{ tmp_path, "hello.bin" });
    defer allocator.free(bin_path);

    var result = try driver.compileFile(.{
        .allocator = allocator,
        .input_path = "codes/hello_println.rs",
        .output_path = asm_path,
        .opt_level = .basic,
        .emit = .assembly,
        .emit_diagnostics = false,
        .exit_on_error = false,
    });
    defer result.deinit();

    try std.testing.expectEqual(driver.CompileStatus.success, result.status);

    const cc_result = try std.process.Child.run(.{
        .allocator = allocator,
        .argv = &.{ "zig", "cc", "-o", bin_path, asm_path },
    });
    defer {
        allocator.free(cc_result.stdout);
        allocator.free(cc_result.stderr);
    }

    switch (cc_result.term) {
        .Exited => |code| try std.testing.expectEqual(@as(u8, 0), code),
        else => return error.TestExpectedEqual,
    }

    const run_result = try std.process.Child.run(.{
        .allocator = allocator,
        .argv = &.{bin_path},
    });
    defer {
        allocator.free(run_result.stdout);
        allocator.free(run_result.stderr);
    }

    switch (run_result.term) {
        .Exited => |code| try std.testing.expectEqual(@as(u8, 0), code),
        else => return error.TestExpectedEqual,
    }

    try std.testing.expectEqualStrings("hello\n", run_result.stdout);
}
