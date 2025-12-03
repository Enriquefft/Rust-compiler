const std = @import("std");
const driver = @import("driver.zig");

/// End-to-end integration tests that compile sample programs from the codes/
/// directory, link the generated assembly into binaries, execute them, and
/// assert the output matches the expected output.
///
/// This module replaces the old test_outputs.sh script by running all output
/// comparison tests within the Zig test framework.
/// A test case with filename and expected output
const TestCase = struct {
    filename: []const u8,
    expected: []const u8,
};

const FailTestCase = struct {
    filename: []const u8,
    expected_error: []const u8,
};

/// All test cases from expected_outputs.txt
/// Format matches: filename=expected_output (with \n for newlines)
const test_cases = [_]TestCase{
    .{ .filename = "closures_test1.rs", .expected = "6 11" },
    .{ .filename = "closures_test2.rs", .expected = "6 15" },
    .{ .filename = "conditionals_and_ranges_test1.rs", .expected = "even 12" },
    .{ .filename = "conditionals_and_ranges_test2.rs", .expected = "36 big" },
    .{ .filename = "control_flow_test1.rs", .expected = "4" },
    .{ .filename = "control_flow_test2.rs", .expected = "10" },
    .{ .filename = "conversions_test1.rs", .expected = "-12 -24 5 0" },
    .{ .filename = "conversions_test2.rs", .expected = "1 245 255" },
    .{ .filename = "core_language_test1.rs", .expected = "1 6 10" },
    .{ .filename = "core_language_test2.rs", .expected = "8 14" },
    .{ .filename = "dynamic_data_test1.rs", .expected = "3" },
    .{ .filename = "dynamic_data_test2.rs", .expected = "Alice 30" },
    .{ .filename = "expressions_test1.rs", .expected = "18 1 1" },
    .{ .filename = "expressions_test2.rs", .expected = "65 55 25" },
    .{ .filename = "functions_and_methods_test1.rs", .expected = "16" },
    .{ .filename = "functions_and_methods_test2.rs", .expected = "2" },
    .{ .filename = "generics_test1.rs", .expected = "1 2" },
    .{ .filename = "generics_test2.rs", .expected = "hi 42" },
    .{ .filename = "hello_println.rs", .expected = "hello" },
    .{ .filename = "input1.rs", .expected = "20\n10\n1000000" },
    .{ .filename = "input2.rs", .expected = "10" },
    .{ .filename = "input3.rs", .expected = "46" },
    .{ .filename = "input4.rs", .expected = "21" },
    .{ .filename = "macros_test1.rs", .expected = "hello from println!" },
    .{ .filename = "macros_test2.rs", .expected = "a = 3, b = 4" },
    .{ .filename = "statements_and_patterns_test1.rs", .expected = "2 5" },
    .{ .filename = "statements_and_patterns_test2.rs", .expected = "6" },
    .{ .filename = "types_test1.rs", .expected = "-42 7 3.500000 1 z hello world" },
    .{ .filename = "types_test2.rs", .expected = "4 11 0 4" },
    .{ .filename = "arrays1.rs", .expected = "sum = 18" },
    .{ .filename = "arrays2.rs", .expected = "doubled: 40" },
    .{ .filename = "arrays3.rs", .expected = "(2, 3)\n(-2, 5)\n(11, -4)" },
    .{ .filename = "optimizations.rs", .expected = "Result: 64" },
    .{ .filename = "const_items_test1.rs", .expected = "100 3.141590 0" },
    .{ .filename = "const_items_test2.rs", .expected = "-42 255 2.500000 1" },
    .{ .filename = "struct_init_shorthand_test1.rs", .expected = "10 20" },
    .{ .filename = "struct_init_shorthand_test2.rs", .expected = "100 50 0 0" },
    .{ .filename = "unsafe_blocks_test1.rs", .expected = "42" },
    .{ .filename = "unsafe_blocks_test2.rs", .expected = "20" },
    .{ .filename = "ownership.rs", .expected = "hi" },
};

const failing_test_cases = [_]FailTestCase{
    .{ .filename = "ownership_fail.rs", .expected_error = "use of moved value" },
};

/// Run a single end-to-end test case
fn runE2ETest(allocator: std.mem.Allocator, comptime tc: TestCase) !void {
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();

    const tmp_path = try tmp.dir.realpathAlloc(allocator, ".");
    defer allocator.free(tmp_path);

    // Generate paths based on filename stem
    const stem = tc.filename[0 .. tc.filename.len - 3]; // Remove .rs extension
    const asm_filename = stem ++ ".s";
    const bin_filename = stem ++ ".bin";

    const asm_path = try std.fs.path.join(allocator, &.{ tmp_path, asm_filename });
    defer allocator.free(asm_path);

    const bin_path = try std.fs.path.join(allocator, &.{ tmp_path, bin_filename });
    defer allocator.free(bin_path);

    const input_path = "codes/" ++ tc.filename;

    // Compile the source file
    var result = try driver.compileFile(.{
        .allocator = allocator,
        .input_path = input_path,
        .output_path = asm_path,
        .opt_level = .basic,
        .emit = .assembly,
        .emit_diagnostics = false,
        .exit_on_error = false,
    });
    defer result.deinit();

    if (result.status != .success) {
        std.debug.print("\nCompilation failed for {s}\n", .{tc.filename});
        return error.CompilationFailed;
    }

    // Link the assembly into a binary using gcc (consistent with run_asm.sh)
    const cc_result = try std.process.Child.run(.{
        .allocator = allocator,
        .argv = &.{ "gcc", "-no-pie", "-w", "-o", bin_path, asm_path },
    });
    defer {
        allocator.free(cc_result.stdout);
        allocator.free(cc_result.stderr);
    }

    switch (cc_result.term) {
        .Exited => |code| {
            if (code != 0) {
                std.debug.print("\nLinking failed for {s}: {s}\n", .{ tc.filename, cc_result.stderr });
                return error.LinkingFailed;
            }
        },
        else => {
            std.debug.print("\nLinking process terminated abnormally for {s}\n", .{tc.filename});
            return error.LinkingFailed;
        },
    }

    // Run the compiled binary
    const run_result = try std.process.Child.run(.{
        .allocator = allocator,
        .argv = &.{bin_path},
    });
    defer {
        allocator.free(run_result.stdout);
        allocator.free(run_result.stderr);
    }

    switch (run_result.term) {
        .Exited => |code| {
            if (code != 0) {
                std.debug.print("\nExecution failed for {s} with exit code {d}: {s}\n", .{ tc.filename, code, run_result.stderr });
                return error.ExecutionFailed;
            }
        },
        else => {
            std.debug.print("\nExecution process terminated abnormally for {s}\n", .{tc.filename});
            return error.ExecutionFailed;
        },
    }

    // Compare output - expected output should match stdout with trailing newline
    const expected_with_newline = tc.expected ++ "\n";
    if (!std.mem.eql(u8, run_result.stdout, expected_with_newline)) {
        std.debug.print("\nOutput mismatch for {s}:\n  Expected: '{s}'\n  Actual:   '{s}'\n", .{
            tc.filename,
            expected_with_newline,
            run_result.stdout,
        });
        return error.TestExpectedEqual;
    }
}

fn runFailingTest(allocator: std.mem.Allocator, comptime tc: FailTestCase) !void {
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();

    const tmp_path = try tmp.dir.realpathAlloc(allocator, ".");
    defer allocator.free(tmp_path);

    const stem = tc.filename[0 .. tc.filename.len - 3];
    const asm_filename = stem ++ ".s";
    const asm_path = try std.fs.path.join(allocator, &.{ tmp_path, asm_filename });
    defer allocator.free(asm_path);

    const input_path = "codes/" ++ tc.filename;

    var result = try driver.compileFile(.{
        .allocator = allocator,
        .input_path = input_path,
        .output_path = asm_path,
        .opt_level = .basic,
        .emit = .assembly,
        .emit_diagnostics = false,
        .exit_on_error = false,
    });
    defer result.deinit();

    if (result.status != .errors) {
        std.debug.print("\nCompilation for {s} unexpectedly succeeded\n", .{tc.filename});
        return error.TestExpectedEqual;
    }

    var diag_buffer = std.array_list.Managed(u8).init(allocator);
    defer diag_buffer.deinit();
    try result.diagnostics.emitAllToWriter(&result.source_map, diag_buffer.writer());

    if (std.mem.indexOf(u8, diag_buffer.items, tc.expected_error) == null) {
        std.debug.print("\nExpected diagnostics for {s} to contain '{s}', got:\n{s}\n", .{
            tc.filename,
            tc.expected_error,
            diag_buffer.items,
        });
        return error.TestExpectedEqual;
    }
}

// Generate individual test declarations for each test case
test "e2e output cases" {
    inline for (test_cases) |tc| {
        try runE2ETest(std.testing.allocator, tc);
    }
}

test "e2e failure cases" {
    inline for (failing_test_cases) |tc| {
        try runFailingTest(std.testing.allocator, tc);
    }
}
