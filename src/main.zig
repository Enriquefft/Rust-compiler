const std = @import("std");
const driver = @import("driver.zig");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var args = std.process.args();
    _ = args.next(); // executable name
    const maybe_path = args.next();
    if (maybe_path == null) {
        std.debug.print("usage: {s} <file>\n", .{"rust-compiler"});
        return;
    }

    const path = maybe_path.?;

    var result = try driver.compileFile(.{
        .allocator = allocator,
        .input_path = path,
        .emit_diagnostics = true,
        .exit_on_error = true,
    });
    defer result.deinit();

    switch (result.status) {
        .success => {},
        .errors => unreachable, // compileFile exits when exit_on_error is true
    }
}

test "Run all tests" {
    _ = @import("all_tests.zig");
}
