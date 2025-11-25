const std = @import("std");
const driver = @import("driver.zig");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var args = std.process.args();
    _ = args.next(); // executable name

    var visualize_tokens = false;
    var visualize_ast = false;
    var path: ?[]const u8 = null;

    while (args.next()) |arg| {
        if (std.mem.eql(u8, arg, "--print-tokens")) {
            visualize_tokens = true;
        } else if (std.mem.eql(u8, arg, "--print-ast")) {
            visualize_ast = true;
        } else if (std.mem.startsWith(u8, arg, "-")) {
            printUsage();
            return;
        } else if (path == null) {
            path = arg;
        } else {
            printUsage();
            return;
        }
    }

    if (path == null) {
        printUsage();
        return;
    }

    var result = try driver.compileFile(.{
        .allocator = allocator,
        .input_path = path.?,
        .emit_diagnostics = true,
        .exit_on_error = true,
        .visualize_tokens = visualize_tokens,
        .visualize_ast = visualize_ast,
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

fn printUsage() void {
    std.debug.print("usage: {s} [--print-tokens] [--print-ast] <file>\n", .{"rust-compiler"});
}
