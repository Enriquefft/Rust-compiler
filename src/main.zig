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
    var opt_level: driver.OptLevel = .basic;
    var emit: driver.Emit = .assembly;
    var output_path: ?[]u8 = null;
    var path: ?[]const u8 = null;

    while (args.next()) |arg| {
        if (std.mem.eql(u8, arg, "--print-tokens")) {
            visualize_tokens = true;
        } else if (std.mem.eql(u8, arg, "--print-ast")) {
            visualize_ast = true;
        } else if (std.mem.startsWith(u8, arg, "--opt=")) {
            const value = arg["--opt=".len..];
            const parsed = parseOptLevel(value) orelse {
                printUsage();
                return;
            };
            opt_level = parsed;
        } else if (std.mem.startsWith(u8, arg, "--emit=")) {
            const value = arg["--emit=".len..];
            const parsed = parseEmit(value) orelse {
                printUsage();
                return;
            };
            emit = parsed;
        } else if (std.mem.eql(u8, arg, "-o")) {
            const out_path = args.next() orelse {
                printUsage();
                return;
            };
            if (output_path) |existing| {
                allocator.free(existing);
            }
            output_path = try allocator.dupe(u8, out_path);
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

    const final_output_path = output_path orelse try defaultOutputPath(allocator, path.?, emit);
    defer allocator.free(final_output_path);

    var result = try driver.compileFile(.{
        .allocator = allocator,
        .input_path = path.?,
        .output_path = final_output_path,
        .opt_level = opt_level,
        .emit = emit,
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
    std.debug.print(
        "usage: {s} [--print-tokens] [--print-ast] [--opt=none|basic] [--emit=asm|obj] [-o <path>] <file>\n",
        .{"rust-compiler"},
    );
}

fn parseOptLevel(value: []const u8) ?driver.OptLevel {
    if (std.mem.eql(u8, value, "none")) return .none;
    if (std.mem.eql(u8, value, "basic")) return .basic;
    return null;
}

fn parseEmit(value: []const u8) ?driver.Emit {
    if (std.mem.eql(u8, value, "asm")) return .assembly;
    if (std.mem.eql(u8, value, "obj")) return .object;
    return null;
}

fn defaultOutputPath(allocator: std.mem.Allocator, input_path: []const u8, emit: driver.Emit) ![]u8 {
    const dir = std.fs.path.dirname(input_path) orelse ".";
    const base = std.fs.path.basename(input_path);
    const ext = std.fs.path.extension(base);
    const stem = base[0 .. base.len - ext.len];
    const suffix = switch (emit) {
        .assembly => ".s",
        .object => ".o",
    };
    const filename = try std.fmt.allocPrint(allocator, "{s}{s}", .{ stem, suffix });
    defer allocator.free(filename);
    return std.fs.path.join(allocator, &.{ dir, filename });
}
