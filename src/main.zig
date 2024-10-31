//! Main entry point for the calculator.
const std = @import("std");
const parse = @import("parser.zig").parse;

pub fn main() !void {
    var input: [64]u8 = undefined;
    const stdin = std.io.getStdIn().reader();
    const stdout = std.io.getStdOut().writer();

    _ = try stdin.readUntilDelimiter(&input, '\n');

    try stdout.print("Input: {s}\n", .{input});

    const result = try parse(&input);
    try stdout.print("Result: {}\n", .{result});
}
