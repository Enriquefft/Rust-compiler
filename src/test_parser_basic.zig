const std = @import("std");
const parse = @import("parser.zig").parse;

test "parser evaluates basic addition and subtraction" {
    try test_parser_basic("1 + 2", 3.0);
    try test_parser_basic("5 - 3", 2.0);
}

test "parser evaluates basic multiplication and division" {
    try test_parser_basic("2 * 3", 6.0);
    try test_parser_basic("8 / 4", 2.0);
}

test "parser handles nested parentheses with basic operations" {
    try test_parser_basic("(2 + 3) * 4", 20.0); // 2 + 3 = 5; 5 * 4 = 20
    try test_parser_basic("4 * (6 / 2)", 12.0); // 6 / 2 = 3; 4 * 3 = 12
}

test "parser handles complex nested expressions with basic grammar" {
    try test_parser_basic("(2 + (3 * (4 + 5)))", 29.0); // 4 + 5 = 9; 3 * 9 = 27; 2 + 27 = 29
    try test_parser_basic("((2 * 3) + (4 / 2))", 8.0); // 2 * 3 = 6; 4 / 2 = 2; 6 + 2 = 8
}

test "parser handles multiple operations in sequence with basic grammar" {
    try test_parser_basic("1 + 2 - 3 + 4", 4.0); // 1 + 2 = 3; 3 - 3 = 0; 0 + 4 = 4
    try test_parser_basic("10 / 2 * 3", 15.0); // 10 / 2 = 5; 5 * 3 = 15
}

test "parser evaluates division followed by multiplication with basic grammar" {
    try test_parser_basic("12 / 2 * 3", 18.0); // 12 / 2 = 6; 6 * 3 = 18
}

test "parser handles nested parentheses with multiple operations in basic grammar" {
    try test_parser_basic("((2 + 2) * 3) - 4", 8.0); // 2 + 2 = 4; 4 * 3 = 12; 12 - 4 = 8
    try test_parser_basic("5 - ((1 + 2) * 2)", -1.0); // 1 + 2 = 3; 3 * 2 = 6; 5 - 6 = -1
}

fn test_parser_basic(input: []const u8, expected: f64) !void {
    const result = parse(input);

    try std.testing.expectEqual(expected, result);
}
