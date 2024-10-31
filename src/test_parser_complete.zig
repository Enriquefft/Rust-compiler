const std = @import("std");
const parse = @import("parser.zig").parse;

// Tests for complete grammars
test "parser handles mixed operations and nested parentheses" {
    try test_parser_complete("3 + (4 * (2 + 1))", 15.0); // 2 + 1 = 3; 4 * 3 = 12; 3 + 12 = 15
    try test_parser_complete("((10 / 2) + 3) * 2", 16.0); // 10 / 2 = 5; 5 + 3 = 8; 8 * 2 = 16
}

test "parser handles deeply nested expressions" {
    try test_parser_complete("(((1 + 2) * 3) + 4) * 5", 65.0); // 1 + 2 = 3; 3 * 3 = 9; 9 + 4 = 13; 13 * 5 = 65
    try test_parser_complete("2 * (3 + (4 * (5 + 6)))", 94.0); // 5 + 6 = 11; 4 * 11 = 44; 3 + 44 = 47; 2 * 47 = 94
}

test "parser handles unary minus in complete grammars" {
    try test_parser_complete("-5 + 3", -2.0);
    try test_parser_complete("-(2 + 3) * 4", -20.0); // 2 + 3 = 5; -5 * 4 = -20
}

test "parser handles mixed operations without parentheses" {
    try test_parser_complete("2 + 3 * 4 - 5", 9.0); // 3 * 4 = 12; 2 + 12 = 14; 14 - 5 = 9
    try test_parser_complete("10 / 2 + 6 - 3", 8.0); // 10 / 2 = 5; 5 + 6 = 11; 11 - 3 = 8
}

test "parser handles multiple levels of nested parentheses" {
    try test_parser_complete("(2 * (3 + (4 - 1)))", 12.0); // 4 - 1 = 3; 3 + 3 = 6; 2 * 6 = 12
    try test_parser_complete("((2 + 3) * (4 / 2))", 10.0); // 2 + 3 = 5; 4 / 2 = 2; 5 * 2 = 10
}

test "parser handles floating-point numbers correctly in complete grammars" {
    try test_parser_complete("2.5 * 4", 10.0);
    try test_parser_complete("5.0 / 2.0 + 3.5", 6.0);
}

test "parser evaluates exponentiation" {
    try test_parser_complete("2 ^ 3", 8.0); // 2^3 = 8
    try test_parser_complete("4 ^ (1 + 1)", 16.0); // 1 + 1 = 2; 4^2 = 16
}

test "parser handles exponentiation with parentheses" {
    try test_parser_complete("(2 ^ 3) ^ 2", 64.0); // 2^3 = 8; 8^2 = 64
    try test_parser_complete("2 ^ (3 ^ 2)", 512.0); // 3^2 = 9; 2^9 = 512
}

test "parser evaluates modulo operation" {
    try test_parser_complete("10 % 3", 1);
    try test_parser_complete("20 % 6", 2);
}

test "parser handles factorials correctly" {
    try test_parser_complete("5!", 120.0);
    try test_parser_complete("(2 + 3)!", 120.0); // 2 + 3 = 5; 5! = 120
}

test "parser handles combined exponentiation and factorials" {
    try test_parser_complete("2 ^ (3!)", 64); // 3! = 6; 2^6 = 64
    try test_parser_complete("(2 ^ 3)!", 40320); // 2^3 = 8; 8! = 40320
}

test "parser handles complex mixed operations" {
    try test_parser_complete("2 ^ 3 * 4 + 5!", 152); // 2^3 = 8; 8 * 4 = 32; 5! = 120; 32 + 120 = 152
    try test_parser_complete("10 % 4 ^ ((2 + 1)!)", 10); // 2 + 1 = 3; 3! = 6; 4^6 = 4096; 10 % 4096 = 10
}

fn test_parser_complete(input: []const u8, expected: f64) !void {
    const result = parse(input);

    try std.testing.expectEqual(expected, result);
}
