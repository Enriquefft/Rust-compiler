const std = @import("std");
const parse = @import("parser.zig").parse;
const Lexer = @import("lexer.zig").Lexer;
const pretty_print = @import("ast.zig").pretty_print_program;

pub fn main() !void {
    const input =
        \\fn add(a: i32, b: i32) -> i32 {
        \\    a + b
        \\}
    ;
    var lexer = Lexer{ .input = input };
    const tokens = try lexer.tokenize();
    const ast = try parse(tokens);
    const writer = std.io.getStdOut().writer();
    try pretty_print( writer,ast);


}
