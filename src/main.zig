const std = @import("std");
const parse = @import("parser.zig").parse;
const Lexer = @import("lexer.zig").Lexer;
const Serializer = @import("serialize.zig");

pub fn main() anyerror!void {
    const write_to_file = false;

    const input =
        \\fn add(a: i32, b: i32) -> i32 {
        \\    a + b
        \\}
    ;
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);

    // this will free anything created from this arena
    defer arena.deinit();

    // create an std.mem.Allocator from the arena, this will be
    // the allocator we'll use internally
    const allocator = arena.allocator();

    var lexer = Lexer{ .input = input };
    const tokens = try lexer.tokenize();
    const ast = try parse(tokens, &allocator);

    // Initialize a resizable buffer
    var buffer = std.ArrayList(u8).init(allocator);
    defer buffer.deinit();

    // Get a writer that writes into the buffer
    const writer = buffer.writer();

    // Call your function that writes to the writer
    try Serializer.serialize_program(writer, ast);

    // Retrieve the data written into the buffer
    const output = buffer.items;

    const pretty_output = try Serializer.pretty_print_json(&allocator, output);

    if (write_to_file) {
        var file = try std.fs.cwd().createFile("output.txt", .{});
        defer file.close();
        try file.writer().writeAll(pretty_output);
    } else {
        try std.io.getStdOut().writer().writeAll(pretty_output);
    }
}
