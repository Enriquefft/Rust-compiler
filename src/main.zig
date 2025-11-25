const std = @import("std");
const lexer = @import("frontend/lexer.zig");
const diag = @import("diag/diagnostics.zig");
const source_map = @import("diag/source_map.zig");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var args = std.process.args();
    _ = args.next(); // executable name
    const maybe_path = args.next();
    if (maybe_path == null) {
        const stderr = std.io.getStdErr().writer();
        try stderr.print("usage: {s} <file>\n", .{"rust-compiler"});
        return;
    }

    const path = maybe_path.?;
    const contents = try std.fs.cwd().readFileAlloc(allocator, path, std.math.maxInt(usize));
    defer allocator.free(contents);

    var sm = source_map.SourceMap.init(allocator);
    defer sm.deinit();
    const file_id = try sm.addFile(path, contents);

    var diagnostics = diag.Diagnostics.init(allocator);
    defer diagnostics.deinit();

    const toks = try lexer.lex(allocator, file_id, contents, &diagnostics);
    defer allocator.free(toks);

    var stdout = std.io.getStdOut().writer();
    for (toks) |tok| {
        try stdout.print("{s}\t{s}\n", .{ @tagName(tok.kind), tok.lexeme });
    }

    if (diagnostics.hasErrors()) {
        try diagnostics.emitAll(&sm);
        std.process.exit(1);
    }
}

test "frontend lexer is reachable" {
    _ = @import("frontend/lexer.zig");
}
