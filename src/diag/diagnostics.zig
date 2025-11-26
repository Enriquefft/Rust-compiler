const std = @import("std");
const source_map = @import("source_map.zig");
const Span = source_map.Span;

pub const Severity = enum {
    err,
    warning,
};

pub const Diagnostic = struct {
    severity: Severity,
    message: []const u8,
    primary_span: Span,
};

pub const Diagnostics = struct {
    allocator: std.mem.Allocator,
    entries: std.ArrayListUnmanaged(Diagnostic),

    pub fn init(allocator: std.mem.Allocator) Diagnostics {
        return .{ .allocator = allocator, .entries = .{} };
    }

    pub fn deinit(self: *Diagnostics) void {
        for (self.entries.items) |entry| {
            self.allocator.free(entry.message);
        }
        self.entries.deinit(self.allocator);
    }

    pub fn reportError(self: *Diagnostics, span: Span, msg: []const u8) void {
        self.append(.err, span, msg) catch {};
    }

    pub fn reportWarning(self: *Diagnostics, span: Span, msg: []const u8) void {
        self.append(.warning, span, msg) catch {};
    }

    fn append(self: *Diagnostics, severity: Severity, span: Span, msg: []const u8) !void {
        const owned_msg = try self.allocator.dupe(u8, msg);
        try self.entries.append(self.allocator, .{ .severity = severity, .message = owned_msg, .primary_span = span });
    }

    pub fn hasErrors(self: *Diagnostics) bool {
        for (self.entries.items) |entry| {
            if (entry.severity == .err) return true;
        }
        return false;
    }

    /// Emit diagnostics to stderr with line and column information.
    pub fn emitAll(self: *Diagnostics, source_map_ref: *source_map.SourceMap) !void {
        var out_buffer = try std.ArrayList(u8).initCapacity(self.allocator, 0);
        defer out_buffer.deinit(self.allocator);

        try self.emitAllToWriter(source_map_ref, out_buffer.writer(self.allocator));
        try std.fs.File.stderr().writeAll(out_buffer.items);
    }

    pub fn emitAllToWriter(self: *Diagnostics, source_map_ref: *source_map.SourceMap, writer: anytype) !void {
        for (self.entries.items) |entry| {
            const file = source_map_ref.getFile(entry.primary_span.file_id) orelse {
                try std.fmt.format(
                    writer,
                    "{s}: <unknown>:{d}-{d}: {s}\n",
                    .{
                        switch (entry.severity) {
                            .err => "error",
                            .warning => "warning",
                        },
                        entry.primary_span.start,
                        entry.primary_span.end,
                        entry.message,
                    },
                );
                continue;
            };

            const start_pos = positionAt(file.contents, entry.primary_span.start);
            const end_pos = positionAt(file.contents, entry.primary_span.end);

            try std.fmt.format(
                writer,
                "{s}: {s}:{d}:{d}-{d}:{d}: {s}\n",
                .{
                    switch (entry.severity) {
                        .err => "error",
                        .warning => "warning",
                    },
                    file.path,
                    start_pos.line,
                    start_pos.column,
                    end_pos.line,
                    end_pos.column,
                    entry.message,
                },
            );

            var line_start: usize = 0;
            while (line_start <= file.contents.len) {
                const line_end = std.mem.indexOfScalarPos(u8, file.contents, line_start, '\n') orelse file.contents.len;
                const line_slice = file.contents[line_start..line_end];

                const overlap_start = @max(entry.primary_span.start, line_start);
                const overlap_end = @min(entry.primary_span.end, line_end);

                if (overlap_start < overlap_end) {
                    try std.fmt.format(writer, "{s}\n", .{line_slice});

                    const caret_start = overlap_start - line_start + 1;
                    const caret_len = @max(overlap_end - overlap_start, @as(usize, 1));

                    try writer.writeByteNTimes(' ', caret_start - 1);
                    try writer.writeByteNTimes('^', caret_len);
                    try writer.writeByte('\n');
                }

                if (line_end == file.contents.len) break;
                line_start = line_end + 1;
            }
        }
    }

    const Position = struct {
        line: usize,
        column: usize,
    };

    fn positionAt(contents: []const u8, offset: usize) Position {
        var line: usize = 1;
        var column: usize = 1;
        var i: usize = 0;
        while (i < offset and i < contents.len) : (i += 1) {
            if (contents[i] == '\n') {
                line += 1;
                column = 1;
            } else {
                column += 1;
            }
        }
        return .{ .line = line, .column = column };
    }
};

test "emitAll formats single-line span with caret" {
    const allocator = std.testing.allocator;

    var sm = source_map.SourceMap.init(allocator);
    defer sm.deinit();

    const contents = "let x = 1;\n";
    const file_id = try sm.addFile("test.rs", contents);

    var diags = Diagnostics.init(allocator);
    defer diags.deinit();

    diags.reportError(.{ .file_id = file_id, .start = 4, .end = 9 }, "unused variable");

    var buffer = try std.ArrayList(u8).initCapacity(allocator, 0);
    defer buffer.deinit(allocator);

    try diags.emitAllToWriter(&sm, buffer.writer(allocator));

    const expected = "error: test.rs:1:5-1:10: unused variable\n" ++
        "let x = 1;\n" ++
        "    ^^^^^\n";

    try std.testing.expectEqualStrings(expected, buffer.items);
}

test "emitAll formats multi-line span with carets per line" {
    const allocator = std.testing.allocator;

    var sm = source_map.SourceMap.init(allocator);
    defer sm.deinit();

    const contents = "aaa\nbbb\nccc\n";
    const file_id = try sm.addFile("test.rs", contents);

    var diags = Diagnostics.init(allocator);
    defer diags.deinit();

    diags.reportError(.{ .file_id = file_id, .start = 1, .end = 6 }, "spanning lines");

    var buffer = try std.ArrayList(u8).initCapacity(allocator, 0);
    defer buffer.deinit(allocator);

    try diags.emitAllToWriter(&sm, buffer.writer(allocator));

    const expected = "error: test.rs:1:2-2:3: spanning lines\n" ++
        "aaa\n" ++
        " ^^\n" ++
        "bbb\n" ++
        "^^\n";

    try std.testing.expectEqualStrings(expected, buffer.items);
}
