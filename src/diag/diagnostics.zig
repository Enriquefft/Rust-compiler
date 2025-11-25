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

    /// Emit diagnostics to stderr. The current implementation prints byte offsets;
    /// a richer renderer can be added when line mapping is implemented.
    pub fn emitAll(self: *Diagnostics, source_map_ref: *source_map.SourceMap) !void {
        var writer = std.io.getStdErr().writer();
        for (self.entries.items) |entry| {
            const file = source_map_ref.getFile(entry.primary_span.file_id);
            const path = if (file) |f| f.path else "<unknown>";
            try writer.print("{s}: {s}:{d}-{d}: {s}\n", .{
                switch (entry.severity) {
                    .err => "error",
                    .warning => "warning",
                },
                path,
                entry.primary_span.start,
                entry.primary_span.end,
                entry.message,
            });
        }
    }
};
