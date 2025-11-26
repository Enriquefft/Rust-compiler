const std = @import("std");
const diag = @import("../diag/diagnostics.zig");
const mir = @import("../mir/mir.zig");
const x86_64 = @import("x86_64/codegen.zig");

pub const Artifact = struct {
    allocator: std.mem.Allocator,
    assembly: []const u8,

    pub fn deinit(self: *Artifact) void {
        self.allocator.free(self.assembly);
    }
};

/// Entrypoint for backend codegen. Dispatches to the configured target backend.
pub fn codegen(mir_crate: *const mir.MirCrate, allocator: std.mem.Allocator, diagnostics: *diag.Diagnostics) !Artifact {
    _ = diagnostics; // Target-specific diagnostics would be reported here.
    return x86_64.codegen(mir_crate, allocator);
}

test "backend returns placeholder assembly" {
    var mir_crate = mir.MirCrate.init(std.testing.allocator);
    defer mir_crate.deinit();

    var diagnostics = diag.Diagnostics.init(std.testing.allocator);
    defer diagnostics.deinit();

    var artifact = try codegen(&mir_crate, std.testing.allocator, &diagnostics);
    defer artifact.deinit();

    try std.testing.expect(artifact.assembly.len > 0);
}
