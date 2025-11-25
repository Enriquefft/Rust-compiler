const std = @import("std");
const diag = @import("../diag/diagnostics.zig");
const hir = @import("hir.zig");

pub fn resolve(crate: *hir.Crate, diagnostics: *diag.Diagnostics) !void {
    _ = crate;
    _ = diagnostics;
    // Name resolution will populate symbol tables and attach DefIds to references.
    // Stub implementation intentionally left minimal for incremental bring-up.
}

test "name resolution placeholder does not panic" {
    const allocator = std.testing.allocator;
    var diagnostics = diag.Diagnostics.init(allocator);
    defer diagnostics.deinit();

    var crate = hir.Crate.init(allocator);
    defer crate.deinit();

    try resolve(&crate, &diagnostics);
    try std.testing.expect(!diagnostics.hasErrors());
}
