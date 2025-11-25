const std = @import("std");
const diag = @import("../diag/diagnostics.zig");
const hir = @import("hir.zig");

pub fn typecheck(crate: *hir.Crate, diagnostics: *diag.Diagnostics) !void {
    _ = crate;
    _ = diagnostics;
    // The typechecker will walk HIR expressions and attach concrete types.
    // Placeholder ensures later stages can import the module without failing.
}

test "typechecker placeholder does not panic" {
    const allocator = std.testing.allocator;
    var diagnostics = diag.Diagnostics.init(allocator);
    defer diagnostics.deinit();

    var crate = hir.Crate.init(allocator);
    defer crate.deinit();

    try typecheck(&crate, &diagnostics);
    try std.testing.expect(!diagnostics.hasErrors());
}
