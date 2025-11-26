const std = @import("std");
const diag = @import("../../diag/diagnostics.zig");
const mir = @import("../mir.zig");

/// An optimization or analysis pass executed over MIR.
pub const Pass = struct {
    name: []const u8,
    run: *const fn (allocator: std.mem.Allocator, mir_crate: *mir.MirCrate, diagnostics: *diag.Diagnostics) anyerror!void,
};

/// A no-op pass useful for validating the pass pipeline wiring.
pub const noOpPass = Pass{ .name = "noop", .run = noOpRun };

fn noOpRun(allocator: std.mem.Allocator, mir_crate: *mir.MirCrate, diagnostics: *diag.Diagnostics) anyerror!void {
    _ = allocator;
    _ = mir_crate;
    _ = diagnostics;
}

/// Executes all MIR passes in sequence.
pub fn runAll(allocator: std.mem.Allocator, mir_crate: *mir.MirCrate, diagnostics: *diag.Diagnostics) !void {
    const passes = [_]Pass{noOpPass};
    for (passes) |pass| {
        try pass.run(allocator, mir_crate, diagnostics);
    }
}

test "runAll executes the no-op pass" {
    var mir_crate = mir.MirCrate.init(std.testing.allocator);
    defer mir_crate.deinit();

    var diagnostics = diag.Diagnostics.init(std.testing.allocator);
    defer diagnostics.deinit();

    try runAll(std.testing.allocator, &mir_crate, &diagnostics);
    try std.testing.expect(!diagnostics.hasErrors());
}
