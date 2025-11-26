const std = @import("std");
const mir = @import("../../mir/mir.zig");
const backend = @import("../backend.zig");
const diag = @import("../../diag/diagnostics.zig");
const isel = @import("isel.zig");
const regalloc = @import("regalloc.zig");
const emitter = @import("emitter.zig");

/// Generates x86_64 assembly from MIR.
pub fn codegen(mir_crate: *const mir.MirCrate, allocator: std.mem.Allocator, diagnostics: *diag.Diagnostics) !backend.Artifact {
    var machine_crate = try isel.lowerCrate(allocator, mir_crate, diagnostics);
    defer machine_crate.deinit();

    for (machine_crate.fns) |*func| {
        try regalloc.allocateRegisters(func, allocator, diagnostics);
    }

    const assembly = try emitter.emitAssembly(allocator, &machine_crate);

    return .{ .allocator = allocator, .assembly = assembly };
}
