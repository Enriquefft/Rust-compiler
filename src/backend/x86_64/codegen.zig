const std = @import("std");
const mir = @import("../../mir/mir.zig");
const backend = @import("../backend.zig");

/// Generates placeholder x86_64 assembly from MIR.
pub fn codegen(mir_crate: *const mir.MirCrate, allocator: std.mem.Allocator) !backend.Artifact {
    var buffer = try std.ArrayList(u8).initCapacity(allocator, 0);
    defer buffer.deinit(allocator);

    var writer = buffer.writer(allocator);
    try writer.writeAll("; x86_64 assembly placeholder\n");
    try writer.print("; function count: {d}\n", .{mir_crate.fns.items.len});

    return .{
        .allocator = allocator,
        .assembly = try buffer.toOwnedSlice(allocator),
    };
}
