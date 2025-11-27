const std = @import("std");
const mir = @import("../mir.zig");
const diag = @import("../../diag/diagnostics.zig");
const Pass = @import("passes.zig").Pass;

pub const pass = Pass{
    .name = "mir-debug-dump",
    .run = run,
};

fn run(allocator: std.mem.Allocator, mir_crate: *mir.MirCrate, diagnostics: *diag.Diagnostics) !void {
    _ = allocator;
    _ = diagnostics;

    const stdout = std.debug;

//hope
    for (mir_crate.fns.items, 0..) |*func, fn_index| {
        stdout.print("=== MIR fn {d}: {s} ===\n", .{ fn_index, func.name });
        stdout.print("  return type: {any}\n", .{ func.ret_ty });

        stdout.print("  locals ({d}):\n", .{ func.locals.len });
        for (func.locals, 0..) |ty, local_idx| {
            stdout.print("    L{d}: {any}\n", .{ local_idx, ty });
        }

        // targ + op
        for (func.blocks, 0..) |*block, b_idx| {
            stdout.print("\n  block {d}:\n", .{ b_idx });

            for (block.insts, 0..) |*inst, i_idx| {

                stdout.print(
                    "    inst {d}: dest={any} ty={any} kind={any}\n",
                    .{ i_idx, inst.dest, inst.ty, inst.kind },
                );
            }

            stdout.print("    term: {any}\n", .{ block.term });
        }

        stdout.print("\n", .{});
    }
}
