//! MIR debug dump pass.
//!
//! This pass prints the current state of the MIR crate to standard debug output.
//! It is useful for debugging the compiler pipeline and inspecting MIR after
//! each optimization pass.
//!
//! The output includes function signatures, local variable types, block contents,
//! instructions with their operands, and block terminators.

const std = @import("std");
const mir = @import("../mir.zig");
const diag = @import("../../diag/diagnostics.zig");
const Pass = @import("passes.zig").Pass;

/// Debug pass that prints MIR state to debug output.
pub const pass = Pass{
    .name = "mir-debug-dump",
    .run = run,
};

/// Execute the debug dump, printing all functions in the crate.
fn run(allocator: std.mem.Allocator, mir_crate: *mir.MirCrate, diagnostics: *diag.Diagnostics) !void {
    _ = allocator;
    _ = diagnostics;

    const stdout = std.debug;

    // Iterate over all functions in the crate
    for (mir_crate.fns.items, 0..) |*func, fn_index| {
        stdout.print("=== MIR fn {d}: {s} ===\n", .{ fn_index, func.name });
        stdout.print("  return type: {any}\n", .{func.ret_ty});

        // Print local variable types
        stdout.print("  locals ({d}):\n", .{func.locals.len});
        for (func.locals, 0..) |ty, local_idx| {
            stdout.print("    L{d}: {any}\n", .{ local_idx, ty });
        }

        // Print each basic block with its instructions and terminator
        for (func.blocks, 0..) |*block, b_idx| {
            stdout.print("\n  block {d}:\n", .{b_idx});

            for (block.insts, 0..) |*inst, i_idx| {
                stdout.print(
                    "    inst {d}: dest={any} ty={any} kind={any}\n",
                    .{ i_idx, inst.dest, inst.ty, inst.kind },
                );
            }

            stdout.print("    term: {any}\n", .{block.term});
        }

        stdout.print("\n", .{});
    }
}
