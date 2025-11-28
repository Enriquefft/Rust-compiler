//! MIR optimization pass infrastructure.
//!
//! This module defines the pass interface and provides a registry of available
//! optimization passes. Passes are executed in sequence to transform and optimize
//! MIR before code generation.
//!
//! Available passes:
//! - `noOpPass`: No-op pass for testing pipeline wiring
//! - `constantFoldingPass`: Evaluates constant expressions at compile time
//! - `deadCodeEliminationPass`: Removes unused computations
//! - `cfgSimplifyPass`: Removes unreachable blocks and simplifies control flow
//! - `debugDumpPass`: Prints MIR for debugging purposes

const std = @import("std");
const diag = @import("../../diag/diagnostics.zig");
const mir = @import("../mir.zig");
const constant_folding = @import("constant_folding.zig");
const dead_code_elim = @import("dead_code_elimination.zig");
const cfg_simplify = @import("cfg_simplify.zig");
const debug_dump = @import("debug_dump.zig");

/// Debug dump pass for printing MIR state.
pub const debugDumpPass = debug_dump.pass;

/// An optimization or analysis pass executed over MIR.
/// Each pass has a name for identification and a run function that transforms the crate.
pub const Pass = struct {
    /// Human-readable name for logging and debugging
    name: []const u8,
    /// Function to execute the pass, transforming the MIR crate in place
    run: *const fn (allocator: std.mem.Allocator, mir_crate: *mir.MirCrate, diagnostics: *diag.Diagnostics) anyerror!void,
};

/// A no-op pass useful for validating the pass pipeline wiring.
pub const noOpPass = Pass{ .name = "noop", .run = noOpRun };

/// Constant folding pass: evaluates constant expressions at compile time.
pub const constantFoldingPass = constant_folding.pass;

/// Dead code elimination pass: removes unused computations.
pub const deadCodeEliminationPass = dead_code_elim.pass;

/// CFG simplification pass: removes unreachable blocks and simplifies control flow.
pub const cfgSimplifyPass = cfg_simplify.pass;

fn noOpRun(allocator: std.mem.Allocator, mir_crate: *mir.MirCrate, diagnostics: *diag.Diagnostics) anyerror!void {
    _ = allocator;
    _ = mir_crate;
    _ = diagnostics;
}

/// Run all optimization passes in sequence on the given MIR crate.
///
/// Pass order: noop -> constant folding -> dead code elimination -> CFG simplification
///
/// If `dump_passes` is true, prints MIR state after each pass for debugging.
pub fn runAll(allocator: std.mem.Allocator, mir_crate: *mir.MirCrate, diagnostics: *diag.Diagnostics, dump_passes: bool) !void {
    const passes = [_]Pass{
        noOpPass,
        constantFoldingPass,
        deadCodeEliminationPass,
        cfgSimplifyPass,
    };
    for (passes) |pass| {
        try pass.run(allocator, mir_crate, diagnostics);
        if (dump_passes) {
            try debug_dump.pass.run(allocator, mir_crate, diagnostics);
        }
    }
}

test "runAll sequences configured passes" {
    var mir_crate = mir.MirCrate.init(std.testing.allocator);
    defer mir_crate.deinit();

    var insts = std.ArrayListUnmanaged(mir.Inst){};
    defer insts.deinit(mir_crate.allocator());
    try insts.append(mir_crate.allocator(), .{ .ty = .I32, .dest = 0, .kind = .{ .Bin = .{ .op = .Add, .lhs = .{ .ImmInt = 1 }, .rhs = .{ .ImmInt = 2 } } } });
    try insts.append(mir_crate.allocator(), .{ .ty = .I32, .dest = 1, .kind = .{ .Bin = .{ .op = .Add, .lhs = .{ .ImmInt = 4 }, .rhs = .{ .ImmInt = 1 } } } });

    const block = mir.Block{
        .insts = try insts.toOwnedSlice(mir_crate.allocator()),
        .term = .{ .Ret = .{ .Temp = 1 } },
    };

    var blocks = std.ArrayListUnmanaged(mir.Block){};
    defer blocks.deinit(mir_crate.allocator());
    try blocks.append(mir_crate.allocator(), block);
    try blocks.append(mir_crate.allocator(), .{ .insts = &[_]mir.Inst{}, .term = .{ .Ret = null } });

    try mir_crate.fns.append(mir_crate.allocator(), .{
        .name = "pipeline",
        .params = &[_]mir.LocalId{},
        .locals = &[_]mir.MirType{},
        .ret_ty = .I32,
        .blocks = try blocks.toOwnedSlice(mir_crate.allocator()),
    });

    var diagnostics = diag.Diagnostics.init(std.testing.allocator);
    defer diagnostics.deinit();

    try runAll(std.testing.allocator, &mir_crate, &diagnostics, false);
    try std.testing.expect(!diagnostics.hasErrors());

    const func = mir_crate.fns.items[0];
    try std.testing.expectEqual(@as(usize, 1), func.blocks.len);
    try std.testing.expectEqual(@as(usize, 1), func.blocks[0].insts.len);
    try std.testing.expect(func.blocks[0].insts[0].kind == .Copy);
    try std.testing.expectEqual(@as(i64, 5), func.blocks[0].insts[0].kind.Copy.src.ImmInt);
}
