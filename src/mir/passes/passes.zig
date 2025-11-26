const std = @import("std");
const diag = @import("../../diag/diagnostics.zig");
const mir = @import("../mir.zig");
const constant_folding = @import("constant_folding.zig");
const dead_code_elim = @import("dead_code_elimination.zig");
const cfg_simplify = @import("cfg_simplify.zig");

/// An optimization or analysis pass executed over MIR.
pub const Pass = struct {
    name: []const u8,
    run: *const fn (allocator: std.mem.Allocator, mir_crate: *mir.MirCrate, diagnostics: *diag.Diagnostics) anyerror!void,
};

/// A no-op pass useful for validating the pass pipeline wiring.
pub const noOpPass = Pass{ .name = "noop", .run = noOpRun };
pub const constantFoldingPass = constant_folding.pass;
pub const deadCodeEliminationPass = dead_code_elim.pass;
pub const cfgSimplifyPass = cfg_simplify.pass;

fn noOpRun(allocator: std.mem.Allocator, mir_crate: *mir.MirCrate, diagnostics: *diag.Diagnostics) anyerror!void {
    _ = allocator;
    _ = mir_crate;
    _ = diagnostics;
}

/// Executes all MIR passes in sequence.
pub fn runAll(allocator: std.mem.Allocator, mir_crate: *mir.MirCrate, diagnostics: *diag.Diagnostics) !void {
    const passes = [_]Pass{ noOpPass, constantFoldingPass, deadCodeEliminationPass, cfgSimplifyPass };
    for (passes) |pass| {
        try pass.run(allocator, mir_crate, diagnostics);
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

    try runAll(std.testing.allocator, &mir_crate, &diagnostics);
    try std.testing.expect(!diagnostics.hasErrors());

    const func = mir_crate.fns.items[0];
    try std.testing.expectEqual(@as(usize, 1), func.blocks.len);
    try std.testing.expectEqual(@as(usize, 1), func.blocks[0].insts.len);
    try std.testing.expect(func.blocks[0].insts[0].kind == .Copy);
    try std.testing.expectEqual(@as(i64, 5), func.blocks[0].insts[0].kind.Copy.src.ImmInt);
}
