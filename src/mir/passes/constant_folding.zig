const std = @import("std");
const mir = @import("../mir.zig");
const diag = @import("../../diag/diagnostics.zig");
const Pass = @import("passes.zig").Pass;

/// Evaluates constant expressions and replaces them with immediates.
pub const pass = Pass{ .name = "constant-folding", .run = run };

fn run(allocator: std.mem.Allocator, mir_crate: *mir.MirCrate, diagnostics: *diag.Diagnostics) !void {
    _ = allocator;
    _ = diagnostics;

    for (mir_crate.fns.items) |*func| {
        for (func.blocks) |*block| {
            for (block.insts) |*inst| {
                _ = foldInst(inst);
            }
        }
    }
}

fn foldInst(inst: *mir.Inst) bool {
    switch (inst.kind) {
        .Bin => |bin| {
            if (foldBinary(bin.op, bin.lhs, bin.rhs)) |result| {
                inst.kind = .{ .Copy = .{ .src = result } };
                return true;
            }
        },
        .Cmp => |cmp| {
            if (foldCmp(cmp.op, cmp.lhs, cmp.rhs)) |result| {
                inst.kind = .{ .Copy = .{ .src = .{ .ImmBool = result } } };
                inst.ty = .Bool;
                return true;
            }
        },
        .Unary => |un| {
            if (foldUnary(un.op, un.operand)) |result| {
                inst.kind = .{ .Copy = .{ .src = result } };
                return true;
            }
        },
        else => {},
    }
    return false;
}

fn foldBinary(op: mir.BinOp, lhs: mir.Operand, rhs: mir.Operand) ?mir.Operand {
    switch (lhs) {
        .ImmInt => |lhs_val| switch (rhs) {
            .ImmInt => |rhs_val| {
                const result: i64 = switch (op) {
                    .Add => lhs_val + rhs_val,
                    .Sub => lhs_val - rhs_val,
                    .Mul => lhs_val * rhs_val,
                    .Div => if (rhs_val == 0) return null else @divTrunc(lhs_val, rhs_val),
                    .Mod => if (rhs_val == 0) return null else @rem(lhs_val, rhs_val),
                    else => return null,
                };
                return .{ .ImmInt = result };
            },
            else => {},
        },
        .ImmBool => |lhs_val| switch (rhs) {
            .ImmBool => |rhs_val| {
                const result: bool = switch (op) {
                    .And => lhs_val and rhs_val,
                    .Or => lhs_val or rhs_val,
                    .Xor => lhs_val != rhs_val,
                    else => return null,
                };
                return .{ .ImmBool = result };
            },
            else => {},
        },
        else => {},
    }
    return null;
}

fn foldCmp(op: mir.CmpOp, lhs: mir.Operand, rhs: mir.Operand) ?bool {
    switch (lhs) {
        .ImmInt => |lhs_val| switch (rhs) {
            .ImmInt => |rhs_val| return switch (op) {
                .Eq => lhs_val == rhs_val,
                .Ne => lhs_val != rhs_val,
                .Lt => lhs_val < rhs_val,
                .Le => lhs_val <= rhs_val,
                .Gt => lhs_val > rhs_val,
                .Ge => lhs_val >= rhs_val,
            },
            else => {},
        },
        .ImmBool => |lhs_val| switch (rhs) {
            .ImmBool => |rhs_val| return switch (op) {
                .Eq => lhs_val == rhs_val,
                .Ne => lhs_val != rhs_val,
                else => return null,
            },
            else => {},
        },
        else => {},
    }
    return null;
}

fn foldUnary(op: mir.UnaryOp, operand: mir.Operand) ?mir.Operand {
    return switch (operand) {
        .ImmBool => |val| switch (op) {
            .Not => .{ .ImmBool = !val },
            else => null,
        },
        .ImmInt => |val| switch (op) {
            .Neg => .{ .ImmInt = -val },
            else => null,
        },
        else => null,
    };
}

test "constant folding rewrites binary operations" {
    var mir_crate = mir.MirCrate.init(std.testing.allocator);
    defer mir_crate.deinit();

    var insts = std.ArrayListUnmanaged(mir.Inst){};
    defer insts.deinit(mir_crate.allocator());
    try insts.append(mir_crate.allocator(), .{
        .ty = .I32,
        .dest = 0,
        .kind = .{ .Bin = .{ .op = .Add, .lhs = .{ .ImmInt = 2 }, .rhs = .{ .ImmInt = 3 } } },
    });

    const block = mir.Block{
        .insts = try insts.toOwnedSlice(mir_crate.allocator()),
        .term = .{ .Ret = .{ .Temp = 0 } },
    };

    var blocks = std.ArrayListUnmanaged(mir.Block){};
    defer blocks.deinit(mir_crate.allocator());
    try blocks.append(mir_crate.allocator(), block);

    try mir_crate.fns.append(mir_crate.allocator(), .{
        .name = "add",
        .params = &[_]mir.LocalId{},
        .locals = &[_]mir.MirType{},
        .ret_ty = .I32,
        .blocks = try blocks.toOwnedSlice(mir_crate.allocator()),
    });

    var diagnostics = diag.Diagnostics.init(std.testing.allocator);
    defer diagnostics.deinit();

    try run(std.testing.allocator, &mir_crate, &diagnostics);
    try std.testing.expect(!diagnostics.hasErrors());

    const folded = mir_crate.fns.items[0].blocks[0].insts[0];
    try std.testing.expect(folded.kind == .Copy);
    try std.testing.expectEqual(@as(i64, 5), folded.kind.Copy.src.ImmInt);
}
