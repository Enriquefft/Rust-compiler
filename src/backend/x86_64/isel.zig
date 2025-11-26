const std = @import("std");
const diag = @import("../../diag/diagnostics.zig");
const source_map = @import("../../diag/source_map.zig");
const mir = @import("../../mir/mir.zig");
const machine = @import("machine.zig");

const zero_span = source_map.Span{ .file_id = 0, .start = 0, .end = 0 };

pub const LowerError = error{ Unsupported, OutOfMemory };

pub fn lowerCrate(allocator: std.mem.Allocator, mir_crate: *const mir.MirCrate, diagnostics: *diag.Diagnostics) LowerError!machine.MachineCrate {
    var fns = std.ArrayListUnmanaged(machine.MachineFn){};
    defer fns.deinit(allocator);

    for (mir_crate.fns.items) |func| {
        const lowered = try lowerFn(allocator, func, diagnostics) catch |err| {
            switch (err) {
                error.Unsupported => diagnostics.reportError(zero_span, "unsupported MIR construct in x86_64 lowering"),
                else => {},
            }
            return err;
        };
        try fns.append(allocator, lowered);
    }

    return .{ .allocator = allocator, .fns = try fns.toOwnedSlice(allocator) };
}

fn lowerFn(allocator: std.mem.Allocator, func: mir.MirFn, diagnostics: *diag.Diagnostics) LowerError!machine.MachineFn {
    var blocks = std.ArrayListUnmanaged(machine.MachineBlock){};
    defer blocks.deinit(allocator);

    var vreg_count: machine.VReg = 0;

    for (func.blocks, 0..) |block, idx| {
        var insts = std.ArrayListUnmanaged(machine.InstKind){};
        defer insts.deinit(allocator);

        for (block.insts) |inst| {
            if (try lowerInst(allocator, inst, &insts, diagnostics, &vreg_count)) |_| {} else {
                // no-op
            }
        }

        try lowerTerm(block.term, &insts, diagnostics, &vreg_count, allocator);

        try blocks.append(allocator, .{
            .id = @intCast(idx),
            .insts = try insts.toOwnedSlice(allocator),
        });
    }

    const stack_size = func.locals.len * @sizeOf(i64);

    return .{
        .name = func.name,
        .blocks = try blocks.toOwnedSlice(allocator),
        .stack_size = stack_size,
        .vreg_count = vreg_count,
    };
}

fn lowerInst(
    allocator: std.mem.Allocator,
    inst: mir.Inst,
    insts: *std.ArrayListUnmanaged(machine.InstKind),
    diagnostics: *diag.Diagnostics,
    vreg_count: *machine.VReg,
) LowerError!?void {
    const dest_vreg = if (inst.dest) |tmp| destRegister(tmp, vreg_count) else null;

    switch (inst.kind) {
        .Copy => |payload| {
            if (dest_vreg) |dst| {
                const src = try lowerOperand(payload.src, vreg_count, diagnostics);
                try insts.append(allocator, .{ .Mov = .{ .dst = .{ .VReg = dst }, .src = src } });
            }
        },
        .Bin => |payload| {
            if (dest_vreg) |dst| {
                const lhs = try lowerOperand(payload.lhs, vreg_count, diagnostics);
                const rhs = try lowerOperand(payload.rhs, vreg_count, diagnostics);
                const op = switch (payload.op) {
                    .Add => machine.BinOpcode.add,
                    .Sub => machine.BinOpcode.sub,
                    .Mul => machine.BinOpcode.imul,
                    .Div => machine.BinOpcode.idiv,
                    .Mod => machine.BinOpcode.imod,
                    .And => machine.BinOpcode.and_,
                    .Or => machine.BinOpcode.or_,
                    .Xor => machine.BinOpcode.xor_,
                };
                try insts.append(allocator, .{ .Mov = .{ .dst = .{ .VReg = dst }, .src = lhs } });
                try insts.append(allocator, .{ .Bin = .{ .op = op, .dst = .{ .VReg = dst }, .lhs = .{ .VReg = dst }, .rhs = rhs } });
            }
        },
        .Cmp => |payload| {
            if (dest_vreg) |dst| {
                const lhs = try lowerOperand(payload.lhs, vreg_count, diagnostics);
                const rhs = try lowerOperand(payload.rhs, vreg_count, diagnostics);
                try insts.append(allocator, .{ .Cmp = .{ .lhs = lhs, .rhs = rhs } });
                try insts.append(allocator, .{ .Setcc = .{ .cond = mapCond(payload.op), .dst = .{ .VReg = dst } } });
            }
        },
        .Unary => |payload| {
            if (dest_vreg) |dst| {
                const src = try lowerOperand(payload.operand, vreg_count, diagnostics);
                const op = switch (payload.op) {
                    .Not => machine.UnaryOpcode.not_,
                    .Neg => machine.UnaryOpcode.neg,
                };
                try insts.append(allocator, .{ .Mov = .{ .dst = .{ .VReg = dst }, .src = src } });
                try insts.append(allocator, .{ .Unary = .{ .op = op, .dst = .{ .VReg = dst }, .src = .{ .VReg = dst } } });
            }
        },
        .LoadLocal => |payload| {
            if (dest_vreg) |dst| {
                const mem = localMem(payload.local);
                try insts.append(allocator, .{ .Mov = .{ .dst = .{ .VReg = dst }, .src = mem } });
            }
        },
        .StoreLocal => |payload| {
            const mem = localMem(payload.local);
            const src = try lowerOperand(payload.src, vreg_count, diagnostics);
            try insts.append(allocator, .{ .Mov = .{ .dst = mem, .src = src } });
        },
        else => {
            diagnostics.reportError(zero_span, "unsupported MIR instruction in x86_64 lowering");
            return error.Unsupported;
        },
    }

    return {};
}

fn lowerTerm(
    term: mir.TermKind,
    insts: *std.ArrayListUnmanaged(machine.InstKind),
    diagnostics: *diag.Diagnostics,
    vreg_count: *machine.VReg,
    allocator: std.mem.Allocator,
) LowerError!void {
    switch (term) {
        .Goto => |target| try insts.append(allocator, .{ .Jmp = target }),
        .If => |payload| {
            const cond_op = lowerSimpleOperand(payload.cond, vreg_count, diagnostics) catch |err| switch (err) {
                error.Unsupported => return err,
                else => return err,
            };
            try insts.append(allocator, .{ .Test = .{ .operand = cond_op } });
            try insts.append(allocator, .{ .Jcc = .{ .cond = .ne, .target = payload.then_block } });
            try insts.append(allocator, .{ .Jmp = payload.else_block });
        },
        .Ret => |maybe_op| {
            if (maybe_op) |op| {
                const lowered = lowerSimpleOperand(op, vreg_count, diagnostics) catch |err| return err;
                try insts.append(allocator, .{ .Mov = .{ .dst = .{ .Phys = .rax }, .src = lowered } });
            }
            try insts.append(allocator, .{ .Ret = null });
        },
    }
}

fn lowerOperand(op: mir.Operand, vreg_count: *machine.VReg, diagnostics: *diag.Diagnostics) LowerError!machine.MOperand {
    return lowerSimpleOperand(op, vreg_count, diagnostics) catch |err| switch (err) {
        error.Unsupported => {
            diagnostics.reportError(zero_span, "unsupported operand in x86_64 lowering");
            return err;
        },
        else => return err,
    };
}

fn lowerSimpleOperand(op: mir.Operand, vreg_count: ?*machine.VReg, diagnostics: *diag.Diagnostics) LowerError!machine.MOperand {
    return switch (op) {
        .Temp => |tmp| .{ .VReg = destRegister(tmp, vreg_count) },
        .Local => |local| localMem(local),
        .ImmInt => |v| .{ .Imm = v },
        .ImmBool => |v| .{ .Imm = if (v) 1 else 0 },
        else => {
            diagnostics.reportError(zero_span, "unsupported operand kind for x86_64 lowering");
            return error.Unsupported;
        },
    };
}

fn destRegister(temp: mir.TempId, vreg_count: ?*machine.VReg) machine.VReg {
    const id: machine.VReg = @intCast(temp);
    if (vreg_count) |count| {
        if (id + 1 > count.*) count.* = id + 1;
    }
    return id;
}

fn localMem(local: mir.LocalId) machine.MOperand {
    const offset: i32 = -@as(i32, @intCast((local + 1) * @sizeOf(i64)));
    return .{ .Mem = .{ .base = .rbp, .offset = offset } };
}

fn mapCond(op: mir.CmpOp) machine.Condition {
    return switch (op) {
        .Eq => .eq,
        .Ne => .ne,
        .Lt => .lt,
        .Le => .le,
        .Gt => .gt,
        .Ge => .ge,
    };
}
