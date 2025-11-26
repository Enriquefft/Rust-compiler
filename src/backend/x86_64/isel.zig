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
        const lowered = lowerFn(allocator, func, mir_crate, diagnostics) catch |err| {
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

fn lowerFn(
    allocator: std.mem.Allocator,
    func: mir.MirFn,
    mir_crate: *const mir.MirCrate,
    diagnostics: *diag.Diagnostics,
) LowerError!machine.MachineFn {
    var blocks = std.ArrayListUnmanaged(machine.MachineBlock){};
    defer blocks.deinit(allocator);

    var vreg_count: machine.VReg = 0;

    for (func.blocks, 0..) |block, idx| {
        var insts = std.ArrayListUnmanaged(machine.InstKind){};
        defer insts.deinit(allocator);

        for (block.insts) |inst| {
            if (try lowerInst(allocator, inst, &insts, diagnostics, &vreg_count, mir_crate)) |_| {} else {
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
    mir_crate: *const mir.MirCrate,
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
        .Call => |payload| {
            const target_name = resolveCallTarget(payload.target, mir_crate, diagnostics) catch |err| return err;

            const arg_registers = [_]machine.PhysReg{ .rdi, .rsi, .rdx, .rcx, .r8, .r9 };
            for (payload.args, 0..) |arg, idx| {
                if (idx >= arg_registers.len) break; // simplistic handling: ignore extra args for now
                const lowered = try lowerOperand(arg, vreg_count, diagnostics);
                try insts.append(allocator, .{ .Mov = .{ .dst = .{ .Phys = arg_registers[idx] }, .src = lowered } });
            }

            try insts.append(allocator, .{ .Call = target_name });

            if (dest_vreg) |dst| {
                try insts.append(allocator, .{ .Mov = .{ .dst = .{ .VReg = dst }, .src = .{ .Phys = .rax } } });
            }
        },
        .Range => |payload| {
            if (dest_vreg) |dst| {
                const start = try lowerOperand(payload.start, vreg_count, diagnostics);
                _ = try lowerOperand(payload.end, vreg_count, diagnostics);
                try insts.append(allocator, .{ .Mov = .{ .dst = .{ .VReg = dst }, .src = start } });
            }
        },
        .Index => |payload| {
            if (dest_vreg) |dst| {
                const target = try lowerOperand(payload.target, vreg_count, diagnostics);
                const idx = try lowerOperand(payload.index, vreg_count, diagnostics);
                const mem = switch (target) {
                    .Mem => |base_mem| blk: {
                        const offset = switch (idx) {
                            .Imm => |imm| blk2: {
                                const scaled: i32 = @intCast(imm * @as(i64, @intCast(@sizeOf(i64))));
                                break :blk2 base_mem.offset + scaled;
                            },
                            else => {
                                diagnostics.reportError(zero_span, "indexing currently supports only immediate indices");
                                return error.Unsupported;
                            },
                        };
                        break :blk machine.MOperand{ .Mem = .{ .base = base_mem.base, .offset = offset } };
                    },
                    else => {
                        diagnostics.reportError(zero_span, "unsupported index base for x86_64 lowering");
                        return error.Unsupported;
                    },
                };
                try insts.append(allocator, .{ .Mov = .{ .dst = .{ .VReg = dst }, .src = mem } });
            }
        },
        .Field => |payload| {
            if (dest_vreg) |dst| {
                const target = try lowerOperand(payload.target, vreg_count, diagnostics);
                const mem = switch (target) {
                    .Mem => |base_mem| blk: {
                        // Basic field layout: assume packed i64 fields and use a deterministic pseudo-offset based on the name.
                        var hash: u32 = 0;
                        for (payload.name) |ch| hash = hash * 31 + ch;
                        const field_index: i32 = @intCast(hash % 4); // limit offset growth
                        const offset = base_mem.offset + field_index * @as(i32, @intCast(@sizeOf(i64)));
                        break :blk machine.MOperand{ .Mem = .{ .base = base_mem.base, .offset = offset } };
                    },
                    else => {
                        diagnostics.reportError(zero_span, "unsupported field base for x86_64 lowering");
                        return error.Unsupported;
                    },
                };
                try insts.append(allocator, .{ .Mov = .{ .dst = .{ .VReg = dst }, .src = mem } });
            }
        },
        .Array => |payload| {
            if (dest_vreg) |dst| {
                if (payload.elems.len > 0) {
                    const first = try lowerOperand(payload.elems[0], vreg_count, diagnostics);
                    try insts.append(allocator, .{ .Mov = .{ .dst = .{ .VReg = dst }, .src = first } });
                } else {
                    try insts.append(allocator, .{ .Mov = .{ .dst = .{ .VReg = dst }, .src = .{ .Imm = 0 } } });
                }
            }
        },
        .StructInit => |payload| {
            if (dest_vreg) |dst| {
                if (payload.fields.len > 0) {
                    const first = try lowerOperand(payload.fields[0].value, vreg_count, diagnostics);
                    try insts.append(allocator, .{ .Mov = .{ .dst = .{ .VReg = dst }, .src = first } });
                } else {
                    try insts.append(allocator, .{ .Mov = .{ .dst = .{ .VReg = dst }, .src = .{ .Imm = 0 } } });
                }
            }
        }
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
        .ImmFloat => |v| .{ .Imm = @bitCast(v) },
        .ImmString => |_| .{ .Imm = 0 },
        .Global => |id| .{ .Imm = @intCast(id) },
        else => {
            diagnostics.reportError(zero_span, "unsupported operand kind for x86_64 lowering");
            return error.Unsupported;
        },
    };
}

fn resolveCallTarget(target: mir.Operand, mir_crate: *const mir.MirCrate, diagnostics: *diag.Diagnostics) LowerError![]const u8 {
    return switch (target) {
        .Global => |id| blk: {
            if (id >= mir_crate.fns.items.len) {
                diagnostics.reportError(zero_span, "call target refers to unknown function id");
                return error.Unsupported;
            }
            break :blk mir_crate.fns.items[id].name;
        },
        else => {
            diagnostics.reportError(zero_span, "only direct function calls are supported in x86_64 lowering");
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
