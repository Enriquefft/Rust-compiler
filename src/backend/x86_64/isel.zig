const std = @import("std");
const diag = @import("../../diag/diagnostics.zig");
const source_map = @import("../../diag/source_map.zig");
const mir = @import("../../mir/mir.zig");
const machine = @import("machine.zig");

const zero_span = source_map.Span{ .file_id = 0, .start = 0, .end = 0 };

pub const LowerError = error{ Unsupported, OutOfMemory };

const DataTable = struct {
    allocator: std.mem.Allocator,
    items: std.ArrayListUnmanaged(machine.DataItem) = .{},

    fn init(allocator: std.mem.Allocator) DataTable {
        return .{ .allocator = allocator };
    }

    fn deinit(self: *DataTable) void {
        for (self.items.items) |item| {
            self.allocator.free(item.label);
            self.allocator.free(item.bytes);
        }
        self.items.deinit(self.allocator);
    }

    fn internString(self: *DataTable, value: []const u8) ![]const u8 {
        for (self.items.items) |item| {
            if (std.mem.eql(u8, item.bytes, value)) return item.label;
        }

        const label = try std.fmt.allocPrint(self.allocator, ".Lstr{d}", .{self.items.items.len});
        const copied = try self.allocator.dupe(u8, value);
        try self.items.append(self.allocator, .{ .label = label, .bytes = copied });
        return label;
    }
};

const LowerContext = struct {
    allocator: std.mem.Allocator,
    diagnostics: *diag.Diagnostics,
    mir_crate: *const mir.MirCrate,
    data: *DataTable,
    uses_printf: bool = false,
};

pub fn lowerCrate(allocator: std.mem.Allocator, mir_crate: *const mir.MirCrate, diagnostics: *diag.Diagnostics) LowerError!machine.MachineCrate {
    var fns = std.ArrayListUnmanaged(machine.MachineFn){};
    defer fns.deinit(allocator);

    var data = DataTable.init(allocator);
    errdefer data.deinit();

    var ctx = LowerContext{
        .allocator = allocator,
        .diagnostics = diagnostics,
        .mir_crate = mir_crate,
        .data = &data,
    };

    for (mir_crate.fns.items) |func| {
        const lowered = lowerFn(&ctx, func) catch |err| {
            switch (err) {
                error.Unsupported => diagnostics.reportError(zero_span, "unsupported MIR construct in x86_64 lowering"),
                else => {},
            }
            return err;
        };
        try fns.append(allocator, lowered);
    }

    var externs = std.ArrayListUnmanaged([]const u8){};
    defer externs.deinit(allocator);

    if (ctx.uses_printf) {
        try externs.append(allocator, try allocator.dupe(u8, "printf"));
    }

    const rodata = try data.items.toOwnedSlice(allocator);
    data.items.deinit(allocator);

    return .{
        .allocator = allocator,
        .fns = try fns.toOwnedSlice(allocator),
        .rodata = rodata,
        .externs = try externs.toOwnedSlice(allocator),
    };
}

fn lowerFn(ctx: *LowerContext, func: mir.MirFn) LowerError!machine.MachineFn {
    var blocks = std.ArrayListUnmanaged(machine.MachineBlock){};
    defer blocks.deinit(ctx.allocator);

    var vreg_count: machine.VReg = 0;

    for (func.blocks, 0..) |block, idx| {
        var insts = std.ArrayListUnmanaged(machine.InstKind){};
        defer insts.deinit(ctx.allocator);

        for (block.insts) |inst| {
            if (try lowerInst(ctx, inst, &insts, &vreg_count)) |_| {} else {
                // no-op
            }
        }

        try lowerTerm(ctx, block.term, &insts, &vreg_count);

        try blocks.append(ctx.allocator, .{
            .id = @intCast(idx),
            .insts = try insts.toOwnedSlice(ctx.allocator),
        });
    }

    const stack_size = func.locals.len * @sizeOf(i64);

    return .{
        .name = func.name,
        .blocks = try blocks.toOwnedSlice(ctx.allocator),
        .stack_size = stack_size,
        .vreg_count = vreg_count,
    };
}

fn lowerInst(
    ctx: *LowerContext,
    inst: mir.Inst,
    insts: *std.ArrayListUnmanaged(machine.InstKind),
    vreg_count: *machine.VReg,
) LowerError!?void {
    const dest_vreg = if (inst.dest) |tmp| destRegister(tmp, vreg_count) else null;

    switch (inst.kind) {
        .Copy => |payload| {
            if (dest_vreg) |dst| {
                const src = try lowerOperand(ctx, payload.src, vreg_count);
                try insts.append(ctx.allocator, .{ .Mov = .{ .dst = .{ .VReg = dst }, .src = src } });
            }
        },
        .Bin => |payload| {
            if (dest_vreg) |dst| {
                const lhs = try lowerOperand(ctx, payload.lhs, vreg_count);
                const rhs = try lowerOperand(ctx, payload.rhs, vreg_count);
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
                try insts.append(ctx.allocator, .{ .Mov = .{ .dst = .{ .VReg = dst }, .src = lhs } });
                try insts.append(ctx.allocator, .{ .Bin = .{ .op = op, .dst = .{ .VReg = dst }, .lhs = .{ .VReg = dst }, .rhs = rhs } });
            }
        },
        .Cmp => |payload| {
            if (dest_vreg) |dst| {
                const lhs = try lowerOperand(ctx, payload.lhs, vreg_count);
                const rhs = try lowerOperand(ctx, payload.rhs, vreg_count);
                try insts.append(ctx.allocator, .{ .Cmp = .{ .lhs = lhs, .rhs = rhs } });
                try insts.append(ctx.allocator, .{ .Setcc = .{ .cond = mapCond(payload.op), .dst = .{ .VReg = dst } } });
                try insts.append(ctx.allocator, .{ .Bin = .{ .op = .and_, .dst = .{ .VReg = dst }, .lhs = .{ .VReg = dst }, .rhs = .{ .Imm = 1 } } });
            }
        },
        .Unary => |payload| {
            if (dest_vreg) |dst| {
                const src = try lowerOperand(ctx, payload.operand, vreg_count);
                const op = switch (payload.op) {
                    .Not => machine.UnaryOpcode.not_,
                    .Neg => machine.UnaryOpcode.neg,
                };
                try insts.append(ctx.allocator, .{ .Mov = .{ .dst = .{ .VReg = dst }, .src = src } });
                try insts.append(ctx.allocator, .{ .Unary = .{ .op = op, .dst = .{ .VReg = dst }, .src = .{ .VReg = dst } } });
            }
        },
        .LoadLocal => |payload| {
            if (dest_vreg) |dst| {
                const mem = localMem(payload.local);
                try insts.append(ctx.allocator, .{ .Mov = .{ .dst = .{ .VReg = dst }, .src = mem } });
            }
        },
        .StoreLocal => |payload| {
            const mem = localMem(payload.local);
            const src = try lowerOperand(ctx, payload.src, vreg_count);
            try insts.append(ctx.allocator, .{ .Mov = .{ .dst = mem, .src = src } });
        },
        .Call => |payload| {
            const target = resolveCallTarget(ctx, payload.target, vreg_count) catch |err| return err;

            const is_varargs = switch (target) {
                .Direct => |name| std.mem.eql(u8, name, "printf"),
                .Indirect => false,
            };
            if (is_varargs) ctx.uses_printf = true;

            const arg_registers = [_]machine.PhysReg{ .rdi, .rsi, .rdx, .rcx, .r8, .r9 };
            for (payload.args, 0..) |arg, idx| {
                if (idx >= arg_registers.len) break; // simplistic handling: ignore extra args for now
                const lowered = try lowerOperand(ctx, arg, vreg_count);
                try insts.append(ctx.allocator, .{ .Mov = .{ .dst = .{ .Phys = arg_registers[idx] }, .src = lowered } });
            }

            if (is_varargs) {
                try insts.append(ctx.allocator, .{ .Mov = .{ .dst = .{ .Phys = .rax }, .src = .{ .Imm = 0 } } });
            }

            try insts.append(ctx.allocator, .{ .Call = switch (target) {
                .Direct => |name| .{ .Direct = name },
                .Indirect => |op| .{ .Indirect = op },
            } });

            if (dest_vreg) |dst| {
                try insts.append(ctx.allocator, .{ .Mov = .{ .dst = .{ .VReg = dst }, .src = .{ .Phys = .rax } } });
            }
        },
        .Range => |payload| {
            if (dest_vreg) |dst| {
                const start = try lowerOperand(ctx, payload.start, vreg_count);
                _ = try lowerOperand(ctx, payload.end, vreg_count);
                try insts.append(ctx.allocator, .{ .Mov = .{ .dst = .{ .VReg = dst }, .src = start } });
            }
        },
        .Index => |payload| {
            if (dest_vreg) |dst| {
                const target = try lowerOperand(ctx, payload.target, vreg_count);
                const idx = try lowerOperand(ctx, payload.index, vreg_count);
                const mem = switch (target) {
                    .Mem => |base_mem| blk: {
                        const offset = switch (idx) {
                            .Imm => |imm| blk2: {
                                const scaled: i32 = @intCast(imm * @as(i64, @intCast(@sizeOf(i64))));
                                break :blk2 base_mem.offset + scaled;
                            },
                            else => {
                                ctx.diagnostics.reportError(zero_span, "indexing currently supports only immediate indices");
                                return error.Unsupported;
                            },
                        };
                        break :blk machine.MOperand{ .Mem = .{ .base = base_mem.base, .offset = offset } };
                    },
                    else => {
                        ctx.diagnostics.reportError(zero_span, "unsupported index base for x86_64 lowering");
                        return error.Unsupported;
                    },
                };
                try insts.append(ctx.allocator, .{ .Mov = .{ .dst = .{ .VReg = dst }, .src = mem } });
            }
        },
        .Field => |payload| {
            if (dest_vreg) |dst| {
                const target = try lowerOperand(ctx, payload.target, vreg_count);
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
                        ctx.diagnostics.reportError(zero_span, "unsupported field base for x86_64 lowering");
                        return error.Unsupported;
                    },
                };
                try insts.append(ctx.allocator, .{ .Mov = .{ .dst = .{ .VReg = dst }, .src = mem } });
            }
        },
        .Array => |payload| {
            if (dest_vreg) |dst| {
                if (payload.elems.len > 0) {
                    const first = try lowerOperand(ctx, payload.elems[0], vreg_count);
                    try insts.append(ctx.allocator, .{ .Mov = .{ .dst = .{ .VReg = dst }, .src = first } });
                } else {
                    try insts.append(ctx.allocator, .{ .Mov = .{ .dst = .{ .VReg = dst }, .src = .{ .Imm = 0 } } });
                }
            }
        },
        .StructInit => |payload| {
            if (dest_vreg) |dst| {
                if (payload.fields.len > 0) {
                    const first = try lowerOperand(ctx, payload.fields[0].value, vreg_count);
                    try insts.append(ctx.allocator, .{ .Mov = .{ .dst = .{ .VReg = dst }, .src = first } });
                } else {
                    try insts.append(ctx.allocator, .{ .Mov = .{ .dst = .{ .VReg = dst }, .src = .{ .Imm = 0 } } });
                }
            }
        },
    }

    return {};
}

fn lowerTerm(
    ctx: *LowerContext,
    term: mir.TermKind,
    insts: *std.ArrayListUnmanaged(machine.InstKind),
    vreg_count: *machine.VReg,
) LowerError!void {
    switch (term) {
        .Goto => |target| try insts.append(ctx.allocator, .{ .Jmp = target }),
        .If => |payload| {
            const cond_op = lowerSimpleOperand(ctx, payload.cond, vreg_count) catch |err| switch (err) {
                error.Unsupported => return err,
                else => return err,
            };
            try insts.append(ctx.allocator, .{ .Test = .{ .operand = cond_op } });
            try insts.append(ctx.allocator, .{ .Jcc = .{ .cond = .ne, .target = payload.then_block } });
            try insts.append(ctx.allocator, .{ .Jmp = payload.else_block });
        },
        .Ret => |maybe_op| {
            if (maybe_op) |op| {
                const lowered = lowerSimpleOperand(ctx, op, vreg_count) catch |err| return err;
                try insts.append(ctx.allocator, .{ .Mov = .{ .dst = .{ .Phys = .rax }, .src = lowered } });
            } else {
                try insts.append(ctx.allocator, .{ .Mov = .{ .dst = .{ .Phys = .rax }, .src = .{ .Imm = 0 } } });
            }
            try insts.append(ctx.allocator, .{ .Ret = null });
        },
    }
}

fn lowerOperand(ctx: *LowerContext, op: mir.Operand, vreg_count: *machine.VReg) LowerError!machine.MOperand {
    return lowerSimpleOperand(ctx, op, vreg_count) catch |err| switch (err) {
        error.Unsupported => {
            ctx.diagnostics.reportError(zero_span, "unsupported operand in x86_64 lowering");
            return err;
        },
        else => return err,
    };
}

fn lowerSimpleOperand(ctx: *LowerContext, op: mir.Operand, vreg_count: ?*machine.VReg) LowerError!machine.MOperand {
    return switch (op) {
        .Temp => |tmp| .{ .VReg = destRegister(tmp, vreg_count) },
        .Local => |local| localMem(local),
        .Param => |idx| blk: {
            const arg_registers = [_]machine.PhysReg{ .rdi, .rsi, .rdx, .rcx, .r8, .r9 };
            if (idx < arg_registers.len) break :blk .{ .Phys = arg_registers[idx] };
            ctx.diagnostics.reportError(zero_span, "parameter index exceeds supported register set");
            return error.Unsupported;
        },
        .ImmInt => |v| .{ .Imm = v },
        .ImmBool => |v| .{ .Imm = if (v) 1 else 0 },
        .ImmFloat => |v| .{ .Imm = @bitCast(v) },
        .ImmString => |s| .{ .Label = try ctx.data.internString(s) },
        .Global => |id| blk: {
            if (id >= ctx.mir_crate.fns.items.len) {
                ctx.diagnostics.reportError(zero_span, "operand refers to unknown function id");
                return error.Unsupported;
            }
            break :blk .{ .Label = ctx.mir_crate.fns.items[id].name };
        },
        .Symbol => |name| .{ .Label = name },
        else => {
            ctx.diagnostics.reportError(zero_span, "unsupported operand kind for x86_64 lowering");
            return error.Unsupported;
        },
    };
}

const CallTarget = union(enum) { Direct: []const u8, Indirect: machine.MOperand };

fn resolveCallTarget(ctx: *LowerContext, target: mir.Operand, vreg_count: *machine.VReg) LowerError!CallTarget {
    return switch (target) {
        .Global => |id| blk: {
            if (id >= ctx.mir_crate.fns.items.len) {
                ctx.diagnostics.reportError(zero_span, "call target refers to unknown function id");
                return error.Unsupported;
            }
            break :blk .{ .Direct = ctx.mir_crate.fns.items[id].name };
        },
        .Symbol => |name| .{ .Direct = name },
        else => .{ .Indirect = try lowerOperand(ctx, target, vreg_count) },
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
