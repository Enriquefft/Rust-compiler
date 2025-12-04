const std = @import("std");
const diag = @import("../../diag/diagnostics.zig");
const source_map = @import("../../diag/source_map.zig");
const machine = @import("machine.zig");

const zero_span = source_map.Span{ .file_id = 0, .start = 0, .end = 0 };

pub const AllocError = error{ OutOfRegisters, OutOfMemory };

const Location = union(enum) {
    gpr: machine.PhysReg,
    xmm: machine.XmmReg,
    spill: i32,
};

const RegisterClass = enum { gpr, xmm };

const Scratch = union(enum) {
    gpr: machine.PhysReg,
    xmm: machine.XmmReg,
};

const caller_saved_gprs = [_]machine.PhysReg{ .rax, .rcx, .rdx, .rsi, .rdi, .r8, .r9, .r10, .r11 };
const callee_saved_gprs = [_]machine.PhysReg{ .rbx, .r12, .r13, .r14, .r15 };

const caller_saved_xmms = [_]machine.XmmReg{ .xmm0, .xmm1, .xmm2, .xmm3, .xmm4, .xmm5 };
const callee_saved_xmms = [_]machine.XmmReg{ .xmm6, .xmm7 };

const scratch_gprs = caller_saved_gprs;
const scratch_xmms = caller_saved_xmms;

/// Liveness information for a VReg: the last instruction index where it's used.
/// Used to determine when a register can be reclaimed.
const LiveRange = struct {
    /// Last instruction index in the block where this VReg is used (inclusive)
    last_use: usize,
    /// Whether this VReg is still live (has not been freed yet)
    is_live: bool,
};

/// Compute liveness information for all VRegs in a block.
/// Returns a map from VReg to its last use instruction index within the block.
/// Maximum number of VReg operands that can be collected from a single instruction.
/// Most instructions use at most 2-3 operands (dst, src, and occasionally a third).
/// 4 is chosen to accommodate binary ops (lhs, rhs) and stores (addr, src).
const MAX_OPERANDS_PER_INST: usize = 4;

fn computeLiveness(
    allocator: std.mem.Allocator,
    insts: []const machine.InstKind,
) AllocError!std.AutoHashMap(machine.VReg, LiveRange) {
    var liveness = std.AutoHashMap(machine.VReg, LiveRange).init(allocator);
    errdefer liveness.deinit();

    // Forward pass: track last use of each VReg
    for (insts, 0..) |inst, idx| {
        var operands: [MAX_OPERANDS_PER_INST]?machine.VReg = .{ null, null, null, null };
        collectVRegsUsed(&inst, &operands);

        for (operands) |maybe_vreg| {
            if (maybe_vreg) |vreg| {
                try liveness.put(vreg, .{ .last_use = idx, .is_live = true });
            }
        }
    }

    return liveness;
}

/// Collect VRegs used in an instruction (read operands, not write destinations).
fn collectVRegsUsed(inst: *const machine.InstKind, out: *[4]?machine.VReg) void {
    var count: usize = 0;

    // Helper to add a VReg if it's present
    const addIfVReg = struct {
        fn f(op: machine.MOperand, o: *[4]?machine.VReg, c: *usize) void {
            if (op == .VReg and c.* < 4) {
                o[c.*] = op.VReg;
                c.* += 1;
            }
        }
    }.f;

    switch (inst.*) {
        .Mov => |p| addIfVReg(p.src, out, &count),
        .Movsd => |p| addIfVReg(p.src, out, &count),
        .Bin => |p| {
            addIfVReg(p.lhs, out, &count);
            addIfVReg(p.rhs, out, &count);
        },
        .Unary => |p| addIfVReg(p.src, out, &count),
        .Lea => {},
        .Deref => |p| addIfVReg(p.addr, out, &count),
        .StoreDeref => |p| {
            addIfVReg(p.addr, out, &count);
            addIfVReg(p.src, out, &count);
        },
        .Cvttsd2si => |p| addIfVReg(p.src, out, &count),
        .Cvtsi2sd => |p| addIfVReg(p.src, out, &count),
        .Cmp => |p| {
            addIfVReg(p.lhs, out, &count);
            addIfVReg(p.rhs, out, &count);
        },
        .Setcc => {},
        .Test => |p| addIfVReg(p.operand, out, &count),
        .Push => |p| addIfVReg(p, out, &count),
        .Add => |p| addIfVReg(p.src, out, &count),
        .Jmp => {},
        .Jcc => {},
        .Call => |p| {
            switch (p) {
                .Direct => {},
                .Indirect => |op| addIfVReg(op, out, &count),
            }
        },
        .Ret => |maybe_op| {
            if (maybe_op) |op| addIfVReg(op, out, &count);
        },
    }
}

pub fn allocateRegisters(
    func: *machine.MachineFn,
    allocator: std.mem.Allocator,
    diagnostics: *diag.Diagnostics,
) AllocError!void {
    var map = std.AutoHashMap(machine.VReg, Location).init(allocator);
    defer map.deinit();

    var classes = std.AutoHashMap(machine.VReg, RegisterClass).init(allocator);
    defer classes.deinit();

    const available_gprs = (caller_saved_gprs ++ callee_saved_gprs)[0..];
    const available_xmms = (caller_saved_xmms ++ callee_saved_xmms)[0..];

    var stack_slots: usize = func.stack_size / @sizeOf(i64);

    for (func.blocks) |*block| {
        // Compute liveness for this block
        var liveness = try computeLiveness(allocator, block.insts);
        defer liveness.deinit();

        // Track which physical registers are currently free (not assigned to any live VReg)
        var free_gprs = std.AutoHashMap(machine.PhysReg, void).init(allocator);
        defer free_gprs.deinit();

        var free_xmms = std.AutoHashMap(machine.XmmReg, void).init(allocator);
        defer free_xmms.deinit();

        // Collect registers that are currently in use by mapped VRegs
        var used_gprs = std.AutoHashMap(machine.PhysReg, void).init(allocator);
        defer used_gprs.deinit();
        var used_xmms = std.AutoHashMap(machine.XmmReg, void).init(allocator);
        defer used_xmms.deinit();

        var map_it = map.iterator();
        while (map_it.next()) |entry| {
            switch (entry.value_ptr.*) {
                .gpr => |reg| try used_gprs.put(reg, {}),
                .xmm => |reg| try used_xmms.put(reg, {}),
                .spill => {},
            }
        }

        // Add only unused registers to free sets
        for (available_gprs) |reg| {
            if (!used_gprs.contains(reg)) {
                try free_gprs.put(reg, {});
            }
        }
        for (available_xmms) |reg| {
            if (!used_xmms.contains(reg)) {
                try free_xmms.put(reg, {});
            }
        }

        var rewritten = std.ArrayListUnmanaged(machine.InstKind){};
        defer rewritten.deinit(allocator);

        for (block.insts, 0..) |*inst, inst_idx| {
            // Before processing this instruction, check if any VRegs became dead
            // (their last use was in a prior instruction, meaning they're no longer needed)
            var it = map.iterator();
            while (it.next()) |entry| {
                const vreg = entry.key_ptr.*;
                const loc = entry.value_ptr.*;
                if (liveness.get(vreg)) |live_info| {
                    // A VReg is dead after its last use, so if last_use < inst_idx,
                    // the VReg is no longer needed and its register can be reclaimed
                    if (live_info.last_use < inst_idx and live_info.is_live) {
                        // Free the register
                        switch (loc) {
                            .gpr => |reg| try free_gprs.put(reg, {}),
                            .xmm => |reg| try free_xmms.put(reg, {}),
                            .spill => {},
                        }
                        // Mark as no longer live
                        if (liveness.getPtr(vreg)) |ptr| {
                            ptr.is_live = false;
                        }
                    }
                }
            }

            rewriteOperandsWithLiveness(
                func,
                allocator,
                inst,
                &rewritten,
                &map,
                &classes,
                &free_gprs,
                &free_xmms,
                &stack_slots,
                diagnostics,
                &liveness,
                inst_idx,
            ) catch |err| switch (err) {
                error.OutOfRegisters => return err,
                else => return err,
            };
        }

        allocator.free(block.insts);
        block.insts = try rewritten.toOwnedSlice(allocator);
    }

    func.stack_size = stack_slots * @sizeOf(i64);
}

/// Liveness-aware version of rewriteOperands that uses free register tracking.
/// This version recycles registers from dead VRegs to reduce spills.
///
/// NOTE: Currently optimizes Mov and Bin instructions. Other instruction types
/// are passed through unchanged (using their original operands). Future work
/// could extend liveness-aware allocation to all instruction types.
///
/// Parameters liveness and inst_idx are reserved for future per-instruction
/// liveness decisions but are currently unused as liveness is handled at the
/// block level in allocateRegisters.
fn rewriteOperandsWithLiveness(
    func: *machine.MachineFn,
    allocator: std.mem.Allocator,
    inst: *machine.InstKind,
    rewritten: *std.ArrayListUnmanaged(machine.InstKind),
    map: *std.AutoHashMap(machine.VReg, Location),
    classes: *std.AutoHashMap(machine.VReg, RegisterClass),
    free_gprs: *std.AutoHashMap(machine.PhysReg, void),
    free_xmms: *std.AutoHashMap(machine.XmmReg, void),
    stack_slots: *usize,
    diagnostics: *diag.Diagnostics,
    liveness: *std.AutoHashMap(machine.VReg, LiveRange),
    inst_idx: usize,
) AllocError!void {
    // Reserved for future per-instruction liveness decisions
    _ = liveness;
    _ = inst_idx;

    switch (inst.*) {
        .Mov => |*payload| {
            if (payload.dst == .Phys) {
                try spillIfMappedWithFreeRegs(payload.dst.Phys, map, rewritten, allocator, stack_slots, free_gprs);
            }

            const dst = try materializeWriteWithFreeRegs(
                func,
                payload.dst,
                map,
                classes,
                .gpr,
                free_gprs,
                free_xmms,
                allocator,
                stack_slots,
                diagnostics,
            );
            var src = try materializeReadWithFreeRegs(
                func,
                payload.src,
                map,
                classes,
                .gpr,
                free_gprs,
                free_xmms,
                allocator,
                stack_slots,
                diagnostics,
            );

            if (isMem(dst) and isMem(src)) {
                const scratch = try acquireScratchWithFreeRegs(.gpr, map, rewritten, allocator, stack_slots, free_gprs, free_xmms);
                try rewritten.append(allocator, .{ .Mov = .{ .dst = .{ .Phys = scratch.gpr }, .src = src } });
                src = .{ .Phys = scratch.gpr };
            }

            try rewritten.append(allocator, .{ .Mov = .{ .dst = dst, .src = src } });
        },
        .Bin => |*payload| {
            const dst = try materializeWriteWithFreeRegs(
                func,
                payload.dst,
                map,
                classes,
                .gpr,
                free_gprs,
                free_xmms,
                allocator,
                stack_slots,
                diagnostics,
            );
            _ = try materializeReadWithFreeRegs(
                func,
                payload.lhs,
                map,
                classes,
                .gpr,
                free_gprs,
                free_xmms,
                allocator,
                stack_slots,
                diagnostics,
            );
            var rhs = try materializeReadWithFreeRegs(
                func,
                payload.rhs,
                map,
                classes,
                .gpr,
                free_gprs,
                free_xmms,
                allocator,
                stack_slots,
                diagnostics,
            );

            if (isMem(dst) and isMem(rhs)) {
                const scratch = try acquireScratchWithFreeRegs(.gpr, map, rewritten, allocator, stack_slots, free_gprs, free_xmms);
                try rewritten.append(allocator, .{ .Mov = .{ .dst = .{ .Phys = scratch.gpr }, .src = rhs } });
                rhs = .{ .Phys = scratch.gpr };
            }

            if (payload.op == .imul and isMem(dst)) {
                if (rhs == .Phys and rhs.Phys == .rax) {
                    try spillIfMappedWithFreeRegs(.rcx, map, rewritten, allocator, stack_slots, free_gprs);
                    try rewritten.append(allocator, .{ .Mov = .{ .dst = .{ .Phys = .rcx }, .src = rhs } });
                    rhs = .{ .Phys = .rcx };
                }
                try rewritten.append(allocator, .{ .Mov = .{ .dst = .{ .Phys = .rax }, .src = dst } });
                try rewritten.append(allocator, .{ .Bin = .{ .op = payload.op, .dst = .{ .Phys = .rax }, .lhs = .{ .Phys = .rax }, .rhs = rhs } });
                try rewritten.append(allocator, .{ .Mov = .{ .dst = dst, .src = .{ .Phys = .rax } } });
            } else {
                try rewritten.append(allocator, .{ .Bin = .{ .op = payload.op, .dst = dst, .lhs = dst, .rhs = rhs } });
            }
        },
        .Call => |*payload| {
            // Spill all caller-saved registers before call
            try spillMappedRegistersWithFreeRegs(map, rewritten, allocator, stack_slots, free_gprs, free_xmms, .gpr);
            try spillMappedRegistersWithFreeRegs(map, rewritten, allocator, stack_slots, free_gprs, free_xmms, .xmm);
            switch (payload.*) {
                .Direct => try rewritten.append(allocator, inst.*),
                .Indirect => |op| {
                    const lowered = try materializeReadWithFreeRegs(
                        func,
                        op,
                        map,
                        classes,
                        .gpr,
                        free_gprs,
                        free_xmms,
                        allocator,
                        stack_slots,
                        diagnostics,
                    );
                    try rewritten.append(allocator, .{ .Call = .{ .Indirect = lowered } });
                },
            }
        },
        .Cmp => |*payload| {
            const lhs = try materializeReadWithFreeRegs(
                func,
                payload.lhs,
                map,
                classes,
                .gpr,
                free_gprs,
                free_xmms,
                allocator,
                stack_slots,
                diagnostics,
            );
            var rhs = try materializeReadWithFreeRegs(
                func,
                payload.rhs,
                map,
                classes,
                .gpr,
                free_gprs,
                free_xmms,
                allocator,
                stack_slots,
                diagnostics,
            );

            if (isMem(lhs) and isMem(rhs)) {
                const scratch = try acquireScratchWithFreeRegs(.gpr, map, rewritten, allocator, stack_slots, free_gprs, free_xmms);
                try rewritten.append(allocator, .{ .Mov = .{ .dst = .{ .Phys = scratch.gpr }, .src = rhs } });
                rhs = .{ .Phys = scratch.gpr };
            }

            try rewritten.append(allocator, .{ .Cmp = .{ .lhs = lhs, .rhs = rhs } });
        },
        .Setcc => |*payload| {
            const dst = try materializeWriteWithFreeRegs(
                func,
                payload.dst,
                map,
                classes,
                .gpr,
                free_gprs,
                free_xmms,
                allocator,
                stack_slots,
                diagnostics,
            );
            try rewritten.append(allocator, .{ .Setcc = .{ .cond = payload.cond, .dst = dst } });
        },
        .Test => |*payload| {
            var op = try materializeReadWithFreeRegs(
                func,
                payload.operand,
                map,
                classes,
                .gpr,
                free_gprs,
                free_xmms,
                allocator,
                stack_slots,
                diagnostics,
            );
            if (isMem(op)) {
                const scratch = try acquireScratchWithFreeRegs(.gpr, map, rewritten, allocator, stack_slots, free_gprs, free_xmms);
                try rewritten.append(allocator, .{ .Mov = .{ .dst = .{ .Phys = scratch.gpr }, .src = op } });
                op = .{ .Phys = scratch.gpr };
            }
            try rewritten.append(allocator, .{ .Test = .{ .operand = op } });
        },
        .Lea => |*payload| {
            const dst = try materializeWriteWithFreeRegs(
                func,
                payload.dst,
                map,
                classes,
                .gpr,
                free_gprs,
                free_xmms,
                allocator,
                stack_slots,
                diagnostics,
            );
            if (isMem(dst)) {
                const scratch = try acquireScratchWithFreeRegs(.gpr, map, rewritten, allocator, stack_slots, free_gprs, free_xmms);
                try rewritten.append(allocator, .{ .Lea = .{ .dst = .{ .Phys = scratch.gpr }, .mem = payload.mem } });
                try rewritten.append(allocator, .{ .Mov = .{ .dst = dst, .src = .{ .Phys = scratch.gpr } } });
            } else {
                try rewritten.append(allocator, .{ .Lea = .{ .dst = dst, .mem = payload.mem } });
            }
        },
        .Cvttsd2si => |*payload| {
            const dst = try materializeWriteWithFreeRegs(
                func,
                payload.dst,
                map,
                classes,
                .gpr,
                free_gprs,
                free_xmms,
                allocator,
                stack_slots,
                diagnostics,
            );
            const src = try materializeReadWithFreeRegs(
                func,
                payload.src,
                map,
                classes,
                .xmm,
                free_gprs,
                free_xmms,
                allocator,
                stack_slots,
                diagnostics,
            );
            if (isMem(dst)) {
                const scratch = try acquireScratchWithFreeRegs(.gpr, map, rewritten, allocator, stack_slots, free_gprs, free_xmms);
                try rewritten.append(allocator, .{ .Cvttsd2si = .{ .dst = .{ .Phys = scratch.gpr }, .src = src } });
                try rewritten.append(allocator, .{ .Mov = .{ .dst = dst, .src = .{ .Phys = scratch.gpr } } });
            } else {
                try rewritten.append(allocator, .{ .Cvttsd2si = .{ .dst = dst, .src = src } });
            }
        },
        .Cvtsi2sd => |*payload| {
            const dst = try materializeWriteWithFreeRegs(
                func,
                payload.dst,
                map,
                classes,
                .xmm,
                free_gprs,
                free_xmms,
                allocator,
                stack_slots,
                diagnostics,
            );
            var src = try materializeReadWithFreeRegs(
                func,
                payload.src,
                map,
                classes,
                .gpr,
                free_gprs,
                free_xmms,
                allocator,
                stack_slots,
                diagnostics,
            );
            if (isMem(src)) {
                const scratch = try acquireScratchWithFreeRegs(.gpr, map, rewritten, allocator, stack_slots, free_gprs, free_xmms);
                try rewritten.append(allocator, .{ .Mov = .{ .dst = .{ .Phys = scratch.gpr }, .src = src } });
                src = .{ .Phys = scratch.gpr };
            }
            try rewritten.append(allocator, .{ .Cvtsi2sd = .{ .dst = dst, .src = src } });
        },
        .Movsd => |*payload| {
            const dst = try materializeWriteWithFreeRegs(
                func,
                payload.dst,
                map,
                classes,
                .xmm,
                free_gprs,
                free_xmms,
                allocator,
                stack_slots,
                diagnostics,
            );
            var src = try materializeReadWithFreeRegs(
                func,
                payload.src,
                map,
                classes,
                .xmm,
                free_gprs,
                free_xmms,
                allocator,
                stack_slots,
                diagnostics,
            );
            if (isMem(dst) and isMem(src)) {
                const scratch = try acquireScratchWithFreeRegs(.xmm, map, rewritten, allocator, stack_slots, free_gprs, free_xmms);
                try rewritten.append(allocator, .{ .Movsd = .{ .dst = .{ .Xmm = scratch.xmm }, .src = src } });
                src = .{ .Xmm = scratch.xmm };
            }
            try rewritten.append(allocator, .{ .Movsd = .{ .dst = dst, .src = src } });
        },
        .Unary => |*payload| {
            const dst = try materializeWriteWithFreeRegs(
                func,
                payload.dst,
                map,
                classes,
                .gpr,
                free_gprs,
                free_xmms,
                allocator,
                stack_slots,
                diagnostics,
            );
            var src = try materializeReadWithFreeRegs(
                func,
                payload.src,
                map,
                classes,
                .gpr,
                free_gprs,
                free_xmms,
                allocator,
                stack_slots,
                diagnostics,
            );
            if (isMem(dst) and isMem(src)) {
                const scratch = try acquireScratchWithFreeRegs(.gpr, map, rewritten, allocator, stack_slots, free_gprs, free_xmms);
                try rewritten.append(allocator, .{ .Mov = .{ .dst = .{ .Phys = scratch.gpr }, .src = src } });
                src = .{ .Phys = scratch.gpr };
            }
            try rewritten.append(allocator, .{ .Unary = .{ .op = payload.op, .dst = dst, .src = src } });
        },
        .Deref => |*payload| {
            const dst = try materializeWriteWithFreeRegs(
                func,
                payload.dst,
                map,
                classes,
                .gpr,
                free_gprs,
                free_xmms,
                allocator,
                stack_slots,
                diagnostics,
            );
            var addr = try materializeReadWithFreeRegs(
                func,
                payload.addr,
                map,
                classes,
                .gpr,
                free_gprs,
                free_xmms,
                allocator,
                stack_slots,
                diagnostics,
            );
            if (isMem(addr) or addr == .Imm) {
                const scratch = try acquireScratchWithFreeRegs(.gpr, map, rewritten, allocator, stack_slots, free_gprs, free_xmms);
                try rewritten.append(allocator, .{ .Mov = .{ .dst = .{ .Phys = scratch.gpr }, .src = addr } });
                addr = .{ .Phys = scratch.gpr };
            }
            if (isMem(dst)) {
                try rewritten.append(allocator, .{ .Deref = .{ .dst = .{ .Phys = .rax }, .addr = addr } });
                try rewritten.append(allocator, .{ .Mov = .{ .dst = dst, .src = .{ .Phys = .rax } } });
            } else {
                try rewritten.append(allocator, .{ .Deref = .{ .dst = dst, .addr = addr } });
            }
        },
        .StoreDeref => |*payload| {
            var addr = try materializeReadWithFreeRegs(
                func,
                payload.addr,
                map,
                classes,
                .gpr,
                free_gprs,
                free_xmms,
                allocator,
                stack_slots,
                diagnostics,
            );
            var src = try materializeReadWithFreeRegs(
                func,
                payload.src,
                map,
                classes,
                .gpr,
                free_gprs,
                free_xmms,
                allocator,
                stack_slots,
                diagnostics,
            );
            if (isMem(addr) or addr == .Imm) {
                const scratch = try acquireScratchWithFreeRegs(.gpr, map, rewritten, allocator, stack_slots, free_gprs, free_xmms);
                try rewritten.append(allocator, .{ .Mov = .{ .dst = .{ .Phys = scratch.gpr }, .src = addr } });
                addr = .{ .Phys = scratch.gpr };
            }
            if (isMem(src)) {
                // Check if addr is in rax before loading src to rax
                if (addr == .Phys and addr.Phys == .rax) {
                    // addr is in rax, use rcx for src instead
                    try spillIfMappedWithFreeRegs(.rcx, map, rewritten, allocator, stack_slots, free_gprs);
                    try rewritten.append(allocator, .{ .Mov = .{ .dst = .{ .Phys = .rcx }, .src = src } });
                    src = .{ .Phys = .rcx };
                } else {
                    try rewritten.append(allocator, .{ .Mov = .{ .dst = .{ .Phys = .rax }, .src = src } });
                    src = .{ .Phys = .rax };
                }
            }
            try rewritten.append(allocator, .{ .StoreDeref = .{ .addr = addr, .src = src } });
        },
        .Push => |*payload| {
            var op = try materializeReadWithFreeRegs(
                func,
                payload.*,
                map,
                classes,
                .gpr,
                free_gprs,
                free_xmms,
                allocator,
                stack_slots,
                diagnostics,
            );
            if (isMem(op)) {
                const scratch = try acquireScratchWithFreeRegs(.gpr, map, rewritten, allocator, stack_slots, free_gprs, free_xmms);
                try rewritten.append(allocator, .{ .Mov = .{ .dst = .{ .Phys = scratch.gpr }, .src = op } });
                op = .{ .Phys = scratch.gpr };
            }
            try rewritten.append(allocator, .{ .Push = op });
        },
        .Add => |*payload| {
            var dst = try materializeWriteWithFreeRegs(
                func,
                payload.dst,
                map,
                classes,
                .gpr,
                free_gprs,
                free_xmms,
                allocator,
                stack_slots,
                diagnostics,
            );
            var src = try materializeReadWithFreeRegs(
                func,
                payload.src,
                map,
                classes,
                .gpr,
                free_gprs,
                free_xmms,
                allocator,
                stack_slots,
                diagnostics,
            );
            if (isMem(dst)) {
                const scratch = try acquireScratchWithFreeRegs(.gpr, map, rewritten, allocator, stack_slots, free_gprs, free_xmms);
                try rewritten.append(allocator, .{ .Mov = .{ .dst = .{ .Phys = scratch.gpr }, .src = dst } });
                dst = .{ .Phys = scratch.gpr };
            }
            if (isMem(src)) {
                const scratch = try acquireScratchWithFreeRegs(.gpr, map, rewritten, allocator, stack_slots, free_gprs, free_xmms);
                try rewritten.append(allocator, .{ .Mov = .{ .dst = .{ .Phys = scratch.gpr }, .src = src } });
                src = .{ .Phys = scratch.gpr };
            }
            try rewritten.append(allocator, .{ .Add = .{ .dst = dst, .src = src } });
        },
        // For other instruction types, fall through to simple handling
        else => {
            // Use the original operands as-is for instructions we haven't specialized
            try rewritten.append(allocator, inst.*);
        },
    }
}

fn rewriteOperands(
    func: *machine.MachineFn,
    allocator: std.mem.Allocator,
    inst: *machine.InstKind,
    rewritten: *std.ArrayListUnmanaged(machine.InstKind),
    map: *std.AutoHashMap(machine.VReg, Location),
    classes: *std.AutoHashMap(machine.VReg, RegisterClass),
    available_gprs: []const machine.PhysReg,
    available_xmms: []const machine.XmmReg,
    phys_used_gpr: *usize,
    phys_used_xmm: *usize,
    stack_slots: *usize,
    diagnostics: *diag.Diagnostics,
) AllocError!void {
    switch (inst.*) {
        .Mov => |*payload| {
            if (payload.dst == .Phys) {
                try spillIfMapped(payload.dst.Phys, map, rewritten, allocator, stack_slots);
            }

            const dst = try materializeWrite(
                func,
                payload.dst,
                map,
                classes,
                .gpr,
                available_gprs,
                available_xmms,
                phys_used_gpr,
                phys_used_xmm,
                allocator,
                stack_slots,
                diagnostics,
            );
            var src = try materializeRead(
                func,
                payload.src,
                map,
                classes,
                .gpr,
                available_gprs,
                available_xmms,
                phys_used_gpr,
                phys_used_xmm,
                allocator,
                stack_slots,
                diagnostics,
            );

            if (isMem(dst) and isMem(src)) {
                const scratch = try acquireScratch(.gpr, map, rewritten, allocator, stack_slots);
                try rewritten.append(allocator, .{ .Mov = .{ .dst = .{ .Phys = scratch.gpr }, .src = src } });
                src = .{ .Phys = scratch.gpr };
            }

            try rewritten.append(allocator, .{ .Mov = .{ .dst = dst, .src = src } });
        },
        .Bin => |*payload| {
            const dst = try materializeWrite(
                func,
                payload.dst,
                map,
                classes,
                .gpr,
                available_gprs,
                available_xmms,
                phys_used_gpr,
                phys_used_xmm,
                allocator,
                stack_slots,
                diagnostics,
            );
            _ = try materializeRead(
                func,
                payload.lhs,
                map,
                classes,
                .gpr,
                available_gprs,
                available_xmms,
                phys_used_gpr,
                phys_used_xmm,
                allocator,
                stack_slots,
                diagnostics,
            );
            var rhs = try materializeRead(
                func,
                payload.rhs,
                map,
                classes,
                .gpr,
                available_gprs,
                available_xmms,
                phys_used_gpr,
                phys_used_xmm,
                allocator,
                stack_slots,
                diagnostics,
            );

            if (isMem(dst) and isMem(rhs)) {
                const scratch = try acquireScratch(.gpr, map, rewritten, allocator, stack_slots);
                try rewritten.append(allocator, .{ .Mov = .{ .dst = .{ .Phys = scratch.gpr }, .src = rhs } });
                rhs = .{ .Phys = scratch.gpr };
            }

            if (payload.op == .imul and isMem(dst)) {
                // For imul with memory destination, we need to use rax.
                // If rhs is in rax, we need to save it to a different register first.
                if (rhs == .Phys and rhs.Phys == .rax) {
                    // rhs is in rax, save it to rcx (a different scratch register)
                    try spillIfMapped(.rcx, map, rewritten, allocator, stack_slots);
                    try rewritten.append(allocator, .{ .Mov = .{ .dst = .{ .Phys = .rcx }, .src = rhs } });
                    rhs = .{ .Phys = .rcx };
                }
                try rewritten.append(allocator, .{ .Mov = .{ .dst = .{ .Phys = .rax }, .src = dst } });
                try rewritten.append(allocator, .{ .Bin = .{ .op = payload.op, .dst = .{ .Phys = .rax }, .lhs = .{ .Phys = .rax }, .rhs = rhs } });
                try rewritten.append(allocator, .{ .Mov = .{ .dst = dst, .src = .{ .Phys = .rax } } });
            } else {
                try rewritten.append(allocator, .{ .Bin = .{ .op = payload.op, .dst = dst, .lhs = dst, .rhs = rhs } });
            }
        },
        .Unary => |*payload| {
            const dst = try materializeWrite(
                func,
                payload.dst,
                map,
                classes,
                .gpr,
                available_gprs,
                available_xmms,
                phys_used_gpr,
                phys_used_xmm,
                allocator,
                stack_slots,
                diagnostics,
            );
            const src = try materializeRead(
                func,
                payload.src,
                map,
                classes,
                .gpr,
                available_gprs,
                available_xmms,
                phys_used_gpr,
                phys_used_xmm,
                allocator,
                stack_slots,
                diagnostics,
            );
            try rewritten.append(allocator, .{ .Unary = .{ .op = payload.op, .dst = dst, .src = src } });
        },
        .Lea => |*payload| {
            const dst = try materializeWrite(
                func,
                payload.dst,
                map,
                classes,
                .gpr,
                available_gprs,
                available_xmms,
                phys_used_gpr,
                phys_used_xmm,
                allocator,
                stack_slots,
                diagnostics,
            );
            if (isMem(dst)) {
                const scratch = try acquireScratch(.gpr, map, rewritten, allocator, stack_slots);
                try rewritten.append(allocator, .{ .Lea = .{ .dst = .{ .Phys = scratch.gpr }, .mem = payload.mem } });
                try rewritten.append(allocator, .{ .Mov = .{ .dst = dst, .src = .{ .Phys = scratch.gpr } } });
            } else {
                try rewritten.append(allocator, .{ .Lea = .{ .dst = dst, .mem = payload.mem } });
            }
        },
        .Deref => |*payload| {
            const dst = try materializeWrite(
                func,
                payload.dst,
                map,
                classes,
                .gpr,
                available_gprs,
                available_xmms,
                phys_used_gpr,
                phys_used_xmm,
                allocator,
                stack_slots,
                diagnostics,
            );
            var addr = try materializeRead(
                func,
                payload.addr,
                map,
                classes,
                .gpr,
                available_gprs,
                available_xmms,
                phys_used_gpr,
                phys_used_xmm,
                allocator,
                stack_slots,
                diagnostics,
            );
            if (isMem(addr)) {
                const scratch = try acquireScratch(.gpr, map, rewritten, allocator, stack_slots);
                try rewritten.append(allocator, .{ .Mov = .{ .dst = .{ .Phys = scratch.gpr }, .src = addr } });
                addr = .{ .Phys = scratch.gpr };
            }
            if (isMem(dst)) {
                try rewritten.append(allocator, .{ .Deref = .{ .dst = .{ .Phys = .rax }, .addr = addr } });
                try rewritten.append(allocator, .{ .Mov = .{ .dst = dst, .src = .{ .Phys = .rax } } });
            } else {
                try rewritten.append(allocator, .{ .Deref = .{ .dst = dst, .addr = addr } });
            }
        },
        .StoreDeref => |*payload| {
            var addr = try materializeRead(
                func,
                payload.addr,
                map,
                classes,
                .gpr,
                available_gprs,
                available_xmms,
                phys_used_gpr,
                phys_used_xmm,
                allocator,
                stack_slots,
                diagnostics,
            );
            var src = try materializeRead(
                func,
                payload.src,
                map,
                classes,
                .gpr,
                available_gprs,
                available_xmms,
                phys_used_gpr,
                phys_used_xmm,
                allocator,
                stack_slots,
                diagnostics,
            );
            if (isMem(addr) or addr == .Imm) {
                const scratch = try acquireScratch(.gpr, map, rewritten, allocator, stack_slots);
                try rewritten.append(allocator, .{ .Mov = .{ .dst = .{ .Phys = scratch.gpr }, .src = addr } });
                addr = .{ .Phys = scratch.gpr };
            }
            if (isMem(src)) {
                // Check if addr is in rax before loading src to rax
                if (addr == .Phys and addr.Phys == .rax) {
                    // addr is in rax, use rcx for src instead
                    try spillIfMapped(.rcx, map, rewritten, allocator, stack_slots);
                    try rewritten.append(allocator, .{ .Mov = .{ .dst = .{ .Phys = .rcx }, .src = src } });
                    src = .{ .Phys = .rcx };
                } else {
                    try rewritten.append(allocator, .{ .Mov = .{ .dst = .{ .Phys = .rax }, .src = src } });
                    src = .{ .Phys = .rax };
                }
            }
            try rewritten.append(allocator, .{ .StoreDeref = .{ .addr = addr, .src = src } });
        },
        .Cvttsd2si => |*payload| {
            const dst = try materializeWrite(
                func,
                payload.dst,
                map,
                classes,
                .gpr,
                available_gprs,
                available_xmms,
                phys_used_gpr,
                phys_used_xmm,
                allocator,
                stack_slots,
                diagnostics,
            );
            const src = try materializeRead(
                func,
                payload.src,
                map,
                classes,
                .xmm,
                available_gprs,
                available_xmms,
                phys_used_gpr,
                phys_used_xmm,
                allocator,
                stack_slots,
                diagnostics,
            );
            try rewritten.append(allocator, .{ .Cvttsd2si = .{ .dst = dst, .src = src } });
        },
        .Cvtsi2sd => |*payload| {
            const dst = try materializeWrite(
                func,
                payload.dst,
                map,
                classes,
                .xmm,
                available_gprs,
                available_xmms,
                phys_used_gpr,
                phys_used_xmm,
                allocator,
                stack_slots,
                diagnostics,
            );
            const src = try materializeRead(
                func,
                payload.src,
                map,
                classes,
                .gpr,
                available_gprs,
                available_xmms,
                phys_used_gpr,
                phys_used_xmm,
                allocator,
                stack_slots,
                diagnostics,
            );
            try rewritten.append(allocator, .{ .Cvtsi2sd = .{ .dst = dst, .src = src } });
        },
        .Cmp => |*payload| {
            const lhs = try materializeRead(
                func,
                payload.lhs,
                map,
                classes,
                .gpr,
                available_gprs,
                available_xmms,
                phys_used_gpr,
                phys_used_xmm,
                allocator,
                stack_slots,
                diagnostics,
            );
            var rhs = try materializeRead(
                func,
                payload.rhs,
                map,
                classes,
                .gpr,
                available_gprs,
                available_xmms,
                phys_used_gpr,
                phys_used_xmm,
                allocator,
                stack_slots,
                diagnostics,
            );

            if (isMem(lhs) and isMem(rhs)) {
                const scratch = try acquireScratch(.gpr, map, rewritten, allocator, stack_slots);
                try rewritten.append(allocator, .{ .Mov = .{ .dst = .{ .Phys = scratch.gpr }, .src = rhs } });
                rhs = .{ .Phys = scratch.gpr };
            }

            try rewritten.append(allocator, .{ .Cmp = .{ .lhs = lhs, .rhs = rhs } });
        },
        .Setcc => |*payload| {
            const dst = try materializeWrite(
                func,
                payload.dst,
                map,
                classes,
                .gpr,
                available_gprs,
                available_xmms,
                phys_used_gpr,
                phys_used_xmm,
                allocator,
                stack_slots,
                diagnostics,
            );
            try rewritten.append(allocator, .{ .Setcc = .{ .cond = payload.cond, .dst = dst } });
        },
        .Test => |*payload| {
            var op = try materializeRead(
                func,
                payload.operand,
                map,
                classes,
                .gpr,
                available_gprs,
                available_xmms,
                phys_used_gpr,
                phys_used_xmm,
                allocator,
                stack_slots,
                diagnostics,
            );
            if (isMem(op)) {
                const scratch = try acquireScratch(.gpr, map, rewritten, allocator, stack_slots);
                try rewritten.append(allocator, .{ .Mov = .{ .dst = .{ .Phys = scratch.gpr }, .src = op } });
                op = .{ .Phys = scratch.gpr };
            }
            try rewritten.append(allocator, .{ .Test = .{ .operand = op } });
        },
        .Push => |*payload| {
            var op = try materializeRead(
                func,
                payload.*,
                map,
                classes,
                .gpr,
                available_gprs,
                available_xmms,
                phys_used_gpr,
                phys_used_xmm,
                allocator,
                stack_slots,
                diagnostics,
            );
            if (isMem(op)) {
                const scratch = try acquireScratch(.gpr, map, rewritten, allocator, stack_slots);
                try rewritten.append(allocator, .{ .Mov = .{ .dst = .{ .Phys = scratch.gpr }, .src = op } });
                op = .{ .Phys = scratch.gpr };
            }
            try rewritten.append(allocator, .{ .Push = op });
        },
        .Add => |*payload| {
            const dst = try materializeWrite(
                func,
                payload.dst,
                map,
                classes,
                .gpr,
                available_gprs,
                available_xmms,
                phys_used_gpr,
                phys_used_xmm,
                allocator,
                stack_slots,
                diagnostics,
            );
            var src = try materializeRead(
                func,
                payload.src,
                map,
                classes,
                .gpr,
                available_gprs,
                available_xmms,
                phys_used_gpr,
                phys_used_xmm,
                allocator,
                stack_slots,
                diagnostics,
            );
            if (isMem(dst) and isMem(src)) {
                const scratch = try acquireScratch(.gpr, map, rewritten, allocator, stack_slots);
                try rewritten.append(allocator, .{ .Mov = .{ .dst = .{ .Phys = scratch.gpr }, .src = src } });
                src = .{ .Phys = scratch.gpr };
            }
            try rewritten.append(allocator, .{ .Add = .{ .dst = dst, .src = src } });
        },
        .Call => |*payload| {
            try spillMappedRegisters(map, rewritten, allocator, stack_slots, .gpr);
            try spillMappedRegisters(map, rewritten, allocator, stack_slots, .xmm);
            switch (payload.*) {
                .Direct => try rewritten.append(allocator, inst.*),
                .Indirect => |op| {
                    const lowered = try materializeRead(
                        func,
                        op,
                        map,
                        classes,
                        .gpr,
                        available_gprs,
                        available_xmms,
                        phys_used_gpr,
                        phys_used_xmm,
                        allocator,
                        stack_slots,
                        diagnostics,
                    );
                    try rewritten.append(allocator, .{ .Call = .{ .Indirect = lowered } });
                },
            }
        },
        .Jmp, .Jcc, .Ret => try rewritten.append(allocator, inst.*),
        .Movsd => |*payload| {
            const dst = try materializeWrite(
                func,
                payload.dst,
                map,
                classes,
                .xmm,
                available_gprs,
                available_xmms,
                phys_used_gpr,
                phys_used_xmm,
                allocator,
                stack_slots,
                diagnostics,
            );
            var src = try materializeRead(
                func,
                payload.src,
                map,
                classes,
                .xmm,
                available_gprs,
                available_xmms,
                phys_used_gpr,
                phys_used_xmm,
                allocator,
                stack_slots,
                diagnostics,
            );
            if (isMem(dst) and isMem(src)) {
                const scratch = try acquireScratch(.xmm, map, rewritten, allocator, stack_slots);
                try rewritten.append(allocator, .{ .Movsd = .{ .dst = .{ .Xmm = scratch.xmm }, .src = src } });
                src = .{ .Xmm = scratch.xmm };
            }
            try rewritten.append(allocator, .{ .Movsd = .{ .dst = dst, .src = src } });
        },
    }
}

fn ensureClass(
    vreg: machine.VReg,
    classes: *std.AutoHashMap(machine.VReg, RegisterClass),
    desired: RegisterClass,
) AllocError!void {
    if (classes.getPtr(vreg)) |cls| {
        if (cls.* != desired) {
            return AllocError.OutOfRegisters;
        }
        return;
    }
    try classes.put(vreg, desired);
}

fn assign(
    func: *machine.MachineFn,
    vreg: machine.VReg,
    map: *std.AutoHashMap(machine.VReg, Location),
    classes: *std.AutoHashMap(machine.VReg, RegisterClass),
    class: RegisterClass,
    available_gprs: []const machine.PhysReg,
    available_xmms: []const machine.XmmReg,
    phys_used_gpr: *usize,
    phys_used_xmm: *usize,
    allocator: std.mem.Allocator,
    stack_slots: *usize,
    diagnostics: *diag.Diagnostics,
) AllocError!Location {
    if (map.get(vreg)) |loc| return loc;

    try ensureClass(vreg, classes, class);

    switch (class) {
        .gpr => {
            if (phys_used_gpr.* < available_gprs.len) {
                const reg = available_gprs[phys_used_gpr.*];
                phys_used_gpr.* += 1;
                if (isCalleeSavedGpr(reg)) {
                    const offset = try reserveCalleeSavedGpr(func, reg, allocator, stack_slots);
                    _ = offset;
                }
                try map.put(vreg, .{ .gpr = reg });
                return .{ .gpr = reg };
            }
        },
        .xmm => {
            if (phys_used_xmm.* < available_xmms.len) {
                const reg = available_xmms[phys_used_xmm.*];
                phys_used_xmm.* += 1;
                if (isCalleeSavedXmm(reg)) {
                    const offset = try reserveCalleeSavedXmm(func, reg, allocator, stack_slots);
                    _ = offset;
                }
                try map.put(vreg, .{ .xmm = reg });
                return .{ .xmm = reg };
            }
        },
    }

    if (class == .gpr and available_gprs.len == 0) {
        diagnostics.reportError(zero_span, "ran out of registers during allocation");
        return error.OutOfRegisters;
    }

    const slot_size: usize = if (class == .xmm) 2 else 1;
    const offset = allocateStackSlots(stack_slots, slot_size);
    const loc = Location{ .spill = offset };
    try map.put(vreg, loc);
    return loc;
}

/// Liveness-aware register assignment that uses free register tracking.
/// Prefers reusing freed registers over allocating new ones to reduce spills.
fn assignWithFreeRegs(
    func: *machine.MachineFn,
    vreg: machine.VReg,
    map: *std.AutoHashMap(machine.VReg, Location),
    classes: *std.AutoHashMap(machine.VReg, RegisterClass),
    class: RegisterClass,
    free_gprs: *std.AutoHashMap(machine.PhysReg, void),
    free_xmms: *std.AutoHashMap(machine.XmmReg, void),
    allocator: std.mem.Allocator,
    stack_slots: *usize,
    diagnostics: *diag.Diagnostics,
) AllocError!Location {
    if (map.get(vreg)) |loc| return loc;

    try ensureClass(vreg, classes, class);

    switch (class) {
        .gpr => {
            // Try to get a free GPR from the free set, preferring caller-saved first
            // First, try caller-saved registers
            for (caller_saved_gprs) |reg| {
                if (free_gprs.contains(reg)) {
                    _ = free_gprs.remove(reg);
                    try map.put(vreg, .{ .gpr = reg });
                    return .{ .gpr = reg };
                }
            }
            // Then, try callee-saved registers
            for (callee_saved_gprs) |reg| {
                if (free_gprs.contains(reg)) {
                    _ = free_gprs.remove(reg);
                    const offset = try reserveCalleeSavedGpr(func, reg, allocator, stack_slots);
                    _ = offset;
                    try map.put(vreg, .{ .gpr = reg });
                    return .{ .gpr = reg };
                }
            }
        },
        .xmm => {
            // Try to get a free XMM from the free set, preferring caller-saved first
            // First, try caller-saved registers
            for (caller_saved_xmms) |reg| {
                if (free_xmms.contains(reg)) {
                    _ = free_xmms.remove(reg);
                    try map.put(vreg, .{ .xmm = reg });
                    return .{ .xmm = reg };
                }
            }
            // Then, try callee-saved registers
            for (callee_saved_xmms) |reg| {
                if (free_xmms.contains(reg)) {
                    _ = free_xmms.remove(reg);
                    const offset = try reserveCalleeSavedXmm(func, reg, allocator, stack_slots);
                    _ = offset;
                    try map.put(vreg, .{ .xmm = reg });
                    return .{ .xmm = reg };
                }
            }
        },
    }

    // No free registers available, spill to stack
    // Note: This is a diagnostic warning, not an error that stops compilation.
    // The register allocator will spill to stack when registers are exhausted.
    switch (class) {
        .gpr => {
            if (free_gprs.count() == 0) {
                diagnostics.reportWarning(zero_span, "GPR registers exhausted, spilling to stack");
            }
        },
        .xmm => {
            if (free_xmms.count() == 0) {
                diagnostics.reportWarning(zero_span, "XMM registers exhausted, spilling to stack");
            }
        },
    }

    const slot_size: usize = if (class == .xmm) 2 else 1;
    const offset = allocateStackSlots(stack_slots, slot_size);
    const loc = Location{ .spill = offset };
    try map.put(vreg, loc);
    return loc;
}

fn spillMappedRegisters(
    map: *std.AutoHashMap(machine.VReg, Location),
    rewritten: *std.ArrayListUnmanaged(machine.InstKind),
    allocator: std.mem.Allocator,
    stack_slots: *usize,
    class: RegisterClass,
) AllocError!void {
    var it = map.iterator();
    while (it.next()) |entry| {
        switch (entry.value_ptr.*) {
            .gpr => |reg| {
                if (class == .gpr and isCallerSavedGpr(reg)) {
                    const offset = allocateStackSlots(stack_slots, 1);
                    entry.value_ptr.* = .{ .spill = offset };
                    try rewritten.append(allocator, .{ .Mov = .{ .dst = .{ .Mem = .{ .base = .rbp, .offset = offset } }, .src = .{ .Phys = reg } } });
                }
            },
            .xmm => |reg| {
                if (class == .xmm and isCallerSavedXmm(reg)) {
                    const offset = allocateStackSlots(stack_slots, 2);
                    entry.value_ptr.* = .{ .spill = offset };
                    try rewritten.append(allocator, .{ .Movsd = .{ .dst = .{ .Mem = .{ .base = .rbp, .offset = offset } }, .src = .{ .Xmm = reg } } });
                }
            },
            .spill => {},
        }
    }
}

fn spillMappedRegistersWithFreeRegs(
    map: *std.AutoHashMap(machine.VReg, Location),
    rewritten: *std.ArrayListUnmanaged(machine.InstKind),
    allocator: std.mem.Allocator,
    stack_slots: *usize,
    free_gprs: *std.AutoHashMap(machine.PhysReg, void),
    free_xmms: *std.AutoHashMap(machine.XmmReg, void),
    class: RegisterClass,
) AllocError!void {
    var it = map.iterator();
    while (it.next()) |entry| {
        switch (entry.value_ptr.*) {
            .gpr => |reg| {
                if (class == .gpr and isCallerSavedGpr(reg)) {
                    const offset = allocateStackSlots(stack_slots, 1);
                    entry.value_ptr.* = .{ .spill = offset };
                    try rewritten.append(allocator, .{ .Mov = .{ .dst = .{ .Mem = .{ .base = .rbp, .offset = offset } }, .src = .{ .Phys = reg } } });
                    // Register is now spilled, mark it as free
                    try free_gprs.put(reg, {});
                }
            },
            .xmm => |reg| {
                if (class == .xmm and isCallerSavedXmm(reg)) {
                    const offset = allocateStackSlots(stack_slots, 2);
                    entry.value_ptr.* = .{ .spill = offset };
                    try rewritten.append(allocator, .{ .Movsd = .{ .dst = .{ .Mem = .{ .base = .rbp, .offset = offset } }, .src = .{ .Xmm = reg } } });
                    // Register is now spilled, mark it as free
                    try free_xmms.put(reg, {});
                }
            },
            .spill => {},
        }
    }
}

fn materializeRead(
    func: *machine.MachineFn,
    operand: machine.MOperand,
    map: *std.AutoHashMap(machine.VReg, Location),
    classes: *std.AutoHashMap(machine.VReg, RegisterClass),
    class: RegisterClass,
    available_gprs: []const machine.PhysReg,
    available_xmms: []const machine.XmmReg,
    phys_used_gpr: *usize,
    phys_used_xmm: *usize,
    allocator: std.mem.Allocator,
    stack_slots: *usize,
    diagnostics: *diag.Diagnostics,
) AllocError!machine.MOperand {
    return switch (operand) {
        .VReg => |vreg| toOperand(try assign(func, vreg, map, classes, class, available_gprs, available_xmms, phys_used_gpr, phys_used_xmm, allocator, stack_slots, diagnostics)),
        else => operand,
    };
}

fn materializeWrite(
    func: *machine.MachineFn,
    operand: machine.MOperand,
    map: *std.AutoHashMap(machine.VReg, Location),
    classes: *std.AutoHashMap(machine.VReg, RegisterClass),
    class: RegisterClass,
    available_gprs: []const machine.PhysReg,
    available_xmms: []const machine.XmmReg,
    phys_used_gpr: *usize,
    phys_used_xmm: *usize,
    allocator: std.mem.Allocator,
    stack_slots: *usize,
    diagnostics: *diag.Diagnostics,
) AllocError!machine.MOperand {
    return switch (operand) {
        .VReg => |vreg| toOperand(try assign(func, vreg, map, classes, class, available_gprs, available_xmms, phys_used_gpr, phys_used_xmm, allocator, stack_slots, diagnostics)),
        else => operand,
    };
}

fn toOperand(loc: Location) machine.MOperand {
    return switch (loc) {
        .gpr => |reg| .{ .Phys = reg },
        .xmm => |reg| .{ .Xmm = reg },
        .spill => |offset| .{ .Mem = .{ .base = .rbp, .offset = offset } },
    };
}

fn isMem(op: machine.MOperand) bool {
    return switch (op) {
        .Mem => true,
        else => false,
    };
}

fn isCalleeSavedGpr(reg: machine.PhysReg) bool {
    return std.mem.indexOfScalar(machine.PhysReg, &callee_saved_gprs, reg) != null;
}

fn isCallerSavedGpr(reg: machine.PhysReg) bool {
    return std.mem.indexOfScalar(machine.PhysReg, &caller_saved_gprs, reg) != null;
}

fn isCalleeSavedXmm(reg: machine.XmmReg) bool {
    return std.mem.indexOfScalar(machine.XmmReg, &callee_saved_xmms, reg) != null;
}

fn isCallerSavedXmm(reg: machine.XmmReg) bool {
    return std.mem.indexOfScalar(machine.XmmReg, &caller_saved_xmms, reg) != null;
}

fn allocateStackSlots(stack_slots: *usize, slots: usize) i32 {
    stack_slots.* += slots;
    return -@as(i32, @intCast(stack_slots.*)) * @as(i32, @intCast(@sizeOf(i64)));
}

fn reserveCalleeSavedGpr(func: *machine.MachineFn, reg: machine.PhysReg, allocator: std.mem.Allocator, stack_slots: *usize) AllocError!i32 {
    _ = allocator;
    for (func.callee_saved_gprs.items) |entry| {
        if (entry.reg == reg) return entry.offset;
    }
    const offset = allocateStackSlots(stack_slots, 1);
    try func.callee_saved_gprs.append(.{ .reg = reg, .offset = offset });
    return offset;
}

fn reserveCalleeSavedXmm(func: *machine.MachineFn, reg: machine.XmmReg, allocator: std.mem.Allocator, stack_slots: *usize) AllocError!i32 {
    _ = allocator;
    for (func.callee_saved_xmms.items) |entry| {
        if (entry.reg == reg) return entry.offset;
    }
    const offset = allocateStackSlots(stack_slots, 2);
    try func.callee_saved_xmms.append(.{ .reg = reg, .offset = offset });
    return offset;
}

fn spillIfMapped(
    reg: machine.PhysReg,
    map: *std.AutoHashMap(machine.VReg, Location),
    rewritten: *std.ArrayListUnmanaged(machine.InstKind),
    allocator: std.mem.Allocator,
    stack_slots: *usize,
) AllocError!void {
    var it = map.iterator();
    while (it.next()) |entry| {
        if (entry.value_ptr.* == .gpr and entry.value_ptr.gpr == reg) {
            const offset = allocateStackSlots(stack_slots, 1);
            try rewritten.append(allocator, .{ .Mov = .{ .dst = .{ .Mem = .{ .base = .rbp, .offset = offset } }, .src = .{ .Phys = reg } } });
            entry.value_ptr.* = .{ .spill = offset };
            break;
        }
    }
}

fn acquireScratch(
    class: RegisterClass,
    map: *std.AutoHashMap(machine.VReg, Location),
    rewritten: *std.ArrayListUnmanaged(machine.InstKind),
    allocator: std.mem.Allocator,
    stack_slots: *usize,
) AllocError!Scratch {
    const candidates_gpr = scratch_gprs;
    const candidates_xmm = scratch_xmms;

    switch (class) {
        .gpr => {
            for (candidates_gpr) |candidate| {
                if (try freeGprRegisterIfMapped(candidate, map, rewritten, allocator, stack_slots)) return .{ .gpr = candidate };
            }
            return error.OutOfRegisters;
        },
        .xmm => {
            for (candidates_xmm) |candidate| {
                if (try freeXmmRegisterIfMapped(candidate, map, rewritten, allocator, stack_slots)) return .{ .xmm = candidate };
            }
            return error.OutOfRegisters;
        },
    }
}

fn freeGprRegisterIfMapped(
    reg: machine.PhysReg,
    map: *std.AutoHashMap(machine.VReg, Location),
    rewritten: *std.ArrayListUnmanaged(machine.InstKind),
    allocator: std.mem.Allocator,
    stack_slots: *usize,
) AllocError!bool {
    var it = map.iterator();
    while (it.next()) |entry| {
        switch (entry.value_ptr.*) {
            .gpr => |assigned| {
                if (assigned == reg) {
                    const offset = allocateStackSlots(stack_slots, 1);
                    entry.value_ptr.* = .{ .spill = offset };
                    try rewritten.append(allocator, .{ .Mov = .{ .dst = .{ .Mem = .{ .base = .rbp, .offset = offset } }, .src = .{ .Phys = assigned } } });
                    return true;
                }
            },
            .xmm => {},
            .spill => {},
        }
    }
    return true;
}

fn freeXmmRegisterIfMapped(
    reg: machine.XmmReg,
    map: *std.AutoHashMap(machine.VReg, Location),
    rewritten: *std.ArrayListUnmanaged(machine.InstKind),
    allocator: std.mem.Allocator,
    stack_slots: *usize,
) AllocError!bool {
    var it = map.iterator();
    while (it.next()) |entry| {
        switch (entry.value_ptr.*) {
            .gpr => {},
            .xmm => |assigned| {
                if (assigned == reg) {
                    const offset = allocateStackSlots(stack_slots, 2);
                    entry.value_ptr.* = .{ .spill = offset };
                    try rewritten.append(allocator, .{ .Movsd = .{ .dst = .{ .Mem = .{ .base = .rbp, .offset = offset } }, .src = .{ .Xmm = assigned } } });
                    return true;
                }
            },
            .spill => {},
        }
    }
    return true;
}

/// Liveness-aware materializeRead using free register tracking.
fn materializeReadWithFreeRegs(
    func: *machine.MachineFn,
    operand: machine.MOperand,
    map: *std.AutoHashMap(machine.VReg, Location),
    classes: *std.AutoHashMap(machine.VReg, RegisterClass),
    class: RegisterClass,
    free_gprs: *std.AutoHashMap(machine.PhysReg, void),
    free_xmms: *std.AutoHashMap(machine.XmmReg, void),
    allocator: std.mem.Allocator,
    stack_slots: *usize,
    diagnostics: *diag.Diagnostics,
) AllocError!machine.MOperand {
    return switch (operand) {
        .VReg => |vreg| toOperand(try assignWithFreeRegs(func, vreg, map, classes, class, free_gprs, free_xmms, allocator, stack_slots, diagnostics)),
        else => operand,
    };
}

/// Liveness-aware materializeWrite using free register tracking.
fn materializeWriteWithFreeRegs(
    func: *machine.MachineFn,
    operand: machine.MOperand,
    map: *std.AutoHashMap(machine.VReg, Location),
    classes: *std.AutoHashMap(machine.VReg, RegisterClass),
    class: RegisterClass,
    free_gprs: *std.AutoHashMap(machine.PhysReg, void),
    free_xmms: *std.AutoHashMap(machine.XmmReg, void),
    allocator: std.mem.Allocator,
    stack_slots: *usize,
    diagnostics: *diag.Diagnostics,
) AllocError!machine.MOperand {
    return switch (operand) {
        .VReg => |vreg| toOperand(try assignWithFreeRegs(func, vreg, map, classes, class, free_gprs, free_xmms, allocator, stack_slots, diagnostics)),
        else => operand,
    };
}

/// Liveness-aware spillIfMapped that updates free register set.
fn spillIfMappedWithFreeRegs(
    reg: machine.PhysReg,
    map: *std.AutoHashMap(machine.VReg, Location),
    rewritten: *std.ArrayListUnmanaged(machine.InstKind),
    allocator: std.mem.Allocator,
    stack_slots: *usize,
    free_gprs: *std.AutoHashMap(machine.PhysReg, void),
) AllocError!void {
    var it = map.iterator();
    while (it.next()) |entry| {
        if (entry.value_ptr.* == .gpr and entry.value_ptr.gpr == reg) {
            const offset = allocateStackSlots(stack_slots, 1);
            try rewritten.append(allocator, .{ .Mov = .{ .dst = .{ .Mem = .{ .base = .rbp, .offset = offset } }, .src = .{ .Phys = reg } } });
            entry.value_ptr.* = .{ .spill = offset };
            // Register is now spilled, mark it as free
            try free_gprs.put(reg, {});
            break;
        }
    }
}

/// Liveness-aware acquireScratch using free register tracking.
fn acquireScratchWithFreeRegs(
    class: RegisterClass,
    map: *std.AutoHashMap(machine.VReg, Location),
    rewritten: *std.ArrayListUnmanaged(machine.InstKind),
    allocator: std.mem.Allocator,
    stack_slots: *usize,
    free_gprs: *std.AutoHashMap(machine.PhysReg, void),
    free_xmms: *std.AutoHashMap(machine.XmmReg, void),
) AllocError!Scratch {
    switch (class) {
        .gpr => {
            // First try to get a free register
            var it = free_gprs.keyIterator();
            if (it.next()) |reg_ptr| {
                const reg = reg_ptr.*;
                _ = free_gprs.remove(reg);
                return .{ .gpr = reg };
            }
            // Fall back to spilling
            for (scratch_gprs) |candidate| {
                if (try freeGprRegisterIfMapped(candidate, map, rewritten, allocator, stack_slots)) {
                    return .{ .gpr = candidate };
                }
            }
            return error.OutOfRegisters;
        },
        .xmm => {
            // First try to get a free register
            var it = free_xmms.keyIterator();
            if (it.next()) |reg_ptr| {
                const reg = reg_ptr.*;
                _ = free_xmms.remove(reg);
                return .{ .xmm = reg };
            }
            // Fall back to spilling
            for (scratch_xmms) |candidate| {
                if (try freeXmmRegisterIfMapped(candidate, map, rewritten, allocator, stack_slots)) {
                    return .{ .xmm = candidate };
                }
            }
            return error.OutOfRegisters;
        },
    }
}

test "regalloc records callee-saved usage" {
    var diagnostics = diag.Diagnostics.init(std.testing.allocator);
    defer diagnostics.deinit();

    const insts = try std.testing.allocator.alloc(machine.InstKind, caller_saved_gprs.len + 1);
    for (insts, 0..) |*inst, idx| {
        inst.* = .{ .Mov = .{ .dst = .{ .VReg = @intCast(idx) }, .src = .{ .Imm = 0 } } };
    }

    var blocks = try std.testing.allocator.alloc(machine.MachineBlock, 1);
    blocks[0] = .{ .id = 0, .insts = insts };

    var func = machine.MachineFn{
        .name = "callee_saves",
        .blocks = blocks,
        .stack_size = 0,
        .vreg_count = insts.len,
        .callee_saved_gprs = std.array_list.Managed(machine.CalleeSavedGpr).init(std.testing.allocator),
        .callee_saved_xmms = std.array_list.Managed(machine.CalleeSavedXmm).init(std.testing.allocator),
    };
    defer func.deinit(std.testing.allocator);

    try allocateRegisters(&func, std.testing.allocator, &diagnostics);

    const saved = func.callee_saved_gprs.items[0..func.callee_saved_gprs.items.len];
    try std.testing.expectEqual(@as(usize, 1), saved.len);
    try std.testing.expectEqual(machine.PhysReg.rbx, saved[0].reg);
    try std.testing.expectEqual(@as(usize, 8), func.stack_size);
    try std.testing.expectEqual(machine.MOperand{ .Phys = .rbx }, func.blocks[0].insts[caller_saved_gprs.len].Mov.dst);
}
