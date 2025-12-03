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

    var phys_used_gpr: usize = 0;
    var phys_used_xmm: usize = 0;
    var stack_slots: usize = func.stack_size / @sizeOf(i64);

    for (func.blocks) |*block| {
        var rewritten = std.ArrayListUnmanaged(machine.InstKind){};
        defer rewritten.deinit(allocator);

        for (block.insts) |*inst| {
            rewriteOperands(
                func,
                allocator,
                inst,
                &rewritten,
                &map,
                &classes,
                available_gprs,
                available_xmms,
                &phys_used_gpr,
                &phys_used_xmm,
                &stack_slots,
                diagnostics,
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
