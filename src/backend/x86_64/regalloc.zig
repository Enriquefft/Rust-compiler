const std = @import("std");
const diag = @import("../../diag/diagnostics.zig");
const source_map = @import("../../diag/source_map.zig");
const machine = @import("machine.zig");

const zero_span = source_map.Span{ .file_id = 0, .start = 0, .end = 0 };

pub const AllocError = error{ OutOfRegisters, OutOfMemory };

const Location = union(enum) {
    phys: machine.PhysReg,
    spill: i32,
};

pub fn allocateRegisters(
    func: *machine.MachineFn,
    allocator: std.mem.Allocator,
    diagnostics: *diag.Diagnostics,
) AllocError!void {
    var map = std.AutoHashMap(machine.VReg, Location).init(allocator);
    defer map.deinit();

    const available = [_]machine.PhysReg{
        .rax, .rbx, .rcx, .rdx, .rsi, .rdi, .r8, .r9, .r10,
    };
    const spill_scratch: machine.PhysReg = .r11;

    var phys_used: usize = 0;
    var spill_slots: usize = func.stack_size / @sizeOf(i64);

    for (func.blocks) |*block| {
        var rewritten = std.ArrayListUnmanaged(machine.InstKind){};
        defer rewritten.deinit(allocator);

        for (block.insts) |*inst| {
            rewriteOperands(
                allocator,
                inst,
                &rewritten,
                &map,
                &available,
                spill_scratch,
                &phys_used,
                &spill_slots,
                diagnostics,
            ) catch |err| switch (err) {
                error.OutOfRegisters => return err,
                else => return err,
            };
        }

        allocator.free(block.insts);
        block.insts = try rewritten.toOwnedSlice(allocator);
    }

    func.stack_size = spill_slots * @sizeOf(i64);
}

fn rewriteOperands(
    allocator: std.mem.Allocator,
    inst: *machine.InstKind,
    rewritten: *std.ArrayListUnmanaged(machine.InstKind),
    map: *std.AutoHashMap(machine.VReg, Location),
    available: []const machine.PhysReg,
    spill_scratch: machine.PhysReg,
    phys_used: *usize,
    spill_slots: *usize,
    diagnostics: *diag.Diagnostics,
) AllocError!void {
    switch (inst.*) {
        .Mov => |*payload| {
            const dst = try materializeWrite(payload.dst, map, available, spill_scratch, phys_used, spill_slots, diagnostics);
            var src = try materializeRead(payload.src, map, available, spill_scratch, phys_used, spill_slots, diagnostics);

            if (isMem(dst) and isMem(src)) {
                try rewritten.append(allocator, .{ .Mov = .{ .dst = .{ .Phys = spill_scratch }, .src = src } });
                src = .{ .Phys = spill_scratch };
            }

            try rewritten.append(allocator, .{ .Mov = .{ .dst = dst, .src = src } });
        },
        .Bin => |*payload| {
            const dst = try materializeWrite(payload.dst, map, available, spill_scratch, phys_used, spill_slots, diagnostics);
            _ = try materializeRead(payload.lhs, map, available, spill_scratch, phys_used, spill_slots, diagnostics);
            var rhs = try materializeRead(payload.rhs, map, available, spill_scratch, phys_used, spill_slots, diagnostics);

            if (isMem(dst) and isMem(rhs)) {
                try rewritten.append(allocator, .{ .Mov = .{ .dst = .{ .Phys = spill_scratch }, .src = rhs } });
                rhs = .{ .Phys = spill_scratch };
            }

            try rewritten.append(allocator, .{ .Bin = .{ .op = payload.op, .dst = dst, .lhs = dst, .rhs = rhs } });
        },
        .Unary => |*payload| {
            const dst = try materializeWrite(payload.dst, map, available, spill_scratch, phys_used, spill_slots, diagnostics);
            const src = try materializeRead(payload.src, map, available, spill_scratch, phys_used, spill_slots, diagnostics);
            try rewritten.append(allocator, .{ .Unary = .{ .op = payload.op, .dst = dst, .src = src } });
        },
        .Cmp => |*payload| {
            const lhs = try materializeRead(payload.lhs, map, available, spill_scratch, phys_used, spill_slots, diagnostics);
            var rhs = try materializeRead(payload.rhs, map, available, spill_scratch, phys_used, spill_slots, diagnostics);

            if (isMem(lhs) and isMem(rhs)) {
                try rewritten.append(allocator, .{ .Mov = .{ .dst = .{ .Phys = spill_scratch }, .src = rhs } });
                rhs = .{ .Phys = spill_scratch };
            }

            try rewritten.append(allocator, .{ .Cmp = .{ .lhs = lhs, .rhs = rhs } });
        },
        .Setcc => |*payload| {
            const dst = try materializeWrite(payload.dst, map, available, spill_scratch, phys_used, spill_slots, diagnostics);
            try rewritten.append(allocator, .{ .Setcc = .{ .cond = payload.cond, .dst = dst } });
        },
        .Test => |*payload| {
            const op = try materializeRead(payload.operand, map, available, spill_scratch, phys_used, spill_slots, diagnostics);
            try rewritten.append(allocator, .{ .Test = .{ .operand = op } });
        },
        .Jmp, .Jcc, .Call, .Ret => try rewritten.append(allocator, inst.*),
    }
}

fn assign(
    vreg: machine.VReg,
    map: *std.AutoHashMap(machine.VReg, Location),
    available: []const machine.PhysReg,
    spill_scratch: machine.PhysReg,
    phys_used: *usize,
    spill_slots: *usize,
    diagnostics: *diag.Diagnostics,
) AllocError!Location {
    if (map.get(vreg)) |loc| return loc;

    if (phys_used.* < available.len) {
        const phys = available[phys_used.*];
        phys_used.* += 1;
        try map.put(vreg, .{ .phys = phys });
        return .{ .phys = phys };
    }

    if (available.len == 0 and spill_scratch == .rax) {
        diagnostics.reportError(zero_span, "ran out of registers during allocation");
        return error.OutOfRegisters;
    }

    spill_slots.* += 1;
    const offset: i32 = -@as(i32, @intCast(spill_slots.*)) * @as(i32, @intCast(@sizeOf(i64)));
    const loc = Location{ .spill = offset };
    try map.put(vreg, loc);
    return loc;
}

fn materializeRead(
    operand: machine.MOperand,
    map: *std.AutoHashMap(machine.VReg, Location),
    available: []const machine.PhysReg,
    spill_scratch: machine.PhysReg,
    phys_used: *usize,
    spill_slots: *usize,
    diagnostics: *diag.Diagnostics,
) AllocError!machine.MOperand {
    return switch (operand) {
        .VReg => |vreg| toOperand(try assign(vreg, map, available, spill_scratch, phys_used, spill_slots, diagnostics)),
        else => operand,
    };
}

fn materializeWrite(
    operand: machine.MOperand,
    map: *std.AutoHashMap(machine.VReg, Location),
    available: []const machine.PhysReg,
    spill_scratch: machine.PhysReg,
    phys_used: *usize,
    spill_slots: *usize,
    diagnostics: *diag.Diagnostics,
) AllocError!machine.MOperand {
    return switch (operand) {
        .VReg => |vreg| toOperand(try assign(vreg, map, available, spill_scratch, phys_used, spill_slots, diagnostics)),
        else => operand,
    };
}

fn toOperand(loc: Location) machine.MOperand {
    return switch (loc) {
        .phys => |reg| .{ .Phys = reg },
        .spill => |offset| .{ .Mem = .{ .base = .rbp, .offset = offset } },
    };
}

fn isMem(op: machine.MOperand) bool {
    return switch (op) {
        .Mem => true,
        else => false,
    };
}
