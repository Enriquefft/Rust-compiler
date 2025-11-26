const std = @import("std");
const diag = @import("../../diag/diagnostics.zig");
const source_map = @import("../../diag/source_map.zig");
const machine = @import("machine.zig");

const zero_span = source_map.Span{ .file_id = 0, .start = 0, .end = 0 };

pub const AllocError = error{OutOfRegisters};

pub fn allocateRegisters(func: *machine.MachineFn, diagnostics: *diag.Diagnostics) AllocError!void {
    var map = std.AutoHashMap(machine.VReg, machine.PhysReg).init(std.heap.page_allocator);
    defer map.deinit();

    const available = [_]machine.PhysReg{
        .rax, .rbx, .rcx, .rdx, .rsi, .rdi, .r8, .r9, .r10, .r11,
    };

    for (func.blocks) |*block| {
        for (block.insts) |*inst| {
            rewriteOperands(inst, &map, &available, diagnostics) catch |err| switch (err) {
                error.OutOfRegisters => return err,
                else => return err,
            };
        }
    }
}

fn rewriteOperands(
    inst: *machine.InstKind,
    map: *std.AutoHashMap(machine.VReg, machine.PhysReg),
    available: []const machine.PhysReg,
    diagnostics: *diag.Diagnostics,
) AllocError!void {
    switch (inst.*) {
        .Mov => |*payload| {
            payload.dst = try materialize(payload.dst, map, available, diagnostics);
            payload.src = try materialize(payload.src, map, available, diagnostics);
        },
        .Bin => |*payload| {
            payload.dst = try materialize(payload.dst, map, available, diagnostics);
            payload.lhs = try materialize(payload.lhs, map, available, diagnostics);
            payload.rhs = try materialize(payload.rhs, map, available, diagnostics);
        },
        .Unary => |*payload| {
            payload.dst = try materialize(payload.dst, map, available, diagnostics);
            payload.src = try materialize(payload.src, map, available, diagnostics);
        },
        .Cmp => |*payload| {
            payload.lhs = try materialize(payload.lhs, map, available, diagnostics);
            payload.rhs = try materialize(payload.rhs, map, available, diagnostics);
        },
        .Setcc => |*payload| {
            payload.dst = try materialize(payload.dst, map, available, diagnostics);
        },
        .Test => |*payload| {
            payload.operand = try materialize(payload.operand, map, available, diagnostics);
        },
        .Jmp, .Jcc, .Call, .Ret => {},
    }
}

fn materialize(
    operand: machine.MOperand,
    map: *std.AutoHashMap(machine.VReg, machine.PhysReg),
    available: []const machine.PhysReg,
    diagnostics: *diag.Diagnostics,
) AllocError!machine.MOperand {
    return switch (operand) {
        .VReg => |vreg| .{ .Phys = try assign(vreg, map, available, diagnostics) },
        else => operand,
    };
}

fn assign(
    vreg: machine.VReg,
    map: *std.AutoHashMap(machine.VReg, machine.PhysReg),
    available: []const machine.PhysReg,
    diagnostics: *diag.Diagnostics,
) AllocError!machine.PhysReg {
    if (map.get(vreg)) |phys| return phys;

    if (map.count() >= available.len) {
        diagnostics.reportError(zero_span, "ran out of registers during allocation");
        return error.OutOfRegisters;
    }

    const phys = available[map.count()];
    try map.put(vreg, phys);
    return phys;
}
