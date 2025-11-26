const std = @import("std");
const machine = @import("machine.zig");

pub fn emitAssembly(allocator: std.mem.Allocator, mc: *const machine.MachineCrate) ![]u8 {
    var buffer = std.ArrayList(u8).init(allocator);
    errdefer buffer.deinit();

    var writer = buffer.writer();

    for (mc.fns) |func| {
        try writer.print("global {s}\n{s}:\n", .{ func.name, func.name });
        try emitPrologue(&writer, func.stack_size);

        for (func.blocks) |block| {
            try writer.print(".L{s}_{d}:\n", .{ func.name, block.id });
            for (block.insts) |inst| {
                try emitInst(&writer, inst, func.name);
            }
        }

        try writer.writeAll("    leave\n    ret\n\n");
    }

    return try buffer.toOwnedSlice();
}

fn emitPrologue(writer: anytype, stack_size: usize) !void {
    try writer.writeAll("    push rbp\n    mov rbp, rsp\n");
    if (stack_size > 0) {
        try writer.print("    sub rsp, {d}\n", .{stack_size});
    }
}

fn emitInst(writer: anytype, inst: machine.InstKind, fn_name: []const u8) !void {
    switch (inst) {
        .Mov => |payload| try writer.print("    mov {s}, {s}\n", .{ fmtOperand(payload.dst), fmtOperand(payload.src) }),
        .Bin => |payload| try writer.print("    {s} {s}, {s}\n", .{ binMnemonic(payload.op), fmtOperand(payload.dst), fmtOperand(payload.rhs) }),
        .Unary => |payload| try writer.print("    {s} {s}\n", .{ unaryMnemonic(payload.op), fmtOperand(payload.dst) }),
        .Cmp => |payload| try writer.print("    cmp {s}, {s}\n", .{ fmtOperand(payload.lhs), fmtOperand(payload.rhs) }),
        .Setcc => |payload| try writer.print("    set{s} {s}\n", .{ condSuffix(payload.cond), fmtOperand(payload.dst) }),
        .Test => |payload| try writer.print("    test {s}, {s}\n", .{ fmtOperand(payload.operand), fmtOperand(payload.operand) }),
        .Jmp => |target| try writer.print("    jmp .L{s}_{d}\n", .{ fn_name, target }),
        .Jcc => |payload| try writer.print("    j{s} .L{s}_{d}\n", .{ condSuffix(payload.cond), fn_name, payload.target }),
        .Call => |name| try writer.print("    call {s}\n", .{name}),
        .Ret => |_| {},
    }
}

fn binMnemonic(op: machine.BinOpcode) []const u8 {
    return switch (op) {
        .add => "add",
        .sub => "sub",
        .imul => "imul",
        .idiv => "idiv",
        .imod => "idiv", // remainder requires idiv; follow-up lowering can insert moves
        .and_ => "and",
        .or_ => "or",
        .xor_ => "xor",
    };
}

fn unaryMnemonic(op: machine.UnaryOpcode) []const u8 {
    return switch (op) {
        .not_ => "not",
        .neg => "neg",
    };
}

fn condSuffix(cond: machine.Condition) []const u8 {
    return switch (cond) {
        .eq => "e",
        .ne => "ne",
        .lt => "l",
        .le => "le",
        .gt => "g",
        .ge => "ge",
    };
}

fn fmtOperand(op: machine.MOperand) []const u8 {
    return switch (op) {
        .Phys => |reg| physName(reg),
        .Imm => |val| blk: {
            var buf: [32]u8 = undefined;
            const text = std.fmt.bufPrint(&buf, "{d}", .{val}) catch return "0";
            break :blk text;
        },
        .Mem => |mem| blk: {
            var buf: [32]u8 = undefined;
            const off = if (mem.offset < 0) mem.offset else mem.offset;
            const text = std.fmt.bufPrint(&buf, "[{s}{s}{d}]", .{
                physName(mem.base),
                if (mem.offset < 0) "" else "+",
                off,
            }) catch return "[rbp]";
            break :blk text;
        },
        .VReg => |id| blk: {
            var buf: [24]u8 = undefined;
            const text = std.fmt.bufPrint(&buf, "v{d}", .{id}) catch return "v";
            break :blk text;
        },
    };
}

fn physName(reg: machine.PhysReg) []const u8 {
    return switch (reg) {
        .rax => "rax",
        .rbx => "rbx",
        .rcx => "rcx",
        .rdx => "rdx",
        .rsi => "rsi",
        .rdi => "rdi",
        .r8 => "r8",
        .r9 => "r9",
        .r10 => "r10",
        .r11 => "r11",
        .rbp => "rbp",
        .rsp => "rsp",
    };
}
