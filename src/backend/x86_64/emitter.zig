const std = @import("std");
const machine = @import("machine.zig");

pub fn emitAssembly(allocator: std.mem.Allocator, mc: *const machine.MachineCrate) ![]u8 {
    var buffer = std.ArrayListUnmanaged(u8){};
    defer buffer.deinit(allocator);

    var writer = buffer.writer(allocator);
    try writer.writeAll("; x86_64 assembly\n");

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

    return try buffer.toOwnedSlice(allocator);
}

fn emitPrologue(writer: anytype, stack_size: usize) !void {
    try writer.writeAll("    push rbp\n    mov rbp, rsp\n");
    if (stack_size > 0) {
        try writer.print("    sub rsp, {d}\n", .{stack_size});
    }
}

fn emitInst(writer: anytype, inst: machine.InstKind, fn_name: []const u8) !void {
    switch (inst) {
        .Mov => |payload| {
            try writer.writeAll("    mov ");
            try writeOperand(writer, payload.dst);
            try writer.writeAll(", ");
            try writeOperand(writer, payload.src);
            try writer.writeByte('\n');
        },
        .Bin => |payload| {
            try writer.print("    {s} ", .{binMnemonic(payload.op)});
            try writeOperand(writer, payload.dst);
            try writer.writeAll(", ");
            try writeOperand(writer, payload.rhs);
            try writer.writeByte('\n');
        },
        .Unary => |payload| {
            try writer.print("    {s} ", .{unaryMnemonic(payload.op)});
            try writeOperand(writer, payload.dst);
            try writer.writeByte('\n');
        },
        .Cmp => |payload| {
            try writer.writeAll("    cmp ");
            try writeOperand(writer, payload.lhs);
            try writer.writeAll(", ");
            try writeOperand(writer, payload.rhs);
            try writer.writeByte('\n');
        },
        .Setcc => |payload| {
            try writer.print("    set{s} ", .{condSuffix(payload.cond)});
            try writeOperand(writer, payload.dst);
            try writer.writeByte('\n');
        },
        .Test => |payload| {
            try writer.writeAll("    test ");
            try writeOperand(writer, payload.operand);
            try writer.writeAll(", ");
            try writeOperand(writer, payload.operand);
            try writer.writeByte('\n');
        },
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

fn writeOperand(writer: anytype, op: machine.MOperand) !void {
    switch (op) {
        .Phys => |reg| try writer.writeAll(physName(reg)),
        .Imm => |val| try writer.print("{d}", .{val}),
        .Mem => |mem| {
            try writer.writeByte('[');
            try writer.writeAll(physName(mem.base));
            if (mem.offset != 0) {
                const sign: u8 = if (mem.offset < 0) '-' else '+';
                try writer.writeByte(sign);
                const magnitude: i32 = if (mem.offset < 0) -mem.offset else mem.offset;
                try writer.print("{d}", .{magnitude});
            } else {
                try writer.writeAll("+0");
            }
            try writer.writeByte(']');
        },
        .VReg => |id| try writer.print("v{d}", .{id}),
    }
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

test "emitter streams operands directly to writer" {
    var insts = try std.testing.allocator.alloc(machine.InstKind, 3);
    insts[0] = .{ .Mov = .{ .dst = .{ .Phys = .rax }, .src = .{ .Imm = 42 } } };
    insts[1] = .{ .Cmp = .{ .lhs = .{ .Mem = .{ .base = .rbp, .offset = -16 } }, .rhs = .{ .Imm = 0 } } };
    insts[2] = .{ .Bin = .{ .op = .add, .dst = .{ .VReg = 1 }, .lhs = .{ .VReg = 1 }, .rhs = .{ .Mem = .{ .base = .rbp, .offset = 8 } } } };

    var blocks = try std.testing.allocator.alloc(machine.MachineBlock, 1);
    blocks[0] = .{ .id = 0, .insts = insts };

    var funcs = try std.testing.allocator.alloc(machine.MachineFn, 1);
    funcs[0] = .{
        .name = "emit_operands",
        .blocks = blocks,
        .stack_size = 16,
        .vreg_count = 2,
    };

    var mc = machine.MachineCrate{ .allocator = std.testing.allocator, .fns = funcs };
    defer mc.deinit();

    const assembly = try emitAssembly(std.testing.allocator, &mc);
    defer std.testing.allocator.free(assembly);

    const expected = "; x86_64 assembly\n" ++
        "global emit_operands\n" ++
        "emit_operands:\n" ++
        "    push rbp\n    mov rbp, rsp\n" ++
        "    sub rsp, 16\n" ++
        ".Lemit_operands_0:\n" ++
        "    mov rax, 42\n" ++
        "    cmp [rbp-16], 0\n" ++
        "    add v1, [rbp+8]\n" ++
        "    leave\n    ret\n\n";

    try std.testing.expectEqualStrings(expected, assembly);
}
