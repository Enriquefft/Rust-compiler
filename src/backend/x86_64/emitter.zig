const std = @import("std");
const machine = @import("machine.zig");

pub fn emitAssembly(allocator: std.mem.Allocator, mc: *const machine.MachineCrate) ![]u8 {
    var buffer = std.ArrayListUnmanaged(u8){};
    defer buffer.deinit(allocator);

    var writer = buffer.writer(allocator);
    try writer.writeAll("# x86_64 assembly\n.intel_syntax noprefix\n");

    if (mc.externs.len > 0) {
        for (mc.externs) |name| {
            try writer.print(".extern {s}\n", .{name});
        }
        try writer.writeByte('\n');
    }

    if (mc.rodata.len > 0) {
        try writer.writeAll(".section .rodata\n");
        for (mc.rodata) |item| {
            try writer.print("{s}:\n    .byte ", .{item.label});
            for (item.bytes, 0..) |byte, idx| {
                if (idx != 0) try writer.writeAll(", ");
                try writer.print("{d}", .{byte});
            }
            if (item.bytes.len > 0) try writer.writeAll(", ");
            try writer.writeAll("0\n");
        }
        try writer.writeByte('\n');
    }

    if (mc.fns.len > 0) {
        try writer.writeAll(".text\n");
    }

    for (mc.fns) |func| {
        try writer.print(".globl {s}\n{s}:\n", .{ func.name, func.name });
        try emitPrologue(&writer, func.stack_size);

        for (func.blocks) |block| {
            try writer.print(".L{s}_{d}:\n", .{ func.name, block.id });
            for (block.insts) |inst| {
                try emitInst(&writer, inst, func.name);
            }
        }

        try writer.writeAll("    leave\n    ret\n\n");
    }

    try writer.writeAll(".section .note.GNU-stack,\"\",@progbits\n");

    return try buffer.toOwnedSlice(allocator);
}

fn emitPrologue(writer: anytype, stack_size: usize) !void {
    try writer.writeAll("    push rbp\n    mov rbp, rsp\n");
    const aligned_size = blk: {
        const pad = (16 - (stack_size % 16)) % 16;
        break :blk stack_size + pad;
    };
    if (aligned_size > 0) {
        try writer.print("    sub rsp, {d}\n", .{aligned_size});
    }
}

fn emitInst(writer: anytype, inst: machine.InstKind, fn_name: []const u8) !void {
    switch (inst) {
        .Mov => |payload| {
            if (payload.src == .Label) {
                try writer.writeAll("    lea ");
                try writeOperand(writer, payload.dst);
                try writer.print(", [rip + {s}]\n", .{payload.src.Label});
            } else if (payload.dst == .Mem and payload.src == .Imm) {
                try writer.writeAll("    mov qword ptr ");
                try writeMem(writer, payload.dst.Mem);
                try writer.writeAll(", ");
                try writeOperand(writer, payload.src);
                try writer.writeByte('\n');
            } else {
                try writer.writeAll("    mov ");
                try writeOperand(writer, payload.dst);
                try writer.writeAll(", ");
                try writeOperand(writer, payload.src);
                try writer.writeByte('\n');
            }
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
        .Label => |label| try writer.writeAll(label),
        .Mem => |mem| {
            try writeMem(writer, mem);
        },
        .VReg => |id| try writer.print("v{d}", .{id}),
    }
}

fn writeMem(writer: anytype, mem: machine.MemRef) !void {
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

    const empty_data = try std.testing.allocator.alloc(machine.DataItem, 0);
    const empty_externs = try std.testing.allocator.alloc([]const u8, 0);

    var mc = machine.MachineCrate{ .allocator = std.testing.allocator, .fns = funcs, .rodata = empty_data, .externs = empty_externs };
    defer mc.deinit();

    const assembly = try emitAssembly(std.testing.allocator, &mc);
    defer std.testing.allocator.free(assembly);

    const expected = "# x86_64 assembly\n" ++
        ".intel_syntax noprefix\n" ++
        ".text\n" ++
        ".globl emit_operands\n" ++
        "emit_operands:\n" ++
        "    push rbp\n    mov rbp, rsp\n" ++
        "    sub rsp, 16\n" ++
        ".Lemit_operands_0:\n" ++
        "    mov rax, 42\n" ++
        "    cmp [rbp-16], 0\n" ++
        "    add v1, [rbp+8]\n" ++
        "    leave\n    ret\n\n" ++
        ".section .note.GNU-stack,\"\",@progbits\n";

    try std.testing.expectEqualStrings(expected, assembly);
}

test "emitter emits rodata and extern call for printf" {
    var data = try std.testing.allocator.alloc(machine.DataItem, 1);
    data[0] = .{
        .label = try std.testing.allocator.dupe(u8, ".Lstr0"),
        .bytes = try std.testing.allocator.dupe(u8, "hello\n"),
    };

    var insts = try std.testing.allocator.alloc(machine.InstKind, 3);
    insts[0] = .{ .Mov = .{ .dst = .{ .Phys = .rdi }, .src = .{ .Label = data[0].label } } };
    insts[1] = .{ .Mov = .{ .dst = .{ .Phys = .rax }, .src = .{ .Imm = 0 } } };
    insts[2] = .{ .Call = "printf" };

    var blocks = try std.testing.allocator.alloc(machine.MachineBlock, 1);
    blocks[0] = .{ .id = 0, .insts = insts };

    var funcs = try std.testing.allocator.alloc(machine.MachineFn, 1);
    funcs[0] = .{
        .name = "println",
        .blocks = blocks,
        .stack_size = 0,
        .vreg_count = 0,
    };

    var externs = try std.testing.allocator.alloc([]const u8, 1);
    externs[0] = try std.testing.allocator.dupe(u8, "printf");

    var mc = machine.MachineCrate{
        .allocator = std.testing.allocator,
        .fns = funcs,
        .rodata = data,
        .externs = externs,
    };
    defer mc.deinit();

    const assembly = try emitAssembly(std.testing.allocator, &mc);
    defer std.testing.allocator.free(assembly);

    const expected = "# x86_64 assembly\n" ++
        ".intel_syntax noprefix\n" ++
        ".extern printf\n\n" ++
        ".section .rodata\n" ++
        ".Lstr0:\n" ++
        "    .byte 104, 101, 108, 108, 111, 10, 0\n\n" ++
        ".text\n" ++
        ".globl println\n" ++
        "println:\n" ++
        "    push rbp\n    mov rbp, rsp\n" ++
        ".Lprintln_0:\n" ++
        "    lea rdi, [rip + .Lstr0]\n" ++
        "    mov rax, 0\n" ++
        "    call printf\n" ++
        "    leave\n    ret\n\n" ++
        ".section .note.GNU-stack,\"\",@progbits\n";

    try std.testing.expectEqualStrings(expected, assembly);
}
