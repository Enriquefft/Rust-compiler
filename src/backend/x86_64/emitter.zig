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
        try emitCalleeSavedStores(&writer, func);

        for (func.blocks) |block| {
            try writer.print(".L{s}_{d}:\n", .{ func.name, block.id });
            for (block.insts) |inst| {
                try emitInst(&writer, inst, func.name);
            }
        }

        try emitCalleeSavedRestores(&writer, func);
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

fn emitCalleeSavedStores(writer: anytype, func: machine.MachineFn) !void {
    for (func.callee_saved_gprs.items) |entry| {
        try writer.writeAll("    mov qword ptr ");
        try writeMem(writer, .{ .base = .rbp, .offset = entry.offset });
        try writer.writeAll(", ");
        try writeOperand(writer, .{ .Phys = entry.reg });
        try writer.writeByte('\n');
    }

    for (func.callee_saved_xmms.items) |entry| {
        try writer.writeAll("    movsd qword ptr ");
        try writeMem(writer, .{ .base = .rbp, .offset = entry.offset });
        try writer.writeAll(", ");
        try writeOperand(writer, .{ .Xmm = entry.reg });
        try writer.writeByte('\n');
    }
}

fn emitCalleeSavedRestores(writer: anytype, func: machine.MachineFn) !void {
    for (func.callee_saved_gprs.items) |entry| {
        try writer.writeAll("    mov ");
        try writeOperand(writer, .{ .Phys = entry.reg });
        try writer.writeAll(", qword ptr ");
        try writeMem(writer, .{ .base = .rbp, .offset = entry.offset });
        try writer.writeByte('\n');
    }

    for (func.callee_saved_xmms.items) |entry| {
        try writer.writeAll("    movsd ");
        try writeOperand(writer, .{ .Xmm = entry.reg });
        try writer.writeAll(", qword ptr ");
        try writeMem(writer, .{ .base = .rbp, .offset = entry.offset });
        try writer.writeByte('\n');
    }
}

fn emitInst(writer: anytype, inst: machine.InstKind, fn_name: []const u8) !void {
    switch (inst) {
        .Mov => |payload| {
            if (payload.src == .Label and payload.dst == .Mem) {
                try writer.writeAll("    mov qword ptr ");
                try writeMem(writer, payload.dst.Mem);
                try writer.print(", OFFSET FLAT:{s}\n", .{payload.src.Label});
            } else if (payload.src == .Label) {
                try writer.writeAll("    lea ");
                try writeOperand(writer, payload.dst);
                try writer.print(", [rip + {s}]\n", .{payload.src.Label});
            } else if (payload.dst == .Mem and payload.src == .Imm) {
                const imm_val = payload.src.Imm;
                // x86_64 only allows 32-bit signed immediates for memory stores
                // For larger values, we need to use a register
                if (imm_val >= -2147483648 and imm_val <= 2147483647) {
                    try writer.writeAll("    mov qword ptr ");
                    try writeMem(writer, payload.dst.Mem);
                    try writer.writeAll(", ");
                    try writeOperand(writer, payload.src);
                    try writer.writeByte('\n');
                } else {
                    // Need to move through a register for 64-bit immediate
                    try writer.writeAll("    mov r11, ");
                    try writeOperand(writer, payload.src);
                    try writer.writeByte('\n');
                    try writer.writeAll("    mov qword ptr ");
                    try writeMem(writer, payload.dst.Mem);
                    try writer.writeAll(", r11\n");
                }
            } else {
                try writer.writeAll("    mov ");
                try writeOperand(writer, payload.dst);
                try writer.writeAll(", ");
                try writeOperand(writer, payload.src);
                try writer.writeByte('\n');
            }
        },
        .Movsd => |payload| {
            try writer.writeAll("    movsd ");
            try writeOperand(writer, payload.dst);
            try writer.writeAll(", ");
            try writeOperand(writer, payload.src);
            try writer.writeByte('\n');
        },
        .Bin => |payload| {
            if (payload.op == .idiv or payload.op == .imod) {
                // x86-64 idiv is a unary instruction that divides rdx:rax by the operand.
                // Quotient goes to rax, remainder goes to rdx.
                // The Bin instruction has dst containing the current dividend value.
                // We need to:
                // 1. Move dividend (dst) to rax if not already there
                // 2. Sign-extend rax into rdx using cqo
                // 3. Execute idiv with the divisor (rhs)
                // 4. Move result (rax for idiv, rdx for imod) to dst if needed

                // Move dividend to rax if not already there
                if (payload.dst != .Phys or payload.dst.Phys != .rax) {
                    try writer.writeAll("    mov rax, ");
                    try writeOperand(writer, payload.dst);
                    try writer.writeByte('\n');
                }

                // Sign-extend rax into rdx
                try writer.writeAll("    cqo\n");

                // For idiv, the divisor cannot be an immediate - must be in register or memory
                if (payload.rhs == .Imm) {
                    try writer.writeAll("    mov r11, ");
                    try writeOperand(writer, payload.rhs);
                    try writer.writeAll("\n    idiv r11\n");
                } else if (payload.rhs == .Mem) {
                    // Memory operands need size qualifier to avoid ambiguous operand size
                    try writer.writeAll("    idiv qword ptr ");
                    try writeMem(writer, payload.rhs.Mem);
                    try writer.writeByte('\n');
                } else {
                    try writer.writeAll("    idiv ");
                    try writeOperand(writer, payload.rhs);
                    try writer.writeByte('\n');
                }

                // Move result to destination
                if (payload.op == .imod) {
                    // Remainder is in rdx
                    if (payload.dst != .Phys or payload.dst.Phys != .rdx) {
                        try writer.writeAll("    mov ");
                        try writeOperand(writer, payload.dst);
                        try writer.writeAll(", rdx\n");
                    }
                } else {
                    // Quotient is in rax
                    if (payload.dst != .Phys or payload.dst.Phys != .rax) {
                        try writer.writeAll("    mov ");
                        try writeOperand(writer, payload.dst);
                        try writer.writeAll(", rax\n");
                    }
                }
            } else {
                // Handle memory + immediate case: need size qualifier
                if (payload.dst == .Mem and payload.rhs == .Imm) {
                    try writer.print("    {s} qword ptr ", .{binMnemonic(payload.op)});
                    try writeMem(writer, payload.dst.Mem);
                    try writer.writeAll(", ");
                    try writeOperand(writer, payload.rhs);
                    try writer.writeByte('\n');
                } else {
                    try writer.print("    {s} ", .{binMnemonic(payload.op)});
                    try writeOperand(writer, payload.dst);
                    try writer.writeAll(", ");
                    try writeOperand(writer, payload.rhs);
                    try writer.writeByte('\n');
                }
            }
        },
        .Unary => |payload| {
            try writer.print("    {s} ", .{unaryMnemonic(payload.op)});
            try writeOperand(writer, payload.dst);
            try writer.writeByte('\n');
        },
        .Lea => |payload| {
            try writer.writeAll("    lea ");
            try writeOperand(writer, payload.dst);
            try writer.writeAll(", ");
            try writeMem(writer, payload.mem);
            try writer.writeByte('\n');
        },
        .Cvttsd2si => |payload| {
            // cvttsd2si - truncate double to signed integer
            try writer.writeAll("    cvttsd2si ");
            try writeOperand(writer, payload.dst);
            try writer.writeAll(", ");
            try writeOperand(writer, payload.src);
            try writer.writeByte('\n');
        },
        .Cvtsi2sd => |payload| {
            // cvtsi2sd - convert signed integer to double
            try writer.writeAll("    cvtsi2sd ");
            try writeOperand(writer, payload.dst);
            try writer.writeAll(", ");
            try writeOperand(writer, payload.src);
            try writer.writeByte('\n');
        },
        .Deref => |payload| {
            // Dereference: load from the address in addr operand
            // mov dst, [addr]
            try writer.writeAll("    mov ");
            try writeOperand(writer, payload.dst);
            try writer.writeAll(", [");
            switch (payload.addr) {
                .Phys => |reg| try writer.print("{s}", .{physName(reg)}),
                .Mem => |mem| {
                    try writer.writeAll(physName(mem.base));
                    if (mem.offset != 0) {
                        const sign: u8 = if (mem.offset < 0) '-' else '+';
                        try writer.writeByte(sign);
                        const magnitude: i32 = if (mem.offset < 0) -mem.offset else mem.offset;
                        try writer.print("{d}", .{magnitude});
                    }
                },
                else => try writeOperand(writer, payload.addr),
            }
            try writer.writeAll("]\n");
        },
        .StoreDeref => |payload| {
            // Store to dereference: [addr] = src
            // mov [addr], src
            try writer.writeAll("    mov qword ptr [");
            switch (payload.addr) {
                .Phys => |reg| try writer.print("{s}", .{physName(reg)}),
                .Mem => |mem| {
                    try writer.writeAll(physName(mem.base));
                    if (mem.offset != 0) {
                        const sign: u8 = if (mem.offset < 0) '-' else '+';
                        try writer.writeByte(sign);
                        const magnitude: i32 = if (mem.offset < 0) -mem.offset else mem.offset;
                        try writer.print("{d}", .{magnitude});
                    }
                },
                else => try writeOperand(writer, payload.addr),
            }
            try writer.writeAll("], ");
            try writeOperand(writer, payload.src);
            try writer.writeByte('\n');
        },
        .Cmp => |payload| {
            // try writer.writeAll("    cmp ");
            // try writeOperand(writer, payload.lhs);
            // try writer.writeAll(", ");
            // try writeOperand(writer, payload.rhs);
            // try writer.writeByte('\n');

            // Handle memory + immediate case: need size qualifier
            if (payload.lhs == .Mem and payload.rhs == .Imm) {
                try writer.writeAll("    cmp qword ptr ");
                try writeMem(writer, payload.lhs.Mem);
                try writer.writeAll(", ");
                try writeOperand(writer, payload.rhs);
                try writer.writeByte('\n');
            } else {
                try writer.writeAll("    cmp ");
                try writeOperand(writer, payload.lhs);
                try writer.writeAll(", ");
                try writeOperand(writer, payload.rhs);
                try writer.writeByte('\n');
            }
        },
        .Setcc => |payload| {
            try writer.print("    set{s} ", .{condSuffix(payload.cond)});
            switch (payload.dst) {
                .Phys => |reg| try writer.writeAll(physByteName(reg)),
                .Mem => |mem| {
                    try writer.writeAll("byte ptr ");
                    try writeMem(writer, mem);
                },
                .VReg => |id| try writer.print("v{d}", .{id}),
                else => unreachable,
            }
            try writer.writeByte('\n');
        },
        .Test => |payload| {
            try writer.writeAll("    test ");
            try writeOperand(writer, payload.operand);
            try writer.writeAll(", ");
            try writeOperand(writer, payload.operand);
            try writer.writeByte('\n');
        },
        .Push => |payload| {
            try writer.writeAll("    push ");
            try writeOperand(writer, payload);
            try writer.writeByte('\n');
        },
        .Add => |payload| {
            try writer.writeAll("    add ");
            try writeOperand(writer, payload.dst);
            try writer.writeAll(", ");
            try writeOperand(writer, payload.src);
            try writer.writeByte('\n');
        },
        .Jmp => |target| try writer.print("    jmp .L{s}_{d}\n", .{ fn_name, target }),
        .Jcc => |payload| try writer.print("    j{s} .L{s}_{d}\n", .{ condSuffix(payload.cond), fn_name, payload.target }),
        .Call => |target| switch (target) {
            .Direct => |name| try writer.print("    call {s}\n", .{name}),
            .Indirect => |op| {
                try writer.writeAll("    call ");
                try writeOperand(writer, op);
                try writer.writeByte('\n');
            },
        },
        .Ret => |_| try writer.writeAll("    leave\n    ret\n"),
    }
}

fn binMnemonic(op: machine.BinOpcode) []const u8 {
    return switch (op) {
        .add => "add",
        .sub => "sub",
        .imul => "imul",
        .idiv => "idiv",
        .imod => "idiv", // remainder is obtained via idiv; result handling is in emitter
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
        .Xmm => |reg| try writer.writeAll(xmmName(reg)),
        .Imm => |val| try writer.print("{d}", .{val}),
        .Label => |label| try writer.writeAll(label),
        .Mem => |mem| {
            try writeMem(writer, mem);
        },
        .VReg => |id| try writer.print("v{d}", .{id}),
    }
}

fn xmmName(reg: machine.XmmReg) []const u8 {
    return switch (reg) {
        .xmm0 => "xmm0",
        .xmm1 => "xmm1",
        .xmm2 => "xmm2",
        .xmm3 => "xmm3",
        .xmm4 => "xmm4",
        .xmm5 => "xmm5",
        .xmm6 => "xmm6",
        .xmm7 => "xmm7",
    };
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
        .r12 => "r12",
        .r13 => "r13",
        .r14 => "r14",
        .r15 => "r15",
        .rbp => "rbp",
        .rsp => "rsp",
    };
}

fn physByteName(reg: machine.PhysReg) []const u8 {
    return switch (reg) {
        .rax => "al",
        .rbx => "bl",
        .rcx => "cl",
        .rdx => "dl",
        .rsi => "sil",
        .rdi => "dil",
        .r8 => "r8b",
        .r9 => "r9b",
        .r10 => "r10b",
        .r11 => "r11b",
        .r12 => "r12b",
        .r13 => "r13b",
        .r14 => "r14b",
        .r15 => "r15b",
        .rbp => "bpl",
        .rsp => "spl",
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
        .callee_saved_gprs = std.array_list.Managed(machine.CalleeSavedGpr).init(std.testing.allocator),
        .callee_saved_xmms = std.array_list.Managed(machine.CalleeSavedXmm).init(std.testing.allocator),
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
        "    cmp qword ptr [rbp-16], 0\n" ++
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
    insts[2] = .{ .Call = .{ .Direct = "printf" } };

    var blocks = try std.testing.allocator.alloc(machine.MachineBlock, 1);
    blocks[0] = .{ .id = 0, .insts = insts };

    var funcs = try std.testing.allocator.alloc(machine.MachineFn, 1);
    funcs[0] = .{
        .name = "println",
        .blocks = blocks,
        .stack_size = 0,
        .vreg_count = 0,
        .callee_saved_gprs = std.array_list.Managed(machine.CalleeSavedGpr).init(std.testing.allocator),
        .callee_saved_xmms = std.array_list.Managed(machine.CalleeSavedXmm).init(std.testing.allocator),
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

test "emitter saves callee-preserved registers" {
    var blocks = try std.testing.allocator.alloc(machine.MachineBlock, 1);
    blocks[0] = .{ .id = 0, .insts = try std.testing.allocator.alloc(machine.InstKind, 0) };

    var gprs = std.array_list.Managed(machine.CalleeSavedGpr).init(std.testing.allocator);
    try gprs.append(.{ .reg = .rbx, .offset = -8 });

    var xmms = std.array_list.Managed(machine.CalleeSavedXmm).init(std.testing.allocator);
    try xmms.append(.{ .reg = .xmm6, .offset = -24 });

    var funcs = try std.testing.allocator.alloc(machine.MachineFn, 1);
    funcs[0] = .{
        .name = "save_demo",
        .blocks = blocks,
        .stack_size = 32,
        .vreg_count = 0,
        .callee_saved_gprs = gprs,
        .callee_saved_xmms = xmms,
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
        ".globl save_demo\n" ++
        "save_demo:\n" ++
        "    push rbp\n    mov rbp, rsp\n" ++
        "    sub rsp, 32\n" ++
        "    mov qword ptr [rbp-8], rbx\n" ++
        "    movsd qword ptr [rbp-24], xmm6\n" ++
        ".Lsave_demo_0:\n" ++
        "    mov rbx, qword ptr [rbp-8]\n" ++
        "    movsd xmm6, qword ptr [rbp-24]\n" ++
        "    leave\n    ret\n\n" ++
        ".section .note.GNU-stack,\"\",@progbits\n";

    try std.testing.expectEqualStrings(expected, assembly);
}
