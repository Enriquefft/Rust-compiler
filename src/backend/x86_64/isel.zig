const std = @import("std");
const diag = @import("../../diag/diagnostics.zig");
const source_map = @import("../../diag/source_map.zig");
const mir = @import("../../mir/mir.zig");
const machine = @import("machine.zig");

const zero_span = source_map.Span{ .file_id = 0, .start = 0, .end = 0 };

/// Maximum number of struct fields supported for hash-based field indexing.
/// This limits the field layout to avoid collisions in the hash-based approach.
const MAX_STRUCT_FIELDS: u32 = 4;

/// Multiplier for local variable stack allocation.
/// Each local gets this many 8-byte slots to accommodate arrays.
const LOCAL_STACK_MULTIPLIER: u32 = 4;

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
    // Map from VReg to memory location for Index results (to support Ref(Index))
    index_mem_map: std.AutoHashMap(machine.VReg, machine.MemRef) = undefined,
    // Map from VReg to address VReg for dynamic index results (to support Ref(Index) with dynamic indices)
    dynamic_index_addr_map: std.AutoHashMap(machine.VReg, machine.VReg) = undefined,
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
    
    // Initialize index memory map for this function
    ctx.index_mem_map = std.AutoHashMap(machine.VReg, machine.MemRef).init(ctx.allocator);
    defer ctx.index_mem_map.deinit();
    
    // Initialize dynamic index address map for this function
    ctx.dynamic_index_addr_map = std.AutoHashMap(machine.VReg, machine.VReg).init(ctx.allocator);
    defer ctx.dynamic_index_addr_map.deinit();

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

    const stack_size = func.locals.len * @sizeOf(i64) * LOCAL_STACK_MULTIPLIER;

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
                switch (payload.op) {
                    .Not => {
                        try insts.append(ctx.allocator, .{ .Mov = .{ .dst = .{ .VReg = dst }, .src = src } });
                        try insts.append(ctx.allocator, .{ .Unary = .{ .op = machine.UnaryOpcode.not_, .dst = .{ .VReg = dst }, .src = .{ .VReg = dst } } });
                    },
                    .Neg => {
                        try insts.append(ctx.allocator, .{ .Mov = .{ .dst = .{ .VReg = dst }, .src = src } });
                        try insts.append(ctx.allocator, .{ .Unary = .{ .op = machine.UnaryOpcode.neg, .dst = .{ .VReg = dst }, .src = .{ .VReg = dst } } });
                    },
                    .Ref => {
                        // Address-of operation: get the address of the operand
                        switch (src) {
                            .Mem => |mem| {
                                try insts.append(ctx.allocator, .{ .Lea = .{ .dst = .{ .VReg = dst }, .mem = mem } });
                            },
                            .VReg => |vreg| {
                                // Check if this VReg came from an Index operation (static index)
                                if (ctx.index_mem_map.get(vreg)) |mem| {
                                    // Use LEA with the stored memory location
                                    try insts.append(ctx.allocator, .{ .Lea = .{ .dst = .{ .VReg = dst }, .mem = mem } });
                                } else if (ctx.dynamic_index_addr_map.get(vreg)) |addr_vreg| {
                                    // For dynamic index, the address is already computed in addr_vreg
                                    try insts.append(ctx.allocator, .{ .Mov = .{ .dst = .{ .VReg = dst }, .src = .{ .VReg = addr_vreg } } });
                                } else {
                                    // For other VRegs, just copy (simplified - may not be correct for all cases)
                                    try insts.append(ctx.allocator, .{ .Mov = .{ .dst = .{ .VReg = dst }, .src = src } });
                                }
                            },
                            else => {
                                // For non-memory operands, just copy (simplified)
                                try insts.append(ctx.allocator, .{ .Mov = .{ .dst = .{ .VReg = dst }, .src = src } });
                            },
                        }
                    },
                    .Deref => {
                        // Dereference: load from the address in the operand
                        // We need to get the address value into a register, then load from it
                        switch (src) {
                            .Mem => |mem| {
                                // Source is in memory - first load address to scratch, then deref
                                // Use a temp vreg to hold the address
                                const addr_vreg = destRegister(vreg_count.*, vreg_count);
                                try insts.append(ctx.allocator, .{ .Mov = .{ .dst = .{ .VReg = addr_vreg }, .src = .{ .Mem = mem } } });
                                // Now load from the address. We need Deref instruction support.
                                try insts.append(ctx.allocator, .{ .Deref = .{ .dst = .{ .VReg = dst }, .addr = .{ .VReg = addr_vreg } } });
                            },
                            .VReg => {
                                // Source is a vreg containing the address
                                try insts.append(ctx.allocator, .{ .Deref = .{ .dst = .{ .VReg = dst }, .addr = src } });
                            },
                            else => {
                                try insts.append(ctx.allocator, .{ .Mov = .{ .dst = .{ .VReg = dst }, .src = src } });
                            },
                        }
                    },
                }
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
            
            // For array types, also store additional elements from rdx, rcx etc.
            if (inst.ty) |ty| {
                if (ty == .Array) {
                    // Store 2nd element from rdx at offset 8
                    const mem2 = machine.MOperand{ .Mem = .{ .base = mem.Mem.base, .offset = mem.Mem.offset - 8 } };
                    try insts.append(ctx.allocator, .{ .Mov = .{ .dst = mem2, .src = .{ .Phys = .rdx } } });
                    // Store 3rd element from rcx at offset 16
                    const mem3 = machine.MOperand{ .Mem = .{ .base = mem.Mem.base, .offset = mem.Mem.offset - 16 } };
                    try insts.append(ctx.allocator, .{ .Mov = .{ .dst = mem3, .src = .{ .Phys = .rcx } } });
                }
            }
        },
        .StorePtr => |payload| {
            // Store to a pointer: *ptr = value
            const ptr = try lowerOperand(ctx, payload.ptr, vreg_count);
            const src = try lowerOperand(ctx, payload.src, vreg_count);
            try insts.append(ctx.allocator, .{ .StoreDeref = .{ .addr = ptr, .src = src } });
        },
        .Cast => |payload| {
            if (dest_vreg) |dst| {
                const src = try lowerOperand(ctx, payload.src, vreg_count);
                const from_ty = payload.from_ty;
                const to_ty = payload.to_ty;
                
                // Handle float to int conversion
                if ((from_ty == .F32 or from_ty == .F64) and (to_ty == .I32 or to_ty == .I64 or to_ty == .U32 or to_ty == .U64)) {
                    // cvttsd2si - truncate double/float to signed integer
                    // First load float into xmm register (if not already), then convert
                    // For simplicity, we use a direct conversion instruction
                    try insts.append(ctx.allocator, .{ .Cvttsd2si = .{ .dst = .{ .VReg = dst }, .src = src } });
                } else if ((from_ty == .I32 or from_ty == .I64 or from_ty == .U32 or from_ty == .U64) and (to_ty == .F32 or to_ty == .F64)) {
                    // cvtsi2sd - convert integer to double/float
                    try insts.append(ctx.allocator, .{ .Cvtsi2sd = .{ .dst = .{ .VReg = dst }, .src = src } });
                } else {
                    // Default: just copy
                    try insts.append(ctx.allocator, .{ .Mov = .{ .dst = .{ .VReg = dst }, .src = src } });
                }
            }
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
                                // Stack grows downward, so higher indices are at more negative offsets
                                const scaled: i32 = @intCast(imm * @as(i64, @intCast(@sizeOf(i64))));
                                break :blk2 base_mem.offset - scaled;
                            },
                            .VReg, .Phys, .Mem => {
                                // For variable indices, we need to compute the address dynamically
                                // Load the index into a register, multiply by element size, subtract from base
                                vreg_count.* += 1;
                                const idx_vreg = vreg_count.* - 1;

                                // Move index to vreg if needed
                                switch (idx) {
                                    .VReg => {}, // VReg is already in the correct form, no action needed
                                    .Phys => |reg| {
                                        try insts.append(ctx.allocator, .{ .Mov = .{ .dst = .{ .VReg = idx_vreg }, .src = .{ .Phys = reg } } });
                                    },
                                    .Mem => |mem| {
                                        try insts.append(ctx.allocator, .{ .Mov = .{ .dst = .{ .VReg = idx_vreg }, .src = .{ .Mem = mem } } });
                                    },
                                    else => unreachable,
                                }

                                // Use the vreg if we had to move, otherwise use the original
                                const idx_as_vreg: machine.MOperand = switch (idx) {
                                    .VReg => idx,
                                    .Phys, .Mem => .{ .VReg = idx_vreg },
                                    else => unreachable,
                                };

                                // Multiply index by element size (8 bytes for i64)
                                vreg_count.* += 1;
                                const scaled_vreg = vreg_count.* - 1;
                                try insts.append(ctx.allocator, .{ .Mov = .{ .dst = .{ .VReg = scaled_vreg }, .src = idx_as_vreg } });
                                try insts.append(ctx.allocator, .{ .Bin = .{ .op = .imul, .dst = .{ .VReg = scaled_vreg }, .lhs = .{ .VReg = scaled_vreg }, .rhs = .{ .Imm = @sizeOf(i64) } } });

                                // Load base address into a register
                                vreg_count.* += 1;
                                const base_vreg = vreg_count.* - 1;
                                try insts.append(ctx.allocator, .{ .Lea = .{ .dst = .{ .VReg = base_vreg }, .mem = base_mem } });

                                // Subtract scaled index from base (stack grows downward)
                                try insts.append(ctx.allocator, .{ .Bin = .{ .op = .sub, .dst = .{ .VReg = base_vreg }, .lhs = .{ .VReg = base_vreg }, .rhs = .{ .VReg = scaled_vreg } } });

                                // Load value from computed address into dst
                                try insts.append(ctx.allocator, .{ .Deref = .{ .dst = .{ .VReg = dst }, .addr = .{ .VReg = base_vreg } } });
                                
                                // Track that this VReg holds a value from dynamic indexing
                                // When Ref is applied to this VReg, we use base_vreg (the address) instead
                                try ctx.dynamic_index_addr_map.put(dst, base_vreg);
                                
                                return null;
                            },
                            else => {
                                ctx.diagnostics.reportError(zero_span, "indexing currently supports only immediate indices or variables");
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
                // Record the memory location for this VReg (for potential Ref later)
                try ctx.index_mem_map.put(dst, mem.Mem);
            }
        },
        .Field => |payload| {
            if (dest_vreg) |dst| {
                const target = try lowerOperand(ctx, payload.target, vreg_count);
                const mem = switch (target) {
                    .Mem => |base_mem| blk: {
                        // Basic field layout: assume packed i64 fields and use a deterministic pseudo-offset based on the name.
                        var hash: u32 = 0;
                        for (payload.name) |ch| hash = hash *% 31 +% ch;
                        const field_index: i32 = @intCast(hash % MAX_STRUCT_FIELDS);
                        // Subtract offset since locals grow downward (negative offsets from rbp)
                        const offset = base_mem.offset - field_index * @as(i32, @intCast(@sizeOf(i64)));
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
                    // Store all array elements to consecutive memory locations
                    // First element goes to dst, subsequent elements go to adjacent memory
                    const first = try lowerOperand(ctx, payload.elems[0], vreg_count);
                    try insts.append(ctx.allocator, .{ .Mov = .{ .dst = .{ .VReg = dst }, .src = first } });
                    
                    // For additional elements, use designated registers that StoreLocal will pick up
                    // rdx for 2nd element, rcx for 3rd element (matches StoreLocal handling)
                    for (payload.elems[1..], 1..) |elem, idx| {
                        const elem_op = try lowerOperand(ctx, elem, vreg_count);
                        vreg_count.* += 1;
                        const elem_vreg = vreg_count.* - 1;
                        try insts.append(ctx.allocator, .{ .Mov = .{ .dst = .{ .VReg = elem_vreg }, .src = elem_op } });
                        if (idx == 1) {
                            try insts.append(ctx.allocator, .{ .Mov = .{ .dst = .{ .Phys = .rdx }, .src = .{ .VReg = elem_vreg } } });
                        } else if (idx == 2) {
                            try insts.append(ctx.allocator, .{ .Mov = .{ .dst = .{ .Phys = .rcx }, .src = .{ .VReg = elem_vreg } } });
                        }
                    }
                } else {
                    try insts.append(ctx.allocator, .{ .Mov = .{ .dst = .{ .VReg = dst }, .src = .{ .Imm = 0 } } });
                }
            }
        },
        .StructInit => |payload| {
            if (dest_vreg) |dst| {
                // For struct initialization, store first field to dest vreg
                // and second field (if any) to rdx for struct returns
                if (payload.fields.len > 0) {
                    const first = try lowerOperand(ctx, payload.fields[0].value, vreg_count);
                    try insts.append(ctx.allocator, .{ .Mov = .{ .dst = .{ .VReg = dst }, .src = first } });
                    
                    // For 2-field structs, also store second field to rdx for return
                    if (payload.fields.len > 1) {
                        const second = try lowerOperand(ctx, payload.fields[1].value, vreg_count);
                        try insts.append(ctx.allocator, .{ .Mov = .{ .dst = .{ .Phys = .rdx }, .src = second } });
                    }
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
        .RetSecond => .{ .Phys = .rdx }, // Second return value is in rdx
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
    const offset: i32 = -@as(i32, @intCast((local + 1) * @sizeOf(i64) * LOCAL_STACK_MULTIPLIER));
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
