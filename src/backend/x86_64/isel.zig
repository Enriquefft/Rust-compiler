const std = @import("std");
const diag = @import("../../diag/diagnostics.zig");
const source_map = @import("../../diag/source_map.zig");
const mir = @import("../../mir/mir.zig");
const machine = @import("machine.zig");
const shared = @import("../../shared_consts.zig");

const zero_span = source_map.Span{ .file_id = 0, .start = 0, .end = 0 };

// Import shared constants to ensure consistency with MIR lowering
const MAX_STRUCT_FIELDS = shared.MAX_STRUCT_FIELDS;
const ASSUMED_STRUCT_FIELDS = shared.ASSUMED_GENERIC_STRUCT_FIELDS;
const LOCAL_STACK_MULTIPLIER = shared.LOCAL_STACK_MULTIPLIER;
const MAX_EXTRA_ARRAY_ELEMENTS = shared.MAX_EXTRA_ARRAY_ELEMENTS;
const STRUCT_SECOND_FIELD_OFFSET = shared.STRUCT_SECOND_FIELD_OFFSET;

/// Get the field offset from stored struct layouts using declaration order.
/// Falls back to sequential index for known field patterns (x=0, y=1) if struct not found.
/// Uses hash-based index only as a last resort for unknown field names.
fn getFieldOffsetFromLayout(mir_crate: *const mir.MirCrate, struct_name: ?[]const u8, field_name: []const u8) i32 {
    // Try to look up from stored struct layouts first
    if (struct_name) |name| {
        if (mir_crate.getFieldOffset(name, field_name)) |offset| {
            return offset;
        }
    }
    // Fall back to sequential index for common field names
    return getSequentialFieldIndex(field_name);
}

/// Get the sequential field index for a field name.
/// For arrays of structs with known field patterns (x=0, y=1), returns sequential index.
/// Falls back to hash-based index for unknown field names.
fn getSequentialFieldIndex(name: []const u8) i32 {
    if (std.mem.eql(u8, name, "x")) return 0;
    if (std.mem.eql(u8, name, "y")) return 1;
    // Fallback to hash-based index
    var hash: u32 = 0;
    for (name) |ch| hash = hash *% 31 +% ch;
    return @intCast(hash % MAX_STRUCT_FIELDS);
}

/// Load the second field of a struct from memory into rdx.
/// This is used when loading a struct from an array so that both fields
/// are available for subsequent StoreLocal operations.
///
/// Parameters:
/// - insts: The instruction list to append to
/// - allocator: Allocator for instruction list operations
/// - base_addr_vreg: VReg containing the base address of the struct
/// - vreg_count: Pointer to the next VReg ID (will be incremented)
fn loadSecondStructFieldToRdx(
    insts: *std.ArrayListUnmanaged(machine.InstKind),
    allocator: std.mem.Allocator,
    base_addr_vreg: machine.VReg,
    vreg_count: *machine.VReg,
) !void {
    // Compute address of second field
    vreg_count.* += 1;
    const second_addr_vreg = vreg_count.* - 1;
    try insts.append(allocator, .{ .Mov = .{ .dst = .{ .VReg = second_addr_vreg }, .src = .{ .VReg = base_addr_vreg } } });
    try insts.append(allocator, .{ .Bin = .{ .op = .add, .dst = .{ .VReg = second_addr_vreg }, .lhs = .{ .VReg = second_addr_vreg }, .rhs = .{ .Imm = STRUCT_SECOND_FIELD_OFFSET } } });
    // Load second field into rdx
    try insts.append(allocator, .{ .Deref = .{ .dst = .{ .Phys = .rdx }, .addr = .{ .VReg = second_addr_vreg } } });
}

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
    // Current function's local variable types (for type-aware lowering)
    current_fn_locals: []const mir.MirType = &[_]mir.MirType{},
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

    // Set current function's local types for type-aware lowering
    ctx.current_fn_locals = func.locals;

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
        .callee_saved_gprs = std.array_list.Managed(machine.CalleeSavedGpr).init(ctx.allocator),
        .callee_saved_xmms = std.array_list.Managed(machine.CalleeSavedXmm).init(ctx.allocator),
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

            // For array types, also store additional elements from physical registers
            // For struct types from function returns, store the second field from rdx.
            // Skip this for Param sources since parameters are stored to separate locals.
            const is_param_source = payload.src == .Param;
            if (inst.ty) |ty| {
                if (ty == .Array) {
                    // Store elements 2-8 from physical registers at consecutive offsets
                    const extra_regs = [_]machine.PhysReg{ .rdx, .rcx, .r8, .r9, .r10, .r12, .r13 };
                    for (extra_regs, 0..) |reg, i| {
                        const offset = mem.Mem.offset - @as(i32, @intCast((i + 1) * 8));
                        const elem_mem = machine.MOperand{ .Mem = .{ .base = mem.Mem.base, .offset = offset } };
                        try insts.append(ctx.allocator, .{ .Mov = .{ .dst = elem_mem, .src = .{ .Phys = reg } } });
                    }
                } else if (ty == .Struct and !is_param_source) {
                    // For structs stored to locals, store the second field from rdx
                    // Use the same layout as Field access: hash-based field indices with LOCAL_STACK_MULTIPLIER spacing
                    const second_field_mem = machine.MOperand{
                        .Mem = .{
                            .base = mem.Mem.base,
                            .offset = mem.Mem.offset + @as(i32, @truncate(STRUCT_SECOND_FIELD_OFFSET)),
                        },
                    };
                    try insts.append(ctx.allocator, .{ .Mov = .{ .dst = second_field_mem, .src = .{ .Phys = .rdx } } });
                }
            }
        },
        .StorePtr => |payload| {
            // Store to a pointer: *ptr = value
            const ptr = try lowerOperand(ctx, payload.ptr, vreg_count);
            const src = try lowerOperand(ctx, payload.src, vreg_count);
            try insts.append(ctx.allocator, .{ .StoreDeref = .{ .addr = ptr, .src = src } });
        },

        .StoreIndex => |payload| {
            // Store to an indexed location: target[index] = value
            const target = try lowerOperand(ctx, payload.target, vreg_count);
            const index = try lowerOperand(ctx, payload.index, vreg_count);
            const src = try lowerOperand(ctx, payload.src, vreg_count);
            switch (target) {
                .Mem => |base_mem| {
                    switch (index) {
                        .Imm => |imm| {
                            const scaled: i32 = @intCast(imm * @as(i64, @intCast(@sizeOf(i64))));
                            const offset = base_mem.offset - scaled;
                            const mem = machine.MOperand{ .Mem = .{ .base = base_mem.base, .offset = offset } };
                            try insts.append(ctx.allocator, .{ .Mov = .{ .dst = mem, .src = src } });
                        },
                        .VReg, .Phys, .Mem => {
                            // For variable indices, we need to compute the address dynamically
                            // Load the index into a register, multiply by element size, subtract from base
                            vreg_count.* += 1;
                            const idx_vreg = vreg_count.* - 1;

                            // Move index to vreg if needed
                            switch (index) {
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
                            const idx_as_vreg: machine.MOperand = switch (index) {
                                .VReg => index,
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

                            // Store value to computed address
                            try insts.append(ctx.allocator, .{ .StoreDeref = .{ .addr = .{ .VReg = base_vreg }, .src = src } });
                        },
                        else => {
                            ctx.diagnostics.reportError(zero_span, "unsupported index operand for store index");
                            return error.Unsupported;
                        },
                    }
                },
                else => {
                    ctx.diagnostics.reportError(zero_span, "unsupported target for store index");
                    return error.Unsupported;
                },
            }
        },

        .StoreField => |payload| {
            // Store to a struct field: struct.field = value
            const src = try lowerOperand(ctx, payload.src, vreg_count);

            // Check if the target is a Local with Pointer type (i.e., a reference like &mut self)
            const is_pointer_local = switch (payload.target) {
                .Local => |local_id| blk: {
                    if (local_id < ctx.current_fn_locals.len) {
                        const ty = ctx.current_fn_locals[local_id];
                        break :blk (ty == .Pointer);
                    }
                    break :blk false;
                },
                else => false,
            };

            if (is_pointer_local) {
                // Target is a reference/pointer - load the pointer value, then store field through it
                const ptr_mem = try lowerOperand(ctx, payload.target, vreg_count);
                // Load the pointer value into a VReg
                vreg_count.* += 1;
                const ptr_vreg = vreg_count.* - 1;
                try insts.append(ctx.allocator, .{ .Mov = .{ .dst = .{ .VReg = ptr_vreg }, .src = ptr_mem } });

                // Use stored struct layout for field offset, falling back to sequential/hash
                const field_offset: i64 = getFieldOffsetFromLayout(ctx.mir_crate, null, payload.name);

                // Store field through the pointer
                if (field_offset != 0) {
                    vreg_count.* += 1;
                    const field_addr_vreg = vreg_count.* - 1;
                    try insts.append(ctx.allocator, .{ .Mov = .{ .dst = .{ .VReg = field_addr_vreg }, .src = .{ .VReg = ptr_vreg } } });
                    try insts.append(ctx.allocator, .{ .Bin = .{ .op = .add, .dst = .{ .VReg = field_addr_vreg }, .lhs = .{ .VReg = field_addr_vreg }, .rhs = .{ .Imm = field_offset } } });
                    try insts.append(ctx.allocator, .{ .StoreDeref = .{ .addr = .{ .VReg = field_addr_vreg }, .src = src } });
                } else {
                    try insts.append(ctx.allocator, .{ .StoreDeref = .{ .addr = .{ .VReg = ptr_vreg }, .src = src } });
                }
            } else {
                const target = try lowerOperand(ctx, payload.target, vreg_count);
                switch (target) {
                    .Mem => |base_mem| {
                        // Use stored struct layout for field offset based on declaration order
                        // Falls back to sequential/hash if struct layout not found
                        const field_offset = getFieldOffsetFromLayout(ctx.mir_crate, null, payload.name);
                        const offset = base_mem.offset + field_offset;
                        const mem = machine.MOperand{ .Mem = .{ .base = base_mem.base, .offset = offset } };
                        try insts.append(ctx.allocator, .{ .Mov = .{ .dst = mem, .src = src } });
                    },
                    .VReg => |vreg| {
                        // Target is a VReg, meaning it could be:
                        // 1. A dynamically indexed array element (address in dynamic_index_addr_map)
                        // 2. An immediate-indexed array element (memory location in index_mem_map)
                        if (ctx.dynamic_index_addr_map.get(vreg)) |addr_vreg| {
                            // We have the address of the array element, compute field offset from it
                            // Use stored struct layout for field offset
                            const field_offset: i64 = getFieldOffsetFromLayout(ctx.mir_crate, null, payload.name);

                            // Add field offset to the base address
                            vreg_count.* += 1;
                            const field_addr_vreg = vreg_count.* - 1;
                            if (field_offset != 0) {
                                try insts.append(ctx.allocator, .{ .Mov = .{ .dst = .{ .VReg = field_addr_vreg }, .src = .{ .VReg = addr_vreg } } });
                                try insts.append(ctx.allocator, .{ .Bin = .{ .op = .add, .dst = .{ .VReg = field_addr_vreg }, .lhs = .{ .VReg = field_addr_vreg }, .rhs = .{ .Imm = field_offset } } });
                                try insts.append(ctx.allocator, .{ .StoreDeref = .{ .addr = .{ .VReg = field_addr_vreg }, .src = src } });
                            } else {
                                try insts.append(ctx.allocator, .{ .StoreDeref = .{ .addr = .{ .VReg = addr_vreg }, .src = src } });
                            }
                        } else if (ctx.index_mem_map.get(vreg)) |mem_loc| {
                            // VReg came from immediate-indexed array access, we have its memory location
                            // Use stored struct layout for field offset
                            const field_offset: i32 = getFieldOffsetFromLayout(ctx.mir_crate, null, payload.name);
                            const offset = mem_loc.offset + field_offset;
                            const mem = machine.MOperand{ .Mem = .{ .base = mem_loc.base, .offset = offset } };
                            try insts.append(ctx.allocator, .{ .Mov = .{ .dst = mem, .src = src } });
                        } else {
                            ctx.diagnostics.reportError(zero_span, "unsupported VReg target for store field (no address recorded)");
                            return error.Unsupported;
                        }
                    },
                    else => {
                        ctx.diagnostics.reportError(zero_span, "unsupported target for store field");
                        return error.Unsupported;
                    },
                }
            }
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
            const xmm_registers = [_]machine.XmmReg{ .xmm0, .xmm1, .xmm2, .xmm3, .xmm4, .xmm5, .xmm6, .xmm7 };

            // First pass: count float vs non-float args to determine what goes where
            var float_count: usize = 0;
            var gpr_count: usize = 0;
            for (payload.args) |arg| {
                const is_float = switch (arg) {
                    .ImmFloat => true,
                    .Local => |local_id| blk: {
                        if (local_id < ctx.current_fn_locals.len) {
                            const ty = ctx.current_fn_locals[local_id];
                            break :blk (ty == .F32 or ty == .F64);
                        }
                        break :blk false;
                    },
                    else => false,
                };
                if (is_float and is_varargs) {
                    float_count += 1;
                } else {
                    gpr_count += 1;
                }
            }

            // Push extra GPR arguments to stack (in reverse order per ABI)
            var stack_args = std.ArrayListUnmanaged(machine.MOperand){};
            defer stack_args.deinit(ctx.allocator);

            var temp_gpr_count: usize = 0;
            for (payload.args) |arg| {
                const is_float = switch (arg) {
                    .ImmFloat => true,
                    .Local => |local_id| blk: {
                        if (local_id < ctx.current_fn_locals.len) {
                            const ty = ctx.current_fn_locals[local_id];
                            break :blk (ty == .F32 or ty == .F64);
                        }
                        break :blk false;
                    },
                    else => false,
                };
                if (!is_float or !is_varargs) {
                    if (temp_gpr_count >= arg_registers.len) {
                        // This arg needs to go on stack
                        const lowered = try lowerOperand(ctx, arg, vreg_count);
                        try stack_args.append(ctx.allocator, lowered);
                    }
                    temp_gpr_count += 1;
                }
            }

            // Ensure 16-byte stack alignment before call
            // If odd number of stack args, add padding
            const need_padding = (stack_args.items.len % 2) == 1;
            if (need_padding) {
                try insts.append(ctx.allocator, .{ .Bin = .{ .op = .sub, .dst = .{ .Phys = .rsp }, .lhs = .{ .Phys = .rsp }, .rhs = .{ .Imm = 8 } } });
            }

            // Push stack args in reverse order
            var i = stack_args.items.len;
            while (i > 0) {
                i -= 1;
                try insts.append(ctx.allocator, .{ .Push = stack_args.items[i] });
            }

            var xmm_count: usize = 0;
            var gpr_idx: usize = 0;
            for (payload.args) |arg| {
                // Check if this argument is a float type
                const is_float = switch (arg) {
                    .ImmFloat => true,
                    .Local => |local_id| blk: {
                        if (local_id < ctx.current_fn_locals.len) {
                            const ty = ctx.current_fn_locals[local_id];
                            break :blk (ty == .F32 or ty == .F64);
                        }
                        break :blk false;
                    },
                    else => false,
                };

                if (is_float and is_varargs) {
                    // Float args go in XMM registers for varargs
                    if (xmm_count < xmm_registers.len) {
                        // Load from local to XMM
                        switch (arg) {
                            .ImmFloat => |float_val| {
                                const float_bits: i64 = @bitCast(float_val);
                                try insts.append(ctx.allocator, .{ .Mov = .{ .dst = .{ .Phys = .r11 }, .src = .{ .Imm = float_bits } } });
                                try insts.append(ctx.allocator, .{ .Mov = .{ .dst = .{ .Mem = .{ .base = .rsp, .offset = -8 } }, .src = .{ .Phys = .r11 } } });
                                try insts.append(ctx.allocator, .{ .Movsd = .{ .dst = .{ .Xmm = xmm_registers[xmm_count] }, .src = .{ .Mem = .{ .base = .rsp, .offset = -8 } } } });
                            },
                            .Local => |local_id| {
                                const offset: i32 = -@as(i32, @intCast(local_id + 1)) * 32;
                                try insts.append(ctx.allocator, .{ .Movsd = .{ .dst = .{ .Xmm = xmm_registers[xmm_count] }, .src = .{ .Mem = .{ .base = .rbp, .offset = offset } } } });
                            },
                            else => {},
                        }
                        xmm_count += 1;
                    }
                } else {
                    if (gpr_idx < arg_registers.len) {
                        const lowered = try lowerOperand(ctx, arg, vreg_count);
                        try insts.append(ctx.allocator, .{ .Mov = .{ .dst = .{ .Phys = arg_registers[gpr_idx] }, .src = lowered } });
                        gpr_idx += 1;
                    }
                }
            }

            if (is_varargs) {
                // AL must contain the number of XMM registers used
                try insts.append(ctx.allocator, .{ .Mov = .{ .dst = .{ .Phys = .rax }, .src = .{ .Imm = @intCast(xmm_count) } } });
            }

            try insts.append(ctx.allocator, .{ .Call = switch (target) {
                .Direct => |name| .{ .Direct = name },
                .Indirect => |op| .{ .Indirect = op },
            } });

            // Clean up stack after call if we pushed extra args (including alignment padding)
            if (stack_args.items.len > 0 or need_padding) {
                var cleanup_size = stack_args.items.len * 8;
                if (need_padding) cleanup_size += 8;
                try insts.append(ctx.allocator, .{ .Add = .{ .dst = .{ .Phys = .rsp }, .src = .{ .Imm = @intCast(cleanup_size) } } });
            }

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

                // Determine element size based on result type
                // For struct-typed elements, use ASSUMED_STRUCT_FIELDS * 8 bytes
                // For other types, use 8 bytes
                const elem_size: i64 = if (inst.ty) |ty| switch (ty) {
                    .Struct => ASSUMED_STRUCT_FIELDS * @sizeOf(i64) * LOCAL_STACK_MULTIPLIER,
                    else => @sizeOf(i64),
                } else @sizeOf(i64);

                switch (target) {
                    .Mem => |base_mem| {
                        switch (idx) {
                            .Imm => |imm| {
                                // Stack grows downward, so higher indices are at more negative offsets
                                const scaled: i32 = @intCast(imm * elem_size);
                                const offset = base_mem.offset - scaled;
                                const mem = machine.MOperand{ .Mem = .{ .base = base_mem.base, .offset = offset } };
                                try insts.append(ctx.allocator, .{ .Mov = .{ .dst = .{ .VReg = dst }, .src = mem } });

                                // For struct types, also load the second field into rdx
                                // This is needed for StoreLocal to correctly store both fields
                                if (inst.ty) |ty| {
                                    if (ty == .Struct) {
                                        const second_mem = machine.MOperand{ .Mem = .{ .base = base_mem.base, .offset = offset + @as(i32, @truncate(STRUCT_SECOND_FIELD_OFFSET)) } };
                                        try insts.append(ctx.allocator, .{ .Mov = .{ .dst = .{ .Phys = .rdx }, .src = second_mem } });
                                    }
                                }

                                // Record the memory location for this VReg (for potential Ref later)
                                try ctx.index_mem_map.put(dst, mem.Mem);
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

                                // Multiply index by element size
                                vreg_count.* += 1;
                                const scaled_vreg = vreg_count.* - 1;
                                try insts.append(ctx.allocator, .{ .Mov = .{ .dst = .{ .VReg = scaled_vreg }, .src = idx_as_vreg } });
                                try insts.append(ctx.allocator, .{ .Bin = .{ .op = .imul, .dst = .{ .VReg = scaled_vreg }, .lhs = .{ .VReg = scaled_vreg }, .rhs = .{ .Imm = elem_size } } });

                                // Load base address into a register
                                vreg_count.* += 1;
                                const base_vreg = vreg_count.* - 1;
                                try insts.append(ctx.allocator, .{ .Lea = .{ .dst = .{ .VReg = base_vreg }, .mem = base_mem } });

                                // Subtract scaled index from base (stack grows downward)
                                try insts.append(ctx.allocator, .{ .Bin = .{ .op = .sub, .dst = .{ .VReg = base_vreg }, .lhs = .{ .VReg = base_vreg }, .rhs = .{ .VReg = scaled_vreg } } });

                                // Load value from computed address into dst
                                try insts.append(ctx.allocator, .{ .Deref = .{ .dst = .{ .VReg = dst }, .addr = .{ .VReg = base_vreg } } });

                                // For struct types, also load the second field into rdx
                                if (inst.ty) |ty| {
                                    if (ty == .Struct) {
                                        try loadSecondStructFieldToRdx(insts, ctx.allocator, base_vreg, vreg_count);
                                    }
                                }

                                // Track that this VReg holds a value from dynamic indexing
                                // When Ref is applied to this VReg, we use base_vreg (the address) instead
                                try ctx.dynamic_index_addr_map.put(dst, base_vreg);
                            },
                            else => {
                                ctx.diagnostics.reportError(zero_span, "indexing currently supports only immediate indices or variables");
                                return error.Unsupported;
                            },
                        }
                    },
                    .VReg => |ptr_vreg| {
                        // Target is a VReg containing a pointer to the array
                        // This happens when indexing through a reference (e.g., for p in &array)
                        switch (idx) {
                            .Imm => |imm| {
                                // Immediate index: compute offset and load
                                // For pointer-based arrays, we subtract because stack grows downward
                                const scaled: i64 = imm * elem_size;

                                vreg_count.* += 1;
                                const addr_vreg = vreg_count.* - 1;
                                try insts.append(ctx.allocator, .{ .Mov = .{ .dst = .{ .VReg = addr_vreg }, .src = .{ .VReg = ptr_vreg } } });
                                if (scaled != 0) {
                                    try insts.append(ctx.allocator, .{ .Bin = .{ .op = .sub, .dst = .{ .VReg = addr_vreg }, .lhs = .{ .VReg = addr_vreg }, .rhs = .{ .Imm = scaled } } });
                                }

                                // Load value from computed address into dst
                                try insts.append(ctx.allocator, .{ .Deref = .{ .dst = .{ .VReg = dst }, .addr = .{ .VReg = addr_vreg } } });

                                // For struct types, also load the second field into rdx
                                if (inst.ty) |ty| {
                                    if (ty == .Struct) {
                                        try loadSecondStructFieldToRdx(insts, ctx.allocator, addr_vreg, vreg_count);
                                    }
                                }

                                // Track the computed address for subsequent field access or Ref
                                try ctx.dynamic_index_addr_map.put(dst, addr_vreg);
                            },
                            .VReg, .Phys, .Mem => {
                                // Variable index: compute dynamically
                                vreg_count.* += 1;
                                const idx_vreg = vreg_count.* - 1;

                                // Move index to vreg if needed
                                switch (idx) {
                                    .VReg => {}, // Already in VReg form
                                    .Phys => |reg| {
                                        try insts.append(ctx.allocator, .{ .Mov = .{ .dst = .{ .VReg = idx_vreg }, .src = .{ .Phys = reg } } });
                                    },
                                    .Mem => |mem| {
                                        try insts.append(ctx.allocator, .{ .Mov = .{ .dst = .{ .VReg = idx_vreg }, .src = .{ .Mem = mem } } });
                                    },
                                    else => unreachable,
                                }

                                const idx_as_vreg: machine.MOperand = switch (idx) {
                                    .VReg => idx,
                                    .Phys, .Mem => .{ .VReg = idx_vreg },
                                    else => unreachable,
                                };

                                // Multiply index by element size
                                vreg_count.* += 1;
                                const scaled_vreg = vreg_count.* - 1;
                                try insts.append(ctx.allocator, .{ .Mov = .{ .dst = .{ .VReg = scaled_vreg }, .src = idx_as_vreg } });
                                try insts.append(ctx.allocator, .{ .Bin = .{ .op = .imul, .dst = .{ .VReg = scaled_vreg }, .lhs = .{ .VReg = scaled_vreg }, .rhs = .{ .Imm = elem_size } } });

                                // Copy base pointer and subtract scaled index
                                vreg_count.* += 1;
                                const addr_vreg = vreg_count.* - 1;
                                try insts.append(ctx.allocator, .{ .Mov = .{ .dst = .{ .VReg = addr_vreg }, .src = .{ .VReg = ptr_vreg } } });
                                try insts.append(ctx.allocator, .{ .Bin = .{ .op = .sub, .dst = .{ .VReg = addr_vreg }, .lhs = .{ .VReg = addr_vreg }, .rhs = .{ .VReg = scaled_vreg } } });

                                // Load value from computed address into dst
                                try insts.append(ctx.allocator, .{ .Deref = .{ .dst = .{ .VReg = dst }, .addr = .{ .VReg = addr_vreg } } });

                                // For struct types, also load the second field into rdx
                                if (inst.ty) |ty| {
                                    if (ty == .Struct) {
                                        try loadSecondStructFieldToRdx(insts, ctx.allocator, addr_vreg, vreg_count);
                                    }
                                }

                                // Track the computed address for subsequent field access or Ref
                                try ctx.dynamic_index_addr_map.put(dst, addr_vreg);
                            },
                            else => {
                                ctx.diagnostics.reportError(zero_span, "indexing currently supports only immediate indices or variables");
                                return error.Unsupported;
                            },
                        }
                    },
                    else => {
                        ctx.diagnostics.reportError(zero_span, "unsupported index base for x86_64 lowering");
                        return error.Unsupported;
                    },
                }
            }
        },
        .Field => |payload| {
            if (dest_vreg) |dst| {
                // Check if the target is a Local with Pointer type (i.e., a reference like &self)
                // In that case, we need to dereference the pointer first to get the struct base
                const is_pointer_local = switch (payload.target) {
                    .Local => |local_id| blk: {
                        if (local_id < ctx.current_fn_locals.len) {
                            const ty = ctx.current_fn_locals[local_id];
                            break :blk (ty == .Pointer);
                        }
                        break :blk false;
                    },
                    else => false,
                };

                if (is_pointer_local) {
                    // Target is a reference/pointer - load the pointer value, then access field through it
                    const ptr_mem = try lowerOperand(ctx, payload.target, vreg_count);
                    // Load the pointer value into a VReg
                    vreg_count.* += 1;
                    const ptr_vreg = vreg_count.* - 1;
                    try insts.append(ctx.allocator, .{ .Mov = .{ .dst = .{ .VReg = ptr_vreg }, .src = ptr_mem } });

                    // Use stored struct layout for field offset, falling back to sequential/hash
                    const field_offset: i64 = getFieldOffsetFromLayout(ctx.mir_crate, null, payload.name);

                    // Access field through the pointer
                    if (field_offset != 0) {
                        vreg_count.* += 1;
                        const field_addr_vreg = vreg_count.* - 1;
                        try insts.append(ctx.allocator, .{ .Mov = .{ .dst = .{ .VReg = field_addr_vreg }, .src = .{ .VReg = ptr_vreg } } });
                        try insts.append(ctx.allocator, .{ .Bin = .{ .op = .add, .dst = .{ .VReg = field_addr_vreg }, .lhs = .{ .VReg = field_addr_vreg }, .rhs = .{ .Imm = field_offset } } });
                        try insts.append(ctx.allocator, .{ .Deref = .{ .dst = .{ .VReg = dst }, .addr = .{ .VReg = field_addr_vreg } } });
                        try ctx.dynamic_index_addr_map.put(dst, field_addr_vreg);
                    } else {
                        try insts.append(ctx.allocator, .{ .Deref = .{ .dst = .{ .VReg = dst }, .addr = .{ .VReg = ptr_vreg } } });
                        try ctx.dynamic_index_addr_map.put(dst, ptr_vreg);
                    }
                } else {
                    const target = try lowerOperand(ctx, payload.target, vreg_count);
                    switch (target) {
                        .Mem => |base_mem| {
                            // Use stored struct layout for field offset based on declaration order
                            // Falls back to sequential/hash if struct layout not found
                            const field_offset = getFieldOffsetFromLayout(ctx.mir_crate, null, payload.name);
                            const offset = base_mem.offset + field_offset;
                            const mem = machine.MOperand{ .Mem = .{ .base = base_mem.base, .offset = offset } };
                            try insts.append(ctx.allocator, .{ .Mov = .{ .dst = .{ .VReg = dst }, .src = mem } });
                        },
                        .VReg => |vreg| {
                            // Target is a VReg, meaning it could be:
                            // 1. A dynamically indexed array element (address in dynamic_index_addr_map)
                            // 2. An immediate-indexed array element (memory location in index_mem_map)
                            if (ctx.dynamic_index_addr_map.get(vreg)) |addr_vreg| {
                                // We have the address of the array element, compute field offset from it
                                // Use stored struct layout for field offset
                                const field_offset: i64 = getFieldOffsetFromLayout(ctx.mir_crate, null, payload.name);

                                // Add field offset to the base address and load the value
                                vreg_count.* += 1;
                                const field_addr_vreg = vreg_count.* - 1;
                                if (field_offset != 0) {
                                    try insts.append(ctx.allocator, .{ .Mov = .{ .dst = .{ .VReg = field_addr_vreg }, .src = .{ .VReg = addr_vreg } } });
                                    try insts.append(ctx.allocator, .{ .Bin = .{ .op = .add, .dst = .{ .VReg = field_addr_vreg }, .lhs = .{ .VReg = field_addr_vreg }, .rhs = .{ .Imm = field_offset } } });
                                    try insts.append(ctx.allocator, .{ .Deref = .{ .dst = .{ .VReg = dst }, .addr = .{ .VReg = field_addr_vreg } } });
                                } else {
                                    try insts.append(ctx.allocator, .{ .Deref = .{ .dst = .{ .VReg = dst }, .addr = .{ .VReg = addr_vreg } } });
                                }
                                // Record the computed address so StoreField can use it
                                try ctx.dynamic_index_addr_map.put(dst, if (field_offset != 0) field_addr_vreg else addr_vreg);
                            } else if (ctx.index_mem_map.get(vreg)) |mem_loc| {
                                // VReg came from immediate-indexed array access, we have its memory location
                                // Use stored struct layout for field offset
                                const field_offset: i32 = getFieldOffsetFromLayout(ctx.mir_crate, null, payload.name);
                                const offset = mem_loc.offset + field_offset;
                                const mem = machine.MOperand{ .Mem = .{ .base = mem_loc.base, .offset = offset } };
                                try insts.append(ctx.allocator, .{ .Mov = .{ .dst = .{ .VReg = dst }, .src = mem } });
                                // Record for potential StoreField
                                try ctx.index_mem_map.put(dst, mem.Mem);
                            } else {
                                ctx.diagnostics.reportError(zero_span, "unsupported VReg field base (no address recorded)");
                                return error.Unsupported;
                            }
                        },
                        else => {
                            ctx.diagnostics.reportError(zero_span, "unsupported field base for x86_64 lowering");
                            return error.Unsupported;
                        },
                    }
                }
            }
        },
        .Array => |payload| {
            if (dest_vreg) |dst| {
                if (payload.elems.len > 0) {
                    // For arrays, we need to know the local ID to compute proper offsets
                    // The first element goes to dst, subsequent elements go to adjacent memory
                    const first = try lowerOperand(ctx, payload.elems[0], vreg_count);
                    try insts.append(ctx.allocator, .{ .Mov = .{ .dst = .{ .VReg = dst }, .src = first } });

                    // For additional elements, first compute all values into VRegs,
                    // then move to designated physical registers that StoreLocal will pick up
                    // This avoids clobbering issues from interleaved VReg/PhysReg operations
                    var elem_vregs: [MAX_EXTRA_ARRAY_ELEMENTS]machine.VReg = undefined;
                    const num_extra = @min(payload.elems.len - 1, MAX_EXTRA_ARRAY_ELEMENTS);

                    // Phase 1: Compute all elements into VRegs
                    for (payload.elems[1..][0..num_extra], 0..) |elem, i| {
                        const elem_op = try lowerOperand(ctx, elem, vreg_count);
                        vreg_count.* += 1;
                        const elem_vreg = vreg_count.* - 1;
                        try insts.append(ctx.allocator, .{ .Mov = .{ .dst = .{ .VReg = elem_vreg }, .src = elem_op } });
                        elem_vregs[i] = elem_vreg;
                    }

                    // Phase 2: Move all values to physical registers
                    // rdx for 2nd, rcx for 3rd, r8 for 4th, r9 for 5th, r10 for 6th, r12 for 7th, r13 for 8th
                    const extra_regs = [_]machine.PhysReg{ .rdx, .rcx, .r8, .r9, .r10, .r12, .r13 };
                    for (elem_vregs[0..num_extra], 0..) |vreg, i| {
                        if (i < extra_regs.len) {
                            try insts.append(ctx.allocator, .{ .Mov = .{ .dst = .{ .Phys = extra_regs[i] }, .src = .{ .VReg = vreg } } });
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
            // If condition is an immediate, we need to load it into a register first
            // because test imm, imm is not valid x86
            switch (cond_op) {
                .Imm => |imm| {
                    // For immediate conditions (like const bool), check if it's always true/false
                    if (imm != 0) {
                        // Condition is always true, just jump to then block
                        try insts.append(ctx.allocator, .{ .Jmp = payload.then_block });
                    } else {
                        // Condition is always false, just jump to else block
                        try insts.append(ctx.allocator, .{ .Jmp = payload.else_block });
                    }
                },
                else => {
                    try insts.append(ctx.allocator, .{ .Test = .{ .operand = cond_op } });
                    try insts.append(ctx.allocator, .{ .Jcc = .{ .cond = .ne, .target = payload.then_block } });
                    try insts.append(ctx.allocator, .{ .Jmp = payload.else_block });
                },
            }
        },
        .Ret => |maybe_op| {
            if (maybe_op) |op| {
                const lowered = lowerSimpleOperand(ctx, op, vreg_count) catch |err| return err;
                try insts.append(ctx.allocator, .{ .Mov = .{ .dst = .{ .Phys = .rax }, .src = lowered } });

                // For local operands, also return the adjacent local in rdx
                // This handles generic functions returning struct values stored in consecutive locals
                switch (op) {
                    .Local => |local_id| {
                        const next_local_id = local_id + 1;
                        if (next_local_id < ctx.current_fn_locals.len) {
                            const next_local_mem = localMem(next_local_id);
                            try insts.append(ctx.allocator, .{ .Mov = .{ .dst = .{ .Phys = .rdx }, .src = next_local_mem } });
                        }
                    },
                    else => {},
                }
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
        .ImmChar => |c| .{ .Imm = @intCast(c) }, // Unicode codepoint as immediate
        .ImmString => |s| .{ .Label = try ctx.data.internString(s) },
        .Global => |id| blk: {
            if (id >= ctx.mir_crate.fns.items.len) {
                ctx.diagnostics.reportError(zero_span, "operand refers to unknown function id");
                return error.Unsupported;
            }
            break :blk .{ .Label = ctx.mir_crate.fns.items[id].name };
        },
        .Symbol => |name| .{ .Label = name },
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
