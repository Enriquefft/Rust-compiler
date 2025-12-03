//! HIR to MIR lowering pass.
//!
//! This module transforms the High-Level IR (HIR) into Mid-Level IR (MIR).
//! The lowering process converts high-level constructs like control flow statements,
//! expressions, and function definitions into a simpler three-address form with
//! explicit basic blocks and temporaries.
//!
//! Key responsibilities:
//! - Convert HIR functions to MIR functions with explicit basic blocks
//! - Lower control flow (if/else, while, for) to conditional branches and gotos
//! - Transform expressions into sequences of MIR instructions
//! - Handle special constructs like `println!` macro expansion
//! - Map HIR types to MIR types

const std = @import("std");
const diag = @import("../diag/diagnostics.zig");
const hir = @import("../hir/hir.zig");
const mir = @import("mir.zig");
const shared = @import("../shared_consts.zig");

/// Error type for lowering operations, currently only memory allocation failures.
const LowerError = error{OutOfMemory};

// Import shared constants to ensure consistency with backend
const MAX_STRUCT_FIELDS = shared.MAX_STRUCT_FIELDS;
const LOCAL_STACK_MULTIPLIER = shared.LOCAL_STACK_MULTIPLIER;

/// Compute field layout for a struct based on declaration order.
/// Each field is placed at sequential offsets using LOCAL_STACK_MULTIPLIER spacing.
/// Returns the StructLayout or null if memory allocation fails.
fn computeStructLayout(alloc: std.mem.Allocator, struct_item: hir.Struct) LowerError!mir.StructLayout {
    var field_layouts = std.ArrayListUnmanaged(mir.FieldLayout){};
    defer field_layouts.deinit(alloc);

    var current_offset: i32 = 0;
    // Field stride: each field occupies LOCAL_STACK_MULTIPLIER * sizeof(i64) bytes
    const field_stride: i32 = @intCast(@sizeOf(i64) * LOCAL_STACK_MULTIPLIER);

    for (struct_item.fields, 0..) |field, idx| {
        const layout = mir.FieldLayout{
            .name = field.name,
            .offset = current_offset,
            .size = @intCast(field_stride),
            .index = @intCast(idx),
        };
        try field_layouts.append(alloc, layout);
        // Move to next field offset (negative direction for stack growth)
        current_offset -= field_stride;
    }

    const total_size: u32 = @as(u32, @intCast(struct_item.fields.len)) * @as(u32, @intCast(field_stride));

    return mir.StructLayout{
        .name = struct_item.name,
        .fields = try field_layouts.toOwnedSlice(alloc),
        .total_size = total_size,
    };
}

/// Get the field index (local offset index) for a field by name from the struct's fields.
/// Uses declaration order: first field = 0, second field = 1, etc.
/// Returns 0 if field not found (fallback).
fn getFieldIndexFromDeclaration(fields: []const hir.Field, field_name: []const u8) hir.LocalId {
    for (fields, 0..) |field, idx| {
        if (std.mem.eql(u8, field.name, field_name)) {
            return @intCast(idx);
        }
    }
    // Fallback to 0 if field not found
    return 0;
}

/// Lower a complete HIR crate to MIR representation.
///
/// Iterates over all items in the HIR crate and lowers functions to MIR.
/// Built-in functions like `println` without bodies are skipped.
/// Also computes and stores struct layouts based on declaration order.
///
/// Returns the lowered MIR crate or an error if allocation fails.
pub fn lowerFromHir(allocator: std.mem.Allocator, hir_crate: *const hir.Crate, diagnostics: *diag.Diagnostics) LowerError!mir.MirCrate {
    var crate = mir.MirCrate.init(allocator);

    // First pass: compute struct layouts from HIR struct definitions
    for (hir_crate.items.items) |item| {
        switch (item.kind) {
            .Struct => |struct_item| {
                const layout = try computeStructLayout(crate.allocator(), struct_item);
                try crate.struct_layouts.put(crate.allocator(), struct_item.name, layout);
            },
            else => {},
        }
    }

    // Second pass: lower functions
    for (hir_crate.items.items) |item| {
        switch (item.kind) {
            .Function => |fn_item| {
                if (std.mem.eql(u8, fn_item.name, "println") and fn_item.body == null) continue;
                const lowered = try lowerFunction(&crate, fn_item, hir_crate, diagnostics);
                try crate.fns.append(crate.allocator(), lowered);
            },
            else => {},
        }
    }

    return crate;
}

/// Retrieve the fields for a struct type, following resolved struct definitions.
/// Returns null when the type does not refer to a struct.
fn getStructFields(hir_crate: *const hir.Crate, ty_id: hir.TypeId) ?[]const hir.Field {
    if (ty_id >= hir_crate.types.items.len) return null;
    const kind = hir_crate.types.items[ty_id].kind;
    switch (kind) {
        .Struct => |info| {
            if (info.def_id >= hir_crate.items.items.len) return null;
            const item = hir_crate.items.items[info.def_id];
            if (item.kind == .Struct) return item.kind.Struct.fields;
        },
        .Path => |path| {
            if (path.segments.len != 1) return null;
            const name = path.segments[0];
            for (hir_crate.items.items) |item| {
                if (item.kind == .Struct and std.mem.eql(u8, item.kind.Struct.name, name)) {
                    return item.kind.Struct.fields;
                }
            }
        },
        else => {},
    }
    return null;
}

/// Lower a single HIR function to MIR representation.
///
/// Creates a FunctionBuilder to manage block construction and instruction emission,
/// then processes the function body, parameters, and return type.
fn lowerFunction(crate: *mir.MirCrate, func: hir.Function, hir_crate: *const hir.Crate, diagnostics: *diag.Diagnostics) LowerError!mir.MirFn {
    var builder = FunctionBuilder{
        .allocator = crate.allocator(),
        .crate = crate,
        .hir_crate = hir_crate,
        .diagnostics = diagnostics,
        .mir_fn = mir.MirFn{
            .name = func.name,
            .params = func.params,
            .locals = &[_]mir.MirType{},
            .ret_ty = mapType(hir_crate, func.return_type, func.span, diagnostics),
            .blocks = &[_]mir.Block{},
        },
        .param_local_map = std.AutoArrayHashMap(hir.LocalId, hir.LocalId).init(crate.allocator()),
    };

    _ = try builder.beginBlock();

    // Track the next available local ID
    var next_local: hir.LocalId = 0;
    var arg_idx: usize = 0;

    for (func.params, 0..) |param, param_idx| {
        const ty = if (param_idx < func.param_types.len) func.param_types[param_idx] else null;

        // Check if this parameter is a struct type (needs field expansion)
        const struct_fields = if (ty) |ty_id| getStructFields(hir_crate, ty_id) else null;

        // Store the base local for this param in the mapping
        try builder.param_local_map.put(param, next_local);

        // For struct parameters, store each field using StoreField to the same local
        // This ensures all fields are in the same memory region for Field access
        if (struct_fields) |fields| {
            // Allocate space for the struct - use the concrete field count
            for (fields, 0..) |field, idx| {
                try builder.ensureLocal(next_local + @as(hir.LocalId, @intCast(idx)), field.ty, func.span);
                _ = try builder.emitInst(.{
                    .ty = null,
                    .dest = null,
                    .kind = .{
                        .StoreField = .{
                            .target = .{ .Local = next_local },
                            .name = field.name,
                            .src = .{ .Param = @intCast(arg_idx) },
                        },
                    },
                });
                arg_idx += 1;
            }
            next_local += @intCast(fields.len);
        } else {
            // Non-struct parameter: store normally
            try builder.ensureLocal(next_local, ty, func.span);
            _ = try builder.emitInst(.{ .ty = mapType(hir_crate, ty, func.span, diagnostics), .dest = null, .kind = .{ .StoreLocal = .{ .local = next_local, .src = .{ .Param = @intCast(arg_idx) } } } });
            arg_idx += 1;
            next_local += 1;
        }
    }

    // Set the next_local in the builder for use in lowering statements
    builder.next_local = next_local;

    if (func.body) |body_id| {
        const result = try builder.lowerExpr(body_id);
        builder.setTerm(.{ .Ret = result });
    } else {
        builder.setTerm(.{ .Ret = null });
    }

    return try builder.finish();
}

/// Builder for constructing MIR functions incrementally.
///
/// Manages the creation of basic blocks, instruction emission, temporary allocation,
/// and local variable tracking during the lowering of a single function.
const FunctionBuilder = struct {
    /// Allocator for temporary allocations during building
    allocator: std.mem.Allocator,
    /// Reference to the parent MIR crate for nested function creation (lambdas)
    crate: *mir.MirCrate,
    /// Reference to the HIR crate for expression/statement lookup
    hir_crate: *const hir.Crate,
    /// Diagnostics collector for error reporting
    diagnostics: *diag.Diagnostics,
    /// The MIR function being built
    mir_fn: mir.MirFn,
    /// Types of local variables allocated so far
    locals: std.ArrayListUnmanaged(mir.MirType) = .{},
    /// Basic blocks being constructed
    blocks: std.ArrayListUnmanaged(BlockState) = .{},
    /// Index of the current block receiving instructions
    current_block: mir.BlockId = 0,
    /// Next temporary ID to allocate
    next_temp: mir.TempId = 0,
    /// Next local ID to allocate (tracks available local slots)
    next_local: hir.LocalId = 0,
    /// Mapping from HIR parameter/pattern IDs to actual local IDs
    /// Used when struct parameters expand to multiple locals
    param_local_map: std.AutoArrayHashMap(hir.LocalId, hir.LocalId) = std.AutoArrayHashMap(hir.LocalId, hir.LocalId).init(undefined),

    /// State for a basic block under construction.
    const BlockState = struct {
        /// Instructions added to this block so far
        insts: std.ArrayListUnmanaged(mir.Inst) = .{},
        /// Terminator for this block (set when block is complete)
        term: ?mir.TermKind = null,
    };

    /// Begin a new block and make it the current block.
    /// Returns the ID of the newly created block.
    fn beginBlock(self: *FunctionBuilder) LowerError!mir.BlockId {
        const id: mir.BlockId = @intCast(self.blocks.items.len);
        try self.blocks.append(self.allocator, .{});
        self.current_block = id;
        return id;
    }

    /// Create a new block without switching to it.
    /// Used for creating branch targets before populating them.
    fn newBlock(self: *FunctionBuilder) LowerError!mir.BlockId {
        const id: mir.BlockId = @intCast(self.blocks.items.len);
        try self.blocks.append(self.allocator, .{});
        return id;
    }

    /// Switch instruction emission to a different block.
    fn switchTo(self: *FunctionBuilder, id: mir.BlockId) void {
        self.current_block = id;
    }

    /// Set the terminator for the current block.
    fn setTerm(self: *FunctionBuilder, term: mir.TermKind) void {
        self.blocks.items[self.current_block].term = term;
    }

    /// Emit an instruction to the current block.
    /// Returns the destination operand if the instruction has one.
    fn emitInst(self: *FunctionBuilder, inst: mir.Inst) LowerError!?mir.Operand {
        const dest = inst.dest;
        try self.blocks.items[self.current_block].insts.append(self.allocator, inst);
        if (dest) |tmp| {
            return .{ .Temp = tmp };
        }
        return null;
    }

    /// Allocate a new temporary ID.
    fn newTemp(self: *FunctionBuilder) mir.TempId {
        const tmp = self.next_temp;
        self.next_temp += 1;
        return tmp;
    }

    /// Ensure a local slot exists with the given type.
    /// Expands the locals array if necessary.
    fn ensureLocal(self: *FunctionBuilder, local: hir.LocalId, ty_id: ?hir.TypeId, span: hir.Span) LowerError!void {
        const mir_ty = mapType(self.hir_crate, ty_id, span, self.diagnostics);
        while (local >= self.locals.items.len) {
            try self.locals.append(self.allocator, .Unknown);
        }
        if (mir_ty) |ty| {
            self.locals.items[local] = ty;
        } else if (local < self.locals.items.len) {
            self.locals.items[local] = .Unknown;
        }
    }

    /// Lower a HIR expression to MIR instructions.
    /// Returns an operand representing the expression's value, or null for void expressions.
    fn lowerExpr(self: *FunctionBuilder, expr_id: hir.ExprId) LowerError!?mir.Operand {
        const expr = self.hir_crate.exprs.items[expr_id];
        switch (expr.kind) {
            .ConstInt => |v| {
                return .{ .ImmInt = v };
            },
            .ConstFloat => |v| {
                return .{ .ImmFloat = v };
            },
            .ConstBool => |v| {
                return .{ .ImmBool = v };
            },
            .ConstChar => |v| {
                return .{ .ImmChar = v };
            },
            .ConstString => |v| {
                return .{ .ImmString = stripQuotes(v) };
            },
            .LocalRef => |local| {
                // Check if this local was remapped due to struct parameter expansion
                const actual_local = self.param_local_map.get(local) orelse local;
                try self.ensureLocal(actual_local, expr.ty, expr.span);
                return .{ .Local = actual_local };
            },
            .GlobalRef => |def_id| {
                if (def_id >= self.hir_crate.items.items.len) {
                    self.diagnostics.reportError(expr.span, "reference to unknown item during MIR lowering");
                    return null;
                }
                // Look up the item and handle const inlining
                const item = self.hir_crate.items.items[def_id];
                switch (item.kind) {
                    .Function => {
                        return .{ .Symbol = item.kind.Function.name };
                    },
                    .Const => |const_item| {
                        // Inline the const value - recursively lower its value expression
                        return try self.lowerExpr(const_item.value);
                    },
                    else => {
                        return .{ .Global = def_id };
                    },
                }
            },
            .Block => |blk| {
                var last: ?mir.Operand = null;
                for (blk.stmts) |stmt_id| {
                    try self.lowerStmt(stmt_id);
                }
                if (blk.tail) |tail| {
                    last = try self.lowerExpr(tail);
                }
                return last;
            },

            .Unsafe => |unsafe_expr| {
                return try self.lowerExpr(unsafe_expr.body);
            },

            .Binary => |bin| {
                const lhs = try self.lowerExpr(bin.lhs);
                const rhs = try self.lowerExpr(bin.rhs);
                if (lhs == null or rhs == null) return null;
                const lhs_op = lhs.?;
                const rhs_op = rhs.?;
                switch (bin.op) {
                    .LogicalAnd, .LogicalOr => return try self.lowerLogical(bin.op, lhs_op, bin.rhs, expr.ty, expr.span),
                    .Eq, .Ne, .Lt, .Le, .Gt, .Ge => {
                        const tmp = self.newTemp();
                        _ = try self.emitInst(.{ .ty = .Bool, .dest = tmp, .kind = .{ .Cmp = .{ .op = mapCmp(bin.op), .lhs = lhs_op, .rhs = rhs_op } } });
                        return .{ .Temp = tmp };
                    },
                    .Add, .Sub, .Mul, .Div, .Mod => {
                        const tmp = self.newTemp();
                        _ = try self.emitInst(.{ .ty = mapType(self.hir_crate, expr.ty, expr.span, self.diagnostics), .dest = tmp, .kind = .{ .Bin = .{ .op = mapBin(bin.op), .lhs = lhs_op, .rhs = rhs_op } } });
                        return .{ .Temp = tmp };
                    },
                }
            },
            .Unary => |un| {
                const operand = try self.lowerExpr(un.expr);
                if (operand) |op| {
                    if (mapUnary(un.op)) |mir_op| {
                        const tmp = self.newTemp();
                        _ = try self.emitInst(.{ .ty = mapType(self.hir_crate, expr.ty, expr.span, self.diagnostics), .dest = tmp, .kind = .{ .Unary = .{ .op = mir_op, .operand = op } } });
                        return .{ .Temp = tmp };
                    }
                    self.diagnostics.reportError(expr.span, "unary operator not supported in MIR lowering");
                }
                return null;
            },
            .Call => |call| {
                if (self.builtinMacroHandler(call.callee)) |handler| {
                    return try self.lowerBuiltinMacro(handler, call.args, expr.span);
                }

                // Check if this is an array method call like data.len()
                if (self.isArrayMethodCall(call.callee)) |array_method| {
                    return try self.lowerArrayMethodCall(array_method.target_id, array_method.method_name, array_method.array_size, call.args, expr);
                }

                // Check if this is a pointer method call like ptr.is_null()
                if (self.isPointerMethodCall(call.callee)) |ptr_method| {
                    return try self.lowerPointerMethodCall(ptr_method.target_id, ptr_method.method_name, call.args, expr);
                }

                // Check if this is a struct method call like point.offset(1, 2)
                if (self.isStructMethodCall(call.callee)) |struct_method| {
                    return try self.lowerStructMethodCall(struct_method.target_id, struct_method.method_name, struct_method.method_def_id, call.args, expr);
                }

                const callee_op = try self.lowerExpr(call.callee) orelse {
                    self.diagnostics.reportError(expr.span, "missing callee while lowering call");
                    return null;
                };
                var args = std.ArrayListUnmanaged(mir.Operand){};
                defer args.deinit(self.allocator);
                for (call.args) |arg_id| {
                    const arg_expr = self.hir_crate.exprs.items[arg_id];

                    // Check if argument is a struct type - if so, pass all fields
                    var is_struct_arg = false;
                    var struct_fields: ?[]const hir.Field = null;
                    if (arg_expr.ty < self.hir_crate.types.items.len) {
                        const arg_type = self.hir_crate.types.items[arg_expr.ty];
                        switch (arg_type.kind) {
                            .Struct => |info| {
                                if (info.def_id < self.hir_crate.items.items.len) {
                                    const struct_item = self.hir_crate.items.items[info.def_id];
                                    if (struct_item.kind == .Struct) {
                                        is_struct_arg = true;
                                        struct_fields = struct_item.kind.Struct.fields;
                                    }
                                }
                            },
                            .Path => |path| {
                                if (path.segments.len == 1) {
                                    for (self.hir_crate.items.items) |item| {
                                        if (item.kind == .Struct and std.mem.eql(u8, item.kind.Struct.name, path.segments[0])) {
                                            is_struct_arg = true;
                                            struct_fields = item.kind.Struct.fields;
                                            break;
                                        }
                                    }
                                }
                            },
                            else => {},
                        }
                    }

                    if (is_struct_arg and struct_fields != null and arg_expr.kind == .LocalRef) {
                        const base_local = self.param_local_map.get(arg_expr.kind.LocalRef) orelse arg_expr.kind.LocalRef;
                        // Pass each field using declaration order (not hash-based)
                        for (struct_fields.?, 0..) |_, field_idx| {
                            const field_index: hir.LocalId = @intCast(field_idx);
                            const field_local = base_local + field_index;
                            try args.append(self.allocator, .{ .Local = field_local });
                        }
                    } else {
                        if (try self.lowerExpr(arg_id)) |arg_op| {
                            try args.append(self.allocator, arg_op);
                        }
                    }
                }
                const tmp = self.newTemp();
                _ = try self.emitInst(.{ .ty = mapType(self.hir_crate, expr.ty, expr.span, self.diagnostics), .dest = tmp, .kind = .{ .Call = .{ .target = callee_op, .args = try args.toOwnedSlice(self.allocator) } } });
                return .{ .Temp = tmp };
            },
            .Assignment => |assign| {
                const value_op = try self.lowerExpr(assign.value);
                const target_expr = self.hir_crate.exprs.items[assign.target];

                if (target_expr.kind == .LocalRef) {
                    const hir_local = target_expr.kind.LocalRef;
                    // Map HIR local to MIR local (handles struct expansion and reordering)
                    const local = self.param_local_map.get(hir_local) orelse hir_local;
                    try self.ensureLocal(local, target_expr.ty, expr.span);
                    if (value_op) |val| {
                        var final_val = val;
                        if (assign.op != .Assign) {
                            const loaded_tmp = self.newTemp();
                            const mir_ty = mapType(self.hir_crate, target_expr.ty, expr.span, self.diagnostics);
                            _ = try self.emitInst(.{ .ty = mir_ty, .dest = loaded_tmp, .kind = .{ .LoadLocal = .{ .local = local } } });

                            if (mapAssignBin(assign.op)) |bin_op| {
                                const bin_tmp = self.newTemp();
                                _ = try self.emitInst(.{ .ty = mir_ty, .dest = bin_tmp, .kind = .{ .Bin = .{ .op = bin_op, .lhs = .{ .Temp = loaded_tmp }, .rhs = val } } });
                                final_val = .{ .Temp = bin_tmp };
                            }
                        }

                        _ = try self.emitInst(.{ .ty = mapType(self.hir_crate, target_expr.ty, expr.span, self.diagnostics), .dest = null, .kind = .{ .StoreLocal = .{ .local = local, .src = final_val } } });
                    }
                } else if (target_expr.kind == .Unary and target_expr.kind.Unary.op == .Deref) {
                    // Handle dereference assignment: *ptr = value or *ptr += value
                    const ptr_expr_id = target_expr.kind.Unary.expr;
                    const ptr_op = try self.lowerExpr(ptr_expr_id) orelse {
                        self.diagnostics.reportError(expr.span, "could not lower pointer for dereference assignment");
                        return null;
                    };

                    if (value_op) |val| {
                        var final_val = val;
                        if (assign.op != .Assign) {
                            // For compound assignment (*ptr += val), we need to:
                            // 1. Load the current value from the pointer
                            // 2. Perform the binary operation
                            // 3. Store the result back
                            const loaded_tmp = self.newTemp();
                            const mir_ty = mapType(self.hir_crate, target_expr.ty, expr.span, self.diagnostics);
                            _ = try self.emitInst(.{ .ty = mir_ty, .dest = loaded_tmp, .kind = .{ .Unary = .{ .op = .Deref, .operand = ptr_op } } });

                            if (mapAssignBin(assign.op)) |bin_op| {
                                const bin_tmp = self.newTemp();
                                _ = try self.emitInst(.{ .ty = mir_ty, .dest = bin_tmp, .kind = .{ .Bin = .{ .op = bin_op, .lhs = .{ .Temp = loaded_tmp }, .rhs = val } } });
                                final_val = .{ .Temp = bin_tmp };
                            }
                        }

                        _ = try self.emitInst(.{ .ty = mapType(self.hir_crate, target_expr.ty, expr.span, self.diagnostics), .dest = null, .kind = .{ .StorePtr = .{ .ptr = ptr_op, .src = final_val } } });
                    }
                } else if (target_expr.kind == .Index) {
                    // Handle index assignment: array[index] = value or array[index] op= value
                    const array_op = try self.lowerExpr(target_expr.kind.Index.target) orelse {
                        self.diagnostics.reportError(expr.span, "could not lower array for index assignment");
                        return null;
                    };
                    const index_op = try self.lowerExpr(target_expr.kind.Index.index) orelse {
                        self.diagnostics.reportError(expr.span, "could not lower index for index assignment");
                        return null;
                    };
                    if (value_op) |val| {
                        var final_val = val;
                        if (assign.op != .Assign) {
                            // For compound assignment (array[i] += val), we need to:
                            // 1. Load the current value from the array index
                            // 2. Perform the binary operation
                            // 3. Store the result back
                            const loaded_tmp = self.newTemp();
                            const mir_ty = mapType(self.hir_crate, target_expr.ty, expr.span, self.diagnostics);
                            _ = try self.emitInst(.{ .ty = mir_ty, .dest = loaded_tmp, .kind = .{ .Index = .{ .target = array_op, .index = index_op } } });

                            if (mapAssignBin(assign.op)) |bin_op| {
                                const bin_tmp = self.newTemp();
                                _ = try self.emitInst(.{ .ty = mir_ty, .dest = bin_tmp, .kind = .{ .Bin = .{ .op = bin_op, .lhs = .{ .Temp = loaded_tmp }, .rhs = val } } });
                                final_val = .{ .Temp = bin_tmp };
                            }
                        }

                        _ = try self.emitInst(.{ .ty = mapType(self.hir_crate, target_expr.ty, expr.span, self.diagnostics), .dest = null, .kind = .{ .StoreIndex = .{ .target = array_op, .index = index_op, .src = final_val } } });
                    }
                } else if (target_expr.kind == .Field) {
                    // Handle field assignment: struct.field = value or struct.field op= value
                    const struct_op = try self.lowerExpr(target_expr.kind.Field.target) orelse {
                        self.diagnostics.reportError(expr.span, "could not lower struct for field assignment");
                        return null;
                    };
                    const field_name = target_expr.kind.Field.name;
                    if (value_op) |val| {
                        var final_val = val;
                        if (assign.op != .Assign) {
                            // For compound assignment (struct.field += val), we need to:
                            // 1. Load the current value from the field
                            // 2. Perform the binary operation
                            // 3. Store the result back
                            const loaded_tmp = self.newTemp();
                            const mir_ty = mapType(self.hir_crate, target_expr.ty, expr.span, self.diagnostics);
                            _ = try self.emitInst(.{ .ty = mir_ty, .dest = loaded_tmp, .kind = .{ .Field = .{ .target = struct_op, .name = field_name } } });

                            if (mapAssignBin(assign.op)) |bin_op| {
                                const bin_tmp = self.newTemp();
                                _ = try self.emitInst(.{ .ty = mir_ty, .dest = bin_tmp, .kind = .{ .Bin = .{ .op = bin_op, .lhs = .{ .Temp = loaded_tmp }, .rhs = val } } });
                                final_val = .{ .Temp = bin_tmp };
                            }
                        }

                        _ = try self.emitInst(.{ .ty = mapType(self.hir_crate, target_expr.ty, expr.span, self.diagnostics), .dest = null, .kind = .{ .StoreField = .{ .target = struct_op, .name = field_name, .src = final_val } } });
                    }
                } else {
                    self.diagnostics.reportError(expr.span, "assignment target not supported in MIR lowering");
                }
                return null;
            },
            .Return => |ret| {
                const value = if (ret) |val_id| try self.lowerExpr(val_id) else null;
                self.setTerm(.{ .Ret = value });
                const cont = self.newBlock() catch return null;
                self.switchTo(cont);
                return null;
            },
            .If => |iff| {
                const cond = try self.lowerExpr(iff.cond) orelse return null;
                const then_block = try self.newBlock();
                const else_block = try self.newBlock();
                const join_block = try self.newBlock();
                self.setTerm(.{ .If = .{ .cond = cond, .then_block = then_block, .else_block = else_block } });

                // For if expressions with values, use a local variable to store the result
                // This ensures the value is preserved across basic blocks
                var result_local: ?hir.LocalId = null;
                const has_else = iff.else_branch != null;

                self.switchTo(then_block);
                const then_val = try self.lowerExpr(iff.then_branch);
                if (then_val) |tv| {
                    if (has_else) {
                        // Allocate a local for the result
                        result_local = self.next_local;
                        self.next_local += 1;
                        try self.ensureLocal(result_local.?, expr.ty, expr.span);
                        _ = try self.emitInst(.{ .ty = mapType(self.hir_crate, expr.ty, expr.span, self.diagnostics), .dest = null, .kind = .{ .StoreLocal = .{ .local = result_local.?, .src = tv } } });
                    }
                }
                self.setTerm(.{ .Goto = join_block });

                self.switchTo(else_block);
                const else_val = if (iff.else_branch) |else_id| try self.lowerExpr(else_id) else null;
                if (else_val) |ev| {
                    if (result_local == null) {
                        result_local = self.next_local;
                        self.next_local += 1;
                        try self.ensureLocal(result_local.?, expr.ty, expr.span);
                    }
                    _ = try self.emitInst(.{ .ty = mapType(self.hir_crate, expr.ty, expr.span, self.diagnostics), .dest = null, .kind = .{ .StoreLocal = .{ .local = result_local.?, .src = ev } } });
                }
                self.setTerm(.{ .Goto = join_block });

                self.switchTo(join_block);
                if (result_local) |local| return .{ .Local = local };
                if (then_val) |tv| return tv; // No else branch, return then value directly
                return null;
            },
            .While => |while_expr| {
                const cond_block = try self.newBlock();
                const body_block = try self.newBlock();
                const exit_block = try self.newBlock();
                self.setTerm(.{ .Goto = cond_block });

                self.switchTo(cond_block);
                const cond = try self.lowerExpr(while_expr.cond);
                if (cond == null) return null;
                self.setTerm(.{ .If = .{ .cond = cond.?, .then_block = body_block, .else_block = exit_block } });

                self.switchTo(body_block);
                _ = try self.lowerExpr(while_expr.body);
                self.setTerm(.{ .Goto = cond_block });

                self.switchTo(exit_block);
                return null;
            },

            .For => |for_expr| {
                const iter_expr = self.hir_crate.exprs.items[for_expr.iter];

                // 1) Range-based loop (existing behavior)
                if (iter_expr.kind == .Range) {
                    const start_op = try self.lowerExpr(iter_expr.kind.Range.start) orelse return null;
                    const end_op = try self.lowerExpr(iter_expr.kind.Range.end) orelse return null;

                    const mir_ty = mapType(self.hir_crate, iter_expr.ty, expr.span, self.diagnostics);

                    // Allocate a local for the loop variable from next_local and map the pattern ID
                    const loop_local = self.next_local;
                    self.next_local += 1;
                    try self.param_local_map.put(for_expr.pat.id, loop_local);
                    try self.ensureLocal(loop_local, iter_expr.ty, expr.span);
                    _ = try self.emitInst(.{
                        .ty = mir_ty,
                        .dest = null,
                        .kind = .{ .StoreLocal = .{ .local = loop_local, .src = start_op } },
                    });

                    const cond_block = try self.newBlock();
                    const body_block = try self.newBlock();
                    const exit_block = try self.newBlock();
                    self.setTerm(.{ .Goto = cond_block });

                    self.switchTo(cond_block);
                    const cur_tmp = self.newTemp();
                    _ = try self.emitInst(.{
                        .ty = mir_ty,
                        .dest = cur_tmp,
                        .kind = .{ .LoadLocal = .{ .local = loop_local } },
                    });
                    const cmp_tmp = self.newTemp();
                    const cmp_op = if (iter_expr.kind.Range.inclusive) mir.CmpOp.Le else mir.CmpOp.Lt;
                    _ = try self.emitInst(.{
                        .ty = .Bool,
                        .dest = cmp_tmp,
                        .kind = .{
                            .Cmp = .{
                                .op = cmp_op,
                                .lhs = .{ .Temp = cur_tmp },
                                .rhs = end_op,
                            },
                        },
                    });
                    self.setTerm(.{
                        .If = .{
                            .cond = .{ .Temp = cmp_tmp },
                            .then_block = body_block,
                            .else_block = exit_block,
                        },
                    });

                    self.switchTo(body_block);
                    _ = try self.lowerExpr(for_expr.body);

                    const incr_tmp = self.newTemp();
                    _ = try self.emitInst(.{
                        .ty = mir_ty,
                        .dest = incr_tmp,
                        .kind = .{
                            .Bin = .{
                                .op = .Add,
                                .lhs = .{ .Local = loop_local },
                                .rhs = .{ .ImmInt = 1 },
                            },
                        },
                    });
                    _ = try self.emitInst(.{
                        .ty = mir_ty,
                        .dest = null,
                        .kind = .{
                            .StoreLocal = .{
                                .local = loop_local,
                                .src = .{ .Temp = incr_tmp },
                            },
                        },
                    });
                    self.setTerm(.{ .Goto = cond_block });

                    self.switchTo(exit_block);
                    return null;
                }

                // 2) Array-based loop: for elem in array { body }
                const array_info = if (iter_expr.ty < self.hir_crate.types.items.len)
                    self.getArrayInfo(iter_expr.ty)
                else
                    null;

                if (array_info == null) {
                    self.diagnostics.reportError(expr.span, "for loop iterator must be a range or array");
                    return null;
                }

                const elem_ty_id = array_info.?.elem;
                const array_len = array_info.?.len;

                // Evaluate iterator expression once (nums, &nums, etc)
                const array_op = try self.lowerExpr(for_expr.iter) orelse return null;

                // Determine how many local slots are needed for the loop variable
                // For struct elements, we need MAX_STRUCT_FIELDS slots for hash-indexed fields
                var num_elem_slots: hir.LocalId = 1;
                if (elem_ty_id < self.hir_crate.types.items.len) {
                    const elem_ty = self.hir_crate.types.items[elem_ty_id];
                    switch (elem_ty.kind) {
                        .Struct => |info| {
                            if (info.def_id < self.hir_crate.items.items.len) {
                                const s = self.hir_crate.items.items[info.def_id];
                                if (s.kind == .Struct) {
                                    num_elem_slots = MAX_STRUCT_FIELDS;
                                }
                            }
                        },
                        .Path => |path| {
                            if (path.segments.len == 1) {
                                for (self.hir_crate.items.items) |item| {
                                    if (item.kind == .Struct and std.mem.eql(u8, item.kind.Struct.name, path.segments[0])) {
                                        num_elem_slots = MAX_STRUCT_FIELDS;
                                        break;
                                    }
                                }
                            }
                        },
                        else => {},
                    }
                }

                // Allocate locals for the loop variable (element) and map pattern ID
                const loop_local: hir.LocalId = self.next_local;
                self.next_local += num_elem_slots;
                try self.param_local_map.put(for_expr.pat.id, loop_local);
                for (0..num_elem_slots) |i| {
                    try self.ensureLocal(loop_local + @as(hir.LocalId, @intCast(i)), elem_ty_id, expr.span);
                }

                // Allocate a local for the index (we store an i64 counter there)
                const idx_local: hir.LocalId = self.next_local;
                self.next_local += 1;
                try self.ensureLocal(idx_local, null, expr.span);
                if (idx_local < self.locals.items.len) {
                    self.locals.items[idx_local] = .I64;
                }

                // idx = 0
                _ = try self.emitInst(.{
                    .ty = .I64,
                    .dest = null,
                    .kind = .{
                        .StoreLocal = .{
                            .local = idx_local,
                            .src = .{ .ImmInt = 0 },
                        },
                    },
                });

                // Build blocks: cond, body, exit
                const cond_block = try self.newBlock();
                const body_block = try self.newBlock();
                const exit_block = try self.newBlock();
                self.setTerm(.{ .Goto = cond_block });

                // Condition: idx < array_len
                self.switchTo(cond_block);
                const idx_tmp = self.newTemp();
                _ = try self.emitInst(.{
                    .ty = .I64,
                    .dest = idx_tmp,
                    .kind = .{ .LoadLocal = .{ .local = idx_local } },
                });

                const len_tmp = self.newTemp();
                _ = try self.emitInst(.{
                    .ty = .I64,
                    .dest = len_tmp,
                    .kind = .{ .Copy = .{ .src = .{ .ImmInt = array_len } } },
                });

                const cmp_tmp = self.newTemp();
                _ = try self.emitInst(.{
                    .ty = .Bool,
                    .dest = cmp_tmp,
                    .kind = .{
                        .Cmp = .{
                            .op = .Lt,
                            .lhs = .{ .Temp = idx_tmp },
                            .rhs = .{ .Temp = len_tmp },
                        },
                    },
                });

                self.setTerm(.{
                    .If = .{
                        .cond = .{ .Temp = cmp_tmp },
                        .then_block = body_block,
                        .else_block = exit_block,
                    },
                });

                // Body:
                self.switchTo(body_block);

                // Load idx again for indexing
                const idx_body_tmp = self.newTemp();
                _ = try self.emitInst(.{
                    .ty = .I64,
                    .dest = idx_body_tmp,
                    .kind = .{ .LoadLocal = .{ .local = idx_local } },
                });

                // elem = array[idx]
                const elem_tmp = self.newTemp();
                _ = try self.emitInst(.{
                    .ty = mapType(self.hir_crate, elem_ty_id, expr.span, self.diagnostics),
                    .dest = elem_tmp,
                    .kind = .{
                        .Index = .{
                            .target = array_op,
                            .index = .{ .Temp = idx_body_tmp },
                        },
                    },
                });

                // store elem into loop_local (pattern variable)
                _ = try self.emitInst(.{
                    .ty = mapType(self.hir_crate, elem_ty_id, expr.span, self.diagnostics),
                    .dest = null,
                    .kind = .{
                        .StoreLocal = .{
                            .local = loop_local,
                            .src = .{ .Temp = elem_tmp },
                        },
                    },
                });

                // Lower loop body (uses loop_local via LocalRef)
                _ = try self.lowerExpr(for_expr.body);

                // idx += 1
                const idx_old_tmp = self.newTemp();
                _ = try self.emitInst(.{
                    .ty = .I64,
                    .dest = idx_old_tmp,
                    .kind = .{ .LoadLocal = .{ .local = idx_local } },
                });

                const idx_inc_tmp = self.newTemp();
                _ = try self.emitInst(.{
                    .ty = .I64,
                    .dest = idx_inc_tmp,
                    .kind = .{
                        .Bin = .{
                            .op = .Add,
                            .lhs = .{ .Temp = idx_old_tmp },
                            .rhs = .{ .ImmInt = 1 },
                        },
                    },
                });

                _ = try self.emitInst(.{
                    .ty = .I64,
                    .dest = null,
                    .kind = .{
                        .StoreLocal = .{
                            .local = idx_local,
                            .src = .{ .Temp = idx_inc_tmp },
                        },
                    },
                });

                self.setTerm(.{ .Goto = cond_block });

                // Exit:
                self.switchTo(exit_block);
                return null;
            },

            .Cast => |c| {
                const inner = try self.lowerExpr(c.expr) orelse return null;
                const inner_expr = self.hir_crate.exprs.items[c.expr];
                const from_ty = mapType(self.hir_crate, inner_expr.ty, expr.span, self.diagnostics) orelse .Unknown;
                const to_ty = mapType(self.hir_crate, c.ty, expr.span, self.diagnostics) orelse .Unknown;

                // If types are the same or both integer types, just copy
                if (from_ty == to_ty or (isIntegerType(from_ty) and isIntegerType(to_ty))) {
                    return inner;
                }

                // Need actual conversion (e.g., float to int)
                const tmp = self.newTemp();
                _ = try self.emitInst(.{ .ty = to_ty, .dest = tmp, .kind = .{ .Cast = .{ .src = inner, .from_ty = from_ty, .to_ty = to_ty } } });
                return .{ .Temp = tmp };
            },
            .Range => |range| {
                const start = try self.lowerExpr(range.start) orelse return null;
                const end = try self.lowerExpr(range.end) orelse return null;
                const tmp = self.newTemp();
                _ = try self.emitInst(.{ .ty = mapType(self.hir_crate, expr.ty, expr.span, self.diagnostics), .dest = tmp, .kind = .{ .Range = .{ .inclusive = range.inclusive, .start = start, .end = end } } });
                return .{ .Temp = tmp };
            },
            .Index => |index| {
                const target = try self.lowerExpr(index.target) orelse return null;
                const idx = try self.lowerExpr(index.index) orelse return null;
                const tmp = self.newTemp();
                _ = try self.emitInst(.{ .ty = mapType(self.hir_crate, expr.ty, expr.span, self.diagnostics), .dest = tmp, .kind = .{ .Index = .{ .target = target, .index = idx } } });
                return .{ .Temp = tmp };
            },
            .Field => |field| {
                const target = try self.lowerExpr(field.target) orelse return null;
                const tmp = self.newTemp();
                _ = try self.emitInst(.{ .ty = mapType(self.hir_crate, expr.ty, expr.span, self.diagnostics), .dest = tmp, .kind = .{ .Field = .{ .target = target, .name = field.name } } });
                return .{ .Temp = tmp };
            },
            .Array => |elements| {
                var elems = std.ArrayListUnmanaged(mir.Operand){};
                defer elems.deinit(self.allocator);

                for (elements) |elem_id| {
                    if (try self.lowerExpr(elem_id)) |op| {
                        try elems.append(self.allocator, op);
                    }
                }

                const tmp = self.newTemp();
                _ = try self.emitInst(.{ .ty = mapType(self.hir_crate, expr.ty, expr.span, self.diagnostics), .dest = tmp, .kind = .{ .Array = .{ .elems = try elems.toOwnedSlice(self.allocator) } } });
                return .{ .Temp = tmp };
            },
            .StructInit => |struct_init| {
                var fields = std.ArrayListUnmanaged(mir.StructField){};
                defer fields.deinit(self.allocator);

                for (struct_init.fields) |field| {
                    if (try self.lowerExpr(field.value)) |value_op| {
                        try fields.append(self.allocator, .{ .name = field.name, .value = value_op });
                    }
                }

                const tmp = self.newTemp();
                _ = try self.emitInst(.{ .ty = mapType(self.hir_crate, expr.ty, expr.span, self.diagnostics), .dest = tmp, .kind = .{ .StructInit = .{ .fields = try fields.toOwnedSlice(self.allocator) } } });
                return .{ .Temp = tmp };
            },
            .Lambda => |lambda| {
                const fn_id: mir.Operand = blk: {
                    const fn_index: mir.LocalId = @intCast(self.crate.fns.items.len);
                    const name = try std.fmt.allocPrint(self.allocator, "lambda${d}", .{fn_index});
                    var params = try self.allocator.alloc(hir.LocalId, lambda.params.len);
                    for (lambda.params, 0..) |param, idx| {
                        params[idx] = param.id;
                    }

                    const fn_def: hir.Function = .{
                        .def_id = 0,
                        .name = name,
                        .type_params = &[_][]const u8{},
                        .params = params,
                        .param_types = lambda.param_types,
                        .return_type = self.hir_crate.exprs.items[lambda.body].ty,
                        .body = lambda.body,
                        .span = expr.span,
                    };
                    const lowered = try lowerFunction(self.crate, fn_def, self.hir_crate, self.diagnostics);
                    try self.crate.fns.append(self.crate.allocator(), lowered);
                    break :blk .{ .Global = fn_index };
                };

                return fn_id;
            },
            .Path, .MethodCall, .UnresolvedIdent, .Unknown => {
                self.diagnostics.reportError(expr.span, "expression could not be lowered to MIR");
                return null;
            },
        }
    }

    /// Lower a HIR statement to MIR instructions.
    fn lowerStmt(self: *FunctionBuilder, stmt_id: hir.StmtId) LowerError!void {
        const stmt = self.hir_crate.stmts.items[stmt_id];
        switch (stmt.kind) {
            .Let => |let_stmt| {
                // Allocate locals for this let statement
                // For structs, allocate space for all potential hash-indexed fields
                // For arrays of structs, allocate space for all elements * fields
                var num_slots: hir.LocalId = 1;
                var is_struct_array = false;
                if (let_stmt.ty) |ty_id| {
                    if (ty_id < self.hir_crate.types.items.len) {
                        const ty = self.hir_crate.types.items[ty_id];
                        switch (ty.kind) {
                            .Array => |arr| {
                                // Check if this is an array of structs
                                const array_len: hir.LocalId = if (arr.size_const) |sz| @intCast(sz) else 1;
                                if (arr.elem < self.hir_crate.types.items.len) {
                                    const elem_ty = self.hir_crate.types.items[arr.elem];
                                    switch (elem_ty.kind) {
                                        .Struct => |info| {
                                            if (info.def_id < self.hir_crate.items.items.len) {
                                                const s = self.hir_crate.items.items[info.def_id];
                                                if (s.kind == .Struct) {
                                                    const elem_size: hir.LocalId = @intCast(s.kind.Struct.fields.len);
                                                    num_slots = array_len * elem_size;
                                                    is_struct_array = true;
                                                }
                                            }
                                        },
                                        .Path => |path| {
                                            if (path.segments.len == 1) {
                                                for (self.hir_crate.items.items) |item| {
                                                    if (item.kind == .Struct and std.mem.eql(u8, item.kind.Struct.name, path.segments[0])) {
                                                        const elem_size: hir.LocalId = @intCast(item.kind.Struct.fields.len);
                                                        num_slots = array_len * elem_size;
                                                        is_struct_array = true;
                                                        break;
                                                    }
                                                }
                                            }
                                        },
                                        else => {
                                            // For scalar arrays, allocate enough slots to cover all elements
                                            // Each slot is LOCAL_STACK_MULTIPLIER * 8 bytes = 32 bytes
                                            // Array elements are stored at 8-byte intervals
                                            // So we need ceil(array_len / LOCAL_STACK_MULTIPLIER) slots
                                            num_slots = (array_len + LOCAL_STACK_MULTIPLIER - 1) / LOCAL_STACK_MULTIPLIER;
                                            if (num_slots == 0) num_slots = 1;
                                        },
                                    }
                                }
                            },
                            .Struct => |info| {
                                if (info.def_id < self.hir_crate.items.items.len) {
                                    const s = self.hir_crate.items.items[info.def_id];
                                    if (s.kind == .Struct) {
                                        // Use MAX_STRUCT_FIELDS to ensure hash-indexed fields fit
                                        num_slots = MAX_STRUCT_FIELDS;
                                    }
                                }
                            },
                            .Path => |path| {
                                if (path.segments.len == 1) {
                                    for (self.hir_crate.items.items) |item| {
                                        if (item.kind == .Struct and std.mem.eql(u8, item.kind.Struct.name, path.segments[0])) {
                                            // Use MAX_STRUCT_FIELDS to ensure hash-indexed fields fit
                                            num_slots = MAX_STRUCT_FIELDS;
                                            break;
                                        }
                                    }
                                }
                            },
                            else => {},
                        }
                    }
                }

                // Allocate from next_local
                const base_local = self.next_local;
                self.next_local += num_slots;

                // Map the pattern ID to the actual local (base of allocated slots)
                try self.param_local_map.put(let_stmt.pat.id, base_local);

                for (0..num_slots) |i| {
                    try self.ensureLocal(base_local + @as(hir.LocalId, @intCast(i)), let_stmt.ty, stmt.span);
                }

                if (let_stmt.value) |value_id| {
                    const value_expr = self.hir_crate.exprs.items[value_id];

                    // Special handling for struct initialization
                    if (value_expr.kind == .StructInit) {
                        // For struct init, emit StoreLocal for each field directly
                        try self.lowerStructInitToLocal(value_expr.kind.StructInit, base_local, stmt.span);
                    } else if (value_expr.kind == .Array and is_struct_array) {
                        // Special handling for arrays of structs
                        // Get struct fields for storage layout
                        var struct_fields: ?[]const hir.Field = null;
                        if (let_stmt.ty) |ty_id| {
                            if (ty_id < self.hir_crate.types.items.len) {
                                const ty = self.hir_crate.types.items[ty_id];
                                if (ty.kind == .Array) {
                                    const elem_ty_id = ty.kind.Array.elem;
                                    if (elem_ty_id < self.hir_crate.types.items.len) {
                                        const elem_ty = self.hir_crate.types.items[elem_ty_id];
                                        switch (elem_ty.kind) {
                                            .Struct => |info| {
                                                if (info.def_id < self.hir_crate.items.items.len) {
                                                    const s = self.hir_crate.items.items[info.def_id];
                                                    if (s.kind == .Struct) {
                                                        struct_fields = s.kind.Struct.fields;
                                                    }
                                                }
                                            },
                                            .Path => |path| {
                                                if (path.segments.len == 1) {
                                                    for (self.hir_crate.items.items) |item| {
                                                        if (item.kind == .Struct and std.mem.eql(u8, item.kind.Struct.name, path.segments[0])) {
                                                            struct_fields = item.kind.Struct.fields;
                                                            break;
                                                        }
                                                    }
                                                }
                                            },
                                            else => {},
                                        }
                                    }
                                }
                            }
                        }

                        if (struct_fields != null) {
                            // Store each struct element's fields sequentially
                            const fields = struct_fields.?;
                            const elem_size: hir.LocalId = @intCast(fields.len);
                            for (value_expr.kind.Array, 0..) |elem_id, elem_idx| {
                                const elem_expr = self.hir_crate.exprs.items[elem_id];
                                if (elem_expr.kind == .StructInit) {
                                    const struct_init = elem_expr.kind.StructInit;
                                    const elem_base_local = base_local + @as(hir.LocalId, @intCast(elem_idx)) * elem_size;
                                    // Store each field to sequential locals within this element
                                    for (struct_init.fields, 0..) |field, field_idx| {
                                        const value_op = try self.lowerExpr(field.value) orelse continue;
                                        const target_local = elem_base_local + @as(hir.LocalId, @intCast(field_idx));
                                        try self.ensureLocal(target_local, null, stmt.span);
                                        _ = try self.emitInst(.{ .ty = null, .dest = null, .kind = .{ .StoreLocal = .{ .local = target_local, .src = value_op } } });
                                    }
                                }
                            }
                        } else {
                            // Regular array handling
                            if (try self.lowerExpr(value_id)) |value_op| {
                                _ = try self.emitInst(.{ .ty = mapType(self.hir_crate, let_stmt.ty, stmt.span, self.diagnostics), .dest = null, .kind = .{ .StoreLocal = .{ .local = base_local, .src = value_op } } });
                            }
                        }
                    } else {
                        if (try self.lowerExpr(value_id)) |value_op| {
                            // For struct-typed results (from calls), store fields using hash-based layout
                            if (num_slots > 1 and value_expr.kind == .Call) {
                                // Get struct field info to determine hash-based field offsets
                                var field_names: ?[]const []const u8 = null;
                                if (let_stmt.ty) |ty_id| {
                                    if (ty_id < self.hir_crate.types.items.len) {
                                        const ty = self.hir_crate.types.items[ty_id];
                                        switch (ty.kind) {
                                            .Struct => |info| {
                                                if (info.def_id < self.hir_crate.items.items.len) {
                                                    const s = self.hir_crate.items.items[info.def_id];
                                                    if (s.kind == .Struct) {
                                                        const alloc = self.allocator;
                                                        var names = try alloc.alloc([]const u8, s.kind.Struct.fields.len);
                                                        for (s.kind.Struct.fields, 0..) |field, idx| {
                                                            names[idx] = field.name;
                                                        }
                                                        field_names = names;
                                                    }
                                                }
                                            },
                                            .Path => |path| {
                                                if (path.segments.len == 1) {
                                                    for (self.hir_crate.items.items) |item| {
                                                        if (item.kind == .Struct and std.mem.eql(u8, item.kind.Struct.name, path.segments[0])) {
                                                            const alloc = self.allocator;
                                                            var names = try alloc.alloc([]const u8, item.kind.Struct.fields.len);
                                                            for (item.kind.Struct.fields, 0..) |field, idx| {
                                                                names[idx] = field.name;
                                                            }
                                                            field_names = names;
                                                            break;
                                                        }
                                                    }
                                                }
                                            },
                                            else => {},
                                        }
                                    }
                                }

                                if (field_names) |names| {
                                    // Store first return value (rax) to first field using declaration order
                                    if (names.len > 0) {
                                        const first_local = base_local;
                                        _ = try self.emitInst(.{ .ty = mapType(self.hir_crate, let_stmt.ty, stmt.span, self.diagnostics), .dest = null, .kind = .{ .StoreLocal = .{ .local = first_local, .src = value_op } } });
                                    }
                                    // Store second return value (rdx) to second field using declaration order
                                    if (names.len > 1) {
                                        const second_local = base_local + 1;
                                        _ = try self.emitInst(.{ .ty = null, .dest = null, .kind = .{ .StoreLocal = .{ .local = second_local, .src = .RetSecond } } });
                                    }
                                } else {
                                    // Fallback: use sequential layout
                                    _ = try self.emitInst(.{ .ty = mapType(self.hir_crate, let_stmt.ty, stmt.span, self.diagnostics), .dest = null, .kind = .{ .StoreLocal = .{ .local = base_local, .src = value_op } } });
                                    const second_local = base_local + 1;
                                    _ = try self.emitInst(.{ .ty = null, .dest = null, .kind = .{ .StoreLocal = .{ .local = second_local, .src = .RetSecond } } });
                                }
                            } else {
                                _ = try self.emitInst(.{ .ty = mapType(self.hir_crate, let_stmt.ty, stmt.span, self.diagnostics), .dest = null, .kind = .{ .StoreLocal = .{ .local = base_local, .src = value_op } } });
                            }
                        }
                    }
                }
            },
            .Expr => |expr_id| {
                _ = try self.lowerExpr(expr_id);
            },
            .Unknown => {
                self.diagnostics.reportError(stmt.span, "statement could not be lowered to MIR");
            },
        }
    }

    /// Lower a struct initialization expression directly to local variable slots.
    /// Uses declaration order to ensure stable, collision-free field access.
    fn lowerStructInitToLocal(self: *FunctionBuilder, struct_init: hir.StructInit, base_local: hir.LocalId, span: hir.Span) LowerError!void {
        // First, try to find the struct definition to get the correct field order
        var struct_fields_order: ?[]const hir.Field = null;

        // Look up struct definition by path
        if (struct_init.path.len > 0) {
            const struct_name = struct_init.path[struct_init.path.len - 1];
            for (self.hir_crate.items.items) |item| {
                if (item.kind == .Struct and std.mem.eql(u8, item.kind.Struct.name, struct_name)) {
                    struct_fields_order = item.kind.Struct.fields;
                    break;
                }
            }
        }

        // Store each field to a separate local slot using declaration order
        for (struct_init.fields) |field| {
            const value_op = try self.lowerExpr(field.value) orelse continue;

            // Use declaration order from struct definition
            const field_index: hir.LocalId = if (struct_fields_order) |fields|
                getFieldIndexFromDeclaration(fields, field.name)
            else
                // Fallback: iterate struct_init.fields to find position
                blk: {
                    for (struct_init.fields, 0..) |f, idx| {
                        if (std.mem.eql(u8, f.name, field.name)) {
                            break :blk @intCast(idx);
                        }
                    }
                    break :blk 0;
                };

            // Store to local slot: base_local + field_index
            const target_local = base_local + field_index;
            try self.ensureLocal(target_local, null, span);
            _ = try self.emitInst(.{ .ty = null, .dest = null, .kind = .{ .StoreLocal = .{ .local = target_local, .src = value_op } } });
        }
    }

    /// Finalize the function builder and produce the completed MIR function.
    fn finish(self: *FunctionBuilder) LowerError!mir.MirFn {
        var blocks = std.ArrayListUnmanaged(mir.Block){};
        defer blocks.deinit(self.allocator);

        for (self.blocks.items) |*blk_state| {
            const term = blk_state.term orelse mir.TermKind{ .Ret = null };
            try blocks.append(self.allocator, .{ .insts = try blk_state.insts.toOwnedSlice(self.allocator), .term = term });
        }

        const locals_slice = try self.locals.toOwnedSlice(self.allocator);
        const blocks_slice = try blocks.toOwnedSlice(self.allocator);
        self.blocks.deinit(self.allocator);
        self.locals.deinit(self.allocator);
        self.mir_fn.locals = locals_slice;
        self.mir_fn.blocks = blocks_slice;
        return self.mir_fn;
    }

    /// Resolve the builtin macro handler associated with a callee expression, if any.
    fn builtinMacroHandler(self: *FunctionBuilder, callee_id: hir.ExprId) ?hir.BuiltinMacroHandler {
        if (callee_id >= self.hir_crate.exprs.items.len) return null;
        const callee = self.hir_crate.exprs.items[callee_id];
        if (callee.kind != .GlobalRef) return null;

        const def_id = callee.kind.GlobalRef;
        return self.hir_crate.builtin_macros.get(def_id);
    }

    /// Information about a pointer method call (e.g., ptr.is_null()).
    const PointerMethodInfo = struct {
        target_id: hir.ExprId,
        method_name: []const u8,
    };

    /// Check if a callee expression is a pointer method call.
    /// Returns method info if it is, null otherwise.
    fn isPointerMethodCall(self: *FunctionBuilder, callee_id: hir.ExprId) ?PointerMethodInfo {
        if (callee_id >= self.hir_crate.exprs.items.len) return null;
        const callee = self.hir_crate.exprs.items[callee_id];

        // Check if the callee is a Field expression (method call syntax)
        if (callee.kind != .Field) return null;

        const field = callee.kind.Field;
        const target_expr = self.hir_crate.exprs.items[field.target];

        // Check if the target is a pointer type
        if (target_expr.ty >= self.hir_crate.types.items.len) return null;
        const target_type = self.hir_crate.types.items[target_expr.ty];

        switch (target_type.kind) {
            .Pointer => {
                return PointerMethodInfo{
                    .target_id = field.target,
                    .method_name = field.name,
                };
            },
            else => return null,
        }
    }

    /// Lower a pointer method call (e.g., ptr.is_null()).
    fn lowerPointerMethodCall(self: *FunctionBuilder, target_id: hir.ExprId, method_name: []const u8, args: []const hir.ExprId, expr: hir.Expr) LowerError!?mir.Operand {
        _ = args; // Currently no pointer methods use arguments

        if (std.mem.eql(u8, method_name, "is_null")) {
            // is_null() returns true if pointer is null (0), false otherwise
            const ptr_op = try self.lowerExpr(target_id) orelse return null;
            const tmp = self.newTemp();
            // Compare pointer to 0 and return bool result
            _ = try self.emitInst(.{ .ty = .Bool, .dest = tmp, .kind = .{ .Cmp = .{ .op = .Eq, .lhs = ptr_op, .rhs = .{ .ImmInt = 0 } } } });
            return .{ .Temp = tmp };
        } else {
            self.diagnostics.reportError(expr.span, "unknown pointer method");
            return null;
        }
    }

    /// Information about an array method call (e.g., arr.len()).
    const ArrayMethodInfo = struct {
        target_id: hir.ExprId,
        method_name: []const u8,
        array_size: ?i64,
    };

    /// Check if a callee expression is an array method call.
    /// Returns method info if it is, null otherwise.
    fn isArrayMethodCall(self: *FunctionBuilder, callee_id: hir.ExprId) ?ArrayMethodInfo {
        if (callee_id >= self.hir_crate.exprs.items.len) return null;
        const callee = self.hir_crate.exprs.items[callee_id];

        // Check if the callee is a Field expression (method call syntax)
        if (callee.kind != .Field) return null;

        const field = callee.kind.Field;
        const target_expr = self.hir_crate.exprs.items[field.target];

        // Check if the target is an array type
        if (target_expr.ty >= self.hir_crate.types.items.len) return null;
        const target_type = self.hir_crate.types.items[target_expr.ty];

        // Check for array or ref/pointer to array
        const array_size: ?i64 = switch (target_type.kind) {
            .Array => |arr| arr.size_const,
            .Ref => |ref| blk: {
                if (ref.inner >= self.hir_crate.types.items.len) break :blk null;
                const inner_type = self.hir_crate.types.items[ref.inner];
                if (inner_type.kind == .Array) {
                    break :blk inner_type.kind.Array.size_const;
                }
                break :blk null;
            },
            .Pointer => |ptr| blk: {
                if (ptr.inner >= self.hir_crate.types.items.len) break :blk null;
                const inner_type = self.hir_crate.types.items[ptr.inner];
                if (inner_type.kind == .Array) {
                    break :blk inner_type.kind.Array.size_const;
                }
                break :blk null;
            },
            else => null,
        };

        if (array_size != null) {
            return ArrayMethodInfo{
                .target_id = field.target,
                .method_name = field.name,
                .array_size = array_size,
            };
        }
        return null;
    }

    /// Extract array element type and length from a type ID.
    /// Handles direct arrays, references to arrays, and pointers to arrays.
    fn getArrayInfo(self: *FunctionBuilder, ty_id: hir.TypeId) ?struct {
        elem: hir.TypeId,
        len: i64,
    } {
        if (ty_id >= self.hir_crate.types.items.len) return null;
        const ty = self.hir_crate.types.items[ty_id];

        switch (ty.kind) {
            .Array => |arr| {
                if (arr.size_const) |sz| {
                    return .{ .elem = arr.elem, .len = sz };
                }
                return null;
            },
            .Ref => |ref_info| {
                if (ref_info.inner >= self.hir_crate.types.items.len) return null;
                const inner = self.hir_crate.types.items[ref_info.inner];
                if (inner.kind == .Array) {
                    const arr = inner.kind.Array;
                    if (arr.size_const) |sz| {
                        return .{ .elem = arr.elem, .len = sz };
                    }
                }
                return null;
            },
            .Pointer => |ptr_info| {
                if (ptr_info.inner >= self.hir_crate.types.items.len) return null;
                const inner = self.hir_crate.types.items[ptr_info.inner];
                if (inner.kind == .Array) {
                    const arr = inner.kind.Array;
                    if (arr.size_const) |sz| {
                        return .{ .elem = arr.elem, .len = sz };
                    }
                }
                return null;
            },
            else => return null,
        }
    }

    /// Lower an array method call (e.g., arr.len()).
    fn lowerArrayMethodCall(self: *FunctionBuilder, _: hir.ExprId, method_name: []const u8, array_size: ?i64, _: []const hir.ExprId, expr: hir.Expr) LowerError!?mir.Operand {
        if (std.mem.eql(u8, method_name, "len")) {
            // len() returns the static array size
            if (array_size) |size| {
                const tmp = self.newTemp();
                _ = try self.emitInst(.{ .ty = .Usize, .dest = tmp, .kind = .{ .Copy = .{ .src = .{ .ImmInt = size } } } });
                return .{ .Temp = tmp };
            } else {
                self.diagnostics.reportError(expr.span, "array size is unknown for len()");
                return null;
            }
        } else {
            self.diagnostics.reportError(expr.span, "unknown array method");
            return null;
        }
    }

    /// Information about a struct method call (e.g., point.offset(1, 2)).
    const StructMethodInfo = struct {
        target_id: hir.ExprId,
        method_name: []const u8,
        method_def_id: hir.DefId,
    };

    /// Check if a callee expression is a struct method call.
    /// Searches impl blocks to find matching methods.
    fn isStructMethodCall(self: *FunctionBuilder, callee_id: hir.ExprId) ?StructMethodInfo {
        if (callee_id >= self.hir_crate.exprs.items.len) return null;
        const callee = self.hir_crate.exprs.items[callee_id];

        // Check if the callee is a Field expression (method call syntax)
        if (callee.kind != .Field) return null;

        const field = callee.kind.Field;
        const target_expr = self.hir_crate.exprs.items[field.target];

        // Check if the target is a struct type
        if (target_expr.ty >= self.hir_crate.types.items.len) return null;
        const target_type = self.hir_crate.types.items[target_expr.ty];

        const struct_def_id: ?hir.DefId = switch (target_type.kind) {
            .Struct => |info| info.def_id,
            .Path => |path| blk: {
                // Look up the struct by path name
                if (path.segments.len == 1) {
                    const struct_name = path.segments[0];
                    for (self.hir_crate.items.items, 0..) |item, idx| {
                        if (item.kind == .Struct) {
                            if (std.mem.eql(u8, item.kind.Struct.name, struct_name)) {
                                break :blk @intCast(idx);
                            }
                        }
                    }
                }
                break :blk null;
            },
            // Handle references to structs (e.g., &Counter, &mut Counter)
            .Ref => |ref_info| blk: {
                if (ref_info.inner < self.hir_crate.types.items.len) {
                    const inner_ty = self.hir_crate.types.items[ref_info.inner];
                    switch (inner_ty.kind) {
                        .Struct => |info| break :blk info.def_id,
                        .Path => |path| {
                            if (path.segments.len == 1) {
                                const struct_name = path.segments[0];
                                for (self.hir_crate.items.items, 0..) |item, idx| {
                                    if (item.kind == .Struct) {
                                        if (std.mem.eql(u8, item.kind.Struct.name, struct_name)) {
                                            break :blk @intCast(idx);
                                        }
                                    }
                                }
                            }
                        },
                        else => {},
                    }
                }
                break :blk null;
            },
            else => null,
        };

        if (struct_def_id == null) return null;

        // Look for a method with this name in impl blocks
        for (self.hir_crate.items.items) |item| {
            if (item.kind == .Impl) {
                const impl_item = item.kind.Impl;
                // Check if this impl is for our struct
                var matches = false;
                if (impl_item.target < self.hir_crate.types.items.len) {
                    const impl_target = self.hir_crate.types.items[impl_item.target].kind;
                    switch (impl_target) {
                        .Struct => |info| matches = info.def_id == struct_def_id.?,
                        .Path => |path| {
                            if (path.segments.len == 1) {
                                const struct_item = self.hir_crate.items.items[struct_def_id.?];
                                if (struct_item.kind == .Struct) {
                                    matches = std.mem.eql(u8, path.segments[0], struct_item.kind.Struct.name);
                                }
                            }
                        },
                        else => {},
                    }
                }

                if (matches) {
                    // Look for the method
                    for (impl_item.methods) |method_id| {
                        if (method_id < self.hir_crate.items.items.len) {
                            const method_item = self.hir_crate.items.items[method_id];
                            if (method_item.kind == .Function) {
                                const func = method_item.kind.Function;
                                // Method name is mangled as StructName_methodName
                                // Check if it ends with _methodName
                                if (std.mem.endsWith(u8, func.name, field.name) and
                                    func.name.len > field.name.len and
                                    func.name[func.name.len - field.name.len - 1] == '_')
                                {
                                    return StructMethodInfo{
                                        .target_id = field.target,
                                        .method_name = func.name, // Use mangled name
                                        .method_def_id = method_id,
                                    };
                                }
                            }
                        }
                    }
                }
            }
        }
        return null;
    }

    /// Lower a struct method call, passing struct fields as arguments.
    fn lowerStructMethodCall(self: *FunctionBuilder, target_id: hir.ExprId, method_name: []const u8, method_def_id: hir.DefId, args: []const hir.ExprId, expr: hir.Expr) LowerError!?mir.Operand {
        _ = method_name;

        // Lower the target (self) expression
        const target_expr = self.hir_crate.exprs.items[target_id];

        // Create call arguments
        var call_args = std.ArrayListUnmanaged(mir.Operand){};
        defer call_args.deinit(self.allocator);

        // Get the method function to check if first param is a reference (self) type
        const method_item = self.hir_crate.items.items[method_def_id];
        if (method_item.kind != .Function) {
            self.diagnostics.reportError(expr.span, "method is not a function");
            return null;
        }
        const method_func = method_item.kind.Function;

        // Check if the first parameter is a reference type (&self or &mut self)
        var first_param_is_ref = false;
        if (method_func.param_types.len > 0) {
            const first_param_ty = method_func.param_types[0];
            if (first_param_ty < self.hir_crate.types.items.len) {
                const param_type = self.hir_crate.types.items[first_param_ty];
                switch (param_type.kind) {
                    .Ref, .Pointer => first_param_is_ref = true,
                    else => {},
                }
            }
        }

        // For struct method calls with reference parameters (&self, &mut self),
        // pass a pointer to the struct. For value parameters (self), pass the value.
        if (target_expr.kind == .LocalRef) {
            const hir_local = target_expr.kind.LocalRef;
            // Map HIR local to MIR local (handles struct expansion)
            const base_local = self.param_local_map.get(hir_local) orelse hir_local;

            if (first_param_is_ref) {
                // Pass a reference to the struct (address of local)
                // Create a Ref operation to get the address
                const tmp = self.newTemp();
                _ = try self.emitInst(.{ .ty = .Pointer, .dest = tmp, .kind = .{ .Unary = .{ .op = .Ref, .operand = .{ .Local = base_local } } } });
                try call_args.append(self.allocator, .{ .Temp = tmp });
            } else {
                // Pass the struct value - determine field count and pass each field
                var num_fields: usize = 1; // Default to 1 if type info unavailable
                if (target_expr.ty < self.hir_crate.types.items.len) {
                    const target_type = self.hir_crate.types.items[target_expr.ty];
                    switch (target_type.kind) {
                        .Struct => |info| {
                            if (info.def_id < self.hir_crate.items.items.len) {
                                const struct_item = self.hir_crate.items.items[info.def_id];
                                if (struct_item.kind == .Struct) {
                                    num_fields = struct_item.kind.Struct.fields.len;
                                }
                            }
                        },
                        .Path => |path| {
                            // Look up struct by name
                            if (path.segments.len == 1) {
                                const struct_name = path.segments[0];
                                for (self.hir_crate.items.items) |item| {
                                    if (item.kind == .Struct and std.mem.eql(u8, item.kind.Struct.name, struct_name)) {
                                        num_fields = item.kind.Struct.fields.len;
                                        break;
                                    }
                                }
                            }
                        },
                        else => {},
                    }
                }

                // Pass each field as a separate argument
                for (0..num_fields) |field_idx| {
                    const field_local: hir.LocalId = base_local + @as(hir.LocalId, @intCast(field_idx));
                    try call_args.append(self.allocator, .{ .Local = field_local });
                }
            }
        } else {
            // Fallback: just use the lowered expression
            const self_op = try self.lowerExpr(target_id) orelse return null;
            if (first_param_is_ref) {
                // Need to take address of the expression result
                const tmp = self.newTemp();
                _ = try self.emitInst(.{ .ty = .Pointer, .dest = tmp, .kind = .{ .Unary = .{ .op = .Ref, .operand = self_op } } });
                try call_args.append(self.allocator, .{ .Temp = tmp });
            } else {
                try call_args.append(self.allocator, self_op);
            }
        }

        // Add the rest of the arguments
        for (args) |arg_id| {
            if (try self.lowerExpr(arg_id)) |arg_op| {
                try call_args.append(self.allocator, arg_op);
            }
        }

        // Use Symbol to reference the method by name (avoids HIR/MIR index mismatch)
        const tmp = self.newTemp();
        _ = try self.emitInst(.{ .ty = mapType(self.hir_crate, expr.ty, expr.span, self.diagnostics), .dest = tmp, .kind = .{ .Call = .{ .target = .{ .Symbol = method_func.name }, .args = try call_args.toOwnedSlice(self.allocator) } } });
        return .{ .Temp = tmp };
    }

    /// Lower a builtin print-style macro call to a printf call.
    /// Converts Rust-style format strings to C-style format specifiers.
    fn lowerPrintMacro(
        self: *FunctionBuilder,
        macro_label: []const u8,
        args_ids: []const hir.ExprId,
        span: hir.Span,
        append_newline: bool,
    ) LowerError!?mir.Operand {
        if (args_ids.len == 0) {
            var message_buf: [64]u8 = undefined;
            const message = std.fmt.bufPrint(&message_buf, "{s} requires a format string", .{macro_label}) catch "macro requires a format string";
            self.diagnostics.reportError(span, message);
            return null;
        }

        const fmt_expr = self.hir_crate.exprs.items[args_ids[0]];
        if (fmt_expr.kind != .ConstString) {
            var message_buf: [80]u8 = undefined;
            const message = std.fmt.bufPrint(&message_buf, "{s} expects a string literal format", .{macro_label}) catch "macro expects a string literal format";
            self.diagnostics.reportError(span, message);
            return null;
        }

        var fmt_buf = std.ArrayListUnmanaged(u8){};
        defer fmt_buf.deinit(self.allocator);

        var args = std.ArrayListUnmanaged(mir.Operand){};
        defer args.deinit(self.allocator);

        var arg_idx: usize = 1;
        var i: usize = 0;
        const raw_fmt = stripQuotes(fmt_expr.kind.ConstString);
        while (i < raw_fmt.len) : (i += 1) {
            const ch = raw_fmt[i];
            if (ch == '{') {
                if (i + 1 < raw_fmt.len and raw_fmt[i + 1] == '{') {
                    try fmt_buf.append(self.allocator, '{');
                    i += 1;
                    continue;
                }
                const end = std.mem.indexOfScalarPos(u8, raw_fmt, i, '}') orelse {
                    self.diagnostics.reportError(span, "unterminated format placeholder");
                    return null;
                };
                if (arg_idx >= args_ids.len) {
                    self.diagnostics.reportError(span, "not enough arguments for format string");
                    return null;
                }

                const spec = self.printfSpecifier(args_ids[arg_idx], span) orelse return null;
                try fmt_buf.appendSlice(self.allocator, spec);
                if (try self.lowerExpr(args_ids[arg_idx])) |arg_op| {
                    // Check if this is a reference from a method call that needs dereferencing
                    // We only dereference if the expression is a method call returning &T
                    // where T is a concrete type (not a direct &str literal)
                    const arg_expr = self.hir_crate.exprs.items[args_ids[arg_idx]];
                    var needs_deref = false;

                    // Only dereference for method calls (like wrapper.get()) returning &T
                    // where T is a Path (generic type parameter)
                    const is_method_call = (arg_expr.kind == .Call);
                    if (is_method_call and arg_expr.ty < self.hir_crate.types.items.len) {
                        const arg_type = self.hir_crate.types.items[arg_expr.ty];
                        if (arg_type.kind == .Ref) {
                            const ref_info = arg_type.kind.Ref;
                            if (ref_info.inner < self.hir_crate.types.items.len) {
                                const inner_kind = self.hir_crate.types.items[ref_info.inner].kind;
                                switch (inner_kind) {
                                    // For generic Path types from method calls, dereference
                                    .Path => needs_deref = true,
                                    else => {},
                                }
                            }
                        }
                    }

                    if (needs_deref) {
                        // Dereference the pointer to get the value
                        const deref_tmp = self.newTemp();
                        _ = try self.emitInst(.{ .ty = .I64, .dest = deref_tmp, .kind = .{ .Unary = .{ .op = .Deref, .operand = arg_op } } });
                        try args.append(self.allocator, .{ .Temp = deref_tmp });
                    } else {
                        try args.append(self.allocator, arg_op);
                    }
                }

                arg_idx += 1;
                i = end;
                continue;
            } else if (ch == '}' and i + 1 < raw_fmt.len and raw_fmt[i + 1] == '}') {
                try fmt_buf.append(self.allocator, '}');
                i += 1;
                continue;
            }

            try fmt_buf.append(self.allocator, ch);
        }

        if (arg_idx != args_ids.len) {
            var message_buf: [80]u8 = undefined;
            const message = std.fmt.bufPrint(&message_buf, "too many arguments supplied to {s}", .{macro_label}) catch "too many arguments supplied to macro";
            self.diagnostics.reportError(span, message);
            return null;
        }

        if (append_newline and (fmt_buf.items.len == 0 or fmt_buf.items[fmt_buf.items.len - 1] != '\n')) {
            try fmt_buf.append(self.allocator, '\n');
        }

        const fmt_owned = try fmt_buf.toOwnedSlice(self.allocator);

        const call_args = try self.allocator.alloc(mir.Operand, args.items.len + 1);
        call_args[0] = .{ .ImmString = fmt_owned };
        if (args.items.len > 0) {
            std.mem.copyForwards(mir.Operand, call_args[1..], args.items);
        }

        _ = try self.emitInst(.{ .ty = .I32, .dest = null, .kind = .{ .Call = .{ .target = .{ .Symbol = "printf" }, .args = call_args } } });
        return .{ .ImmInt = 0 };
    }

    /// Dispatch lowering for a builtin macro based on its registered handler.
    fn lowerBuiltinMacro(
        self: *FunctionBuilder,
        handler: hir.BuiltinMacroHandler,
        args_ids: []const hir.ExprId,
        span: hir.Span,
    ) LowerError!?mir.Operand {
        switch (handler) {
            .println => return try self.lowerPrintMacro("println!", args_ids, span, true),
            .print => return try self.lowerPrintMacro("print!", args_ids, span, false),
            .eprintln => return try self.lowerPrintMacro("eprintln!", args_ids, span, true),
            .format => {
                self.diagnostics.reportError(span, "format! macro is not supported yet");
                return null;
            },
        }
    }

    /// Determine the printf format specifier for an expression's type.
    fn printfSpecifier(self: *FunctionBuilder, expr_id: hir.ExprId, span: hir.Span) ?[]const u8 {
        if (expr_id >= self.hir_crate.exprs.items.len) {
            self.diagnostics.reportError(span, "unknown argument in builtin macro call");
            return null;
        }
        const expr = self.hir_crate.exprs.items[expr_id];

        // Check HIR type directly for references like &str
        if (expr.ty < self.hir_crate.types.items.len) {
            const hir_type = self.hir_crate.types.items[expr.ty].kind;
            switch (hir_type) {
                .Ref => |ref_info| {
                    // Check what the reference is to
                    if (ref_info.inner < self.hir_crate.types.items.len) {
                        const inner_kind = self.hir_crate.types.items[ref_info.inner].kind;
                        switch (inner_kind) {
                            .Str => return "%s",
                            .String => return "%s",
                            .PrimInt => |int_ty| return switch (int_ty) {
                                .I32, .U32 => "%d",
                                .I64, .U64, .Usize => "%ld",
                            },
                            // For generic type parameters (Path), try to infer from context
                            // If we're referencing something from a call expression that returns a value,
                            // default to %ld for numeric or %s for string-like values
                            .Path => {
                                // Try to look at the expression being passed
                                // For method calls like .get(), infer from the target type
                                if (self.inferFormatFromExpr(expr_id)) |fmt| return fmt;
                                return "%s"; // Default to string for unknown generic refs
                            },
                            else => {},
                        }
                    }
                },
                .Pointer => |ptr_info| {
                    // Check what the pointer is to
                    if (ptr_info.inner < self.hir_crate.types.items.len) {
                        const inner_kind = self.hir_crate.types.items[ptr_info.inner].kind;
                        switch (inner_kind) {
                            .Str => return "%s",
                            .String => return "%s",
                            .PrimInt => |int_ty| return switch (int_ty) {
                                .I32, .U32 => "%d",
                                .I64, .U64, .Usize => "%ld",
                            },
                            else => {},
                        }
                    }
                },
                else => {},
            }
        }

        const ty = mapType(self.hir_crate, expr.ty, span, self.diagnostics) orelse {
            self.diagnostics.reportError(span, "builtin macro argument has unsupported type");
            return null;
        };

        return switch (ty) {
            .I32, .U32 => "%d",
            .I64, .U64, .Usize => "%ld",
            .F32, .F64 => "%f",
            .Bool => "%d",
            .Char => "%c",
            .String, .Str => "%s",
            .Pointer => "%p", // Default pointer format
            else => {
                self.diagnostics.reportError(span, "builtin macro argument type cannot be formatted");
                return null;
            },
        };
    }

    /// Try to infer the format specifier by looking at the expression context.
    /// For method calls on generic containers, try to infer from the original value.
    fn inferFormatFromExpr(self: *FunctionBuilder, expr_id: hir.ExprId) ?[]const u8 {
        if (expr_id >= self.hir_crate.exprs.items.len) return null;
        const expr = self.hir_crate.exprs.items[expr_id];

        switch (expr.kind) {
            // For method calls like wrapper.get(), look at the target's stored value
            .MethodCall => |call| {
                // Get the target expression (e.g., the Wrapper instance)
                if (call.target < self.hir_crate.exprs.items.len) {
                    const target_expr = self.hir_crate.exprs.items[call.target];
                    // If target is a LocalRef, check if we can find what was stored in it
                    if (target_expr.kind == .LocalRef) {
                        // Try to find the let binding that initialized this local
                        return self.inferFormatFromLocal(target_expr.kind.LocalRef);
                    }
                }
            },
            // For regular calls, look at the arguments
            .Call => |call| {
                // If this is a call with a field access callee (like text.get())
                if (call.callee < self.hir_crate.exprs.items.len) {
                    const callee_expr = self.hir_crate.exprs.items[call.callee];
                    if (callee_expr.kind == .Field) {
                        const target = callee_expr.kind.Field.target;
                        if (target < self.hir_crate.exprs.items.len) {
                            const target_expr = self.hir_crate.exprs.items[target];
                            if (target_expr.kind == .LocalRef) {
                                return self.inferFormatFromLocal(target_expr.kind.LocalRef);
                            }
                        }
                    }
                }
            },
            else => {},
        }
        return null;
    }

    /// Try to infer the format specifier from a local variable's initialization.
    fn inferFormatFromLocal(self: *FunctionBuilder, local_id: hir.LocalId) ?[]const u8 {
        // Search through statements to find the let binding
        for (self.hir_crate.stmts.items) |stmt| {
            if (stmt.kind == .Let) {
                const let_stmt = stmt.kind.Let;
                if (let_stmt.pat.id == local_id) {
                    if (let_stmt.value) |value_id| {
                        // Look at the initialization expression
                        if (value_id < self.hir_crate.exprs.items.len) {
                            const value_expr = self.hir_crate.exprs.items[value_id];
                            // If initialized with a constructor call, look at the first argument
                            if (value_expr.kind == .Call) {
                                const call = value_expr.kind.Call;
                                if (call.args.len > 0) {
                                    const first_arg = call.args[0];
                                    if (first_arg < self.hir_crate.exprs.items.len) {
                                        const arg_expr = self.hir_crate.exprs.items[first_arg];
                                        // Check the type of the argument
                                        if (arg_expr.ty < self.hir_crate.types.items.len) {
                                            const arg_type = self.hir_crate.types.items[arg_expr.ty].kind;
                                            switch (arg_type) {
                                                .String, .Str => return "%s",
                                                .Ref => |ref_info| {
                                                    if (ref_info.inner < self.hir_crate.types.items.len) {
                                                        const inner = self.hir_crate.types.items[ref_info.inner].kind;
                                                        if (inner == .Str or inner == .String) return "%s";
                                                    }
                                                },
                                                .PrimInt => |int_ty| return switch (int_ty) {
                                                    .I32, .U32 => "%d",
                                                    .I64, .U64, .Usize => "%ld",
                                                },
                                                else => {},
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
        return null;
    }

    /// Lower a short-circuit logical operation (&&, ||).
    /// Creates conditional branches to implement short-circuit evaluation.
    fn lowerLogical(
        self: *FunctionBuilder,
        op: hir.BinaryOp,
        lhs: mir.Operand,
        rhs_id: hir.ExprId,
        ty: hir.TypeId,
        span: hir.Span,
    ) LowerError!?mir.Operand {
        const rhs_block = try self.newBlock();
        const short_block = try self.newBlock();
        const join_block = try self.newBlock();
        const result_tmp = self.newTemp();

        const then_block = if (op == .LogicalAnd) rhs_block else short_block;
        const else_block = if (op == .LogicalAnd) short_block else rhs_block;

        self.setTerm(.{ .If = .{ .cond = lhs, .then_block = then_block, .else_block = else_block } });

        self.switchTo(short_block);
        const short_val: mir.Operand = if (op == .LogicalAnd) .{ .ImmBool = false } else .{ .ImmBool = true };
        _ = try self.emitInst(.{ .ty = .Bool, .dest = result_tmp, .kind = .{ .Copy = .{ .src = short_val } } });
        self.setTerm(.{ .Goto = join_block });

        self.switchTo(rhs_block);
        if (try self.lowerExpr(rhs_id)) |rhs| {
            _ = try self.emitInst(.{ .ty = mapType(self.hir_crate, ty, span, self.diagnostics), .dest = result_tmp, .kind = .{ .Copy = .{ .src = rhs } } });
        }
        self.setTerm(.{ .Goto = join_block });

        self.switchTo(join_block);
        return .{ .Temp = result_tmp };
    }
};

/// Strips surrounding quotes from a string literal if present.
fn stripQuotes(s: []const u8) []const u8 {
    if (s.len >= 2 and s[0] == '"' and s[s.len - 1] == '"') {
        return s[1 .. s.len - 1];
    }
    return s;
}

/// Map a HIR type to its corresponding MIR type.
/// Handles primitive types, compound types, and path-based type references.
/// Reports diagnostics for unsupported or unknown types.
fn mapType(hir_crate: *const hir.Crate, ty_id: ?hir.TypeId, span: hir.Span, diagnostics: *diag.Diagnostics) ?mir.MirType {
    if (ty_id) |id| {
        if (id >= hir_crate.types.items.len) {
            diagnostics.reportError(span, "unknown type id during MIR lowering");
            return null;
        }
        return switch (hir_crate.types.items[id].kind) {
            .PrimInt => |int_ty| switch (int_ty) {
                .I32 => .I32,
                .I64 => .I64,
                .U32 => .U32,
                .U64 => .U64,
                .Usize => .Usize,
            },
            .PrimFloat => |float_ty| switch (float_ty) {
                .F32 => .F32,
                .F64 => .F64,
            },
            .Bool => .Bool,
            .Char => .Char,
            .String => .String,
            .Str => .Str,
            .Array => .Array,
            .Pointer, .Ref, .Fn => .Pointer,
            .Struct => .Struct,
            .Path => |path| blk: {
                // Try to resolve the path type
                if (path.segments.len == 1) {
                    const name = path.segments[0];
                    // Look for a type alias or struct with this name
                    for (hir_crate.items.items) |item| {
                        switch (item.kind) {
                            .TypeAlias => |alias| {
                                if (std.mem.eql(u8, alias.name, name)) {
                                    // Recursively resolve the aliased type
                                    return mapType(hir_crate, alias.target, span, diagnostics);
                                }
                            },
                            .Struct => |struct_item| {
                                if (std.mem.eql(u8, struct_item.name, name)) {
                                    break :blk .Struct;
                                }
                            },
                            else => {},
                        }
                    }
                }
                diagnostics.reportWarning(span, "type not supported in MIR lowering");
                break :blk null;
            },
            .Unknown => blk: {
                diagnostics.reportWarning(span, "unknown type information during MIR lowering");
                break :blk null;
            },
        };
    }

    diagnostics.reportWarning(span, "missing type information during MIR lowering");
    return null;
}

/// Map a HIR binary operator to the corresponding MIR binary operation.
/// Note: comparison and logical operators return Add as a placeholder since
/// they are handled separately via Cmp instructions.
fn mapBin(op: hir.BinaryOp) mir.BinOp {
    return switch (op) {
        .Add => .Add,
        .Sub => .Sub,
        .Mul => .Mul,
        .Div => .Div,
        .Mod => .Mod,
        .LogicalAnd, .LogicalOr, .Eq, .Ne, .Lt, .Le, .Gt, .Ge => .Add,
    };
}

/// Map a compound assignment operator to its underlying binary operation.
/// Returns null for plain assignment (=).
fn mapAssignBin(op: hir.AssignOp) ?mir.BinOp {
    return switch (op) {
        .Assign => null,
        .AddAssign => .Add,
        .SubAssign => .Sub,
        .MulAssign => .Mul,
        .DivAssign => .Div,
    };
}

/// Map a HIR comparison operator to the corresponding MIR comparison operation.
/// Non-comparison operators return Eq as a placeholder (should not occur in practice).
fn mapCmp(op: hir.BinaryOp) mir.CmpOp {
    return switch (op) {
        .Eq => .Eq,
        .Ne => .Ne,
        .Lt => .Lt,
        .Le => .Le,
        .Gt => .Gt,
        .Ge => .Ge,
        .LogicalAnd, .LogicalOr, .Add, .Sub, .Mul, .Div, .Mod => .Eq,
    };
}

/// Map a HIR unary operator to the corresponding MIR unary operation.
/// Returns null for unsupported operators.
fn mapUnary(op: hir.UnaryOp) ?mir.UnaryOp {
    return switch (op) {
        .Not => .Not,
        .Neg => .Neg,
        .Deref => .Deref,
        .Ref, .RefMut => .Ref,
    };
}

/// Check if a MIR type is an integer type.
fn isIntegerType(ty: mir.MirType) bool {
    return switch (ty) {
        .I32, .I64, .U32, .U64, .Usize => true,
        else => false,
    };
}

test "lower function with simple block" {
    const allocator = std.testing.allocator;
    var diagnostics = diag.Diagnostics.init(allocator);
    defer diagnostics.deinit();

    var crate = hir.Crate.init(allocator);
    defer crate.deinit();

    const span = hir.emptySpan(0);
    const i64_ty = ensureType(&crate, .{ .PrimInt = .I64 });

    const const_expr_id: hir.ExprId = @intCast(crate.exprs.items.len);
    try crate.exprs.append(crate.allocator(), .{ .id = const_expr_id, .kind = .{ .ConstInt = 1 }, .ty = i64_ty, .span = span });

    const local_ref_expr_id: hir.ExprId = @intCast(crate.exprs.items.len);
    try crate.exprs.append(crate.allocator(), .{ .id = local_ref_expr_id, .kind = .{ .LocalRef = 0 }, .ty = i64_ty, .span = span });

    const stmt_id: hir.StmtId = @intCast(crate.stmts.items.len);
    try crate.stmts.append(crate.allocator(), .{ .id = stmt_id, .kind = .{ .Let = .{ .pat = .{ .id = 0, .kind = .{ .Identifier = "x" }, .span = span }, .ty = i64_ty, .value = const_expr_id } }, .span = span });

    const stmt_slice = try crate.allocator().alloc(hir.StmtId, 1);
    stmt_slice[0] = stmt_id;

    const block_expr_id: hir.ExprId = @intCast(crate.exprs.items.len);
    try crate.exprs.append(crate.allocator(), .{ .id = block_expr_id, .kind = .{ .Block = .{ .stmts = stmt_slice, .tail = local_ref_expr_id } }, .ty = i64_ty, .span = span });

    try crate.patterns.append(crate.allocator(), .{ .id = 0, .kind = .{ .Identifier = "x" }, .span = span });

    try crate.items.append(crate.allocator(), .{ .id = 0, .kind = .{ .Function = .{ .def_id = 0, .name = "main", .type_params = &[_][]const u8{}, .params = &[_]hir.LocalId{}, .param_types = &[_]hir.TypeId{}, .return_type = i64_ty, .body = block_expr_id, .span = span } }, .span = span });

    var mir_crate = try lowerFromHir(allocator, &crate, &diagnostics);
    defer mir_crate.deinit();

    try std.testing.expect(!diagnostics.hasErrors());
    try std.testing.expectEqual(@as(usize, 1), mir_crate.fns.items.len);
    const mir_fn = mir_crate.fns.items[0];
    try std.testing.expectEqual(@as(usize, 1), mir_fn.blocks.len);
    try std.testing.expectEqual(@as(usize, 1), mir_fn.locals.len);
    try std.testing.expectEqual(mir.MirType.I64, mir_fn.locals[0]);
}

test "lower char and string literals" {
    const allocator = std.testing.allocator;
    var diagnostics = diag.Diagnostics.init(allocator);
    defer diagnostics.deinit();

    var crate = hir.Crate.init(allocator);
    defer crate.deinit();

    const span = hir.emptySpan(0);
    const char_ty = ensureType(&crate, .Char);
    const string_ty = ensureType(&crate, .String);

    const char_expr_id: hir.ExprId = @intCast(crate.exprs.items.len);
    try crate.exprs.append(crate.allocator(), .{ .id = char_expr_id, .kind = .{ .ConstChar = 'a' }, .ty = char_ty, .span = span });

    const string_expr_id: hir.ExprId = @intCast(crate.exprs.items.len);
    try crate.exprs.append(crate.allocator(), .{ .id = string_expr_id, .kind = .{ .ConstString = "hi" }, .ty = string_ty, .span = span });

    const stmt_id: hir.StmtId = @intCast(crate.stmts.items.len);
    try crate.stmts.append(crate.allocator(), .{ .id = stmt_id, .kind = .{ .Let = .{ .pat = .{ .id = 0, .kind = .{ .Identifier = "c" }, .span = span }, .ty = char_ty, .value = char_expr_id } }, .span = span });

    const stmt_slice = try crate.allocator().alloc(hir.StmtId, 1);
    stmt_slice[0] = stmt_id;

    const block_expr_id: hir.ExprId = @intCast(crate.exprs.items.len);
    try crate.exprs.append(crate.allocator(), .{ .id = block_expr_id, .kind = .{ .Block = .{ .stmts = stmt_slice, .tail = string_expr_id } }, .ty = string_ty, .span = span });

    try crate.patterns.append(crate.allocator(), .{ .id = 0, .kind = .{ .Identifier = "c" }, .span = span });

    try crate.items.append(crate.allocator(), .{ .id = 0, .kind = .{ .Function = .{ .def_id = 0, .name = "main", .type_params = &[_][]const u8{}, .params = &[_]hir.LocalId{}, .param_types = &[_]hir.TypeId{}, .return_type = string_ty, .body = block_expr_id, .span = span } }, .span = span });

    var mir_crate = try lowerFromHir(allocator, &crate, &diagnostics);
    defer mir_crate.deinit();

    try std.testing.expect(!diagnostics.hasErrors());
    const mir_fn = mir_crate.fns.items[0];
    try std.testing.expectEqual(@as(usize, 1), mir_fn.blocks.len);
    try std.testing.expectEqual(@as(usize, 1), mir_fn.locals.len);
    try std.testing.expectEqual(mir.MirType.Char, mir_fn.locals[0]);
    try std.testing.expect(mir_fn.blocks[0].insts[0].kind == .StoreLocal);
    try std.testing.expect(mir_fn.blocks[0].insts[0].kind.StoreLocal.src == .ImmChar);
    try std.testing.expect(mir_fn.blocks[0].term.Ret.?.ImmString.len == 2);
}

fn buildBlockWithTail(crate: *hir.Crate, stmts: []const hir.StmtId, tail: hir.ExprId, ty: hir.TypeId, span: hir.Span) !hir.ExprId {
    const block_expr_id: hir.ExprId = @intCast(crate.exprs.items.len);
    const stmts_copy = try crate.allocator().alloc(hir.StmtId, stmts.len);
    std.mem.copyForwards(hir.StmtId, stmts_copy, stmts);
    try crate.exprs.append(crate.allocator(), .{ .id = block_expr_id, .kind = .{ .Block = .{ .stmts = stmts_copy, .tail = tail } }, .ty = ty, .span = span });
    return block_expr_id;
}

test "lower if expression creates branch blocks" {
    const allocator = std.testing.allocator;
    var diagnostics = diag.Diagnostics.init(allocator);
    defer diagnostics.deinit();

    var crate = hir.Crate.init(allocator);
    defer crate.deinit();

    const span = hir.emptySpan(0);
    const bool_ty = ensureType(&crate, .Bool);
    const i32_ty = ensureType(&crate, .{ .PrimInt = .I32 });

    const cond_id: hir.ExprId = @intCast(crate.exprs.items.len);
    try crate.exprs.append(crate.allocator(), .{ .id = cond_id, .kind = .{ .ConstBool = true }, .ty = bool_ty, .span = span });

    const then_tail: hir.ExprId = @intCast(crate.exprs.items.len);
    try crate.exprs.append(crate.allocator(), .{ .id = then_tail, .kind = .{ .ConstInt = 1 }, .ty = i32_ty, .span = span });
    const then_block = try buildBlockWithTail(&crate, &.{}, then_tail, i32_ty, span);

    const else_tail: hir.ExprId = @intCast(crate.exprs.items.len);
    try crate.exprs.append(crate.allocator(), .{ .id = else_tail, .kind = .{ .ConstInt = 2 }, .ty = i32_ty, .span = span });
    const else_block = try buildBlockWithTail(&crate, &.{}, else_tail, i32_ty, span);

    const if_id: hir.ExprId = @intCast(crate.exprs.items.len);
    try crate.exprs.append(crate.allocator(), .{ .id = if_id, .kind = .{ .If = .{ .cond = cond_id, .then_branch = then_block, .else_branch = else_block } }, .ty = i32_ty, .span = span });

    try crate.items.append(crate.allocator(), .{ .id = 0, .kind = .{ .Function = .{ .def_id = 0, .name = "main", .type_params = &[_][]const u8{}, .params = &[_]hir.LocalId{}, .param_types = &[_]hir.TypeId{}, .return_type = i32_ty, .body = if_id, .span = span } }, .span = span });

    var mir_crate = try lowerFromHir(allocator, &crate, &diagnostics);
    defer mir_crate.deinit();

    try std.testing.expect(!diagnostics.hasErrors());
    const mir_fn = mir_crate.fns.items[0];
    try std.testing.expectEqual(@as(usize, 4), mir_fn.blocks.len);
    try std.testing.expect(mir_fn.blocks[0].term == .If);
}

test "lower range and aggregate expressions" {
    const allocator = std.testing.allocator;
    var diagnostics = diag.Diagnostics.init(allocator);
    defer diagnostics.deinit();

    var crate = hir.Crate.init(allocator);
    defer crate.deinit();

    const span = hir.emptySpan(0);
    const i32_ty = ensureType(&crate, .{ .PrimInt = .I32 });
    const range_ty = ensureType(&crate, .{ .Struct = .{ .def_id = 2, .type_args = &[_]hir.TypeId{} } });
    const array_ty = ensureType(&crate, .{ .Array = .{ .elem = i32_ty, .size_const = 2 } });
    const struct_ty = ensureType(&crate, .{ .Struct = .{ .def_id = 3, .type_args = &[_]hir.TypeId{} } });
    const fn_ty = ensureType(&crate, .{ .Fn = .{ .params = &[_]hir.TypeId{}, .ret = i32_ty } });

    const helper_const_id: hir.ExprId = @intCast(crate.exprs.items.len);
    try crate.exprs.append(crate.allocator(), .{ .id = helper_const_id, .kind = .{ .ConstInt = 7 }, .ty = i32_ty, .span = span });
    try crate.items.append(crate.allocator(), .{ .id = 0, .kind = .{ .Function = .{ .def_id = 1, .name = "helper", .type_params = &[_][]const u8{}, .params = &[_]hir.LocalId{}, .param_types = &[_]hir.TypeId{}, .return_type = i32_ty, .body = helper_const_id, .span = span } }, .span = span });

    const one_id: hir.ExprId = @intCast(crate.exprs.items.len);
    try crate.exprs.append(crate.allocator(), .{ .id = one_id, .kind = .{ .ConstInt = 1 }, .ty = i32_ty, .span = span });

    const two_id: hir.ExprId = @intCast(crate.exprs.items.len);
    try crate.exprs.append(crate.allocator(), .{ .id = two_id, .kind = .{ .ConstInt = 2 }, .ty = i32_ty, .span = span });

    const zero_id: hir.ExprId = @intCast(crate.exprs.items.len);
    try crate.exprs.append(crate.allocator(), .{ .id = zero_id, .kind = .{ .ConstInt = 0 }, .ty = i32_ty, .span = span });

    const range_expr_id: hir.ExprId = @intCast(crate.exprs.items.len);
    try crate.exprs.append(crate.allocator(), .{ .id = range_expr_id, .kind = .{ .Range = .{ .inclusive = true, .start = one_id, .end = two_id } }, .ty = range_ty, .span = span });

    const array_expr_id: hir.ExprId = @intCast(crate.exprs.items.len);
    const array_elems = try crate.allocator().alloc(hir.ExprId, 2);
    array_elems[0] = one_id;
    array_elems[1] = two_id;
    try crate.exprs.append(crate.allocator(), .{ .id = array_expr_id, .kind = .{ .Array = array_elems }, .ty = array_ty, .span = span });

    const index_expr_id: hir.ExprId = @intCast(crate.exprs.items.len);
    try crate.exprs.append(crate.allocator(), .{ .id = index_expr_id, .kind = .{ .Index = .{ .target = array_expr_id, .index = zero_id } }, .ty = i32_ty, .span = span });

    const struct_fields = try crate.allocator().alloc(hir.StructInitField, 1);
    struct_fields[0] = .{ .name = "x", .value = one_id };
    const struct_path = try crate.allocator().alloc([]const u8, 1);
    struct_path[0] = "Point";
    const struct_expr_id: hir.ExprId = @intCast(crate.exprs.items.len);
    try crate.exprs.append(crate.allocator(), .{ .id = struct_expr_id, .kind = .{ .StructInit = .{ .path = struct_path, .fields = struct_fields } }, .ty = struct_ty, .span = span });

    const field_expr_id: hir.ExprId = @intCast(crate.exprs.items.len);
    try crate.exprs.append(crate.allocator(), .{ .id = field_expr_id, .kind = .{ .Field = .{ .target = struct_expr_id, .name = "x" } }, .ty = i32_ty, .span = span });

    const global_ref_id: hir.ExprId = @intCast(crate.exprs.items.len);
    try crate.exprs.append(crate.allocator(), .{ .id = global_ref_id, .kind = .{ .GlobalRef = 1 }, .ty = fn_ty, .span = span });

    const call_expr_id: hir.ExprId = @intCast(crate.exprs.items.len);
    try crate.exprs.append(crate.allocator(), .{ .id = call_expr_id, .kind = .{ .Call = .{ .callee = global_ref_id, .args = &[_]hir.ExprId{} } }, .ty = i32_ty, .span = span });

    const range_stmt_id: hir.StmtId = @intCast(crate.stmts.items.len);
    try crate.stmts.append(crate.allocator(), .{ .id = range_stmt_id, .kind = .{ .Let = .{ .pat = .{ .id = 0, .kind = .{ .Identifier = "range" }, .span = span }, .ty = range_ty, .value = range_expr_id } }, .span = span });

    const array_stmt_id: hir.StmtId = @intCast(crate.stmts.items.len);
    try crate.stmts.append(crate.allocator(), .{ .id = array_stmt_id, .kind = .{ .Let = .{ .pat = .{ .id = 1, .kind = .{ .Identifier = "arr" }, .span = span }, .ty = array_ty, .value = array_expr_id } }, .span = span });

    const index_stmt_id: hir.StmtId = @intCast(crate.stmts.items.len);
    try crate.stmts.append(crate.allocator(), .{ .id = index_stmt_id, .kind = .{ .Let = .{ .pat = .{ .id = 2, .kind = .{ .Identifier = "first" }, .span = span }, .ty = i32_ty, .value = index_expr_id } }, .span = span });

    const struct_stmt_id: hir.StmtId = @intCast(crate.stmts.items.len);
    try crate.stmts.append(crate.allocator(), .{ .id = struct_stmt_id, .kind = .{ .Let = .{ .pat = .{ .id = 3, .kind = .{ .Identifier = "point" }, .span = span }, .ty = struct_ty, .value = struct_expr_id } }, .span = span });

    const field_stmt_id: hir.StmtId = @intCast(crate.stmts.items.len);
    try crate.stmts.append(crate.allocator(), .{ .id = field_stmt_id, .kind = .{ .Let = .{ .pat = .{ .id = 4, .kind = .{ .Identifier = "field" }, .span = span }, .ty = i32_ty, .value = field_expr_id } }, .span = span });

    try crate.patterns.append(crate.allocator(), .{ .id = 0, .kind = .{ .Identifier = "range" }, .span = span });
    try crate.patterns.append(crate.allocator(), .{ .id = 1, .kind = .{ .Identifier = "arr" }, .span = span });
    try crate.patterns.append(crate.allocator(), .{ .id = 2, .kind = .{ .Identifier = "first" }, .span = span });
    try crate.patterns.append(crate.allocator(), .{ .id = 3, .kind = .{ .Identifier = "point" }, .span = span });
    try crate.patterns.append(crate.allocator(), .{ .id = 4, .kind = .{ .Identifier = "field" }, .span = span });

    const stmts_slice = try crate.allocator().alloc(hir.StmtId, 5);
    stmts_slice[0] = range_stmt_id;
    stmts_slice[1] = array_stmt_id;
    stmts_slice[2] = index_stmt_id;
    stmts_slice[3] = struct_stmt_id;
    stmts_slice[4] = field_stmt_id;

    const block_expr_id: hir.ExprId = @intCast(crate.exprs.items.len);

    try crate.exprs.append(crate.allocator(), .{ .id = block_expr_id, .kind = .{ .Block = .{ .stmts = stmts_slice, .tail = call_expr_id } }, .ty = i32_ty, .span = span });
    try crate.items.append(crate.allocator(), .{ .id = 1, .kind = .{ .Function = .{ .def_id = 0, .name = "main", .type_params = &[_][]const u8{}, .params = &[_]hir.LocalId{}, .param_types = &[_]hir.TypeId{}, .return_type = i32_ty, .body = block_expr_id, .span = span } }, .span = span });

    var mir_crate = try lowerFromHir(allocator, &crate, &diagnostics);
    defer mir_crate.deinit();

    try std.testing.expect(!diagnostics.hasErrors());
    try std.testing.expectEqual(@as(usize, 2), mir_crate.fns.items.len);
    const mir_fn = mir_crate.fns.items[1];
    try std.testing.expectEqual(@as(usize, 1), mir_fn.blocks.len);

    var saw_range = false;
    var saw_array = false;
    var saw_index = false;
    var saw_struct = false;
    var saw_field = false;
    var saw_call = false;
    for (mir_fn.blocks[0].insts) |inst| {
        switch (inst.kind) {
            .Range => saw_range = true,
            .Array => saw_array = true,
            .Index => saw_index = true,
            .StructInit => saw_struct = true,
            .Field => saw_field = true,
            .Call => saw_call = true,
            else => {},
        }
    }

    try std.testing.expect(saw_range);
    try std.testing.expect(saw_array);
    try std.testing.expect(saw_index);
    try std.testing.expect(saw_struct);
    try std.testing.expect(saw_field);
    try std.testing.expect(saw_call);
}

test "lower println macro into printf call" {
    const allocator = std.testing.allocator;
    var diagnostics = diag.Diagnostics.init(allocator);
    defer diagnostics.deinit();

    var crate = hir.Crate.init(allocator);
    defer crate.deinit();

    const span = hir.emptySpan(0);
    const string_ty = ensureType(&crate, .String);
    const i32_ty = ensureType(&crate, .{ .PrimInt = .I32 });

    const fmt_expr_id: hir.ExprId = @intCast(crate.exprs.items.len);
    try crate.exprs.append(crate.allocator(), .{ .id = fmt_expr_id, .kind = .{ .ConstString = "value: {}" }, .ty = string_ty, .span = span });

    const arg_expr_id: hir.ExprId = @intCast(crate.exprs.items.len);
    try crate.exprs.append(crate.allocator(), .{ .id = arg_expr_id, .kind = .{ .ConstInt = 7 }, .ty = i32_ty, .span = span });

    const callee_expr_id: hir.ExprId = @intCast(crate.exprs.items.len);
    try crate.exprs.append(crate.allocator(), .{ .id = callee_expr_id, .kind = .{ .GlobalRef = 0 }, .ty = string_ty, .span = span });

    const args_slice = try crate.allocator().alloc(hir.ExprId, 2);
    args_slice[0] = fmt_expr_id;
    args_slice[1] = arg_expr_id;

    const call_expr_id: hir.ExprId = @intCast(crate.exprs.items.len);
    try crate.exprs.append(crate.allocator(), .{ .id = call_expr_id, .kind = .{ .Call = .{ .callee = callee_expr_id, .args = args_slice } }, .ty = i32_ty, .span = span });

    const println_fn: hir.Function = .{ .def_id = 0, .name = "println", .type_params = &[_][]const u8{}, .params = &[_]hir.LocalId{}, .param_types = &[_]hir.TypeId{}, .return_type = null, .body = null, .span = span };
    try crate.items.append(crate.allocator(), .{ .id = 0, .kind = .{ .Function = println_fn }, .span = span });

    try crate.builtin_macros.put(crate.allocator(), 0, .println);

    const main_fn: hir.Function = .{ .def_id = 1, .name = "main", .type_params = &[_][]const u8{}, .params = &[_]hir.LocalId{}, .param_types = &[_]hir.TypeId{}, .return_type = i32_ty, .body = call_expr_id, .span = span };
    try crate.items.append(crate.allocator(), .{ .id = 1, .kind = .{ .Function = main_fn }, .span = span });

    var mir_crate = try lowerFromHir(allocator, &crate, &diagnostics);
    defer mir_crate.deinit();

    try std.testing.expect(!diagnostics.hasErrors());
    try std.testing.expectEqual(@as(usize, 1), mir_crate.fns.items.len);
    const mir_fn = mir_crate.fns.items[0];
    try std.testing.expectEqual(@as(usize, 1), mir_fn.blocks.len);
    try std.testing.expect(mir_fn.blocks[0].insts.len >= 1);
    try std.testing.expect(mir_fn.blocks[0].insts[0].kind == .Call);
    try std.testing.expect(mir_fn.blocks[0].insts[0].kind.Call.target == .Symbol);
    try std.testing.expectEqualStrings("printf", mir_fn.blocks[0].insts[0].kind.Call.target.Symbol);
    try std.testing.expect(mir_fn.blocks[0].insts[0].kind.Call.args.len >= 1);
    try std.testing.expect(mir_fn.blocks[0].insts[0].kind.Call.args[0] == .ImmString);
}

test "monomorphize struct path parameters" {
    const allocator = std.testing.allocator;
    var diagnostics = diag.Diagnostics.init(allocator);
    defer diagnostics.deinit();

    var crate = hir.Crate.init(allocator);
    defer crate.deinit();

    const span = hir.emptySpan(0);
    const i64_ty = ensureType(&crate, .{ .PrimInt = .I64 });

    // Define a generic struct Pair<T> { a: T, b: T }
    const generic_param_buf = try crate.allocator().dupe(u8, "T");
    const generic_param: []const u8 = generic_param_buf;
    const param_segment = try crate.allocator().alloc([]const u8, 1);
    param_segment[0] = generic_param;
    const type_param_ty: hir.TypeId = @intCast(crate.types.items.len);
    try crate.types.append(crate.allocator(), .{ .id = type_param_ty, .kind = .{ .Path = .{ .segments = param_segment, .args = &[_]hir.TypeId{} } } });

    const fields = try crate.allocator().alloc(hir.Field, 2);
    fields[0] = .{ .name = "a", .ty = type_param_ty, .span = span };
    fields[1] = .{ .name = "b", .ty = type_param_ty, .span = span };

    try crate.items.append(crate.allocator(), .{ .id = 0, .kind = .{ .Struct = .{ .def_id = 0, .name = "Pair", .type_params = param_segment, .fields = fields, .span = span } }, .span = span });

    // Function takes Pair<i64> as its single parameter
    const pair_segments = try crate.allocator().alloc([]const u8, 1);
    pair_segments[0] = "Pair";
    const pair_args = try crate.allocator().alloc(hir.TypeId, 1);
    pair_args[0] = i64_ty;
    const pair_path_ty: hir.TypeId = @intCast(crate.types.items.len);
    try crate.types.append(crate.allocator(), .{ .id = pair_path_ty, .kind = .{ .Path = .{ .segments = pair_segments, .args = pair_args } } });

    const fn_params = try crate.allocator().alloc(hir.LocalId, 1);
    fn_params[0] = 0;
    const fn_param_types = try crate.allocator().alloc(hir.TypeId, 1);
    fn_param_types[0] = pair_path_ty;
    const take_fn: hir.Function = .{ .def_id = 1, .name = "take", .type_params = &[_][]const u8{}, .params = fn_params, .param_types = fn_param_types, .return_type = null, .body = null, .span = span };
    try crate.items.append(crate.allocator(), .{ .id = 1, .kind = .{ .Function = take_fn }, .span = span });

    var mir_crate = try lowerFromHir(allocator, &crate, &diagnostics);
    defer mir_crate.deinit();

    try std.testing.expect(!diagnostics.hasErrors());
    try std.testing.expectEqual(@as(usize, 1), mir_crate.fns.items.len);
    const mir_fn = mir_crate.fns.items[0];
    // Two locals for the struct fields
    try std.testing.expectEqual(@as(usize, 2), mir_fn.locals.len);

    // Two store-field instructions initialized from the flattened parameters
    var store_count: usize = 0;
    for (mir_fn.blocks[0].insts) |inst| {
        if (inst.kind == .StoreField) store_count += 1;
    }
    try std.testing.expectEqual(@as(usize, 2), store_count);
}

test "lower print macro preserves format string without newline" {
    const allocator = std.testing.allocator;
    var diagnostics = diag.Diagnostics.init(allocator);
    defer diagnostics.deinit();

    var crate = hir.Crate.init(allocator);
    defer crate.deinit();

    const span = hir.emptySpan(0);
    const string_ty = ensureType(&crate, .String);
    const i32_ty = ensureType(&crate, .{ .PrimInt = .I32 });

    const fmt_expr_id: hir.ExprId = @intCast(crate.exprs.items.len);
    try crate.exprs.append(crate.allocator(), .{ .id = fmt_expr_id, .kind = .{ .ConstString = "value: {}" }, .ty = string_ty, .span = span });

    const arg_expr_id: hir.ExprId = @intCast(crate.exprs.items.len);
    try crate.exprs.append(crate.allocator(), .{ .id = arg_expr_id, .kind = .{ .ConstInt = 7 }, .ty = i32_ty, .span = span });

    const callee_expr_id: hir.ExprId = @intCast(crate.exprs.items.len);
    try crate.exprs.append(crate.allocator(), .{ .id = callee_expr_id, .kind = .{ .GlobalRef = 0 }, .ty = string_ty, .span = span });

    const args_slice = try crate.allocator().alloc(hir.ExprId, 2);
    args_slice[0] = fmt_expr_id;
    args_slice[1] = arg_expr_id;

    const call_expr_id: hir.ExprId = @intCast(crate.exprs.items.len);
    try crate.exprs.append(crate.allocator(), .{ .id = call_expr_id, .kind = .{ .Call = .{ .callee = callee_expr_id, .args = args_slice } }, .ty = i32_ty, .span = span });

    const print_fn: hir.Function = .{ .def_id = 0, .name = "print", .type_params = &[_][]const u8{}, .params = &[_]hir.LocalId{}, .param_types = &[_]hir.TypeId{}, .return_type = null, .body = null, .span = span };
    try crate.items.append(crate.allocator(), .{ .id = 0, .kind = .{ .Function = print_fn }, .span = span });

    try crate.builtin_macros.put(crate.allocator(), 0, .print);

    const main_fn: hir.Function = .{ .def_id = 1, .name = "main", .type_params = &[_][]const u8{}, .params = &[_]hir.LocalId{}, .param_types = &[_]hir.TypeId{}, .return_type = i32_ty, .body = call_expr_id, .span = span };
    try crate.items.append(crate.allocator(), .{ .id = 1, .kind = .{ .Function = main_fn }, .span = span });

    var mir_crate = try lowerFromHir(allocator, &crate, &diagnostics);
    defer mir_crate.deinit();

    try std.testing.expect(!diagnostics.hasErrors());
    const mir_fn = mir_crate.fns.items[0];
    try std.testing.expect(mir_fn.blocks[0].insts.len >= 1);
    const call_inst = mir_fn.blocks[0].insts[0];
    try std.testing.expect(call_inst.kind == .Call);
    const first_arg = call_inst.kind.Call.args[0];
    try std.testing.expect(first_arg == .ImmString);
    try std.testing.expectEqualStrings("value: %d", first_arg.ImmString);
}

test "struct layouts are computed during HIR to MIR lowering" {
    const allocator = std.testing.allocator;
    var diagnostics = diag.Diagnostics.init(allocator);
    defer diagnostics.deinit();

    var crate = hir.Crate.init(allocator);
    defer crate.deinit();

    const span = hir.emptySpan(0);
    const i64_ty = ensureType(&crate, .{ .PrimInt = .I64 });

    // Define a struct with three fields
    const fields = try crate.allocator().alloc(hir.Field, 3);
    fields[0] = .{ .name = "alpha", .ty = i64_ty, .span = span };
    fields[1] = .{ .name = "beta", .ty = i64_ty, .span = span };
    fields[2] = .{ .name = "gamma", .ty = i64_ty, .span = span };

    try crate.items.append(crate.allocator(), .{
        .id = 0,
        .kind = .{ .Struct = .{
            .def_id = 0,
            .name = "TestStruct",
            .type_params = &[_][]const u8{},
            .fields = fields,
            .span = span,
        } },
        .span = span,
    });

    // Define a simple function to trigger lowering
    const const_expr_id: hir.ExprId = @intCast(crate.exprs.items.len);
    try crate.exprs.append(crate.allocator(), .{ .id = const_expr_id, .kind = .{ .ConstInt = 42 }, .ty = i64_ty, .span = span });

    try crate.items.append(crate.allocator(), .{
        .id = 1,
        .kind = .{ .Function = .{
            .def_id = 1,
            .name = "main",
            .type_params = &[_][]const u8{},
            .params = &[_]hir.LocalId{},
            .param_types = &[_]hir.TypeId{},
            .return_type = i64_ty,
            .body = const_expr_id,
            .span = span,
        } },
        .span = span,
    });

    var mir_crate = try lowerFromHir(allocator, &crate, &diagnostics);
    defer mir_crate.deinit();

    try std.testing.expect(!diagnostics.hasErrors());

    // Verify struct layout was computed correctly
    const layout = mir_crate.getStructLayout("TestStruct");
    try std.testing.expect(layout != null);
    try std.testing.expectEqual(@as(usize, 3), layout.?.fields.len);

    // Verify field offsets are in declaration order
    // First field at offset 0, second at -32, third at -64
    const expected_size: u32 = @sizeOf(i64) * shared.LOCAL_STACK_MULTIPLIER;
    try std.testing.expectEqualStrings("alpha", layout.?.fields[0].name);
    try std.testing.expectEqual(@as(i32, 0), layout.?.fields[0].offset);
    try std.testing.expectEqual(@as(u32, 0), layout.?.fields[0].index);

    try std.testing.expectEqualStrings("beta", layout.?.fields[1].name);
    try std.testing.expectEqual(-@as(i32, @intCast(expected_size)), layout.?.fields[1].offset);
    try std.testing.expectEqual(@as(u32, 1), layout.?.fields[1].index);

    try std.testing.expectEqualStrings("gamma", layout.?.fields[2].name);
    try std.testing.expectEqual(-2 * @as(i32, @intCast(expected_size)), layout.?.fields[2].offset);
    try std.testing.expectEqual(@as(u32, 2), layout.?.fields[2].index);
}

test "field indices from declaration order are consistent" {
    const allocator = std.testing.allocator;

    // Create mock HIR fields
    var fields = [_]hir.Field{
        .{ .name = "a", .ty = 0, .span = hir.emptySpan(0) },
        .{ .name = "b", .ty = 0, .span = hir.emptySpan(0) },
        .{ .name = "c", .ty = 0, .span = hir.emptySpan(0) },
        .{ .name = "d", .ty = 0, .span = hir.emptySpan(0) },
    };
    _ = allocator;

    // Verify field indices are returned in declaration order
    try std.testing.expectEqual(@as(hir.LocalId, 0), getFieldIndexFromDeclaration(&fields, "a"));
    try std.testing.expectEqual(@as(hir.LocalId, 1), getFieldIndexFromDeclaration(&fields, "b"));
    try std.testing.expectEqual(@as(hir.LocalId, 2), getFieldIndexFromDeclaration(&fields, "c"));
    try std.testing.expectEqual(@as(hir.LocalId, 3), getFieldIndexFromDeclaration(&fields, "d"));

    // Verify unknown field returns 0 as fallback
    try std.testing.expectEqual(@as(hir.LocalId, 0), getFieldIndexFromDeclaration(&fields, "unknown"));
}

fn ensureType(crate: *hir.Crate, kind: hir.Type.Kind) hir.TypeId {
    for (crate.types.items) |existing| {
        if (std.meta.eql(existing.kind, kind)) return existing.id;
    }

    const id: hir.TypeId = @intCast(crate.types.items.len);
    crate.types.append(crate.allocator(), .{ .id = id, .kind = kind }) catch unreachable;
    return id;
}
