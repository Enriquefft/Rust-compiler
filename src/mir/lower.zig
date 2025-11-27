const std = @import("std");
const diag = @import("../diag/diagnostics.zig");
const hir = @import("../hir/hir.zig");
const mir = @import("mir.zig");

const LowerError = error{OutOfMemory};

pub fn lowerFromHir(allocator: std.mem.Allocator, hir_crate: *const hir.Crate, diagnostics: *diag.Diagnostics) LowerError!mir.MirCrate {
    var crate = mir.MirCrate.init(allocator);

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
    };
    _ = try builder.beginBlock();

    for (func.params) |param| {
        try builder.ensureLocal(param, null, func.span);
    }

    for (func.params, 0..) |param, idx| {
        const ty = if (idx < func.param_types.len) func.param_types[idx] else null;
        _ = try builder.emitInst(.{ .ty = mapType(hir_crate, ty, func.span, diagnostics), .dest = null, .kind = .{ .StoreLocal = .{ .local = param, .src = .{ .Param = @intCast(idx) } } } });
    }

    if (func.body) |body_id| {
        const result = try builder.lowerExpr(body_id);
        builder.setTerm(.{ .Ret = result });
    } else {
        builder.setTerm(.{ .Ret = null });
    }

    return try builder.finish();
}

const FunctionBuilder = struct {
    allocator: std.mem.Allocator,
    crate: *mir.MirCrate,
    hir_crate: *const hir.Crate,
    diagnostics: *diag.Diagnostics,
    mir_fn: mir.MirFn,
    locals: std.ArrayListUnmanaged(mir.MirType) = .{},
    blocks: std.ArrayListUnmanaged(BlockState) = .{},
    current_block: mir.BlockId = 0,
    next_temp: mir.TempId = 0,

    const BlockState = struct {
        insts: std.ArrayListUnmanaged(mir.Inst) = .{},
        term: ?mir.TermKind = null,
    };

    fn beginBlock(self: *FunctionBuilder) LowerError!mir.BlockId {
        const id: mir.BlockId = @intCast(self.blocks.items.len);
        try self.blocks.append(self.allocator, .{});
        self.current_block = id;
        return id;
    }

    fn newBlock(self: *FunctionBuilder) LowerError!mir.BlockId {
        const id: mir.BlockId = @intCast(self.blocks.items.len);
        try self.blocks.append(self.allocator, .{});
        return id;
    }

    fn switchTo(self: *FunctionBuilder, id: mir.BlockId) void {
        self.current_block = id;
    }

    fn setTerm(self: *FunctionBuilder, term: mir.TermKind) void {
        self.blocks.items[self.current_block].term = term;
    }

    fn emitInst(self: *FunctionBuilder, inst: mir.Inst) LowerError!?mir.Operand {
        const dest = inst.dest;
        try self.blocks.items[self.current_block].insts.append(self.allocator, inst);
        if (dest) |tmp| {
            return .{ .Temp = tmp };
        }
        return null;
    }

    fn newTemp(self: *FunctionBuilder) mir.TempId {
        const tmp = self.next_temp;
        self.next_temp += 1;
        return tmp;
    }

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
                return .{ .ImmString = v };
            },
            .LocalRef => |local| {
                try self.ensureLocal(local, expr.ty, expr.span);
                return .{ .Local = local };
            },
            .GlobalRef => |def_id| {
                if (def_id >= self.hir_crate.items.items.len) {
                    self.diagnostics.reportError(expr.span, "reference to unknown item during MIR lowering");
                    return null;
                }
                return .{ .Global = def_id };
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
                if (self.isBuiltinPrintln(call.callee)) {
                    return try self.lowerPrintlnMacro(call.args, expr.span);
                }
                
                // Check if this is a pointer method call like ptr.is_null()
                if (self.isPointerMethodCall(call.callee)) |ptr_method| {
                    return try self.lowerPointerMethodCall(ptr_method.target_id, ptr_method.method_name, call.args, expr);
                }
                
                const callee_op = try self.lowerExpr(call.callee) orelse {
                    self.diagnostics.reportError(expr.span, "missing callee while lowering call");
                    return null;
                };
                var args = std.ArrayListUnmanaged(mir.Operand){};
                defer args.deinit(self.allocator);
                for (call.args) |arg_id| {
                    if (try self.lowerExpr(arg_id)) |arg_op| {
                        try args.append(self.allocator, arg_op);
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
                    const local = target_expr.kind.LocalRef;
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

                self.switchTo(then_block);
                const then_val = try self.lowerExpr(iff.then_branch);
                var result_tmp: ?mir.TempId = null;
                if (then_val) |tv| {
                    result_tmp = self.newTemp();
                    _ = try self.emitInst(.{ .ty = mapType(self.hir_crate, expr.ty, expr.span, self.diagnostics), .dest = result_tmp, .kind = .{ .Copy = .{ .src = tv } } });
                }
                self.setTerm(.{ .Goto = join_block });

                self.switchTo(else_block);
                const else_val = if (iff.else_branch) |else_id| try self.lowerExpr(else_id) else null;
                if (else_val) |ev| {
                    if (result_tmp == null) result_tmp = self.newTemp();
                    _ = try self.emitInst(.{ .ty = mapType(self.hir_crate, expr.ty, expr.span, self.diagnostics), .dest = result_tmp, .kind = .{ .Copy = .{ .src = ev } } });
                }
                self.setTerm(.{ .Goto = join_block });

                self.switchTo(join_block);
                if (result_tmp) |tmp| return .{ .Temp = tmp };
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
                if (iter_expr.kind != .Range) {
                    self.diagnostics.reportError(expr.span, "only range-based for loops are supported");
                    return null;
                }

                const start_op = try self.lowerExpr(iter_expr.kind.Range.start) orelse return null;
                const end_op = try self.lowerExpr(iter_expr.kind.Range.end) orelse return null;

                const mir_ty = mapType(self.hir_crate, iter_expr.ty, expr.span, self.diagnostics);
                try self.ensureLocal(for_expr.pat.id, iter_expr.ty, expr.span);
                _ = try self.emitInst(.{ .ty = mir_ty, .dest = null, .kind = .{ .StoreLocal = .{ .local = for_expr.pat.id, .src = start_op } } });

                const cond_block = try self.newBlock();
                const body_block = try self.newBlock();
                const exit_block = try self.newBlock();
                self.setTerm(.{ .Goto = cond_block });

                self.switchTo(cond_block);
                const cur_tmp = self.newTemp();
                _ = try self.emitInst(.{ .ty = mir_ty, .dest = cur_tmp, .kind = .{ .LoadLocal = .{ .local = for_expr.pat.id } } });
                const cmp_tmp = self.newTemp();
                const cmp_op = if (iter_expr.kind.Range.inclusive) mir.CmpOp.Le else mir.CmpOp.Lt;
                _ = try self.emitInst(.{
                    .ty = .Bool,
                    .dest = cmp_tmp,
                    .kind = .{ .Cmp = .{ .op = cmp_op, .lhs = .{ .Temp = cur_tmp }, .rhs = end_op } },
                });
                self.setTerm(.{ .If = .{ .cond = .{ .Temp = cmp_tmp }, .then_block = body_block, .else_block = exit_block } });

                self.switchTo(body_block);
                _ = try self.lowerExpr(for_expr.body);

                const incr_tmp = self.newTemp();
                _ = try self.emitInst(.{
                    .ty = mir_ty,
                    .dest = incr_tmp,
                    .kind = .{ .Bin = .{ .op = .Add, .lhs = .{ .Local = for_expr.pat.id }, .rhs = .{ .ImmInt = 1 } } },
                });
                _ = try self.emitInst(.{ .ty = mir_ty, .dest = null, .kind = .{ .StoreLocal = .{ .local = for_expr.pat.id, .src = .{ .Temp = incr_tmp } } } });
                self.setTerm(.{ .Goto = cond_block });

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

    fn lowerStmt(self: *FunctionBuilder, stmt_id: hir.StmtId) LowerError!void {
        const stmt = self.hir_crate.stmts.items[stmt_id];
        switch (stmt.kind) {
            .Let => |let_stmt| {
                try self.ensureLocal(let_stmt.pat.id, let_stmt.ty, stmt.span);
                if (let_stmt.value) |value_id| {
                    if (try self.lowerExpr(value_id)) |value_op| {
                        _ = try self.emitInst(.{ .ty = mapType(self.hir_crate, let_stmt.ty, stmt.span, self.diagnostics), .dest = null, .kind = .{ .StoreLocal = .{ .local = let_stmt.pat.id, .src = value_op } } });
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

    fn isBuiltinPrintln(self: *FunctionBuilder, callee_id: hir.ExprId) bool {
        if (callee_id >= self.hir_crate.exprs.items.len) return false;
        const callee = self.hir_crate.exprs.items[callee_id];
        if (callee.kind != .GlobalRef) return false;
        const def_id = callee.kind.GlobalRef;
        if (def_id >= self.hir_crate.items.items.len) return false;
        const item = self.hir_crate.items.items[def_id];
        return item.kind == .Function and std.mem.eql(u8, item.kind.Function.name, "println");
    }

    const PointerMethodInfo = struct {
        target_id: hir.ExprId,
        method_name: []const u8,
    };

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

    fn lowerPointerMethodCall(self: *FunctionBuilder, target_id: hir.ExprId, method_name: []const u8, args: []const hir.ExprId, expr: hir.Expr) LowerError!?mir.Operand {
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
        _ = args;
    }

    fn lowerPrintlnMacro(self: *FunctionBuilder, args_ids: []const hir.ExprId, span: hir.Span) LowerError!?mir.Operand {
        if (args_ids.len == 0) {
            self.diagnostics.reportError(span, "println! requires a format string");
            return null;
        }

        const fmt_expr = self.hir_crate.exprs.items[args_ids[0]];
        if (fmt_expr.kind != .ConstString) {
            self.diagnostics.reportError(span, "println! expects a string literal format");
            return null;
        }

        var fmt_buf = std.ArrayListUnmanaged(u8){};
        defer fmt_buf.deinit(self.allocator);

        var args = std.ArrayListUnmanaged(mir.Operand){};
        defer args.deinit(self.allocator);

        var arg_idx: usize = 1;
        var i: usize = 0;
        var raw_fmt = fmt_expr.kind.ConstString;
        if (raw_fmt.len >= 2 and raw_fmt[0] == '"' and raw_fmt[raw_fmt.len - 1] == '"') {
            raw_fmt = raw_fmt[1 .. raw_fmt.len - 1];
        }
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
                    try args.append(self.allocator, arg_op);
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
            self.diagnostics.reportError(span, "too many arguments supplied to println!");
            return null;
        }

        if (fmt_buf.items.len == 0 or fmt_buf.items[fmt_buf.items.len - 1] != '\n') {
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

    fn printfSpecifier(self: *FunctionBuilder, expr_id: hir.ExprId, span: hir.Span) ?[]const u8 {
        if (expr_id >= self.hir_crate.exprs.items.len) {
            self.diagnostics.reportError(span, "unknown argument in println!");
            return null;
        }
        const expr = self.hir_crate.exprs.items[expr_id];
        const ty = mapType(self.hir_crate, expr.ty, span, self.diagnostics) orelse {
            self.diagnostics.reportError(span, "println! argument has unsupported type");
            return null;
        };

        return switch (ty) {
            .I32, .U32 => "%d",
            .I64, .U64, .Usize => "%ld",
            .F32, .F64 => "%f",
            .Bool => "%d",
            .Char => "%c",
            .String, .Str => "%s",
            else => {
                self.diagnostics.reportError(span, "println! argument type cannot be formatted");
                return null;
            },
        };
    }

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
            .Path => blk: {
                diagnostics.reportWarning(span, "type not supported in MIR lowering");
                break :blk null;
            },
            .Unknown => blk: {
                diagnostics.reportWarning(span, "missing type information during MIR lowering");
                break :blk null;
            },
        };
    }
    diagnostics.reportWarning(span, "missing type information during MIR lowering");
    return null;
}

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

fn mapAssignBin(op: hir.AssignOp) ?mir.BinOp {
    return switch (op) {
        .Assign => null,
        .AddAssign => .Add,
        .SubAssign => .Sub,
        .MulAssign => .Mul,
        .DivAssign => .Div,
    };
}

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

fn mapUnary(op: hir.UnaryOp) ?mir.UnaryOp {
    return switch (op) {
        .Not => .Not,
        .Neg => .Neg,
        .Deref => .Deref,
        .Ref, .RefMut => .Ref,
    };
}

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

    try crate.items.append(crate.allocator(), .{ .id = 0, .kind = .{ .Function = .{ .def_id = 0, .name = "main", .params = &[_]hir.LocalId{}, .param_types = &[_]hir.TypeId{}, .return_type = i64_ty, .body = block_expr_id, .span = span } }, .span = span });

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

    try crate.items.append(crate.allocator(), .{ .id = 0, .kind = .{ .Function = .{ .def_id = 0, .name = "main", .params = &[_]hir.LocalId{}, .param_types = &[_]hir.TypeId{}, .return_type = string_ty, .body = block_expr_id, .span = span } }, .span = span });

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

    try crate.items.append(crate.allocator(), .{ .id = 0, .kind = .{ .Function = .{ .def_id = 0, .name = "main", .params = &[_]hir.LocalId{}, .param_types = &[_]hir.TypeId{}, .return_type = i32_ty, .body = if_id, .span = span } }, .span = span });

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
    try crate.items.append(crate.allocator(), .{ .id = 0, .kind = .{ .Function = .{ .def_id = 1, .name = "helper", .params = &[_]hir.LocalId{}, .param_types = &[_]hir.TypeId{}, .return_type = i32_ty, .body = helper_const_id, .span = span } }, .span = span });

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
    try crate.items.append(crate.allocator(), .{ .id = 1, .kind = .{ .Function = .{ .def_id = 0, .name = "main", .params = &[_]hir.LocalId{}, .param_types = &[_]hir.TypeId{}, .return_type = i32_ty, .body = block_expr_id, .span = span } }, .span = span });

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

    const println_fn: hir.Function = .{ .def_id = 0, .name = "println", .params = &[_]hir.LocalId{}, .param_types = &[_]hir.TypeId{}, .return_type = null, .body = null, .span = span };
    try crate.items.append(crate.allocator(), .{ .id = 0, .kind = .{ .Function = println_fn }, .span = span });

    const main_fn: hir.Function = .{ .def_id = 1, .name = "main", .params = &[_]hir.LocalId{}, .param_types = &[_]hir.TypeId{}, .return_type = i32_ty, .body = call_expr_id, .span = span };
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

fn ensureType(crate: *hir.Crate, kind: hir.Type.Kind) hir.TypeId {
    for (crate.types.items) |existing| {
        if (std.meta.eql(existing.kind, kind)) return existing.id;
    }

    const id: hir.TypeId = @intCast(crate.types.items.len);
    crate.types.append(crate.allocator(), .{ .id = id, .kind = kind }) catch unreachable;
    return id;
}
