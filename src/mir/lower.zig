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
        .mir_crate = crate,
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
    mir_crate: *mir.MirCrate,
    hir_crate: *const hir.Crate,
    diagnostics: *diag.Diagnostics,
    mir_fn: mir.MirFn,
    locals: std.ArrayListUnmanaged(mir.MirType) = .{},
    blocks: std.ArrayListUnmanaged(BlockState) = .{},
    current_block: mir.BlockId = 0,
    next_temp: mir.TempId = 0,
    printf_id: ?u32 = null,

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

    fn ensurePrintfOperand(self: *FunctionBuilder) LowerError!mir.Operand {
        if (self.printf_id) |id| return .{ .Global = id };
        if (self.mir_crate.builtin_printf) |id| {
            self.printf_id = id;
            return .{ .Global = id };
        }

        const id: u32 = @intCast(self.mir_crate.fns.items.len);
        try self.mir_crate.fns.append(self.mir_crate.allocator(), .{
            .name = "printf",
            .params = &[_]mir.LocalId{},
            .locals = &[_]mir.MirType{},
            .ret_ty = .I32,
            .blocks = &[_]mir.Block{},
            .is_extern = true,
        });
        self.mir_crate.builtin_printf = id;
        self.printf_id = id;
        return .{ .Global = id };
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
                    const tmp = self.newTemp();
                    _ = try self.emitInst(.{ .ty = mapType(self.hir_crate, expr.ty, expr.span, self.diagnostics), .dest = tmp, .kind = .{ .Unary = .{ .op = mapUnary(un.op), .operand = op } } });
                    return .{ .Temp = tmp };
                }
                return null;
            },
            .Call => |call| {
                if (isBuiltinPrintln(self.hir_crate, call.callee)) {
                    return try self.lowerPrintln(call, expr);
                }
                const callee_op = try self.lowerExpr(call.callee) orelse {
                    self.diagnostics.reportError(expr.span, "missing callee while lowering call");
                    return null;
                };
                if (callee_op != .Global) {
                    self.diagnostics.reportError(expr.span, "only direct function calls are supported in MIR lowering");
                    return null;
                }
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
                        _ = try self.emitInst(.{ .ty = mapType(self.hir_crate, target_expr.ty, expr.span, self.diagnostics), .dest = null, .kind = .{ .StoreLocal = .{ .local = local, .src = val } } });
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
            .Cast => |c| {
                self.diagnostics.reportWarning(expr.span, "casts lowered as no-op in MIR");
                return try self.lowerExpr(c.expr);
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
            .UnresolvedIdent, .Unknown => {
                self.diagnostics.reportError(expr.span, "expression could not be lowered to MIR");
                return null;
            },
        }
    }

    fn lowerPrintln(self: *FunctionBuilder, call: @FieldType(hir.Expr.Kind, "Call"), expr: hir.Expr) LowerError!?mir.Operand {
        if (call.args.len == 0) {
            self.diagnostics.reportError(expr.span, "println! requires a format string");
            return null;
        }

        const fmt_expr = self.hir_crate.exprs.items[call.args[0]];
        if (fmt_expr.kind != .ConstString) {
            self.diagnostics.reportError(fmt_expr.span, "println! format must be a string literal");
            return null;
        }

        var arg_types = std.ArrayListUnmanaged(mir.MirType){};
        defer arg_types.deinit(self.allocator);

        var lowered_args = std.ArrayListUnmanaged(mir.Operand){};
        defer lowered_args.deinit(self.allocator);

        for (call.args[1..]) |arg_id| {
            const hir_arg = self.hir_crate.exprs.items[arg_id];
            const arg_ty = mapType(self.hir_crate, hir_arg.ty, hir_arg.span, self.diagnostics) orelse return null;
            try arg_types.append(self.allocator, arg_ty);

            const lowered = try self.lowerExpr(arg_id) orelse return null;
            try lowered_args.append(self.allocator, lowered);
        }

        const fmt_literal = fmt_expr.kind.ConstString;
        const fmt_string = try self.buildPrintfFormat(unquoteString(fmt_literal), arg_types.items, fmt_expr.span) orelse return null;

        var call_args = std.ArrayListUnmanaged(mir.Operand){};
        defer call_args.deinit(self.allocator);
        try call_args.append(self.allocator, .{ .ImmString = fmt_string });
        try call_args.appendSlice(self.allocator, lowered_args.items);

        const target = try self.ensurePrintfOperand();
        const tmp = self.newTemp();
        _ = try self.emitInst(.{ .ty = mapType(self.hir_crate, expr.ty, expr.span, self.diagnostics), .dest = tmp, .kind = .{ .Call = .{ .target = target, .args = try call_args.toOwnedSlice(self.allocator) } } });
        return .{ .Temp = tmp };
    }

    fn buildPrintfFormat(self: *FunctionBuilder, literal: []const u8, arg_types: []const mir.MirType, span: hir.Span) LowerError!?[]const u8 {
        var buffer = std.ArrayListUnmanaged(u8){};
        errdefer buffer.deinit(self.allocator);

        var idx: usize = 0;
        var arg_idx: usize = 0;
        while (idx < literal.len) : (idx += 1) {
            switch (literal[idx]) {
                '{' => {
                    if (idx + 1 < literal.len and literal[idx + 1] == '{') {
                        try buffer.append(self.allocator, '{');
                        idx += 1;
                        continue;
                    }
                    const close = std.mem.indexOfScalarPos(u8, literal, idx + 1, '}') orelse {
                        self.diagnostics.reportError(span, "unclosed '{' in println! format string");
                        return null;
                    };
                    if (arg_idx >= arg_types.len) {
                        self.diagnostics.reportError(span, "println! is missing arguments for format placeholders");
                        return null;
                    }
                    const spec = mapPrintfSpecifier(arg_types[arg_idx]) orelse {
                        self.diagnostics.reportError(span, "println! argument type is not supported by printf lowering");
                        return null;
                    };
                    try buffer.appendSlice(self.allocator, spec);
                    arg_idx += 1;
                    idx = close;
                },
                '}' => {
                    if (idx + 1 < literal.len and literal[idx + 1] == '}') {
                        try buffer.append(self.allocator, '}');
                        idx += 1;
                        continue;
                    }
                    self.diagnostics.reportError(span, "unmatched '}' in println! format string");
                    return null;
                },
                else => try buffer.append(self.allocator, literal[idx]),
            }
        }

        if (arg_idx != arg_types.len) {
            self.diagnostics.reportError(span, "println! has more arguments than format placeholders");
            return null;
        }

        if (literal.len == 0 or literal[literal.len - 1] != '\n') {
            try buffer.append(self.allocator, '\n');
        }

        return try buffer.toOwnedSlice(self.allocator);
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

fn isBuiltinPrintln(hir_crate: *const hir.Crate, callee_id: hir.ExprId) bool {
    const callee = hir_crate.exprs.items[callee_id];
    return callee.kind == .UnresolvedIdent and std.mem.eql(u8, callee.kind.UnresolvedIdent, "println");
}

fn unquoteString(lit: []const u8) []const u8 {
    if (lit.len >= 2 and lit[0] == '"' and lit[lit.len - 1] == '"') {
        return lit[1 .. lit.len - 1];
    }
    return lit;
}

fn mapPrintfSpecifier(ty: mir.MirType) ?[]const u8 {
    return switch (ty) {
        .I32 => "%d",
        .I64 => "%ld",
        .U32 => "%u",
        .U64 => "%lu",
        .Usize => "%zu",
        .F32, .F64 => "%f",
        .Bool => "%d",
        .Char => "%c",
        .String, .Str => "%s",
        else => null,
    };
}

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
            .Pointer, .Ref => .Pointer,
            .Struct => .Struct,
            .Fn, .Path => blk: {
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

fn mapUnary(op: hir.UnaryOp) mir.UnaryOp {
    return switch (op) {
        .Not => .Not,
        .Neg => .Neg,
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

    try crate.items.append(crate.allocator(), .{ .id = 0, .kind = .{ .Function = .{ .def_id = 0, .name = "main", .params = &[_]hir.LocalId{}, .return_type = i64_ty, .body = block_expr_id, .span = span } }, .span = span });

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

    try crate.items.append(crate.allocator(), .{ .id = 0, .kind = .{ .Function = .{ .def_id = 0, .name = "main", .params = &[_]hir.LocalId{}, .return_type = string_ty, .body = block_expr_id, .span = span } }, .span = span });

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

    try crate.items.append(crate.allocator(), .{ .id = 0, .kind = .{ .Function = .{ .def_id = 0, .name = "main", .params = &[_]hir.LocalId{}, .return_type = i32_ty, .body = if_id, .span = span } }, .span = span });

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
    try crate.items.append(crate.allocator(), .{ .id = 0, .kind = .{ .Function = .{ .def_id = 1, .name = "helper", .params = &[_]hir.LocalId{}, .return_type = i32_ty, .body = helper_const_id, .span = span } }, .span = span });

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

    try crate.items.append(crate.allocator(), .{ .id = 1, .kind = .{ .Function = .{ .def_id = 0, .name = "main", .params = &[_]hir.LocalId{}, .return_type = i32_ty, .body = block_expr_id, .span = span } }, .span = span });

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

fn ensureType(crate: *hir.Crate, kind: hir.Type.Kind) hir.TypeId {
    for (crate.types.items) |existing| {
        if (std.meta.eql(existing.kind, kind)) return existing.id;
    }

    const id: hir.TypeId = @intCast(crate.types.items.len);
    crate.types.append(crate.allocator(), .{ .id = id, .kind = kind }) catch unreachable;
    return id;
}
