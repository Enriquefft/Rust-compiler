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
    hir_crate: *const hir.Crate,
    diagnostics: *diag.Diagnostics,
    mir_fn: mir.MirFn,
    locals: std.ArrayListUnmanaged(mir.MirType) = .{},
    blocks: std.ArrayListUnmanaged(BlockState) = .{},
    current_block: mir.BlockId = 0,

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
            .LocalRef => |local| {
                try self.ensureLocal(local, expr.ty, expr.span);
                return .{ .Local = local };
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
            .GlobalRef => {
                self.diagnostics.reportError(expr.span, "global references not yet supported in MIR lowering");
                return null;
            },
            .ConstChar, .ConstString => {
                self.diagnostics.reportError(expr.span, "literal type not yet supported in MIR lowering");
                return null;
            },
            .UnresolvedIdent, .Unknown => {
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
            .Array, .Pointer, .Ref, .Fn, .Struct, .Path, .Unknown => blk: {
                diagnostics.reportError(span, "type not supported in MIR lowering");
                break :blk null;
            },
        };
    }
    diagnostics.reportWarning(span, "missing type information during MIR lowering");
    return null;
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

fn ensureType(crate: *hir.Crate, kind: hir.Type.Kind) hir.TypeId {
    for (crate.types.items) |existing| {
        if (std.meta.eql(existing.kind, kind)) return existing.id;
    }

    const id: hir.TypeId = @intCast(crate.types.items.len);
    crate.types.append(crate.allocator(), .{ .id = id, .kind = kind }) catch unreachable;
    return id;
}
