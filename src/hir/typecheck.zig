const std = @import("std");
const diag = @import("../diag/diagnostics.zig");
const hir = @import("hir.zig");

const Error = error{OutOfMemory};

pub fn typecheck(crate: *hir.Crate, diagnostics: *diag.Diagnostics) Error!void {
    for (crate.items.items) |*item| {
        switch (item.kind) {
            .Function => |*func| try typecheckFunction(crate, func, diagnostics),
            .Struct => |*structure| try typecheckStruct(crate, structure, diagnostics),
            .TypeAlias => |*alias| try ensureKnownType(crate, alias.target, item.span, diagnostics),
            else => {},
        }
    }
}

fn typecheckFunction(crate: *hir.Crate, func: *hir.Function, diagnostics: *diag.Diagnostics) Error!void {
    var locals = std.AutoHashMap(hir.LocalId, hir.TypeId).init(crate.allocator());
    defer locals.deinit();

    if (func.body) |body_id| {
        const body_ty = try checkExpr(crate, body_id, diagnostics, &locals);
        if (func.return_type) |ret_ty| {
            if (!typesEqual(crate, ret_ty, body_ty)) {
                diagnostics.reportError(func.span, "function return type does not match body");
            }
        } else {
            func.return_type = body_ty;
        }
    }
}

fn typecheckStruct(crate: *hir.Crate, structure: *hir.Struct, diagnostics: *diag.Diagnostics) Error!void {
    for (structure.fields) |field| {
        try ensureKnownType(crate, field.ty, field.span, diagnostics);
    }
}

fn ensureKnownType(crate: *hir.Crate, ty: hir.TypeId, span: hir.Span, diagnostics: *diag.Diagnostics) Error!void {
    if (ty >= crate.types.items.len) {
        diagnostics.reportError(span, "unknown type reference");
        return;
    }

    const kind = crate.types.items[ty].kind;
    if (kind == .Unknown) {
        diagnostics.reportError(span, "type could not be resolved");
    }
}

fn checkExpr(
    crate: *hir.Crate,
    expr_id: hir.ExprId,
    diagnostics: *diag.Diagnostics,
    locals: *std.AutoHashMap(hir.LocalId, hir.TypeId),
) Error!hir.TypeId {
    const expr = &crate.exprs.items[expr_id];
    const span = expr.span;
    switch (expr.kind) {
        .ConstInt => {
            expr.ty = try ensureType(crate, .{ .PrimInt = .I64 });
        },
        .ConstFloat => {
            expr.ty = try ensureType(crate, .{ .PrimFloat = .F64 });
        },
        .ConstBool => {
            expr.ty = try ensureType(crate, .Bool);
        },
        .ConstChar => {
            expr.ty = try ensureType(crate, .Char);
        },
        .ConstString => {
            expr.ty = try ensureType(crate, .String);
        },
        .LocalRef => |local_id| {
            if (locals.get(local_id)) |ty| {
                expr.ty = ty;
            } else {
                diagnostics.reportError(span, "use of local with unknown type");
                expr.ty = try ensureType(crate, .Unknown);
            }
        },
        .GlobalRef => |def_id| {
            if (def_id < crate.items.items.len) {
                const item = crate.items.items[def_id];
                switch (item.kind) {
                    .Function => |fn_item| {
                        if (fn_item.return_type) |ret| {
                            expr.ty = ret;
                        } else {
                            expr.ty = try ensureType(crate, .Unknown);
                        }
                    },
                    .Struct => {
                        expr.ty = try ensureType(crate, .{ .Struct = .{ .def_id = def_id, .type_args = &[_]hir.TypeId{} } });
                    },
                    .TypeAlias => |alias| expr.ty = alias.target,
                    else => expr.ty = try ensureType(crate, .Unknown),
                }
            } else {
                diagnostics.reportError(span, "reference to missing item");
                expr.ty = try ensureType(crate, .Unknown);
            }
        },
        .Block => |block| {
            var last_ty: ?hir.TypeId = null;
            for (block.stmts) |stmt_id| {
                try checkStmt(crate, stmt_id, diagnostics, locals, &last_ty);
            }
            if (block.tail) |tail| {
                last_ty = try checkExpr(crate, tail, diagnostics, locals);
            }
            expr.ty = last_ty orelse try ensureType(crate, .Unknown);
        },
        .UnresolvedIdent => {
            diagnostics.reportError(span, "identifier was not resolved before type checking");
            expr.ty = try ensureType(crate, .Unknown);
        },
        .Unknown => {
            expr.ty = try ensureType(crate, .Unknown);
        },
    }

    return expr.ty;
}

fn checkStmt(
    crate: *hir.Crate,
    stmt_id: hir.StmtId,
    diagnostics: *diag.Diagnostics,
    locals: *std.AutoHashMap(hir.LocalId, hir.TypeId),
    last_ty: *?hir.TypeId,
) Error!void {
    const stmt = &crate.stmts.items[stmt_id];
    switch (stmt.kind) {
        .Let => |*let_stmt| {
            var declared_ty = let_stmt.ty;
            if (let_stmt.value) |value_id| {
                const value_ty = try checkExpr(crate, value_id, diagnostics, locals);
                if (declared_ty) |*ty_id| {
                    if (!typesEqual(crate, ty_id.*, value_ty)) {
                        diagnostics.reportError(stmt.span, "mismatched types in let binding");
                    }
                } else {
                    declared_ty = value_ty;
                    let_stmt.ty = value_ty;
                }
            }

            if (let_stmt.pat.kind == .Identifier) {
                try locals.put(let_stmt.pat.id, declared_ty orelse try ensureType(crate, .Unknown));
            }
            last_ty.* = declared_ty;
        },
        .Expr => |expr_id| {
            last_ty.* = try checkExpr(crate, expr_id, diagnostics, locals);
        },
        .Unknown => {},
    }
}

fn ensureType(crate: *hir.Crate, kind: hir.Type.Kind) Error!hir.TypeId {
    for (crate.types.items) |existing| {
        if (std.meta.eql(existing.kind, kind)) return existing.id;
    }

    const id: hir.TypeId = @intCast(crate.types.items.len);
    try crate.types.append(crate.allocator(), .{ .id = id, .kind = kind });
    return id;
}

fn typesEqual(crate: *hir.Crate, lhs: hir.TypeId, rhs: hir.TypeId) bool {
    if (lhs == rhs) return true;
    if (lhs >= crate.types.items.len or rhs >= crate.types.items.len) return false;
    return std.meta.eql(crate.types.items[lhs].kind, crate.types.items[rhs].kind);
}

test "typechecker assigns literal types and detects mismatches" {
    const allocator = std.testing.allocator;
    var diagnostics = diag.Diagnostics.init(allocator);
    defer diagnostics.deinit();

    var crate = hir.Crate.init(allocator);
    defer crate.deinit();

    const span = hir.emptySpan(0);

    const bool_ty = try ensureType(&crate, .Bool);
    _ = try ensureType(&crate, .{ .PrimInt = .I64 });

    const value_expr_id: hir.ExprId = @intCast(crate.exprs.items.len);
    try crate.exprs.append(crate.allocator(), .{ .id = value_expr_id, .kind = .{ .ConstBool = true }, .ty = 0, .span = span });

    const stmt_id: hir.StmtId = @intCast(crate.stmts.items.len);
    try crate.stmts.append(crate.allocator(), .{ .id = stmt_id, .kind = .{ .Let = .{ .pat = .{ .id = 0, .kind = .{ .Identifier = "x" }, .span = span }, .ty = bool_ty, .value = value_expr_id } }, .span = span });

    const block_stmts = try crate.allocator().alloc(hir.StmtId, 1);
    block_stmts[0] = stmt_id;
    const block_expr_id: hir.ExprId = @intCast(crate.exprs.items.len);
    try crate.exprs.append(crate.allocator(), .{ .id = block_expr_id, .kind = .{ .Block = .{ .stmts = block_stmts, .tail = null } }, .ty = 0, .span = span });

    try crate.items.append(crate.allocator(), .{ .id = 0, .kind = .{ .Function = .{ .def_id = 0, .name = "main", .params = &[_]hir.LocalId{}, .return_type = bool_ty, .body = block_expr_id, .span = span } }, .span = span });

    try typecheck(&crate, &diagnostics);

    try std.testing.expectEqual(bool_ty, crate.exprs.items[value_expr_id].ty);
    try std.testing.expect(!diagnostics.hasErrors());

    // Now introduce a mismatch.
    crate.stmts.items[stmt_id].kind.Let.ty = bool_ty;
    crate.exprs.items[value_expr_id].kind = .{ .ConstInt = 1 };
    try typecheck(&crate, &diagnostics);
    try std.testing.expect(diagnostics.hasErrors());
}
