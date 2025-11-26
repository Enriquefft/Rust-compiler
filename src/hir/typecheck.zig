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
        .Cast => |c| {
            expr.ty = c.ty;
        },
        .Binary => |bin| {
            const lhs_ty = try checkExpr(crate, bin.lhs, diagnostics, locals);
            const rhs_ty = try checkExpr(crate, bin.rhs, diagnostics, locals);

            switch (bin.op) {
                .LogicalOr, .LogicalAnd => {
                    if (!isBool(crate, lhs_ty) or !isBool(crate, rhs_ty)) {
                        diagnostics.reportError(span, "logical operators require boolean operands");
                    }
                    expr.ty = try ensureType(crate, .Bool);
                },
                .Eq, .Ne, .Lt, .Le, .Gt, .Ge => {
                    if (!typesEqual(crate, lhs_ty, rhs_ty)) {
                        diagnostics.reportError(span, "comparison operands must have the same type");
                    }
                    expr.ty = try ensureType(crate, .Bool);
                },
                .Add, .Sub, .Mul, .Div, .Mod => {
                    if (!typesEqual(crate, lhs_ty, rhs_ty)) {
                        diagnostics.reportError(span, "arithmetic operands must have the same type");
                        expr.ty = try ensureType(crate, .Unknown);
                    } else if (!isNumeric(crate, lhs_ty)) {
                        diagnostics.reportError(span, "arithmetic operands must be numeric");
                        expr.ty = try ensureType(crate, .Unknown);
                    } else {
                        expr.ty = lhs_ty;
                    }
                },
            }
        },
        .Unary => |un| {
            const operand_ty = try checkExpr(crate, un.expr, diagnostics, locals);
            switch (un.op) {
                .Not => {
                    if (!isBool(crate, operand_ty)) diagnostics.reportError(span, "`!` expects a boolean operand");
                    expr.ty = try ensureType(crate, .Bool);
                },
                .Neg => {
                    if (!isNumeric(crate, operand_ty)) diagnostics.reportError(span, "unary `-` expects a numeric operand");
                    expr.ty = operand_ty;
                },
            }
        },
        .Call => |call| {
            const callee_ty = try checkExpr(crate, call.callee, diagnostics, locals);
            var param_types: []hir.TypeId = &[_]hir.TypeId{};
            var ret_ty: hir.TypeId = try ensureType(crate, .Unknown);

            if (isBuiltinPrintln(crate, call.callee)) {
                for (call.args) |arg_id| {
                    _ = try checkExpr(crate, arg_id, diagnostics, locals);
                }
                expr.ty = ret_ty;
            } else {
                if (callee_ty < crate.types.items.len) {
                    switch (crate.types.items[callee_ty].kind) {
                        .Fn => |fn_ty| {
                            param_types = fn_ty.params;
                            ret_ty = fn_ty.ret;
                        },
                        else => {},
                    }
                }

                if (param_types.len != call.args.len) {
                    diagnostics.reportError(span, "argument count does not match function type");
                }

                for (call.args, 0..) |arg_id, idx| {
                    const arg_ty = try checkExpr(crate, arg_id, diagnostics, locals);
                    if (idx < param_types.len and !typesEqual(crate, arg_ty, param_types[idx])) {
                        diagnostics.reportError(span, "argument type does not match parameter");
                    }
                }
                expr.ty = ret_ty;
            }
        },
        .Assignment => |assign| {
            const target_ty = try checkExpr(crate, assign.target, diagnostics, locals);
            const value_ty = try checkExpr(crate, assign.value, diagnostics, locals);

            if (!typesEqual(crate, target_ty, value_ty)) {
                diagnostics.reportError(span, "assignment types do not match");
            }

            expr.ty = target_ty;
        },
        .Return => |ret| {
            if (ret) |value_id| {
                expr.ty = try checkExpr(crate, value_id, diagnostics, locals);
            } else {
                expr.ty = try ensureType(crate, .Unknown);
            }
        },
        .If => |ifs| {
            const cond_ty = try checkExpr(crate, ifs.cond, diagnostics, locals);
            if (!isBool(crate, cond_ty)) diagnostics.reportError(span, "if condition must be boolean");

            const then_ty = try checkExpr(crate, ifs.then_branch, diagnostics, locals);
            if (ifs.else_branch) |else_id| {
                const else_ty = try checkExpr(crate, else_id, diagnostics, locals);
                if (typesEqual(crate, then_ty, else_ty)) {
                    expr.ty = then_ty;
                } else {
                    diagnostics.reportError(span, "if branches must have the same type");
                    expr.ty = try ensureType(crate, .Unknown);
                }
            } else {
                expr.ty = try ensureType(crate, .Unknown);
            }
        },
        .While => |wh| {
            const cond_ty = try checkExpr(crate, wh.cond, diagnostics, locals);
            if (!isBool(crate, cond_ty)) diagnostics.reportError(span, "while condition must be boolean");
            _ = try checkExpr(crate, wh.body, diagnostics, locals);
            expr.ty = try ensureType(crate, .Unknown);
        },
        .Range => |range| {
            const start_ty = try checkExpr(crate, range.start, diagnostics, locals);
            const end_ty = try checkExpr(crate, range.end, diagnostics, locals);
            if (!typesEqual(crate, start_ty, end_ty)) {
                diagnostics.reportError(span, "range bounds must have the same type");
                expr.ty = try ensureType(crate, .Unknown);
            } else {
                expr.ty = start_ty;
            }
        },
        .Index => |index| {
            const target_ty = try checkExpr(crate, index.target, diagnostics, locals);
            _ = try checkExpr(crate, index.index, diagnostics, locals);
            expr.ty = switch (getArrayElementType(crate, target_ty)) {
                .ok => |elem_ty| elem_ty,
                .err => blk: {
                    diagnostics.reportError(span, "indexing requires an array type");
                    break :blk try ensureType(crate, .Unknown);
                },
            };
        },
        .Field => |field| {
            const target_ty = try checkExpr(crate, field.target, diagnostics, locals);
            expr.ty = try resolveFieldType(crate, target_ty, field.name, span, diagnostics);
        },
        .Array => |elements| {
            var element_ty: ?hir.TypeId = null;
            for (elements) |elem_id| {
                const ty = try checkExpr(crate, elem_id, diagnostics, locals);
                if (element_ty) |existing| {
                    if (!typesEqual(crate, existing, ty)) {
                        diagnostics.reportError(span, "array elements must have the same type");
                    }
                } else {
                    element_ty = ty;
                }
            }

            const arr_ty = if (element_ty) |elem|
                try ensureType(crate, .{ .Array = .{ .elem = elem, .size_const = @intCast(elements.len) } })
            else
                try ensureType(crate, .Unknown);

            expr.ty = arr_ty;
        },
        .StructInit => |init| {
            const maybe_def_id = findStructDef(crate, init.path);
            if (maybe_def_id) |def_id| {
                const struct_item = crate.items.items[def_id].kind.Struct;
                for (init.fields) |field_init| {
                    const value_ty = try checkExpr(crate, field_init.value, diagnostics, locals);
                    const expected_ty = findFieldType(struct_item, field_init.name);
                    if (expected_ty) |ty| {
                        if (!typesEqual(crate, ty, value_ty)) {
                            diagnostics.reportError(span, "struct field type mismatch");
                        }
                    } else {
                        diagnostics.reportError(span, "unknown field in struct initializer");
                    }
                }

                expr.ty = try ensureType(crate, .{ .Struct = .{ .def_id = def_id, .type_args = &[_]hir.TypeId{} } });
            } else {
                diagnostics.reportError(span, "unknown struct initializer");
                expr.ty = try ensureType(crate, .Unknown);
            }
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

fn isBool(crate: *hir.Crate, ty: hir.TypeId) bool {
    return ty < crate.types.items.len and crate.types.items[ty].kind == .Bool;
}

fn isNumeric(crate: *hir.Crate, ty: hir.TypeId) bool {
    if (ty >= crate.types.items.len) return false;
    return switch (crate.types.items[ty].kind) {
        .PrimInt, .PrimFloat => true,
        else => false,
    };
}

fn getArrayElementType(crate: *hir.Crate, ty: hir.TypeId) union(enum) { ok: hir.TypeId, err: void } {
    if (ty < crate.types.items.len) {
        switch (crate.types.items[ty].kind) {
            .Array => |arr| return .{ .ok = arr.elem },
            .Ref => |ref_ty| return getArrayElementType(crate, ref_ty.inner),
            .Pointer => |ptr_ty| return getArrayElementType(crate, ptr_ty.inner),
            else => {},
        }
    }
    return .{ .err = {} };
}

fn findStructDef(crate: *hir.Crate, path: [][]const u8) ?hir.DefId {
    if (path.len == 0) return null;
    const name = path[path.len - 1];
    for (crate.items.items, 0..) |item, idx| {
        if (item.kind == .Struct and std.mem.eql(u8, item.kind.Struct.name, name)) {
            return @intCast(idx);
        }
    }
    return null;
}

fn findFieldType(structure: hir.Struct, name: []const u8) ?hir.TypeId {
    for (structure.fields) |field| {
        if (std.mem.eql(u8, field.name, name)) return field.ty;
    }
    return null;
}

fn resolveFieldType(crate: *hir.Crate, ty: hir.TypeId, name: []const u8, span: hir.Span, diagnostics: *diag.Diagnostics) Error!hir.TypeId {
    if (ty >= crate.types.items.len) {
        diagnostics.reportError(span, "unknown type for field access");
        return ensureType(crate, .Unknown);
    }

    switch (crate.types.items[ty].kind) {
        .Struct => |info| {
            if (info.def_id < crate.items.items.len) {
                const item = crate.items.items[info.def_id];
                if (item.kind == .Struct) {
                    if (findFieldType(item.kind.Struct, name)) |field_ty| {
                        return field_ty;
                    }
                    diagnostics.reportError(span, "unknown field on struct");
                    return ensureType(crate, .Unknown);
                }
            }
            diagnostics.reportError(span, "invalid struct reference in type");
            return ensureType(crate, .Unknown);
        },
        else => {
            diagnostics.reportError(span, "field access on non-struct type");
            return ensureType(crate, .Unknown);
        },
    }
}

fn isBuiltinPrintln(crate: *hir.Crate, callee_id: hir.ExprId) bool {
    if (callee_id >= crate.exprs.items.len) return false;
    const expr = crate.exprs.items[callee_id];
    if (expr.kind != .GlobalRef) return false;
    const def_id = expr.kind.GlobalRef;
    if (def_id >= crate.items.items.len) return false;
    const item = crate.items.items[def_id];
    return item.kind == .Function and std.mem.eql(u8, item.kind.Function.name, "println");
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

test "typechecker handles composite expressions" {
    const allocator = std.testing.allocator;
    var diagnostics = diag.Diagnostics.init(allocator);
    defer diagnostics.deinit();

    var crate = hir.Crate.init(allocator);
    defer crate.deinit();

    const span = hir.emptySpan(0);

    const bool_ty = try ensureType(&crate, .Bool);
    const i64_ty = try ensureType(&crate, .{ .PrimInt = .I64 });

    const int_expr_id: hir.ExprId = @intCast(crate.exprs.items.len);
    try crate.exprs.append(crate.allocator(), .{ .id = int_expr_id, .kind = .{ .ConstInt = 1 }, .ty = 0, .span = span });

    const bool_expr_id: hir.ExprId = @intCast(crate.exprs.items.len);
    try crate.exprs.append(crate.allocator(), .{ .id = bool_expr_id, .kind = .{ .ConstBool = true }, .ty = 0, .span = span });

    const binary_expr_id: hir.ExprId = @intCast(crate.exprs.items.len);
    try crate.exprs.append(crate.allocator(), .{ .id = binary_expr_id, .kind = .{ .Binary = .{ .op = .Add, .lhs = int_expr_id, .rhs = int_expr_id } }, .ty = 0, .span = span });

    const unary_expr_id: hir.ExprId = @intCast(crate.exprs.items.len);
    try crate.exprs.append(crate.allocator(), .{ .id = unary_expr_id, .kind = .{ .Unary = .{ .op = .Not, .expr = bool_expr_id } }, .ty = 0, .span = span });

    const array_elements = try crate.allocator().alloc(hir.ExprId, 2);
    array_elements[0] = int_expr_id;
    array_elements[1] = int_expr_id;
    const array_expr_id: hir.ExprId = @intCast(crate.exprs.items.len);
    try crate.exprs.append(crate.allocator(), .{ .id = array_expr_id, .kind = .{ .Array = array_elements }, .ty = 0, .span = span });

    const struct_field = hir.Field{ .name = "value", .ty = i64_ty, .span = span };
    const struct_fields = try crate.allocator().alloc(hir.Field, 1);
    struct_fields[0] = struct_field;

    try crate.items.append(crate.allocator(), .{ .id = 0, .kind = .{ .Struct = .{ .def_id = 0, .name = "Wrapper", .fields = struct_fields, .span = span } }, .span = span });
    const struct_ty = try ensureType(&crate, .{ .Struct = .{ .def_id = 0, .type_args = &[_]hir.TypeId{} } });

    const struct_fields_init = try crate.allocator().alloc(hir.StructInitField, 1);
    struct_fields_init[0] = .{ .name = "value", .value = int_expr_id };
    const path_segments = try crate.allocator().alloc([]const u8, 1);
    path_segments[0] = "Wrapper";
    const struct_init_expr_id: hir.ExprId = @intCast(crate.exprs.items.len);
    try crate.exprs.append(crate.allocator(), .{ .id = struct_init_expr_id, .kind = .{ .StructInit = .{ .path = path_segments, .fields = struct_fields_init } }, .ty = 0, .span = span });

    const field_expr_id: hir.ExprId = @intCast(crate.exprs.items.len);
    try crate.exprs.append(crate.allocator(), .{ .id = field_expr_id, .kind = .{ .Field = .{ .target = struct_init_expr_id, .name = "value" } }, .ty = 0, .span = span });

    var locals = std.AutoHashMap(hir.LocalId, hir.TypeId).init(crate.allocator());
    defer locals.deinit();

    try std.testing.expectEqual(i64_ty, try checkExpr(&crate, binary_expr_id, &diagnostics, &locals));
    try std.testing.expectEqual(bool_ty, try checkExpr(&crate, unary_expr_id, &diagnostics, &locals));
    const expected_arr_ty = try ensureType(&crate, .{ .Array = .{ .elem = i64_ty, .size_const = 2 } });
    try std.testing.expectEqual(expected_arr_ty, try checkExpr(&crate, array_expr_id, &diagnostics, &locals));
    try std.testing.expectEqual(struct_ty, try checkExpr(&crate, struct_init_expr_id, &diagnostics, &locals));
    try std.testing.expectEqual(i64_ty, try checkExpr(&crate, field_expr_id, &diagnostics, &locals));
    try std.testing.expect(!diagnostics.hasErrors());
}

test "typechecker reports invalid expressions" {
    const allocator = std.testing.allocator;
    var diagnostics = diag.Diagnostics.init(allocator);
    defer diagnostics.deinit();

    var crate = hir.Crate.init(allocator);
    defer crate.deinit();

    const span = hir.emptySpan(0);

    _ = try ensureType(&crate, .Bool);
    _ = try ensureType(&crate, .{ .PrimInt = .I64 });

    const int_expr_id: hir.ExprId = @intCast(crate.exprs.items.len);
    try crate.exprs.append(crate.allocator(), .{ .id = int_expr_id, .kind = .{ .ConstInt = 1 }, .ty = 0, .span = span });
    const bool_expr_id: hir.ExprId = @intCast(crate.exprs.items.len);
    try crate.exprs.append(crate.allocator(), .{ .id = bool_expr_id, .kind = .{ .ConstBool = true }, .ty = 0, .span = span });

    const bad_binary_expr_id: hir.ExprId = @intCast(crate.exprs.items.len);
    try crate.exprs.append(crate.allocator(), .{ .id = bad_binary_expr_id, .kind = .{ .Binary = .{ .op = .Add, .lhs = int_expr_id, .rhs = bool_expr_id } }, .ty = 0, .span = span });

    const mixed_array = try crate.allocator().alloc(hir.ExprId, 2);
    mixed_array[0] = int_expr_id;
    mixed_array[1] = bool_expr_id;
    const array_expr_id: hir.ExprId = @intCast(crate.exprs.items.len);
    try crate.exprs.append(crate.allocator(), .{ .id = array_expr_id, .kind = .{ .Array = mixed_array }, .ty = 0, .span = span });

    var locals = std.AutoHashMap(hir.LocalId, hir.TypeId).init(crate.allocator());
    defer locals.deinit();

    _ = try checkExpr(&crate, bad_binary_expr_id, &diagnostics, &locals);
    _ = try checkExpr(&crate, array_expr_id, &diagnostics, &locals);

    try std.testing.expect(diagnostics.hasErrors());
}
