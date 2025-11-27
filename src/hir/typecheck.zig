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

    for (func.params, 0..) |local_id, idx| {
        const ty = if (idx < func.param_types.len)
            func.param_types[idx]
        else
            try ensureType(crate, .Unknown);
        try locals.put(local_id, ty);
    }

    if (func.body) |body_id| {
        const body_ty = try checkExpr(crate, body_id, diagnostics, &locals, false);
        if (func.return_type) |ret_ty| {
            if (!typesCompatible(crate, ret_ty, body_ty)) {
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
in_unsafe: bool,
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
                        const ret_ty = fn_item.return_type orelse try ensureType(crate, .Unknown);
                        expr.ty = try ensureType(crate, .{ .Fn = .{ .params = fn_item.param_types, .ret = ret_ty } });
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
                try checkStmt(crate, stmt_id, diagnostics, locals, &last_ty, in_unsafe);
            }
            if (block.tail) |tail| {
                last_ty = try checkExpr(crate, tail, diagnostics, locals, in_unsafe);
            }
            expr.ty = last_ty orelse try ensureType(crate, .Unknown);
        },

        .Unsafe => |u| {
    // Inside the unsafe block body, we typecheck with in_unsafe = true.
    const body_ty = try checkExpr(crate, u.body, diagnostics, locals, true);
    expr.ty = body_ty;
},

        .UnresolvedIdent => {
            diagnostics.reportError(span, "identifier was not resolved before type checking");
            expr.ty = try ensureType(crate, .Unknown);
        },
        .Cast => |c| {
            // Type check the inner expression first
            _ = try checkExpr(crate, c.expr, diagnostics, locals, in_unsafe);
            expr.ty = c.ty;
        },
        .Binary => |bin| {
            const lhs_ty = try checkExpr(crate, bin.lhs, diagnostics, locals, in_unsafe);
            const rhs_ty = try checkExpr(crate, bin.rhs, diagnostics, locals, in_unsafe);

            switch (bin.op) {
                .LogicalOr, .LogicalAnd => {
                    if (!isBool(crate, lhs_ty) or !isBool(crate, rhs_ty)) {
                        diagnostics.reportError(span, "logical operators require boolean operands");
                    }
                    expr.ty = try ensureType(crate, .Bool);
                },
                .Eq, .Ne, .Lt, .Le, .Gt, .Ge => {
                    if (!typesCompatible(crate, lhs_ty, rhs_ty)) {
                        diagnostics.reportError(span, "comparison operands must have the same type");
                    }
                    expr.ty = try ensureType(crate, .Bool);
                },
                .Add, .Sub, .Mul, .Div, .Mod => {
                    if (!typesCompatible(crate, lhs_ty, rhs_ty)) {
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
            const operand_ty = try checkExpr(crate, un.expr, diagnostics, locals, in_unsafe);
            switch (un.op) {
                .Not => {
                    if (!isBool(crate, operand_ty)) diagnostics.reportError(span, "`!` expects a boolean operand");
                    expr.ty = try ensureType(crate, .Bool);
                },
                .Neg => {
                    if (!isNumeric(crate, operand_ty)) diagnostics.reportError(span, "unary `-` expects a numeric operand");
                    expr.ty = operand_ty;
                },


                .Deref => {
    if (operand_ty >= crate.types.items.len) {
        diagnostics.reportError(span, "unknown operand type for deref");
        expr.ty = try ensureType(crate, .Unknown);
    } else {
        expr.ty = switch (crate.types.items[operand_ty].kind) {
            .Pointer => |ptr| blk: {
                if (!in_unsafe) {
                    diagnostics.reportError(
                        span,
                        "dereferencing a raw pointer is unsafe; wrap this in `unsafe { ... }`",
                    );
                }
                break :blk ptr.inner;
            },
            .Ref => |r| r.inner,
            else => blk: {
                diagnostics.reportError(span, "cannot dereference non-pointer type");
                break :blk try ensureType(crate, .Unknown);
            },
        };
    }
},


                .Ref, .RefMut => {
                    const inner = operand_ty;
                    expr.ty = try ensureType(crate, .{ .Ref = .{ .mutable = un.op == .RefMut, .inner = inner } });
                },
            }
        },
        .Call => |call| {
            // Check if this is an array method call (e.g., data.len())
            if (isArrayMethodCall(crate, call.callee, diagnostics, locals, in_unsafe)) |method_info| {
                // Handle array method calls
                if (std.mem.eql(u8, method_info.method_name, "len")) {
                    // len() takes no arguments and returns usize
                    if (call.args.len != 0) {
                        diagnostics.reportError(span, "len() takes no arguments");
                    }
                    expr.ty = try ensureType(crate, .{ .PrimInt = .Usize });
                } else {
                    diagnostics.reportError(span, "unknown array method");
                    expr.ty = try ensureType(crate, .Unknown);
                }
            } else if (isPointerMethodCall(crate, call.callee, diagnostics, locals, in_unsafe)) |method_info| {
                // Handle pointer method calls
                if (std.mem.eql(u8, method_info.method_name, "is_null")) {
                    // is_null() takes no arguments and returns bool
                    if (call.args.len != 0) {
                        diagnostics.reportError(span, "is_null() takes no arguments");
                    }
                    expr.ty = try ensureType(crate, .Bool);
                } else {
                    diagnostics.reportError(span, "unknown pointer method");
                    expr.ty = try ensureType(crate, .Unknown);
                }
            } else if (isStructMethodCall(crate, call.callee, diagnostics, locals, in_unsafe)) |method_info| {
                // Handle struct method calls (e.g., point.offset(1, 2))
                // Type check all arguments
                for (call.args) |arg_id| {
                    _ = try checkExpr(crate, arg_id, diagnostics, locals, in_unsafe);
                }
                // Set the return type from the method
                expr.ty = method_info.ret_ty;
            } else {
                const callee_ty = try checkExpr(crate, call.callee, diagnostics, locals, in_unsafe);
                var param_types: []hir.TypeId = &[_]hir.TypeId{};
                var ret_ty: hir.TypeId = try ensureType(crate, .Unknown);

                if (isBuiltinPrintln(crate, call.callee)) {
                    for (call.args) |arg_id| {
                        _ = try checkExpr(crate, arg_id, diagnostics, locals, in_unsafe);
                    }
                    expr.ty = ret_ty;
                } else {
                    if (callee_ty < crate.types.items.len) {
                        switch (crate.types.items[callee_ty].kind) {
                            .Fn => |fn_ty| {
                                std.debug.print("Function call detected with callee type {any}\n", .{fn_ty});
                                if (fn_ty.params.len == 0 and call.args.len > 0) {
                                    const inferred = try crate.allocator().alloc(hir.TypeId, call.args.len);
                                    for (inferred) |*slot| {
                                        slot.* = try ensureType(crate, .Unknown);
                                    }
                                    param_types = inferred;
                                } else {
                                    param_types = fn_ty.params;
                                }
                                ret_ty = fn_ty.ret;
                            },
                            else => {},
                        }
                    }

                    if (param_types.len != 0 and param_types.len != call.args.len) {
                        std.debug.print("Expected {d} args, got {d}\n", .{ param_types.len, call.args.len });
                        diagnostics.reportError(span, "argument count does not match function type");
                    }

                    for (call.args, 0..) |arg_id, idx| {
                        const arg_ty = try checkExpr(crate, arg_id, diagnostics, locals, in_unsafe);
                        if (idx < param_types.len) {
                            const expected = param_types[idx];
                            const expected_kind = if (expected < crate.types.items.len)
                                crate.types.items[expected].kind
                            else
                                .Unknown;
                            if (expected_kind != .Fn and !typesCompatible(crate, arg_ty, expected)) {
                                diagnostics.reportError(span, "argument type does not match parameter");
                            }
                        }
                    }
                    expr.ty = ret_ty;
                }
            }
        },
        .MethodCall => |call| {
            _ = try checkExpr(crate, call.target, diagnostics, locals, in_unsafe);
            for (call.args) |arg_id| {
                _ = try checkExpr(crate, arg_id, diagnostics, locals, in_unsafe);
            }
            expr.ty = try ensureType(crate, .Unknown);
        },
        .Assignment => |assign| {
            const target_ty = try checkExpr(crate, assign.target, diagnostics, locals, in_unsafe);
            const value_ty = try checkExpr(crate, assign.value, diagnostics, locals, in_unsafe);

            if (!typesCompatible(crate, target_ty, value_ty)) {

                std.debug.print("Assignment type mismatch: target {any}, value {any}\n", .{crate.types.items[target_ty], crate.types.items[value_ty]});

                diagnostics.reportError(span, "assignment types do not match");
            }

            expr.ty = target_ty;
        },
        .Return => |ret| {
            if (ret) |value_id| {
                expr.ty = try checkExpr(crate, value_id, diagnostics, locals, in_unsafe);
            } else {
                expr.ty = try ensureType(crate, .Unknown);
            }
        },
        .If => |ifs| {
            const cond_ty = try checkExpr(crate, ifs.cond, diagnostics, locals, in_unsafe);
            if (!isBool(crate, cond_ty)) diagnostics.reportError(span, "if condition must be boolean");

            const then_ty = try checkExpr(crate, ifs.then_branch, diagnostics, locals, in_unsafe);
            if (ifs.else_branch) |else_id| {
                const else_ty = try checkExpr(crate, else_id, diagnostics, locals, in_unsafe);
                if (typesCompatible(crate, then_ty, else_ty)) {
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
            const cond_ty = try checkExpr(crate, wh.cond, diagnostics, locals, in_unsafe);
            if (!isBool(crate, cond_ty)) diagnostics.reportError(span, "while condition must be boolean");
            _ = try checkExpr(crate, wh.body, diagnostics, locals, in_unsafe);
            expr.ty = try ensureType(crate, .Unknown);
        },
        .For => |for_expr| {
            const iter_ty = try checkExpr(crate, for_expr.iter, diagnostics, locals, in_unsafe);

            // Check if the iterator is a range expression
            const iter_expr = &crate.exprs.items[for_expr.iter];
            const elem_ty = if (iter_expr.kind == .Range) blk: {
                // For range expressions, the element type is the type of the range bounds
                break :blk iter_ty;
            } else blk: {
                // Otherwise, expect an array (or ref/pointer to array)
                break :blk switch (getArrayElementType(crate, iter_ty)) {
                    .ok => |et| et,
                    .err => blk2: {
                        diagnostics.reportError(span, "for loop iterator must be an array (or ref/pointer to array)");
                        break :blk2 try ensureType(crate, .Unknown);
                    },
                };
            };

            const pat_ty = try ensurePatternBinding(crate, for_expr.pat.id, locals);
            if (!isUnknown(crate, pat_ty) and !typesCompatible(crate, pat_ty, elem_ty)) {
                diagnostics.reportError(span, "for pattern type does not match iterator");
            }
            try locals.put(for_expr.pat.id, elem_ty);
            _ = try checkExpr(crate, for_expr.body, diagnostics, locals, in_unsafe);
            expr.ty = try ensureType(crate, .Unknown);
        },
        .Range => |range| {
            const start_ty = try checkExpr(crate, range.start, diagnostics, locals, in_unsafe);
            const end_ty = try checkExpr(crate, range.end, diagnostics, locals, in_unsafe);
            if (!typesCompatible(crate, start_ty, end_ty)) {
                diagnostics.reportError(span, "range bounds must have the same type");
                expr.ty = try ensureType(crate, .Unknown);
            } else {
                expr.ty = start_ty;
            }
        },
        .Index => |index| {
            const target_ty = try checkExpr(crate, index.target, diagnostics, locals, in_unsafe);
            _ = try checkExpr(crate, index.index, diagnostics, locals, in_unsafe);
            expr.ty = switch (getArrayElementType(crate, target_ty)) {
                .ok => |elem_ty| elem_ty,
                .err => blk: {
                    diagnostics.reportError(span, "indexing requires an array type");
                    break :blk try ensureType(crate, .Unknown);
                },
            };
        },
        .Field => |field| {
            const target_ty = try checkExpr(crate, field.target, diagnostics, locals, in_unsafe);
            expr.ty = try resolveFieldType(crate, target_ty, field.name, span, diagnostics);
        },
        .Array => |elements| {
            var element_ty: ?hir.TypeId = null;
            for (elements) |elem_id| {
                const ty = try checkExpr(crate, elem_id, diagnostics, locals, in_unsafe);
                if (element_ty) |existing| {
                    if (!typesCompatible(crate, existing, ty)) {
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
                    const value_ty = try checkExpr(crate, field_init.value, diagnostics, locals, in_unsafe);
                    const expected_ty = findFieldType(struct_item, field_init.name);
                    if (expected_ty) |ty| {
                        if (!typesCompatible(crate, ty, value_ty)) {
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
        .Lambda => |lambda| {
            var lambda_locals = std.AutoHashMap(hir.LocalId, hir.TypeId).init(diagnostics.allocator);
            defer lambda_locals.deinit();

            var param_types = try crate.allocator().alloc(hir.TypeId, lambda.params.len);
            for (lambda.params, 0..) |param, idx| {
                const annotated = if (idx < lambda.param_types.len) lambda.param_types[idx] else null;
                const ty_id = annotated orelse try ensureType(crate, .Unknown);
                param_types[idx] = ty_id;
                try lambda_locals.put(param.id, ty_id);
            }

            var outer_iter = locals.iterator();
            while (outer_iter.next()) |entry| {
                try lambda_locals.put(entry.key_ptr.*, entry.value_ptr.*);
            }

            const body_ty = try checkExpr(crate, lambda.body, diagnostics, &lambda_locals, in_unsafe);
            expr.ty = try ensureType(crate, .{ .Fn = .{ .params = param_types, .ret = body_ty } });
        },
        .Path => {
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
    in_unsafe: bool,
) Error!void {
    const stmt = &crate.stmts.items[stmt_id];
    switch (stmt.kind) {
        .Let => |*let_stmt| {
            var declared_ty = let_stmt.ty;
            if (let_stmt.value) |value_id| {
                const value_ty = try checkExpr(crate, value_id, diagnostics, locals, in_unsafe);
                if (declared_ty) |*ty_id| {


                    if (!typesCompatible(crate, ty_id.*, value_ty)) {
                    // view types

                        std.debug.print("Declared type: {any}, Value type: {any}\n", .{crate.types.items[ty_id.*], crate.types.items[value_ty]});

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
            last_ty.* = try checkExpr(crate, expr_id, diagnostics, locals, in_unsafe);
        },
        .Unknown => {},
    }
}

fn ensurePatternBinding(crate: *hir.Crate, local_id: hir.LocalId, locals: *std.AutoHashMap(hir.LocalId, hir.TypeId)) Error!hir.TypeId {
    if (locals.get(local_id)) |ty| return ty;
    const ty = try ensureType(crate, .Unknown);
    try locals.put(local_id, ty);
    return ty;
}

fn ensureType(crate: *hir.Crate, kind: hir.Type.Kind) Error!hir.TypeId {
    for (crate.types.items) |existing| {
        if (std.meta.eql(existing.kind, kind)) return existing.id;
    }

    const id: hir.TypeId = @intCast(crate.types.items.len);
    try crate.types.append(crate.allocator(), .{ .id = id, .kind = kind });
    return id;
}

fn isUnknown(crate: *hir.Crate, ty: hir.TypeId) bool {
    return ty >= crate.types.items.len or crate.types.items[ty].kind == .Unknown;
}

fn typesEqual(crate: *hir.Crate, lhs: hir.TypeId, rhs: hir.TypeId) bool {
    if (lhs == rhs) return true;
    if (lhs >= crate.types.items.len or rhs >= crate.types.items.len) return false;
    return std.meta.eql(crate.types.items[lhs].kind, crate.types.items[rhs].kind);
}

/// Resolves a type through type aliases. If the type is a Path that refers to a type alias,
/// returns the underlying type. Otherwise returns the original type.
fn resolveType(crate: *hir.Crate, ty: hir.TypeId) hir.TypeId {
    if (ty >= crate.types.items.len) return ty;

    const kind = crate.types.items[ty].kind;
    switch (kind) {
        .Path => |path| {
            // Look up the type alias by name
            if (path.segments.len == 1) {
                const name = path.segments[0];
                for (crate.items.items) |item| {
                    if (item.kind == .TypeAlias) {
                        const alias = item.kind.TypeAlias;
                        if (std.mem.eql(u8, alias.name, name)) {
                            // Recursively resolve in case of chained aliases
                            return resolveType(crate, alias.target);
                        }
                    }
                }
            }
            return ty;
        },
        else => return ty,
    }
}

fn typesCompatible(crate: *hir.Crate, lhs: hir.TypeId, rhs: hir.TypeId) bool {
    // Resolve type aliases before comparing
    const resolved_lhs = resolveType(crate, lhs);
    const resolved_rhs = resolveType(crate, rhs);

    if (typesEqual(crate, resolved_lhs, resolved_rhs)) return true;
    if (resolved_lhs >= crate.types.items.len or resolved_rhs >= crate.types.items.len) return false;

    if (isUnknown(crate, resolved_lhs) or isUnknown(crate, resolved_rhs)) return true;

    const lhs_kind = crate.types.items[resolved_lhs].kind;
    const rhs_kind = crate.types.items[resolved_rhs].kind;

    return switch (lhs_kind) {
        .PrimInt => rhs_kind == .PrimInt,
        .PrimFloat => rhs_kind == .PrimFloat,
        .Fn => switch (rhs_kind) {
            .Fn => true,
            else => false,
        },
        .Pointer => |lhs_ptr| switch (rhs_kind) {
            .Pointer => |rhs_ptr| lhs_ptr.mutable == rhs_ptr.mutable and typesCompatible(crate, lhs_ptr.inner, rhs_ptr.inner),
            // Allow Ref to Pointer coercion (like Rust's implicit conversion)
            .Ref => |rhs_ref| !lhs_ptr.mutable or rhs_ref.mutable and typesCompatible(crate, lhs_ptr.inner, rhs_ref.inner),
            else => false,
        },
        .Ref => |lhs_ref| switch (rhs_kind) {
            .Ref => |rhs_ref| lhs_ref.mutable == rhs_ref.mutable and typesCompatible(crate, lhs_ref.inner, rhs_ref.inner),
            else => false,
        },
        .Struct => |lhs_struct| switch (rhs_kind) {
            .Struct => |rhs_struct| lhs_struct.def_id == rhs_struct.def_id,
            .Path => |path| structMatchesPath(crate, lhs_struct.def_id, path.segments),
            else => false,
        },
        .Path => |lhs_path| switch (rhs_kind) {
            .Struct => |rhs_struct| structMatchesPath(crate, rhs_struct.def_id, lhs_path.segments),
            .Path => |rhs_path| pathsMatch(lhs_path.segments, rhs_path.segments),
            else => false,
        },

          .Array => |lhs_arr| switch (rhs_kind) {
            .Array => |rhs_arr| blk: {
                if (lhs_arr.size_const != rhs_arr.size_const) break :blk false;

                break :blk typesCompatible(crate, lhs_arr.elem, rhs_arr.elem);
            },
            else => false,
        },

        else => false,
    };
}

fn isBool(crate: *hir.Crate, ty: hir.TypeId) bool {
    const resolved = resolveType(crate, ty);
    return resolved < crate.types.items.len and crate.types.items[resolved].kind == .Bool;
}

fn isNumeric(crate: *hir.Crate, ty: hir.TypeId) bool {
    const resolved = resolveType(crate, ty);
    if (resolved >= crate.types.items.len) return false;
    return switch (crate.types.items[resolved].kind) {
        .PrimInt, .PrimFloat => true,
        else => false,
    };
}

fn structMatchesPath(crate: *hir.Crate, struct_def_id: hir.DefId, path_segments: [][]const u8) bool {
    if (path_segments.len != 1) return false;
    const path_name = path_segments[0];
    if (struct_def_id < crate.items.items.len) {
        const item = crate.items.items[struct_def_id];
        if (item.kind == .Struct) {
            return std.mem.eql(u8, item.kind.Struct.name, path_name);
        }
    }
    return false;
}

fn pathsMatch(lhs_segments: [][]const u8, rhs_segments: [][]const u8) bool {
    if (lhs_segments.len != rhs_segments.len) return false;
    for (lhs_segments, rhs_segments) |l, r| {
        if (!std.mem.eql(u8, l, r)) return false;
    }
    return true;
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

    const resolved_ty = resolveType(crate, ty);
    if (resolved_ty >= crate.types.items.len) {
        diagnostics.reportError(span, "unknown type for field access");
        return ensureType(crate, .Unknown);
    }

    switch (crate.types.items[resolved_ty].kind) {
        .Struct => |info| {
            if (info.def_id < crate.items.items.len) {
                const item = crate.items.items[info.def_id];
                if (item.kind == .Struct) {
                    // First check for fields
                    if (findFieldType(item.kind.Struct, name)) |field_ty| {
                        return field_ty;
                    }

                    // If no field found, check for methods in impl blocks
                    if (findMethodType(crate, info.def_id, name)) |method_ty| {
                        return method_ty;
                    }

                    diagnostics.reportError(span, "unknown field on struct");
                    return ensureType(crate, .Unknown);
                }
            }
            diagnostics.reportError(span, "invalid struct reference in type");
            return ensureType(crate, .Unknown);
        },
        .Path => |path| {
            // Try to find the struct this path refers to
            if (path.segments.len == 1) {
                const struct_name = path.segments[0];
                for (crate.items.items, 0..) |item, idx| {
                    if (item.kind == .Struct) {
                        const struct_item = item.kind.Struct;
                        if (std.mem.eql(u8, struct_item.name, struct_name)) {
                            // Found the struct - check for field
                            if (findFieldType(struct_item, name)) |field_ty| {
                                return field_ty;
                            }

                            // Check for methods
                            if (findMethodType(crate, @intCast(idx), name)) |method_ty| {
                                return method_ty;
                            }

                            diagnostics.reportError(span, "unknown field on struct");
                            return ensureType(crate, .Unknown);
                        }
                    }
                }
            }
            diagnostics.reportError(span, "unresolved path type for field access");
            return ensureType(crate, .Unknown);
        },
        // Pointer types can have methods like is_null()
        .Pointer => {
            // For pointer methods, we return Unknown here and let the Call handling resolve it
            // This field access will be treated as a method reference
            return ensureType(crate, .Unknown);
        },
        else => {
            diagnostics.reportError(span, "field access on non-struct type");
            return ensureType(crate, .Unknown);
        },
    }
}

fn findMethodType(crate: *hir.Crate, struct_def_id: hir.DefId, method_name: []const u8) ?hir.TypeId {
    // Look through all impl blocks for this struct
    for (crate.items.items) |item| {
        if (item.kind == .Impl) {
            const impl_item = item.kind.Impl;
            // Check if this impl is for the struct we're looking at
            // The target should be a Path or Struct type referring to our struct
            if (impl_item.target < crate.types.items.len) {
                const target_kind = crate.types.items[impl_item.target].kind;
                var matches = false;
                switch (target_kind) {
                    .Struct => |info| {
                        matches = info.def_id == struct_def_id;
                    },
                    .Path => |path| {
                        // Check if the path name matches the struct name
                        if (path.segments.len == 1) {
                            const struct_item = crate.items.items[struct_def_id];
                            if (struct_item.kind == .Struct) {
                                matches = std.mem.eql(u8, path.segments[0], struct_item.kind.Struct.name);
                            }
                        }
                    },
                    else => {},
                }

                if (matches) {
                    // Look for the method in this impl block
                    for (impl_item.methods) |method_id| {
                        if (method_id < crate.items.items.len) {
                            const method_item = crate.items.items[method_id];
                            if (method_item.kind == .Function) {
                                const func = method_item.kind.Function;
                                // Method name is mangled as StructName_methodName
                                // Check if it ends with _methodName
                                const name_matches = std.mem.endsWith(u8, func.name, method_name) and
                                    func.name.len > method_name.len and
                                    func.name[func.name.len - method_name.len - 1] == '_';
                                if (name_matches) {
                                    // Return the method's return type
                                    if (func.return_type) |ret_ty| {
                                        return ret_ty;
                                    }
                                    // If no return type, return a placeholder
                                    return ensureType(crate, .Unknown) catch null;
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

fn isBuiltinPrintln(crate: *hir.Crate, callee_id: hir.ExprId) bool {
    if (callee_id >= crate.exprs.items.len) return false;
    const expr = crate.exprs.items[callee_id];
    if (expr.kind != .GlobalRef) return false;
    const def_id = expr.kind.GlobalRef;
    if (def_id >= crate.items.items.len) return false;
    const item = crate.items.items[def_id];
    return item.kind == .Function and std.mem.eql(u8, item.kind.Function.name, "println");
}

const PointerMethodInfo = struct {
    target_ty: hir.TypeId,
    method_name: []const u8,
};

fn isPointerMethodCall(
    crate: *hir.Crate,
    callee_id: hir.ExprId,
    diagnostics: *diag.Diagnostics,
    locals: *std.AutoHashMap(hir.LocalId, hir.TypeId),
in_unsafe: bool,
) ?PointerMethodInfo {
    if (callee_id >= crate.exprs.items.len) return null;
    const callee_expr = crate.exprs.items[callee_id];

    // Check if the callee is a Field expression
    if (callee_expr.kind != .Field) return null;

    const field = callee_expr.kind.Field;

    // Check if the target is a pointer type
    const target_ty = checkExpr(crate, field.target, diagnostics, locals, in_unsafe) catch return null;
    if (target_ty >= crate.types.items.len) return null;

    switch (crate.types.items[target_ty].kind) {
        .Pointer => {
            return PointerMethodInfo{
                .target_ty = target_ty,
                .method_name = field.name,
            };
        },
        else => return null,
    }
}

const ArrayMethodInfo = struct {
    target_ty: hir.TypeId,
    method_name: []const u8,
};

fn isArrayMethodCall(
    crate: *hir.Crate,
    callee_id: hir.ExprId,
    diagnostics: *diag.Diagnostics,
    locals: *std.AutoHashMap(hir.LocalId, hir.TypeId),
    in_unsafe: bool,
) ?ArrayMethodInfo {
    if (callee_id >= crate.exprs.items.len) return null;
    const callee_expr = crate.exprs.items[callee_id];

    // Check if the callee is a Field expression
    if (callee_expr.kind != .Field) return null;

    const field = callee_expr.kind.Field;

    // Check if the target is an array type (or ref/pointer to array)
    const target_ty = checkExpr(crate, field.target, diagnostics, locals, in_unsafe) catch return null;
    if (target_ty >= crate.types.items.len) return null;

    // Check if it's an array or ref/pointer to array
    const is_array = switch (crate.types.items[target_ty].kind) {
        .Array => true,
        .Ref => |ref| blk: {
            if (ref.inner >= crate.types.items.len) break :blk false;
            break :blk crate.types.items[ref.inner].kind == .Array;
        },
        .Pointer => |ptr| blk: {
            if (ptr.inner >= crate.types.items.len) break :blk false;
            break :blk crate.types.items[ptr.inner].kind == .Array;
        },
        else => false,
    };

    if (is_array) {
        return ArrayMethodInfo{
            .target_ty = target_ty,
            .method_name = field.name,
        };
    }
    return null;
}

const StructMethodInfo = struct {
    target_ty: hir.TypeId,
    method_name: []const u8,
    ret_ty: hir.TypeId,
};

fn isStructMethodCall(
    crate: *hir.Crate,
    callee_id: hir.ExprId,
    diagnostics: *diag.Diagnostics,
    locals: *std.AutoHashMap(hir.LocalId, hir.TypeId),
in_unsafe: bool,
) ?StructMethodInfo {
    if (callee_id >= crate.exprs.items.len) return null;
    const callee_expr = crate.exprs.items[callee_id];

    // Check if the callee is a Field expression
    if (callee_expr.kind != .Field) return null;

    const field = callee_expr.kind.Field;

    // Check if the target is a struct type
    const target_ty = checkExpr(crate, field.target, diagnostics, locals, in_unsafe) catch return null;
    const resolved_ty = resolveType(crate, target_ty);
    if (resolved_ty >= crate.types.items.len) return null;

    switch (crate.types.items[resolved_ty].kind) {
        .Struct => |info| {
            // Look for a method with this name in impl blocks
            for (crate.items.items) |item| {
                if (item.kind == .Impl) {
                    const impl_item = item.kind.Impl;
                    // Check if this impl is for our struct
                    var matches = false;
                    if (impl_item.target < crate.types.items.len) {
                        const target_kind = crate.types.items[impl_item.target].kind;
                        switch (target_kind) {
                            .Struct => |impl_info| {
                                matches = impl_info.def_id == info.def_id;
                            },
                            .Path => |path| {
                                if (path.segments.len == 1) {
                                    const struct_item = crate.items.items[info.def_id];
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
                            if (method_id < crate.items.items.len) {
                                const method_item = crate.items.items[method_id];
                                if (method_item.kind == .Function) {
                                    const func = method_item.kind.Function;
                                    // Method name is mangled as StructName_methodName
                                    // Check if it ends with _methodName
                                    const name_matches = std.mem.endsWith(u8, func.name, field.name) and
                                        func.name.len > field.name.len and
                                        func.name[func.name.len - field.name.len - 1] == '_';
                                    if (name_matches) {
                                        // Found the method - return its return type
                                        const ret_ty = func.return_type orelse blk: {
                                            const unknown = ensureType(crate, .Unknown) catch return null;
                                            break :blk unknown;
                                        };
                                        return StructMethodInfo{
                                            .target_ty = target_ty,
                                            .method_name = field.name,
                                            .ret_ty = ret_ty,
                                        };
                                    }
                                }
                            }
                        }
                    }
                }
            }
            return null;
        },
        else => return null,
    }
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

    try crate.items.append(crate.allocator(), .{ .id = 0, .kind = .{ .Function = .{ .def_id = 0, .name = "main", .params = &[_]hir.LocalId{}, .param_types = &[_]hir.TypeId{}, .return_type = bool_ty, .body = block_expr_id, .span = span } }, .span = span });

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

    try std.testing.expectEqual(i64_ty, try checkExpr(&crate, binary_expr_id, &diagnostics, &locals, false));
    try std.testing.expectEqual(bool_ty, try checkExpr(&crate, unary_expr_id, &diagnostics, &locals, false));
    const expected_arr_ty = try ensureType(&crate, .{ .Array = .{ .elem = i64_ty, .size_const = 2 } });
    try std.testing.expectEqual(expected_arr_ty, try checkExpr(&crate, array_expr_id, &diagnostics, &locals, false));
    try std.testing.expectEqual(struct_ty, try checkExpr(&crate, struct_init_expr_id, &diagnostics, &locals, false));
    try std.testing.expectEqual(i64_ty, try checkExpr(&crate, field_expr_id, &diagnostics, &locals, false));
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

    _ = try checkExpr(&crate, bad_binary_expr_id, &diagnostics, &locals, false);
    _ = try checkExpr(&crate, array_expr_id, &diagnostics, &locals, false);

    try std.testing.expect(diagnostics.hasErrors());
}
