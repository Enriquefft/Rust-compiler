//! Type checking module for the HIR.
//!
//! This module performs type checking on the HIR, assigning types to all expressions
//! and validating type consistency throughout the program. It enforces:
//!
//! - Operator/type compatibility (e.g., `+` requires numeric operands)
//! - Return type matching function signatures
//! - Assignment type compatibility
//! - Condition expressions must be boolean
//! - Array elements must have the same type
//! - Struct field types must match initializers
//!
//! ## Type Inference
//!
//! For expressions without explicit type annotations (e.g., `let x = 1;`),
//! the type checker infers types from the context. Literals default to:
//! - Integer literals: `i64`
//! - Float literals: `f64`
//! - Boolean literals: `bool`
//! - Character literals: `char`
//! - String literals: `String`
//!
//! ## Limitations
//!
//! - Limited generic type support
//! - No trait bounds checking

const std = @import("std");
const diag = @import("../diag/diagnostics.zig");
const hir = @import("hir.zig");

const TypeParamSet = struct {
    map: std.StringHashMapUnmanaged(void) = .{},

    pub fn init(allocator: std.mem.Allocator, params: []const []const u8) !TypeParamSet {
        var set = TypeParamSet{};
        try set.map.ensureTotalCapacity(allocator, @intCast(params.len));
        for (params) |param| {
            try set.map.put(allocator, param, {});
        }
        return set;
    }

    pub fn empty() TypeParamSet {
        return .{};
    }

    pub fn deinit(self: *TypeParamSet, allocator: std.mem.Allocator) void {
        self.map.deinit(allocator);
    }

    pub fn contains(self: *const TypeParamSet, name: []const u8) bool {
        return self.map.get(name) != null;
    }
};

/// Error type for type checking operations (currently only allocation errors).
const Error = error{OutOfMemory};

/// Performs type checking on all items in the HIR crate.
///
/// This function traverses all functions, structs, and type aliases,
/// assigning types to expressions and validating type consistency.
///
/// ## Parameters
/// - `crate`: The HIR crate to type check (modified in place).
/// - `diagnostics`: Diagnostics collector for error reporting.
pub fn typecheck(crate: *hir.Crate, diagnostics: *diag.Diagnostics) Error!void {
    for (crate.items.items) |*item| {
        switch (item.kind) {
            .Function => |*func| try typecheckFunction(crate, func, diagnostics),
            .Struct => |*structure| try typecheckStruct(crate, structure, diagnostics),
            .TypeAlias => |*alias| {
                const empty_type_params = TypeParamSet.empty();
                try ensureKnownType(crate, alias.target, item.span, diagnostics, &empty_type_params);
            },
            .Const => |*const_item| try typecheckConst(crate, const_item, diagnostics),
            else => {},
        }
    }
}

// Type checks a function, including its parameters and body.
// Infers the return type if not explicitly specified.
fn typecheckFunction(crate: *hir.Crate, func: *hir.Function, diagnostics: *diag.Diagnostics) Error!void {
    var locals = std.AutoHashMap(hir.LocalId, hir.TypeId).init(crate.allocator());
    defer locals.deinit();

    var type_params = try TypeParamSet.init(crate.allocator(), func.type_params);
    defer type_params.deinit(crate.allocator());

    for (func.param_types) |param_ty| {
        try ensureKnownType(crate, param_ty, func.span, diagnostics, &type_params);
    }
    if (func.return_type) |ret| {
        try ensureKnownType(crate, ret, func.span, diagnostics, &type_params);
    }

    for (func.params, 0..) |local_id, idx| {
        const ty = if (idx < func.param_types.len)
            func.param_types[idx]
        else
            try ensureType(crate, .Unknown);
        try locals.put(local_id, ty);
    }

    if (func.body) |body_id| {
        const body_ty = try checkExpr(crate, body_id, diagnostics, &locals, false, &type_params);
        if (func.return_type) |ret_ty| {
            if (!typesCompatible(crate, ret_ty, body_ty, &type_params)) {
                diagnostics.reportError(func.span, "function return type does not match body");
            }
        } else {
            func.return_type = body_ty;
        }
    }
}

// Type checks a struct definition, ensuring all field types are known.
fn typecheckStruct(crate: *hir.Crate, structure: *hir.Struct, diagnostics: *diag.Diagnostics) Error!void {
    var type_params = try TypeParamSet.init(crate.allocator(), structure.type_params);
    defer type_params.deinit(crate.allocator());
    for (structure.fields) |field| {
        try ensureKnownType(crate, field.ty, field.span, diagnostics, &type_params);
    }
}

// Type checks a const definition, ensuring type matches value.
fn typecheckConst(crate: *hir.Crate, const_item: *hir.Const, diagnostics: *diag.Diagnostics) Error!void {
    var empty_locals = std.AutoHashMap(hir.LocalId, hir.TypeId).init(crate.allocator());
    defer empty_locals.deinit();
    const empty_type_params = TypeParamSet.empty();
    try ensureKnownType(crate, const_item.ty, const_item.span, diagnostics, &empty_type_params);
    const value_ty = try checkExpr(crate, const_item.value, diagnostics, &empty_locals, false, &empty_type_params);
    if (!typesCompatible(crate, const_item.ty, value_ty, &empty_type_params)) {
        diagnostics.reportError(const_item.span, "const type does not match initializer");
    }
}

// Validates that a type is known (not Unknown) and reports an error if not.
fn ensureKnownType(
    crate: *hir.Crate,
    ty: hir.TypeId,
    span: hir.Span,
    diagnostics: *diag.Diagnostics,
    type_params: *const TypeParamSet,
) Error!void {
    if (ty >= crate.types.items.len) {
        diagnostics.reportError(span, "unknown type reference");
        return;
    }

    const kind = crate.types.items[ty].kind;
    switch (kind) {
        .Unknown => diagnostics.reportError(span, "type could not be resolved"),
        .Struct => |info| {
            for (info.type_args) |arg| {
                try ensureKnownType(crate, arg, span, diagnostics, type_params);
            }
        },
        .Path => |path| {
            for (path.args) |arg| {
                try ensureKnownType(crate, arg, span, diagnostics, type_params);
            }
            if (path.segments.len == 1) {
                const name = path.segments[0];
                if (type_params.contains(name)) return;
                if (!isKnownTypeName(crate, name)) {
                    diagnostics.reportError(span, "undeclared type or type parameter");
                }
            }
        },
        else => {},
    }
}

// Recursively type checks an expression and returns its type.
// The in_unsafe flag indicates whether we're inside an unsafe block.
fn checkExpr(
    crate: *hir.Crate,
    expr_id: hir.ExprId,
    diagnostics: *diag.Diagnostics,
    locals: *std.AutoHashMap(hir.LocalId, hir.TypeId),
    in_unsafe: bool,
    type_params: *const TypeParamSet,
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
            // String literals in Rust have type &str, not String
            const str_ty = try ensureType(crate, .Str);
            expr.ty = try ensureType(crate, .{ .Ref = .{ .mutable = false, .inner = str_ty } });
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
                    .Const => |const_item| expr.ty = const_item.ty,
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
                try checkStmt(crate, stmt_id, diagnostics, locals, &last_ty, in_unsafe, type_params);
            }
            if (block.tail) |tail| {
                last_ty = try checkExpr(crate, tail, diagnostics, locals, in_unsafe, type_params);
            }
            expr.ty = last_ty orelse try ensureType(crate, .Unknown);
        },

        .Unsafe => |u| {
            // Inside the unsafe block body, we typecheck with in_unsafe = true.
            const body_ty = try checkExpr(crate, u.body, diagnostics, locals, true, type_params);
            expr.ty = body_ty;
        },

        .UnresolvedIdent => {
            diagnostics.reportError(span, "identifier was not resolved before type checking");
            expr.ty = try ensureType(crate, .Unknown);
        },
        .Cast => |c| {
            // Type check the inner expression first
            _ = try checkExpr(crate, c.expr, diagnostics, locals, in_unsafe, type_params);
            try ensureKnownType(crate, c.ty, span, diagnostics, type_params);
            expr.ty = c.ty;
        },
        .Binary => |bin| {
            const lhs_ty = try checkExpr(crate, bin.lhs, diagnostics, locals, in_unsafe, type_params);
            const rhs_ty = try checkExpr(crate, bin.rhs, diagnostics, locals, in_unsafe, type_params);

            switch (bin.op) {
                .LogicalOr, .LogicalAnd => {
                    if (!isBool(crate, lhs_ty) or !isBool(crate, rhs_ty)) {
                        diagnostics.reportError(span, "logical operators require boolean operands");
                    }
                    expr.ty = try ensureType(crate, .Bool);
                },
                .Eq, .Ne, .Lt, .Le, .Gt, .Ge => {
                    if (!typesCompatible(crate, lhs_ty, rhs_ty, type_params)) {
                        diagnostics.reportError(span, "comparison operands must have the same type");
                    }
                    expr.ty = try ensureType(crate, .Bool);
                },
                .Add, .Sub, .Mul, .Div, .Mod => {
                    if (!typesCompatible(crate, lhs_ty, rhs_ty, type_params)) {
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
            const operand_ty = try checkExpr(crate, un.expr, diagnostics, locals, in_unsafe, type_params);
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
            if (isArrayMethodCall(crate, call.callee, diagnostics, locals, in_unsafe, type_params)) |method_info| {
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
            } else if (isPointerMethodCall(crate, call.callee, diagnostics, locals, in_unsafe, type_params)) |method_info| {
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
            } else if (isStructMethodCall(crate, call.callee, diagnostics, locals, in_unsafe, type_params)) |method_info| {
                // Handle struct method calls (e.g., point.offset(1, 2))
                // Type check all arguments
                for (call.args) |arg_id| {
                    _ = try checkExpr(crate, arg_id, diagnostics, locals, in_unsafe, type_params);
                }
                // Set the return type from the method
                expr.ty = method_info.ret_ty;
            } else {
                const callee_ty = try checkExpr(crate, call.callee, diagnostics, locals, in_unsafe, type_params);
                var param_types: []hir.TypeId = &[_]hir.TypeId{};
                var ret_ty: hir.TypeId = try ensureType(crate, .Unknown);

                // Get the callee's type parameters for generic function calls
                const callee_type_params_slice = getCalleeTypeParams(crate, call.callee) orelse &[_][]const u8{};
                var callee_type_params = try TypeParamSet.init(crate.allocator(), callee_type_params_slice);
                defer callee_type_params.deinit(crate.allocator());

                if (isBuiltinPrintln(crate, call.callee)) {
                    for (call.args) |arg_id| {
                        _ = try checkExpr(crate, arg_id, diagnostics, locals, in_unsafe, type_params);
                    }
                    expr.ty = ret_ty;
                } else {
                    if (callee_ty < crate.types.items.len) {
                        switch (crate.types.items[callee_ty].kind) {
                            .Fn => |fn_ty| {
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
                        diagnostics.reportError(span, "argument count does not match function type");
                    }

                    for (call.args, 0..) |arg_id, idx| {
                        // Check if this arg is a lambda that needs type inference from expected param type
                        if (idx < param_types.len) {
                            const expected = param_types[idx];
                            if (expected < crate.types.items.len) {
                                const expected_kind = crate.types.items[expected].kind;
                                if (expected_kind == .Fn) {
                                    // This argument should be a function type - try to infer lambda params
                                    try inferLambdaTypes(crate, arg_id, expected_kind.Fn.params);
                                }
                            }
                        }

                        const arg_ty = try checkExpr(crate, arg_id, diagnostics, locals, in_unsafe, type_params);
                        if (idx < param_types.len) {
                            const expected = param_types[idx];
                            const expected_kind = if (expected < crate.types.items.len)
                                crate.types.items[expected].kind
                            else
                                .Unknown;
                            // Use callee's type parameters for compatibility check
                            if (expected_kind != .Fn and !typesCompatible(crate, arg_ty, expected, &callee_type_params)) {
                                diagnostics.reportError(span, "argument type does not match parameter");
                            }

                            // For generic functions: if the return type is a type parameter that
                            // matches this parameter's type, infer the return type from the argument
                            if (expected < crate.types.items.len and ret_ty < crate.types.items.len) {
                                const ret_kind = crate.types.items[ret_ty].kind;
                                if (ret_kind == .Path and expected_kind == .Path) {
                                    const ret_path = ret_kind.Path;
                                    const param_path = expected_kind.Path;
                                    // If return type and param type are the same type parameter (e.g., both "T")
                                    if (ret_path.segments.len == 1 and param_path.segments.len == 1 and
                                        std.mem.eql(u8, ret_path.segments[0], param_path.segments[0]))
                                    {
                                        // Use the concrete argument type as the return type
                                        ret_ty = arg_ty;
                                    }
                                }
                            }
                        }
                    }
                    expr.ty = ret_ty;
                }
            }
        },
        .MethodCall => |call| {
            _ = try checkExpr(crate, call.target, diagnostics, locals, in_unsafe, type_params);
            for (call.args) |arg_id| {
                _ = try checkExpr(crate, arg_id, diagnostics, locals, in_unsafe, type_params);
            }
            expr.ty = try ensureType(crate, .Unknown);
        },
        .Assignment => |assign| {
            const target_ty = try checkExpr(crate, assign.target, diagnostics, locals, in_unsafe, type_params);
            const value_ty = try checkExpr(crate, assign.value, diagnostics, locals, in_unsafe, type_params);

            if (!typesCompatible(crate, target_ty, value_ty, type_params)) {
                diagnostics.reportError(span, "assignment types do not match");
            }

            expr.ty = target_ty;
        },
        .Return => |ret| {
            if (ret) |value_id| {
                expr.ty = try checkExpr(crate, value_id, diagnostics, locals, in_unsafe, type_params);
            } else {
                expr.ty = try ensureType(crate, .Unknown);
            }
        },
        .If => |ifs| {
            const cond_ty = try checkExpr(crate, ifs.cond, diagnostics, locals, in_unsafe, type_params);
            if (!isBool(crate, cond_ty)) diagnostics.reportError(span, "if condition must be boolean");

            const then_ty = try checkExpr(crate, ifs.then_branch, diagnostics, locals, in_unsafe, type_params);
            if (ifs.else_branch) |else_id| {
                const else_ty = try checkExpr(crate, else_id, diagnostics, locals, in_unsafe, type_params);
                if (typesCompatible(crate, then_ty, else_ty, type_params)) {
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
            const cond_ty = try checkExpr(crate, wh.cond, diagnostics, locals, in_unsafe, type_params);
            if (!isBool(crate, cond_ty)) diagnostics.reportError(span, "while condition must be boolean");
            _ = try checkExpr(crate, wh.body, diagnostics, locals, in_unsafe, type_params);
            expr.ty = try ensureType(crate, .Unknown);
        },
        .For => |for_expr| {
            const iter_ty = try checkExpr(crate, for_expr.iter, diagnostics, locals, in_unsafe, type_params);

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
            if (!isUnknown(crate, pat_ty) and !typesCompatible(crate, pat_ty, elem_ty, type_params)) {
                diagnostics.reportError(span, "for pattern type does not match iterator");
            }
            try locals.put(for_expr.pat.id, elem_ty);
            _ = try checkExpr(crate, for_expr.body, diagnostics, locals, in_unsafe, type_params);
            expr.ty = try ensureType(crate, .Unknown);
        },
        .Range => |range| {
            const start_ty = try checkExpr(crate, range.start, diagnostics, locals, in_unsafe, type_params);
            const end_ty = try checkExpr(crate, range.end, diagnostics, locals, in_unsafe, type_params);
            if (!typesCompatible(crate, start_ty, end_ty, type_params)) {
                diagnostics.reportError(span, "range bounds must have the same type");
                expr.ty = try ensureType(crate, .Unknown);
            } else {
                expr.ty = start_ty;
            }
        },
        .Index => |index| {
            const target_ty = try checkExpr(crate, index.target, diagnostics, locals, in_unsafe, type_params);
            _ = try checkExpr(crate, index.index, diagnostics, locals, in_unsafe, type_params);
            expr.ty = switch (getArrayElementType(crate, target_ty)) {
                .ok => |elem_ty| elem_ty,
                .err => blk: {
                    diagnostics.reportError(span, "indexing requires an array type");
                    break :blk try ensureType(crate, .Unknown);
                },
            };
        },
        .Field => |field| {
            const target_ty = try checkExpr(crate, field.target, diagnostics, locals, in_unsafe, type_params);
            expr.ty = try resolveFieldType(crate, target_ty, field.name, span, diagnostics);
        },
        .Array => |elements| {
            var element_ty: ?hir.TypeId = null;
            for (elements) |elem_id| {
                const ty = try checkExpr(crate, elem_id, diagnostics, locals, in_unsafe, type_params);
                if (element_ty) |existing| {
                    if (!typesCompatible(crate, existing, ty, type_params)) {
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
                const unknown_ty = try ensureType(crate, .Unknown);
                const inferred_args = try crate.allocator().alloc(hir.TypeId, struct_item.type_params.len);
                @memset(inferred_args, unknown_ty);
                for (init.fields) |field_init| {
                    const value_ty = try checkExpr(crate, field_init.value, diagnostics, locals, in_unsafe, type_params);
                    const expected_ty = findFieldType(struct_item, field_init.name);
                    if (expected_ty) |ty| {
                        const expected_kind = if (ty < crate.types.items.len) crate.types.items[ty].kind else null;
                        if (expected_kind) |kind| {
                            if (kind == .Path) {
                                const path_info = kind.Path;
                                if (path_info.segments.len == 1) {
                                    if (findStructTypeParamIndex(struct_item, path_info.segments[0])) |param_idx| {
                                        const inferred = inferred_args[param_idx];
                                        if (isUnknown(crate, inferred)) {
                                            inferred_args[param_idx] = value_ty;
                                        } else if (!typesCompatible(crate, inferred, value_ty, type_params)) {
                                            diagnostics.reportError(span, "struct field type mismatch");
                                        }

                                        const compare_ty = inferred_args[param_idx];
                                        if (!isUnknown(crate, compare_ty) and !typesCompatible(crate, compare_ty, value_ty, type_params)) {
                                            diagnostics.reportError(span, "struct field type mismatch");
                                        }
                                        continue;
                                    }
                                }
                            }
                        }

                        if (!typesCompatible(crate, ty, value_ty, type_params)) {
                            diagnostics.reportError(span, "struct field type mismatch");
                        }
                    } else {
                        diagnostics.reportError(span, "unknown field in struct initializer");
                    }
                }

                expr.ty = try ensureType(crate, .{ .Struct = .{ .def_id = def_id, .type_args = inferred_args } });
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

            const body_ty = try checkExpr(crate, lambda.body, diagnostics, &lambda_locals, in_unsafe, type_params);
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

// Type checks a statement and updates the locals map with any new bindings.
fn checkStmt(
    crate: *hir.Crate,
    stmt_id: hir.StmtId,
    diagnostics: *diag.Diagnostics,
    locals: *std.AutoHashMap(hir.LocalId, hir.TypeId),
    last_ty: *?hir.TypeId,
    in_unsafe: bool,
    type_params: *const TypeParamSet,
) Error!void {
    const stmt = &crate.stmts.items[stmt_id];
    switch (stmt.kind) {
        .Let => |*let_stmt| {
            var declared_ty = let_stmt.ty;
            if (let_stmt.value) |value_id| {
                const value_ty = try checkExpr(crate, value_id, diagnostics, locals, in_unsafe, type_params);
                if (declared_ty) |*ty_id| {
                    try ensureKnownType(crate, ty_id.*, stmt.span, diagnostics, type_params);
                    if (!typesCompatible(crate, ty_id.*, value_ty, type_params)) {
                        diagnostics.reportError(stmt.span, "mismatched types in let binding");
                    }
                } else {
                    declared_ty = value_ty;
                    let_stmt.ty = value_ty;
                }
            } else if (declared_ty) |ty_id| {
                try ensureKnownType(crate, ty_id, stmt.span, diagnostics, type_params);
            }

            if (let_stmt.pat.kind == .Identifier) {
                try locals.put(let_stmt.pat.id, declared_ty orelse try ensureType(crate, .Unknown));
            }
            last_ty.* = declared_ty;
        },
        .Expr => |expr_id| {
            last_ty.* = try checkExpr(crate, expr_id, diagnostics, locals, in_unsafe, type_params);
        },
        .Unknown => {},
    }
}

/// Infer lambda parameter types from expected function type.
/// This allows unannotated lambda parameters like `|n| n * 2` to get their types
/// from the calling context when passed to a function expecting `fn(i32) -> i32`.
fn inferLambdaTypes(crate: *hir.Crate, expr_id: hir.ExprId, expected_fn_params: []hir.TypeId) Error!void {
    if (expr_id >= crate.exprs.items.len) return;
    const expr = &crate.exprs.items[expr_id];
    if (expr.kind != .Lambda) return;

    const lambda = &expr.kind.Lambda;

    // Update lambda param_types with expected types
    for (lambda.params, 0..) |_, idx| {
        if (idx < lambda.param_types.len and idx < expected_fn_params.len) {
            // If the param type is Unknown, replace with expected type
            const current_ty = lambda.param_types[idx];
            if (current_ty < crate.types.items.len and crate.types.items[current_ty].kind == .Unknown) {
                lambda.param_types[idx] = expected_fn_params[idx];
            }
        }
    }
}

// Ensures a pattern binding exists in the locals map, creating an Unknown type if needed.
fn ensurePatternBinding(crate: *hir.Crate, local_id: hir.LocalId, locals: *std.AutoHashMap(hir.LocalId, hir.TypeId)) Error!hir.TypeId {
    if (locals.get(local_id)) |ty| return ty;
    const ty = try ensureType(crate, .Unknown);
    try locals.put(local_id, ty);
    return ty;
}

// Finds or creates a type with the given kind in the type arena.
// Uses O(1) cache lookup for common primitive types, falls back to linear scan for complex types.
// Returns the existing TypeId if found, otherwise creates a new type.
fn ensureType(crate: *hir.Crate, kind: hir.Type.Kind) Error!hir.TypeId {
    // O(1) cache lookup for common primitive types
    switch (kind) {
        .PrimInt => |int_kind| {
            const cached = switch (int_kind) {
                .U32 => crate.type_cache.u32_ty,
                .U64 => crate.type_cache.u64_ty,
                .Usize => crate.type_cache.usize_ty,
                .I32 => crate.type_cache.i32_ty,
                .I64 => crate.type_cache.i64_ty,
            };
            if (cached) |ty| return ty;
        },
        .PrimFloat => |float_kind| {
            const cached = switch (float_kind) {
                .F32 => crate.type_cache.f32_ty,
                .F64 => crate.type_cache.f64_ty,
            };
            if (cached) |ty| return ty;
        },
        .Bool => if (crate.type_cache.bool_ty) |ty| return ty,
        .Char => if (crate.type_cache.char_ty) |ty| return ty,
        .String => if (crate.type_cache.string_ty) |ty| return ty,
        .Str => if (crate.type_cache.str_ty) |ty| return ty,
        .Unknown => if (crate.type_cache.unknown_ty) |ty| return ty,
        else => {},
    }

    // Fall back to linear scan for complex types (Array, Ref, Pointer, Fn, Struct, Path)
    for (crate.types.items) |existing| {
        if (typeKindsEqual(existing.kind, kind)) return existing.id;
    }

    // Create a new type and update cache if applicable
    const id: hir.TypeId = @intCast(crate.types.items.len);
    try crate.types.append(crate.allocator(), .{ .id = id, .kind = kind });

    // Update cache for primitive types
    switch (kind) {
        .PrimInt => |int_kind| switch (int_kind) {
            .U32 => crate.type_cache.u32_ty = id,
            .U64 => crate.type_cache.u64_ty = id,
            .Usize => crate.type_cache.usize_ty = id,
            .I32 => crate.type_cache.i32_ty = id,
            .I64 => crate.type_cache.i64_ty = id,
        },
        .PrimFloat => |float_kind| switch (float_kind) {
            .F32 => crate.type_cache.f32_ty = id,
            .F64 => crate.type_cache.f64_ty = id,
        },
        .Bool => crate.type_cache.bool_ty = id,
        .Char => crate.type_cache.char_ty = id,
        .String => crate.type_cache.string_ty = id,
        .Str => crate.type_cache.str_ty = id,
        .Unknown => crate.type_cache.unknown_ty = id,
        else => {},
    }

    return id;
}

/// Compares two Type.Kind values for structural equality, comparing slices by content.
fn typeKindsEqual(a: hir.Type.Kind, b: hir.Type.Kind) bool {
    const TagType = @typeInfo(hir.Type.Kind).@"union".tag_type.?;
    const a_tag: TagType = a;
    const b_tag: TagType = b;
    if (a_tag != b_tag) return false;

    return switch (a) {
        .Struct => |a_struct| {
            const b_struct = b.Struct;
            return a_struct.def_id == b_struct.def_id and
                std.mem.eql(hir.TypeId, a_struct.type_args, b_struct.type_args);
        },
        .Array => |a_arr| {
            const b_arr = b.Array;
            return a_arr.elem == b_arr.elem and a_arr.size_const == b_arr.size_const;
        },
        .Ref => |a_ref| {
            const b_ref = b.Ref;
            return a_ref.mutable == b_ref.mutable and a_ref.inner == b_ref.inner;
        },
        .Pointer => |a_ptr| {
            const b_ptr = b.Pointer;
            return a_ptr.mutable == b_ptr.mutable and a_ptr.inner == b_ptr.inner;
        },
        .Fn => |a_fn| {
            const b_fn = b.Fn;
            return std.mem.eql(hir.TypeId, a_fn.params, b_fn.params) and a_fn.ret == b_fn.ret;
        },
        .Path => |a_path| {
            const b_path = b.Path;
            if (a_path.segments.len != b_path.segments.len) return false;
            for (a_path.segments, b_path.segments) |a_seg, b_seg| {
                if (!std.mem.eql(u8, a_seg, b_seg)) return false;
            }
            return std.mem.eql(hir.TypeId, a_path.args, b_path.args);
        },
        else => std.meta.eql(a, b),
    };
}


// Checks if a type is Unknown (unresolved or error type).
fn isUnknown(crate: *hir.Crate, ty: hir.TypeId) bool {
    return ty >= crate.types.items.len or crate.types.items[ty].kind == .Unknown;
}

// Checks if two types are structurally equal.
fn typesEqual(crate: *hir.Crate, lhs: hir.TypeId, rhs: hir.TypeId) bool {
    if (lhs == rhs) return true;
    if (lhs >= crate.types.items.len or rhs >= crate.types.items.len) return false;
    return std.meta.eql(crate.types.items[lhs].kind, crate.types.items[rhs].kind);
}

/// Resolves a type through type aliases. If the type is a Path that refers to a type alias,
/// returns the underlying type. Otherwise returns the original type.
/// Uses O(1) HashMap lookups for struct and type alias resolution.
fn resolveType(crate: *hir.Crate, ty: hir.TypeId) hir.TypeId {
    if (ty >= crate.types.items.len) return ty;

    const kind = crate.types.items[ty].kind;
    switch (kind) {
        .Path => |path| {
            // Look up the type by name (could be a type alias or struct)
            if (path.segments.len == 1) {
                const name = path.segments[0];

                // Check for type alias first using O(1) lookup
                // getTypeAliasDef is reliable - it only returns DefIds for TypeAlias items
                if (crate.getTypeAliasDef(name)) |def_id| {
                    const alias = crate.items.items[def_id].kind.TypeAlias;
                    // Recursively resolve in case of chained aliases
                    return resolveType(crate, alias.target);
                }

                // Check for struct using O(1) lookup
                if (crate.getStructDef(name)) |def_id| {
                    // Find or create a Struct type for this def_id and argument list
                    for (crate.types.items) |existing| {
                        if (existing.kind == .Struct and existing.kind.Struct.def_id == def_id and std.mem.eql(hir.TypeId, existing.kind.Struct.type_args, path.args)) {
                            return existing.id;
                        }
                    }

                    // Create a monomorphized struct type with the concrete arguments seen on the path
                    var copied_args = std.ArrayListUnmanaged(hir.TypeId){};
                    defer copied_args.deinit(crate.allocator());
                    for (path.args) |arg| {
                        copied_args.append(crate.allocator(), arg) catch return ty;
                    }
                    const owned_args = copied_args.toOwnedSlice(crate.allocator()) catch return ty;

                    const struct_ty_id = crate.types.items.len;
                    crate.types.appendAssumeCapacity(.{ .id = @intCast(struct_ty_id), .kind = .{ .Struct = .{ .def_id = def_id, .type_args = owned_args } } });
                    return @intCast(struct_ty_id);
                }
            }
            return ty;
        },
        else => return ty,
    }
}

// Checks if two types are compatible for assignment or comparison.
// Resolves type aliases and allows coercion between compatible types.
fn typesCompatible(crate: *hir.Crate, lhs: hir.TypeId, rhs: hir.TypeId, type_params: *const TypeParamSet) bool {
    // Resolve type aliases before comparing
    const resolved_lhs = resolveType(crate, lhs);
    const resolved_rhs = resolveType(crate, rhs);

    if (typesEqual(crate, resolved_lhs, resolved_rhs)) return true;
    if (resolved_lhs >= crate.types.items.len or resolved_rhs >= crate.types.items.len) return false;

    if (isUnknown(crate, resolved_lhs) or isUnknown(crate, resolved_rhs)) return true;

    const lhs_kind = crate.types.items[resolved_lhs].kind;
    const rhs_kind = crate.types.items[resolved_rhs].kind;

    // Check if rhs is a declared type parameter. If so, it's compatible with any type (for generic type inference)
    if (rhs_kind == .Path) {
        const rhs_path = rhs_kind.Path;
        if (rhs_path.segments.len == 1 and type_params.contains(rhs_path.segments[0])) {
            return true;
        }
    }

    return switch (lhs_kind) {
        .PrimInt => rhs_kind == .PrimInt,
        .PrimFloat => rhs_kind == .PrimFloat,
        .Fn => switch (rhs_kind) {
            .Fn => true,
            else => false,
        },
        .Pointer => |lhs_ptr| switch (rhs_kind) {
            .Pointer => |rhs_ptr| lhs_ptr.mutable == rhs_ptr.mutable and typesCompatible(crate, lhs_ptr.inner, rhs_ptr.inner, type_params),
            // Allow Ref to Pointer coercion (like Rust's implicit conversion)
            .Ref => |rhs_ref| !lhs_ptr.mutable or rhs_ref.mutable and typesCompatible(crate, lhs_ptr.inner, rhs_ref.inner, type_params),
            else => false,
        },
        .Ref => |lhs_ref| switch (rhs_kind) {
            .Ref => |rhs_ref| lhs_ref.mutable == rhs_ref.mutable and typesCompatible(crate, lhs_ref.inner, rhs_ref.inner, type_params),
            // Allow &str to be compatible with String (simplified for this compiler)
            .String => isStr(crate, lhs_ref.inner),
            else => false,
        },
        // Allow String to be compatible with &str (simplified for this compiler)
        .String => switch (rhs_kind) {
            .String => true,
            .Ref => |rhs_ref| isStr(crate, rhs_ref.inner),
            else => false,
        },
        // Allow str to be compatible with String
        .Str => rhs_kind == .Str or rhs_kind == .String,
        .Struct => |lhs_struct| switch (rhs_kind) {
            .Struct => |rhs_struct| lhs_struct.def_id == rhs_struct.def_id,
            .Path => |path| structMatchesPath(crate, lhs_struct.def_id, path.segments),
            else => false,
        },
        .Path => |lhs_path| switch (rhs_kind) {
            .Struct => |rhs_struct| structMatchesPath(crate, rhs_struct.def_id, lhs_path.segments),
            .Path => |rhs_path| pathsMatch(lhs_path.segments, rhs_path.segments),
            else => lhs_path.segments.len == 1 and type_params.contains(lhs_path.segments[0]),
        },

        .Array => |lhs_arr| switch (rhs_kind) {
            .Array => |rhs_arr| blk: {
                if (lhs_arr.size_const != rhs_arr.size_const) break :blk false;

                break :blk typesCompatible(crate, lhs_arr.elem, rhs_arr.elem, type_params);
            },
            else => false,
        },

        else => false,
    };
}

// Checks if a type is the boolean type.
fn isBool(crate: *hir.Crate, ty: hir.TypeId) bool {
    const resolved = resolveType(crate, ty);
    return resolved < crate.types.items.len and crate.types.items[resolved].kind == .Bool;
}

// Checks if a type is the str type.
fn isStr(crate: *hir.Crate, ty: hir.TypeId) bool {
    const resolved = resolveType(crate, ty);
    return resolved < crate.types.items.len and crate.types.items[resolved].kind == .Str;
}

// Checks if a type is numeric (integer or floating-point).
fn isNumeric(crate: *hir.Crate, ty: hir.TypeId) bool {
    const resolved = resolveType(crate, ty);
    if (resolved >= crate.types.items.len) return false;
    return switch (crate.types.items[resolved].kind) {
        .PrimInt, .PrimFloat => true,
        else => false,
    };
}

// Checks if a struct definition matches a path (single-segment paths only).
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

// Checks if two paths are equal (same segments in the same order).
fn pathsMatch(lhs_segments: [][]const u8, rhs_segments: [][]const u8) bool {
    if (lhs_segments.len != rhs_segments.len) return false;
    for (lhs_segments, rhs_segments) |l, r| {
        if (!std.mem.eql(u8, l, r)) return false;
    }
    return true;
}

// Checks if a name refers to a known type (struct or type alias).
// Uses O(1) HashMap lookup if index is available.
fn isKnownTypeName(crate: *hir.Crate, type_name: []const u8) bool {
    // Use O(1) lookup via name indexes
    if (crate.getStructDef(type_name) != null) return true;
    if (crate.getTypeAliasDef(type_name) != null) return true;
    return false;
}

// Extracts the element type from an array type (or ref/pointer to array).
// Returns an error variant if the type is not an array.
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

// Finds a struct definition by its path.
// Uses O(1) HashMap lookup if index is available.
fn findStructDef(crate: *hir.Crate, path: [][]const u8) ?hir.DefId {
    if (path.len == 0) return null;
    const name = path[path.len - 1];
    // Use O(1) lookup via name index
    return crate.getStructDef(name);
}

// Finds the type of a field in a struct by name.
fn findFieldType(structure: hir.Struct, name: []const u8) ?hir.TypeId {
    for (structure.fields) |field| {
        if (std.mem.eql(u8, field.name, name)) return field.ty;
    }
    return null;
}

// Returns the index of a type parameter on a struct definition.
fn findStructTypeParamIndex(structure: hir.Struct, type_name: []const u8) ?usize {
    for (structure.type_params, 0..) |param, idx| {
        if (std.mem.eql(u8, param, type_name)) return idx;
    }
    return null;
}

fn resolveStructFieldType(
    crate: *hir.Crate,
    structure: hir.Struct,
    type_args: []const hir.TypeId,
    field_ty: hir.TypeId,
) Error!hir.TypeId {
    if (field_ty >= crate.types.items.len) return ensureType(crate, .Unknown);
    const field_kind = crate.types.items[field_ty].kind;
    if (field_kind == .Path) {
        const path_info = field_kind.Path;
        if (path_info.segments.len == 1) {
            if (findStructTypeParamIndex(structure, path_info.segments[0])) |param_idx| {
                if (param_idx < type_args.len) {
                    const arg_ty = type_args[param_idx];
                    if (!isUnknown(crate, arg_ty)) return arg_ty;
                }
                return ensureType(crate, .Unknown);
            }
        }
    }
    return field_ty;
}

// Resolves the type of a field access expression.
// First checks for struct fields, then for methods in impl blocks.
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
                        const resolved = try resolveStructFieldType(crate, item.kind.Struct, info.type_args, field_ty);
                        return resolved;
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
            // Try to find the struct this path refers to using O(1) lookup
            if (path.segments.len == 1) {
                const struct_name = path.segments[0];
                if (crate.getStructDef(struct_name)) |def_id| {
                    if (def_id < crate.items.items.len) {
                        const item = crate.items.items[def_id];
                        if (item.kind == .Struct) {
                            const struct_item = item.kind.Struct;
                            // Found the struct - check for field
                            if (findFieldType(struct_item, name)) |field_ty| {
                                const resolved_type_args = if (path.args.len == struct_item.type_params.len)
                                    path.args
                                else
                                    &[_]hir.TypeId{};
                                const resolved = try resolveStructFieldType(crate, struct_item, resolved_type_args, field_ty);
                                return resolved;
                            }

                            // Check for methods
                            if (findMethodType(crate, def_id, name)) |method_ty| {
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
        // Reference types (e.g., &self, &mut self) - dereference and recurse
        .Ref => |ref_info| {
            // Dereference the reference and check the inner type
            return resolveFieldType(crate, ref_info.inner, name, span, diagnostics);
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

// Finds the return type of a method in an impl block for a given struct.
// Methods are stored with mangled names (StructName_methodName).
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

// Checks if a callee expression refers to the builtin println function.
fn isBuiltinPrintln(crate: *hir.Crate, callee_id: hir.ExprId) bool {
    if (callee_id >= crate.exprs.items.len) return false;
    const expr = crate.exprs.items[callee_id];
    if (expr.kind != .GlobalRef) return false;
    const def_id = expr.kind.GlobalRef;
    if (def_id >= crate.items.items.len) return false;
    const item = crate.items.items[def_id];
    return item.kind == .Function and std.mem.eql(u8, item.kind.Function.name, "println");
}

/// Gets the type parameters of a callee function from a GlobalRef expression.
fn getCalleeTypeParams(crate: *hir.Crate, callee_id: hir.ExprId) ?[]const []const u8 {
    if (callee_id >= crate.exprs.items.len) return null;
    const expr = crate.exprs.items[callee_id];
    if (expr.kind != .GlobalRef) return null;
    const def_id = expr.kind.GlobalRef;
    if (def_id >= crate.items.items.len) return null;
    const item = crate.items.items[def_id];
    if (item.kind == .Function) {
        return item.kind.Function.type_params;
    }
    return null;
}

/// Helper struct for pointer method call detection.
const PointerMethodInfo = struct {
    target_ty: hir.TypeId,
    method_name: []const u8,
};

// Detects if a call expression is a pointer method call (e.g., ptr.is_null()).
fn isPointerMethodCall(
    crate: *hir.Crate,
    callee_id: hir.ExprId,
    diagnostics: *diag.Diagnostics,
    locals: *std.AutoHashMap(hir.LocalId, hir.TypeId),
    in_unsafe: bool,
    type_params: *const TypeParamSet,
) ?PointerMethodInfo {
    if (callee_id >= crate.exprs.items.len) return null;
    const callee_expr = crate.exprs.items[callee_id];

    // Check if the callee is a Field expression
    if (callee_expr.kind != .Field) return null;

    const field = callee_expr.kind.Field;

    // Check if the target is a pointer type
    const target_ty = checkExpr(crate, field.target, diagnostics, locals, in_unsafe, type_params) catch return null;
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

/// Helper struct for array method call detection.
const ArrayMethodInfo = struct {
    target_ty: hir.TypeId,
    method_name: []const u8,
};

// Detects if a call expression is an array method call (e.g., arr.len()).
fn isArrayMethodCall(
    crate: *hir.Crate,
    callee_id: hir.ExprId,
    diagnostics: *diag.Diagnostics,
    locals: *std.AutoHashMap(hir.LocalId, hir.TypeId),
    in_unsafe: bool,
    type_params: *const TypeParamSet,
) ?ArrayMethodInfo {
    if (callee_id >= crate.exprs.items.len) return null;
    const callee_expr = crate.exprs.items[callee_id];

    // Check if the callee is a Field expression
    if (callee_expr.kind != .Field) return null;

    const field = callee_expr.kind.Field;

    // Check if the target is an array type (or ref/pointer to array)
    const target_ty = checkExpr(crate, field.target, diagnostics, locals, in_unsafe, type_params) catch return null;
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

/// Helper struct for struct method call detection.
const StructMethodInfo = struct {
    target_ty: hir.TypeId,
    method_name: []const u8,
    ret_ty: hir.TypeId,
};

// Detects if a call expression is a struct method call (e.g., point.offset(1, 2)).
fn isStructMethodCall(
    crate: *hir.Crate,
    callee_id: hir.ExprId,
    diagnostics: *diag.Diagnostics,
    locals: *std.AutoHashMap(hir.LocalId, hir.TypeId),
    in_unsafe: bool,
    type_params: *const TypeParamSet,
) ?StructMethodInfo {
    if (callee_id >= crate.exprs.items.len) return null;
    const callee_expr = crate.exprs.items[callee_id];

    // Check if the callee is a Field expression
    if (callee_expr.kind != .Field) return null;

    const field = callee_expr.kind.Field;

    // Check if the target is a struct type
    const target_ty = checkExpr(crate, field.target, diagnostics, locals, in_unsafe, type_params) catch return null;
    const resolved_ty = resolveType(crate, target_ty);
    if (resolved_ty >= crate.types.items.len) return null;

    // Extract the struct def_id from the type (handling both direct structs and references to structs)
    const struct_def_id: ?hir.DefId = switch (crate.types.items[resolved_ty].kind) {
        .Struct => |info| info.def_id,
        .Ref => |ref_info| blk: {
            // Dereference the ref to get the inner type
            if (ref_info.inner < crate.types.items.len) {
                const inner_ty = crate.types.items[ref_info.inner];
                if (inner_ty.kind == .Struct) {
                    break :blk inner_ty.kind.Struct.def_id;
                }
            }
            break :blk null;
        },
        else => null,
    };

    if (struct_def_id == null) return null;
    const info_def_id = struct_def_id.?;

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
                        matches = impl_info.def_id == info_def_id;
                    },
                    .Path => |path| {
                        if (path.segments.len == 1) {
                            const struct_item = crate.items.items[info_def_id];
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

    try crate.items.append(crate.allocator(), .{ .id = 0, .kind = .{ .Function = .{ .def_id = 0, .name = "main", .type_params = &[_][]const u8{}, .params = &[_]hir.LocalId{}, .param_types = &[_]hir.TypeId{}, .return_type = bool_ty, .body = block_expr_id, .span = span } }, .span = span });

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

    try crate.items.append(crate.allocator(), .{ .id = 0, .kind = .{ .Struct = .{ .def_id = 0, .name = "Wrapper", .type_params = &[_][]const u8{}, .fields = struct_fields, .span = span } }, .span = span });
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
    const empty_type_params = TypeParamSet.empty();

    try std.testing.expectEqual(i64_ty, try checkExpr(&crate, binary_expr_id, &diagnostics, &locals, false, &empty_type_params));
    try std.testing.expectEqual(bool_ty, try checkExpr(&crate, unary_expr_id, &diagnostics, &locals, false, &empty_type_params));
    const expected_arr_ty = try ensureType(&crate, .{ .Array = .{ .elem = i64_ty, .size_const = 2 } });
    try std.testing.expectEqual(expected_arr_ty, try checkExpr(&crate, array_expr_id, &diagnostics, &locals, false, &empty_type_params));
    try std.testing.expectEqual(struct_ty, try checkExpr(&crate, struct_init_expr_id, &diagnostics, &locals, false, &empty_type_params));
    try std.testing.expectEqual(i64_ty, try checkExpr(&crate, field_expr_id, &diagnostics, &locals, false, &empty_type_params));
    try std.testing.expect(!diagnostics.hasErrors());
}

test "typechecker infers generic struct field types from init values" {
    const allocator = std.testing.allocator;
    var diagnostics = diag.Diagnostics.init(allocator);
    defer diagnostics.deinit();

    var crate = hir.Crate.init(allocator);
    defer crate.deinit();

    const span = hir.emptySpan(0);

    const bool_ty = try ensureType(&crate, .Bool);
    const type_params = try crate.allocator().alloc([]const u8, 1);
    type_params[0] = "T";

    const t_segments = try crate.allocator().alloc([]const u8, 1);
    t_segments[0] = "T";
    const generic_ty = try ensureType(&crate, .{ .Path = .{ .segments = t_segments, .args = &[_]hir.TypeId{} } });

    const struct_fields = try crate.allocator().alloc(hir.Field, 1);
    struct_fields[0] = .{ .name = "value", .ty = generic_ty, .span = span };

    try crate.items.append(crate.allocator(), .{ .id = 0, .kind = .{ .Struct = .{ .def_id = 0, .name = "Boxed", .type_params = type_params, .fields = struct_fields, .span = span } }, .span = span });

    const bool_expr_id: hir.ExprId = @intCast(crate.exprs.items.len);
    try crate.exprs.append(crate.allocator(), .{ .id = bool_expr_id, .kind = .{ .ConstBool = true }, .ty = 0, .span = span });

    const struct_fields_init = try crate.allocator().alloc(hir.StructInitField, 1);
    struct_fields_init[0] = .{ .name = "value", .value = bool_expr_id };
    const path_segments = try crate.allocator().alloc([]const u8, 1);
    path_segments[0] = "Boxed";
    const struct_init_expr_id: hir.ExprId = @intCast(crate.exprs.items.len);
    try crate.exprs.append(crate.allocator(), .{ .id = struct_init_expr_id, .kind = .{ .StructInit = .{ .path = path_segments, .fields = struct_fields_init } }, .ty = 0, .span = span });

    const field_expr_id: hir.ExprId = @intCast(crate.exprs.items.len);
    try crate.exprs.append(crate.allocator(), .{ .id = field_expr_id, .kind = .{ .Field = .{ .target = struct_init_expr_id, .name = "value" } }, .ty = 0, .span = span });

    var locals = std.AutoHashMap(hir.LocalId, hir.TypeId).init(crate.allocator());
    defer locals.deinit();

    const empty_type_params = TypeParamSet.empty();

    const struct_ty = try checkExpr(&crate, struct_init_expr_id, &diagnostics, &locals, false, &empty_type_params);
    try std.testing.expectEqual(bool_ty, try checkExpr(&crate, field_expr_id, &diagnostics, &locals, false, &empty_type_params));

    try std.testing.expect(!diagnostics.hasErrors());

    const struct_kind = crate.types.items[struct_ty].kind;
    try std.testing.expect(struct_kind == .Struct);
    try std.testing.expectEqual(@as(usize, 1), struct_kind.Struct.type_args.len);
    try std.testing.expectEqual(bool_ty, struct_kind.Struct.type_args[0]);
}

test "typechecker reports conflicts when generic fields infer different types" {
    const allocator = std.testing.allocator;
    var diagnostics = diag.Diagnostics.init(allocator);
    defer diagnostics.deinit();

    var crate = hir.Crate.init(allocator);
    defer crate.deinit();

    const span = hir.emptySpan(0);

    const bool_ty = try ensureType(&crate, .Bool);
    const i64_ty = try ensureType(&crate, .{ .PrimInt = .I64 });

    const type_params = try crate.allocator().alloc([]const u8, 1);
    type_params[0] = "T";

    const t_segments = try crate.allocator().alloc([]const u8, 1);
    t_segments[0] = "T";
    const generic_ty = try ensureType(&crate, .{ .Path = .{ .segments = t_segments, .args = &[_]hir.TypeId{} } });

    const struct_fields = try crate.allocator().alloc(hir.Field, 2);
    struct_fields[0] = .{ .name = "first", .ty = generic_ty, .span = span };
    struct_fields[1] = .{ .name = "second", .ty = generic_ty, .span = span };

    try crate.items.append(crate.allocator(), .{ .id = 0, .kind = .{ .Struct = .{ .def_id = 0, .name = "Pair", .type_params = type_params, .fields = struct_fields, .span = span } }, .span = span });

    const int_expr_id: hir.ExprId = @intCast(crate.exprs.items.len);
    try crate.exprs.append(crate.allocator(), .{ .id = int_expr_id, .kind = .{ .ConstInt = 1 }, .ty = 0, .span = span });

    const bool_expr_id: hir.ExprId = @intCast(crate.exprs.items.len);
    try crate.exprs.append(crate.allocator(), .{ .id = bool_expr_id, .kind = .{ .ConstBool = true }, .ty = 0, .span = span });

    const struct_fields_init = try crate.allocator().alloc(hir.StructInitField, 2);
    struct_fields_init[0] = .{ .name = "first", .value = int_expr_id };
    struct_fields_init[1] = .{ .name = "second", .value = bool_expr_id };
    const path_segments = try crate.allocator().alloc([]const u8, 1);
    path_segments[0] = "Pair";

    const struct_init_expr_id: hir.ExprId = @intCast(crate.exprs.items.len);
    try crate.exprs.append(crate.allocator(), .{ .id = struct_init_expr_id, .kind = .{ .StructInit = .{ .path = path_segments, .fields = struct_fields_init } }, .ty = 0, .span = span });

    var locals = std.AutoHashMap(hir.LocalId, hir.TypeId).init(crate.allocator());
    defer locals.deinit();

    const empty_type_params = TypeParamSet.empty();

    _ = try checkExpr(&crate, struct_init_expr_id, &diagnostics, &locals, false, &empty_type_params);

    try std.testing.expect(diagnostics.hasErrors());

    const struct_kind = crate.types.items[crate.exprs.items[struct_init_expr_id].ty].kind;
    try std.testing.expect(struct_kind == .Struct);
    try std.testing.expectEqual(@as(usize, 1), struct_kind.Struct.type_args.len);
    try std.testing.expectEqual(i64_ty, struct_kind.Struct.type_args[0]);
    try std.testing.expect(!typesCompatible(&crate, struct_kind.Struct.type_args[0], bool_ty, &empty_type_params));
}

test "typechecker reports undeclared type parameters" {
    const allocator = std.testing.allocator;
    var diagnostics = diag.Diagnostics.init(allocator);
    defer diagnostics.deinit();

    var crate = hir.Crate.init(allocator);
    defer crate.deinit();

    const span = hir.emptySpan(0);

    const type_params = try crate.allocator().alloc([]const u8, 1);
    type_params[0] = "T";

    const u_segments = try crate.allocator().alloc([]const u8, 1);
    u_segments[0] = "U";
    const unknown_ty = try ensureType(&crate, .{ .Path = .{ .segments = u_segments, .args = &[_]hir.TypeId{} } });

    const struct_fields = try crate.allocator().alloc(hir.Field, 1);
    struct_fields[0] = .{ .name = "value", .ty = unknown_ty, .span = span };

    try crate.items.append(crate.allocator(), .{ .id = 0, .kind = .{ .Struct = .{ .def_id = 0, .name = "Holder", .type_params = type_params, .fields = struct_fields, .span = span } }, .span = span });

    try typecheck(&crate, &diagnostics);
    try std.testing.expect(diagnostics.hasErrors());
}

test "typesCompatible only treats declared params as generic" {
    const allocator = std.testing.allocator;
    var crate = hir.Crate.init(allocator);
    defer crate.deinit();

    const t_segments = try crate.allocator().alloc([]const u8, 1);
    t_segments[0] = "T";
    const t_ty = try ensureType(&crate, .{ .Path = .{ .segments = t_segments, .args = &[_]hir.TypeId{} } });
    const bool_ty = try ensureType(&crate, .Bool);

    const params = try crate.allocator().alloc([]const u8, 1);
    params[0] = "T";
    var declared = try TypeParamSet.init(crate.allocator(), params);
    defer declared.deinit(crate.allocator());

    const empty_type_params = TypeParamSet.empty();

    try std.testing.expect(!typesCompatible(&crate, t_ty, bool_ty, &empty_type_params));
    try std.testing.expect(typesCompatible(&crate, t_ty, bool_ty, &declared));
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

    const empty_type_params = TypeParamSet.empty();

    _ = try checkExpr(&crate, bad_binary_expr_id, &diagnostics, &locals, false, &empty_type_params);
    _ = try checkExpr(&crate, array_expr_id, &diagnostics, &locals, false, &empty_type_params);

    try std.testing.expect(diagnostics.hasErrors());
}
