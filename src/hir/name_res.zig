//! Name resolution module for the HIR.
//!
//! This module performs name resolution on the HIR, converting unresolved
//! identifiers to either local references (`LocalRef`) or global references
//! (`GlobalRef`). It builds symbol tables for module-level and function-level
//! scopes and reports errors for unresolved or duplicate names.
//!
//! ## Resolution Process
//!
//! 1. Build module-level symbol table with all top-level items (functions, structs, type aliases).
//! 2. For each function, build a local symbol table with parameters.
//! 3. Traverse the function body, resolving identifiers and binding let patterns.
//! 4. Report diagnostics for unresolved names or duplicate definitions.
//!
//! ## Scoping Rules
//!
//! - Function parameters are in scope for the entire function body.
//! - Let bindings shadow earlier bindings with the same name (except within the same scope).
//! - Lambdas get a copy of the enclosing scope's locals.

const std = @import("std");
const diag = @import("../diag/diagnostics.zig");
const hir = @import("hir.zig");

/// Error type for name resolution operations (currently only allocation errors).
const Error = error{OutOfMemory};

/// Resolves all names in the given HIR crate.
///
/// This function populates the module-level symbol table with all top-level items,
/// then resolves identifiers in each function body to local or global references.
///
/// ## Parameters
/// - `crate`: The HIR crate to perform name resolution on (modified in place).
/// - `diagnostics`: Diagnostics collector for error reporting.
pub fn resolve(crate: *hir.Crate, diagnostics: *diag.Diagnostics) Error!void {
    const println_def = try ensureBuiltinPrintln(crate);

    var module_symbols = std.StringHashMap(hir.DefId).init(crate.allocator());
    defer module_symbols.deinit();

    try module_symbols.put(println_def.name, println_def.def_id);

    // Populate the module-level symbol table and catch duplicate items.
    for (crate.items.items) |item| {
        switch (item.kind) {
            .Function => |func| {
                if (func.def_id == println_def.def_id and func.body == null) continue;
                try insertGlobal(&module_symbols, func.name, func.def_id, item.span, diagnostics);
            },
            .Struct => |structure| try insertGlobal(&module_symbols, structure.name, structure.def_id, item.span, diagnostics),
            .TypeAlias => |alias| try insertGlobal(&module_symbols, alias.name, alias.def_id, item.span, diagnostics),
            else => {},
        }
    }

    for (crate.items.items) |*item| {
        switch (item.kind) {
            .Function => |*func| {
                var locals = std.StringHashMap(hir.LocalId).init(crate.allocator());
                defer locals.deinit();

                var next_local: hir.LocalId = 0;
                for (func.params, 0..) |param_pat_id, idx| {
                    if (param_pat_id >= crate.patterns.items.len) {
                        diagnostics.reportError(func.span, "function parameter references missing pattern");
                        continue;
                    }

                    const pat = &crate.patterns.items[param_pat_id];
                    try bindPattern(pat, &locals, &next_local, diagnostics, null);
                    func.params[idx] = pat.id;
                }

                if (func.body) |body_id| {
                    try resolveExpr(crate, body_id, &module_symbols, &locals, &next_local, diagnostics);
                }
            },
            else => {},
        }
    }
}

/// Helper struct to hold builtin function info.
const Builtin = struct { name: []const u8, def_id: hir.DefId };

// Ensures the builtin println function exists in the crate.
// If not present, adds a synthetic println function declaration.
fn ensureBuiltinPrintln(crate: *hir.Crate) Error!Builtin {
    for (crate.items.items) |item| {
        if (item.kind == .Function and std.mem.eql(u8, item.kind.Function.name, "println")) {
            return .{ .name = item.kind.Function.name, .def_id = item.id };
        }
    }

    const id: hir.DefId = @intCast(crate.items.items.len);
    const name = try crate.allocator().dupe(u8, "println");
    const span = hir.emptySpan(0);
    const func: hir.Function = .{ .def_id = id, .name = name, .params = &[_]hir.LocalId{}, .param_types = &[_]hir.TypeId{}, .return_type = null, .body = null, .span = span };
    try crate.items.append(crate.allocator(), .{ .id = id, .kind = .{ .Function = func }, .span = span });
    return .{ .name = name, .def_id = id };
}

// Inserts a global symbol into the module symbol table.
// Reports an error if a symbol with the same name already exists.
fn insertGlobal(
    module_symbols: *std.StringHashMap(hir.DefId),
    name: []const u8,
    def_id: hir.DefId,
    span: hir.Span,
    diagnostics: *diag.Diagnostics,
) Error!void {
    if (try module_symbols.fetchPut(name, def_id)) |existing| {
        const message = std.fmt.allocPrint(
            diagnostics.allocator,
            "duplicate definition of `{s}`; previously defined with def_id {d}",
            .{ name, existing.value },
        ) catch name;
        diagnostics.reportError(span, message);
        if (message.ptr != name.ptr) diagnostics.allocator.free(message);
        return;
    }

    try module_symbols.put(name, def_id);
}

// Binds a pattern to the local scope.
// For identifier patterns, adds the name to the locals table.
// For wildcard patterns, just allocates a local ID without adding to scope.
// Reports duplicate binding errors if the name is already in scope.
fn bindPattern(
    pat: *hir.Pattern,
    locals: *std.StringHashMap(hir.LocalId),
    next_local: *hir.LocalId,
    diagnostics: *diag.Diagnostics,
    scope_start: ?hir.LocalId,
) Error!void {
    switch (pat.kind) {
        .Identifier => |name| {
            if (locals.get(name)) |existing| {
                if (scope_start) |start| {
                    if (existing >= start) {
                        const message = std.fmt.allocPrint(
                            diagnostics.allocator,
                            "duplicate local binding `{s}` previously defined as local {d}",
                            .{ name, existing },
                        ) catch name;
                        diagnostics.reportError(pat.span, message);
                        if (message.ptr != name.ptr) diagnostics.allocator.free(message);
                        return;
                    }
                } else {
                    const message = std.fmt.allocPrint(
                        diagnostics.allocator,
                        "duplicate local binding `{s}` previously defined as local {d}",
                        .{ name, existing },
                    ) catch name;
                    diagnostics.reportError(pat.span, message);
                    if (message.ptr != name.ptr) diagnostics.allocator.free(message);
                    return;
                }
            }

            try locals.put(name, next_local.*);
            pat.id = next_local.*;
            next_local.* += 1;
        },
        .Wildcard => {
            pat.id = next_local.*;
            next_local.* += 1;
        },
    }
}

// Resolves names in a statement (let bindings and expression statements).
fn resolveStmt(
    crate: *hir.Crate,
    stmt_id: hir.StmtId,
    module_symbols: *std.StringHashMap(hir.DefId),
    locals: *std.StringHashMap(hir.LocalId),
    next_local: *hir.LocalId,
    diagnostics: *diag.Diagnostics,
) Error!void {
    const stmt = &crate.stmts.items[stmt_id];
    switch (stmt.kind) {
        .Let => |*let_stmt| {
            if (let_stmt.value) |expr_id| {
                try resolveExpr(crate, expr_id, module_symbols, locals, next_local, diagnostics);
            }
            try bindPattern(&let_stmt.pat, locals, next_local, diagnostics, null);
        },
        .Expr => |expr_id| {
            try resolveExpr(crate, expr_id, module_symbols, locals, next_local, diagnostics);
        },
        .Unknown => {},
    }
}

// Recursively resolves names in an expression.
// Converts UnresolvedIdent and Path expressions to LocalRef or GlobalRef.
fn resolveExpr(
    crate: *hir.Crate,
    expr_id: hir.ExprId,
    module_symbols: *std.StringHashMap(hir.DefId),
    locals: *std.StringHashMap(hir.LocalId),
    next_local: *hir.LocalId,
    diagnostics: *diag.Diagnostics,
) Error!void {
    var expr = &crate.exprs.items[expr_id];
    switch (expr.kind) {
        .UnresolvedIdent => |name| {
            if (locals.get(name)) |local_id| {
                expr.kind = .{ .LocalRef = local_id };
                return;
            }
            if (module_symbols.get(name)) |def_id| {
                expr.kind = .{ .GlobalRef = def_id };
                return;
            }

            const message = std.fmt.allocPrint(
                diagnostics.allocator,
                "unresolved identifier `{s}`",
                .{name},
            ) catch name;
            diagnostics.reportError(expr.span, message);
            if (message.ptr != name.ptr) diagnostics.allocator.free(message);
        },
        .Block => |block| {
            for (block.stmts) |stmt_id| {
                try resolveStmt(crate, stmt_id, module_symbols, locals, next_local, diagnostics);
            }
            if (block.tail) |tail| try resolveExpr(crate, tail, module_symbols, locals, next_local, diagnostics);
        },

        .Unsafe => |unsafe_block| {
            try resolveExpr(crate, unsafe_block.body, module_symbols, locals, next_local, diagnostics);
        },

        .Binary => |binary| {
            try resolveExpr(crate, binary.lhs, module_symbols, locals, next_local, diagnostics);
            try resolveExpr(crate, binary.rhs, module_symbols, locals, next_local, diagnostics);
        },
        .Unary => |unary| try resolveExpr(crate, unary.expr, module_symbols, locals, next_local, diagnostics),
        .Call => |call| {
            try resolveExpr(crate, call.callee, module_symbols, locals, next_local, diagnostics);
            for (call.args) |arg| {
                try resolveExpr(crate, arg, module_symbols, locals, next_local, diagnostics);
            }
        },
        .MethodCall => |method_call| {
            try resolveExpr(crate, method_call.target, module_symbols, locals, next_local, diagnostics);
            for (method_call.args) |arg| {
                try resolveExpr(crate, arg, module_symbols, locals, next_local, diagnostics);
            }
        },
        .Assignment => |assign| {
            try resolveExpr(crate, assign.target, module_symbols, locals, next_local, diagnostics);
            try resolveExpr(crate, assign.value, module_symbols, locals, next_local, diagnostics);
        },
        .Return => |maybe_val| if (maybe_val) |val| try resolveExpr(crate, val, module_symbols, locals, next_local, diagnostics),
        .If => |if_expr| {
            try resolveExpr(crate, if_expr.cond, module_symbols, locals, next_local, diagnostics);
            try resolveExpr(crate, if_expr.then_branch, module_symbols, locals, next_local, diagnostics);
            if (if_expr.else_branch) |else_branch| {
                try resolveExpr(crate, else_branch, module_symbols, locals, next_local, diagnostics);
            }
        },
        .While => |while_expr| {
            try resolveExpr(crate, while_expr.cond, module_symbols, locals, next_local, diagnostics);
            try resolveExpr(crate, while_expr.body, module_symbols, locals, next_local, diagnostics);
        },
        .Range => |range| {
            try resolveExpr(crate, range.start, module_symbols, locals, next_local, diagnostics);
            try resolveExpr(crate, range.end, module_symbols, locals, next_local, diagnostics);
        },
        .For => |*for_expr| {
            try resolveExpr(crate, for_expr.iter, module_symbols, locals, next_local, diagnostics);
            try bindPattern(&for_expr.pat, locals, next_local, diagnostics, null);
            try resolveExpr(crate, for_expr.body, module_symbols, locals, next_local, diagnostics);
        },
        .Cast => |cast| try resolveExpr(crate, cast.expr, module_symbols, locals, next_local, diagnostics),
        .Index => |index| {
            try resolveExpr(crate, index.target, module_symbols, locals, next_local, diagnostics);
            try resolveExpr(crate, index.index, module_symbols, locals, next_local, diagnostics);
        },
        .Field => |field| try resolveExpr(crate, field.target, module_symbols, locals, next_local, diagnostics),
        .Array => |elements| {
            for (elements) |elem| {
                try resolveExpr(crate, elem, module_symbols, locals, next_local, diagnostics);
            }
        },
        .StructInit => |init| {
            for (init.fields) |field| {
                try resolveExpr(crate, field.value, module_symbols, locals, next_local, diagnostics);
            }
        },
        .Lambda => |lambda| {
            var lambda_locals = try locals.clone();
            defer lambda_locals.deinit();

            var lambda_next_local = next_local.*;
            const scope_start = lambda_next_local;
            for (lambda.params) |*param| {
                try bindPattern(param, &lambda_locals, &lambda_next_local, diagnostics, scope_start);
            }
            try resolveExpr(crate, lambda.body, module_symbols, &lambda_locals, &lambda_next_local, diagnostics);
            next_local.* = lambda_next_local;
        },
        .Path => |path| {
            if (path.segments.len == 1) {
                const single_name = path.segments[0];
                if (locals.get(single_name)) |local_id| {
                    expr.kind = .{ .LocalRef = local_id };
                    return;
                }
                if (module_symbols.get(single_name)) |def_id| {
                    expr.kind = .{ .GlobalRef = def_id };
                    return;
                }
                const message = std.fmt.allocPrint(diagnostics.allocator, "unresolved identifier `{s}`", .{single_name}) catch single_name;
                diagnostics.reportError(expr.span, message);
                if (message.ptr != single_name.ptr) diagnostics.allocator.free(message);
            } else if (path.segments.len == 2) {
                // Two-segment paths like `Counter::new` - look for associated function
                const type_name = path.segments[0];
                const method_name = path.segments[1];

                // First check if the first segment is a struct/type name
                var struct_def_id: ?hir.DefId = null;
                for (crate.items.items) |item| {
                    switch (item.kind) {
                        .Struct => |struct_item| {
                            if (std.mem.eql(u8, struct_item.name, type_name)) {
                                struct_def_id = struct_item.def_id;
                                break;
                            }
                        },
                        else => {},
                    }
                }

                if (struct_def_id != null) {
                    // Look for the mangled method name in module symbols
                    // Methods are mangled as StructName_methodName
                    var mangled_buf: [256]u8 = undefined;
                    const mangled_name = std.fmt.bufPrint(&mangled_buf, "{s}_{s}", .{ type_name, method_name }) catch {
                        diagnostics.reportError(expr.span, "method name too long");
                        return;
                    };

                    if (module_symbols.get(mangled_name)) |def_id| {
                        expr.kind = .{ .GlobalRef = def_id };
                        return;
                    }
                }

                // If not found as associated function, try just the last segment
                if (module_symbols.get(method_name)) |def_id| {
                    expr.kind = .{ .GlobalRef = def_id };
                    return;
                }
                const message = std.fmt.allocPrint(diagnostics.allocator, "unresolved path `{s}::{s}`", .{ type_name, method_name }) catch method_name;
                diagnostics.reportError(expr.span, message);
                if (message.ptr != method_name.ptr) diagnostics.allocator.free(message);
            } else if (path.segments.len > 0) {
                const name = path.segments[path.segments.len - 1];
                if (module_symbols.get(name)) |def_id| {
                    expr.kind = .{ .GlobalRef = def_id };
                    return;
                }
                const message = std.fmt.allocPrint(diagnostics.allocator, "unresolved path ending in `{s}`", .{name}) catch name;
                diagnostics.reportError(expr.span, message);
                if (message.ptr != name.ptr) diagnostics.allocator.free(message);
            }
        },
        .LocalRef, .GlobalRef, .ConstInt, .ConstFloat, .ConstBool, .ConstChar, .ConstString, .Unknown => {},
    }
}

test "name resolution resolves globals and locals" {
    const allocator = std.testing.allocator;
    var diagnostics = diag.Diagnostics.init(allocator);
    defer diagnostics.deinit();

    var crate = hir.Crate.init(allocator);
    defer crate.deinit();

    const span = hir.emptySpan(0);

    // Prepare expressions for `foo`'s body: reference to `bar` and an unresolved name.
    const bar_ref_id: hir.ExprId = @intCast(crate.exprs.items.len);
    try crate.exprs.append(crate.allocator(), .{
        .id = bar_ref_id,
        .kind = .{ .UnresolvedIdent = "bar" },
        .ty = 0,
        .span = span,
    });

    const missing_ref_id: hir.ExprId = @intCast(crate.exprs.items.len);
    try crate.exprs.append(crate.allocator(), .{
        .id = missing_ref_id,
        .kind = .{ .UnresolvedIdent = "missing" },
        .ty = 0,
        .span = span,
    });

    const stmt_ids = try crate.allocator().alloc(hir.StmtId, 2);
    stmt_ids[0] = @intCast(crate.stmts.items.len);
    try crate.stmts.append(crate.allocator(), .{ .id = stmt_ids[0], .kind = .{ .Expr = bar_ref_id }, .span = span });
    stmt_ids[1] = @intCast(crate.stmts.items.len);
    try crate.stmts.append(crate.allocator(), .{ .id = stmt_ids[1], .kind = .{ .Expr = missing_ref_id }, .span = span });

    const block_expr_id: hir.ExprId = @intCast(crate.exprs.items.len);
    try crate.exprs.append(crate.allocator(), .{
        .id = block_expr_id,
        .kind = .{ .Block = .{ .stmts = stmt_ids, .tail = null } },
        .ty = 0,
        .span = span,
    });

    const foo_def: hir.DefId = 0;
    const bar_def: hir.DefId = 1;
    try crate.items.append(crate.allocator(), .{ .id = foo_def, .kind = .{ .Function = .{
        .def_id = foo_def,
        .name = "foo",
        .params = &[_]hir.LocalId{},
        .param_types = &[_]hir.TypeId{},
        .return_type = null,
        .body = block_expr_id,
        .span = span,
    } }, .span = span });

    try crate.items.append(crate.allocator(), .{ .id = bar_def, .kind = .{ .Function = .{
        .def_id = bar_def,
        .name = "bar",
        .params = &[_]hir.LocalId{},
        .param_types = &[_]hir.TypeId{},
        .return_type = null,
        .body = null,
        .span = span,
    } }, .span = span });

    try resolve(&crate, &diagnostics);

    try std.testing.expectEqual(@as(hir.DefId, bar_def), crate.exprs.items[bar_ref_id].kind.GlobalRef);
    try std.testing.expect(diagnostics.hasErrors());
}

test "name resolution binds function parameters" {
    const allocator = std.testing.allocator;
    var diagnostics = diag.Diagnostics.init(allocator);
    defer diagnostics.deinit();

    var crate = hir.Crate.init(allocator);
    defer crate.deinit();

    const span = hir.emptySpan(0);

    const param_ref_id: hir.ExprId = @intCast(crate.exprs.items.len);
    try crate.exprs.append(crate.allocator(), .{
        .id = param_ref_id,
        .kind = .{ .UnresolvedIdent = "a" },
        .ty = 0,
        .span = span,
    });

    try crate.patterns.append(crate.allocator(), .{ .id = 0, .kind = .{ .Identifier = "a" }, .span = span });

    try crate.types.append(crate.allocator(), .{ .id = 0, .kind = .Unknown });

    const block_expr_id: hir.ExprId = @intCast(crate.exprs.items.len);
    try crate.exprs.append(crate.allocator(), .{
        .id = block_expr_id,
        .kind = .{ .Block = .{ .stmts = &[_]hir.StmtId{}, .tail = param_ref_id } },
        .ty = 0,
        .span = span,
    });

    const params = try crate.allocator().alloc(hir.LocalId, 1);
    params[0] = 0;
    const param_types = try crate.allocator().alloc(hir.TypeId, 1);
    param_types[0] = 0;

    try crate.items.append(crate.allocator(), .{ .id = 0, .kind = .{ .Function = .{
        .def_id = 0,
        .name = "with_param",
        .params = params,
        .param_types = param_types,
        .return_type = null,
        .body = block_expr_id,
        .span = span,
    } }, .span = span });

    try resolve(&crate, &diagnostics);

    try std.testing.expect(!diagnostics.hasErrors());
    switch (crate.exprs.items[param_ref_id].kind) {
        .LocalRef => |local_id| try std.testing.expectEqual(@as(hir.LocalId, 0), local_id),
        else => try std.testing.expect(false),
    }
}
