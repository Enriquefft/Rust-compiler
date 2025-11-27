const std = @import("std");
const hir = @import("hir.zig");
const AllocError = std.mem.Allocator.Error;

const TreePrinter = struct {
    allocator: std.mem.Allocator,
    prefix: std.ArrayListUnmanaged(u8),
    segment_lengths: std.ArrayListUnmanaged(usize),

    fn init(allocator: std.mem.Allocator) TreePrinter {
        return .{
            .allocator = allocator,
            .prefix = .{},
            .segment_lengths = .{},
        };
    }

    fn deinit(self: *TreePrinter) void {
        self.prefix.deinit(self.allocator);
        self.segment_lengths.deinit(self.allocator);
    }

    fn push(self: *TreePrinter, is_last: bool) !void {
        const segment: []const u8 = if (is_last) "    " else "│   ";
        try self.prefix.appendSlice(self.allocator, segment);
        try self.segment_lengths.append(self.allocator, segment.len);
    }

    fn pop(self: *TreePrinter) void {
        if (self.segment_lengths.items.len == 0) return;

        const last_len = self.segment_lengths.pop().?;
        const new_len = self.prefix.items.len - last_len;
        self.prefix.shrinkRetainingCapacity(new_len);
    }

    fn printNode(self: *TreePrinter, is_last: bool, label: []const u8) !void {
        const connector: []const u8 = if (self.prefix.items.len == 0)
            ""
        else if (is_last)
            "└── "
        else
            "├── ";

        std.debug.print("{s}{s}{s}\n", .{ self.prefix.items, connector, label });
    }

    fn printNodeFmt(self: *TreePrinter, is_last: bool, comptime fmt: []const u8, args: anytype) !void {
        const label = try std.fmt.allocPrint(self.allocator, fmt, args);
        defer self.allocator.free(label);
        try self.printNode(is_last, label);
    }
};

pub fn printCrateTree(allocator: std.mem.Allocator, crate: *const hir.Crate) AllocError!void {
    var printer = TreePrinter.init(allocator);
    defer printer.deinit();

    try printer.printNode(true, "HIR Crate");
    if (crate.items.items.len == 0) return;

    try printer.push(true);
    for (crate.items.items, 0..) |item, idx| {
        try printItem(&printer, crate, item, idx == crate.items.items.len - 1);
    }
    printer.pop();
}

fn printItem(printer: *TreePrinter, crate: *const hir.Crate, item: hir.Item, is_last: bool) AllocError!void {
    switch (item.kind) {
        .Function => |func| {
            try printer.printNodeFmt(is_last, "Function {s} (def_id: {d})", .{ func.name, func.def_id });
            try printer.push(is_last);
            try printParams(printer, crate, func.params, func.param_types, false);
            if (func.return_type) |ret_ty| {
                try printType(printer, crate, ret_ty, false, "Return");
            } else {
                try printer.printNode(false, "Return: ()");
            }
            if (func.body) |body_id| {
                try printExpr(printer, crate, body_id, true);
            } else {
                try printer.printNode(true, "Body: (none)");
            }
            printer.pop();
        },
        .Struct => |st| {
            try printer.printNodeFmt(is_last, "Struct {s} (def_id: {d})", .{ st.name, st.def_id });
            try printer.push(is_last);
            if (st.fields.len == 0) {
                try printer.printNode(true, "Fields: (none)");
            } else {
                for (st.fields, 0..) |field, field_idx| {
                    const last_field = field_idx == st.fields.len - 1;
                    try printer.printNodeFmt(last_field, "Field {s}", .{field.name});
                    try printer.push(last_field);
                    try printType(printer, crate, field.ty, true, "Type");
                    printer.pop();
                }
            }
            printer.pop();
        },
        .TypeAlias => |alias| {
            try printer.printNodeFmt(is_last, "TypeAlias {s} (def_id: {d})", .{ alias.name, alias.def_id });
            try printer.push(is_last);
            try printType(printer, crate, alias.target, true, "Target");
            printer.pop();
        },
        .Impl => |impl| {
            try printer.printNodeFmt(is_last, "Impl (def_id: {d})", .{impl.def_id});
            try printer.push(is_last);
            try printType(printer, crate, impl.target, impl.methods.len == 0, "Target");
            for (impl.methods, 0..) |method_id, method_idx| {
                const last_method = method_idx == impl.methods.len - 1;
                if (method_id < crate.items.items.len) {
                    try printItem(printer, crate, crate.items.items[method_id], last_method);
                }
            }
            printer.pop();
        },
        .Empty => {
            try printer.printNode(is_last, "Empty");
        },
    }
}

fn printParams(printer: *TreePrinter, crate: *const hir.Crate, params: []const hir.LocalId, param_types: []const hir.TypeId, is_last: bool) AllocError!void {
    try printer.printNode(is_last, "Params");
    if (params.len == 0) return;

    try printer.push(is_last);
    for (params, 0..) |param_id, idx| {
        const last_param = idx == params.len - 1;
        if (param_id < crate.patterns.items.len) {
            const pattern = crate.patterns.items[param_id];
            const type_id = if (idx < param_types.len) param_types[idx] else 0;
            try printer.printNodeFmt(last_param, "Param (id: {d})", .{param_id});
            try printer.push(last_param);
            try printPattern(printer, pattern, false);
            try printType(printer, crate, type_id, true, "Type");
            printer.pop();
        }
    }
    printer.pop();
}

fn printPattern(printer: *TreePrinter, pattern: hir.Pattern, is_last: bool) AllocError!void {
    switch (pattern.kind) {
        .Identifier => |name| try printer.printNodeFmt(is_last, "Pattern Identifier {s}", .{name}),
        .Wildcard => try printer.printNode(is_last, "Pattern _"),
    }
}

fn printExpr(printer: *TreePrinter, crate: *const hir.Crate, expr_id: hir.ExprId, is_last: bool) AllocError!void {
    if (expr_id >= crate.exprs.items.len) {
        try printer.printNodeFmt(is_last, "Expr (invalid id: {d})", .{expr_id});
        return;
    }

    const expr = crate.exprs.items[expr_id];
    switch (expr.kind) {
        .LocalRef => |local_id| try printer.printNodeFmt(is_last, "LocalRef {d}", .{local_id}),
        .GlobalRef => |def_id| try printer.printNodeFmt(is_last, "GlobalRef {d}", .{def_id}),
        .ConstInt => |val| try printer.printNodeFmt(is_last, "ConstInt {d}", .{val}),
        .ConstFloat => |val| try printer.printNodeFmt(is_last, "ConstFloat {d}", .{val}),
        .ConstBool => |val| try printer.printNodeFmt(is_last, "ConstBool {}", .{val}),
        .ConstChar => |val| try printer.printNodeFmt(is_last, "ConstChar {d}", .{val}),
        .ConstString => |val| try printer.printNodeFmt(is_last, "ConstString \"{s}\"", .{val}),
        .UnresolvedIdent => |name| try printer.printNodeFmt(is_last, "UnresolvedIdent {s}", .{name}),
        .Path => |path| {
            try printer.printNode(is_last, "Path");
            try printer.push(is_last);
            for (path.segments, 0..) |seg, idx| {
                try printer.printNodeFmt(idx == path.segments.len - 1 and path.args.len == 0, "Segment {s}", .{seg});
            }
            if (path.args.len > 0) {
                try printer.printNode(true, "GenericArgs");
                try printer.push(true);
                for (path.args, 0..) |arg, idx| {
                    try printType(printer, crate, arg, idx == path.args.len - 1, "Arg");
                }
                printer.pop();
            }
            printer.pop();
        },
        .Binary => |bin| {
            try printer.printNodeFmt(is_last, "Binary ({s})", .{@tagName(bin.op)});
            try printer.push(is_last);
            try printExpr(printer, crate, bin.lhs, false);
            try printExpr(printer, crate, bin.rhs, true);
            printer.pop();
        },
        .Unary => |un| {
            try printer.printNodeFmt(is_last, "Unary ({s})", .{@tagName(un.op)});
            try printer.push(is_last);
            try printExpr(printer, crate, un.expr, true);
            printer.pop();
        },
        .Call => |call| {
            try printer.printNode(is_last, "Call");
            try printer.push(is_last);
            try printExpr(printer, crate, call.callee, call.args.len == 0);
            for (call.args, 0..) |arg, idx| {
                try printExpr(printer, crate, arg, idx == call.args.len - 1);
            }
            printer.pop();
        },
        .MethodCall => |mc| {
            try printer.printNodeFmt(is_last, "MethodCall {s}", .{mc.name});
            try printer.push(is_last);
            try printExpr(printer, crate, mc.target, mc.args.len == 0);
            for (mc.args, 0..) |arg, idx| {
                try printExpr(printer, crate, arg, idx == mc.args.len - 1);
            }
            printer.pop();
        },
        .Assignment => |assign| {
            try printer.printNodeFmt(is_last, "Assignment ({s})", .{@tagName(assign.op)});
            try printer.push(is_last);
            try printExpr(printer, crate, assign.target, false);
            try printExpr(printer, crate, assign.value, true);
            printer.pop();
        },
        .Return => |val| {
            try printer.printNode(is_last, "Return");
            if (val) |ret_expr| {
                try printer.push(is_last);
                try printExpr(printer, crate, ret_expr, true);
                printer.pop();
            }
        },
        .If => |if_expr| {
            try printer.printNode(is_last, "If");
            try printer.push(is_last);
            try printExpr(printer, crate, if_expr.cond, false);
            try printExpr(printer, crate, if_expr.then_branch, if_expr.else_branch == null);
            if (if_expr.else_branch) |else_branch| {
                try printExpr(printer, crate, else_branch, true);
            }
            printer.pop();
        },
        .While => |while_expr| {
            try printer.printNode(is_last, "While");
            try printer.push(is_last);
            try printExpr(printer, crate, while_expr.cond, false);
            try printExpr(printer, crate, while_expr.body, true);
            printer.pop();
        },
        .For => |for_expr| {
            try printer.printNode(is_last, "For");
            try printer.push(is_last);
            try printPattern(printer, for_expr.pat, false);
            try printExpr(printer, crate, for_expr.iter, false);
            try printExpr(printer, crate, for_expr.body, true);
            printer.pop();
        },
        .Range => |range| {
            try printer.printNodeFmt(is_last, "Range (inclusive: {})", .{range.inclusive});
            try printer.push(is_last);
            try printExpr(printer, crate, range.start, false);
            try printExpr(printer, crate, range.end, true);
            printer.pop();
        },
        .Cast => |cast| {
            try printer.printNode(is_last, "Cast");
            try printer.push(is_last);
            try printExpr(printer, crate, cast.expr, false);
            try printType(printer, crate, cast.ty, true, "Type");
            printer.pop();
        },
        .Index => |index| {
            try printer.printNode(is_last, "Index");
            try printer.push(is_last);
            try printExpr(printer, crate, index.target, false);
            try printExpr(printer, crate, index.index, true);
            printer.pop();
        },
        .Field => |field| {
            try printer.printNodeFmt(is_last, "Field {s}", .{field.name});
            try printer.push(is_last);
            try printExpr(printer, crate, field.target, true);
            printer.pop();
        },
        .Array => |elements| {
            try printer.printNode(is_last, "Array");
            try printer.push(is_last);
            for (elements, 0..) |elem, idx| {
                try printExpr(printer, crate, elem, idx == elements.len - 1);
            }
            printer.pop();
        },
        .StructInit => |init| {
            try printer.printNode(is_last, "StructInit");
            try printer.push(is_last);
            // Print path segments
            try printer.printNode(init.fields.len == 0, "Path");
            try printer.push(init.fields.len == 0);
            for (init.path, 0..) |seg, idx| {
                try printer.printNodeFmt(idx == init.path.len - 1, "Segment {s}", .{seg});
            }
            printer.pop();
            // Print fields
            for (init.fields, 0..) |field, idx| {
                const last_field = idx == init.fields.len - 1;
                try printer.printNodeFmt(last_field, "Field {s}", .{field.name});
                try printer.push(last_field);
                try printExpr(printer, crate, field.value, true);
                printer.pop();
            }
            printer.pop();
        },
        .Lambda => |lambda| {
            try printer.printNode(is_last, "Lambda");
            try printer.push(is_last);
            // Print params
            try printer.printNode(false, "Params");
            try printer.push(false);
            for (lambda.params, 0..) |param, idx| {
                const last_param = idx == lambda.params.len - 1;
                try printPattern(printer, param, last_param);
            }
            printer.pop();
            try printExpr(printer, crate, lambda.body, true);
            printer.pop();
        },
        .Block => |block| {
            try printer.printNode(is_last, "Block");
            if (block.stmts.len == 0 and block.tail == null) return;

            try printer.push(is_last);
            for (block.stmts, 0..) |stmt_id, idx| {
                const last_stmt = idx == block.stmts.len - 1 and block.tail == null;
                try printStmt(printer, crate, stmt_id, last_stmt);
            }
            if (block.tail) |tail_id| {
                try printExpr(printer, crate, tail_id, true);
            }
            printer.pop();
        },
        .Unknown => {
            try printer.printNode(is_last, "Unknown");
        },
    }
}

fn printStmt(printer: *TreePrinter, crate: *const hir.Crate, stmt_id: hir.StmtId, is_last: bool) AllocError!void {
    if (stmt_id >= crate.stmts.items.len) {
        try printer.printNodeFmt(is_last, "Stmt (invalid id: {d})", .{stmt_id});
        return;
    }

    const stmt = crate.stmts.items[stmt_id];
    switch (stmt.kind) {
        .Let => |let_stmt| {
            try printer.printNode(is_last, "Let");
            try printer.push(is_last);
            try printPattern(printer, let_stmt.pat, let_stmt.ty == null and let_stmt.value == null);
            if (let_stmt.ty) |ty_id| {
                try printType(printer, crate, ty_id, let_stmt.value == null, "Type");
            }
            if (let_stmt.value) |val_id| {
                try printExpr(printer, crate, val_id, true);
            }
            printer.pop();
        },
        .Expr => |expr_id| {
            try printer.printNode(is_last, "ExprStmt");
            try printer.push(is_last);
            try printExpr(printer, crate, expr_id, true);
            printer.pop();
        },
        .Unknown => {
            try printer.printNode(is_last, "UnknownStmt");
        },
    }
}

fn printType(printer: *TreePrinter, crate: *const hir.Crate, type_id: hir.TypeId, is_last: bool, label: []const u8) AllocError!void {
    if (type_id >= crate.types.items.len) {
        try printer.printNodeFmt(is_last, "{s}: (invalid type id: {d})", .{ label, type_id });
        return;
    }

    const ty = crate.types.items[type_id];
    switch (ty.kind) {
        .PrimInt => |int_ty| try printer.printNodeFmt(is_last, "{s}: {s}", .{ label, @tagName(int_ty) }),
        .PrimFloat => |float_ty| try printer.printNodeFmt(is_last, "{s}: {s}", .{ label, @tagName(float_ty) }),
        .Bool => try printer.printNodeFmt(is_last, "{s}: bool", .{label}),
        .Char => try printer.printNodeFmt(is_last, "{s}: char", .{label}),
        .Str => try printer.printNodeFmt(is_last, "{s}: str", .{label}),
        .String => try printer.printNodeFmt(is_last, "{s}: String", .{label}),
        .Array => |arr| {
            try printer.printNodeFmt(is_last, "{s}: Array", .{label});
            try printer.push(is_last);
            try printType(printer, crate, arr.elem, true, "Element");
            printer.pop();
        },
        .Pointer => |ptr| {
            try printer.printNodeFmt(is_last, "{s}: Pointer (mut: {})", .{ label, ptr.mutable });
            try printer.push(is_last);
            try printType(printer, crate, ptr.inner, true, "Target");
            printer.pop();
        },
        .Ref => |ref| {
            try printer.printNodeFmt(is_last, "{s}: Reference (mut: {})", .{ label, ref.mutable });
            try printer.push(is_last);
            try printType(printer, crate, ref.inner, true, "Target");
            printer.pop();
        },
        .Fn => |fn_ty| {
            try printer.printNodeFmt(is_last, "{s}: Fn", .{label});
            try printer.push(is_last);
            for (fn_ty.params, 0..) |param_ty, idx| {
                try printType(printer, crate, param_ty, idx == fn_ty.params.len - 1 and fn_ty.ret == 0, "Param");
            }
            try printType(printer, crate, fn_ty.ret, true, "Return");
            printer.pop();
        },
        .Struct => |st| {
            try printer.printNodeFmt(is_last, "{s}: Struct (def_id: {d})", .{ label, st.def_id });
        },
        .Path => |path| {
            try printer.printNodeFmt(is_last, "{s}: Path", .{label});
            try printer.push(is_last);
            for (path.segments, 0..) |seg, idx| {
                try printer.printNodeFmt(idx == path.segments.len - 1 and path.args.len == 0, "Segment {s}", .{seg});
            }
            if (path.args.len > 0) {
                try printer.printNode(true, "GenericArgs");
                try printer.push(true);
                for (path.args, 0..) |arg, idx| {
                    try printType(printer, crate, arg, idx == path.args.len - 1, "Arg");
                }
                printer.pop();
            }
            printer.pop();
        },
        .Unknown => try printer.printNodeFmt(is_last, "{s}: Unknown", .{label}),
    }
}

test "printCrateTree prints empty crate" {
    const allocator = std.testing.allocator;
    var crate = hir.Crate.init(allocator);
    defer crate.deinit();

    try printCrateTree(allocator, &crate);
}

test "printCrateTree prints function with body" {
    const allocator = std.testing.allocator;
    var crate = hir.Crate.init(allocator);
    defer crate.deinit();

    // Add a simple block expression
    try crate.exprs.append(crate.allocator(), .{
        .id = 0,
        .kind = .{ .Block = .{ .stmts = &[_]hir.StmtId{}, .tail = null } },
        .ty = 0,
        .span = .{ .file_id = 0, .start = 0, .end = 0 },
    });

    // Add a function item
    try crate.items.append(crate.allocator(), .{
        .id = 0,
        .kind = .{ .Function = .{
            .def_id = 0,
            .name = "main",
            .params = &[_]hir.LocalId{},
            .param_types = &[_]hir.TypeId{},
            .return_type = null,
            .body = 0,
            .span = .{ .file_id = 0, .start = 0, .end = 0 },
        } },
        .span = .{ .file_id = 0, .start = 0, .end = 0 },
    });

    try printCrateTree(allocator, &crate);
}
