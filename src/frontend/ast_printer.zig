const std = @import("std");
const ast = @import("ast.zig");
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


pub fn printCrateTree(allocator: std.mem.Allocator, crate: ast.Crate) AllocError!void {
    var printer = TreePrinter.init(allocator);
    defer printer.deinit();

    try printer.printNode(true, "Crate");
    if (crate.items.len == 0) return;

    try printer.push(true);
    for (crate.items, 0..) |item, idx| {
        try printItem(&printer, item, idx == crate.items.len - 1);
    }
    printer.pop();
}

fn printItem(printer: *TreePrinter, item: ast.Item, is_last: bool) AllocError!void {
    switch (item.tag) {
        .Fn => {
            const fn_item = item.data.Fn;
            try printer.printNodeFmt(is_last, "Fn {s}", .{fn_item.name.name});
            try printer.push(is_last);
            try printIdentifierList(printer, "Generics", fn_item.generics, false);
            try printParams(printer, fn_item.params, false);
            if (fn_item.return_type) |ret| {
                try printType(printer, &ret, false, "Return");
            } else {
                try printer.printNode(false, "Return: ()");
            }
            try printBlock(printer, fn_item.body, true);
            printer.pop();
        },
        .Struct => {
            const st = item.data.Struct;
            try printer.printNodeFmt(is_last, "Struct {s}", .{st.name.name});
            try printer.push(is_last);
            try printIdentifierList(printer, "Generics", st.generics, st.fields.len == 0);
            for (st.fields, 0..) |field, field_idx| {
                const last_field = field_idx == st.fields.len - 1;
                try printer.printNodeFmt(last_field, "Field {s}", .{field.name.name});
                try printer.push(last_field);
                try printType(printer, &field.ty, true, "Type");
                printer.pop();
            }
            printer.pop();
        },
        .Impl => {
            const imp = item.data.Impl;
            try printer.printNode(is_last, "Impl");
            try printer.push(is_last);
            try printIdentifierList(printer, "Generics", imp.generics, false);
            try printType(printer, &imp.target, imp.methods.len == 0, "Target");
            for (imp.methods, 0..) |method, method_idx| {
                try printItem(printer, .{ .tag = .Fn, .span = method.span, .data = .{ .Fn = method } }, method_idx == imp.methods.len - 1);
            }
            printer.pop();
        },
        .TypeAlias => {
            const alias = item.data.TypeAlias;
            try printer.printNodeFmt(is_last, "TypeAlias {s}", .{alias.name.name});
            try printer.push(is_last);
            try printIdentifierList(printer, "Generics", alias.generics, false);
            try printType(printer, &alias.aliased_type, true, "Aliased");
            printer.pop();
        },
        .Empty => {
            try printer.printNode(is_last, "Empty");
        },
    }
}

fn printIdentifierList(printer: *TreePrinter, label: []const u8, identifiers: []const ast.Identifier, is_last: bool) AllocError!void {
    if (identifiers.len == 0) {
        try printer.printNode(is_last, label);
        return;
    }

    try printer.printNode(is_last, label);
    try printer.push(is_last);
    for (identifiers, 0..) |ident, idx| {
        try printer.printNodeFmt(idx == identifiers.len - 1, "{s}", .{ident.name});
    }
    printer.pop();
}

fn printParams(printer: *TreePrinter, params: []const ast.Param, is_last: bool) AllocError!void {
    try printer.printNode(is_last, "Params");
    if (params.len == 0) return;

    try printer.push(is_last);
    for (params, 0..) |param, idx| {
        const last_param = idx == params.len - 1;
        try printer.printNodeFmt(last_param, "Param ({s})", .{@tagName(param.kind)});
        try printer.push(last_param);
        try printPattern(printer, param.pattern, param.ty == null);
        if (param.ty) |ty| {
            try printType(printer, &ty, true, "Type");
        }
        printer.pop();
    }
    printer.pop();
}

fn printPattern(printer: *TreePrinter, pattern: ast.Pattern, is_last: bool) AllocError!void {
    switch (pattern.tag) {
        .Identifier => try printer.printNodeFmt(is_last, "Pattern Identifier {s}", .{pattern.data.Identifier.name}),
        .Wildcard => try printer.printNode(is_last, "Pattern _"),
    }
}

fn printBlock(printer: *TreePrinter, block: ast.Block, is_last: bool) AllocError!void {
    try printer.printNode(is_last, "Block");
    if (block.stmts.len == 0 and block.result == null) return;

    try printer.push(is_last);
    for (block.stmts, 0..) |stmt, idx| {
        const last_stmt = idx == block.stmts.len - 1 and block.result == null;
        try printStmt(printer, stmt, last_stmt);
    }
    if (block.result) |expr| {
        try printExpr(printer, expr.*, true);
    }
    printer.pop();
}

fn printStmt(printer: *TreePrinter, stmt: ast.Stmt, is_last: bool) AllocError!void {
    switch (stmt.tag) {
        .Let => {
            const let_stmt = stmt.data.Let;
            try printer.printNodeFmt(is_last, "Let (mutable: {})", .{let_stmt.mutable});
            try printer.push(is_last);
            try printPattern(printer, let_stmt.pattern, let_stmt.ty == null and let_stmt.value == null);
            if (let_stmt.ty) |ty| {
                try printType(printer, &ty, let_stmt.value == null, "Type");
            }
            if (let_stmt.value) |expr_ptr| {
                try printExpr(printer, expr_ptr.*, true);
            }
            printer.pop();
        },
        .Expr => {
            try printer.printNode(is_last, "ExprStmt");
            try printer.push(is_last);
            try printExpr(printer, stmt.data.Expr.expr, true);
            printer.pop();
        },
        .While => {
            try printer.printNode(is_last, "While");
            try printer.push(is_last);
            try printExpr(printer, stmt.data.While.condition.*, false);
            try printBlock(printer, stmt.data.While.body, true);
            printer.pop();
        },
        .If => {
            try printer.printNode(is_last, "If");
            try printer.push(is_last);
            try printExpr(printer, stmt.data.If.condition.*, false);
            try printBlock(printer, stmt.data.If.then_block, stmt.data.If.else_expr == null);
            if (stmt.data.If.else_expr) |else_expr| {
                try printExpr(printer, else_expr.*, true);
            }
            printer.pop();
        },
        .For => {
            try printer.printNode(is_last, "For");
            try printer.push(is_last);
            try printPattern(printer, stmt.data.For.pattern, false);
            try printExpr(printer, stmt.data.For.iterator.*, false);
            try printBlock(printer, stmt.data.For.body, true);
            printer.pop();
        },
        .Return => {
            try printer.printNode(is_last, "Return");
            if (stmt.data.Return.value == null) return;
            try printer.push(is_last);
            try printExpr(printer, stmt.data.Return.value.?.*, true);
            printer.pop();
        },
        .Empty => {
            try printer.printNode(is_last, "EmptyStmt");
        },
    }
}

fn printExpr(printer: *TreePrinter, expr: ast.Expr, is_last: bool) AllocError!void {
    switch (expr.tag) {
        .Literal => try printer.printNodeFmt(is_last, "Literal ({s}) {s}", .{
            @tagName(expr.data.Literal.kind),
            expr.data.Literal.lexeme,
        }),
        .Path => {
            try printer.printNode(is_last, "Path");
            try printer.push(is_last);
            try printPath(printer, expr.data.Path, true);
            printer.pop();
        },
        .Block => try printBlock(printer, expr.data.Block, is_last),
        .If => {
            try printer.printNode(is_last, "IfExpr");
            try printer.push(is_last);
            try printExpr(printer, expr.data.If.condition.*, false);
            try printBlock(printer, expr.data.If.then_block, expr.data.If.else_expr == null);
            if (expr.data.If.else_expr) |else_expr| {
                try printExpr(printer, else_expr.*, true);
            }
            printer.pop();
        },
        .While => {
            try printer.printNode(is_last, "WhileExpr");
            try printer.push(is_last);
            try printExpr(printer, expr.data.While.condition.*, false);
            try printBlock(printer, expr.data.While.body, true);
            printer.pop();
        },
        .For => {
            try printer.printNode(is_last, "ForExpr");
            try printer.push(is_last);
            try printPattern(printer, expr.data.For.pattern, false);
            try printExpr(printer, expr.data.For.iterator.*, false);
            try printBlock(printer, expr.data.For.body, true);
            printer.pop();
        },
        .Return => {
            try printer.printNode(is_last, "ReturnExpr");
            if (expr.data.Return.value == null) return;
            try printer.push(is_last);
            try printExpr(printer, expr.data.Return.value.?.*, true);
            printer.pop();
        },
        .Binary => {
            try printer.printNodeFmt(is_last, "Binary ({s})", .{@tagName(expr.data.Binary.op)});
            try printer.push(is_last);
            try printExpr(printer, expr.data.Binary.left.*, false);
            try printExpr(printer, expr.data.Binary.right.*, true);
            printer.pop();
        },
        .Unary => {
            try printer.printNodeFmt(is_last, "Unary ({s})", .{@tagName(expr.data.Unary.op)});
            try printer.push(is_last);
            try printExpr(printer, expr.data.Unary.expr.*, true);
            printer.pop();
        },
        .Call => {
            try printer.printNode(is_last, "Call");
            try printer.push(is_last);
            try printExpr(printer, expr.data.Call.callee.*, expr.data.Call.args.len == 0);
            for (expr.data.Call.args, 0..) |arg, idx| {
                try printExpr(printer, arg, idx == expr.data.Call.args.len - 1);
            }
            printer.pop();
        },
        .Index => {
            try printer.printNode(is_last, "Index");
            try printer.push(is_last);
            try printExpr(printer, expr.data.Index.target.*, false);
            try printExpr(printer, expr.data.Index.index.*, true);
            printer.pop();
        },
        .Field => {
            try printer.printNodeFmt(is_last, "Field {s}", .{expr.data.Field.field.name});
            try printer.push(is_last);
            try printExpr(printer, expr.data.Field.target.*, true);
            printer.pop();
        },
        .MethodCall => {
            try printer.printNodeFmt(is_last, "MethodCall {s}", .{expr.data.MethodCall.method.name});
            try printer.push(is_last);
            try printExpr(printer, expr.data.MethodCall.target.*, expr.data.MethodCall.args.len == 0);
            for (expr.data.MethodCall.args, 0..) |arg, idx| {
                try printExpr(printer, arg, idx == expr.data.MethodCall.args.len - 1);
            }
            printer.pop();
        },
        .Assignment => {
            try printer.printNodeFmt(is_last, "Assignment ({s})", .{@tagName(expr.data.Assignment.op)});
            try printer.push(is_last);
            try printExpr(printer, expr.data.Assignment.target.*, false);
            try printExpr(printer, expr.data.Assignment.value.*, true);
            printer.pop();
        },
        .Cast => {
            try printer.printNode(is_last, "Cast");
            try printer.push(is_last);
            try printExpr(printer, expr.data.Cast.expr.*, false);
            try printType(printer, expr.data.Cast.ty, true, "Type");
            printer.pop();
        },
        .Range => {
            try printer.printNodeFmt(is_last, "Range (inclusive: {})", .{expr.data.Range.inclusive});
            try printer.push(is_last);
            try printExpr(printer, expr.data.Range.start.*, false);
            try printExpr(printer, expr.data.Range.end.*, true);
            printer.pop();
        },
        .Lambda => {
            try printer.printNode(is_last, "Lambda");
            try printer.push(is_last);
            try printParams(printer, expr.data.Lambda.params, expr.data.Lambda.body == .Expr);
            switch (expr.data.Lambda.body) {
                .Expr => |body_expr| try printExpr(printer, body_expr.*, true),
                .Block => |body_block| try printBlock(printer, body_block, true),
            }
            printer.pop();
        },
        .Array => {
            try printer.printNode(is_last, "Array");
            try printer.push(is_last);
            for (expr.data.Array.elements, 0..) |element, idx| {
                try printExpr(printer, element, idx == expr.data.Array.elements.len - 1);
            }
            printer.pop();
        },
        .StructInit => {
            try printer.printNode(is_last, "StructInit");
            try printer.push(is_last);
            try printPath(printer, expr.data.StructInit.path, expr.data.StructInit.fields.len == 0);
            for (expr.data.StructInit.fields, 0..) |field, idx| {
                const last_field = idx == expr.data.StructInit.fields.len - 1;
                try printer.printNodeFmt(last_field, "Field {s}", .{field.name.name});
                try printer.push(last_field);
                try printExpr(printer, field.value, true);
                printer.pop();
            }
            printer.pop();
        },
        .Paren => {
            try printer.printNode(is_last, "Paren");
            try printer.push(is_last);
            try printExpr(printer, expr.data.Paren.*, true);
            printer.pop();
        },
    }
}

fn printType(printer: *TreePrinter, ty: *const ast.Type, is_last: bool, label: []const u8) AllocError!void {
    switch (ty.tag) {
        .Path => {
            try printer.printNodeFmt(is_last, "{s} Path", .{label});
            try printer.push(is_last);
            try printPath(printer, ty.data.Path, true);
            printer.pop();
        },
        .Primitive => try printer.printNodeFmt(is_last, "{s} Primitive {s}", .{ label, @tagName(ty.data.Primitive) }),
        .Array => {
            try printer.printNodeFmt(is_last, "{s} Array", .{label});
            try printer.push(is_last);
            try printType(printer, ty.data.Array.element_type, false, "Element");
            try printExpr(printer, ty.data.Array.size_expr.*, true);
            printer.pop();
        },
        .Pointer => {
            try printer.printNodeFmt(is_last, "{s} Pointer (mut: {})", .{ label, ty.data.Pointer.is_mut });
            try printer.push(is_last);
            try printType(printer, ty.data.Pointer.child, true, "Target");
            printer.pop();
        },
        .Reference => {
            try printer.printNodeFmt(is_last, "{s} Reference (mut: {})", .{ label, ty.data.Reference.is_mut });
            try printer.push(is_last);
            try printType(printer, ty.data.Reference.child, true, "Target");
            printer.pop();
        },
        .Function => {
            try printer.printNodeFmt(is_last, "{s} Function", .{label});
            try printer.push(is_last);
            for (ty.data.Function.params) |param_ty| {
                try printType(printer, &param_ty, false, "Param");
            }
            try printType(printer, ty.data.Function.return_type, true, "Return");
            printer.pop();
        },
    }
}

fn printPath(printer: *TreePrinter, path: ast.Path, is_last: bool) AllocError!void {
    try printer.printNode(is_last, "Path");
    if (path.segments.len == 0 and path.generic_args.len == 0) return;

    try printer.push(is_last);
    for (path.segments, 0..) |segment, idx| {
        const last_segment = idx == path.segments.len - 1 and path.generic_args.len == 0;
        try printer.printNodeFmt(last_segment, "Segment {s}", .{segment.name});
    }
    if (path.generic_args.len > 0) {
        try printer.printNode(true, "GenericArgs");
        try printer.push(true);
        for (path.generic_args, 0..) |arg, idx| {
            try printType(printer, &arg, idx == path.generic_args.len - 1, "Arg");
        }
        printer.pop();
    }
    printer.pop();
}
