const std = @import("std");
const diag = @import("../diag/diagnostics.zig");
const source_map = @import("../diag/source_map.zig");
const ast = @import("../frontend/ast.zig");
const name_res = @import("name_res.zig");
const typecheck = @import("typecheck.zig");

pub const DefId = u32;
pub const LocalId = u32;
pub const TypeId = u32;
pub const ExprId = u32;
pub const StmtId = u32;
pub const ItemId = u32;

pub const Type = struct {
    id: TypeId,
    kind: Kind,

    pub const Kind = union(enum) {
        PrimInt: enum { U32, U64, Usize, I32, I64 },
        PrimFloat: enum { F32, F64 },
        Bool,
        Char,
        Str,
        String,
        Array: struct { elem: TypeId, size_const: ?i64 },
        Pointer: struct { mutable: bool, inner: TypeId },
        Ref: struct { mutable: bool, inner: TypeId },
        Fn: struct { params: []TypeId, ret: TypeId },
        Struct: struct { def_id: DefId, type_args: []TypeId },
        Path: struct { segments: [][]const u8, args: []TypeId },
        Unknown,
    };
};

pub const Pattern = struct {
    id: LocalId,
    kind: Kind,
    span: Span,

    pub const Kind = union(enum) {
        Identifier: []const u8,
        Wildcard,
    };
};

pub const Expr = struct {
    id: ExprId,
    kind: Kind,
    ty: TypeId,
    span: Span,

    pub const Kind = union(enum) {
        LocalRef: LocalId,
        GlobalRef: DefId,
        ConstInt: i64,
        ConstFloat: f64,
        ConstBool: bool,
        ConstChar: u21,
        ConstString: []const u8,
        UnresolvedIdent: []const u8,
        Path: struct { segments: [][]const u8, args: []TypeId },
        Binary: struct { op: BinaryOp, lhs: ExprId, rhs: ExprId },
        Unary: struct { op: UnaryOp, expr: ExprId },
        Call: struct { callee: ExprId, args: []ExprId },
        MethodCall: struct { target: ExprId, name: []const u8, args: []ExprId },
        Assignment: struct { target: ExprId, op: AssignOp, value: ExprId },
        Return: ?ExprId,
        If: struct { cond: ExprId, then_branch: ExprId, else_branch: ?ExprId },
        While: struct { cond: ExprId, body: ExprId },
        For: struct { pat: Pattern, iter: ExprId, body: ExprId },
        Range: struct { inclusive: bool, start: ExprId, end: ExprId },
        Cast: struct { expr: ExprId, ty: TypeId },
        Index: struct { target: ExprId, index: ExprId },
        Field: struct { target: ExprId, name: []const u8 },
        Array: []ExprId,
        StructInit: StructInit,
        Lambda: struct { params: []Pattern, param_types: []TypeId, body: ExprId },
        Block: struct { stmts: []StmtId, tail: ?ExprId },
        Unknown,
    };
};

pub const StructInit = struct {
    path: [][]const u8,
    fields: []StructInitField,
};

pub const StructInitField = struct {
    name: []const u8,
    value: ExprId,
};

pub const BinaryOp = enum {
    LogicalOr,
    LogicalAnd,
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
    Add,
    Sub,
    Mul,
    Div,
    Mod,
};

pub const UnaryOp = enum {
    Not,
    Neg,
    Deref,
    Ref,
    RefMut,
};

pub const AssignOp = enum {
    Assign,
    AddAssign,
    SubAssign,
    MulAssign,
    DivAssign,
};

pub const Stmt = struct {
    id: StmtId,
    kind: Kind,
    span: Span,

    pub const Kind = union(enum) {
        Let: struct { pat: Pattern, ty: ?TypeId, value: ?ExprId },
        Expr: ExprId,
        Unknown,
    };
};

pub const Item = struct {
    id: ItemId,
    kind: Kind,
    span: Span,

    pub const Kind = union(enum) {
        Function: Function,
        Struct: Struct,
        TypeAlias: TypeAlias,
        Impl: Impl,
        Empty,
    };
};

pub const Function = struct {
    def_id: DefId,
    name: []const u8,
    params: []LocalId,
    param_types: []TypeId,
    return_type: ?TypeId,
    body: ?ExprId,
    span: Span,
};

pub const Struct = struct {
    def_id: DefId,
    name: []const u8,
    fields: []Field,
    span: Span,
};

pub const Field = struct {
    name: []const u8,
    ty: TypeId,
    span: Span,
};

pub const TypeAlias = struct {
    def_id: DefId,
    name: []const u8,
    target: TypeId,
    span: Span,
};

pub const Impl = struct {
    def_id: DefId,
    target: TypeId,
    methods: []ItemId,
    span: Span,
};

pub const Crate = struct {
    arena: std.heap.ArenaAllocator,
    items: std.ArrayListUnmanaged(Item),
    exprs: std.ArrayListUnmanaged(Expr),
    stmts: std.ArrayListUnmanaged(Stmt),
    types: std.ArrayListUnmanaged(Type),
    patterns: std.ArrayListUnmanaged(Pattern),

    pub fn init(backing_allocator: std.mem.Allocator) Crate {
        return .{
            .arena = std.heap.ArenaAllocator.init(backing_allocator),
            .items = .{},
            .exprs = .{},
            .stmts = .{},
            .types = .{},
            .patterns = .{},
        };
    }

    pub fn deinit(self: *Crate) void {
        self.items.deinit(self.arena.allocator());
        self.exprs.deinit(self.arena.allocator());
        self.stmts.deinit(self.arena.allocator());
        self.types.deinit(self.arena.allocator());
        self.patterns.deinit(self.arena.allocator());
        self.arena.deinit();
    }

    pub fn allocator(self: *Crate) std.mem.Allocator {
        return self.arena.allocator();
    }
};

pub const Span = source_map.Span;
const LowerError = std.mem.Allocator.Error || error{InvalidLiteral};

pub fn lowerFromAst(allocator: std.mem.Allocator, ast_crate: ast.Crate, diagnostics: *diag.Diagnostics) !Crate {
    var crate = Crate.init(allocator);
    var name_table = std.StringHashMap(Span).init(allocator);
    defer name_table.deinit();

    var next_type_id: TypeId = 0;

    for (ast_crate.items, 0..) |item, idx| {
        const def_id: DefId = @intCast(idx);
        switch (item.tag) {
            .Fn => try lowerFn(&crate, &name_table, def_id, item, diagnostics, &next_type_id),
            .Struct => try lowerStruct(&crate, &name_table, def_id, item, diagnostics, &next_type_id),
            .TypeAlias => try lowerTypeAlias(&crate, &name_table, def_id, item, diagnostics, &next_type_id),
            .Impl => try lowerImpl(&crate, def_id, item, &next_type_id),
            .Empty => try crate.items.append(crate.allocator(), .{ .id = @intCast(def_id), .kind = .Empty, .span = item.span }),
        }
    }

    return crate;
}

fn lowerFn(
    crate: *Crate,
    name_table: *std.StringHashMap(Span),
    def_id: DefId,
    item: ast.Item,
    diagnostics: *diag.Diagnostics,
    next_type_id: *TypeId,
) LowerError!void {
    const fn_data = item.data.Fn;
    const owned_name = try internName(crate, name_table, fn_data.name, diagnostics);
    var params_buffer = std.ArrayListUnmanaged(LocalId){};
    defer params_buffer.deinit(crate.allocator());
    var param_types = std.ArrayListUnmanaged(TypeId){};
    defer param_types.deinit(crate.allocator());

    for (fn_data.params) |param| {
        const local = try lowerPattern(crate, param.pattern, diagnostics);
        const ty_id = if (param.ty) |param_ty|
            try lowerType(crate, param_ty, diagnostics, next_type_id)
        else
            try appendUnknownType(crate, next_type_id);
        try params_buffer.append(crate.allocator(), local);
        try param_types.append(crate.allocator(), ty_id);
    }

    const return_ty = if (fn_data.return_type) |ret_ty|
        try lowerType(crate, ret_ty, diagnostics, next_type_id)
    else
        null;

    const body_expr = try lowerBlock(crate, fn_data.body, diagnostics, next_type_id);

    const fn_item = Function{
        .def_id = def_id,
        .name = owned_name,
        .params = try params_buffer.toOwnedSlice(crate.allocator()),
        .param_types = try param_types.toOwnedSlice(crate.allocator()),
        .return_type = return_ty,
        .body = body_expr,
        .span = fn_data.span,
    };
    try crate.items.append(crate.allocator(), .{ .id = @intCast(def_id), .kind = .{ .Function = fn_item }, .span = item.span });
}

fn lowerStruct(
    crate: *Crate,
    name_table: *std.StringHashMap(Span),
    def_id: DefId,
    item: ast.Item,
    diagnostics: *diag.Diagnostics,
    next_type_id: *TypeId,
) LowerError!void {
    const data = item.data.Struct;
    const owned_name = try internName(crate, name_table, data.name, diagnostics);
    var fields = std.ArrayListUnmanaged(Field){};
    defer fields.deinit(crate.allocator());

    for (data.fields) |field| {
        const field_ty = try lowerType(crate, field.ty, diagnostics, next_type_id);
        const name = try crate.allocator().dupe(u8, field.name.name);
        try fields.append(crate.allocator(), .{ .name = name, .ty = field_ty, .span = field.span });
    }

    const struct_item = Struct{
        .def_id = def_id,
        .name = owned_name,
        .fields = try fields.toOwnedSlice(crate.allocator()),
        .span = data.span,
    };
    try crate.items.append(crate.allocator(), .{ .id = @intCast(def_id), .kind = .{ .Struct = struct_item }, .span = item.span });
}

fn lowerTypeAlias(
    crate: *Crate,
    name_table: *std.StringHashMap(Span),
    def_id: DefId,
    item: ast.Item,
    diagnostics: *diag.Diagnostics,
    next_type_id: *TypeId,
) LowerError!void {
    const data = item.data.TypeAlias;
    const owned_name = try internName(crate, name_table, data.name, diagnostics);
    const target_type = try lowerType(crate, data.aliased_type, diagnostics, next_type_id);
    const alias_item = TypeAlias{
        .def_id = def_id,
        .name = owned_name,
        .target = target_type,
        .span = data.span,
    };
    try crate.items.append(crate.allocator(), .{ .id = @intCast(def_id), .kind = .{ .TypeAlias = alias_item }, .span = item.span });
}

fn lowerImpl(
    crate: *Crate,
    def_id: DefId,
    item: ast.Item,
    next_type_id: *TypeId,
) LowerError!void {
    const data = item.data.Impl;
    const target = try appendUnknownType(crate, next_type_id);
    const impl_item = Impl{
        .def_id = def_id,
        .target = target,
        .methods = &[_]ItemId{},
        .span = data.span,
    };
    try crate.items.append(crate.allocator(), .{ .id = @intCast(def_id), .kind = .{ .Impl = impl_item }, .span = item.span });
}

fn appendUnknownType(crate: *Crate, next_type_id: *TypeId) LowerError!TypeId {
    const id = next_type_id.*;
    next_type_id.* += 1;
    try crate.types.append(crate.allocator(), .{ .id = id, .kind = .Unknown });
    return id;
}

fn lowerType(crate: *Crate, ty: ast.Type, diagnostics: *diag.Diagnostics, next_type_id: *TypeId) LowerError!TypeId {
    const id = next_type_id.*;
    next_type_id.* += 1;

    if (crate.types.items.len != id) {
        std.debug.panic("type id mismatch: expected {d}, have {d}", .{ id, crate.types.items.len });
    }

    try crate.types.append(crate.allocator(), .{ .id = id, .kind = .Unknown });

    const kind: Type.Kind = switch (ty.tag) {
        .Primitive => switch (ty.data.Primitive) {
            .u32 => .{ .PrimInt = .U32 },
            .u64 => .{ .PrimInt = .U64 },
            .usize => .{ .PrimInt = .Usize },
            .i32 => .{ .PrimInt = .I32 },
            .i64 => .{ .PrimInt = .I64 },
            .f32 => .{ .PrimFloat = .F32 },
            .f64 => .{ .PrimFloat = .F64 },
            .bool => .Bool,
            .char => .Char,
            .str => .Str,
            .String => .String,
        },
        .Pointer => blk: {
            const inner = try lowerType(crate, ty.data.Pointer.child.*, diagnostics, next_type_id);
            break :blk .{ .Pointer = .{ .mutable = ty.data.Pointer.is_mut, .inner = inner } };
        },
        .Reference => blk: {
            const inner = try lowerType(crate, ty.data.Reference.child.*, diagnostics, next_type_id);
            break :blk .{ .Ref = .{ .mutable = ty.data.Reference.is_mut, .inner = inner } };
        },
        .Array => blk: {
            const elem = try lowerType(crate, ty.data.Array.element_type.*, diagnostics, next_type_id);
            const size_const = try evalArraySize(ty.data.Array.size_expr.*);
            break :blk .{ .Array = .{ .elem = elem, .size_const = size_const } };
        },
        .Function => blk: {
            var params = std.ArrayListUnmanaged(TypeId){};
            defer params.deinit(crate.allocator());
            for (ty.data.Function.params) |param_ty| {
                try params.append(crate.allocator(), try lowerType(crate, param_ty, diagnostics, next_type_id));
            }
            const ret = try lowerType(crate, ty.data.Function.return_type.*, diagnostics, next_type_id);
            break :blk .{ .Fn = .{ .params = try params.toOwnedSlice(crate.allocator()), .ret = ret } };
        },
        .Path => blk: {
            var segments = try crate.allocator().alloc([]const u8, ty.data.Path.segments.len);
            for (ty.data.Path.segments, 0..) |seg, idx| {
                segments[idx] = try crate.allocator().dupe(u8, seg.name);
            }

            var args = std.ArrayListUnmanaged(TypeId){};
            defer args.deinit(crate.allocator());
            for (ty.data.Path.generic_args) |arg_ty| {
                try args.append(crate.allocator(), try lowerType(crate, arg_ty, diagnostics, next_type_id));
            }

            break :blk .{ .Path = .{ .segments = segments, .args = try args.toOwnedSlice(crate.allocator()) } };
        },
    };
    crate.types.items[id].kind = kind;
    return id;
}

fn evalArraySize(expr: ast.Expr) !?i64 {
    switch (expr.tag) {
        .Literal => {
            if (expr.data.Literal.kind == .Int) {
                return std.fmt.parseInt(i64, expr.data.Literal.lexeme, 10) catch null;
            }
            return null;
        },
        else => return null,
    }
}

fn lowerPattern(crate: *Crate, pattern: ast.Pattern, diagnostics: *diag.Diagnostics) LowerError!LocalId {
    _ = diagnostics;
    const id: LocalId = @intCast(crate.patterns.items.len);
    const kind: Pattern.Kind = switch (pattern.tag) {
        .Identifier => .{ .Identifier = try crate.allocator().dupe(u8, pattern.data.Identifier.name) },
        .Wildcard => .Wildcard,
    };
    try crate.patterns.append(crate.allocator(), .{ .id = id, .kind = kind, .span = pattern.span });
    return id;
}

fn lowerExpr(crate: *Crate, expr: ast.Expr, diagnostics: *diag.Diagnostics, next_type_id: *TypeId) LowerError!ExprId {
    switch (expr.tag) {
        .Literal => {
            const lit = expr.data.Literal;
            const ty = try literalType(crate, lit, diagnostics, next_type_id);
            const kind: Expr.Kind = switch (lit.kind) {
                .Int => .{ .ConstInt = std.fmt.parseInt(i64, lit.lexeme, 10) catch 0 },
                .Float => .{ .ConstFloat = std.fmt.parseFloat(f64, lit.lexeme) catch 0 },
                .Bool => .{ .ConstBool = std.mem.eql(u8, lit.lexeme, "true") },
                .Char => blk: {
                    const codepoint = try parseCharLiteral(lit.lexeme);
                    break :blk .{ .ConstChar = codepoint };
                },
                .String => .{ .ConstString = lit.lexeme },
            };

            const id: ExprId = @intCast(crate.exprs.items.len);
            try crate.exprs.append(crate.allocator(), .{ .id = id, .kind = kind, .ty = ty, .span = expr.span });
            return id;
        },
        .Binary => {
            const lhs = try lowerExpr(crate, expr.data.Binary.left.*, diagnostics, next_type_id);
            const rhs = try lowerExpr(crate, expr.data.Binary.right.*, diagnostics, next_type_id);
            const kind: Expr.Kind = .{ .Binary = .{ .op = mapBinaryOp(expr.data.Binary.op), .lhs = lhs, .rhs = rhs } };
            const id: ExprId = @intCast(crate.exprs.items.len);
            try crate.exprs.append(crate.allocator(), .{
                .id = id,
                .kind = kind,
                .ty = try appendUnknownType(crate, next_type_id),
                .span = expr.span,
            });
            return id;
        },
        .Unary => {
            const operand = try lowerExpr(crate, expr.data.Unary.expr.*, diagnostics, next_type_id);
            const op = switch (expr.data.Unary.op) {
                .Not => UnaryOp.Not,
                .Neg => UnaryOp.Neg,
                .Deref => UnaryOp.Deref,
                .Ref => UnaryOp.Ref,
                .RefMut => UnaryOp.RefMut,
            };
            const id: ExprId = @intCast(crate.exprs.items.len);
            try crate.exprs.append(crate.allocator(), .{
                .id = id,
                .kind = .{ .Unary = .{ .op = op, .expr = operand } },
                .ty = try appendUnknownType(crate, next_type_id),
                .span = expr.span,
            });
            return id;
        },
        .Call => {
            const callee = try lowerExpr(crate, expr.data.Call.callee.*, diagnostics, next_type_id);
            var args = std.ArrayListUnmanaged(ExprId){};
            defer args.deinit(crate.allocator());
            for (expr.data.Call.args) |arg| {
                try args.append(crate.allocator(), try lowerExpr(crate, arg, diagnostics, next_type_id));
            }
            const id: ExprId = @intCast(crate.exprs.items.len);
            try crate.exprs.append(crate.allocator(), .{
                .id = id,
                .kind = .{ .Call = .{ .callee = callee, .args = try args.toOwnedSlice(crate.allocator()) } },
                .ty = try appendUnknownType(crate, next_type_id),
                .span = expr.span,
            });
            return id;
        },
        .Assignment => {
            const lhs = try lowerExpr(crate, expr.data.Assignment.target.*, diagnostics, next_type_id);
            const rhs = try lowerExpr(crate, expr.data.Assignment.value.*, diagnostics, next_type_id);
            const id: ExprId = @intCast(crate.exprs.items.len);
            try crate.exprs.append(crate.allocator(), .{
                .id = id,
                .kind = .{ .Assignment = .{ .target = lhs, .op = mapAssignOp(expr.data.Assignment.op), .value = rhs } },
                .ty = try appendUnknownType(crate, next_type_id),
                .span = expr.span,
            });
            return id;
        },
        .Return => {
            const value = if (expr.data.Return.value) |val|
                try lowerExpr(crate, val.*, diagnostics, next_type_id)
            else
                null;
            const id: ExprId = @intCast(crate.exprs.items.len);
            try crate.exprs.append(crate.allocator(), .{ .id = id, .kind = .{ .Return = value }, .ty = try appendUnknownType(crate, next_type_id), .span = expr.span });
            return id;
        },
        .If => {
            const cond = try lowerExpr(crate, expr.data.If.condition.*, diagnostics, next_type_id);
            const then_branch = try lowerBlock(crate, expr.data.If.then_block, diagnostics, next_type_id);
            const else_branch = if (expr.data.If.else_expr) |else_ptr|
                try lowerExpr(crate, else_ptr.*, diagnostics, next_type_id)
            else
                null;
            const id: ExprId = @intCast(crate.exprs.items.len);
            try crate.exprs.append(crate.allocator(), .{
                .id = id,
                .kind = .{ .If = .{ .cond = cond, .then_branch = then_branch, .else_branch = else_branch } },
                .ty = try appendUnknownType(crate, next_type_id),
                .span = expr.span,
            });
            return id;
        },
        .While => {
            const cond = try lowerExpr(crate, expr.data.While.condition.*, diagnostics, next_type_id);
            const body = try lowerBlock(crate, expr.data.While.body, diagnostics, next_type_id);
            const id: ExprId = @intCast(crate.exprs.items.len);
            try crate.exprs.append(crate.allocator(), .{ .id = id, .kind = .{ .While = .{ .cond = cond, .body = body } }, .ty = try appendUnknownType(crate, next_type_id), .span = expr.span });
            return id;
        },
        .Range => {
            const start = try lowerExpr(crate, expr.data.Range.start.*, diagnostics, next_type_id);
            const end = try lowerExpr(crate, expr.data.Range.end.*, diagnostics, next_type_id);
            const id: ExprId = @intCast(crate.exprs.items.len);
            try crate.exprs.append(crate.allocator(), .{
                .id = id,
                .kind = .{ .Range = .{ .inclusive = expr.data.Range.inclusive, .start = start, .end = end } },
                .ty = try appendUnknownType(crate, next_type_id),
                .span = expr.span,
            });
            return id;
        },
        .Cast => {
            const inner = try lowerExpr(crate, expr.data.Cast.expr.*, diagnostics, next_type_id);
            const ty = try lowerType(crate, expr.data.Cast.ty.*, diagnostics, next_type_id);
            const id: ExprId = @intCast(crate.exprs.items.len);
            try crate.exprs.append(crate.allocator(), .{ .id = id, .kind = .{ .Cast = .{ .expr = inner, .ty = ty } }, .ty = ty, .span = expr.span });
            return id;
        },
        .Index => {
            const target = try lowerExpr(crate, expr.data.Index.target.*, diagnostics, next_type_id);
            const index = try lowerExpr(crate, expr.data.Index.index.*, diagnostics, next_type_id);
            const id: ExprId = @intCast(crate.exprs.items.len);
            try crate.exprs.append(crate.allocator(), .{ .id = id, .kind = .{ .Index = .{ .target = target, .index = index } }, .ty = try appendUnknownType(crate, next_type_id), .span = expr.span });
            return id;
        },
        .Field => {
            const target = try lowerExpr(crate, expr.data.Field.target.*, diagnostics, next_type_id);
            const name = try crate.allocator().dupe(u8, expr.data.Field.field.name);
            const id: ExprId = @intCast(crate.exprs.items.len);
            try crate.exprs.append(crate.allocator(), .{ .id = id, .kind = .{ .Field = .{ .target = target, .name = name } }, .ty = try appendUnknownType(crate, next_type_id), .span = expr.span });
            return id;
        },
        .Array => {
            var elements = std.ArrayListUnmanaged(ExprId){};
            defer elements.deinit(crate.allocator());
            for (expr.data.Array.elements) |elem| {
                try elements.append(crate.allocator(), try lowerExpr(crate, elem, diagnostics, next_type_id));
            }
            const id: ExprId = @intCast(crate.exprs.items.len);
            try crate.exprs.append(crate.allocator(), .{
                .id = id,
                .kind = .{ .Array = try elements.toOwnedSlice(crate.allocator()) },
                .ty = try appendUnknownType(crate, next_type_id),
                .span = expr.span,
            });
            return id;
        },
        .StructInit => {
            const path_segments = try crate.allocator().alloc([]const u8, expr.data.StructInit.path.segments.len);
            for (expr.data.StructInit.path.segments, 0..) |seg, idx| {
                path_segments[idx] = try crate.allocator().dupe(u8, seg.name);
            }

            var fields = std.ArrayListUnmanaged(StructInitField){};
            defer fields.deinit(crate.allocator());
            for (expr.data.StructInit.fields) |field| {
                const value_id = try lowerExpr(crate, field.value, diagnostics, next_type_id);
                const name = try crate.allocator().dupe(u8, field.name.name);
                try fields.append(crate.allocator(), .{ .name = name, .value = value_id });
            }

            const id: ExprId = @intCast(crate.exprs.items.len);
            try crate.exprs.append(crate.allocator(), .{
                .id = id,
                .kind = .{ .StructInit = .{ .path = path_segments, .fields = try fields.toOwnedSlice(crate.allocator()) } },
                .ty = try appendUnknownType(crate, next_type_id),
                .span = expr.span,
            });
            return id;
        },
        .For => {
            const pat_id = try lowerPattern(crate, expr.data.For.pattern, diagnostics);
            const iter = try lowerExpr(crate, expr.data.For.iterator.*, diagnostics, next_type_id);
            const body = try lowerBlock(crate, expr.data.For.body, diagnostics, next_type_id);
            const id: ExprId = @intCast(crate.exprs.items.len);
            try crate.exprs.append(crate.allocator(), .{
                .id = id,
                .kind = .{ .For = .{ .pat = crate.patterns.items[pat_id], .iter = iter, .body = body } },
                .ty = try appendUnknownType(crate, next_type_id),
                .span = expr.span,
            });
            return id;
        },
        .MethodCall => {
            const target = try lowerExpr(crate, expr.data.MethodCall.target.*, diagnostics, next_type_id);
            var args = std.ArrayListUnmanaged(ExprId){};
            defer args.deinit(crate.allocator());
            for (expr.data.MethodCall.args) |arg| {
                try args.append(crate.allocator(), try lowerExpr(crate, arg, diagnostics, next_type_id));
            }
            const name = try crate.allocator().dupe(u8, expr.data.MethodCall.method.name);
            const id: ExprId = @intCast(crate.exprs.items.len);
            try crate.exprs.append(crate.allocator(), .{
                .id = id,
                .kind = .{ .MethodCall = .{ .target = target, .name = name, .args = try args.toOwnedSlice(crate.allocator()) } },
                .ty = try appendUnknownType(crate, next_type_id),
                .span = expr.span,
            });
            return id;
        },
        .Lambda => {
            var params = std.ArrayListUnmanaged(Pattern){};
            defer params.deinit(crate.allocator());
            var param_types = std.ArrayListUnmanaged(TypeId){};
            defer param_types.deinit(crate.allocator());
            for (expr.data.Lambda.params) |param| {
                const pat_id = try lowerPattern(crate, param.pattern, diagnostics);
                const ty_id = if (param.ty) |param_ty|
                    try lowerType(crate, param_ty, diagnostics, next_type_id)
                else
                    try appendUnknownType(crate, next_type_id);
                try params.append(crate.allocator(), crate.patterns.items[pat_id]);
                try param_types.append(crate.allocator(), ty_id);
            }

            const body_expr = switch (expr.data.Lambda.body) {
                .Expr => |e| try lowerExpr(crate, e.*, diagnostics, next_type_id),
                .Block => |b| try lowerBlock(crate, b, diagnostics, next_type_id),
            };

            const id: ExprId = @intCast(crate.exprs.items.len);
            try crate.exprs.append(crate.allocator(), .{
                .id = id,
                .kind = .{ .Lambda = .{
                    .params = try params.toOwnedSlice(crate.allocator()),
                    .param_types = try param_types.toOwnedSlice(crate.allocator()),
                    .body = body_expr,
                } },
                .ty = try appendUnknownType(crate, next_type_id),
                .span = expr.span,
            });
            return id;
        },
        .Paren => return try lowerExpr(crate, expr.data.Paren.*, diagnostics, next_type_id),
        .Block => {
            return try lowerBlock(crate, expr.data.Block, diagnostics, next_type_id);
        },
        .Path => {
            var segments = try crate.allocator().alloc([]const u8, expr.data.Path.segments.len);
            for (expr.data.Path.segments, 0..) |seg, idx| {
                segments[idx] = try crate.allocator().dupe(u8, seg.name);
            }

            var args = std.ArrayListUnmanaged(TypeId){};
            defer args.deinit(crate.allocator());
            for (expr.data.Path.generic_args) |arg_ty| {
                try args.append(crate.allocator(), try lowerType(crate, arg_ty, diagnostics, next_type_id));
            }

            const id: ExprId = @intCast(crate.exprs.items.len);
            try crate.exprs.append(crate.allocator(), .{
                .id = id,
                .kind = .{ .Path = .{ .segments = segments, .args = try args.toOwnedSlice(crate.allocator()) } },
                .ty = try appendUnknownType(crate, next_type_id),
                .span = expr.span,
            });
            return id;
        },
    }
}

fn lowerBlock(crate: *Crate, block: ast.Block, diagnostics: *diag.Diagnostics, next_type_id: *TypeId) LowerError!ExprId {
    var stmts = std.ArrayListUnmanaged(StmtId){};
    defer stmts.deinit(crate.allocator());

    for (block.stmts) |stmt| {
        try stmts.append(crate.allocator(), try lowerStmt(crate, stmt, diagnostics, next_type_id));
    }

    const tail_expr_id = if (block.result) |expr_ptr|
        try lowerExpr(crate, expr_ptr.*, diagnostics, next_type_id)
    else
        null;

    const id: ExprId = @intCast(crate.exprs.items.len);
    try crate.exprs.append(crate.allocator(), .{
        .id = id,
        .kind = .{ .Block = .{ .stmts = try stmts.toOwnedSlice(crate.allocator()), .tail = tail_expr_id } },
        .ty = try appendUnknownType(crate, next_type_id),
        .span = block.span,
    });
    return id;
}

fn lowerStmt(crate: *Crate, stmt: ast.Stmt, diagnostics: *diag.Diagnostics, next_type_id: *TypeId) LowerError!StmtId {
    const kind: Stmt.Kind = switch (stmt.tag) {
        .Let => blk: {
            const let_data = stmt.data.Let;
            const pat_id = try lowerPattern(crate, let_data.pattern, diagnostics);
            const ty = if (let_data.ty) |t|
                try lowerType(crate, t, diagnostics, next_type_id)
            else
                null;
            const value = if (let_data.value) |v|
                try lowerExpr(crate, v.*, diagnostics, next_type_id)
            else
                null;
            break :blk .{ .Let = .{ .pat = crate.patterns.items[pat_id], .ty = ty, .value = value } };
        },
        .Expr => .{ .Expr = try lowerExpr(crate, stmt.data.Expr.expr, diagnostics, next_type_id) },
        .Return => blk: {
            const expr_node = ast.Expr{ .tag = .Return, .span = stmt.span, .data = .{ .Return = stmt.data.Return } };
            break :blk .{ .Expr = try lowerExpr(crate, expr_node, diagnostics, next_type_id) };
        },
        .If => blk: {
            const expr_node = ast.Expr{ .tag = .If, .span = stmt.span, .data = .{ .If = stmt.data.If } };
            break :blk .{ .Expr = try lowerExpr(crate, expr_node, diagnostics, next_type_id) };
        },
        .While => blk: {
            const expr_node = ast.Expr{ .tag = .While, .span = stmt.span, .data = .{ .While = stmt.data.While } };
            break :blk .{ .Expr = try lowerExpr(crate, expr_node, diagnostics, next_type_id) };
        },
        .For => blk: {
            const expr_node = ast.Expr{ .tag = .For, .span = stmt.span, .data = .{ .For = stmt.data.For } };
            break :blk .{ .Expr = try lowerExpr(crate, expr_node, diagnostics, next_type_id) };
        },
        .Empty => .Unknown,
    };

    const id: StmtId = @intCast(crate.stmts.items.len);
    try crate.stmts.append(crate.allocator(), .{ .id = id, .kind = kind, .span = stmt.span });
    return id;
}

fn literalType(crate: *Crate, lit: ast.Literal, diagnostics: *diag.Diagnostics, next_type_id: *TypeId) LowerError!TypeId {
    _ = diagnostics;
    const ty_id = next_type_id.*;
    next_type_id.* += 1;
    const kind: Type.Kind = switch (lit.kind) {
        .Int => .{ .PrimInt = .I64 },
        .Float => .{ .PrimFloat = .F64 },
        .Bool => .Bool,
        .Char => .Char,
        .String => .String,
    };
    try crate.types.append(crate.allocator(), .{ .id = ty_id, .kind = kind });
    return ty_id;
}

fn mapBinaryOp(op: ast.BinaryOp) BinaryOp {
    return switch (op) {
        .LogicalOr => .LogicalOr,
        .LogicalAnd => .LogicalAnd,
        .Eq => .Eq,
        .Ne => .Ne,
        .Lt => .Lt,
        .Le => .Le,
        .Gt => .Gt,
        .Ge => .Ge,
        .Add => .Add,
        .Sub => .Sub,
        .Mul => .Mul,
        .Div => .Div,
        .Mod => .Mod,
    };
}

fn mapAssignOp(op: ast.AssignOp) AssignOp {
    return switch (op) {
        .Assign => .Assign,
        .AddAssign => .AddAssign,
        .SubAssign => .SubAssign,
        .MulAssign => .MulAssign,
        .DivAssign => .DivAssign,
    };
}

fn parseCharLiteral(lexeme: []const u8) !u21 {
    if (lexeme.len == 0)
        return error.InvalidLiteral;
    // Naive parsing assuming lexeme already stripped of quotes.
    var iter = std.unicode.Utf8Iterator{ .bytes = lexeme, .i = 0 };
    if (iter.nextCodepointSlice()) |cp| {
        return std.unicode.utf8Decode(cp) catch return error.InvalidLiteral;
    }
    return error.InvalidLiteral;
}

fn appendUnknownExpr(crate: *Crate, span: Span) LowerError!ExprId {
    const id: ExprId = @intCast(crate.exprs.items.len);
    try crate.exprs.append(crate.allocator(), .{ .id = id, .kind = .Unknown, .ty = 0, .span = span });
    return id;
}

fn internName(
    crate: *Crate,
    name_table: *std.StringHashMap(Span),
    identifier: ast.Identifier,
    diagnostics: *diag.Diagnostics,
) ![]const u8 {
    const existing = name_table.get(identifier.name);
    if (existing) |prev_span| {
        const message = std.fmt.allocPrint(
            crate.allocator(),
            "duplicate item `{s}` previously defined at byte offsets {d}-{d}",
            .{ identifier.name, prev_span.start, prev_span.end },
        ) catch identifier.name;
        diagnostics.reportError(identifier.span, message);
        if (message.ptr != identifier.name.ptr) {
            crate.allocator().free(message);
        }
    }

    const owned = try crate.allocator().dupe(u8, identifier.name);
    try name_table.put(owned, identifier.span);
    return owned;
}

// Placeholder name resolution and type checking entry points.
pub fn performNameResolution(crate: *Crate, diagnostics: *diag.Diagnostics) !void {
    try name_res.resolve(crate, diagnostics);
}

pub fn performTypeCheck(crate: *Crate, diagnostics: *diag.Diagnostics) !void {
    try typecheck.typecheck(crate, diagnostics);
}

pub fn emptySpan(file_id: source_map.FileId) Span {
    return .{ .file_id = file_id, .start = 0, .end = 0 };
}

fn buildDummyBlock(crate: *Crate, span: Span) !ExprId {
    var stmt_ids = std.ArrayListUnmanaged(StmtId){};
    defer stmt_ids.deinit(crate.allocator());

    const block_expr_id: ExprId = @intCast(crate.exprs.items.len);
    try crate.exprs.append(crate.allocator(), .{
        .id = block_expr_id,
        .kind = .{ .Block = .{ .stmts = try stmt_ids.toOwnedSlice(crate.allocator()), .tail = null } },
        .ty = 0,
        .span = span,
    });

    return block_expr_id;
}

test "lowerFromAst builds crate and reports duplicates" {
    const allocator = std.testing.allocator;
    var diagnostics = diag.Diagnostics.init(allocator);
    defer diagnostics.deinit();

    var ast_arena = std.heap.ArenaAllocator.init(allocator);
    defer ast_arena.deinit();
    const ast_alloc = ast_arena.allocator();

    const dummy_span = Span{ .file_id = 0, .start = 0, .end = 1 };

    var items = try ast_alloc.alloc(ast.Item, 2);
    items[0] = .{
        .tag = .Fn,
        .span = dummy_span,
        .data = .{ .Fn = .{
            .name = .{ .name = "main", .span = dummy_span },
            .generics = &[_]ast.Identifier{},
            .params = &[_]ast.Param{},
            .return_type = null,
            .body = .{ .stmts = &[_]ast.Stmt{}, .result = null, .span = dummy_span },
            .span = dummy_span,
        } },
    };
    items[1] = .{
        .tag = .Fn,
        .span = dummy_span,
        .data = .{ .Fn = .{
            .name = .{ .name = "main", .span = dummy_span },
            .generics = &[_]ast.Identifier{},
            .params = &[_]ast.Param{},
            .return_type = null,
            .body = .{ .stmts = &[_]ast.Stmt{}, .result = null, .span = dummy_span },
            .span = dummy_span,
        } },
    };

    var lowered = try lowerFromAst(allocator, .{ .items = items }, &diagnostics);
    defer lowered.deinit();

    try std.testing.expectEqual(@as(usize, 2), lowered.items.items.len);
    try std.testing.expect(diagnostics.hasErrors());
}

test "lowerFromAst lowers function params return type and body" {
    const allocator = std.testing.allocator;
    var diagnostics = diag.Diagnostics.init(allocator);
    defer diagnostics.deinit();

    var ast_arena = std.heap.ArenaAllocator.init(allocator);
    defer ast_arena.deinit();
    const ast_alloc = ast_arena.allocator();

    const span = emptySpan(0);
    const param_pattern = ast.Pattern{ .tag = .Identifier, .span = span, .data = .{ .Identifier = .{ .name = "x", .span = span } } };
    const param_ty = ast.Type{ .tag = .Primitive, .span = span, .data = .{ .Primitive = .u32 } };
    const ret_ty = ast.Type{ .tag = .Primitive, .span = span, .data = .{ .Primitive = .i64 } };

    const literal_expr_ptr = try ast_alloc.create(ast.Expr);
    literal_expr_ptr.* = .{ .tag = .Literal, .span = span, .data = .{ .Literal = .{ .kind = .Int, .lexeme = "42" } } };

    var params = [_]ast.Param{.{ .pattern = param_pattern, .ty = param_ty, .span = span, .kind = .Normal }};
    var items = try ast_alloc.alloc(ast.Item, 1);
    items[0] = .{
        .tag = .Fn,
        .span = span,
        .data = .{ .Fn = .{
            .name = .{ .name = "foo", .span = span },
            .generics = &[_]ast.Identifier{},
            .params = params[0..],
            .return_type = ret_ty,
            .body = .{ .stmts = &[_]ast.Stmt{}, .result = literal_expr_ptr, .span = span },
            .span = span,
        } },
    };

    var lowered = try lowerFromAst(allocator, .{ .items = items }, &diagnostics);
    defer lowered.deinit();

    try std.testing.expect(!diagnostics.hasErrors());
    const fn_item = lowered.items.items[0].kind.Function;
    try std.testing.expectEqual(@as(usize, 1), fn_item.params.len);
    try std.testing.expect(fn_item.return_type != null);
    try std.testing.expect(fn_item.body != null);

    const param = lowered.patterns.items[fn_item.params[0]];
    try std.testing.expect(param.kind == .Identifier);
    try std.testing.expect(lowered.types.items[fn_item.return_type.?].kind != .Unknown);

    const body_expr = lowered.exprs.items[fn_item.body.?];
    switch (body_expr.kind) {
        .Block => |block| {
            try std.testing.expect(block.tail != null);
            const literal_expr = lowered.exprs.items[block.tail.?];
            try std.testing.expect(literal_expr.kind == .ConstInt);
        },
        else => try std.testing.expect(false),
    }
}

test "lowerStruct and lowerTypeAlias preserve type information" {
    const allocator = std.testing.allocator;
    var diagnostics = diag.Diagnostics.init(allocator);
    defer diagnostics.deinit();

    var ast_arena = std.heap.ArenaAllocator.init(allocator);
    defer ast_arena.deinit();
    const ast_alloc = ast_arena.allocator();

    const span = emptySpan(0);
    const bool_type = ast.Type{ .tag = .Primitive, .span = span, .data = .{ .Primitive = .bool } };
    const alias_inner = ast.Type{ .tag = .Pointer, .span = span, .data = .{ .Pointer = .{ .is_mut = true, .child = try ast_alloc.create(ast.Type) } } };
    alias_inner.data.Pointer.child.* = ast.Type{ .tag = .Primitive, .span = span, .data = .{ .Primitive = .u64 } };
    var fields = [_]ast.Field{.{ .name = .{ .name = "x", .span = span }, .ty = bool_type, .span = span }};

    var items = try ast_alloc.alloc(ast.Item, 2);
    items[0] = .{
        .tag = .Struct,
        .span = span,
        .data = .{ .Struct = .{
            .name = .{ .name = "Point", .span = span },
            .generics = &[_]ast.Identifier{},
            .fields = fields[0..],
            .span = span,
        } },
    };
    items[1] = .{
        .tag = .TypeAlias,
        .span = span,
        .data = .{ .TypeAlias = .{
            .name = .{ .name = "Alias", .span = span },
            .generics = &[_]ast.Identifier{},
            .aliased_type = alias_inner,
            .span = span,
        } },
    };

    var lowered = try lowerFromAst(allocator, .{ .items = items }, &diagnostics);
    defer lowered.deinit();

    try std.testing.expect(!diagnostics.hasErrors());
    const struct_item = lowered.items.items[0].kind.Struct;
    try std.testing.expectEqual(@as(usize, 1), struct_item.fields.len);
    try std.testing.expect(lowered.types.items[struct_item.fields[0].ty].kind != .Unknown);

    const alias_item = lowered.items.items[1].kind.TypeAlias;
    try std.testing.expect(lowered.types.items[alias_item.target].kind != .Unknown);
}

test "lowerExpr lowers binary and return statements" {
    const allocator = std.testing.allocator;
    var diagnostics = diag.Diagnostics.init(allocator);
    defer diagnostics.deinit();

    var ast_arena = std.heap.ArenaAllocator.init(allocator);
    defer ast_arena.deinit();
    const ast_alloc = ast_arena.allocator();

    const span = emptySpan(0);
    const one_ptr = try ast_alloc.create(ast.Expr);
    one_ptr.* = .{ .tag = .Literal, .span = span, .data = .{ .Literal = .{ .kind = .Int, .lexeme = "1" } } };
    const two_ptr = try ast_alloc.create(ast.Expr);
    two_ptr.* = .{ .tag = .Literal, .span = span, .data = .{ .Literal = .{ .kind = .Int, .lexeme = "2" } } };
    const bin_ptr = try ast_alloc.create(ast.Expr);
    bin_ptr.* = .{ .tag = .Binary, .span = span, .data = .{ .Binary = .{ .op = .Add, .left = one_ptr, .right = two_ptr } } };

    var stmts = [_]ast.Stmt{
        .{ .tag = .Expr, .span = span, .data = .{ .Expr = .{ .expr = bin_ptr.* } } },
        .{ .tag = .Return, .span = span, .data = .{ .Return = .{ .value = bin_ptr } } },
    };

    var items = try ast_alloc.alloc(ast.Item, 1);
    items[0] = .{
        .tag = .Fn,
        .span = span,
        .data = .{ .Fn = .{
            .name = .{ .name = "main", .span = span },
            .generics = &[_]ast.Identifier{},
            .params = &[_]ast.Param{},
            .return_type = null,
            .body = .{ .stmts = stmts[0..], .result = null, .span = span },
            .span = span,
        } },
    };

    var lowered = try lowerFromAst(allocator, .{ .items = items }, &diagnostics);
    defer lowered.deinit();

    try std.testing.expect(!diagnostics.hasErrors());
    const body_expr = lowered.items.items[0].kind.Function.body.?;
    const block = lowered.exprs.items[body_expr].kind.Block;
    try std.testing.expectEqual(@as(usize, 2), block.stmts.len);

    const return_stmt = lowered.stmts.items[block.stmts[1]].kind.Expr;
    const ret_expr = lowered.exprs.items[return_stmt];
    try std.testing.expect(ret_expr.kind == .Return);
    const ret_val_id = ret_expr.kind.Return.?;
    const bin_expr = lowered.exprs.items[ret_val_id];
    try std.testing.expect(bin_expr.kind == .Binary);
}
