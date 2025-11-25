const std = @import("std");
const diag = @import("../diag/diagnostics.zig");
const source_map = @import("../diag/source_map.zig");
const ast = @import("../frontend/ast.zig");

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
        Block: struct { stmts: []StmtId, tail: ?ExprId },
        Unknown,
    };
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

const Span = source_map.Span;

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
) !void {
    const fn_data = item.data.Fn;
    const owned_name = try internName(crate, name_table, fn_data.name, diagnostics);
    const fn_item = Function{
        .def_id = def_id,
        .name = owned_name,
        .params = &[_]LocalId{},
        .return_type = null,
        .body = null,
        .span = fn_data.span,
    };
    try crate.items.append(crate.allocator(), .{ .id = @intCast(def_id), .kind = .{ .Function = fn_item }, .span = item.span });

    _ = try appendUnknownType(crate, next_type_id);
    _ = try appendUnknownExpr(crate, item.span);
}

fn lowerStruct(
    crate: *Crate,
    name_table: *std.StringHashMap(Span),
    def_id: DefId,
    item: ast.Item,
    diagnostics: *diag.Diagnostics,
    next_type_id: *TypeId,
) !void {
    const data = item.data.Struct;
    const owned_name = try internName(crate, name_table, data.name, diagnostics);
    const struct_item = Struct{
        .def_id = def_id,
        .name = owned_name,
        .fields = &[_]Field{},
        .span = data.span,
    };
    try crate.items.append(crate.allocator(), .{ .id = @intCast(def_id), .kind = .{ .Struct = struct_item }, .span = item.span });

    _ = try appendUnknownType(crate, next_type_id);
}

fn lowerTypeAlias(
    crate: *Crate,
    name_table: *std.StringHashMap(Span),
    def_id: DefId,
    item: ast.Item,
    diagnostics: *diag.Diagnostics,
    next_type_id: *TypeId,
) !void {
    const data = item.data.TypeAlias;
    const owned_name = try internName(crate, name_table, data.name, diagnostics);
    const target_type = try appendUnknownType(crate, next_type_id);
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
) !void {
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

fn appendUnknownType(crate: *Crate, next_type_id: *TypeId) !TypeId {
    const id = next_type_id.*;
    next_type_id.* += 1;
    try crate.types.append(crate.allocator(), .{ .id = id, .kind = .Unknown });
    return id;
}

fn appendUnknownExpr(crate: *Crate, span: Span) !ExprId {
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
    _ = crate;
    _ = diagnostics;
}

pub fn performTypeCheck(crate: *Crate, diagnostics: *diag.Diagnostics) !void {
    _ = crate;
    _ = diagnostics;
}

pub fn emptySpan(file_id: source_map.FileId) Span {
    return .{ .file_id = file_id, .start = 0, .end = 0 };
}

fn buildDummyBlock(crate: *Crate, span: Span) !ExprId {
    var stmt_ids = std.ArrayList(StmtId).init(crate.allocator());
    defer stmt_ids.deinit();

    const block_expr_id: ExprId = @intCast(crate.exprs.items.len);
    try crate.exprs.append(crate.allocator(), .{
        .id = block_expr_id,
        .kind = .{ .Block = .{ .stmts = try stmt_ids.toOwnedSlice(), .tail = null } },
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
