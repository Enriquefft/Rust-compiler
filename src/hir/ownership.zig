const std = @import("std");
const diag = @import("../diag/diagnostics.zig");
const hir = @import("hir.zig");

/// Minimal ownership and borrowing checker.
///
/// The checker enforces a handful of Rust-inspired guarantees:
/// - Non-Copy locals are moved on by-value use. Further uses trigger an error.
/// - A mutable borrow requires exclusive access (no other borrows active).
/// - Immutable borrows may coexist, but block mutable borrows and moves.
/// - Borrows end at lexical scope boundaries (block expressions).
///
/// The analysis is intentionally lightweight: it uses a single pass over the
/// typed HIR, assumes type checking has already succeeded, and restricts
/// itself to locals and block scoping. Field projections and pointer aliasing
/// are treated conservatively (only direct locals are tracked).
pub fn checkOwnership(crate: *hir.Crate, diagnostics: *diag.Diagnostics) Error!void {
    var checker = BorrowChecker{
        .crate = crate,
        .diagnostics = diagnostics,
        .locals = std.AutoHashMap(hir.LocalId, LocalState).init(crate.allocator()),
        .scopes = .{},
        .next_lifetime = 0,
    };
    defer checker.deinit();

    for (crate.items.items) |*item| {
        switch (item.kind) {
            .Function => |*func| try checker.checkFunction(func),
            else => {},
        }
    }
}

const Error = std.mem.Allocator.Error;

const BorrowChecker = struct {
    crate: *hir.Crate,
    diagnostics: *diag.Diagnostics,
    locals: std.AutoHashMap(hir.LocalId, LocalState),
    scopes: std.ArrayListUnmanaged(Scope),
    next_lifetime: LifetimeId,

    fn deinit(self: *BorrowChecker) void {
        self.locals.deinit();
        for (self.scopes.items) |*scope| {
            scope.borrows.deinit(self.crate.allocator());
        }
        self.scopes.deinit(self.crate.allocator());
    }

    fn checkFunction(self: *BorrowChecker, func: *hir.Function) Error!void {
        // Reset per-function state.
        self.locals.clearRetainingCapacity();
        for (self.scopes.items) |*scope| {
            scope.borrows.deinit(self.crate.allocator());
        }
        self.scopes.clearRetainingCapacity();
        self.next_lifetime = 0;

        // Seed locals with parameters.
        for (func.params, 0..) |local_id, idx| {
            const ty = if (idx < func.param_types.len) func.param_types[idx] else getUnknownTypeId();
            try self.locals.put(local_id, .{ .ty = ty });
        }

        // Borrow checking only applies when a body exists.
        if (func.body) |body| {
            try self.enterScope();
            defer self.exitScope();
            try self.checkExpr(body, .Consume);
        }
    }

    fn enterScope(self: *BorrowChecker) Error!void {
        const lifetime = self.next_lifetime;
        self.next_lifetime += 1;
        try self.scopes.append(self.crate.allocator(), .{
            .lifetime = lifetime,
            .borrows = std.ArrayListUnmanaged(Borrow){},
        });
    }

    fn exitScope(self: *BorrowChecker) void {
        var scope = self.scopes.pop() orelse return;
        defer scope.borrows.deinit(self.crate.allocator());

        for (scope.borrows.items) |borrow| {
            if (self.locals.getPtr(borrow.local)) |state| {
                if (borrow.mutable)
                    state.mutable_borrowed = false
                else if (state.immutable_borrows > 0)
                    state.immutable_borrows -= 1;
            }
        }
    }

    fn checkStmt(self: *BorrowChecker, stmt_id: hir.StmtId) Error!void {
        const stmt = self.crate.stmts.items[stmt_id];

        switch (stmt.kind) {
            .Let => |let_stmt| {
                if (let_stmt.value) |value| {
                    try self.checkExpr(value, .Consume);
                }
                const ty = let_stmt.ty orelse if (let_stmt.value) |value| self.crate.exprs.items[value].ty else getUnknownTypeId();
                try self.registerLocal(let_stmt.pat, ty);
                if (let_stmt.value != null) {
                    try self.markReinitialized(let_stmt.pat.id);
                }
            },
            .Expr => |expr_id| try self.checkExpr(expr_id, .Consume),
            .Unknown => {},
        }
    }

    fn checkExpr(self: *BorrowChecker, expr_id: hir.ExprId, mode: UseMode) Error!void {
        const expr = self.crate.exprs.items[expr_id];
        const span = expr.span;

        switch (expr.kind) {
            .ConstInt, .ConstFloat, .ConstBool, .ConstChar, .ConstString, .Unknown => {},
            .LocalRef => |local_id| try self.useLocal(local_id, span, mode),
            .GlobalRef => {},
            .Unary => |unary| {
                switch (unary.op) {
                    .Ref => {
                        try self.checkExpr(unary.expr, .Inspect);
                        try self.handleBorrow(unary.expr, span, false);
                    },
                    .RefMut => {
                        try self.checkExpr(unary.expr, .Inspect);
                        try self.handleBorrow(unary.expr, span, true);
                    },
                    else => try self.checkExpr(unary.expr, .Consume),
                }
            },
            .Binary => |bin| {
                try self.checkExpr(bin.lhs, .Consume);
                try self.checkExpr(bin.rhs, .Consume);
            },
            .Assignment => |assign| {
                try self.checkExpr(assign.target, .Inspect);
                try self.checkExpr(assign.value, .Consume);
                try self.handleAssignment(assign.target, span);
            },
            .Call => |call| {
                try self.checkExpr(call.callee, .Inspect);
                for (call.args) |arg| try self.checkExpr(arg, .Consume);
            },
            .MethodCall => |m| {
                try self.checkExpr(m.target, .Inspect);
                for (m.args) |arg| try self.checkExpr(arg, .Consume);
            },
            .Return => |maybe| if (maybe) |value| try self.checkExpr(value, .Consume),
            .If => |iff| {
                try self.checkExpr(iff.cond, .Consume);
                try self.enterScope();
                try self.checkExpr(iff.then_branch, .Consume);
                self.exitScope();
                if (iff.else_branch) |else_branch| {
                    try self.enterScope();
                    try self.checkExpr(else_branch, .Consume);
                    self.exitScope();
                }
            },
            .While => |wh| {
                try self.enterScope();
                defer self.exitScope();
                try self.checkExpr(wh.cond, .Consume);
                try self.checkExpr(wh.body, .Consume);
            },
            .For => |f| {
                try self.checkExpr(f.iter, .Consume);
                try self.enterScope();
                const iter_ty = self.crate.exprs.items[f.iter].ty;
                try self.registerLocal(f.pat, iter_ty);
                try self.markReinitialized(f.pat.id);
                try self.checkExpr(f.body, .Consume);
                self.exitScope();
            },
            .Range => |range| {
                try self.checkExpr(range.start, .Consume);
                try self.checkExpr(range.end, .Consume);
            },
            .Index => |index| {
                try self.checkExpr(index.target, .Inspect);
                try self.checkExpr(index.index, .Consume);
            },
            .Field => |field| try self.checkExpr(field.target, .Inspect),
            .Array => |elements| {
                for (elements) |elem| try self.checkExpr(elem, .Consume);
            },
            .StructInit => |init| {
                for (init.fields) |field_init| try self.checkExpr(field_init.value, .Consume);
            },
            .Lambda => |lambda| {
                try self.enterScope();
                for (lambda.params, 0..) |param, idx| {
                    const ty = if (idx < lambda.param_types.len) lambda.param_types[idx] else getUnknownTypeId();
                    try self.registerLocal(param, ty);
                    try self.markReinitialized(param.id);
                }
                try self.checkExpr(lambda.body, .Consume);
                self.exitScope();
            },
            .Block => |block| {
                try self.enterScope();
                for (block.stmts) |stmt_id| try self.checkStmt(stmt_id);
                if (block.tail) |tail| try self.checkExpr(tail, .Consume);
                self.exitScope();
            },
            .Unsafe => |u| try self.checkExpr(u.body, .Consume),
            .Cast => |c| try self.checkExpr(c.expr, .Consume),
            .Path, .UnresolvedIdent => {},
        }
    }

    fn handleAssignment(self: *BorrowChecker, target_id: hir.ExprId, span: hir.Span) Error!void {
        const target = self.crate.exprs.items[target_id];
        switch (target.kind) {
            .LocalRef => |local_id| {
                if (self.locals.getPtr(local_id)) |state| {
                    if (state.mutable_borrowed or state.immutable_borrows > 0) {
                        self.diagnostics.reportError(span, "cannot assign to a borrowed local");
                        return;
                    }
                }
                try self.markReinitialized(local_id);
            },
            else => {},
        }
    }

    fn handleBorrow(self: *BorrowChecker, expr_id: hir.ExprId, span: hir.Span, mutable: bool) Error!void {
        const expr = self.crate.exprs.items[expr_id];
        if (expr.kind != .LocalRef) return;
        const local_id = expr.kind.LocalRef;
        if (self.locals.getPtr(local_id)) |state| {
            if (state.moved) {
                self.diagnostics.reportError(span, "cannot borrow a moved value");
                return;
            }
            if (mutable) {
                if (state.mutable_borrowed or state.immutable_borrows > 0) {
                    self.diagnostics.reportError(span, "cannot take mutable borrow while another borrow is active");
                    return;
                }
                state.mutable_borrowed = true;
            } else {
                if (state.mutable_borrowed) {
                    self.diagnostics.reportError(span, "cannot take immutable borrow while a mutable borrow is active");
                    return;
                }
                state.immutable_borrows += 1;
            }
            if (self.scopes.items.len > 0) {
                var scope = &self.scopes.items[self.scopes.items.len - 1];
                try scope.borrows.append(self.crate.allocator(), .{ .local = local_id, .mutable = mutable });
            }
        }
    }

    fn registerLocal(self: *BorrowChecker, pat: hir.Pattern, ty: hir.TypeId) Error!void {
        switch (pat.kind) {
            .Identifier => if (self.locals.getPtr(pat.id)) |state| {
                state.* = .{ .ty = ty };
            } else {
                try self.locals.put(pat.id, .{ .ty = ty });
            },
            .Wildcard => {},
        }
    }

    fn markReinitialized(self: *BorrowChecker, local_id: hir.LocalId) Error!void {
        if (self.locals.getPtr(local_id)) |state| {
            state.* = .{ .ty = state.ty };
        } else {
            try self.locals.put(local_id, .{ .ty = getUnknownTypeId() });
        }
    }

    fn useLocal(self: *BorrowChecker, local_id: hir.LocalId, span: hir.Span, mode: UseMode) Error!void {
        if (self.locals.getPtr(local_id)) |state| {
            if (state.moved) {
                self.diagnostics.reportError(span, "use of moved value");
                return;
            }
            if (mode == .Consume and (state.mutable_borrowed or state.immutable_borrows > 0) and !isCopyType(self.crate, state.ty)) {
                self.diagnostics.reportError(span, "cannot move out of a borrowed value");
                return;
            }
            if (mode == .Consume and !isCopyType(self.crate, state.ty)) {
                state.moved = true;
            }
        } else {
            // Unknown locals are treated as non-Copy to stay conservative.
            if (mode == .Consume) {
                try self.locals.put(local_id, .{ .ty = getUnknownTypeId(), .moved = true });
            }
        }
    }
};

const LocalState = struct {
    ty: hir.TypeId,
    moved: bool = false,
    immutable_borrows: usize = 0,
    mutable_borrowed: bool = false,
};

const Borrow = struct {
    local: hir.LocalId,
    mutable: bool,
};

const Scope = struct {
    lifetime: LifetimeId,
    borrows: std.ArrayListUnmanaged(Borrow),
};

const LifetimeId = u32;

const UseMode = enum { Inspect, Consume };

fn isCopyType(crate: *hir.Crate, ty_id: hir.TypeId) bool {
    if (ty_id >= crate.types.items.len) return false;
    return switch (crate.types.items[ty_id].kind) {
        .PrimInt, .PrimFloat, .Bool, .Char, .Pointer, .Ref, .Fn, .Str => true,
        else => false,
    };
}

fn getUnknownTypeId() hir.TypeId {
    return 0;
}

// === Tests ===

test "ownership checker rejects second move of non-copy local" {
    const allocator = std.testing.allocator;
    var diagnostics = diag.Diagnostics.init(allocator);
    defer diagnostics.deinit();

    var crate = hir.Crate.init(allocator);
    defer crate.deinit();

    const span = hir.emptySpan(0);
    const string_ty: hir.TypeId = @intCast(crate.types.items.len);
    try crate.types.append(crate.allocator(), .{ .id = string_ty, .kind = .String });

    // let x: String = ...;
    const x_pat = try appendIdentPattern(&crate, "x", span);
    const init_expr = try appendConst(&crate, span);
    const let_x = try appendLetStmt(&crate, x_pat, string_ty, init_expr, span);

    // let y = x;
    const x_ref_expr = try appendLocalRef(&crate, x_pat.id, span);
    const let_y = try appendLetStmt(&crate, try appendIdentPattern(&crate, "y", span), string_ty, x_ref_expr, span);

    // let z = x; // should fail, x already moved
    const second_move = try appendLocalRef(&crate, x_pat.id, span);
    const let_z = try appendLetStmt(&crate, try appendIdentPattern(&crate, "z", span), string_ty, second_move, span);

    const stmts = try crate.allocator().alloc(hir.StmtId, 3);
    stmts[0] = let_x;
    stmts[1] = let_y;
    stmts[2] = let_z;

    const body = try appendBlockExpr(&crate, stmts, null, span);
    try appendFunction(&crate, body, &[_]hir.LocalId{}, &[_]hir.TypeId{}, span);

    try checkOwnership(&crate, &diagnostics);
    try std.testing.expect(diagnostics.hasErrors());
    try std.testing.expectEqual(@as(usize, 1), diagnostics.entries.items.len);
    try std.testing.expect(std.mem.eql(u8, diagnostics.entries.items[0].message, "use of moved value"));
}

test "borrows end at block boundaries" {
    const allocator = std.testing.allocator;
    var diagnostics = diag.Diagnostics.init(allocator);
    defer diagnostics.deinit();

    var crate = hir.Crate.init(allocator);
    defer crate.deinit();

    const span = hir.emptySpan(0);
    const string_ty: hir.TypeId = @intCast(crate.types.items.len);
    try crate.types.append(crate.allocator(), .{ .id = string_ty, .kind = .String });

    const x_pat = try appendIdentPattern(&crate, "x", span);
    const init_expr = try appendConst(&crate, span);
    const let_x = try appendLetStmt(&crate, x_pat, string_ty, init_expr, span);

    // { let r = &x; }
    const x_ref_expr = try appendLocalRef(&crate, x_pat.id, span);
    const borrow_expr = try appendUnary(&crate, .Ref, x_ref_expr, span);
    const let_r = try appendLetStmt(&crate, try appendIdentPattern(&crate, "r", span), string_ty, borrow_expr, span);
    const inner_stmts = try crate.allocator().alloc(hir.StmtId, 1);
    inner_stmts[0] = let_r;
    const inner_block = try appendBlockExpr(&crate, inner_stmts, null, span);
    const inner_stmt = try appendExprStmt(&crate, inner_block, span);

    // let m = &mut x; // should succeed after borrow scope ends
    const x_mut_ref_expr = try appendUnary(&crate, .RefMut, try appendLocalRef(&crate, x_pat.id, span), span);
    const let_m = try appendLetStmt(&crate, try appendIdentPattern(&crate, "m", span), string_ty, x_mut_ref_expr, span);

    const stmts = try crate.allocator().alloc(hir.StmtId, 3);
    stmts[0] = let_x;
    stmts[1] = inner_stmt;
    stmts[2] = let_m;

    const body = try appendBlockExpr(&crate, stmts, null, span);
    try appendFunction(&crate, body, &[_]hir.LocalId{}, &[_]hir.TypeId{}, span);

    try checkOwnership(&crate, &diagnostics);
    try std.testing.expect(!diagnostics.hasErrors());
}

test "mutable borrow blocked by active shared borrows" {
    const allocator = std.testing.allocator;
    var diagnostics = diag.Diagnostics.init(allocator);
    defer diagnostics.deinit();

    var crate = hir.Crate.init(allocator);
    defer crate.deinit();

    const span = hir.emptySpan(0);
    const string_ty: hir.TypeId = @intCast(crate.types.items.len);
    try crate.types.append(crate.allocator(), .{ .id = string_ty, .kind = .String });

    const x_pat = try appendIdentPattern(&crate, "x", span);
    const init_expr = try appendConst(&crate, span);
    const let_x = try appendLetStmt(&crate, x_pat, string_ty, init_expr, span);

    const first_borrow = try appendUnary(&crate, .Ref, try appendLocalRef(&crate, x_pat.id, span), span);
    const let_r1 = try appendLetStmt(&crate, try appendIdentPattern(&crate, "r1", span), string_ty, first_borrow, span);

    const mut_borrow = try appendUnary(&crate, .RefMut, try appendLocalRef(&crate, x_pat.id, span), span);
    const let_r2 = try appendLetStmt(&crate, try appendIdentPattern(&crate, "r2", span), string_ty, mut_borrow, span);

    const stmts = try crate.allocator().alloc(hir.StmtId, 3);
    stmts[0] = let_x;
    stmts[1] = let_r1;
    stmts[2] = let_r2;

    const body = try appendBlockExpr(&crate, stmts, null, span);
    try appendFunction(&crate, body, &[_]hir.LocalId{}, &[_]hir.TypeId{}, span);

    try checkOwnership(&crate, &diagnostics);
    try std.testing.expect(diagnostics.hasErrors());
    try std.testing.expectEqualStrings("cannot take mutable borrow while another borrow is active", diagnostics.entries.items[0].message);
}

fn appendIdentPattern(crate: *hir.Crate, name: []const u8, span: hir.Span) !hir.Pattern {
    const id: hir.LocalId = @intCast(crate.patterns.items.len);
    try crate.patterns.append(crate.allocator(), .{ .id = id, .kind = .{ .Identifier = name }, .span = span });
    return crate.patterns.items[id];
}

fn appendConst(crate: *hir.Crate, span: hir.Span) !hir.ExprId {
    const expr_id: hir.ExprId = @intCast(crate.exprs.items.len);
    try crate.exprs.append(crate.allocator(), .{ .id = expr_id, .kind = .{ .ConstInt = 0 }, .ty = 0, .span = span });
    return expr_id;
}

fn appendLocalRef(crate: *hir.Crate, local_id: hir.LocalId, span: hir.Span) !hir.ExprId {
    const expr_id: hir.ExprId = @intCast(crate.exprs.items.len);
    try crate.exprs.append(crate.allocator(), .{ .id = expr_id, .kind = .{ .LocalRef = local_id }, .ty = 0, .span = span });
    return expr_id;
}

fn appendUnary(crate: *hir.Crate, op: hir.UnaryOp, inner: hir.ExprId, span: hir.Span) !hir.ExprId {
    const expr_id: hir.ExprId = @intCast(crate.exprs.items.len);
    try crate.exprs.append(crate.allocator(), .{ .id = expr_id, .kind = .{ .Unary = .{ .op = op, .expr = inner } }, .ty = 0, .span = span });
    return expr_id;
}

fn appendLetStmt(crate: *hir.Crate, pat: hir.Pattern, ty: hir.TypeId, value: hir.ExprId, span: hir.Span) !hir.StmtId {
    const stmt_id: hir.StmtId = @intCast(crate.stmts.items.len);
    try crate.stmts.append(crate.allocator(), .{
        .id = stmt_id,
        .kind = .{ .Let = .{ .pat = pat, .ty = ty, .value = value } },
        .span = span,
    });
    return stmt_id;
}

fn appendExprStmt(crate: *hir.Crate, expr_id: hir.ExprId, span: hir.Span) !hir.StmtId {
    const stmt_id: hir.StmtId = @intCast(crate.stmts.items.len);
    try crate.stmts.append(crate.allocator(), .{ .id = stmt_id, .kind = .{ .Expr = expr_id }, .span = span });
    return stmt_id;
}

fn appendBlockExpr(crate: *hir.Crate, stmts: []hir.StmtId, tail: ?hir.ExprId, span: hir.Span) !hir.ExprId {
    const expr_id: hir.ExprId = @intCast(crate.exprs.items.len);
    try crate.exprs.append(crate.allocator(), .{
        .id = expr_id,
        .kind = .{ .Block = .{ .stmts = stmts, .tail = tail } },
        .ty = 0,
        .span = span,
    });
    return expr_id;
}

fn appendFunction(crate: *hir.Crate, body: hir.ExprId, params: []const hir.LocalId, param_types: []const hir.TypeId, span: hir.Span) Error!void {
    const params_owned = try crate.allocator().dupe(hir.LocalId, params);
    const param_types_owned = try crate.allocator().dupe(hir.TypeId, param_types);
    try crate.items.append(crate.allocator(), .{ .id = 0, .kind = .{ .Function = .{ .def_id = 0, .name = "f", .type_params = &[_][]const u8{}, .params = params_owned, .param_types = param_types_owned, .return_type = null, .body = body, .span = span } }, .span = span });
}
