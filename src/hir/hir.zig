//! High-level Intermediate Representation (HIR) module.
//!
//! This module defines the HIR data structures used to represent a resolved and
//! typed variant of the AST. The HIR is the main output of the frontend and serves
//! as the input to the MIR lowering phase.
//!
//! ## Key Concepts
//!
//! - **DefId**: Identifies top-level items (functions, structs, type aliases, impls).
//! - **LocalId**: Identifies function parameters and local variable bindings.
//! - **TypeId**: References types stored in the crate's type arena.
//! - **ExprId / StmtId / ItemId**: References to expressions, statements, and items.
//!
//! ## HIR Lowering Pipeline
//!
//! The function `lowerFromAst` transforms an AST crate into an HIR crate by:
//! 1. Converting AST items (functions, structs, type aliases, impls) to HIR items.
//! 2. Lowering expressions and statements recursively.
//! 3. Allocating types into the type arena.
//!
//! After lowering, name resolution (`name_res.zig`) and type checking (`typecheck.zig`)
//! are performed to produce a fully resolved and typed HIR.

const std = @import("std");
const diag = @import("../diag/diagnostics.zig");
const source_map = @import("../diag/source_map.zig");
const ast = @import("../frontend/ast.zig");
const name_res = @import("name_res.zig");
const typecheck = @import("typecheck.zig");

/// Unique identifier for top-level definitions (functions, structs, type aliases, impls).
pub const DefId = u32;

/// Unique identifier for local bindings (function parameters, let bindings).
pub const LocalId = u32;

/// Unique identifier for types stored in the crate's type arena.
pub const TypeId = u32;

/// Unique identifier for expressions stored in the crate's expression arena.
pub const ExprId = u32;

/// Unique identifier for statements stored in the crate's statement arena.
pub const StmtId = u32;

/// Unique identifier for items stored in the crate's item arena.
pub const ItemId = u32;

/// Represents a type in the HIR.
///
/// Types are interned in the crate's type arena and referenced by `TypeId`.
/// The `kind` field determines the specific type variant.
pub const Type = struct {
    /// Unique identifier of this type in the type arena.
    id: TypeId,
    /// The specific kind of type.
    kind: Kind,

    /// Discriminated union of all possible type kinds.
    pub const Kind = union(enum) {
        /// Primitive integer types (signed and unsigned).
        PrimInt: enum { U32, U64, Usize, I32, I64 },
        /// Primitive floating-point types.
        PrimFloat: enum { F32, F64 },
        /// Boolean type.
        Bool,
        /// Character type (Unicode scalar value).
        Char,
        /// String slice type (&str).
        Str,
        /// Owned string type (String).
        String,
        /// Fixed-size array type with element type and optional compile-time size.
        Array: struct { elem: TypeId, size_const: ?i64 },
        /// Raw pointer type with mutability flag.
        Pointer: struct { mutable: bool, inner: TypeId },
        /// Reference type with mutability flag.
        Ref: struct { mutable: bool, inner: TypeId },
        /// Function type with parameter types and return type.
        Fn: struct { params: []TypeId, ret: TypeId },
        /// Struct type referencing a definition with optional type arguments.
        Struct: struct { def_id: DefId, type_args: []TypeId },
        /// Unresolved path type (to be resolved during type checking).
        Path: struct { segments: [][]const u8, args: []TypeId },
        /// Placeholder for unresolved or error types.
        Unknown,
    };
};

/// Represents a pattern used in bindings and match arms.
///
/// Patterns occur in function parameters, let bindings, for loops, and match expressions.
pub const Pattern = struct {
    /// Unique identifier for this pattern in the pattern arena.
    id: LocalId,
    /// The specific kind of pattern.
    kind: Kind,
    /// Source location span for diagnostics.
    span: Span,

    /// Discriminated union of all possible pattern kinds.
    pub const Kind = union(enum) {
        /// Named binding pattern that introduces a local variable.
        Identifier: []const u8,
        /// Wildcard pattern that matches anything without binding.
        Wildcard,
    };
};

/// Represents an expression in the HIR.
///
/// Expressions are the primary computational units. Each expression has a type
/// (assigned during type checking) and a source span for diagnostics.
pub const Expr = struct {
    /// Unique identifier for this expression in the expression arena.
    id: ExprId,
    /// The specific kind of expression.
    kind: Kind,
    /// Type of this expression (filled during type checking).
    ty: TypeId,
    /// Source location span for diagnostics.
    span: Span,

    /// Discriminated union of all possible expression kinds.
    pub const Kind = union(enum) {
        /// Reference to a local variable by its LocalId.
        LocalRef: LocalId,
        /// Reference to a global item (function, struct, etc.) by its DefId.
        GlobalRef: DefId,
        /// Integer constant literal.
        ConstInt: i64,
        /// Floating-point constant literal.
        ConstFloat: f64,
        /// Boolean constant literal.
        ConstBool: bool,
        /// Character constant literal (Unicode codepoint).
        ConstChar: u21,
        /// String constant literal.
        ConstString: []const u8,
        /// Unresolved identifier (to be resolved during name resolution).
        UnresolvedIdent: []const u8,
        /// Path expression with segments and optional generic arguments.
        Path: struct { segments: [][]const u8, args: []TypeId },
        /// Binary operation (arithmetic, comparison, logical).
        Binary: struct { op: BinaryOp, lhs: ExprId, rhs: ExprId },
        /// Unary operation (negation, not, reference, dereference).
        Unary: struct { op: UnaryOp, expr: ExprId },
        /// Function call with callee expression and arguments.
        Call: struct { callee: ExprId, args: []ExprId },
        /// Method call on a target expression.
        MethodCall: struct { target: ExprId, name: []const u8, args: []ExprId },
        /// Assignment expression with optional compound operator.
        Assignment: struct { target: ExprId, op: AssignOp, value: ExprId },
        /// Return expression with optional value.
        Return: ?ExprId,
        /// Conditional expression with condition, then branch, and optional else branch.
        If: struct { cond: ExprId, then_branch: ExprId, else_branch: ?ExprId },
        /// While loop with condition and body.
        While: struct { cond: ExprId, body: ExprId },
        /// For loop with pattern, iterator expression, and body.
        For: struct { pat: Pattern, iter: ExprId, body: ExprId },
        /// Range expression (exclusive or inclusive).
        Range: struct { inclusive: bool, start: ExprId, end: ExprId },
        /// Type cast expression.
        Cast: struct { expr: ExprId, ty: TypeId },
        /// Array/slice indexing expression.
        Index: struct { target: ExprId, index: ExprId },
        /// Field access expression.
        Field: struct { target: ExprId, name: []const u8 },
        /// Array literal expression.
        Array: []ExprId,
        /// Struct initialization expression.
        StructInit: StructInit,
        /// Lambda/closure expression.
        Lambda: struct { params: []Pattern, param_types: []TypeId, body: ExprId },
        /// Block expression containing statements and optional tail expression.
        Block: struct { stmts: []StmtId, tail: ?ExprId },
        /// Unsafe block expression.
        Unsafe: struct { body: ExprId },
        /// Placeholder for unresolved or error expressions.
        Unknown,
    };
};

/// Struct initialization expression data.
///
/// Contains the path identifying the struct type and the field initializers.
pub const StructInit = struct {
    /// Path segments identifying the struct type.
    path: [][]const u8,
    /// List of field initializers.
    fields: []StructInitField,
};

/// A single field initializer in a struct initialization expression.
pub const StructInitField = struct {
    /// Name of the field being initialized.
    name: []const u8,
    /// Expression providing the initial value.
    value: ExprId,
};

/// Binary operators for arithmetic, comparison, and logical operations.
pub const BinaryOp = enum {
    /// Logical OR (||).
    LogicalOr,
    /// Logical AND (&&).
    LogicalAnd,
    /// Equality comparison (==).
    Eq,
    /// Inequality comparison (!=).
    Ne,
    /// Less than comparison (<).
    Lt,
    /// Less than or equal comparison (<=).
    Le,
    /// Greater than comparison (>).
    Gt,
    /// Greater than or equal comparison (>=).
    Ge,
    /// Addition (+).
    Add,
    /// Subtraction (-).
    Sub,
    /// Multiplication (*).
    Mul,
    /// Division (/).
    Div,
    /// Modulo/remainder (%).
    Mod,
};

/// Unary operators for negation, reference, and dereference operations.
pub const UnaryOp = enum {
    /// Logical NOT (!).
    Not,
    /// Arithmetic negation (-).
    Neg,
    /// Pointer/reference dereference (*).
    Deref,
    /// Immutable reference (&).
    Ref,
    /// Mutable reference (&mut).
    RefMut,
};

/// Assignment operators for simple and compound assignment.
pub const AssignOp = enum {
    /// Simple assignment (=).
    Assign,
    /// Addition assignment (+=).
    AddAssign,
    /// Subtraction assignment (-=).
    SubAssign,
    /// Multiplication assignment (*=).
    MulAssign,
    /// Division assignment (/=).
    DivAssign,
};

/// Represents a statement in the HIR.
///
/// Statements are executed for their side effects and optionally produce a value
/// (as the tail expression of a block).
pub const Stmt = struct {
    /// Unique identifier for this statement in the statement arena.
    id: StmtId,
    /// The specific kind of statement.
    kind: Kind,
    /// Source location span for diagnostics.
    span: Span,

    /// Discriminated union of all possible statement kinds.
    pub const Kind = union(enum) {
        /// Let binding statement with pattern, optional type annotation, and optional initializer.
        Let: struct { pat: Pattern, ty: ?TypeId, value: ?ExprId },
        /// Expression statement.
        Expr: ExprId,
        /// Placeholder for unresolved or error statements.
        Unknown,
    };
};

/// Represents a top-level item in the HIR.
///
/// Items are the building blocks of a crate: functions, structs, type aliases, and impls.
pub const Item = struct {
    /// Unique identifier for this item in the item arena.
    id: ItemId,
    /// The specific kind of item.
    kind: Kind,
    /// Source location span for diagnostics.
    span: Span,

    /// Discriminated union of all possible item kinds.
    pub const Kind = union(enum) {
        /// Function definition.
        Function: Function,
        /// Struct definition.
        Struct: Struct,
        /// Type alias definition.
        TypeAlias: TypeAlias,
        /// Impl block containing methods.
        Impl: Impl,
        /// Empty item (placeholder).
        Empty,
    };
};

/// Represents a function definition in the HIR.
pub const Function = struct {
    /// Definition ID for this function.
    def_id: DefId,
    /// Function name.
    name: []const u8,
    /// Parameter local IDs (indices into the pattern arena).
    params: []LocalId,
    /// Parameter types (indices into the type arena).
    param_types: []TypeId,
    /// Optional return type (inferred if not specified).
    return_type: ?TypeId,
    /// Optional function body (extern functions have no body).
    body: ?ExprId,
    /// Source location span for diagnostics.
    span: Span,
};

/// Represents a struct definition in the HIR.
pub const Struct = struct {
    /// Definition ID for this struct.
    def_id: DefId,
    /// Struct name.
    name: []const u8,
    /// List of struct fields.
    fields: []Field,
    /// Source location span for diagnostics.
    span: Span,
};

/// Represents a field in a struct definition.
pub const Field = struct {
    /// Field name.
    name: []const u8,
    /// Field type.
    ty: TypeId,
    /// Source location span for diagnostics.
    span: Span,
};

/// Represents a type alias definition in the HIR.
pub const TypeAlias = struct {
    /// Definition ID for this type alias.
    def_id: DefId,
    /// Alias name.
    name: []const u8,
    /// Target type that this alias refers to.
    target: TypeId,
    /// Source location span for diagnostics.
    span: Span,
};

/// Represents an impl block in the HIR.
///
/// Impl blocks define methods for a type.
pub const Impl = struct {
    /// Definition ID for this impl block.
    def_id: DefId,
    /// Target type for which methods are being implemented.
    target: TypeId,
    /// List of method item IDs.
    methods: []ItemId,
    /// Source location span for diagnostics.
    span: Span,
};

/// The root container for all HIR data structures.
///
/// A crate contains arenas for items, expressions, statements, types, and patterns.
/// All HIR nodes are stored in these arenas and referenced by their respective IDs.
pub const Crate = struct {
    /// Arena allocator for all HIR allocations.
    arena: std.heap.ArenaAllocator,
    /// All top-level items in the crate.
    items: std.ArrayListUnmanaged(Item),
    /// All expressions in the crate.
    exprs: std.ArrayListUnmanaged(Expr),
    /// All statements in the crate.
    stmts: std.ArrayListUnmanaged(Stmt),
    /// All types in the crate.
    types: std.ArrayListUnmanaged(Type),
    /// All patterns in the crate.
    patterns: std.ArrayListUnmanaged(Pattern),

    /// Initializes a new empty crate with the given backing allocator.
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

    /// Releases all memory associated with this crate.
    pub fn deinit(self: *Crate) void {
        self.items.deinit(self.arena.allocator());
        self.exprs.deinit(self.arena.allocator());
        self.stmts.deinit(self.arena.allocator());
        self.types.deinit(self.arena.allocator());
        self.patterns.deinit(self.arena.allocator());
        self.arena.deinit();
    }

    /// Returns the arena allocator for making allocations within this crate.
    pub fn allocator(self: *Crate) std.mem.Allocator {
        return self.arena.allocator();
    }
};

/// Re-export of `Span` from the source map module for convenience.
pub const Span = source_map.Span;

/// Error type for lowering operations, combining allocation errors with literal parsing errors.
const LowerError = std.mem.Allocator.Error || error{InvalidLiteral};

/// Lowers an AST crate to an HIR crate.
///
/// This function transforms the parsed AST into the HIR representation by:
/// 1. Converting each AST item (function, struct, type alias, impl) to its HIR equivalent.
/// 2. Detecting and reporting duplicate item names.
/// 3. Lowering expressions, statements, and types recursively.
///
/// After this pass, name resolution and type checking should be performed to fully resolve the HIR.
///
/// ## Parameters
/// - `allocator`: The allocator used for the HIR crate's arena.
/// - `ast_crate`: The parsed AST crate to lower.
/// - `diagnostics`: Diagnostics collector for error reporting.
///
/// ## Returns
/// The lowered HIR crate, or an error if allocation fails.
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

// Lowers an AST function item to an HIR function item.
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

// Lowers an AST struct item to an HIR struct item.
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

// Lowers an AST type alias item to an HIR type alias item.
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

// Lowers an AST impl block to HIR, adding each method as a separate function item.
fn lowerImpl(
    crate: *Crate,
    def_id: DefId,
    item: ast.Item,
    next_type_id: *TypeId,
) LowerError!void {
    const data = item.data.Impl;

    // Lower the target type
    var diagnostics_dummy = @import("../diag/diagnostics.zig").Diagnostics.init(crate.allocator());
    defer diagnostics_dummy.deinit();
    const target = try lowerType(crate, data.target, &diagnostics_dummy, next_type_id);

    // Get the struct name for method name mangling
    var struct_name: []const u8 = "";

    // Create a struct type for the self parameter based on the target type path
    var self_type: TypeId = target;
    if (data.target.tag == .Path) {
        // Look up the struct by name to get a proper struct type
        const path = data.target.data.Path;
        if (path.segments.len == 1) {
            struct_name = path.segments[0].name;
            for (crate.items.items, 0..) |crate_item, idx| {
                if (crate_item.kind == .Struct) {
                    const struct_item = crate_item.kind.Struct;
                    if (std.mem.eql(u8, struct_item.name, struct_name)) {
                        // Create a struct type referring to this def
                        const struct_ty_id = next_type_id.*;
                        next_type_id.* += 1;
                        try crate.types.append(crate.allocator(), .{ .id = struct_ty_id, .kind = .{ .Struct = .{ .def_id = @intCast(idx), .type_args = &[_]TypeId{} } } });
                        self_type = struct_ty_id;
                        break;
                    }
                }
            }
        }
    }

    // Lower methods - add each method as a separate item
    var method_ids = std.ArrayListUnmanaged(ItemId){};
    defer method_ids.deinit(crate.allocator());

    for (data.methods) |method| {
        const method_def_id: DefId = @intCast(crate.items.items.len);

        // Lower method parameters
        var params_buffer = std.ArrayListUnmanaged(LocalId){};
        defer params_buffer.deinit(crate.allocator());
        var param_types = std.ArrayListUnmanaged(TypeId){};
        defer param_types.deinit(crate.allocator());

        for (method.params) |param| {
            const local = try lowerPattern(crate, param.pattern, &diagnostics_dummy);

            // Determine parameter type
            const ty_id = switch (param.kind) {
                .SelfValue => self_type,
                .SelfRef => blk: {
                    // Create a reference type to the struct
                    const ref_ty_id = next_type_id.*;
                    next_type_id.* += 1;
                    try crate.types.append(crate.allocator(), .{ .id = ref_ty_id, .kind = .{ .Ref = .{ .mutable = false, .inner = self_type } } });
                    break :blk ref_ty_id;
                },
                .SelfRefMut => blk: {
                    // Create a mutable reference type to the struct
                    const ref_ty_id = next_type_id.*;
                    next_type_id.* += 1;
                    try crate.types.append(crate.allocator(), .{ .id = ref_ty_id, .kind = .{ .Ref = .{ .mutable = true, .inner = self_type } } });
                    break :blk ref_ty_id;
                },
                .Normal => if (param.ty) |param_ty|
                    try lowerType(crate, param_ty, &diagnostics_dummy, next_type_id)
                else
                    try appendUnknownType(crate, next_type_id),
            };
            try params_buffer.append(crate.allocator(), local);
            try param_types.append(crate.allocator(), ty_id);
        }

        const return_ty = if (method.return_type) |ret_ty|
            try lowerType(crate, ret_ty, &diagnostics_dummy, next_type_id)
        else
            null;

        const body_expr = try lowerBlock(crate, method.body, &diagnostics_dummy, next_type_id);

        // Create mangled method name: StructName_methodName to avoid libc conflicts
        const method_name = if (struct_name.len > 0)
            try std.fmt.allocPrint(crate.allocator(), "{s}_{s}", .{ struct_name, method.name.name })
        else
            try crate.allocator().dupe(u8, method.name.name);

        const fn_item = Function{
            .def_id = method_def_id,
            .name = method_name,
            .params = try params_buffer.toOwnedSlice(crate.allocator()),
            .param_types = try param_types.toOwnedSlice(crate.allocator()),
            .return_type = return_ty,
            .body = body_expr,
            .span = method.span,
        };
        try crate.items.append(crate.allocator(), .{ .id = @intCast(method_def_id), .kind = .{ .Function = fn_item }, .span = method.span });
        try method_ids.append(crate.allocator(), @intCast(method_def_id));
    }

    const impl_item = Impl{
        .def_id = def_id,
        .target = target,
        .methods = try method_ids.toOwnedSlice(crate.allocator()),
        .span = data.span,
    };
    try crate.items.append(crate.allocator(), .{ .id = @intCast(def_id), .kind = .{ .Impl = impl_item }, .span = item.span });
}

// Allocates a new Unknown type and returns its TypeId.
fn appendUnknownType(crate: *Crate, next_type_id: *TypeId) LowerError!TypeId {
    const id = next_type_id.*;
    next_type_id.* += 1;
    try crate.types.append(crate.allocator(), .{ .id = id, .kind = .Unknown });
    return id;
}

// Lowers an AST type to an HIR type and returns its TypeId.
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

// Evaluates an array size expression at compile time, returning the size as i64 if constant.
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

// Lowers an AST pattern to an HIR pattern and returns its LocalId.
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

// Lowers an AST expression to an HIR expression and returns its ExprId.
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

        .Unsafe => {
            const body_block_id = try lowerBlock(
                crate,
                expr.data.Unsafe.block,
                diagnostics,
                next_type_id,
            );

            const id: ExprId = @intCast(crate.exprs.items.len);
            try crate.exprs.append(
                crate.allocator(),
                .{
                    .id = id,
                    .kind = .{ .Unsafe = .{ .body = body_block_id } },
                    .ty = try appendUnknownType(crate, next_type_id),
                    .span = expr.span,
                },
            );
            return id;
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

// Lowers an AST block to an HIR block expression and returns its ExprId.
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

// Lowers an AST statement to an HIR statement and returns its StmtId.
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

// Infers the type for a literal expression and returns its TypeId.
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

// Maps an AST binary operator to an HIR binary operator.
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

// Maps an AST assignment operator to an HIR assignment operator.
fn mapAssignOp(op: ast.AssignOp) AssignOp {
    return switch (op) {
        .Assign => .Assign,
        .AddAssign => .AddAssign,
        .SubAssign => .SubAssign,
        .MulAssign => .MulAssign,
        .DivAssign => .DivAssign,
    };
}

// Parses a character literal lexeme and returns the Unicode codepoint.
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

// Allocates an Unknown expression (used as a placeholder for error recovery).
fn appendUnknownExpr(crate: *Crate, span: Span) LowerError!ExprId {
    const id: ExprId = @intCast(crate.exprs.items.len);
    try crate.exprs.append(crate.allocator(), .{ .id = id, .kind = .Unknown, .ty = 0, .span = span });
    return id;
}

// Interns a name and detects duplicate definitions.
// Reports an error if the name already exists in the name table.
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

/// Performs name resolution on the HIR crate.
///
/// This resolves all identifiers to their corresponding definitions (local or global)
/// and reports any unresolved or duplicate names.
pub fn performNameResolution(crate: *Crate, diagnostics: *diag.Diagnostics) !void {
    try name_res.resolve(crate, diagnostics);
}

/// Performs type checking on the HIR crate.
///
/// This assigns types to all expressions and validates type consistency
/// (operand types, return types, assignment compatibility, etc.).
pub fn performTypeCheck(crate: *Crate, diagnostics: *diag.Diagnostics) !void {
    try typecheck.typecheck(crate, diagnostics);
}

/// Creates an empty span for the given file ID.
///
/// Useful for constructing synthetic HIR nodes during testing or error recovery.
pub fn emptySpan(file_id: source_map.FileId) Span {
    return .{ .file_id = file_id, .start = 0, .end = 0 };
}

// Builds a dummy empty block expression (used for error recovery).
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
