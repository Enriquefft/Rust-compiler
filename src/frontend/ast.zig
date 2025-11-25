const source_map = @import("../diag/source_map.zig");
const Span = source_map.Span;

/// Top-level crate containing all parsed items.
pub const Crate = struct {
    items: []Item,
};

pub const Identifier = struct {
    name: []const u8,
    span: Span,
};

pub const Item = struct {
    tag: Tag,
    span: Span,
    data: union(Tag) {
        Fn: FnItem,
        Struct: StructItem,
        Impl: ImplItem,
        TypeAlias: TypeAliasItem,
        Empty: void,
    },

    pub const Tag = enum {
        Fn,
        Struct,
        Impl,
        TypeAlias,
        Empty,
    };
};

pub const FnItem = struct {
    name: Identifier,
    generics: []Identifier,
    params: []Param,
    return_type: ?Type,
    body: Block,
    span: Span,
};

pub const StructItem = struct {
    name: Identifier,
    generics: []Identifier,
    fields: []Field,
    span: Span,
};

pub const ImplItem = struct {
    generics: []Identifier,
    target: Type,
    methods: []FnItem,
    span: Span,
};

pub const TypeAliasItem = struct {
    name: Identifier,
    generics: []Identifier,
    aliased_type: Type,
    span: Span,
};

pub const Field = struct {
    name: Identifier,
    ty: Type,
    span: Span,
};

pub const Param = struct {
    pattern: Pattern,
    ty: ?Type,
    span: Span,
    kind: Kind,

    pub const Kind = enum {
        Normal,
        SelfValue,
        SelfRef,
        SelfRefMut,
    };
};

pub const Pattern = struct {
    tag: Tag,
    span: Span,
    data: union(Tag) {
        Identifier: Identifier,
        Wildcard: void,
    },

    pub const Tag = enum {
        Identifier,
        Wildcard,
    };
};

pub const Type = struct {
    tag: Tag,
    span: Span,
    data: union(Tag) {
        Path: Path,
        Primitive: Primitive,
        Array: ArrayType,
        Pointer: PointerType,
        Reference: ReferenceType,
        Function: FunctionType,
    },

    pub const Tag = enum {
        Path,
        Primitive,
        Array,
        Pointer,
        Reference,
        Function,
    };
};

pub const Primitive = enum {
    u32,
    u64,
    usize,
    i32,
    i64,
    f32,
    f64,
    bool,
    char,
    str,
    String,
};

pub const PointerType = struct {
    is_mut: bool,
    child: *Type,
};

pub const ReferenceType = struct {
    is_mut: bool,
    child: *Type,
};

pub const FunctionType = struct {
    params: []Type,
    return_type: *Type,
};

pub const ArrayType = struct {
    element_type: *Type,
    size_expr: *Expr,
};

pub const Path = struct {
    segments: []Identifier,
    generic_args: []Type,
    span: Span,
};

pub const Expr = struct {
    tag: Tag,
    span: Span,
    data: union(Tag) {
        Literal: Literal,
        Path: Path,
        Block: Block,
        If: IfExpr,
        While: WhileExpr,
        For: ForExpr,
        Return: ReturnExpr,
        Binary: BinaryExpr,
        Unary: UnaryExpr,
        Call: CallExpr,
        Index: IndexExpr,
        Field: FieldExpr,
        MethodCall: MethodCallExpr,
        Assignment: AssignmentExpr,
        Cast: CastExpr,
        Range: RangeExpr,
        Lambda: LambdaExpr,
        Array: ArrayExpr,
        StructInit: StructInitExpr,
        Paren: *Expr,
    },

    pub const Tag = enum {
        Literal,
        Path,
        Block,
        If,
        While,
        For,
        Return,
        Binary,
        Unary,
        Call,
        Index,
        Field,
        MethodCall,
        Assignment,
        Cast,
        Range,
        Lambda,
        Array,
        StructInit,
        Paren,
    };
};

pub const Literal = struct {
    kind: Kind,
    lexeme: []const u8,

    pub const Kind = enum {
        Int,
        Float,
        Bool,
        Char,
        String,
    };
};

pub const BinaryExpr = struct {
    op: BinaryOp,
    left: *Expr,
    right: *Expr,
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

pub const RangeExpr = struct {
    inclusive: bool,
    start: *Expr,
    end: *Expr,
};

pub const UnaryExpr = struct {
    op: UnaryOp,
    expr: *Expr,
};

pub const UnaryOp = enum {
    Not,
    Neg,
    Deref,
    Ref,
    RefMut,
};

pub const AssignmentExpr = struct {
    target: *Expr,
    op: AssignOp,
    value: *Expr,
};

pub const AssignOp = enum {
    Assign,
    AddAssign,
    SubAssign,
    MulAssign,
    DivAssign,
};

pub const CastExpr = struct {
    expr: *Expr,
    ty: *Type,
};

pub const IfExpr = struct {
    condition: *Expr,
    then_block: Block,
    else_expr: ?*Expr,
};

pub const WhileExpr = struct {
    condition: *Expr,
    body: Block,
};

pub const ForExpr = struct {
    pattern: Pattern,
    iterator: *Expr,
    body: Block,
};

pub const ReturnExpr = struct {
    value: ?*Expr,
};

pub const CallExpr = struct {
    callee: *Expr,
    args: []Expr,
};

pub const IndexExpr = struct {
    target: *Expr,
    index: *Expr,
};

pub const FieldExpr = struct {
    target: *Expr,
    field: Identifier,
};

pub const MethodCallExpr = struct {
    target: *Expr,
    method: Identifier,
    args: []Expr,
};

pub const LambdaExpr = struct {
    params: []Param,
    body: LambdaBody,
};

pub const LambdaBody = union(enum) {
    Expr: *Expr,
    Block: Block,
};

pub const ArrayExpr = struct {
    elements: []Expr,
};

pub const StructInitExpr = struct {
    path: Path,
    fields: []StructInitField,
};

pub const StructInitField = struct {
    name: Identifier,
    value: Expr,
};

pub const Block = struct {
    stmts: []Stmt,
    result: ?*Expr,
    span: Span,
};

pub const Stmt = struct {
    tag: Tag,
    span: Span,
    data: union(Tag) {
        Let: LetStmt,
        Expr: ExprStmt,
        While: WhileExpr,
        If: IfExpr,
        For: ForExpr,
        Return: ReturnExpr,
        Empty: void,
    },

    pub const Tag = enum {
        Let,
        Expr,
        While,
        If,
        For,
        Return,
        Empty,
    };
};

pub const LetStmt = struct {
    mutable: bool,
    pattern: Pattern,
    ty: ?Type,
    value: ?*Expr,
};

pub const ExprStmt = struct {
    expr: Expr,
};
