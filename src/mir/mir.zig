const std = @import("std");

pub const TempId = u32;
pub const LocalId = u32;
pub const BlockId = u32;

pub const MirType = enum {
    I32,
    I64,
    U32,
    U64,
    Usize,
    F32,
    F64,
    Bool,
    Char,
    String,
    Str,
    Array,
    Pointer,
    Struct,
    Unknown,
};

pub const Operand = union(enum) {
    Temp: TempId,
    Local: LocalId,
    Param: u32,
    RetSecond, // Second return value (rdx in x86-64 calling convention)
    ImmInt: i64,
    ImmFloat: f64,
    ImmBool: bool,
    ImmChar: u21,
    ImmString: []const u8,
    Symbol: []const u8,
    Global: u32,
};

pub const BinOp = enum { Add, Sub, Mul, Div, Mod, And, Or, Xor };
pub const CmpOp = enum { Eq, Ne, Lt, Le, Gt, Ge };
pub const UnaryOp = enum { Not, Neg, Ref, Deref };

pub const TermKind = union(enum) {
    Goto: BlockId,
    If: struct { cond: Operand, then_block: BlockId, else_block: BlockId },
    Ret: ?Operand,
};

pub const InstKind = union(enum) {
    Copy: struct { src: Operand },

    Bin: struct { op: BinOp, lhs: Operand, rhs: Operand },
    Cmp: struct { op: CmpOp, lhs: Operand, rhs: Operand },
    Unary: struct { op: UnaryOp, operand: Operand },
    Cast: struct { src: Operand, from_ty: MirType, to_ty: MirType },

    LoadLocal: struct { local: LocalId },
    StoreLocal: struct { local: LocalId, src: Operand },

    Call: struct {
        target: Operand,
        args: []Operand,
    },

    Range: struct { inclusive: bool, start: Operand, end: Operand },
    Index: struct { target: Operand, index: Operand },
    Field: struct { target: Operand, name: []const u8 },
    Array: struct { elems: []Operand },
    StructInit: struct { fields: []StructField },
};

pub const StructField = struct { name: []const u8, value: Operand };

pub const Inst = struct {
    ty: ?MirType,
    dest: ?TempId,
    kind: InstKind,
};

pub const Block = struct {
    insts: []Inst,
    term: TermKind,
};

pub const MirFn = struct {
    name: []const u8,
    params: []LocalId,
    locals: []MirType,
    ret_ty: ?MirType,
    blocks: []Block,
};

pub const MirCrate = struct {
    arena: std.heap.ArenaAllocator,
    fns: std.ArrayListUnmanaged(MirFn),

    pub fn init(backing_allocator: std.mem.Allocator) MirCrate {
        return .{ .arena = std.heap.ArenaAllocator.init(backing_allocator), .fns = .{} };
    }

    pub fn deinit(self: *MirCrate) void {
        self.fns.deinit(self.arena.allocator());
        self.arena.deinit();
    }

    pub fn allocator(self: *MirCrate) std.mem.Allocator {
        return self.arena.allocator();
    }
};
