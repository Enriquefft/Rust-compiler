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
    Unknown,
};

pub const Operand = union(enum) {
    Temp: TempId,
    Local: LocalId,
    ImmInt: i64,
    ImmFloat: f64,
    ImmBool: bool,
};

pub const BinOp = enum { Add, Sub, Mul, Div, Mod, And, Or, Xor };
pub const CmpOp = enum { Eq, Ne, Lt, Le, Gt, Ge };

pub const TermKind = union(enum) {
    Goto: BlockId,
    If: struct { cond: Operand, then_block: BlockId, else_block: BlockId },
    Ret: ?Operand,
};

pub const InstKind = union(enum) {
    Copy: struct { src: Operand },

    Bin: struct { op: BinOp, lhs: Operand, rhs: Operand },
    Cmp: struct { op: CmpOp, lhs: Operand, rhs: Operand },

    LoadLocal: struct { local: LocalId },
    StoreLocal: struct { local: LocalId, src: Operand },

    Call: struct {
        fn_id: u32,
        args: []Operand,
    },
};

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
