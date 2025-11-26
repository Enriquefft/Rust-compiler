const std = @import("std");

pub const PhysReg = enum {
    rax,
    rbx,
    rcx,
    rdx,
    rsi,
    rdi,
    r8,
    r9,
    r10,
    r11,
    rbp,
    rsp,
};

pub const Condition = enum {
    eq,
    ne,
    lt,
    le,
    gt,
    ge,
};

pub const VReg = u32;

pub const MemRef = struct {
    base: PhysReg,
    offset: i32,
};

pub const MOperand = union(enum) {
    VReg: VReg,
    Phys: PhysReg,
    Imm: i64,
    Label: []const u8,
    Mem: MemRef,
};

pub const BinOpcode = enum { add, sub, imul, idiv, imod, and_, or_, xor_ };

pub const UnaryOpcode = enum { not_, neg };

pub const InstKind = union(enum) {
    Mov: struct { dst: MOperand, src: MOperand },
    Bin: struct { op: BinOpcode, dst: MOperand, lhs: MOperand, rhs: MOperand },
    Unary: struct { op: UnaryOpcode, dst: MOperand, src: MOperand },
    Cmp: struct { lhs: MOperand, rhs: MOperand },
    Setcc: struct { cond: Condition, dst: MOperand },
    Test: struct { operand: MOperand },
    Jmp: u32,
    Jcc: struct { cond: Condition, target: u32 },
    Call: union(enum) { Direct: []const u8, Indirect: MOperand },
    Ret: ?MOperand,
};

pub const MachineBlock = struct {
    id: u32,
    insts: []InstKind,
};

pub const MachineFn = struct {
    name: []const u8,
    blocks: []MachineBlock,
    stack_size: usize,
    vreg_count: usize,

    pub fn deinit(self: *MachineFn, allocator: std.mem.Allocator) void {
        for (self.blocks) |*blk| {
            allocator.free(blk.insts);
        }
        allocator.free(self.blocks);
    }
};

pub const DataItem = struct {
    label: []const u8,
    bytes: []const u8,
};

pub const MachineCrate = struct {
    allocator: std.mem.Allocator,
    fns: []MachineFn,
    rodata: []DataItem,
    externs: []const []const u8,

    pub fn deinit(self: *MachineCrate) void {
        for (self.fns) |*f| {
            f.deinit(self.allocator);
        }
        self.allocator.free(self.fns);

        for (self.rodata) |item| {
            self.allocator.free(item.label);
            self.allocator.free(item.bytes);
        }
        self.allocator.free(self.rodata);

        for (self.externs) |name| {
            self.allocator.free(name);
        }
        self.allocator.free(self.externs);
    }
};
