//! Mid-Level Intermediate Representation (MIR) for the Rust-subset compiler.
//!
//! MIR is a simple, non-SSA, three-address IR with explicit basic blocks and locals.
//! It serves as the main target for optimizations and the source for code generation.
//! The representation uses typed temporaries, explicit control flow through block terminators,
//! and models all variable state through `LocalId` and `{Load,Store}Local` instructions.

const std = @import("std");

/// Unique identifier for a temporary value produced by an instruction.
/// Temporaries are single-assignment within each instruction, simplifying data flow analysis.
pub const TempId = u32;

/// Unique identifier for a function local variable or parameter.
/// Maps directly from HIR LocalId and represents stack-allocated storage.
pub const LocalId = u32;

/// Unique identifier for a basic block within a function.
/// Block 0 is always the entry block of a function.
pub const BlockId = u32;

/// Represents the type of a MIR value.
/// Maps from HIR types to a simplified set of primitive and compound types
/// used during optimization and code generation.
pub const MirType = enum {
    /// 32-bit signed integer
    I32,
    /// 64-bit signed integer
    I64,
    /// 32-bit unsigned integer
    U32,
    /// 64-bit unsigned integer
    U64,
    /// Platform-native unsigned integer for sizes and indices
    Usize,
    /// 32-bit floating point
    F32,
    /// 64-bit floating point
    F64,
    /// Boolean value (true/false)
    Bool,
    /// Unicode scalar value (21-bit codepoint)
    Char,
    /// Owned string type
    String,
    /// Borrowed string slice type
    Str,
    /// Fixed-size array type
    Array,
    /// Raw or smart pointer type
    Pointer,
    /// User-defined struct type
    Struct,
    /// Type could not be determined during lowering
    Unknown,
};

/// Represents a source operand in MIR instructions.
/// Operands can be temporaries, locals, parameters, immediate values, or symbolic references.
pub const Operand = union(enum) {
    /// Reference to a temporary value produced by a prior instruction
    Temp: TempId,
    /// Reference to a local variable or function parameter
    Local: LocalId,
    /// Direct reference to a function parameter by index (used before storing to local)
    Param: u32,
    /// Second return value location (rdx in x86-64 calling convention)
    /// Used for returning multiple values from functions
    RetSecond,
    /// Immediate signed integer constant
    ImmInt: i64,
    /// Immediate floating-point constant
    ImmFloat: f64,
    /// Immediate boolean constant
    ImmBool: bool,
    /// Immediate Unicode character constant (21-bit codepoint)
    ImmChar: u21,
    /// Immediate string literal constant
    ImmString: []const u8,
    /// Symbolic reference to a named entity (e.g., function name)
    Symbol: []const u8,
    /// Reference to a global definition by its DefId
    Global: u32,
};

/// Binary arithmetic and logical operations supported in MIR.
pub const BinOp = enum {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    And,
    Or,
    Xor,
};

/// Comparison operations that produce boolean results.
pub const CmpOp = enum {
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
};

/// Unary operations supported in MIR.
pub const UnaryOp = enum {
    /// Logical not (for booleans)
    Not,
    /// Arithmetic negation (for integers/floats)
    Neg,
    /// Take address of a value
    Ref,
    /// Dereference a pointer
    Deref,
};

/// Block terminator instructions that transfer control flow.
/// Every basic block ends with exactly one terminator.
pub const TermKind = union(enum) {
    /// Unconditional jump to a target block
    Goto: BlockId,
    /// Conditional branch based on a boolean condition
    If: struct {
        /// Boolean operand determining branch direction
        cond: Operand,
        /// Block to jump to when condition is true
        then_block: BlockId,
        /// Block to jump to when condition is false
        else_block: BlockId,
    },
    /// Return from the function with an optional value
    Ret: ?Operand,
};

/// The kind of operation performed by an instruction.
/// Each instruction produces at most one temporary value in its destination.
pub const InstKind = union(enum) {
    /// Copy a value from source to destination
    Copy: struct { src: Operand },

    /// Binary arithmetic or logical operation
    Bin: struct { op: BinOp, lhs: Operand, rhs: Operand },
    /// Comparison operation producing a boolean result
    Cmp: struct { op: CmpOp, lhs: Operand, rhs: Operand },
    /// Unary operation (negation, not, reference, dereference)
    Unary: struct { op: UnaryOp, operand: Operand },
    /// Type cast between compatible types
    Cast: struct { src: Operand, from_ty: MirType, to_ty: MirType },

    /// Load a value from a local variable into a temporary
    LoadLocal: struct { local: LocalId },
    /// Store a value into a local variable
    StoreLocal: struct { local: LocalId, src: Operand },
    /// Store a value through a pointer
    StorePtr: struct { ptr: Operand, src: Operand },

    /// Function call with target and arguments
    Call: struct {
        /// The function to call (Symbol, Global, or indirect through Temp)
        target: Operand,
        /// Arguments to pass to the function
        args: []Operand,
    },

    /// Construct a range value (used in for loops)
    Range: struct { inclusive: bool, start: Operand, end: Operand },
    /// Index into an array or slice
    Index: struct { target: Operand, index: Operand },
    /// Access a field of a struct
    Field: struct { target: Operand, name: []const u8 },
    /// Construct an array literal
    Array: struct { elems: []Operand },
    /// Construct a struct literal
    StructInit: struct { fields: []StructField },
};

/// A field initializer in a struct literal expression.
pub const StructField = struct {
    /// Name of the field being initialized
    name: []const u8,
    /// Value to assign to the field
    value: Operand,
};

/// A single MIR instruction with optional type and destination.
pub const Inst = struct {
    /// Type of the result value, if the instruction produces one
    ty: ?MirType,
    /// Destination temporary for the result, if any
    dest: ?TempId,
    /// The operation this instruction performs
    kind: InstKind,
};

/// A basic block containing a sequence of instructions and a terminator.
/// Control flow within a block is sequential; branching only occurs at the terminator.
pub const Block = struct {
    /// Instructions executed in order within this block
    insts: []Inst,
    /// Terminator that ends this block and transfers control
    term: TermKind,
};

/// A MIR function definition containing locals and control flow graph.
pub const MirFn = struct {
    /// Function name for linking and debugging
    name: []const u8,
    /// Parameter local IDs (subset of locals that are function parameters)
    params: []LocalId,
    /// Types of all local variables (indexed by LocalId)
    locals: []MirType,
    /// Return type of the function, or null for void functions
    ret_ty: ?MirType,
    /// Basic blocks forming the function's control flow graph
    blocks: []Block,
};

/// A collection of MIR functions representing a compilation unit.
pub const MirCrate = struct {
    /// Arena allocator for all MIR data structures
    arena: std.heap.ArenaAllocator,
    /// List of functions in this crate
    fns: std.ArrayListUnmanaged(MirFn),

    /// Initialize an empty MIR crate with the given backing allocator.
    pub fn init(backing_allocator: std.mem.Allocator) MirCrate {
        return .{ .arena = std.heap.ArenaAllocator.init(backing_allocator), .fns = .{} };
    }

    /// Release all memory associated with this crate.
    pub fn deinit(self: *MirCrate) void {
        self.fns.deinit(self.arena.allocator());
        self.arena.deinit();
    }

    /// Get the arena allocator for allocating MIR data.
    pub fn allocator(self: *MirCrate) std.mem.Allocator {
        return self.arena.allocator();
    }
};
