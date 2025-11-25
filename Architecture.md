# Rust-Subset Compiler Architecture

This document describes the architecture and code structure for a Rust-subset compiler implemented in Zig.

The grammar is defined separately in `grammar.ebnf` and is not repeated here. This document focuses on the modules, data structures, and compilation pipeline.

---

## 1. Goals and Non-Goals

### 1.1 Goals

- Implement a **Rust-like language** as defined in `grammar.ebnf`.
- Compiler written in **Zig**.
- End-to-end pipeline from **source** to **x86-64**:
  - Lexing → Parsing → AST → HIR → MIR → Optimizations → x86-64 → assembly/object code.
- Incorporate **simple optimizations**:
  - Constant folding
  - Dead code elimination (DCE)
  - Simple CFG simplification
- Emit **x86-64** code, initially as textual assembly (`.s`).
- Produce useful **diagnostics** (file/line/column, error messages).

### 1.2 Non-Goals (initially)

- Full Rust semantics (lifetimes, borrow checker, traits, full macro system).
- Advanced optimizations (loop unrolling, vectorization, inlining, etc.).
- Cross-platform codegen beyond x86-64 in the first version.
- Full support for all grammar features at once:
  - Some constructs (e.g., macros, advanced generics, method resolution) may initially be partially supported or rejected with diagnostics.

---

## 2. High-Level Pipeline

The compiler pipeline is:

```text
Source (.rs-like)
  → Lexer (tokens)
  → Parser (AST)
  → HIR (resolved, typed AST)
  → MIR (mid-level IR, non-SSA, basic blocks)
  → MIR optimization passes
  → x86-64 backend (machine IR, regalloc, peephole)
  → Assembly (.s) or Object (.o)
````

Each stage is implemented as a separate module with clear inputs and outputs.

---

## 3. Project Layout

Proposed directory structure:

```text
src/
  main.zig
  driver.zig

  diag/
    source_map.zig
    diagnostics.zig

  frontend/
    tokens.zig
    lexer.zig
    ast.zig
    parser.zig

  hir/
    hir.zig
    name_res.zig
    typecheck.zig

  mir/
    mir.zig
    builder.zig
    passes/
      const_fold.zig
      dce.zig
      cfg_simplify.zig

  backend/
    x86_64/
      abi.zig
      isel.zig
      regalloc.zig
      peephole.zig
      emitter.zig

  util/
    arena.zig
    interner.zig
    cfg.zig
```

Additional files:

* `grammar.ebnf` – language grammar (already exists).
* `architecture.md` – this document.
* `tests/` – test inputs and expected outputs (see section 10).

---

## 4. Entry Point and Driver

### 4.1 `src/main.zig`

Responsibilities:

* Parse command-line arguments.
* Set up global allocator.
* Invoke `driver.compileFile`.

Key options:

* Input file path.
* Output mode: `.s` (assembly), possibly `.o` later.
* Optimization level: `none` / `basic`.

### 4.2 `src/driver.zig`

Responsibilities:

* Orchestrate **entire compilation pipeline** for a single file.
* Provide a single function:

```zig
pub const CompileOptions = struct {
    opt_level: enum { none, basic },
    emit: enum { assembly, object },
};

pub fn compileFile(
    allocator: std.mem.Allocator,
    path: []const u8,
    options: CompileOptions,
) !void;
```

Steps inside `compileFile`:

1. Load file contents.
2. Initialize diagnostics and source map.
3. Lex input → tokens.
4. Parse tokens → AST crate.
5. Lower AST → HIR crate (name resolution).
6. Typecheck HIR.
7. Lower HIR → MIR crate.
8. Run MIR optimization passes (depending on `opt_level`).
9. Lower MIR → x86-64 machine IR.
10. Run register allocation and peephole passes.
11. Emit assembly or object file.

---

## 5. Diagnostics and Source Map (`diag/`)

### 5.1 `source_map.zig`

Responsibilities:

* Track source files and their contents.
* Provide mapping from byte offsets to `(line, column)` for diagnostics.
* Represent spans:

```zig
pub const FileId = u32;

pub const Span = struct {
    file_id: FileId,
    start: usize, // byte offset
    end: usize,   // byte offset
};
```

### 5.2 `diagnostics.zig`

Responsibilities:

* Central diagnostics collector.
* Store errors/warnings with messages and spans.
* Pretty-print messages with annotated source snippets.

Key types:

```zig
pub const Severity = enum { error, warning };

pub const Diagnostic = struct {
    severity: Severity,
    message: []const u8,
    primary_span: Span,
    // optional additional spans/notes
};

pub const Diagnostics = struct {
    // internal storage, allocator, etc.

    pub fn init(allocator: std.mem.Allocator) Diagnostics;
    pub fn deinit(self: *Diagnostics) void;

    pub fn reportError(self: *Diagnostics, span: Span, msg: []const u8) void;
    pub fn reportWarning(self: *Diagnostics, span: Span, msg: []const u8) void;

    pub fn hasErrors(self: *Diagnostics) bool;
    pub fn emitAll(self: *Diagnostics, source_map: *SourceMap) !void;
};
```

All phases should log to `Diagnostics` instead of panicking on user errors.

---

## 6. Frontend (`frontend/`)

### 6.1 Tokens (`tokens.zig`)

Responsibilities:

* Define token kinds and token structures used by lexer and parser.

Essential enum:

* `TokenKind`:

  * identifiers
  * literals (int, float, bool, char, string)
  * keywords (`fn`, `struct`, `impl`, `type`, `let`, `mut`, `if`, `else`, `while`, `for`, `in`, `return`, `as`, boolean literals)
  * punctuation (`(`, `)`, `{`, `}`, `[`, `]`, `,`, `;`, `:`, `::`, `->`, `.`, `!`, `&&`, `||`)
  * operators (`+`, `-`, `*`, `/`, `%`, `=`, `+=`, `-=`, `*=`, `/=`, `==`, `!=`, `<`, `<=`, `>`, `>=`, `..`, `..=`)

Token type:

```zig
pub const Token = struct {
    kind: TokenKind,
    lexeme: []const u8, // slice into original source
    span: Span,         // from SourceMap
};
```

### 6.2 Lexer (`lexer.zig`)

Responsibilities:

* Implement lexical analysis per `grammar.ebnf` token-level rules.
* Convert source text into `[]Token`.
* Distinguish identifiers vs keywords (including `BoolLit`).
* Handle numeric literals (`IntLit`, `FloatLit`), strings, chars.
* Attach spans for diagnostics.

Public API:

```zig
pub fn lex(
    allocator: std.mem.Allocator,
    file_id: FileId,
    src: []const u8,
    diagnostics: *Diagnostics,
) ![]Token;
```

### 6.3 AST (`ast.zig`)

Responsibilities:

* Represent parsed syntax tree closely following `grammar.ebnf`.
* Keep AST untyped and unresolved, but structurally accurate.
* Use ID-based arenas for nodes.

Key arena IDs:

```zig
pub const ExprId = u32;
pub const StmtId = u32;
pub const ItemId = u32;
pub const TypeSyntaxId = u32;
pub const PatternId = u32;
```

Representative enums:

* `ExprKind`:

  * literals
  * `Path` / variable references
  * binary operators, unary operators
  * assignment expressions
  * `if` expressions
  * `for`, `while`, etc. as expression/statement nodes
  * function calls, method calls, indexing, field access
  * block `{ ... }`
  * struct literal, array literal
  * closure/lambda (if implemented)

* `StmtKind`:

  * `Let` statement
  * `Expr` (with or without semicolon)
  * `Return`
  * `While`
  * `For`
  * `If` (statement form)
  * Empty `;`

* `ItemKind`:

  * `FnItem`
  * `StructItem`
  * `ImplItem`
  * `TypeAliasItem`

AST crate type:

```zig
pub const AstCrate = struct {
    items: []ItemId;
    // arenas for Expr, Stmt, TypeSyntax, Pattern, etc.
};
```

### 6.4 Parser (`parser.zig`)

Responsibilities:

* Implement recursive descent / Pratt parser guided by `grammar.ebnf`.
* Convert token stream into `AstCrate`.
* Report syntax errors via `Diagnostics`.
* Recover from some errors to continue parsing and show multiple errors.

Public API:

```zig
pub fn parseCrate(
    allocator: std.mem.Allocator,
    tokens: []const Token,
    diagnostics: *Diagnostics,
) !AstCrate;
```

Note: `grammar.ebnf` is the canonical spec – parser should follow it.

---

## 7. HIR: Name Resolution and Type Checking (`hir/`)

### 7.1 HIR Representation (`hir.zig`)

Responsibilities:

* Represent a **resolved, typed** variant of the AST.
* Use IDs for functions, types, locals, etc.
* Attach types to expressions.

Core IDs:

```zig
pub const DefId = u32;    // top-level item (fn, struct, type alias, etc.)
pub const LocalId = u32;  // function parameters and local bindings
pub const TypeId = u32;
pub const HirExprId = u32;
pub const HirStmtId = u32;
pub const HirItemId = u32;
```

Type representation (initially limited to core types from `PrimType` and basic compound types):

```zig
pub const HirTypeKind = union(enum) {
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
    // Type parameters / generics may be added later
};
```

Expression representation:

```zig
pub const HirExprKind = union(enum) {
    LocalRef: LocalId,
    GlobalRef: DefId,      // e.g. function or static variable
    ConstInt: i64,
    ConstFloat: f64,
    ConstBool: bool,
    ConstChar: u21,
    ConstString: []const u8,

    Binary: struct { op: BinaryOp, lhs: HirExprId, rhs: HirExprId },
    Unary: struct { op: UnaryOp, expr: HirExprId },

    Call: struct { callee: HirExprId, args: []HirExprId },
    MethodCall: struct { recv: HirExprId, method_def: DefId, args: []HirExprId },

    Block: struct { stmts: []HirStmtId, tail: ?HirExprId },

    If: struct { cond: HirExprId, then_block: HirExprId, else_block: ?HirExprId },
    While: struct { cond: HirExprId, body: HirExprId },
    For: struct { pat: PatternId, iter: HirExprId, body: HirExprId }, // may be desugared later

    Assign: struct { place: HirExprId, value: HirExprId },
    AssignOp: struct { op: BinaryOp, place: HirExprId, value: HirExprId },
    // etc.
};

pub const HirExpr = struct {
    kind: HirExprKind,
    ty: TypeId,
};
```

HIR crate:

```zig
pub const HirCrate = struct {
    items: []HirItemId;
    // arenas for HIR exprs, stmts, types, patterns, etc.
};
```

### 7.2 Name Resolution (`name_res.zig`)

Responsibilities:

* Build symbol tables for:

  * Module scope (functions, structs, type aliases).
  * Struct fields.
  * Impl methods.
  * Function scopes (parameters, locals).
* Resolve:

  * Paths to `DefId`s.
  * Local bindings (pattern bindings) to `LocalId`s.
* Handle shadowing and scoping rules.
* Issue diagnostics for:

  * Unresolved names.
  * Duplicate definitions.

### 7.3 Type Checking (`typecheck.zig`

Responsibilities:

* Walk HIR expressions and statements to ensure type correctness.
* Infer types where syntax omits them (e.g. `let x = 1;`).
* Enforce:

  * Operator/type compatibility (`+` on integers, etc.).
  * Return type matching function signature.
  * Assignment compatibility (`lhs` type vs RHS type).
  * Basic type rules for casts (`as`).

Output:

* Fully typed `HirCrate` where all `HirExpr.ty` fields are filled.
* Diagnostics on type errors.

Borrow checker and lifetime analysis are explicitly **out of scope** for the initial version.

---

## 8. MIR: Mid-Level IR (`mir/`)

MIR is a **simple, non-SSA, three-address IR** with explicit basic blocks and locals. It is the main target for optimizations and the source for code generation.

### 8.1 MIR Representation (`mir.zig`)

Core IDs:

```zig
pub const TempId = u32;   // temporaries produced by instructions
pub const LocalId = u32;  // function locals/parameters (mapped from HIR LocalId)
pub const BlockId = u32;
```

Types:

```zig
pub const MirType = enum {
    I32,
    I64,
    U32,
    U64,
    Usize,
    F32,
    F64,
    Bool,
    // extend as needed
};
```

Operands:

```zig
pub const Operand = union(enum) {
    Temp: TempId,
    Local: LocalId,
    ImmInt: i64,
    ImmFloat: f64,
    ImmBool: bool,
};
```

Operations:

```zig
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
        fn_id: u32,       // mapping to Hir/DefId or backend func
        args: []Operand,
    },

    // Potential extensions:
    // LoadField, StoreField, LoadIndex, StoreIndex, etc.
};

pub const Inst = struct {
    ty: ?MirType,   // type of result, if any
    dest: ?TempId,  // destination temp, if any
    kind: InstKind,
};

pub const Block = struct {
    insts: []Inst,
    term: TermKind,
};

pub const MirFn = struct {
    name: []const u8,
    params: []LocalId,     // parameters as locals
    locals: []MirType,     // locals[LocalId] -> MirType
    ret_ty: MirType,
    blocks: []Block,
};

pub const MirCrate = struct {
    fns: []MirFn;
};
```

### 8.2 MIR Builder (`builder.zig`)

Responsibilities:

* Lower each `HirFn` to `MirFn`.
* Allocate `LocalId` indices for parameters and locals.
* Maintain a current block and generate additional blocks as needed.
* For each HIR expression:

  * Emit MIR `Inst`s and `TermKind`.
  * Return an `Operand` representing the value (typically `Operand.Temp`).

Key lowering patterns:

* **Assignments:**

  * `x = 1`:

    * `t0 = Copy(ImmInt(1))`
    * `StoreLocal(local_x, t0)`

* **Binary expressions:**

  * `a + b`:

    * Lower `a` → `op_a`
    * Lower `b` → `op_b`
    * `t = Bin(Add, op_a, op_b)`

* **If/else:**

  * Introduce `cond_block`, `then_block`, `else_block`, `join_block`.
  * Lower condition to `cond_operand`.
  * Emit `TermKind.If`.
  * Lower then/else bodies into their blocks.
  * Branch to `join_block` at ends of both branches.
  * If the `if` has a value, use locals or temps to carry the merged value.

* **While loops:**

  * Basic structure:

    ```text
    entry → loop_cond → loop_body → loop_cond
                       ↘ exit
    ```

  * Lower condition at `loop_cond`.

  * `TermKind.If` to branch into body or exit.

### 8.3 MIR Design Rationale

* Non-SSA simplifies implementation (no PHI nodes).
* All variable state is modeled through `LocalId` and `{Load, Store}Local`.
* Temporaries (`TempId`) are single-assignment for each instruction, simplifying data flow reasoning.
* MIR is still typed (`MirType`), making optimization and codegen safer.

---

## 9. MIR Optimization Passes (`mir/passes/`)

All passes operate on `MirCrate` and are optional based on `CompileOptions.opt_level`.

### 9.1 Constant Folding (`const_fold.zig`)

Responsibilities:

* For each `MirFn` / `Block` / `Inst`:

  * If `Bin` or `Cmp` has constant operands (`ImmInt`, `ImmBool`, etc.), evaluate at compile time.
  * Replace instruction with `Copy Imm*` result, adjust `ty` and `dest` accordingly.
  * Propagate constants in simple chains.

### 9.2 Dead Code Elimination (`dce.zig`)

Responsibilities:

* First pass: mark used temps.

  * For each operand in all instructions and terminators, mark `TempId` as used.
* Second pass: remove instructions that:

  * produce an unused `TempId` in `dest`, and
  * have no side effects (`StoreLocal`, `Call` considered to have side effects).
* Simplify functions by removing useless computations.

### 9.3 CFG Simplification (`cfg_simplify.zig`)

Responsibilities:

* If `TermKind.If` has constant condition:

  * Replace with `Goto` to taken branch.
* Identify unreachable blocks (never jumped to) and remove them.
* Optionally merge trivial blocks:

  * Block `A` with no instructions and `Goto B`, and `A` has a single predecessor, can be folded into its predecessor.

---

## 10. x86-64 Backend (`backend/x86_64/`)

Targets: x86-64, initially System V ABI (Linux/macOS). Windows x64 ABI can be added later.

### 10.1 Machine IR Representation

Core types:

```zig
pub const PhysReg = enum {
    RAX, RBX, RCX, RDX,
    RSI, RDI, R8, R9, R10, R11,
    R12, R13, R14, R15,
    RBP, RSP,
};

pub const VReg = u32; // virtual register for temps

pub const MemRef = struct {
    base: PhysReg,
    offset: i32,
    // optional index/scale later
};

pub const MOperand = union(enum) {
    VReg: VReg,
    Phys: PhysReg,
    Imm: i64,
    Mem: MemRef,
};

pub const MOpcode = enum {
    Mov,
    Add, Sub, Imul, Idiv,
    And, Or, Xor,
    Cmp, Test,
    Jmp,
    Jcc, // with condition code enum
    Call,
    Ret,
    // push/pop, lea, etc. as needed
};

pub const MInst = struct {
    op: MOpcode,
    dst: ?MOperand,
    src1: ?MOperand,
    src2: ?MOperand,
};

pub const MBlock = struct {
    insts: []MInst,
};

pub const MFn = struct {
    name: []const u8,
    blocks: []MBlock,
    stack_size: usize,
};

pub const MachineCrate = struct {
    fns: []MFn,
};
```

### 10.2 ABI Handling (`abi.zig`)

Responsibilities:

* Implement calling convention:

  * Map function parameters to registers/stack slots.
  * Align stack.
  * Handle return values.
* Provide utility functions used by instruction selection:

```zig
pub fn assignParamsToLocals(
    fn: *MirFn,
    abi_info: *AbiInfo,
) void;
```

### 10.3 Instruction Selection (`isel.zig`)

Responsibilities:

* Lower `MirFn` to `MFn`.
* Map:

  * MIR `LocalId` → stack slots.
  * MIR `TempId` → virtual registers (`VReg`).
* Translate instructions:

  * `Bin` → sequence of `Mov` + `Add/Sub/Imul/Idiv`.
  * `Cmp` → `Cmp`/`Test` plus subsequent `Jcc` usage.
  * `LoadLocal` / `StoreLocal` → `Mov` with `MemRef`.
  * `TermKind.If` → `Cmp/Test` + `Jcc`.
  * `TermKind.Goto` → `Jmp`.
  * `TermKind.Ret` → move return value into return register and `Ret`.
* Insert prologue and epilogue:

  * `push rbp; mov rbp, rsp; sub rsp, stack_size;`
  * `leave; ret`.

Public API:

```zig
pub fn lowerCrate(
    allocator: std.mem.Allocator,
    mir: *const MirCrate,
) !MachineCrate;
```

### 10.4 Register Allocation (`regalloc.zig`)

Responsibilities:

* Implement a simple **linear scan** register allocator on `MFn`.

Steps:

1. Assign each `VReg` an interval: first and last instruction index where it is used.
2. Sort intervals by start.
3. Maintain active set of intervals and available physical registers (e.g., caller-saved).
4. On overlap:

   * If no free register, choose a victim interval to spill.
   * Insert loads/stores for spilled intervals.
5. Rewrite `MOperand.VReg` into:

   * `MOperand.Phys` or
   * `MOperand.Mem` using stack slots.

### 10.5 Peephole Optimizations (`peephole.zig`)

Responsibilities:

* Walk `MFn` instructions and apply small local transformations:

Examples:

* Eliminate `mov reg, reg`.
* Eliminate `add reg, 0` / `sub reg, 0`.
* Simplify simple `cmp`/`test` patterns if possible.

### 10.6 Emitter (`emitter.zig`)

Responsibilities:

* Convert `MachineCrate` to textual assembly (`.s`) or object (`.o`).

Initial focus:

* Emit `.s` with:

  * Global labels.
  * Function prologue/epilogue.
  * Data sections if needed.

API:

```zig
pub fn emit(
    mc: *const MachineCrate,
    options: CompileOptions,
    input_path: []const u8,
) !void;
```

---

## 11. Utilities (`util/`)

### 11.1 Arena Allocator (`arena.zig`)

Responsibilities:

* Provide bump-allocator style arenas for AST, HIR, MIR.
* Avoid many small allocations from the general allocator.

### 11.2 String/Identifier Interner (`interner.zig`)

Responsibilities:

* Map `[]const u8` to a stable `IdentId` (u32).
* Use for identifiers, type names, etc.
* Enable fast equality checks and deduplication.

### 11.3 CFG Helpers (`cfg.zig`)

Responsibilities:

* Common functions for CFG analysis on MIR/Machine IR:

  * Predecessor/successor computation.
  * Reachability analysis.
  * Block orderings if needed.

---

## 12. Testing Strategy

### 12.1 Layout

* `tests/lexer/` – lexer tests (source → token streams).
* `tests/parser/` – parser tests (source → AST shape).
* `tests/hir/` – name resolution and type checking tests.
* `tests/mir/` – MIR generation tests (source → MIR dumps).
* `tests/backend/` – assembly output tests and runtime execution tests.

### 12.2 Types of Tests

* **Unit tests** inside each module (Zig `test` blocks).
* **Golden-file tests**:

  * input `.rs`-like file
  * expected `.mir`, `.s` snapshots for comparison.
* **Execution tests**:

  * compile source to `.s`, assemble and link, run executable,
  * check exit code or printed output.

---

## 13. Incremental Implementation Plan

1. Core infra:

   * `source_map`, `diagnostics`, `arena`, `interner`.
2. Lexing + parsing:

   * Enough of grammar to handle basic `fn main()` with expressions and control flow.
3. HIR + name resolution:

   * Functions, locals, basic expressions.
4. Typechecker:

   * Primitive types and arithmetic, control flow.
5. MIR + builder:

   * Simple functions, `let`, assignments, `if`, `while`.
6. MIR passes:

   * Constant folding, DCE, CFG simplify.
7. Backend:

   * x86-64 lowering for arithmetic, locals, and returns.
   * Register allocation and emission.
8. Extend to cover more of grammar (`struct`, `impl`, `for`, method calls, etc.).
