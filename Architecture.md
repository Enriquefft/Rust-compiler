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

Current directory structure:

```text
src/
  main.zig              # Entry point and CLI argument parsing
  driver.zig            # Compiler driver orchestrating the pipeline
  all_tests.zig         # Aggregated test runner
  shared_consts.zig     # Shared constants across modules

  diag/
    source_map.zig      # Source file tracking and span mapping
    diagnostics.zig     # Central diagnostics collector

  frontend/
    tokens.zig          # Token kinds and structures
    lexer.zig           # Lexical analysis
    ast.zig             # AST node definitions
    ast_printer.zig     # AST pretty-printing for debugging
    parser.zig          # Recursive descent parser

  hir/
    hir.zig             # HIR representation and lowering from AST
    hir_printer.zig     # HIR pretty-printing for debugging
    name_res.zig        # Name resolution pass
    typecheck.zig       # Type checking pass

  mir/
    mir.zig             # MIR representation
    lower.zig           # HIR to MIR lowering (builder)
    mir_printer.zig     # MIR pretty-printing for debugging
    passes/
      passes.zig        # Pass infrastructure and runner
      constant_folding.zig
      dead_code_elimination.zig
      cfg_simplify.zig
      debug_dump.zig

  backend/
    backend.zig         # Backend entry point
    x86_64/
      codegen.zig       # x86-64 code generation orchestration
      machine.zig       # Machine IR representation
      isel.zig          # Instruction selection
      regalloc.zig      # Register allocation
      emitter.zig       # Assembly text emission

codes/                  # Sample programs and test inputs
```

Additional files:

* `grammar.ebnf` – language grammar.
* `Architecture.md` – this document.
* `Features.md` – language subset description.
* `README.md` – project overview.

> **Note:** The `util/` directory mentioned in earlier versions of this document is not currently implemented. Utility functions are inlined in the modules that use them. A separate `tests/` directory with golden files may be added in the future.

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
  * closure/lambda

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
    For: struct { pat: PatternId, iter: HirExprId, body: HirExprId },

    Assign: struct { place: HirExprId, value: HirExprId },
    AssignOp: struct { op: BinaryOp, place: HirExprId, value: HirExprId },
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
    Char,
    String,
    Str
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

### 8.2 MIR Lowering (`lower.zig`)

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


    * **For loops (`for pat in expr { body }`):**

      We do not desugar `for` into a `while` node in HIR. Instead, the MIR
      builder lowers `for` directly to basic blocks using `TermKind.If` and `TermKind.Goto`.

      Initial implementation only supports `for` over numeric ranges:

      * `for x in a..b { body }`
      * `for x in a..=b { body }`

      where `a` and `b` are integer expressions.

      Lowering pattern:

      1. Lower the range expression:
         * For `a..b`:
           * `start = lowerExpr(a)`
           * `end   = lowerExpr(b)`
           * `inclusive = false`
         * For `a..=b`:
           * `start = lowerExpr(a)`
           * `end   = lowerExpr(b)`
           * `inclusive = true`

         Allocate locals for `start`, `end`, and the loop index `idx`.
         Emit stores:
         * `StoreLocal(local_start, start)`
         * `StoreLocal(local_end, end)`
         * `StoreLocal(local_idx, start)`

      2. Create blocks:
         * `loop_cond`: evaluates the loop condition.
         * `loop_body`: executes the body.
         * `loop_step`: increments the index.
         * `loop_exit`: continuation after the loop.

      3. From the current block, emit `TermKind.Goto(loop_cond)`.

      4. In `loop_cond`:
         * Load `idx` and `end` into temps.
         * Depending on `inclusive`:
           * Half-open: `cond = Cmp(Lt, idx, end)`
           * Closed:    `cond = Cmp(Le, idx, end)`
         * Emit `TermKind.If { cond, then_block = loop_body, else_block = loop_exit }`.

      5. In `loop_body`:
         * Bind the loop pattern:
           * If `pat` is an identifier `x`, map it to the same local as `idx`,
             or create a new local for `x` and copy `idx` into it.
           * If `pat` is `_`, no binding is created.
         * Lower the loop body statements into `loop_body` using the usual
           stmt/expr lowering rules.
         * At the end of `loop_body`, emit `TermKind.Goto(loop_step)`.

      6. In `loop_step`:
         * Load `idx` into a temp, emit `t = Bin(Add, idx, ImmInt(1))`,
           then `StoreLocal(local_idx, t)`.
         * Emit `TermKind.Goto(loop_cond)`.

      7. After lowering, set the current block to `loop_exit`.

      Because MIR has no `break`/`continue` in this subset, there are no
      extra edges out of the loop body. If we introduce `break`/`continue`
      later, they will target `loop_exit` / `loop_step` respectively.

* **`println!` macro:**

  * Parsed as a macro call but lowered directly in MIR builder so the rest of
    the pipeline sees a normal `Call`.
  * Behaves like Rust's `println!` surface:

    * `println!();` emits a newline-only call.
    * `println!("hello there!");` prints the literal string with a trailing newline.
    * `println!("format {} arguments", "some");` supports `{}` placeholders.
    * `println!("format {local_variable} arguments");` captures in-scope
      identifiers inline.

  * Lowering strategy:

    * Append an implicit newline to the format string if the user didn't
      include one.
    * Convert Rust-style `{}` / `{name}` placeholders to a C-style format
      string (`%s`, `%lld`, `%d`, etc.) chosen from the argument types.
    * Emit a single `InstKind.Call` targeting a backend-declared `printf` and
      pass the rewritten format string plus evaluated operands as arguments.



### 8.3 MIR Design Rationale

* Non-SSA simplifies implementation (no PHI nodes).
* All variable state is modeled through `LocalId` and `{Load, Store}Local`.
* Temporaries (`TempId`) are single-assignment for each instruction, simplifying data flow reasoning.
* MIR is still typed (`MirType`), making optimization and codegen safer.

---

## 9. MIR Optimization Passes (`mir/passes/`)

All passes operate on `MirCrate` and are optional based on `CompileOptions.opt_level`.

### 9.1 Constant Folding (`constant_folding.zig`)

Responsibilities:

* For each `MirFn` / `Block` / `Inst`:

  * If `Bin` or `Cmp` has constant operands (`ImmInt`, `ImmBool`, etc.), evaluate at compile time.
  * Replace instruction with `Copy Imm*` result, adjust `ty` and `dest` accordingly.
  * Propagate constants in simple chains.

### 9.2 Dead Code Elimination (`dead_code_elimination.zig`)

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

### 10.2 Instruction Selection (`isel.zig`)

Responsibilities:

* Lower `MirFn` to `MachineFn`.
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
* Handle ABI concerns inline:

  * Map function parameters to registers/stack slots per System V ABI.
  * Align stack appropriately.
* Insert prologue and epilogue:

  * `push rbp; mov rbp, rsp; sub rsp, stack_size;`
  * `leave; ret`.

Public API:

```zig
pub fn lowerCrate(
    allocator: std.mem.Allocator,
    mir: *const MirCrate,
    diagnostics: *Diagnostics,
) !MachineCrate;
```

### 10.3 Register Allocation (`regalloc.zig`)

Responsibilities:

* Implement a simple **linear scan** register allocator on `MachineFn`.

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

### 10.4 Emitter (`emitter.zig`)

Responsibilities:

* Convert `MachineCrate` to textual assembly (`.s`).

Current implementation:

* Emit `.s` with:

  * Global labels.
  * Function prologue/epilogue.
  * Data sections for string literals.
  * External symbol declarations (e.g., `printf`).

API:

```zig
pub fn emitAssembly(
    allocator: std.mem.Allocator,
    mc: *const MachineCrate,
) ![]const u8;
```

> **Note:** Object file (`.o`) emission is not yet implemented. A dedicated `peephole.zig` pass is planned but not currently present.

### 10.5 `println!` lowering and libc interop

* Backend assumes the MIR builder has rewritten `println!` into a direct call
  to `printf` with a C-style format string and any evaluated operands.
* No runtime shim is introduced; the emitter must:

  * Declare `printf` as an external symbol and follow System V varargs
    conventions when lowering the `Call`.
  * Place the rewritten format string (with the trailing `\n`) in a data
    section accessible to the function using it.
  * Ensure arguments are passed in the correct registers/stack slots for a
    varargs call (ints in `rdi`/`rsi`/`rdx`/`rcx`/`r8`/`r9`, floats in `xmm*`,
    extras on the stack).

* Because the call goes straight to libc, linking must pull in the platform's
  C standard library; no intermediary Zig helper is provided.

---

## 11. Utilities

> **Note:** A dedicated `util/` directory is not currently implemented. The compiler uses:
>
> * Zig's built-in `std.heap.ArenaAllocator` for arena allocation within each IR crate (AST, HIR, MIR).
> * Inline string handling within modules rather than a separate interner.
> * CFG analysis functions embedded in the optimization passes and backend.
>
> Future versions may extract common utilities into a shared module.

---

## 12. Testing Strategy

### 12.1 Current Layout

Tests are implemented as inline Zig `test` blocks within source modules:

* `src/frontend/lexer.zig` – lexer unit tests
* `src/frontend/parser.zig` – parser unit tests (via `parser_tests.zig`)
* `src/hir/hir.zig` – HIR lowering tests
* `src/mir/passes/*.zig` – MIR pass tests
* `src/backend/backend.zig` – backend integration tests
* `src/driver.zig` – end-to-end compilation tests
* `src/all_tests.zig` – aggregated test runner importing all modules

Sample source files are located in `codes/` and used for integration testing.

### 12.2 Types of Tests

* **Unit tests** inside each module (Zig `test` blocks), run via `zig build test`.
* **End-to-end tests** in `driver.zig` that compile source snippets and verify outputs.
* **Sample programs** in `codes/` directory for manual testing and validation.

### 12.3 Future Enhancements

* **Golden-file tests** with input `.rs`-like files and expected `.mir`, `.s` snapshots.
* **Execution tests** that compile, assemble, link, and run executables to check output.
* Dedicated `tests/` directory structure may be added as the test suite grows.

---

## 13. Incremental Implementation Plan

1. Core infra:

   * `source_map`, `diagnostics`.
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
