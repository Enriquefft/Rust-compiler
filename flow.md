# Compilation Flow: From Source to Assembly

This document provides a detailed walkthrough of how the Rust-subset compiler transforms source code into x86-64 assembly, with emphasis on complex logic, edge cases, and how key features are implemented at each stage.

---

## Table of Contents

1. [Overview](#1-overview)
2. [Lexer](#2-lexer)
3. [Parser](#3-parser)
4. [Diagnostics System](#4-diagnostics-system)
5. [HIR (High-Level IR)](#5-hir-high-level-ir)
6. [MIR (Mid-Level IR)](#6-mir-mid-level-ir)
7. [Code Generation (x86-64 Backend)](#7-code-generation-x86-64-backend)

---

## 1. Overview

The compiler follows a multi-stage pipeline:

```
Source Code (.rs-like)
    │
    ▼
┌─────────┐
│  Lexer  │  Tokenizes source into tokens
└────┬────┘
     │
     ▼
┌─────────┐
│ Parser  │  Builds Abstract Syntax Tree (AST)
└────┬────┘
     │
     ▼
┌─────────┐
│   HIR   │  Name resolution, type checking, borrow checking
└────┬────┘
     │
     ▼
┌─────────┐
│   MIR   │  Control flow graph, optimization passes
└────┬────┘
     │
     ▼
┌─────────┐
│ Backend │  Instruction selection, register allocation, emission
└────┬────┘
     │
     ▼
  Assembly (.s)
```

Each stage transforms the representation while adding semantic information and validating correctness.

---

## 2. Lexer

**Location:** `src/frontend/lexer.zig`

### Overview

The lexer performs lexical analysis, converting raw source text into a stream of tokens. It is implemented as a single-pass scanner that handles:

- **Whitespace and comments:** Skips spaces, tabs, newlines, and both line (`//`) and block (`/* */`) comments.
- **Operators and punctuation:** Recognizes single and multi-character operators like `::`, `->`, `+=`, `..=`, etc.
- **Literals:** Integer, float, boolean (`true`/`false`), character (`'a'`), and string (`"hello"`) literals.
- **Identifiers and keywords:** Distinguishes between user identifiers and reserved keywords.

### Token Structure

```zig
pub const Token = struct {
    kind: TokenKind,
    lexeme: []const u8,  // Slice into original source
    span: Span,          // File ID, start/end byte offsets
};
```

### Edge Cases Handled

1. **Multi-character operators:** The lexer uses lookahead to distinguish `=` from `==`, `-` from `->`, `.` from `..` and `..=`.

2. **Numeric literals with decimals:** A sequence like `123.456` is recognized as a float, while `123` alone is an integer.

3. **Escape sequences in strings:** The lexer preserves raw string content; escape processing happens during HIR lowering.

4. **Block comment nesting:** Unterminated block comments are reported as diagnostics rather than causing a panic.

---

## 3. Parser

**Location:** `src/frontend/parser.zig`

### Overview

The parser implements a recursive descent parser with Pratt parsing for expression precedence. It converts the token stream into an Abstract Syntax Tree (AST) that closely mirrors the grammar in `grammar.ebnf`.

### Parsing Technique

- **Recursive descent** for statements and declarations (functions, structs, impl blocks).
- **Pratt parsing (operator precedence)** for expressions, handling associativity and precedence levels.

### Expression Precedence (lowest to highest)

1. Assignment: `=`, `+=`, `-=`, `*=`, `/=`
2. Logical OR: `||`
3. Logical AND: `&&`
4. Equality: `==`, `!=`
5. Comparison: `<`, `<=`, `>`, `>=`
6. Range: `..`, `..=`
7. Addition: `+`, `-`
8. Multiplication: `*`, `/`, `%`
9. Cast: `as`
10. Unary: `!`, `-`, `*`, `&`, `&mut`
11. Postfix: calls `()`, index `[]`, field `.`, method `.name()`

### Key Parsing Challenges

**1. Struct Literal Ambiguity:**

The parser must distinguish between:
- Block after condition: `if x { ... }`
- Struct literal: `Foo { x: 1 }`

Solution: The `no_struct_literal` flag disables struct literal parsing in contexts where a block is expected after an expression (if condition, while condition, for iterator).

**2. Generic Angle Brackets:**

Parsing `<T>` for generics conflicts with comparison operators. The parser uses context to determine intent - generics appear after type names and function names.

**3. Error Recovery:**

On syntax errors, the parser calls `synchronize()` to skip tokens until reaching a statement boundary (semicolon, `fn`, `struct`, etc.), allowing multiple errors to be reported.

---

## 4. Diagnostics System

**Location:** `src/diag/diagnostics.zig`, `src/diag/source_map.zig`

### Source Map

The source map tracks:
- File contents indexed by `FileId`
- Spans representing byte ranges within files
- Mapping from byte offsets to line/column for user-friendly output

```zig
pub const Span = struct {
    file_id: FileId,
    start: usize,  // Byte offset
    end: usize,    // Byte offset
};
```

### Diagnostics Collector

All compiler phases report errors and warnings to a central diagnostics collector rather than panicking:

```zig
pub const Diagnostics = struct {
    pub fn reportError(self: *Diagnostics, span: Span, msg: []const u8) void;
    pub fn reportWarning(self: *Diagnostics, span: Span, msg: []const u8) void;
    pub fn hasErrors(self: *Diagnostics) bool;
};
```

### Benefits

- **Multiple errors:** Users see all errors in one compilation attempt.
- **Rich context:** Errors include source location and can show code snippets.
- **Recoverable phases:** Each phase continues processing after non-fatal errors.

---

## 5. HIR (High-Level IR)

**Location:** `src/hir/`

The HIR (High-level Intermediate Representation) is a resolved, typed variant of the AST. It transforms unresolved identifiers into explicit references and attaches type information to all expressions.

### 5.1 AST to HIR Lowering

**Location:** `src/hir/hir.zig`

The initial HIR lowering converts AST nodes to HIR nodes while:
- Flattening nested structures into arena-indexed collections
- Converting paths to `UnresolvedIdent` for later resolution
- Preserving spans for error reporting

#### Key Transformations

**Macro Calls → Function Calls:**
```rust
println!("hello {}!", x);
```
Becomes a call expression with `println` as the callee and the format string plus arguments as parameters.

**Struct Initialization:**
```rust
let p = Point { x: 1, y: 2 };
```
Lowered to a `StructInit` expression with path `["Point"]` and fields with their values.

**Lambda Expressions:**
```rust
let f = |x: i32| x + 1;
```
Lowered with explicit parameter patterns, optional type annotations, and a body expression.

### 5.2 Name Resolution

**Location:** `src/hir/name_res.zig`

Name resolution converts unresolved identifiers to either local references (`LocalRef`) or global references (`GlobalRef`).

#### Resolution Process

1. **Module-level symbol table:** Built first with all top-level items (functions, structs, type aliases, constants).

2. **Function-local resolution:** For each function:
   - Add parameters to local symbol table
   - Traverse body, resolving identifiers and binding let patterns
   - Lambdas get a copy of the enclosing scope's locals

#### Scoping Rules

- Function parameters are in scope for the entire body
- Let bindings shadow earlier bindings with the same name (except within the same scope)
- Duplicate definitions in the same scope produce an error

#### Edge Cases

**1. Two-Segment Paths (`Counter::new`):**
Associated functions are resolved by first checking for a struct with the type name, then looking up the mangled method name (`Counter_new`).

**2. Builtin Macro Registration:**
Special functions like `println`, `print`, `format`, `eprintln` are registered as builtin macros with handlers for special lowering:

```zig
const builtin_macro_specs = [_]BuiltinMacroSpec{
    .{ .name = "println", .handler = .println },
    .{ .name = "print", .handler = .print },
    // ...
};
```

**3. Method Name Mangling:**
Methods in impl blocks are mangled as `StructName_methodName` to create unique global names:
```rust
impl Point {
    fn new(x: i32, y: i32) -> Point { ... }
}
// Mangled to: Point_new
```

### 5.3 Type Checking

**Location:** `src/hir/typecheck.zig`

Type checking assigns types to all expressions and validates type consistency.

#### Type Inference

Expressions without explicit annotations have types inferred from context:
- Integer literals → `i64`
- Float literals → `f64`
- Boolean literals → `bool`
- Character literals → `char`
- String literals → `&str` (or `String` for explicit `String::from(...)`)

#### Type Compatibility Rules

The `typesCompatible` function determines if two types can be used together:

1. **Primitive numeric types:** Different integer types (`i32`, `i64`, etc.) are compatible for arithmetic
2. **Reference coercion:** `&T` and `&mut T` can coerce in certain contexts
3. **String compatibility:** `String` and `&str` are compatible for printing
4. **Generic type parameters:** A declared type parameter matches any concrete type
5. **Pointer/reference conversion:** Implicit conversion from reference to raw pointer is allowed

#### Operator Type Checking

| Operator Category | Required Operand Types | Result Type |
|-------------------|----------------------|-------------|
| `+`, `-`, `*`, `/`, `%` | Same numeric type on both sides | Same as operands |
| `==`, `!=`, `<`, `<=`, `>`, `>=` | Same type on both sides | `bool` |
| `&&`, `||` | `bool` on both sides | `bool` |
| `!` | `bool` | `bool` |
| `-` (unary) | Numeric | Same as operand |
| `*` (deref) | Pointer or reference | Inner type |
| `&`, `&mut` | Any | `&T` or `&mut T` |

#### Edge Cases in Type Checking

**1. Generic Functions:**
```rust
fn identity<T>(x: T) -> T { x }
```
The return type is inferred from the argument type when the function is called.

**2. Generic Struct Field Access:**
```rust
struct Wrapper<T> { value: T }
let w = Wrapper { value: 42 };
```
The type checker infers `T = i64` from the field initializer and uses it for subsequent field accesses.

**3. Array Element Type Consistency:**
All elements in an array literal must have the same type:
```rust
let arr = [1, 2, "hello"];  // ERROR: array elements must have the same type
```

**4. Unsafe Context Tracking:**
Dereferencing raw pointers requires an `unsafe` block:
```rust
let ptr: *const i32 = ...;
let x = *ptr;  // ERROR: unsafe operation outside unsafe block

unsafe {
    let x = *ptr;  // OK
}
```

**5. Method Resolution with `&self`:**
When calling `point.offset(1, 2)`, the type checker determines if `offset` takes `self`, `&self`, or `&mut self` and validates the call accordingly.

### 5.4 Ownership and Borrowing

**Location:** `src/hir/ownership.zig`

The borrow checker enforces Rust-inspired ownership semantics at the HIR level.

#### Copy vs Move Types

**Copy types** (can be used after assignment):
- Primitives: `i32`, `i64`, `f32`, `f64`, `bool`, `char`
- References: `&T`, `&mut T`
- Raw pointers: `*const T`, `*mut T`
- Function types: `fn(...) -> T`
- String slices: `str`

**Move types** (ownership transferred on use):
- `String`
- User-defined structs
- Arrays (treated conservatively)

#### Borrow Rules Enforced

1. **Move tracking:** Non-Copy values can only be used once:
```rust
let s = String::from("hello");
let t = s;  // s moved to t
println!("{}", s);  // ERROR: use of moved value
```

2. **Mutable borrow exclusivity:** Only one mutable borrow at a time:
```rust
let mut x = 5;
let r1 = &mut x;
let r2 = &mut x;  // ERROR: cannot take mutable borrow while another is active
```

3. **Immutable borrows block mutable borrows:**
```rust
let mut x = 5;
let r1 = &x;
let r2 = &mut x;  // ERROR: cannot take mutable borrow while immutable borrow is active
```

4. **Cannot move borrowed values:**
```rust
let s = String::from("hello");
let r = &s;
let t = s;  // ERROR: cannot move out of a borrowed value
```

#### Lexical Lifetimes

Borrows end at block boundaries (lexical scope):
```rust
let mut x = 5;
{
    let r = &x;
}  // Borrow of x ends here
let r2 = &mut x;  // OK: previous borrow is out of scope
```

#### Scope and State Management

The checker maintains:
- `LocalState` per variable: type, moved flag, immutable borrow count, mutable borrowed flag
- `Scope` stack: tracks active borrows per scope
- Borrows are released when their containing scope exits

---

## 6. MIR (Mid-Level IR)

**Location:** `src/mir/`

MIR is a simple, non-SSA, three-address IR with explicit basic blocks. It serves as the target for optimization passes and the source for code generation.

### 6.1 HIR to MIR Lowering

**Location:** `src/mir/lower.zig`

The lowering transforms high-level constructs into a control-flow graph of basic blocks with simple instructions.

#### MIR Representation

**Operands:**
```zig
pub const Operand = union(enum) {
    Temp: TempId,           // Virtual register
    Local: LocalId,         // Stack slot
    Param: u32,             // Function parameter
    ImmInt: i64,            // Integer constant
    ImmFloat: f64,          // Float constant
    ImmBool: bool,          // Boolean constant
    ImmChar: u21,           // Character constant
    ImmString: []const u8,  // String literal
    Symbol: []const u8,     // Function name
    Global: u32,            // Global definition ID
};
```

**Instructions:**
```zig
pub const InstKind = union(enum) {
    Copy: { src: Operand },
    Bin: { op: BinOp, lhs: Operand, rhs: Operand },
    Cmp: { op: CmpOp, lhs: Operand, rhs: Operand },
    Unary: { op: UnaryOp, operand: Operand },
    LoadLocal: { local: LocalId },
    StoreLocal: { local: LocalId, src: Operand },
    StorePtr: { ptr: Operand, src: Operand },
    StoreIndex: { target: Operand, index: Operand, src: Operand },
    StoreField: { target: Operand, name: []const u8, src: Operand },
    Call: { target: Operand, args: []Operand },
    Index: { target: Operand, index: Operand },
    Field: { target: Operand, name: []const u8 },
    Array: { elems: []Operand },
    StructInit: { fields: []StructField },
    // ...
};
```

**Block Terminators:**
```zig
pub const TermKind = union(enum) {
    Goto: BlockId,
    If: { cond: Operand, then_block: BlockId, else_block: BlockId },
    Ret: ?Operand,
};
```

### 6.2 Control Flow Lowering

#### If Expressions

```rust
let x = if cond { 1 } else { 2 };
```

Lowered to:
```
Block 0 (entry):
    t0 = <cond evaluation>
    If t0 then Block1 else Block2

Block 1 (then):
    StoreLocal result_local, 1
    Goto Block3

Block 2 (else):
    StoreLocal result_local, 2
    Goto Block3

Block 3 (join):
    // result available in result_local
```

#### While Loops

```rust
while cond {
    body
}
```

Lowered to:
```
Block 0 (entry):
    Goto Block1

Block 1 (cond):
    t0 = <cond evaluation>
    If t0 then Block2 else Block3

Block 2 (body):
    <body instructions>
    Goto Block1

Block 3 (exit):
    // continue execution
```

#### For Loops (Range-based)

```rust
for i in 0..10 {
    body
}
```

Lowered to:
```
Block 0 (entry):
    StoreLocal loop_var, 0
    Goto Block1

Block 1 (cond):
    t0 = LoadLocal loop_var
    t1 = Cmp Lt t0, 10
    If t1 then Block2 else Block3

Block 2 (body):
    <body instructions>
    t2 = LoadLocal loop_var
    t3 = Bin Add t2, 1
    StoreLocal loop_var, t3
    Goto Block1

Block 3 (exit):
    // continue execution
```

#### For Loops (Array-based)

```rust
for elem in array {
    body
}
```

The lowering creates an index counter and loads elements by index:
1. Initialize index to 0
2. Condition: index < array.len
3. Body: load element at index, execute body, increment index

#### Short-Circuit Logical Operators

`&&` and `||` are lowered to conditional branches to implement short-circuit evaluation:

```rust
a && b
```

Lowered to:
```
Block 0:
    t0 = <a>
    If t0 then Block1 else Block2

Block 1:
    t1 = <b>
    StoreTemp result, t1
    Goto Block3

Block 2:
    StoreTemp result, false
    Goto Block3

Block 3:
    // result in result temp
```

### 6.3 Struct and Array Handling

#### Struct Layout Computation

Struct layouts are computed during HIR→MIR lowering based on declaration order:

```zig
pub const FieldLayout = struct {
    name: []const u8,
    offset: i32,         // Offset from struct base (negative for stack growth)
    size: u32,           // Field size in bytes
    index: u32,          // Declaration order index
};
```

Each field occupies `LOCAL_STACK_MULTIPLIER × 8` bytes (32 bytes by default).

**Example:**
```rust
struct Point { x: i64, y: i64 }
```
Layout:
- `x`: offset 0, index 0
- `y`: offset -32, index 1
- Total size: 64 bytes

#### Struct Initialization

```rust
let p = Point { x: 1, y: 2 };
```

Lowered to separate `StoreField` instructions:
```
StoreField local_p, "x", 1
StoreField local_p, "y", 2
```

#### Struct Parameter Passing

Structs passed as parameters are "flattened" - each field is passed as a separate argument:

```rust
fn consume(p: Point) { ... }
consume(my_point);
```

MIR generates:
```
Call consume, [my_point.x, my_point.y]
```

The callee reconstructs the struct from individual parameters.

#### Array Storage

Arrays allocate contiguous stack slots. For an array of N elements:
- Elements are stored at 8-byte intervals
- Local slots needed: `ceil(N / LOCAL_STACK_MULTIPLIER)`

For arrays of structs, the element size is multiplied by the struct's field count.

### 6.4 MIR Optimization Passes

**Location:** `src/mir/passes/`

Passes are executed in sequence: noop → constant folding → dead code elimination → CFG simplification.

#### Constant Folding

**Location:** `src/mir/passes/constant_folding.zig`

Evaluates compile-time constant expressions:

```
t0 = Bin Add, ImmInt(2), ImmInt(3)
```
Becomes:
```
t0 = Copy ImmInt(5)
```

Supported operations:
- Arithmetic: `+`, `-`, `*`, `/`, `%` on integers
- Comparisons: `==`, `!=`, `<`, `<=`, `>`, `>=`
- Logical: `&&`, `||`, `!`
- Boolean short-circuit evaluation

**Edge Cases:**
- Division by zero is not folded (runtime error)
- Float operations are handled separately
- Only immediate operands are folded

#### Dead Code Elimination

**Location:** `src/mir/passes/dead_code_elimination.zig`

Removes instructions whose results are never used:

1. **Mark used temps:** Scan all instructions and terminators for operand references
2. **Remove dead instructions:** Instructions producing unused temps with no side effects

**Side-effect instructions (never removed):**
- `StoreLocal`, `StorePtr`, `StoreIndex`, `StoreField`
- `Call` (may have side effects)

#### CFG Simplification

**Location:** `src/mir/passes/cfg_simplify.zig`

Simplifies control flow:

1. **Constant condition elimination:**
   ```
   If true then B1 else B2  →  Goto B1
   ```

2. **Unreachable block removal:** Blocks never jumped to are eliminated

3. **Block merging:** A block with only a `Goto` terminator and single predecessor can be merged

---

## 7. Code Generation (x86-64 Backend)

**Location:** `src/backend/x86_64/`

The backend transforms MIR into x86-64 assembly following the System V AMD64 ABI.

### 7.1 Instruction Selection

**Location:** `src/backend/x86_64/isel.zig`

Converts MIR instructions to machine instructions with virtual registers.

#### Register Mapping

- **Temporaries (`TempId`)** → Virtual registers (`VReg`)
- **Locals (`LocalId`)** → Stack slots (`[rbp - offset]`)
- **Parameters** → According to ABI (rdi, rsi, rdx, rcx, r8, r9, then stack)

#### Key Instruction Translations

**Binary Operations:**
```
MIR: t2 = Bin Add, t0, t1
x86: mov vreg2, vreg0
     add vreg2, vreg1
```

**Comparisons:**
```
MIR: t1 = Cmp Lt, t0, ImmInt(10)
x86: cmp vreg0, 10
     setl al
     movzx vreg1, al
```

**Division (special handling):**
Division uses `idiv` which requires dividend in `rax:rdx`:
```
MIR: t2 = Bin Div, t0, t1
x86: mov rax, vreg0
     cqo              ; Sign-extend rax into rdx
     idiv vreg1
     mov vreg2, rax   ; Quotient
```

For modulo, the result comes from `rdx` instead.

**Function Calls:**
```
MIR: t0 = Call "add", [arg0, arg1]
x86: mov rdi, arg0
     mov rsi, arg1
     call add
     mov vreg0, rax
```

#### Field Access

Field offsets are looked up from stored struct layouts:

```
MIR: Field { target: Local(0), name: "y" }
```

The backend computes the memory reference:
```
[rbp - local_offset - field_offset]
```

#### Array Indexing

```
MIR: Index { target: Local(arr), index: t0 }
```

Dynamic indices require address computation:
```
x86: lea r11, [rbp - arr_offset]
     mov rax, vreg0       ; index
     imul rax, 8          ; element size
     add r11, rax
     mov vreg_result, [r11]
```

### 7.2 Register Allocation

**Location:** `src/backend/x86_64/regalloc.zig`

Implements linear scan register allocation.

#### Algorithm

1. **Compute live intervals:** For each VReg, find first and last use instruction index

2. **Sort by start:** Process intervals in order of their start points

3. **Allocate registers:**
   - Maintain set of active intervals (currently in registers)
   - When processing a new interval:
     - Expire intervals that end before current start
     - If free register available, assign it
     - Otherwise, spill (either current or longest-living active interval)

4. **Handle spills:**
   - Allocate stack slot for spilled VReg
   - Insert load before uses, store after definitions

#### Available Registers

Caller-saved (preferred): `rax`, `rcx`, `rdx`, `rsi`, `rdi`, `r8`, `r9`, `r10`, `r11`

Callee-saved (preserved across calls): `rbx`, `r12`, `r13`, `r14`, `r15`

Special purpose: `rbp` (frame pointer), `rsp` (stack pointer)

#### XMM Registers for Floats

Float values use XMM registers: `xmm0` through `xmm15`

### 7.3 Assembly Emission

**Location:** `src/backend/x86_64/emitter.zig`

Produces Intel syntax x86-64 assembly.

#### Generated Assembly Structure

```asm
# x86_64 assembly
.intel_syntax noprefix
.extern printf

.section .rodata
.Lstr0:
    .byte 72, 101, 108, 108, 111, 0   ; "Hello\0"

.text
.globl main
main:
    push rbp
    mov rbp, rsp
    sub rsp, 64          ; Stack frame

    ; ... function body ...

    leave
    ret

.section .note.GNU-stack,"",@progbits
```

#### String Literals

String literals are placed in `.rodata` with unique labels:
```asm
.Lstr0:
    .byte 72, 101, 108, 108, 111, 10, 0  ; "Hello\n\0"
```

#### Printf Lowering

`println!` macros are lowered to `printf` calls:

```rust
println!("value: {}", x);
```

Becomes:
```asm
    lea rdi, [rip + .Lstr0]  ; Format string: "value: %ld\n"
    mov rsi, [rbp - 8]       ; x value
    xor eax, eax             ; varargs: 0 float args
    call printf
```

Format conversion: `{}` → `%ld` (i64), `%d` (i32), `%f` (float), `%s` (string)

### 7.4 Edge Cases in Codegen

#### Large Immediate Values

x86-64 only allows 32-bit signed immediates for memory stores. Values outside this range require a register intermediary:

```asm
; For storing large constant to memory:
    mov r11, 0x123456789ABC
    mov qword ptr [rbp - 8], r11
```

#### Division Operand Constraints

`idiv` cannot use immediate operands:
```asm
; MIR: Bin Div, Local(0), ImmInt(5)
    mov r11, 5
    mov rax, [rbp - 8]
    cqo
    idiv r11
```

#### Callee-Saved Register Preservation

Functions that use callee-saved registers must save and restore them:
```asm
main:
    push rbp
    mov rbp, rsp
    sub rsp, 64
    mov qword ptr [rbp - 56], rbx   ; Save rbx
    mov qword ptr [rbp - 64], r12   ; Save r12

    ; ... use rbx and r12 ...

    mov rbx, qword ptr [rbp - 56]   ; Restore rbx
    mov r12, qword ptr [rbp - 64]   ; Restore r12
    leave
    ret
```

#### Unsafe Pointer Dereference

Raw pointer dereference generates a simple memory access:
```asm
; *ptr = 42
    mov rax, [rbp - 8]      ; Load ptr
    mov qword ptr [rax], 42 ; Store through ptr
```

#### Reference Creation

Taking a reference generates `lea` (load effective address):
```asm
; let r = &x
    lea rax, [rbp - 8]
    mov [rbp - 16], rax
```

#### Struct Method Calls with `&self`

Methods with reference parameters receive a pointer:
```asm
; point.offset(1, 2) where offset takes &self
    lea rdi, [rbp - 24]     ; Address of point
    mov rsi, 1              ; dx argument
    mov rdx, 2              ; dy argument
    call Point_offset
```

#### Array Length Intrinsic

`arr.len()` is compiled to a constant since array sizes are known at compile time:
```asm
; let n = arr.len();  where arr: [i32; 10]
    mov qword ptr [rbp - 8], 10
```

---

## Summary

The compilation flow progressively transforms source code through increasingly lower-level representations:

1. **Lexer:** Text → Tokens (handles syntax edge cases)
2. **Parser:** Tokens → AST (manages precedence and ambiguity)
3. **HIR:** AST → Resolved, typed IR (validates semantics, tracks ownership)
4. **MIR:** HIR → CFG with simple instructions (enables optimization)
5. **Backend:** MIR → Assembly (handles ABI, registers, memory layout)

Each stage adds semantic information while lowering abstraction level, with careful handling of edge cases throughout to ensure correct code generation for the full range of supported Rust-subset programs.
