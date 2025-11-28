# Failing Tests Analysis Report

**Generated:** 2025-11-28  
**Test Results:** 27 passed, 6 failed, 0 skipped

This report provides a comprehensive analysis of failing tests, their root causes, required fixes, and any grammar changes needed.

---

## Summary of Failing Tests

| Test File | Failure Type | Root Cause | Severity |
|-----------|-------------|------------|----------|
| `arrays3.rs` | Compilation failed | Unsupported index base for x86_64 lowering | Medium |
| `dummy_lib.rs` | Execution failed | Missing `main` function (by design) | Low (Expected) |
| `expressions_test1.rs` | Execution failed | Assembly error: ambiguous operand size for `idiv` | High |
| `functions_and_methods_test2.rs` | Compilation failed | Unresolved associated function `Counter::new` | High |
| `generics_test1.rs` | Compilation failed | Generic type inference failure | High |
| `generics_test2.rs` | Compilation failed | Missing struct field shorthand syntax + integer suffix parsing | High |

---

## Detailed Analysis

### 1. expressions_test1.rs - Assembly `idiv` Operand Size Bug

**Test File:**
```rust
fn main() {
    let a:i32 = 8;
    let b:i32 = 3;
    let arithmetic = (a + b) * (a - b) / b;
    let comparisons = (a > b) as i32 + (a == b) as i32 + (a <= b) as i32;
    let boolean = (a > b && b % 2 == 1) || false;
    println!("{} {} {}", arithmetic, comparisons, boolean);
}
```

**Expected Output:** `18 1 true`

**Error Message:**
```
/tmp/.../expressions_test1.s:27: Error: ambiguous operand size for `idiv'
```

**Root Cause:**  
The x86-64 backend emitter generates `idiv [rbp-64]` without a size qualifier. When the divisor is a memory operand, the assembler needs to know whether it's a 32-bit or 64-bit operation.

**Bug Location:** `src/backend/x86_64/emitter.zig:138`

**Current Code (problematic):**
```zig
} else {
    try writer.writeAll("    idiv ");
    try writeOperand(writer, payload.rhs);
    try writer.writeByte('\n');
}
```

**Fix Required:**
When `payload.rhs` is a `.Mem` operand, prefix it with `qword ptr`:
```zig
} else {
    if (payload.rhs == .Mem) {
        try writer.writeAll("    idiv qword ptr ");
        try writeMem(writer, payload.rhs.Mem);
    } else {
        try writer.writeAll("    idiv ");
        try writeOperand(writer, payload.rhs);
    }
    try writer.writeByte('\n');
}
```

**Difficulty:** Easy  
**Files to Modify:** `src/backend/x86_64/emitter.zig`

---

### 2. arrays3.rs - Unsupported Index Base for Nested Struct Field Access

**Test File:**
```rust
struct Point { x: i32, y: i32 }

fn main() {
    let mut points: [Point; 3] = [
        Point { x: 1, y: 2 },
        Point { x: -3, y: 4 },
        Point { x: 10, y: -5 },
    ];

    for i in 0..points.len() {
        points[i].x += 1;
        points[i].y += 1;
    }

    for p in &points {
        println!("({}, {})", p.x, p.y);
    }
}
```

**Expected Output:**
```
(2, 3)
(-2, 5)
(11, -4)
```

**Error Message:**
```
error: unsupported index base for x86_64 lowering
```

**Root Cause:**  
The x86-64 instruction selection code (`isel.zig`) cannot handle compound place expressions like `points[i].x`. When indexing into an array element and then accessing a field, the backend encounters an unsupported operand combination.

**Bug Location:** `src/backend/x86_64/isel.zig:664-667`

**Current Limitation:**
```zig
else => {
    ctx.diagnostics.reportError(zero_span, "unsupported index base for x86_64 lowering");
    return error.Unsupported;
}
```

**What's Missing:**
1. The `Index` instruction handling in `isel.zig` needs to support cases where the target is not a simple `Mem` operand
2. For array-of-structs with dynamic indexing and field access, the backend needs to:
   - Calculate the element address dynamically
   - Then apply the field offset to get the final address
3. The `for p in &points` iteration over references is also unimplemented

**Fix Required:**
Extend the `Index` and `Field` instruction lowering to handle:
- Dynamic array indexing with subsequent field access
- Reference iteration (`&points`)

**Difficulty:** Hard  
**Files to Modify:** `src/backend/x86_64/isel.zig`, potentially `src/mir/lower.zig`

---

### 3. functions_and_methods_test2.rs - Unresolved Associated Function

**Test File:**
```rust
struct Counter { value: i32 }

impl Counter {
    fn new() -> Counter { Counter { value: 0 } }
    fn inc(&mut self) { self.value += 1; }
    fn current(&self) -> i32 { self.value }
}

fn main() {
    let mut counter = Counter::new();
    counter.inc();
    counter.inc();
    println!("{}", counter.current());
}
```

**Expected Output:** `2`

**Error Message:**
```
error: unresolved path ending in `new`
    let mut counter = Counter::new();
                      ^^^^^^^^^^^^
```

**Root Cause:**  
The name resolution system in `src/hir/name_res.zig` does not resolve **associated functions** (static methods) called via path syntax like `Counter::new()`. It only recognizes:
1. Local variable names
2. Top-level function names in module scope

It does NOT look up functions defined in `impl` blocks when called via `Type::method()` syntax.

**Bug Location:** `src/hir/name_res.zig:324-333`

**Current Code:**
```zig
} else if (path.segments.len > 0) {
    const name = path.segments[path.segments.len - 1];
    if (module_symbols.get(name)) |def_id| {
        expr.kind = .{ .GlobalRef = def_id };
        return;
    }
    // ... error reporting
}
```

**What's Missing:**
1. During resolution, when encountering a path like `Counter::new`:
   - Look up `Counter` as a type/struct
   - Find the corresponding `impl Counter` block
   - Search for a method named `new` in that impl block
2. Build an index of impl methods per type during HIR construction

**Fix Required:**
1. In HIR lowering (`hir.zig`), build a map from type names to their impl methods
2. In name resolution (`name_res.zig`), when resolving multi-segment paths:
   - If the first segment is a type name, look for the method in that type's impl block
   - Mark the expression as an associated function call

**Difficulty:** Medium-Hard  
**Files to Modify:** `src/hir/hir.zig`, `src/hir/name_res.zig`

---

### 4. generics_test1.rs - Generic Type Inference Failure

**Test File:**
```rust
fn identity<T>(value: T) -> T { value }

struct Pair<T> { a: T, b: T }

fn main() {
    let numbers = Pair { a: 1, b: 2};
    let mirrored = identity(numbers);
    println!("{} {}", mirrored.a, mirrored.b);
}
```

**Expected Output:** `1 2`

**Error Messages:**
```
error: struct field type mismatch
    let numbers = Pair { a: 1, b: 2};
                  ^^^^^^^^^^^^^^^^^^
error: argument type does not match parameter
    let mirrored = identity(numbers);
                   ^^^^^^^^^^^^^^^^^
error: unresolved path type for field access
    println!("{} {}", mirrored.a, mirrored.b);
                      ^^^^^^^^^^
```

**Root Cause:**  
The type checker does not properly handle **generic type instantiation**:

1. **Struct initialization without explicit type parameters**: When writing `Pair { a: 1, b: 2 }`, the compiler doesn't infer `T = i64` from the field values
2. **Generic function instantiation**: The call `identity(numbers)` requires instantiating `identity<T>` with the concrete type, but type inference fails
3. **Cascading errors**: Because the type is unknown, field access on `mirrored.a` also fails

**Bug Locations:** 
- `src/hir/typecheck.zig` - Type inference for generic structs and functions
- `src/hir/hir.zig` - Generic type substitution

**What's Missing:**
1. **Generic struct instantiation**: Infer `T` from field initializers when `Pair { a: 1, b: 2 }` is used without `Pair::<i64> { ... }`
2. **Generic function type inference**: When calling `identity(x)`, infer `T` from argument type
3. **Type substitution**: Replace type parameters with concrete types during instantiation

**Fix Required:**
Implement basic generic type inference and substitution:
1. In type checking, when checking `StructInit` for a generic struct:
   - Infer type parameter from field types
   - Create a concrete instantiation of the generic type
2. In type checking, when checking `Call` to a generic function:
   - Infer type parameters from argument types
   - Substitute type parameters in return type

**Difficulty:** Hard  
**Files to Modify:** `src/hir/typecheck.zig`, `src/hir/hir.zig`

---

### 5. generics_test2.rs - Multiple Parser and Lexer Issues

**Test File:**
```rust
struct Wrapper<T> { value: T }

impl<T> Wrapper<T> {
    fn new(value: T) -> Wrapper<T> { Wrapper { value } }
    fn get(&self) -> &T { &self.value }
}

fn main() {
    let text = Wrapper::new(String::from("hi"));
    let number = Wrapper::new(42u64);
    println!("{} {}", text.get(), number.get());
}
```

**Expected Output:** `hi 42`

**Error Messages:**
```
error: expected ':' after field name
    fn new(value: T) -> Wrapper<T> { Wrapper { value } }
                                                     ^
error: expected ')' after call
    let number = Wrapper::new(42u64);
                                ^^^
```

**Root Causes:**

#### Issue A: Struct Field Shorthand Syntax Not Supported

The parser requires explicit `name: value` syntax for struct initialization but Rust allows `{ value }` as shorthand for `{ value: value }`.

**Bug Location:** `src/frontend/parser.zig:703-706`

**Current Code:**
```zig
const field_name_tok = self.expectConsume(.Identifier, "expected field name") orelse break;
_ = self.expectConsume(.Colon, "expected ':' after field name") orelse break;
```

**Grammar Change Required:**
```ebnf
Init = Ident , ":" , Expr 
     | Ident ;  (* shorthand: { value } means { value: value } *)
```

**Fix Required:**
Make the `:` optional when parsing struct init fields:
```zig
const field_name_tok = self.expectConsume(.Identifier, "expected field name") orelse break;
const value: ast.Expr = if (self.match(.Colon)) {
    self.parseExpr() orelse break
} else {
    // Shorthand: { field } means { field: field }
    ast.Expr{ .tag = .Path, .span = field_name_tok.span, .data = .{ .Path = ... } }
};
```

#### Issue B: Integer Literal Suffixes Not Supported

The lexer does not recognize integer literal suffixes like `42u64`, `10i32`, etc. It tokenizes `42` as an integer literal and then fails when encountering `u64`.

**Bug Location:** `src/frontend/lexer.zig:174-184`

**Current Code:**
```zig
'0'...'9' => {
    const start = i;
    var end = scanNumber(src, i);
    // ... handles float but not suffixes
    try appendSimple(&list, allocator, .IntLit, file_id, start, end, src);
    i = end;
}
```

**Grammar Change Required:**
```ebnf
IntLit = digit , { digit } , [ IntSuffix ] ;
IntSuffix = "u8" | "u16" | "u32" | "u64" | "usize"
          | "i8" | "i16" | "i32" | "i64" | "isize" ;
```

**Fix Required:**
After scanning digits, check for valid integer suffixes:
```zig
'0'...'9' => {
    const start = i;
    var end = scanNumber(src, i);
    const is_float = ...;
    if (is_float) {
        // handle float
    } else {
        // Check for integer suffix
        end = scanIntegerSuffix(src, end);
        try appendSimple(&list, allocator, .IntLit, file_id, start, end, src);
    }
    i = end;
}
```

**Difficulty:** Medium  
**Files to Modify:** `src/frontend/parser.zig`, `src/frontend/lexer.zig`, `grammar.ebnf`

---

### 6. dummy_lib.rs - Expected Failure (Library Crate)

**Test File:**
```rust
// dummy_lib.rs
// Exists only so Cargo has a target; not actually used.
fn _dummy() {}
```

**Expected Output:** (empty)

**Error Message:**
```
/usr/bin/ld: undefined reference to `main'
```

**Root Cause:**  
This file intentionally has no `main` function as it's meant to be a library. The test harness treats it as an executable and fails during linking.

**Fix Required:**  
Either:
1. Remove `dummy_lib.rs` from the test expected outputs, OR
2. Mark it as a library crate and skip execution, OR
3. Add a dummy `main` function for testing purposes

**Difficulty:** Very Easy  
**Files to Modify:** `codes/expected_outputs.txt` or `codes/dummy_lib.rs`

---

## Summary of Required Changes

### Bug Fixes

| Priority | Issue | File(s) | Effort |
|----------|-------|---------|--------|
| High | `idiv` memory operand size | `src/backend/x86_64/emitter.zig` | Easy |
| High | Associated function resolution | `src/hir/name_res.zig`, `src/hir/hir.zig` | Medium |
| High | Generic type inference | `src/hir/typecheck.zig`, `src/hir/hir.zig` | Hard |
| Medium | Complex array indexing with field access | `src/backend/x86_64/isel.zig` | Hard |

### Grammar Changes

```ebnf
(* Add struct field initialization shorthand *)
Init = Ident , [ ":" , Expr ] ;

(* Add integer literal suffixes *)
IntLit = digit , { digit } , [ IntSuffix ] ;
IntSuffix = "u8" | "u16" | "u32" | "u64" | "usize"
          | "i8" | "i16" | "i32" | "i64" | "isize" ;
```

### New Features to Implement

1. **Struct field shorthand syntax**: `{ value }` → `{ value: value }`
2. **Integer literal suffixes**: `42u64`, `10i32`, etc.
3. **Associated function resolution**: `Type::method()` syntax
4. **Basic generic type inference**: Infer type parameters from usage

---

## Recommended Fix Order

1. **Easy wins first:**
   - Fix `idiv` size qualifier (unblocks `expressions_test1.rs`)
   - Fix `dummy_lib.rs` expected output

2. **Parser/Lexer fixes:**
   - Add integer suffix support (unblocks parsing `42u64`)
   - Add struct field shorthand (unblocks `Wrapper { value }`)

3. **Semantic analysis:**
   - Implement associated function resolution (unblocks `Counter::new()`)
   - Improve generic type inference (unblocks `Pair { a: 1, b: 2 }`)

4. **Backend improvements:**
   - Handle complex array indexing patterns (unblocks `arrays3.rs`)

---

## Files Summary

| File | Changes Needed |
|------|---------------|
| `src/backend/x86_64/emitter.zig` | Fix `idiv` memory operand size qualifier |
| `src/backend/x86_64/isel.zig` | Handle complex indexing patterns |
| `src/frontend/lexer.zig` | Add integer suffix scanning |
| `src/frontend/parser.zig` | Add struct field shorthand parsing |
| `src/hir/name_res.zig` | Add associated function resolution |
| `src/hir/hir.zig` | Build impl method index, type substitution |
| `src/hir/typecheck.zig` | Implement generic type inference |
| `grammar.ebnf` | Document Init shorthand, IntSuffix |
| `codes/expected_outputs.txt` | Fix `dummy_lib.rs` expectation |
