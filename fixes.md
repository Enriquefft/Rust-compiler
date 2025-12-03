# Test Failures Analysis and Solutions

This document summarizes the failing tests identified by running `zig test src/main.zig`, their root causes, and proposed solutions.

## Summary

**Total tests run:** 115  
**Passed:** 105  
**Failed:** 10  

---

## Unit Test Failures

### 1. `frontend.parser.test.parse unsafe block as tail expression`

**File:** `src/frontend/parser.zig:1183`

**Error:** `TestUnexpectedResult`  
```
try std.testing.expect(fn_item.body.result != null);
```

**Description:**  
The test parses `fn main() -> i32 { unsafe { 42 } }` and expects that `fn_item.body.result` is not null (i.e., the `unsafe { 42 }` block should be parsed as a tail expression). However, the result is null.

**Root Cause:**  
The parser is not correctly treating `unsafe { ... }` blocks as tail expressions when they appear at the end of a function body. The `unsafe` block is likely being parsed as a statement rather than a tail expression that produces a value.

**Proposed Solution:**  
Investigate the parser's handling of `unsafe` blocks in the expression position. Ensure that when an `unsafe` block appears as the last item in a function body (without a semicolon), it is parsed as a tail expression (result) rather than a statement. This likely requires adjustments to `parsePrimary()` or `parseBlockContents()` in `src/frontend/parser.zig`.

---

### 2. `backend.x86_64.emitter.test.emitter saves callee-preserved registers`

**File:** `src/backend/x86_64/emitter.zig:618`

**Error:** `TestExpectedEqual`  
```
Expected:
    movsd qword ptr [rbp-24], xmm6
Found:
    movsd [rbp-24], xmm6
```

**Description:**  
The emitter test expects `movsd` instructions to include the `qword ptr` size specifier when using memory operands, but the generated assembly omits it.

**Root Cause:**  
In the `emitCalleeSavedSaves()` and `emitCalleeSavedRestores()` functions (lines 77-100), when emitting `movsd` instructions for XMM registers, the code calls `writeOperand()` for memory operands which in turn calls `writeMem()`. The `writeMem()` function only outputs the memory reference in `[rbp-offset]` format without the `qword ptr` prefix. However, the test's expected output includes `qword ptr`.

**Proposed Solution:**  
Either:
1. **Update the test expectation**: The `movsd` instruction in Intel syntax doesn't strictly require `qword ptr` since the XMM register size is unambiguous. Update the test expectation to match the actual (correct) output.
2. **Update the emitter**: Add `qword ptr` prefix to `movsd` instructions with memory operands for consistency with other move instructions. This would require modifying `emitCalleeSavedSaves()` and `emitCalleeSavedRestores()` to write `qword ptr` before the memory operand.

---

### 3. `hir.typecheck.test.typechecker handles composite expressions`

**File:** `src/hir/typecheck.zig:1431`

**Error:** `TestExpectedEqual`  
```
expected 2, found 5
```

**Description:**  
The typechecker test expects the struct initialization expression to have type ID 2 (the struct type), but instead gets type ID 5.

**Root Cause:**  
The test manually constructs a type `struct_ty` with `ensureType()` expecting it to be ID 2. However, when `checkExpr()` processes the `StructInit` expression, it generates a different type ID (5). This suggests that:
1. The struct type is being created differently during typechecking vs. test setup
2. Type IDs are not stable or the test's assumptions about type ID ordering are incorrect
3. The typechecker may be creating duplicate type entries for the struct

**Proposed Solution:**  
1. **Debug the type assignment**: Add logging or tracing to understand which types are being assigned which IDs during test execution.
2. **Compare types structurally**: Instead of comparing type IDs directly, consider comparing the actual type structures to verify correctness.
3. **Fix test setup**: Ensure the test correctly registers the struct definition before calling `ensureType()` so that the same type ID is used consistently.

---

### 4. `mir.lower.test.lower print macro preserves format string without newline`

**File:** `src/mir/lower.zig:2600`

**Error:** `TestUnexpectedResult`  
```
try std.testing.expect(mir_fn.blocks[0].insts.len >= 1);
```

**Description:**  
The test expects the lowered MIR function to have at least one instruction in its first block, but `blocks[0].insts.len` is 0.

**Root Cause:**  
The test constructs a HIR crate with a `main` function whose body is a `Call` expression to a `print` macro. When lowering the crate, the MIR function is not receiving any instructions. This could be because:
1. The test setup is incomplete (missing required HIR items or connections)
2. The lowering logic skips the first function (the `print` builtin at def_id 0) and processes `main` at def_id 1, but there's a mismatch
3. The `lowerFromHir()` function doesn't correctly lower the call expression into MIR instructions

**Proposed Solution:**  
1. Review the test setup to ensure all required HIR structures are properly initialized
2. Check the `lowerFromHir()` function to verify it correctly iterates over and lowers all functions
3. Ensure that `main` function's body (a `Call` expression) is properly recognized and lowered to MIR instructions

---

## End-to-End Test Failures

### 5. `e2e: arrays3.rs`

**Error:** `ExecutionFailed` (Segmentation fault)

**Description:**  
The compiled binary crashes with a segmentation fault when executed.

**Source Code:**
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

**Root Cause:**  
Analyzing the generated assembly reveals issues with:
1. **Struct field access through array indexing**: The code computes field offsets incorrectly. The expression `points[i].y += 1` uses offset calculations that appear to corrupt memory.
2. **The increment loop**: Line 80 shows `mov qword ptr [rax], rax` which stores a register value into the memory location it points to, causing corruption.
3. **For-each loop over references**: The iteration `for p in &points` may not correctly handle references to array elements.

**Proposed Solution:**  
1. Fix the generated code for compound assignment (`+=`) on struct fields accessed through array indexing
2. Review the backend code generation for field access expressions combined with array indexing
3. Ensure proper register usage to avoid self-referential stores

---

### 6. `e2e: expressions_test2.rs`

**Error:** `TestExpectedEqual`  
```
Expected: '65 55 25'
Actual:   '65 -9 25'
```

**Description:**  
The second value should be 55 (total - data[0] = 65 - 10 = 55), but the output shows -9.

**Source Code:**
```rust
fn main() {
    let mut data = [10, 20, 30];
    let mut ptr: *mut i32 = &mut data[1];
    unsafe { *ptr += 5; };  // data[1] = 25
    let mut total = 0;
    for i in 0..data.len() {
        let value_ref = &data[i];
        total += *value_ref;
    }
    // total = 10 + 25 + 30 = 65
    let mut accumulator = 1;
    accumulator *= total;  // accumulator = 65
    accumulator -= data[0];  // accumulator = 65 - 10 = 55
    println!("{} {} {}", total, accumulator, unsafe { *ptr });
}
```

**Root Cause:**  
Looking at the generated assembly (around lines 86-100), the compound operations are incorrectly ordered or use wrong operands:
```asm
mov rax, [rbp-288]  ; reads uninitialized memory
...
imul rax, rax       ; multiplies wrong value
```

The issue is that `accumulator *= total` and `accumulator -= data[0]` operations are incorrectly using operands from wrong memory locations.

**Proposed Solution:**  
1. Review the MIR lowering or backend code generation for compound assignment operators (`*=`, `-=`)
2. Ensure source and destination operands are correctly identified for compound operations
3. Fix register/memory allocation for these operations

---

### 7. `e2e: functions_and_methods_test2.rs`

**Error:** `TestExpectedEqual`  
```
Expected: '2'
Actual:   '1917596298' (garbage value, varies on each run)
```

**Description:**  
The counter should return 2 after two increments, but returns uninitialized/garbage value.

**Source Code:**
```rust
impl Counter {
    fn new() -> Counter { Counter { value: 0 } }
    fn inc(&mut self) { self.value += 1; }
    fn current(&self) -> i32 { self.value }
}
```

**Root Cause:**  
Looking at `Counter_current` in the generated assembly:
```asm
mov [rbp-32], rdi      ; store self pointer
mov rax, [rbp-32]      ; load self pointer
mov rcx, rax
add rcx, -32           ; WRONG: using negative offset from self pointer
mov rdx, [rcx]         ; reading from wrong location
```

The issue is that `self.value` access uses `add rcx, -32` which is incorrect. The field offset calculation for `self` references is wrong—it's treating `self` as a stack-relative address rather than a pointer to the struct.

**Proposed Solution:**  
1. Fix the field access code generation for `self` references in impl methods
2. When accessing fields through `self` (a pointer), the offset should be positive from the struct base, not relative to the stack frame
3. Review how method receivers are handled in the backend

---

### 8. `e2e: generics_test1.rs`

**Error:** `CompilationFailed`  
```
error: codes/generics_test1.rs:8:20-8:37: argument type does not match parameter
    let mirrored = identity(numbers);
                   ^^^^^^^^^^^^^^^^^
```

**Description:**  
Generic function `identity<T>` cannot accept a `Pair<i32>` struct.

**Root Cause:**  
The typechecker does not correctly instantiate generic functions when called with struct types as type arguments. The generic type parameter `T` is not being unified with `Pair { a: T, b: T }` correctly.

**Proposed Solution:**  
1. Review generic function instantiation in the typechecker
2. Ensure that struct types can be used as type arguments for generic parameters
3. Fix type unification for complex types (structs containing generics)

---

### 9. `e2e: generics_test2.rs`

**Error:** `CompilationFailed`  
```
error: argument type does not match parameter
    let text = Wrapper::new(String::from("hi"));
               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
error: argument type does not match parameter
    let number = Wrapper::new(42u64);
                 ^^^^^^^^^^^^^^^^^^^
```

**Description:**  
Generic impl methods `Wrapper<T>::new()` fail type checking with both `String` and `u64` arguments.

**Root Cause:**  
The typechecker does not correctly handle generic impl blocks. When calling `Wrapper::new(value)`, the type parameter `T` is not being inferred from the argument type or is not being properly propagated to the impl method signature.

**Proposed Solution:**  
1. Implement or fix type inference for generic impl methods
2. Ensure impl block type parameters (`impl<T> Wrapper<T>`) are correctly scoped and substituted when type-checking method calls
3. Review the path resolution for associated functions on generic types

---

### 10. `e2e: struct_init_shorthand_test2.rs`

**Error:** `TestExpectedEqual`  
```
Expected: '100 50 0 0'
Actual:   '13238272 1 100 50'
```

**Description:**  
Struct fields are printed in wrong order and with wrong values.

**Source Code:**
```rust
struct Rectangle { width: i32, height: i32, x: i32, y: i32 }

fn main() {
    let width = 100;
    let height = 50;
    let rect = Rectangle { width, height, x: 0, y: 0 };
    println!("{} {} {} {}", rect.width, rect.height, rect.x, rect.y);
}
```

**Root Cause:**  
Looking at the generated assembly, the `printf` arguments are loaded from wrong memory locations:
```asm
mov rax, [rbp-288]    ; reading uninitialized memory for rect.width
mov rcx, [rbp-320]    ; reading uninitialized memory for rect.height
mov rdx, [rbp-96]     ; this should be width but is height location
mov rsi, [rbp-128]    ; this should be height but is x location
```

The struct field initialization with shorthand syntax (`width`, `height` without `: value`) is placing values at incorrect offsets, and field access is reading from wrong locations.

**Proposed Solution:**  
1. Review struct initialization shorthand lowering to ensure fields are stored at correct offsets
2. Fix field access to use correct offsets when reading struct fields
3. Ensure struct layout is computed correctly and used consistently

---

## Summary of Root Causes

| Category | Count | Issues |
|----------|-------|--------|
| Parser | 1 | Unsafe block as tail expression |
| Backend/Emitter | 1 | Missing `qword ptr` in movsd |
| Typechecker | 2 | Struct type ID mismatch, generic instantiation |
| MIR Lowering | 1 | Empty instruction block for macro call |
| Code Generation | 5 | Struct field access, compound assignment, method self-reference |

## Priority Recommendations

1. **High Priority** - Code generation bugs (issues 5-7, 10) cause incorrect program behavior
2. **Medium Priority** - Generic type handling (issues 8-9) prevents valid programs from compiling  
3. **Low Priority** - Test mismatches (issues 1-4) may be test bugs rather than compiler bugs
