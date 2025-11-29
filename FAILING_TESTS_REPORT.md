# Failing Tests Analysis Report

**Generated:** 2025-11-29  
**Test Results:** 30 passed, 3 failed, 1 skipped

This report provides a comprehensive analysis of failing tests, their root causes, required fixes, and any grammar changes needed.

---

## Summary

Since the initial report (2025-11-28), significant progress has been made:

### ✅ **Fixed Issues** (now passing)
| Test File | Original Issue | Status |
|-----------|---------------|--------|
| `expressions_test1.rs` | `idiv` operand size bug | ✅ FIXED |
| `functions_and_methods_test2.rs` | Unresolved associated function | ✅ FIXED |
| `generics_test1.rs` | Generic struct ABI / field access issues | ✅ FIXED |

### ⏭️ **Handled by Design**
| Test File | Reason | Status |
|-----------|--------|--------|
| `dummy_lib.rs` | Library crate (no main) | ⏭️ SKIPPED |

### ❌ **Still Failing**
| Test File | Failure Type | Root Cause | Severity |
|-----------|-------------|------------|----------|
| `arrays3.rs` | Output mismatch | Field access within array iteration produces wrong y-values | High |
| `generics_test2.rs` | Output mismatch | Generic method returns pointer instead of dereferenced value + printf format specifier issues | High |
| `optimizations.rs` | Compilation hangs | Missing `const` item support and `loop` construct in grammar/parser | High |

---

## Detailed Analysis of Remaining Failures

### 1. arrays3.rs - Field Access Bug in Array Iteration

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

**Actual Output:**
```
(2, 0)
(-2, 1)
(11, 2)
```

**Root Cause Analysis:**

The test now compiles and runs, but produces incorrect y-values. The x-values are correct (`2, -2, 11` = original + 1), but y-values are `0, 1, 2` instead of `3, 5, -4`.

**Observed Pattern:**
- The y-values `0, 1, 2` correspond to the loop index `i` rather than the actual field values
- This suggests the loop counter is being printed or stored instead of `points[i].y`

**Root Cause Hypothesis:**
The issue appears to be in how compound place expressions `points[i].y` are lowered. The x-field access works correctly, but y-field access (second field) may be reading from the wrong memory location or using the index value instead.

**Bug Location:** `src/backend/x86_64/isel.zig` or `src/mir/lower.zig`

**Potential Issues:**
1. **Field offset calculation**: For the second field `y`, the offset should be `sizeof(i32) = 4 bytes` from the element base, but may be incorrectly computed
2. **Register allocation conflict**: The loop index register may be clobbering the field access result
3. **Memory addressing mode**: The compound address `base + (index * element_size) + field_offset` may have incorrect field offset for non-first fields

**Limitations:**
- Complex place expressions like `array[dynamic_index].field` require multi-step address calculation
- The current x86-64 backend may not correctly combine dynamic indexing with field offsets

**Potential Solutions:**

| Approach | Description | Complexity |
|----------|-------------|------------|
| **Separate address computation** | Compute the element address first, then add field offset in a second step | Medium |
| **Register-based addressing** | Use LEA instruction to compute `base + index*size`, then add field offset | Medium |
| **MIR-level decomposition** | Split `array[i].field` into `let elem = &array[i]; elem.field` during lowering | Hard |

**Difficulty:** Medium  
**Files to Modify:** `src/backend/x86_64/isel.zig`

---

### 2. generics_test2.rs - Generic Method Return Types and Format Specifiers

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

**Actual Output:** `0x7fffed5cef80 0x7fffed5cef60` (pointers instead of values)

**Root Cause Analysis:**

The `get()` method returns pointers (memory addresses) instead of the dereferenced values. The `println!` macro is printing the raw pointer values using the `%p` format specifier.

**Issues Identified:**

1. **`get()` returns pointer instead of value**:
   - The method `get(&self) -> &T` should return a reference to `self.value`
   - Currently it returns `&self` (the struct pointer) instead of `&self.value`
   - The field offset for `value` is not being applied

2. **Printf format specifier wrong**:
   - Format string uses `"%p %p"` (pointer format)
   - Should be `"%s %llu"` for String and u64 (unsigned)
   - The `printfSpecifier` function defaults to `%p` for `Pointer`/`Ref` types
   - For `&T` where `T` is String or integer, it should use the inner type's format

**Limitations:**

1. **Generic reference return types**: The compiler doesn't properly handle returning references to generic type fields
2. **Printf format resolution**: The format specifier logic doesn't recursively unwrap reference types to determine the correct format
3. **Generic method codegen**: Field access through generic self parameters may not compute correct offsets

**Potential Solutions:**

| Approach | Description | Complexity | Impact |
|----------|-------------|------------|--------|
| **Fix field offset in get()** | Ensure `&self.value` computes `self_ptr + field_offset` | Medium | Fixes pointer value |
| **Recursive format specifier** | Modify `printfSpecifier` to unwrap `&T` and use inner type's format | Easy | Fixes output format |
| **Dereference in println!** | Auto-dereference `&T` values when printing | Medium | Better UX |
| **Monomorphize generics** | Generate specialized code for each `Wrapper<T>` instantiation | Hard | Complete fix |

**Bug Locations:**
- `src/mir/lower.zig:1591-1641` - `printfSpecifier` function doesn't handle generic refs properly
- `src/backend/x86_64/isel.zig` - Return value handling for `&T` types

**Recommended Fix Order:**
1. Fix `printfSpecifier` to recursively unwrap `&T` types (easy win)
2. Fix field offset calculation for `&self.value` (medium)
3. Consider monomorphization for complete generic support (long-term)

**Difficulty:** Medium-Hard  
**Files to Modify:**
- `src/mir/lower.zig` - Printf specifier logic
- `src/backend/x86_64/isel.zig` - Reference return handling

---

### 3. optimizations.rs - Missing Grammar Constructs

**Test File:**
```rust
const USE_FAST_PATH: bool = true;
const SCALE_A: i64 = 2 * 2 * 2;      // 8  → constant folding
const SCALE_B: i64 = 10 - 3;         // 7  → constant folding

fn slow_path(x: i64) -> i64 {
    let mut acc = 0;
    let mut i = 0;
    while i < 100 {
        acc += (x * i) % 7;
        i += 1;
    }
    acc
}

fn fast_path(x: i64) -> i64 {
    let unused_product = x * 12345; // DCE: result not used
    if x == 0 { return 0; }
    let doubled = x + x;
    let triple = doubled * 3;
    let useless = triple - triple;  // always 0
    let final_val = triple + useless;
    final_val
}

fn choose_path(x: i64) -> i64 {
    if USE_FAST_PATH {
        let scaled = x * SCALE_A + SCALE_B;
        fast_path(scaled)
    } else {
        slow_path(x)
    }
}

fn main() {
    let data = [1, 2, 3, 4, 5, 6, 7, 8];
    let mut total: i64 = 0;
    for i in 0..data.len() {
        let v = data[i] as i64;
        let score = choose_path(v);
        total += score;
    }
    if false {
        println!("Debug total: {}", total);
    }
    let debug_only = total * 9999;   // DCE
    println!("Result: {}", total % 1000);
}
```

**Expected Output:** `Result: 64`

**Actual Behavior:** Compilation hangs (timeout after 30 seconds)

**Root Cause Analysis:**

The compiler hangs during parsing/compilation due to missing language constructs in the grammar and parser. According to `grammar_update.md`, the following features are needed:

**Missing Language Features:**

1. **`const` items** (Critical):
   ```rust
   const USE_FAST_PATH: bool = true;
   const SCALE_A: i64 = 2 * 2 * 2;
   ```
   - The grammar (`grammar.ebnf`) does not include `ConstItem`
   - The parser doesn't recognize `const` as an item-level keyword
   - This likely causes the parser to enter an infinite loop or hang

2. **`loop { ... }` construct** (Not used in this test but mentioned in grammar_update.md):
   - The `loop` keyword for infinite loops is not in the grammar
   - Would need: `LoopExpr = "loop" , Block ;`

**Grammar Changes Required:**

```ebnf
(* Add to Keyword list *)
Keyword = ... | "const" | "loop" ;

(* Add ConstItem to Item *)
Item = FnItem | StructItem | ImplItem | TypeAliasItem | ConstItem | ";" ;

(* Define ConstItem *)
ConstItem = "const" , Ident , ":" , Type , "=" , Expr , ";" ;

(* Optional: Add loop expression *)
LoopExpr = "loop" , Block ;
```

**Parser Changes Required:**

In `src/frontend/parser.zig`:
1. Add `const` to the keyword/token enum
2. Implement `parseConstItem()` function similar to `parseTypeAliasItem()`
3. Add `const` case to the item parsing switch

**Limitations:**

| Feature | Status | Impact |
|---------|--------|--------|
| `const` items | **Missing** | Blocks all code using module-level constants |
| `loop` construct | Missing | Blocks infinite loop patterns |
| Constant folding | N/A | Would benefit optimization passes once const items work |

**Potential Solutions:**

| Approach | Description | Complexity | Recommendation |
|----------|-------------|------------|----------------|
| **Add const item support** | Extend grammar and parser for `const` declarations | Medium | **Required** |
| **Treat const as let** | Parse `const` like `let` (ignoring immutability) | Easy | Workaround only |
| **Inline constants manually** | Rewrite test to use literals instead of constants | Easy | Not a real fix |
| **Add loop support** | Extend grammar for `loop { }` construct | Easy | Future improvement |

**Why the Compiler Hangs:**

When the parser encounters `const USE_FAST_PATH`, it doesn't recognize `const` as a valid item start token. Depending on error recovery logic, this may cause:
1. Infinite retry loop trying to parse an expression or statement
2. Token consumption getting stuck on the unrecognized keyword
3. Stack overflow from recursive descent without progress

**Difficulty:** Medium  
**Files to Modify:**
- `grammar.ebnf` - Add ConstItem production
- `src/frontend/tokens.zig` - Add `const` and `loop` keywords
- `src/frontend/parser.zig` - Implement const item parsing
- `src/frontend/lexer.zig` - Recognize new keywords

---

## Grammar Status

### ✅ Already Implemented

| Feature | Grammar | Implementation |
|---------|---------|----------------|
| Struct field shorthand | `Init = Ident , [ ":" , Expr ] ;` | `parser.zig:703-723` |
| Integer literal suffixes | `IntLit = digit , { digit } , [ IntSuffix ] ;` | `lexer.zig:182-184` |
| Modulo operator (`%`) | `Mul = Cast, { ("*" \| "/" \| "%") , Cast } ;` | Implemented |
| Macro calls | `MacroCall = Path , "!" , Call ;` | Implemented |
| Primitive types | `bool`, `i64`, etc. | Implemented |

**Note:** Integer suffix type information may not be properly propagated to later compiler phases.

### ❌ Missing (Required for `optimizations.rs`)

| Feature | Proposed Grammar | Status |
|---------|------------------|--------|
| `const` items | `ConstItem = "const" , Ident , ":" , Type , "=" , Expr , ";" ;` | **Missing** |
| `loop` construct | `LoopExpr = "loop" , Block ;` | Missing |

---

## Summary of Required Fixes

### Critical Priority (Blocking)

| Issue | Description | Files | Effort | Blocks |
|-------|-------------|-------|--------|--------|
| **const items** | Parser doesn't recognize `const` keyword | `src/frontend/tokens.zig`, `src/frontend/parser.zig`, `grammar.ebnf` | Medium | `optimizations.rs` |

### High Priority

| Issue | Description | Files | Effort | Blocks |
|-------|-------------|-------|--------|--------|
| Array field offset bug | Second field (`y`) returns loop index instead of value | `src/backend/x86_64/isel.zig` | Medium | `arrays3.rs` |
| Generic method returns | `&T` field access returns struct pointer instead of field pointer | `src/backend/x86_64/isel.zig` | Medium | `generics_test2.rs` |
| Printf for generic refs | `%p` format used instead of inner type format | `src/mir/lower.zig` | Easy | `generics_test2.rs` |

### Medium Priority (Future)

| Issue | Description | Files | Effort |
|-------|-------------|-------|--------|
| `loop` construct | Infinite loop syntax not supported | `src/frontend/tokens.zig`, `src/frontend/parser.zig` | Easy |
| Reference iteration | `for p in &array` unsupported | `src/mir/lower.zig` | Medium |

---

## Recommended Fix Order

1. **Add `const` item support** (Medium - unblocks `optimizations.rs`)
   - Add `const` keyword to tokens
   - Implement `parseConstItem()` in parser
   - Update grammar.ebnf

2. **Printf format specifier** (Easy win)
   - Fix `printfSpecifier` to recursively unwrap `&T` types
   - Partial fix for `generics_test2.rs`

3. **Array field offset calculation** (Medium)
   - Fix second field access in `array[i].field` pattern
   - Fixes `arrays3.rs`

4. **Generic method return (`&T`)** (Medium)
   - Fix field reference access to return `&self.value` not `&self`
   - Fixes `generics_test2.rs`

5. **Add `loop` support** (Easy - future improvement)
   - Extend grammar for `loop { }` construct

---

## Files Summary

| File | Changes Needed |
|------|---------------|
| `grammar.ebnf` | Add `ConstItem` and optionally `LoopExpr` |
| `src/frontend/tokens.zig` | Add `const` and `loop` keywords |
| `src/frontend/parser.zig` | Implement const item parsing |
| `src/mir/lower.zig` | Fix printf specifier for generic refs |
| `src/backend/x86_64/isel.zig` | Fix array field offset; fix &T field access |

---

## Test Status History

| Date | Passed | Failed | Skipped | Notes |
|------|--------|--------|---------|-------|
| 2025-11-28 | 27 | 6 | 0 | Initial report |
| 2025-11-29 (AM) | 29 | 3 | 1 | Fixed `expressions_test1.rs`, `functions_and_methods_test2.rs` |
| 2025-11-29 (Current) | 30 | 3 | 1 | Fixed `generics_test1.rs` |

**Current Status:** 30 passed, 3 failed, 1 skipped

**Remaining Failures:**
- `arrays3.rs` - Output mismatch (field offset bug)
- `generics_test2.rs` - Output mismatch (pointer instead of value)
- `optimizations.rs` - Compilation hangs (missing `const` support)
