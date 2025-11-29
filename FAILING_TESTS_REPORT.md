# Failing Tests Analysis Report

**Generated:** 2025-11-29  
**Test Results:** 30 passed, 2 failed, 2 skipped

This report provides a comprehensive analysis of failing tests, their root causes, required fixes, and any grammar changes needed.

---

## Summary

Since the initial report (2025-11-28), significant progress has been made:

### ✅ **Fixed Issues** (now passing)
| Test File | Original Issue | Status |
|-----------|---------------|--------|
| `expressions_test1.rs` | `idiv` operand size bug | ✅ FIXED |
| `functions_and_methods_test2.rs` | Unresolved associated function | ✅ FIXED |
| `generics_test1.rs` | Generic struct ABI issues | ✅ FIXED |

### ⏭️ **Handled by Design**
| Test File | Reason | Status |
|-----------|--------|--------|
| `dummy_lib.rs` | Library crate (no main) | ⏭️ SKIPPED |
| `optimizations.rs` | No expected output defined | ⏭️ SKIPPED |

### ❌ **Still Failing**
| Test File | Failure Type | Root Cause | Severity |
|-----------|-------------|------------|----------|
| `arrays3.rs` | Output mismatch | Incorrect field offset in array element access | High |
| `generics_test2.rs` | Output mismatch | Generic method return type + format specifier issues | High |

---

## Detailed Analysis of Remaining Failures

### 1. arrays3.rs - Multi-field Struct Array Field Access Bug

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

The test now compiles and runs, but produces incorrect output. Analysis shows:
- The `x` field values are correct after `+= 1`: 1+1=2, -3+1=-2, 10+1=11 ✓
- The `y` field values are completely wrong: showing 0, 1, 2 instead of 3, 5, -4

The issue is in the **field access within `for p in &points`** loop:
1. The loop variable `p` is correctly a pointer to each Point element
2. When accessing `p.y`, the compiler reads from the wrong memory location
3. The `y` values (0, 1, 2) match the loop indices, suggesting the loop counter is being read instead of the field

**Bug Location:** `src/backend/x86_64/isel.zig` - Field access through reference/pointer

**Technical Details:**

Looking at the generated assembly in the print loop:
```asm
mov r11, [rbp-376]            # Load p.x correctly
mov [rbp-256], r11
mov r11, [rbp-288]            # This should load p.y...
mov [rbp-360], r11            # ...but it loads the loop index instead!
```

The issue is that the second field access (`p.y`) reads from `[rbp-288]` which is the loop index variable, not the offset within the Point struct. The field offset calculation for the `y` field (offset 32 from Point base, since each i32 is stored in 8-byte slots) is not being correctly combined with the dereferenced pointer address.

**Limitation:** The current x86-64 instruction selection only supports field offset 0 for pointer-based struct access (see `isel.zig` line 831: `const field_offset: i64 = 0`).

**Potential Solutions:**

1. **Track struct layout information** (Recommended):
   - Store field offset information in the MIR or pass it to isel
   - When accessing a field through a pointer, compute `pointer_value + field_offset`
   - Currently the code at line 834 handles non-zero offsets but `field_offset` is hardcoded to 0

2. **Implement proper field index tracking**:
   - The `Field` instruction has a `field_idx` that should be used to compute offset
   - Need to know the struct's type to calculate `field_idx * field_size`
   
3. **Quick fix** (Limited scope):
   - For 2-field structs with known layout, calculate offset as `field_idx * 8`
   - Add this calculation before the field access instruction

**Files to Modify:**
- `src/backend/x86_64/isel.zig:802-844` - Add field offset calculation for pointer-based access
- `src/mir/lower.zig` - Consider passing struct type info to field operations

**Difficulty:** Medium
**Estimated Fix:** 20-50 lines of code to properly track and apply field offsets

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

**Actual Output:** `0x7ffc3c6df280 0x7ffc3c6df260` (pointers instead of values)

**Root Cause Analysis:**

Analysis of the generated assembly reveals multiple independent issues:

**Generated Assembly (key sections):**
```asm
Wrapper_get:
    mov [rbp-32], rdi           # Store self pointer
    mov rax, [rbp-32]           # Load self pointer
    mov rbx, [rax]              # Dereference (loads value but unused!)
    mov rax, rax                # Returns self pointer, not &self.value!
    ret

main:
    mov rdi, 42                 # Integer value is correct!
    call Wrapper_new
    ...
    lea rdi, [rip + .Lstr1]     # Format string is "%p %p" (wrong!)
```

**Issues Identified:**

1. **`get()` returns wrong reference** (Most Critical):
   - The method `get(&self) -> &T` should return `&self.value` (pointer to the value field)
   - Currently it returns `&self` (pointer to the struct itself)
   - The line `mov rax, rax` shows the pointer is being returned unchanged
   - This is a field access issue similar to arrays3.rs

2. **Printf format specifier wrong** (Secondary Issue):
   - Format string is `"%p %p"` (pointer format for both arguments)
   - Should be `"%s %ld"` for String and u64 dereferenced values
   - The `printfSpecifier` function defaults to `%p` for `Pointer` types returned from generic methods
   - Even if the format were correct, the wrong values would be printed due to issue #1

**Bug Locations:**
- `src/backend/x86_64/isel.zig:802-844` - Field access returns wrong address
- `src/mir/lower.zig:1714-1777` - `printfSpecifier` function should look through generic returns

**Potential Solutions:**

1. **Fix field reference return** (Primary fix):
   ```asm
   # Current (wrong):
   mov rax, [rbp-32]    # rax = self
   mov rbx, [rax]       # load value (unused!)
   mov rax, rax         # return self (wrong!)
   
   # Correct:
   mov rax, [rbp-32]    # rax = self  
   add rax, 0           # rax = &self.value (field_offset = 0 for first field)
   ```
   For the `value` field in `Wrapper<T>`, the offset is 0, so `&self.value == self`.
   But the current code doesn't correctly handle the reference-to-field operation.

2. **Enhance printf format detection**:
   - When the argument type is `&T` from a generic method return:
     - Resolve `T` to the concrete type (String or u64)
     - Use the format for `&String` → `"%s"` or `&u64` → `"%ld"`
   - Add automatic dereferencing for reference types in println! handling

3. **Alternative approach - Auto-dereference in println!**:
   - Detect when println! receives a reference type
   - Generate code to dereference before passing to printf
   - Use the dereferenced type's format specifier

**Difficulty:** Medium-Hard

**Files to Modify:**
| File | Changes |
|------|---------|
| `src/backend/x86_64/isel.zig` | Fix reference return for field access |
| `src/mir/lower.zig` | Improve printf specifier for generic refs |
| `src/hir/typecheck.zig` | Better type resolution for generic returns |

**Estimated Fix:** 
- Issue 1 (field reference): ~30 lines
- Issue 2 (printf format): ~20 lines

---

## Grammar Status

The grammar is complete and correctly implemented. All language features needed for the test cases are supported at the parsing level.

### ✅ Struct Field Initialization Shorthand (Implemented)
```ebnf
Init = Ident , [ ":" , Expr ] ;
```
Verified: Parser at `parser.zig:703-723` handles shorthand syntax.

### ✅ Integer Literal Suffixes (Implemented)
```ebnf
IntLit = digit , { digit } , [ IntSuffix ] ;
IntSuffix = "u8" | "u16" | "u32" | "u64" | "usize"
          | "i8" | "i16" | "i32" | "i64" | "isize" ;
```
Verified: Lexer at `lexer.zig:182-184` correctly parses integer suffixes.

**Note:** Grammar and parsing are correct. The remaining issues are in the code generation (x86-64 backend) and type handling phases, not in parsing.

---

## Summary of Required Fixes

### High Priority

| Issue | Description | Files | Effort |
|-------|-------------|-------|--------|
| Field offset in pointer access | Second+ fields accessed incorrectly through pointers | `isel.zig` | Medium |
| Generic method return value | `&self.value` returns `&self` instead | `isel.zig` | Medium |
| Printf format for refs | `%p` used for reference types in println! | `lower.zig` | Easy |

### Root Cause Pattern

Both failing tests share a common underlying issue: **field access through pointers/references doesn't correctly compute field offsets**. The code at `isel.zig:831` hardcodes `field_offset = 0`, which works for single-field structs and first fields, but fails for:
- Second field (`y`) in `Point { x, y }`
- Field access via generic return type (`&self.value`)

---

## Recommended Fix Order

1. **Field offset calculation** (Fixes both tests partially)
   - In `isel.zig`, replace `const field_offset: i64 = 0;` with proper offset calculation
   - Use `field_idx * 8` for i64/pointer fields (simplest approach)
   - This would fix arrays3.rs and help generics_test2.rs

2. **Printf format specifier** (Easy win for generics_test2.rs)
   - In `printfSpecifier`, when type is `Pointer` pointing to a known type:
     - `Pointer -> String` → use `"%s"`
     - `Pointer -> PrimInt` → use the integer's format
   - This is a straightforward pattern match addition

3. **Generic method return** (May be fixed by #1)
   - If field offsets are handled correctly, `&self.value` should work
   - May need additional work if field types are different sizes

---

## Files Summary

| File | Priority | Changes Needed |
|------|----------|---------------|
| `src/backend/x86_64/isel.zig` | **High** | Fix field offset calculation for pointer-based access (~20 lines) |
| `src/mir/lower.zig` | Medium | Enhance `printfSpecifier` for pointer types (~10 lines) |

---

## Alternative Approaches

### 1. Passing Struct Type Info to Backend
Instead of hardcoding field offsets, pass struct type information through MIR:
- Add `struct_type_id` to Field instruction
- Look up field offsets from type registry
- **Pros:** Handles arbitrary struct layouts
- **Cons:** Requires changes across multiple compiler phases

### 2. Computing Offsets at MIR Level
Generate explicit pointer arithmetic in MIR instead of Field instructions:
- Lower `p.y` to `*(p + 8)` in MIR
- Backend only sees simple pointer operations
- **Pros:** Simpler backend
- **Cons:** More complex MIR lowering

### 3. Quick Fix: Assume 8-byte Fields
For the current test cases, assume all fields are 8 bytes:
```zig
const field_offset: i64 = @intCast(payload.field_idx * 8);
```
- **Pros:** Minimal code change, fixes current tests
- **Cons:** Won't work for mixed-size fields

---

## Test Status History

| Date | Passed | Failed | Skipped | Notes |
|------|--------|--------|---------|-------|
| 2025-11-28 | 27 | 6 | 0 | Initial run |
| 2025-11-29 (morning) | 29 | 3 | 1 | +2 fixed, +1 skip |
| 2025-11-29 (current) | 30 | 2 | 2 | generics_test1.rs now passes! |

**Progress:** +3 tests fixed since 2025-11-28, +1 additional skip identified

---

## Conclusion

The remaining 2 failures (`arrays3.rs` and `generics_test2.rs`) share a common root cause: **incorrect field offset handling for pointer/reference-based struct access**. A targeted fix in `src/backend/x86_64/isel.zig` (approximately 20-30 lines of code) should resolve both issues.

The compiler's parsing, type checking, and basic code generation are working correctly. The issue is localized to the x86-64 instruction selection phase's handling of multi-field struct access through indirection.
