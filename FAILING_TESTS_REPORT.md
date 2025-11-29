# Failing Tests Analysis Report

**Generated:** 2025-11-29  
**Test Results:** 29 passed, 3 failed, 1 skipped

This report provides a comprehensive analysis of failing tests, their root causes, required fixes, and any grammar changes needed.

---

## Summary

Since the initial report (2025-11-28), significant progress has been made:

### ✅ **Fixed Issues** (now passing)
| Test File | Original Issue | Status |
|-----------|---------------|--------|
| `expressions_test1.rs` | `idiv` operand size bug | ✅ FIXED |
| `functions_and_methods_test2.rs` | Unresolved associated function | ✅ FIXED |

### ⏭️ **Handled by Design**
| Test File | Reason | Status |
|-----------|--------|--------|
| `dummy_lib.rs` | Library crate (no main) | ⏭️ SKIPPED |

### ❌ **Still Failing**
| Test File | Failure Type | Root Cause | Severity |
|-----------|-------------|------------|----------|
| `arrays3.rs` | Compilation failed | Unsupported index base for x86_64 lowering | High |
| `generics_test1.rs` | Output mismatch | Generic struct ABI and field access issues | High |
| `generics_test2.rs` | Output mismatch | Generic method return type + format specifier issues | High |

---

## Detailed Analysis of Remaining Failures

### 1. arrays3.rs - Complex Array Indexing with Field Access

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

**Root Cause Analysis:**

The x86-64 instruction selection code (`isel.zig`) cannot handle compound place expressions like `points[i].x`. When indexing into an array element and then accessing a field, the backend encounters an unsupported operand combination.

**Bug Location:** `src/backend/x86_64/isel.zig:703-706`

**What's Missing:**
1. **Dynamic array indexing with field access**: When the index is a runtime variable (`i`), the backend needs to:
   - Load the loop index value
   - Calculate `base_address + (index * element_size) + field_offset`
   - Use this computed address for the load/store

2. **Reference iteration** (`for p in &points`): Iterating over references to array elements is unimplemented

**Technical Details:**
The `Index` instruction handling at line 703 falls into the `else` branch because the base operand after dynamic indexing doesn't produce a simple `Mem` operand that subsequent `Field` operations can work with.

**Fix Required:**

In `src/backend/x86_64/isel.zig`, extend the `Index` instruction handling to:
1. When the index is dynamic (VReg), compute element address in a temporary register
2. Return this register as the base for subsequent field access
3. Handle the case where `Field` follows an `Index` by combining the offsets

Example approach:
```zig
// For dynamic indexing:
// 1. Load base array address into temp register
// 2. Multiply index by element size
// 3. Add to get element address
// 4. For subsequent field access, add field offset
```

For `for p in &points`, the lowering needs to:
1. Create an iterator that yields pointers to array elements
2. Track the current element pointer in a loop variable

**Difficulty:** Hard  
**Files to Modify:** `src/backend/x86_64/isel.zig`, potentially `src/mir/lower.zig`

---

### 2. generics_test1.rs - Generic Struct ABI and Field Access

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

**Actual Output:** `1 140726689479992` (second field is garbage)

**Root Cause Analysis:**

The test now compiles and runs, but produces incorrect output. Analysis of the generated assembly shows:

**Generated Assembly (key sections):**
```asm
identity:
    mov [rbp-32], rdi       # Only stores first argument
    mov rax, [rbp-32]       # Returns only rax
    ret

main:
    mov qword ptr [rbp-64], 1   # Store a = 1
    mov qword ptr [rbp-96], 2   # Store b = 2
    mov rdi, [rbp-64]           # Pass only 'a' field
    mov rsi, [rbp-96]           # Pass 'b' in rsi (but identity ignores it!)
    call identity
    mov [rbp-192], rax          # Store return value (only 'a')
    mov [rbp-200], rdx          # Store rdx as 'b' (but rdx was never set!)
```

**Issues Identified:**

1. **Struct passing ABI violation**: The `Pair` struct (16 bytes, 2 x i64) should be passed as two separate registers OR on the stack. Currently:
   - Caller passes `a` in `rdi` and `b` in `rsi`
   - Callee (`identity`) only reads `rdi` and stores it, ignoring `rsi`

2. **Struct return ABI violation**: According to System V AMD64 ABI:
   - Structs ≤16 bytes with two INTEGER fields should return in `rax` + `rdx`
   - Currently `identity` only returns `rax`, leaving `rdx` uninitialized

3. **Generic type size handling**: The generic function `identity<T>` doesn't know the actual size of `T` at codegen time and defaults to single-value handling

**Bug Locations:**
- `src/mir/lower.zig:76-145` - Parameter expansion for generic functions
- `src/backend/x86_64/isel.zig` - Return value handling for multi-register types
- `src/hir/typecheck.zig` - Generic type instantiation

**Fix Required:**

1. **Generic monomorphization**: When instantiating `identity<Pair<i64>>`, substitute the concrete type and generate code aware of the struct's size
   
2. **Multi-field struct ABI**: Ensure all struct fields are:
   - Passed in appropriate registers/stack slots
   - Returned in `rax` + `rdx` (for 2-field structs)

3. **Type-aware codegen for generics**: The lowering phase needs to know the concrete type `T = Pair<i64>` to generate correct ABI

**Difficulty:** Hard  
**Files to Modify:** 
- `src/hir/typecheck.zig` - Generic instantiation
- `src/mir/lower.zig` - Generic function lowering
- `src/backend/x86_64/isel.zig` - Multi-register return handling

---

### 3. generics_test2.rs - Generic Method Return Types and Format Specifiers

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

**Actual Output:** `0x7ffd12ec3cd0 0x7ffd12ec3cb0` (pointers instead of values)

**Root Cause Analysis:**

Analysis of the generated assembly reveals multiple issues:

**Generated Assembly (key sections):**
```asm
Wrapper_get:
    mov rax, [rbp-32]       # Load 'self' pointer
    mov rbx, [rax]          # Dereference to get value (but result ignored!)
    mov rax, rax            # Returns pointer, not dereferenced value
    ret

main:
    mov rdi, 0              # 42u64 becomes 0 (integer suffix not used!)
    call Wrapper_new
    ...
    lea rdi, [rip + .Lstr1] # Format string is "%p %p" (pointer format!)
```

**Issues Identified:**

1. **`get()` returns pointer instead of value**:
   - The method `get(&self) -> &T` should return a reference to `self.value`
   - Currently it returns `&self` (the struct pointer) instead of `&self.value`
   - The `mov rbx, [rax]` loads the value but then `mov rax, rax` overwrites it with the pointer

2. **Integer suffix not propagated**:
   - `42u64` is parsed correctly (lexer has suffix support)
   - But the literal value `0` is passed instead of `42`
   - The suffix type information may be lost during lowering

3. **Printf format specifier wrong**:
   - Format string is `"%p %p"` (pointer format)
   - Should be `"%s %llu"` for String and u64 (unsigned)
   - The `printfSpecifier` function at line 1636 defaults to `%p` for `Pointer` type
   - For `&T` where `T` is String or integer, it should dereference and use the inner type's format

**Bug Locations:**
- `src/mir/lower.zig:1591-1641` - `printfSpecifier` function doesn't handle generic refs properly
- `src/backend/x86_64/isel.zig` - Return value handling for `&T` types
- `src/frontend/parser.zig` or `src/hir/hir.zig` - Integer suffix value preservation

**Fix Required:**

1. **Fix `get()` return value**: The field access `&self.value` should:
   - Compute `&self + field_offset` 
   - Return this address (reference to the field)
   - Currently returning `&self` instead

2. **Integer suffix propagation**: When parsing `42u64`:
   - The lexer correctly scans "42u64" as one token
   - Ensure the parser extracts the value `42` and type `u64`
   - Propagate type info to HIR literal expression

3. **Printf format for generic references**:
   ```zig
   // In printfSpecifier, for Ref types:
   .Ref => |ref_info| {
       // Recursively get format for inner type
       const inner_format = getFormatForType(ref_info.inner);
       // For &String -> "%s", for &u64 -> "%llu"
       return inner_format;
   }
   ```

**Difficulty:** Medium-Hard  
**Files to Modify:**
- `src/mir/lower.zig` - Printf specifier logic
- `src/backend/x86_64/isel.zig` - Reference return handling
- `src/hir/hir.zig` or `src/frontend/parser.zig` - Integer suffix handling

---

## Grammar Status

The grammar has already been updated with the required changes:

### ✅ Struct Field Initialization Shorthand (Already Implemented)
```ebnf
(* Current grammar allows shorthand *)
Init = Ident , [ ":" , Expr ] ;
```
Verified: Parser at `parser.zig:703-723` handles shorthand syntax.

### ✅ Integer Literal Suffixes (Already Implemented)
```ebnf
IntLit = digit , { digit } , [ IntSuffix ] ;
IntSuffix = "u8" | "u16" | "u32" | "u64" | "usize"
          | "i8" | "i16" | "i32" | "i64" | "isize" ;
```
Verified: Lexer at `lexer.zig:182-184` calls `scanIntegerSuffix`.

**Note:** The grammar changes are implemented in the lexer/parser, but the type information from suffixes may not be properly propagated to later phases.

---

## Summary of Required Fixes

### High Priority

| Issue | Description | Files | Effort |
|-------|-------------|-------|--------|
| Generic struct ABI | Multi-field structs not correctly passed/returned | `isel.zig`, `lower.zig` | Hard |
| Generic method returns | `&T` field access returns wrong pointer | `isel.zig` | Medium |
| Printf for generic refs | `%p` used instead of inner type format | `lower.zig` | Easy |
| Integer suffix value | Value from suffixed literals may be lost | `parser.zig`, `hir.zig` | Medium |

### Medium Priority

| Issue | Description | Files | Effort |
|-------|-------------|-------|--------|
| Dynamic array indexing | `array[i].field` pattern unsupported | `isel.zig` | Hard |
| Reference iteration | `for p in &array` unsupported | `lower.zig` | Medium |

---

## Recommended Fix Order

1. **Printf format specifier** (Easy win)
   - Fix `printfSpecifier` to handle `&T` types by looking at inner type
   - Partial fix for `generics_test2.rs`

2. **Integer suffix propagation** (Medium)
   - Ensure `42u64` stores value `42` with type `u64`
   - Helps `generics_test2.rs`

3. **Generic method return (`&T`)** (Medium)
   - Fix field reference access to return correct address
   - Fixes `generics_test2.rs`

4. **Generic struct ABI** (Hard)
   - Implement proper multi-register passing/return for structs in generic contexts
   - Fixes `generics_test1.rs`

5. **Dynamic array indexing** (Hard)
   - Implement runtime index computation for array-of-structs
   - Fixes `arrays3.rs`

---

## Files Summary

| File | Changes Needed |
|------|---------------|
| `src/mir/lower.zig` | Fix printf specifier for generic refs; improve generic lowering |
| `src/backend/x86_64/isel.zig` | Handle multi-register returns; fix &T field access; support dynamic array indexing |
| `src/hir/typecheck.zig` | Generic type instantiation improvements |
| `src/frontend/parser.zig` | Ensure integer suffix type info is preserved |

---

## Test Status History

| Date | Passed | Failed | Skipped |
|------|--------|--------|---------|
| 2025-11-28 | 27 | 6 | 0 |
| 2025-11-29 | 29 | 3 | 1 |

Progress: +2 tests fixed, +1 properly skipped
