# Codebase Analysis: Bandaid Solutions, Temporary Logic, Placeholders, and Scalability Issues

This document provides a comprehensive analysis of the Rust-subset compiler codebase, identifying areas that may need attention for long-term maintainability and scalability.

## Table of Contents

1. [Hardcoded Constants and Magic Numbers](#hardcoded-constants-and-magic-numbers)
2. [Bandaid and Temporary Solutions](#bandaid-and-temporary-solutions)
3. [Placeholders and Incomplete Implementations](#placeholders-and-incomplete-implementations)
4. [Logic That Doesn't Scale](#logic-that-doesnt-scale)
5. [Architecture Concerns](#architecture-concerns)
6. [Recommendations Summary](#recommendations-summary)
7. [Current Failing Tests](#current-failing-tests)

---

## Hardcoded Constants and Magic Numbers

### 1. Backend: Struct Field Limits (`src/shared_consts.zig`)

**Status**: ✅ FIXED - Constants centralized and limits increased

**Location**: `src/shared_consts.zig`

```zig
pub const MAX_STRUCT_FIELDS: u32 = 16;
pub const ASSUMED_GENERIC_STRUCT_FIELDS: u32 = 4;
pub const LOCAL_STACK_MULTIPLIER: u32 = 4;
pub const MAX_EXTRA_ARRAY_ELEMENTS: usize = 7;
pub const STRUCT_SECOND_FIELD_OFFSET: i64 = -@as(i64, @intCast(@sizeOf(i64) * LOCAL_STACK_MULTIPLIER));
```

**Previous Issues**:
- `MAX_STRUCT_FIELDS: u32 = 4` - Limited all structs to 4 fields maximum.
- `ASSUMED_STRUCT_FIELDS: u32 = 2` - Assumed all generic type parameters are 2-field structs.
- Constants were duplicated between `isel.zig` and `lower.zig`.

**Resolution**:
- Centralized all shared constants into `src/shared_consts.zig`
- Increased `MAX_STRUCT_FIELDS` from 4 to 16 (4x improvement)
- Increased `ASSUMED_GENERIC_STRUCT_FIELDS` from 2 to 4 (2x improvement)
- Both `src/mir/lower.zig` and `src/backend/x86_64/isel.zig` now import from the shared module

**Remaining Impact**: Still fails silently for structs larger than 16 fields; no error reporting.

---

### ~~2. MIR Lower: Duplicated Hardcoded Constants (`src/mir/lower.zig`)~~

**Status**: ✅ FIXED - Merged into shared module

This issue has been resolved by centralizing constants in `src/shared_consts.zig`.

---

### ~~2. Register Allocator: Fixed Register Set (`src/backend/x86_64/regalloc.zig`)~~

**Status**: ✅ FIXED - Allocator now uses caller- and callee-preserved GPRs/XMMs and saves them in the prologue/epilogue.

The register allocator now distinguishes integer versus floating-point register classes, allocates XMM registers, and records callee-saved usage so the emitter can spill and restore them automatically. Scratch register selection no longer reserves `r11` globally.

---

### ~~3. Parser: Debug Print Statements (`src/frontend/parser.zig`)~~

**Status**: ✅ FIXED - Debug prints removed

The debug print statements have been removed from the parser.

---

### 5. Name Resolution: Fixed Buffer Size (`src/hir/name_res.zig`)

**Location**: Lines 347-348

```zig
var mangled_buf: [256]u8 = undefined;
const mangled_name = std.fmt.bufPrint(&mangled_buf, "{s}_{s}", .{ type_name, method_name }) catch {
    diagnostics.reportError(expr.span, "method name too long");
    return;
};
```

**Issues**:
- Method names limited to 256 characters total
- Silently fails with a generic error for long names

**Recommendation**: Use dynamic allocation for mangled names.

---

## Bandaid and Temporary Solutions

### ~~1. Generic Type Parameter Handling (`src/mir/lower.zig`)~~

**Status**: ✅ FIXED - Limits removed and monomorphization threaded into lowering

Generic parameters are now tracked through type resolution, creating monomorphized struct types from concrete arguments. MIR lowering uses real struct field layouts instead of single-letter heuristics or fixed field assumptions.

---

### 2. Type Compatibility Workarounds (`src/hir/typecheck.zig`)

**Location**: Lines 700-768

```zig
// Check if rhs is a type parameter (single-segment path that doesn't resolve to a known type)
// If so, it's compatible with any type (for generic type inference)
if (rhs_kind == .Path) {
    const rhs_path = rhs_kind.Path;
    if (rhs_path.segments.len == 1 and !isKnownTypeName(crate, rhs_path.segments[0])) {
        return true;
    }
}
// ...
// If the path is a single-segment path that doesn't resolve to a known type,
// treat it as a generic type parameter that's compatible with any type.
// This enables basic generic type inference for structs like Pair<T>.
```

**Issues**:
- Any unknown single-segment type path is treated as a type parameter
- Could mask actual type errors (typos become valid "generics")
- No proper generic constraint tracking

**Recommendation**: Explicitly track declared type parameters from generic declarations.

---

### ~~3. Type Parameter Field Access Default (`src/hir/typecheck.zig`)~~

**Status**: ✅ FIXED - Generic struct field types are inferred from instantiation context instead of defaulting to `i64`.

Field accesses now reuse inferred or explicit type arguments from the struct type, and generic parameters inside struct initializers are resolved using the initializer value types. Remaining work is broader generic constraint tracking beyond simple type parameter substitution.

---

### 4. Hash-Based Field Indexing (`src/backend/x86_64/isel.zig`)

**Location**: Lines 39-46

```zig
fn getSequentialFieldIndex(name: []const u8) i32 {
    if (std.mem.eql(u8, name, "x")) return 0;
    if (std.mem.eql(u8, name, "y")) return 1;
    // Fallback to hash-based index
    var hash: u32 = 0;
    for (name) |ch| hash = hash *% 31 +% ch;
    return @intCast(hash % MAX_STRUCT_FIELDS);
}
```

**Issues**:
- Only `x` and `y` have guaranteed stable indices
- Hash collisions possible for other field names (modulo 4)
- Different fields could map to same slot

**Recommendation**: Use struct definition order for field indexing, or implement proper field offset tracking.

---

### ~~5. Builtin println Detection (`src/hir/name_res.zig`)~~

**Status**: ✅ FIXED - Builtin macros now use a registry with lowering handlers

The ad-hoc `println` detection was replaced by a registry that installs builtin macros (`println!`, `print!`, `format!`, `eprintln!`) and records their MIR lowering handlers during name resolution. The registry feeds a def-id-to-handler map consumed by MIR lowering, removing hardcoded string comparisons and making it easy to add future macros without duplicating code.

---

## Placeholders and Incomplete Implementations

### ~~1. Object Emission (`src/driver.zig`)~~

**Status**: ✅ FIXED - Backend-generated assembly is now assembled into object files via Zig's toolchain with diagnostics for failures.

Object emission now feeds backend assembly into Zig's assembler to produce `.o` outputs. Errors surface as compiler diagnostics when the assembler or filesystem writes fail.

---

### 2. Unknown Type/Expression Kinds

Throughout the codebase, `Unknown` variants are used as error recovery:

**HIR Types** (`src/hir/hir.zig`):
```zig
pub const Kind = union(enum) {
    // ...
    Unknown,  // Placeholder for unresolved or error types
};
```

**HIR Expressions**:
```zig
Unknown,  // Placeholder for unresolved or error expressions
```

**MIR Types** (`src/mir/mir.zig`):
```zig
Unknown,  // Type could not be determined during lowering
```

**Issues**:
- `Unknown` propagates through the compiler without proper error handling
- Can lead to incorrect code generation if not caught

---

### 3. Method Call Type Inference (`src/hir/typecheck.zig`)

**Location**: Lines 381-387

```zig
.MethodCall => |call| {
    _ = try checkExpr(crate, call.target, diagnostics, locals, in_unsafe);
    for (call.args) |arg_id| {
        _ = try checkExpr(crate, arg_id, diagnostics, locals, in_unsafe);
    }
    expr.ty = try ensureType(crate, .Unknown);  // Always Unknown!
},
```

**Issues**:
- Method calls always have `Unknown` type
- No return type inference for methods

---

### 4. Lambda Return Type Inference (`src/hir/typecheck.zig`)

**Location**: Lines 594-612

```zig
fn inferLambdaTypes(crate: *hir.Crate, expr_id: hir.ExprId, expected_fn_params: []hir.TypeId) Error!void {
    // ...
    // Only updates param types, doesn't infer body type based on expected return
}
```

**Issues**:
- Only infers parameter types, not return types
- Limited to replacing `Unknown` with expected

---

## Logic That Doesn't Scale

### 1. Linear Item Lookups (`src/hir/typecheck.zig`, `src/hir/name_res.zig`)

Multiple locations perform O(n) scans through all items:

```zig
for (crate.items.items) |item| {
    if (item.kind == .Struct and std.mem.eql(u8, item.kind.Struct.name, struct_name)) {
        // ...
    }
}
```

**Impact**: Quadratic time complexity for programs with many items.

**Recommendation**: Build hash maps for name→item lookups during lowering.

---

### 2. Type Interning Linear Search (`src/hir/typecheck.zig`)

**Location**: Lines 624-631

```zig
fn ensureType(crate: *hir.Crate, kind: hir.Type.Kind) Error!hir.TypeId {
    for (crate.types.items) |existing| {
        if (std.meta.eql(existing.kind, kind)) return existing.id;
    }
    // ... append new type
}
```

**Issues**:
- O(n) scan for every type lookup
- `std.meta.eql` performs deep comparison including slice contents

**Impact**: Type checking becomes O(n²) with number of types.

**Recommendation**: Use hash-based type interning.

---

### 3. Register Allocation: Linear VReg Mapping

**Location**: `src/backend/x86_64/regalloc.zig`

```zig
var map = std.AutoHashMap(machine.VReg, Location).init(allocator);
```

While using a hash map, the spill logic scans all mapped registers:

```zig
var it = map.iterator();
while (it.next()) |entry| {
    if (entry.value_ptr.* == .phys and entry.value_ptr.phys == dst_reg) {
        // Spill this VReg...
    }
}
```

**Impact**: O(n) per instruction with n live variables.

**Recommendation**: Maintain reverse mapping (PhysReg→VReg) for O(1) lookups.

---

### 4. Struct Field Hash Collisions

With only 4 slots (`MAX_STRUCT_FIELDS`), field names hash to same locations:

```zig
return @intCast(hash % MAX_STRUCT_FIELDS);  // MAX_STRUCT_FIELDS = 4
```

**Impact**: Any struct with field names that hash to same slot (mod 4) will corrupt data.

**Example collisions**: Using the hash formula `hash = hash *% 31 +% ch`, then `% 4`:
- Fields "a" (hash 97) and "e" (hash 101) both map to slot 1
- Fields "b" (hash 98) and "f" (hash 102) both map to slot 2
- Fields "first" and "value" both map to slot 3

This means a struct like `{ first: i32, value: i32, data: i32, count: i32 }` may have field overlaps.

---

### 5. No Incremental Compilation

The entire compilation pipeline runs from scratch for every file:
- No caching of parsed/lowered results
- No dependency tracking between files
- Single-threaded execution

---

## Architecture Concerns

### 1. No SSA Form in MIR

The MIR uses mutable locals instead of SSA:
```zig
StoreLocal: struct { local: LocalId, src: Operand },
```

**Impact**: Makes dataflow analysis and optimization more complex.

---

### 2. Backend Tightly Coupled to x86-64

No abstraction layer between MIR and target architecture:
- `src/backend/backend.zig` directly calls `x86_64.codegen`
- No mechanism for adding ARM, RISC-V, etc.

---

### 3. Minimal Lifetime/Borrow Analysis

A lightweight ownership/borrow checker now runs after type checking. It tracks
move-only locals, enforces simple borrowing rules (shared vs. unique), and ends
borrows at block boundaries. The model is intentionally conservative and
currently only reasons about locals.

---

### 4. No Module/Crate System

Currently single-file compilation only:
- No `mod` declarations
- No `use` statements
- No separate compilation

---

## Recommendations Summary

### High Priority
1. ~~**Remove hardcoded struct limits**~~ ✅ FIXED - Limits increased significantly (MAX_STRUCT_FIELDS: 4→16, ASSUMED_GENERIC_STRUCT_FIELDS: 2→4)
2. ~~**Centralize shared constants**~~ ✅ FIXED - Created `src/shared_consts.zig` shared module
3. ~~**Remove debug prints**~~ ✅ FIXED - Parser debug output removed
4. **Fix generic type handling** - ⚠️ PARTIALLY IMPROVED - Limits increased but proper type parameter tracking not yet implemented

### Medium Priority
5. **Implement proper field layout** - Use definition order instead of hash-based indexing
6. **Add type lookup caching** - Use hash maps for O(1) lookups
7. **Implement method type inference** - Don't default to Unknown
8. **Expand builtin macro support** - At least `print!`, `format!`

### Low Priority / Long-term
9. **Consider SSA MIR** - Would simplify optimization passes
10. **Add backend abstraction** - Enable multi-target compilation
11. **Implement incremental compilation** - Cache intermediate results
12. **Add borrow checking** - Essential for Rust semantics

---

## Current Failing Tests

This section tracks the currently failing tests (from `zig test src/all_tests.zig`), their root causes, and the most direct fixes.

### 1. `frontend.parser.test.parse unsafe block as tail expression`

- **Failure**: The parsed function body has `result == null`, so the unsafe block is not captured as the tail expression.
- **Root Cause**: The parser’s tail-expression detection treats `unsafe { ... }` encountered at the end of a block as a statement, not an expression result. The `KwUnsafe` path returns `ast.Expr.Tag.Block`, but the surrounding block parsing does not plumb that value into the `result` slot when it is the final item.
- **Best Fix**: Update `parseBlock`/`parseFunction` tail handling to treat `KwUnsafe`-originated blocks like other expression nodes—i.e., if the final item is an unsafe block without a trailing semicolon, set `body.result` to that expression instead of emitting only a statement node. This preserves the tail expression for `-> T` functions.

### 2. `e2e_tests.test.e2e: arrays3.rs`

- **Failure**: Array elements print as `(0, 1033491000)` instead of the shifted `(2, 3)`, `(-2, 5)`, `(11, -4)`.
- **Root Cause**: Arrays of structs are indexed with incorrect byte strides, so the backend reads/writes the wrong addresses when computing `points[i]`. The index arithmetic currently uses pointer-sized steps instead of the struct’s full size, leading to corrupted field loads/stores during iteration.
- **Best Fix**: Rework array indexing/lowering to multiply indices by the element type’s layout (size + alignment) rather than a fixed pointer stride. Centralize element-size queries so both load and store paths use the same struct layout metadata.

### 3. `e2e_tests.test.e2e: core_language_test1.rs`

- **Failure**: Output is `1 4 7` instead of `1 6 10` after calling `Point::offset(self, dx, dy)` by value.
- **Root Cause**: Struct copies for by-value method receivers drop or misplace fields—the second field (`y`) is copied using an incorrect offset/size, so the callee sees stale or zeroed data. This mirrors the array-of-structs stride bug: struct layout information is not respected when materializing call arguments.
- **Best Fix**: Ensure struct moves/copies use the full computed layout (per-type field offsets and total size) when spilling `self` to the call frame. Share the same layout logic between method-call lowering and general struct copy routines to avoid hardcoded offsets.

---

## Files Analyzed

- `src/frontend/lexer.zig`
- `src/frontend/parser.zig`
- `src/frontend/ast.zig`
- `src/hir/hir.zig`
- `src/hir/name_res.zig`
- `src/hir/typecheck.zig`
- `src/mir/mir.zig`
- `src/mir/lower.zig`
- `src/mir/passes/passes.zig`
- `src/backend/backend.zig`
- `src/backend/x86_64/codegen.zig`
- `src/backend/x86_64/isel.zig`
- `src/backend/x86_64/regalloc.zig`
- `src/backend/x86_64/emitter.zig`
- `src/diag/diagnostics.zig`
- `src/driver.zig`
- `src/shared_consts.zig` (NEW - centralized constants module)
