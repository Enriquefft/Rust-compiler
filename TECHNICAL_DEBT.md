# Technical Debt Analysis Report

This document identifies bandaid solutions, temporary solutions, placeholders, and logic that doesn't scale within the Rust-compiler codebase.

---

## Table of Contents

1. [Bandaid Solutions](#1-bandaid-solutions)
2. [Temporary Solutions](#2-temporary-solutions)
3. [Placeholders](#3-placeholders)
4. [Logic That Doesn't Scale](#4-logic-that-doesnt-scale)
5. [Summary of Priority Issues](#5-summary-of-priority-issues)

---

## 1. Bandaid Solutions

These are quick fixes that address symptoms rather than root causes.

### 1.1 Sequential Field Index Fallback with Hardcoded Names

**Status:** ✅ **FIXED**

**Previous Location:** `src/backend/x86_64/isel.zig:62-69`

**Resolution:** The `getSequentialFieldIndex` function with hash-based fallback has been completely removed from the codebase. The struct layout system now exclusively uses declaration order for field layout computation, ensuring stable, collision-free access. See `src/shared_consts.zig` lines 7-12 for the current field layout strategy documentation.

---

### 1.2 Generic Function Parameter Workaround

**Location:** `src/backend/x86_64/isel.zig:369-383`

```zig
// For Param sources from generic functions (where type is Unknown), also store next param
// This handles generic functions like identity<T>(value: T) where T might be a struct
if (is_param_source) {
    const param_idx = payload.src.Param;
    // If this is param 0 (first argument), also store param 1 (rsi) to the adjacent slot
    if (param_idx == 0) {
        // ... stores rsi unconditionally
    }
}
```

**Issue:** For generic functions with unknown types, the code unconditionally stores the second register parameter assuming the first parameter might be a struct. This is a workaround for lack of proper monomorphization.

**Impact:** Wastes stack space for non-struct parameters and creates confusing behavior.

**Recommendation:** Implement proper monomorphization to know concrete types at code generation time.

---

### 1.3 Always Return Second Local for Struct Compatibility

**Location:** `src/backend/x86_64/isel.zig:1089-1100`

```zig
// For local operands, also return the adjacent local in rdx
// This handles generic functions returning struct values stored in consecutive locals
switch (op) {
    .Local => |local_id| {
        const next_local_id = local_id + 1;
        // Always try to return the second local for struct compatibility
        // For non-struct types, the caller will ignore rdx
        const next_local_mem = localMem(next_local_id);
        try insts.append(ctx.allocator, .{ .Mov = .{ .dst = .{ .Phys = .rdx }, .src = next_local_mem } });
    },
    else => {},
}
```

**Issue:** Always loads and returns an adjacent local in `rdx` "just in case" the return type is a struct.

**Impact:** Unnecessary memory access for non-struct returns; may read garbage if local+1 doesn't exist.

**Recommendation:** Use type information to determine if the return is actually a struct.

---

## 2. Temporary Solutions

These are implementations marked or designed as temporary that need proper replacement.

### 2.1 MAX_STRUCT_FIELDS Fixed Limit

**Status:** ✅ **FIXED**

**Location:** `src/shared_consts.zig:17` and `src/mir/lower.zig:37-38`

```zig
pub const MAX_STRUCT_FIELDS: u32 = 16;
```

**Previous Issue:** Hard limit of 16 struct fields that was used throughout MIR lowering and codegen with no error reporting.

**Resolution:** The code now explicitly reports an error when a struct exceeds the limit:
```zig
if (struct_item.fields.len > MAX_STRUCT_FIELDS) {
    diagnostics.reportError(struct_item.span, "struct has too many fields: maximum supported is 16");
}
```
The driver checks `hasErrors()` before proceeding to code generation, so compilation stops gracefully with a clear error message instead of producing incorrect code.

**Note:** The limit still exists, but the behavior is now fail-fast rather than silent corruption.

**Remaining Work:** Consider computing actual struct sizes dynamically instead of using a fixed maximum.

---

### 2.2 ASSUMED_GENERIC_STRUCT_FIELDS Workaround

**Location:** `src/shared_consts.zig:20-23`

```zig
/// Assumed number of fields for generic type parameters when the concrete type is unknown.
/// This is a workaround for lack of full monomorphization - assumes structs have this many fields.
/// Generic types with more fields than this value may have incorrect behavior.
pub const ASSUMED_GENERIC_STRUCT_FIELDS: u32 = 4;
```

**Issue:** When dealing with generic types, the code assumes 4 fields rather than knowing the actual type. Generic types with more fields than this value may have incorrect behavior.

**Impact:** Generic code handling structs with more than 4 fields may malfunction silently.

**Recommendation:** Implement proper monomorphization to eliminate this assumption.

---

### 2.3 Builtin Macro Detection by Name

**Location:** `src/mir/lower.zig:99`

```zig
// Skip builtin macros (println, print) with no body
if (fn_item.body == null and (std.mem.eql(u8, fn_item.name, "println") or std.mem.eql(u8, fn_item.name, "print"))) continue;
```

**Issue:** Builtin macros are detected by string comparison on the function name.

**Impact:** Custom functions named `println` or `print` with no body will be incorrectly skipped.

**Recommendation:** Use a proper builtin registry or attribute system.

---

### 2.4 Two-Field Struct Limitation

**Location:** `src/shared_consts.zig:43-44`

```zig
/// Note: Currently only 2-field structs are fully supported for passing/returning
/// through registers. The first field goes in rax/vreg, the second in rdx.
```

**Issue:** The calling convention implementation only properly handles structs with exactly 2 fields.

**Impact:** Structs with 3+ fields passed by value will have incorrect behavior.

**Recommendation:** Implement proper struct passing per System V ABI (aggregate return rules).

---

### 2.5 Lexical Borrow Checking Only

**Location:** `src/hir/ownership.zig:11-16`

```zig
/// The analysis is intentionally lightweight: it uses a single pass over the
/// typed HIR, assumes type checking has already succeeded, and restricts
/// itself to locals and block scoping. Field projections and pointer aliasing
/// are treated conservatively (only direct locals are tracked).
```

**Issue:** The ownership checker only tracks direct locals with lexical scoping, not field projections or pointer aliasing.

**Impact:** Invalid programs may pass ownership checking; valid programs with complex borrowing may fail.

**Recommendation:** Implement NLL (Non-Lexical Lifetimes) or at least field-level tracking.

---

## 3. Placeholders

These are incomplete implementations that need to be filled in.

### 3.1 `format!` Macro Not Implemented

**Location:** `src/mir/lower.zig:1864-1866`

```zig
.format => {
    self.diagnostics.reportError(span, "format! macro is not supported yet");
    return null;
},
```

**Issue:** The `format!` macro is recognized but not implemented.

**Recommendation:** Implement `format!` similar to `println!` but returning a string.

---

### 3.2 Unknown Type Fallbacks

**Location:** Multiple files

The codebase has many patterns like:

```zig
.Unknown => blk: {
    diagnostics.reportWarning(span, "unknown type information during MIR lowering");
    break :blk null;
},
```

**Issue:** Unknown types are used as placeholders when type inference fails, leading to downstream issues.

**Recommendation:** Improve type inference or fail more explicitly.

---

### 3.3 getFieldIndexFromDeclaration Fallback to Zero

**Status:** ✅ **FIXED**

**Location:** `src/mir/lower.zig:72-80`

```zig
fn getFieldIndexFromDeclaration(fields: []const hir.Field, field_name: []const u8) ?hir.LocalId {
    for (fields, 0..) |field, idx| {
        if (std.mem.eql(u8, field.name, field_name)) {
            return @intCast(idx);
        }
    }
    // Field not found - return null instead of incorrect fallback
    return null;
}
```

**Previous Issue:** Previously returned 0 when field was not found instead of reporting an error.

**Resolution:** The function now returns an optional type (`?hir.LocalId`). Callers handle the `null` case by reporting a proper error:
```zig
getFieldIndexFromDeclaration(fields, field.name) orelse blk: {
    self.diagnostics.reportError(span, "field not found in struct definition");
    break :blk 0;
}
```
The driver checks `hasErrors()` before code generation, ensuring the program fails gracefully.

**Test Coverage:** Unit test at line 2717 verifies: `try std.testing.expectEqual(@as(?hir.LocalId, null), getFieldIndexFromDeclaration(&fields, "unknown"));`

---

### 3.4 MethodCall and UnresolvedIdent in MIR Lowering

**Location:** `src/mir/lower.zig:1030-1033`

```zig
.Path, .MethodCall, .UnresolvedIdent, .Unknown => {
    self.diagnostics.reportError(expr.span, "expression could not be lowered to MIR");
    return null;
},
```

**Issue:** These expression types are not fully handled in MIR lowering.

**Recommendation:** Resolve method calls and paths fully before MIR lowering, or implement proper lowering.

---

## 4. Logic That Doesn't Scale

These are implementations that work for small inputs but will fail or become problematic at scale.

### 4.1 Linear Field Search in Struct Layouts

**Location:** `src/mir/mir.zig:287-295`

```zig
pub fn getFieldOffset(self: *const MirCrate, struct_name: []const u8, field_name: []const u8) ?i32 {
    const layout = self.struct_layouts.get(struct_name) orelse return null;
    for (layout.fields) |field| {
        if (std.mem.eql(u8, field.name, field_name)) {
            return field.offset;
        }
    }
    return null;
}
```

**Issue:** O(n) field lookup for each field access, where n is the number of fields. Note that this correctly returns `null` when field is not found, which is proper error handling.

**Impact:** Hot paths with many field accesses on large structs will be slow at compile time.

**Recommendation:** Use a hash map for field lookups or pre-compute field name-to-offset mapping during layout construction.

---

### 4.2 Linear Search for Struct Type Resolution

**Location:** `src/mir/lower.zig:446-457`

```zig
if (path.segments.len == 1) {
    for (self.hir_crate.items.items) |item| {
        if (item.kind == .Struct and std.mem.eql(u8, item.kind.Struct.name, path.segments[0])) {
            is_struct_arg = true;
            struct_fields = item.kind.Struct.fields;
            break;
        }
    }
}
```

**Issue:** Searches through all items to find a struct by name, O(n) where n is number of items.

**Impact:** Compilation time grows linearly with crate size for each type resolution.

**Recommendation:** Build a name-to-item lookup table during HIR construction.

---

### 4.3 O(n) Method Lookup in Impl Blocks

**Location:** `src/mir/lower.zig:1574-1619`

```zig
for (self.hir_crate.items.items) |item| {
    if (item.kind == .Impl) {
        const impl_item = item.kind.Impl;
        // Check if this impl is for our struct
        // ...
        for (impl_item.methods) |method_id| {
            // Search through methods
        }
    }
}
```

**Issue:** Nested loops: O(impls * methods) for each method call resolution.

**Impact:** Quadratic time complexity for codebases with many impl blocks.

**Recommendation:** Build method resolution tables during HIR processing.

---

### 4.4 No Type Caching in HIR

**Location:** `src/hir/hir.zig` and `src/hir/typecheck.zig`

**Issue:** Types are appended to an arena without deduplication. Identical types (e.g., multiple `i64` references) create multiple entries.

**Impact:** Memory usage grows unnecessarily; type comparisons require deep equality checks.

**Recommendation:** Implement type interning with a hash-consing approach.

---

### 4.5 String Interning Only for Data Literals

**Location:** `src/backend/x86_64/isel.zig:113-121`

```zig
fn internString(self: *DataTable, value: []const u8) ![]const u8 {
    for (self.items.items) |item| {
        if (std.mem.eql(u8, item.bytes, value)) return item.label;
    }
    // Create new entry
}
```

**Issue:** String interning uses linear search; only applies to data literals, not identifiers/names.

**Recommendation:** Use a hash map and extend interning to all string data.

---

### 4.6 Per-Expression Type Allocation

**Location:** `src/hir/hir.zig:763-768`

```zig
fn appendUnknownType(crate: *Crate, next_type_id: *TypeId) LowerError!TypeId {
    const id = next_type_id.*;
    next_type_id.* += 1;
    try crate.types.append(crate.allocator(), .{ .id = id, .kind = .Unknown });
    return id;
}
```

**Issue:** Every expression gets its own type ID even before type inference, allocating Unknown types liberally.

**Impact:** Type arena grows rapidly with expression count.

**Recommendation:** Share type IDs for identical types; use a sentinel Unknown type ID.

---

### 4.7 Register Allocation is Naive

**Location:** `src/backend/x86_64/regalloc.zig`

**Issue:** Register allocation appears to be simple linear scan or trivial allocation without live range analysis.

**Impact:** Poor code quality and excessive spills for functions with many live variables.

**Recommendation:** Implement proper live range analysis with graph coloring or linear scan.

---

### 4.8 No Loop Optimizations

**Location:** `src/mir/passes/`

**Issue:** MIR passes include constant folding, dead code elimination, and CFG simplification, but no loop-focused optimizations (LICM, unrolling, etc.).

**Impact:** Loop-heavy code will not benefit from common optimizations.

**Recommendation:** Add loop analysis and optimizations as compilation scales.

---

## 5. Summary of Priority Issues

### Critical (Data Correctness at Risk)
*All critical issues have been resolved:*
1. ✅ **FIXED** - Hash-based field index fallback can cause data corruption → Removed; uses declaration order
2. ✅ **FIXED** - MAX_STRUCT_FIELDS limit silently fails for large structs → Now reports error and stops compilation
3. ✅ **FIXED** - getFieldIndexFromDeclaration returns 0 for missing fields → Returns optional; callers report error

### High (Significant Functionality Gaps)
1. Two-field struct limitation for register passing
2. ASSUMED_GENERIC_STRUCT_FIELDS workaround
3. Missing monomorphization

### Medium (Performance and Maintainability)
1. Linear search patterns throughout codebase
2. No type interning/caching
3. Naive register allocation

### Low (Nice to Have)
1. format! macro not implemented
2. Missing loop optimizations
3. Lexical-only borrow checking

---

## Appendix: Files Referenced

- `src/shared_consts.zig` - Shared constants including struct limits
- `src/mir/lower.zig` - HIR to MIR lowering
- `src/mir/mir.zig` - MIR data structures
- `src/backend/x86_64/isel.zig` - Instruction selection
- `src/hir/hir.zig` - HIR data structures and lowering from AST
- `src/hir/ownership.zig` - Ownership/borrow checking
- `src/hir/typecheck.zig` - Type checking
- `src/frontend/parser.zig` - Parser implementation
