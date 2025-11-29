# Codebase Analysis: Bandaid Solutions, Temporary Logic, Placeholders, and Scalability Issues

This document provides a comprehensive analysis of the Rust-subset compiler codebase, identifying areas that may need attention for long-term maintainability and scalability.

## Table of Contents

1. [Hardcoded Constants and Magic Numbers](#hardcoded-constants-and-magic-numbers)
2. [Bandaid and Temporary Solutions](#bandaid-and-temporary-solutions)
3. [Placeholders and Incomplete Implementations](#placeholders-and-incomplete-implementations)
4. [Logic That Doesn't Scale](#logic-that-doesnt-scale)
5. [Architecture Concerns](#architecture-concerns)
6. [Recommendations Summary](#recommendations-summary)

---

## Hardcoded Constants and Magic Numbers

### 1. Backend: Fixed Struct Field Limits (`src/backend/x86_64/isel.zig`)

**Location**: Lines 10-34

```zig
const MAX_STRUCT_FIELDS: u32 = 4;
const ASSUMED_STRUCT_FIELDS: u32 = 2;
const LOCAL_STACK_MULTIPLIER: u32 = 4;
const MAX_EXTRA_ARRAY_ELEMENTS: usize = 7;
const STRUCT_SECOND_FIELD_OFFSET: i64 = -@as(i64, @intCast(@sizeOf(i64) * LOCAL_STACK_MULTIPLIER));
```

**Issues**:
- `MAX_STRUCT_FIELDS: u32 = 4` - Limits all structs to 4 fields maximum. Any struct with more fields will have field hash collisions.
- `ASSUMED_STRUCT_FIELDS: u32 = 2` - Assumes all generic type parameters are 2-field structs.
- `MAX_EXTRA_ARRAY_ELEMENTS: usize = 7` - Limits array initialization to 8 elements when passing through registers.

**Impact**: Fails silently for larger structs; no error reporting for field count violations.

**Recommendation**: Either increase limits significantly or implement dynamic field layout computation.

---

### 2. MIR Lower: Duplicated Hardcoded Constants (`src/mir/lower.zig`)

**Location**: Lines 23-34

```zig
const MAX_STRUCT_FIELDS: u32 = 4;
const ASSUMED_GENERIC_STRUCT_FIELDS: u32 = 2;
const LOCAL_STACK_MULTIPLIER: u32 = 4;
```

**Issues**:
- Constants are duplicated between `isel.zig` and `lower.zig` with a comment "Must match the same constant in backend/x86_64/isel.zig"
- Manual synchronization required - easy to miss when one changes.

**Recommendation**: Create a shared constants module or use comptime assertions to verify consistency.

---

### 3. Register Allocator: Fixed Register Set (`src/backend/x86_64/regalloc.zig`)

**Location**: Lines 23-24

```zig
const available = [_]machine.PhysReg{
    .rax, .rcx, .rdx, .rsi, .rdi, .r8, .r9, .r10,
};
const spill_scratch: machine.PhysReg = .r11;
```

**Issues**:
- Only 8 registers available for allocation
- `r11` permanently reserved for spill operations
- Callee-saved registers (`rbx`, `r12`-`r15`) not utilized
- No floating-point register allocation (XMM registers)

**Impact**: Excessive spilling for complex functions; suboptimal code generation.

**Recommendation**: Implement proper callee-saved register handling and XMM allocation.

---

### 4. Parser: Debug Print Statements (`src/frontend/parser.zig`)

**Location**: Lines 74, 82

```zig
std.debug.print("Parsing function: {s}\n", .{name.name});
// ... 
std.debug.print("Parsed function: {s}\n", .{name.name});
```

**Issues**:
- Debug print statements left in production code
- Always prints to stderr during parsing

**Recommendation**: Remove or gate behind a debug flag.

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

### 1. Generic Type Parameter Handling (`src/mir/lower.zig`)

**Location**: Lines 105-171

```zig
// Check if this is a generic type parameter (single letter like T, U, V)
// Generic params are typically single uppercase letters
if (struct_name.len == 1 and struct_name[0] >= 'A' and struct_name[0] <= 'Z') {
    is_generic_param = true;
}
// ...
// For generic type parameters (T, U, etc.), assume they might be structs
// and allocate space for ASSUMED_GENERIC_STRUCT_FIELDS fields
// This is a workaround for lack of full monomorphization
```

**Issues**:
- Detects generic params by checking for single uppercase letters (naive heuristic)
- Assumes all generic type parameters are 2-field structs
- No actual monomorphization - just guesses at runtime

**Impact**: Generic types with >2 fields will corrupt memory; non-struct generic types waste space.

**Example of corruption**: If a generic function receives a 3-field struct `Point3D { x, y, z }`, only `x` and `y` are stored. When the function returns or accesses the struct, `z` contains garbage from stack memory, potentially causing incorrect calculations or crashes.

**Recommendation**: Implement proper monomorphization or at minimum, track generic constraints.

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

### 3. Type Parameter Field Access Default (`src/hir/typecheck.zig`)

**Location**: Lines 884-894, 920-930

```zig
// Check if the field type is a type parameter (Path that's not a known type)
// If so, default to i64 for basic generic support
if (field_type_kind == .Path) {
    const path_info = field_type_kind.Path;
    if (path_info.segments.len == 1 and !isKnownTypeName(crate, path_info.segments[0])) {
        // Type parameter - use default i64 type
        return ensureType(crate, .{ .PrimInt = .I64 });
    }
}
```

**Issues**:
- Generic field types default to `i64` regardless of actual usage
- Will produce wrong types for float, string, bool, or struct generic parameters

**Recommendation**: Infer generic parameter types from actual usage sites.

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

### 5. Builtin println Detection (`src/hir/name_res.zig`)

**Location**: Lines 87-103

```zig
fn ensureBuiltinPrintln(crate: *hir.Crate) Error!Builtin {
    for (crate.items.items) |item| {
        if (item.kind == .Function and std.mem.eql(u8, item.kind.Function.name, "println")) {
            return .{ .name = item.kind.Function.name, .def_id = item.id };
        }
    }

    const id: hir.DefId = @intCast(crate.items.items.len);
    const name = try crate.allocator().dupe(u8, "println");
    const span = hir.emptySpan(0);
    // Creates a synthetic function with no body - body = null indicates builtin
    const func: hir.Function = .{
        .def_id = id,
        .name = name,
        .params = &[_]hir.LocalId{},
        .param_types = &[_]hir.TypeId{},
        .return_type = null,
        .body = null,
        .span = span,
    };
    try crate.items.append(crate.allocator(), .{ .id = id, .kind = .{ .Function = func }, .span = span });
    return .{ .name = name, .def_id = id };
}
```

**Issues**:
- Only supports `println` macro, not general macro system
- Hardcoded function name string matching
- No support for `print!`, `format!`, `eprintln!`, etc.

**Recommendation**: Implement proper macro system or at least a registry of builtin macros.

---

## Placeholders and Incomplete Implementations

### 1. Object Emission (`src/driver.zig`)

**Location**: Lines 193-197

```zig
.object =>
    diagnostics.reportError(
        .{ .file_id = file_id, .start = 0, .end = 0 },
        "object emission is not implemented yet",
    ),
```

**Status**: Completely unimplemented - always errors.

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

### 3. Missing Lifetime/Borrow Analysis

The type checker notes:
```zig
/// ## Limitations
/// - No borrow checking or lifetime analysis
/// - Limited generic type support
/// - No trait bounds checking
```

---

### 4. No Module/Crate System

Currently single-file compilation only:
- No `mod` declarations
- No `use` statements
- No separate compilation

---

## Recommendations Summary

### High Priority
1. **Remove hardcoded struct limits** - Either compute actual sizes or increase limits significantly
2. **Centralize shared constants** - Create a shared module for duplicated constants
3. **Remove debug prints** - Remove or gate parser debug output
4. **Fix generic type handling** - Track actual type parameters instead of guessing

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
