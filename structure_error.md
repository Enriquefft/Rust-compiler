# Struct Representation Structural Fix

## 1. Symptom

For the program:

```rust
struct Point { x: i32, y: i32 }

fn add(a: i32, b: i32) -> i32 { a + b }

impl Point {
    fn offset(self, dx: i32, dy: i32) -> Point {
        Point { x: self.x + dx, y: self.y + dy }
    }
}

fn main() {
    let start = Point { x: 1, y: 2 };
    let moved = start.offset(3, 4);
    let sum = add(moved.x, moved.y);
    println!("{} {} {}", start.x, moved.y, sum);
}
````

The MIR is logically correct and describes:

* `offset(self, dx, dy) -> Point` as operating on `self.x`, `self.y` and returning a `Point`.
* `main` as:

  * building `start: Point`,
  * calling `Point_offset(start, 3, 4) -> moved`,
  * computing `sum = add(moved.x, moved.y)`,
  * printing `start.x`, `moved.y`, `sum`.

Expected output:

```text
1 6 10
```

Actual output from generated assembly:

```text
1 27262976 27262980
```

The first value (`start.x`) is correct; `moved.y` and `sum` are garbage.

---

## 2. Root Cause (Structural Mismatch)

### 2.1 Single-local vs multi-local struct representation

Conceptually, MIR intends:

* Each struct value (`MirType.Struct`) is represented as **one** `LocalId` / temporary.
* Fields are accessed via `InstKind.Field` on that single local.
* In memory, that local’s stack region contains all fields, laid out contiguously.

But in the current lowering:

1. Struct values (especially `self` in methods) are effectively split into **multiple integer locals** instead of one `Struct` local.
2. `InstKind.Field` is lowered assuming that **all struct fields live in one local’s stack chunk**.
3. `StoreLocal` only ever writes the **first 8 bytes** for non-array types; additional struct words (e.g. `rdx` for second field) are not stored into the same local.

This creates contradictions between the MIR model and the actual layout produced by codegen.

### 2.2 Concrete evidence in `isel.zig`

**Stack layout for locals**

```zig
const LOCAL_STACK_MULTIPLIER: u32 = 4;                // isel.zig:15

// Per-function stack size:
const stack_size = func.locals.len * @sizeOf(i64) *
    LOCAL_STACK_MULTIPLIER;                            // isel.zig:139–145

fn localMem(local: mir.LocalId) machine.MOperand {     // isel.zig:626–628
    const offset: i32 =
        -@as(i32, @intCast((local + 1) * @sizeOf(i64) * LOCAL_STACK_MULTIPLIER));
    return .{ .Mem = .{ .base = .rbp, .offset = offset } };
}
```

Each MIR local gets a 32-byte chunk on the stack; `local 0` at `[rbp-32]`, `local 1` at `[rbp-64]`, and so on.

**StoreLocal lowering**

```zig
.StoreLocal => |payload| {                           // isel.zig:258–277
    const mem = localMem(payload.local);
    const src = try lowerOperand(ctx, payload.src, vreg_count);
    try insts.append(ctx.allocator,
        .{ .Mov = .{ .dst = mem, .src = src } });

    // Arrays: store extra elements from rdx, rcx, r8
    if (inst.ty) |ty| {
        if (ty == .Array) {                          // isel.zig:265–274
            const mem2 = machine.MOperand{
                .Mem = .{ .base = mem.Mem.base, .offset = mem.Mem.offset - 8 } };
            try insts.append(ctx.allocator,
                .{ .Mov = .{ .dst = mem2, .src = .{ .Phys = .rdx } } });

            const mem3 = machine.MOperand{
                .Mem = .{ .base = mem.Mem.base, .offset = mem.Mem.offset - 16 } };
            try insts.append(ctx.allocator,
                .{ .Mov = .{ .dst = mem3, .src = .{ .Phys = .rcx } } });

            const mem4 = machine.MOperand{
                .Mem = .{ .base = mem.Mem.base, .offset = mem.Mem.offset - 24 } };
            try insts.append(ctx.allocator,
                .{ .Mov = .{ .dst = mem4, .src = .{ .Phys = .r8 } } });
        }
    }
}
```

For structs there is **no** extra handling: only the first word is stored.

**Field lowering**

```zig
.Field => |payload| {                                // isel.zig:450–466
    if (dest_vreg) |dst| {
        const target = try lowerOperand(ctx, payload.target, vreg_count);
        const mem = switch (target) {
            .Mem => |base_mem| blk: {
                // hash(field_name) -> field_index
                var hash: u32 = 0;
                for (payload.name) |ch| hash = hash *% 31 +% ch;
                const field_index: i32 =
                    @intCast(hash % MAX_STRUCT_FIELDS);
                // locals grow downward; fields are in successive 8-byte slots
                const offset = base_mem.offset -
                    field_index * @as(i32, @intCast(@sizeOf(i64)));
                break :blk machine.MOperand{
                    .Mem = .{ .base = base_mem.base, .offset = offset } };
            },
            else => { ... },
        };
        try insts.append(ctx.allocator,
            .{ .Mov = .{ .dst = .{ .VReg = dst }, .src = mem } });
    }
}
```

`Field` assumes a **single base_mem** where all struct fields are packed in contiguous 8-byte slots. E.g., for a two-field `Point`:

* `"x"` → `[base]`
* `"y"` → `[base-8]`

**StructInit lowering**

```zig
.StructInit => |payload| {                           // isel.zig:510–522
    if (dest_vreg) |dst| {
        if (payload.fields.len > 0) {
            const first = try lowerOperand(
                ctx, payload.fields[0].value, vreg_count);
            try insts.append(ctx.allocator,
                .{ .Mov = .{ .dst = .{ .VReg = dst }, .src = first } });

            if (payload.fields.len > 1) {
                const second = try lowerOperand(
                    ctx, payload.fields[1].value, vreg_count);
                // second field goes to rdx
                try insts.append(ctx.allocator,
                    .{ .Mov = .{ .dst = .{ .Phys = .rdx }, .src = second } });
            }
        } else {
            ...
        }
    }
}
```

This sets up a **two-register struct value** `(vreg_dst, rdx)` for returns, but `StoreLocal` never writes `rdx` into the same local’s chunk.

**Net effect in the example**

* `Point_offset` sees `self` as multiple integer locals, but `Field self.y` is compiled assuming `self` is a single struct local.
* At the call site, `Point_offset`’s returned struct is half-stored:

  * first field is written into the struct local (via `StoreLocal`),
  * second field never lands in that local’s memory, so `Field moved.y` reads an uninitialized slot.

---

## 3. Design Goal

Define and enforce a single, consistent invariant:

> **Invariant:** A `MirType.Struct` value corresponds to exactly one MIR local/temporary.
> That local’s storage is a contiguous chunk of 8-byte slots on the stack. `Field` accesses these slots, and all code that produces or consumes struct values (parameters, locals, returns) must respect this layout.

The rest of the design must be made consistent with this invariant.

---

## 4. Full Fix – Alternative A (Recommended): First-class struct values

### 4.1 HIR → MIR: enforce “one struct = one local”

In the lowering from HIR to MIR:

1. **Struct locals**

   For:

   ```rust
   let start = Point { x: 1, y: 2 };
   ```

   MIR should look like:

   ```text
   // locals: [0: Struct(Point), ...]
   t0 = StructInit { x: ImmInt(1), y: ImmInt(2) }   // Inst.ty = Struct
   StoreLocal 0 <- t0                              // Local 0 is the struct
   ```

   Do **not**:

   * Create separate integer locals for `start.x` and `start.y`.
   * Emit `StoreLocal` into multiple locals for each field.

2. **Method parameters**

   For:

   ```rust
   impl Point {
       fn offset(self, dx: i32, dy: i32) -> Point { ... }
   }
   ```

   Ensure the MIR function for `Point_offset` has:

   ```text
   params = [local_self, local_dx, local_dy]
   locals[local_self] : MirType.Struct
   locals[local_dx]   : MirType.I32
   locals[local_dy]   : MirType.I32
   ```

   And the body uses:

   ```text
   t0 = Field Local(local_self).x
   t1 = Bin Add t0, Local(local_dx)
   t2 = Field Local(local_self).y
   t3 = Bin Add t2, Local(local_dy)
   t4 = StructInit { x: t1, y: t3 }
   Ret t4
   ```

   There should be **no separate locals** that represent `self.x` / `self.y`.

3. **Method calls**

   For:

   ```rust
   let moved = start.offset(3, 4);
   ```

   MIR:

   ```text
   t0 = StructInit { x: ImmInt(1), y: ImmInt(2) }
   StoreLocal local_start <- t0

   t1 = Call @Point_offset(Local(local_start), ImmInt(3), ImmInt(4))
   StoreLocal local_moved <- t1
   ```

   Again: one argument for the whole `Point`, not multiple for its fields.

Result: MIR now exactly matches the “single struct local” model that `Field` and the stack layout already assume.

### 4.2 Codegen: make struct layout coherent

With MIR fixed, the backend needs to make the memory layout reflect the invariant.

1. **Stack layout**

   Keep `LOCAL_STACK_MULTIPLIER` and `localMem` as-is; each local gets a 32-byte window. For a 2-field `Point`:

   * `Field "x"`: `[base]` (e.g. `[rbp-96]`)
   * `Field "y"`: `[base-8]` (e.g. `[rbp-104]`)

   This matches your current `Field` lowering.

2. **StoreLocal for structs**

   Extend `StoreLocal` to handle `MirType.Struct` like it already does for arrays:

   ```zig
   .StoreLocal => |payload| {
       const mem = localMem(payload.local);
       const src = try lowerOperand(ctx, payload.src, vreg_count);
       try insts.append(ctx.allocator,
           .{ .Mov = .{ .dst = mem, .src = src } });

       if (inst.ty) |ty| {
           if (ty == .Array) {
               // existing array handling...
           } else if (ty == .Struct) {
               // For now: assume small 2-field struct in (src, rdx)
               const second_field_mem = machine.MOperand{
                   .Mem = .{
                       .base = mem.Mem.base,
                       .offset = mem.Mem.offset -
                           @as(i32, @intCast(@sizeOf(i64))),
                   },
               };
               try insts.append(ctx.allocator,
                   .{ .Mov = .{ .dst = second_field_mem,
                                .src = .{ .Phys = .rdx } } });
           }
       }
   }
   ```

   This ensures that when you store a `Point` result into a local:

   * first field goes to `[base]`,
   * second field goes to `[base-8]`,
     exactly where `Field` expects them.

3. **StructInit and Ret**

   Keep your existing conventions:

   * `StructInit` builds values in `(vreg_dst, rdx)` for 2-field structs (isel.zig:510–522),
   * `Ret` moves the first part into `rax`, leaving `rdx` as second part.

   The call/return ABI is now consistent:

   * callee: returns `Point` in `(rax, rdx)`,
   * caller: `StoreLocal local_moved <- t_ret` writes both fields into `local_moved`’s chunk on the stack,
   * `Field local_moved.x/y` read the correct slots.

4. **Parameters for struct arguments**

   For now, you can keep passing struct arguments via registers/stack according to your chosen ABI and **immediately `StoreLocal` them into the struct local** at function entry. The important constraint is:

   * All fields of the struct must be written into the single struct local’s chunk before any `Field` is executed.

   E.g., for `self: Point`:

   ```asm
   ; entry to Point_offset
   ; rdi, rsi hold x and y (per your current ABI)
   mov [rbp-32], rdi       ; self.x
   mov [rbp-40], rsi       ; self.y (same local, next slot)
   ```

   This replaces the current pattern where `self`’s fields are stored in different locals.

## 6. Recommendation

1. Fix HIR→MIR so that every struct value (locals, parameters, `self`, returns) is a single `MirType.Struct` local/temporary.
2. At function entry, store struct parameters into that local’s stack window in a layout consistent with `Field`.
3. Extend `StoreLocal` to write all struct fields for struct values (at least the first two fields via `(src, rdx)` as you already use in `StructInit`/`Ret`).
4. Keep `Field`’s contiguous-slot model and `localMem` unchanged.

This is a structural fix that aligns MIR, stack layout, field access, and the ABI. With these changes, the example program should consistently produce:

```text
1 6 10
```
