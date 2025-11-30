# Known Bugs

This document tracks known bugs in the Rust compiler implementation.

## Bug 1: Struct Copy Semantics in Method Calls

**Affected Test:** `core_language_test1.rs`

**Status:** Open

### Description

When a struct is passed by value to a method and the original struct is accessed afterwards, the compiler produces incorrect values. The struct copy operation before the method call appears to not preserve the original values correctly.

### Expected vs Actual Output

- **Expected:** `1 6 10`
- **Actual:** `1 4 7`

### Test Case

```rust
struct Point { x: i32, y: i32 }

type MyInt = i32;

fn add(a: MyInt, b: MyInt) -> MyInt {
    a + b
}

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
```

### Analysis

The issue appears in how struct values are handled when passed to methods that take `self` by value:

1. `start.x` prints `1` (correct - original x value)
2. `moved.y` should be `2 + 4 = 6`, but prints `4` (incorrect)
3. `sum` should be `(1+3) + (2+4) = 4 + 6 = 10`, but prints `7` (incorrect, computed as `4 + 3` or similar)

The bug seems to be related to how struct field offsets are computed or how the struct is copied when passed to the method.

---

## Bug 2: Arrays of Structs Produce Garbage Values

**Affected Test:** `arrays3.rs`

**Status:** Open

### Description

When iterating over an array of structs and accessing struct fields, the compiler produces garbage values instead of the correct field values.

### Expected vs Actual Output

- **Expected:**
  ```
  (2, 3)
  (-2, 5)
  (11, -4)
  ```

- **Actual:**
  ```
  (0, -117849544)
  (0, -117849544)
  (0, -117849544)
  ```

### Test Case

```rust
struct Point {
    x: i32,
    y: i32,
}

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

### Analysis

The garbage values (-117849544) suggest uninitialized memory access. Possible causes, in order of investigation priority:

1. **[Most Likely]** For-in loop iteration over struct references not computing correct addresses - the iteration produces the same garbage values for all elements, suggesting the loop is reading from the same incorrect memory location
2. Struct field access via reference not using correct offsets - could explain why both x and y are wrong
3. Wrong element offset calculation when indexing into array of structs - the mutation phase (`points[i].x += 1`) might work correctly but reading fails
4. Incorrect struct size calculation when used in arrays - less likely as the array mutation appears to work

---

## Bug 3: Unsafe Block with Statements Causes Compiler Hang

**Affected Test:** `unsafe_blocks_test2.rs`

**Status:** Open

### Description

The compiler hangs indefinitely (infinite loop) when compiling an unsafe block that contains statements rather than just an expression.

### Test Case

```rust
fn main() {
    let mut value: i32 = 10;
    
    unsafe {
        value = 20;
    }
    
    println!("{}", value);
}
```

### Expected Output

`20`

### Analysis

The compiler successfully handles `unsafe_blocks_test1.rs` which uses an unsafe block as an expression:
```rust
let x: i32 = unsafe { 42 };
```

However, it hangs when the unsafe block contains statements (like an assignment) rather than returning a value. 

**Suspected Phase:** The hang likely occurs in one of the following phases:
1. **HIR lowering** - When processing unsafe block statements that don't produce a value
2. **MIR generation** - When building the MIR for statement-only blocks
3. **Code generation** - Less likely, as the hang occurs early (no output is produced)

The difference between the working and failing cases is that the working case uses unsafe as an expression (returns a value), while the failing case uses it as a statement block (void/unit type). This suggests the lowering logic for statement-only unsafe blocks may enter an infinite loop or recursion.
