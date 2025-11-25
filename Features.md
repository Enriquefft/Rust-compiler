# Minimal Rust-Subset Compiler – Features

This document describes the language subset implemented by the minimal compiler and is consistent with the current grammar.

## 1. Core language

- Compilation unit:
  `Crate = { Item }`
- Items:
  - Function: `fn name(...) -> T { ... }`
  - Struct: `struct Name { ... }`
  - Type alias: `type Name = T;`
  - `impl` blocks for inherent methods on types (no traits).
- Entry point: `fn main() { ... }`.
- Blocks: `{ stmt* [expr] }`, last expression (without `;`) is the value.

## 2. Types

### 2.1 Primitive types

- Integers: `i32`, `i64`, `u32`, `u64`, `usize`
- Floats: `f32`, `f64`
- Booleans: `bool`
- Character: `char`
- String-related:
  - `str` (normally used as `&str`)
  - `String` (owned string)

### 2.2 Composite / derived types

- Arrays: `[T; N]` where `N` is a constant integer expression.
- References: `&T`, `&mut T`.
- Raw pointers: `*const T`, `*mut T`.
- Function types: `fn(T1, T2, ...) -> R`.
- Named types and generics via paths:
  - `Path = Ident ( "::" Ident )* [ "<" Type, ... ">" ]`.

### 2.3 Structs and type aliases

- Structs:
  ```rust
  struct Point {
      x: i32,
      y: i32,
  }
  ```

- Type aliases:

  ```rust
  type MyInt = i64;
  type Point2D = Point;
  ```

## 3. Expressions and operators

### 3.1 Arithmetic and logical operators

* Arithmetic: `+`, `-`, `*`, `/`, `%` on numeric types.
* Comparisons: `==`, `!=`, `<`, `<=`, `>`, `>=`.
* Boolean logic: `&&`, `||`, `!`.
* Unary: `-expr`, `!expr`, pointer deref `*expr`, reference `&expr`, `&mut expr`.

### 3.2 Assignments

* Place expressions: variables, field access, indexing, dereference:

  * `place = expr`
  * Compound assignments: `+=`, `-=`, `*=`, `/=`.
* L-values:

  * `x`
  * `x[i]`
  * `x.field`
  * `*ptr`
  * Parenthesized: `(place)`

### 3.3 Type conversions

* Explicit casts with `as`:

  ```rust
  let x: i64 = 10i32 as i64;
  let y: i32 = 3.5f32 as i32;
  let p: *const i32 = &x as *const i32;
  ```

### 3.4 Conditionals and ranges

* Conditional expressions:

  ```rust
  let x = if cond { a } else { b };
  ```
* Range expressions:

  * Half-open: `a..b`
  * Closed: `a..=b`

## 4. Control flow

* `if` / `else if` / `else` as statements and expressions.
* `while` loops:

  ```rust
  while cond {
      ...
  }
  ```
* `for` loops:

  ```rust
  for x in expr {
      ...
  }
  ```

  Grammar-level support; semantics depend on the runtime model of iteration.
* `return`:

  * `return;`
  * `return expr;`
  * Implicit return via final expression in a block.

> Note: `break` and `continue` are not part of this subset.

## 5. Statements, bindings, and patterns

* Let-bindings:

  ```rust
  let x = expr;
  let mut x: T = expr;
  let x: T;
  ```
* Patterns supported:

  * Identifier: `x`
  * Wildcard: `_`
* Expression statements: `expr;`
* Empty statement: `;`

## 6. Functions and methods

* Function definitions:

  ```rust
  fn add(a: i32, b: i32) -> i32 {
      a + b
  }
  ```
* Parameters:

  * By value: `x: T`
  * By reference: `x: &T`, `x: &mut T`
* Methods in inherent impl blocks (no traits):

  ```rust
  impl Point {
      fn length_sq(&self) -> i32 {
          self.x * self.x + self.y * self.y
      }
  }
  ```

## 7. Generics

* Generic parameters on functions, structs, and impls:

  ```rust
  fn id<T>(x: T) -> T { x }

  struct Pair<T> {
      a: T,
      b: T,
  }

  impl<T> Pair<T> {
      fn first(&self) -> &T { &self.a }
  }
  ```
* Generic arguments:

  * `Pair<i32>`
  * `Option<Box<Node<i32>>>`
* No trait bounds (`T: Ord`, etc.) in this subset.

## 8. Lambdas / closures

* Closure expressions:

  ```rust
  let f = |x: i32| x + 1;
  let g = |x, y| { x + y };
  ```
* Closures may be:

  * Assigned to variables.
  * Passed to functions.
* Function types are used to type callable values:

  ```rust
  fn apply(f: fn(i32) -> i32, x: i32) -> i32 {
      f(x)
  }
  ```

## 9. Macros (parsed but treated as function calls)

* Macro-call syntax is allowed:

  ```rust
  println!("value = {}", x);
  ```
* Semantically treated as if writing:

  ```rust
  println("value = {}", x);
  ```
* No user-defined macro expansion or hygienic macro system.

## 10. Dynamic data and memory model (semantic)

The grammar is purely syntactic, but the compiler/runtime supports:

* Heap allocation via user-defined types like `Box<T>` and `String`.
* Linked and tree-like structures:

  ```rust
  struct Node<T> {
      value: T,
      next: Option<Box<Node<T>>>,
  }
  ```
