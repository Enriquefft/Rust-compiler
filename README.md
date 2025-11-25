# Rust-Subset Compiler (Zig)

This repository hosts a Zig-based compiler for a Rust-like language subset. The project combines a well-scoped grammar with a staged compilation pipeline that lowers high-level Rust-like constructs to x86-64 assembly. Use this README as an entry point to the language features, grammar, architecture, and developer workflows documented elsewhere in the repository.

<!--toc:start-->

- [Rust-Subset Compiler (Zig)](#rust-subset-compiler-zig)
  - [Project goals](#project-goals)
  - [Language subset](#language-subset)
  - [Grammar reference](#grammar-reference)
  - [Compiler architecture](#compiler-architecture)
  - [Repository layout](#repository-layout)
  - [Building and running](#building-and-running)
  - [Testing](#testing)
  - [Documentation index](#documentation-index)
  - [Authors](#authors)
  - [Bibliography](#bibliography)
  <!--toc:end-->

## Project goals

- Implement a Rust-like language defined by the accompanying EBNF grammar and feature list.
- Provide an end-to-end pipeline from source code through lexing, parsing, HIR/MIR construction, optimization, and x86-64 code generation.
- Deliver helpful diagnostics and a maintainable modular codebase written in Zig.
- Keep early scope focused: simple optimizations (constant folding, DCE, CFG simplification), inherent impls without traits, and x86-64 as the initial backend target.【F:Architecture.md†L1-L69】

## Language subset

Key capabilities supported by the compiler align with the documented Rust-like subset:

- **Core items:** functions, structs with fields, type aliases, and inherent `impl` blocks. The compilation unit is a crate containing a sequence of items, with `fn main()` serving as the entry point.【F:Features.md†L6-L33】
- **Types:** primitive integers (`i32`, `i64`, `u32`, `u64`, `usize`), floats, booleans, `char`, and string-related types, plus arrays `[T; N]`, references `&T` / `&mut T`, raw pointers, function types, and generic paths with type arguments.【F:Features.md†L35-L81】
- **Expressions and statements:** arithmetic and logical operators, explicit casts with `as`, assignments (including compound assignments) to place expressions, conditionals, ranges (`a..b`, `a..=b`), `while` and `for` loops, and `return` with optional values. Expression statements and empty statements are supported; `break` and `continue` are intentionally omitted.【F:Features.md†L83-L143】
- **Bindings and patterns:** `let` bindings with optional mutability and type annotations, identifier or wildcard patterns, and block expressions that yield a value when the final expression omits a semicolon.【F:Features.md†L117-L143】
- **Functions, methods, and generics:** functions and inherent methods accept by-value or reference parameters, support generics on functions/structs/impls, and allow generic arguments in paths. Trait bounds are out of scope for this subset.【F:Features.md†L145-L207】
- **Closures and macros:** closure expressions are first-class and typed via function types; macro call syntax is parsed but treated as a plain function call without hygienic expansion.【F:Features.md†L209-L249】

## Grammar reference

The language grammar is formalized in `grammar.ebnf`. Highlights include identifier rules (supporting raw identifiers), literals, type forms (arrays, references, pointers, function types, paths with generics), item definitions (`fn`, `struct`, `impl`, `type`), statements, expressions (with operator precedence), and closure syntax. Use this file as the authoritative syntactic reference when extending the parser or adding new language constructs.【F:grammar.ebnf†L1-L99】【F:grammar.ebnf†L101-L199】

## Compiler architecture

The compiler is structured as staged transformations with clear module boundaries:

1. **Lexing and parsing:** convert source text into tokens and an AST following `grammar.ebnf`.
2. **HIR (High-Level IR):** resolve names, annotate types, and validate constructs.
3. **MIR (Mid-Level IR):** build basic-block-based, non-SSA IR suitable for analysis and optimization.
4. **MIR passes:** apply constant folding, dead code elimination, and CFG simplification.
5. **x86-64 backend:** lower MIR to machine IR, perform register allocation, peephole optimizations, and emit assembly or object output.

Modules are organized under `frontend/`, `hir/`, `mir/`, `backend/`, and `util/` namespaces, with dedicated files for diagnostics, interning, arenas, CFG helpers, and backend components like ABI handling, instruction selection, register allocation, and emission.【F:Architecture.md†L11-L118】【F:Architecture.md†L120-L190】

## Repository layout

Planned directory structure mirrors the architecture description:

- `src/main.zig` and `src/driver.zig`: entry point and compiler driver.
- `src/diag/`: source mapping and diagnostics utilities.
- `src/frontend/`: tokens, lexer, AST, and parser.
- `src/hir/`: HIR representation, name resolution, and type checking.
- `src/mir/`: MIR representation, builder, and optimization passes.
- `src/backend/x86_64/`: ABI, instruction selection, register allocation, peephole, and emitter modules.
- `src/util/`: arenas, interner, and CFG helpers.
- `tests/`: lexer, parser, HIR, MIR, backend, and execution tests (golden files and runtime checks).【F:Architecture.md†L71-L188】

## Building and running

The project uses Zig’s build system. Common workflows:

- Build the compiler executable:
  ```bash
  zig build
  ```
- Run the compiler with arguments:
  ```bash
  zig build run -- <compiler-args>
  ```
- Generate module documentation for the lexer and parser (tests configured to emit docs):
  ```bash
  zig build docs
  ```

The build script (`build.zig`) exposes standard target/optimization options and installs the `rust-compiler` executable by default.【F:build.zig†L1-L73】

## Testing

Unit tests are wired through Zig’s test runner:

```bash
zig build test
```

This target compiles and runs tests for the main binary and module-level test blocks (including lexer and parser documentation tests). Extend the `tests/` directories with golden files and runtime checks as outlined in the architecture document.【F:build.zig†L55-L67】【F:Architecture.md†L190-L257】

## Documentation index

- `Features.md`: detailed language subset description and examples.
- `grammar.ebnf`: formal grammar specification.
- `Architecture.md`: compiler architecture, modules, and implementation roadmap.

Refer to these documents when modifying the grammar, extending language features, or implementing pipeline stages to ensure consistency across the compiler.

## Authors

<table>
    <tr>
        <th style="width:24%;">Enrique Flores</th>
    </tr>
    <tr>
        <td><a href="https://github.com/Enriquefft"><img src="https://avatars.githubusercontent.com/u/60308719?v=4"></a></td>
    </tr>
</table>

## Bibliography

