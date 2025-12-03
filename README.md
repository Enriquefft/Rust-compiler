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
- Keep early scope focused: simple optimizations (constant folding, DCE, CFG simplification), inherent impls without traits, and x86-64 as the initial backend target.

## Language subset

Key capabilities supported by the compiler align with the documented Rust-like subset:

- **Core items:** functions, structs with fields, type aliases, const items, and inherent \`impl\` blocks. The compilation unit is a crate containing a sequence of items, with \`fn main()\` serving as the entry point.
- **Types:** primitive integers (\`i32\`, \`i64\`, \`u32\`, \`u64\`, \`usize\`), floats (\`f32\`, \`f64\`), booleans, \`char\`, and string-related types (\`str\`, \`String\`), plus arrays \`[T; N]\`, references \`&T\` / \`&mut T\`, raw pointers, function types, and generic paths with type arguments.
- **Expressions and statements:** arithmetic and logical operators, explicit casts with \`as\`, assignments (including compound assignments) to place expressions, conditionals, ranges (\`a..b\`, \`a..=b\`), \`while\` and \`for\` loops, and \`return\` with optional values. Expression statements and empty statements are supported. **Note:** \`break\` and \`continue\` are intentionally omitted from this subset.
- **Bindings and patterns:** \`let\` bindings with optional mutability and type annotations, identifier or wildcard patterns, and block expressions that yield a value when the final expression omits a semicolon.
- **Functions, methods, and generics:** functions and inherent methods accept by-value or reference parameters (\`self\`, \`&self\`, \`&mut self\`), support generics on functions/structs/impls, and allow generic arguments in paths. Trait bounds are out of scope for this subset.
- **Closures and macros:** closure expressions are first-class and typed via function types; macro call syntax (e.g., \`println!\`) is parsed but treated as a plain function call without hygienic expansion.
- **Unsafe blocks:** \`unsafe { ... }\` blocks are syntactically supported and parsed as expressions.

## Grammar reference

The language grammar is formalized in \`grammar.ebnf\`. Highlights include identifier rules, literals, type forms (arrays, references, pointers, function types, paths with generics), item definitions (\`fn\`, \`struct\`, \`impl\`, \`type\`, \`const\`), statements, expressions (with operator precedence), and closure syntax. Use this file as the authoritative syntactic reference when extending the parser or adding new language constructs.

> **Note:** Raw identifiers (\`r#\` prefix) are defined in the Rust specification but are **not implemented** in this compiler. The grammar has been updated to reflect this.

## Compiler architecture

The compiler is structured as staged transformations with clear module boundaries:

1. **Lexing and parsing:** convert source text into tokens and an AST following \`grammar.ebnf\`.
2. **HIR (High-Level IR):** resolve names, annotate types, and validate constructs.
3. **MIR (Mid-Level IR):** build basic-block-based, non-SSA IR suitable for analysis and optimization.
4. **MIR passes:** apply constant folding, dead code elimination, and CFG simplification.
5. **x86-64 backend:** lower MIR to machine IR, perform register allocation, and emit assembly output.

Modules are organized under \`frontend/\`, \`hir/\`, \`mir/\`, \`backend/\`, and \`diag/\` namespaces. See \`Architecture.md\` for detailed module descriptions.

## Repository layout

Current directory structure:

- \`src/main.zig\` and \`src/driver.zig\`: entry point and compiler driver.
- \`src/diag/\`: source mapping and diagnostics utilities.
- \`src/frontend/\`: tokens, lexer, AST, AST printer, and parser.
- \`src/hir/\`: HIR representation, HIR printer, name resolution, and type checking.
- \`src/mir/\`: MIR representation, lowering, MIR printer, and optimization passes.
- \`src/backend/\`: backend entry point and x86-64 code generation (machine IR, instruction selection, register allocation, emitter).
- \`codes/\`: sample programs and test inputs.

## Building and running

The project uses Zig's build system. Common workflows:

- Build the compiler executable:
  \`\`\`bash
  zig build
  \`\`\`
- Run the compiler with arguments:
  \`\`\`bash
  zig build run -- <file.rs>
  \`\`\`
- Run the compiler with debugging output:
  \`\`\`bash
  zig build run -- --print-tokens --print-ast --print-hir --print-mir <file.rs>
  \`\`\`

Available compiler options:
- \`--print-tokens\`: display lexer output
- \`--print-ast\`: display parsed AST
- \`--print-hir\`: display HIR after name resolution and type checking
- \`--print-mir\`: display MIR after optimization passes
- \`--print-passes-changes\`: show MIR state after each optimization pass
- \`--opt=none|basic\`: optimization level (default: \`basic\`)
- \`--emit=asm|obj\`: output format (default: \`asm\`)
- \`-o <path>\`: output file path

The build script (\`build.zig\`) exposes standard target/optimization options and installs the \`rust-compiler\` executable by default.

## Testing

Unit tests are wired through Zig's test runner:

\`\`\`bash
zig build test
\`\`\`

This target compiles and runs tests for the main binary and module-level test blocks. Tests are implemented as inline Zig \`test\` blocks within source modules (e.g., lexer tests, parser tests, HIR lowering tests, driver integration tests).

Sample programs in the \`codes/\` directory can be used for manual testing and validation.

## Documentation index

- \`Features.md\`: detailed language subset description and examples.
- \`grammar.ebnf\`: formal grammar specification.
- \`Architecture.md\`: compiler architecture, modules, and implementation roadmap.
- \`AGENTS.md\`: agent-specific instructions for automated tools.

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
