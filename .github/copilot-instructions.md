# Copilot Instructions

This document provides custom instructions for GitHub Copilot when working with this repository.

## Project Overview

This repository implements a Rust-subset compiler in Zig. For detailed information, refer to:

- **[Architecture.md](../Architecture.md)** - High-level architecture, stages, module expectations, and data flows between lexer, parser, HIR/MIR, optimization passes, and x86-64 backend components.
- **[Features.md](../Features.md)** - Supported language features and semantic boundaries. Align new grammar or semantic changes with this document.
- **[grammar.ebnf](../grammar.ebnf)** - The canonical grammar definition. Parser changes should trace back to this file.
- **[README.md](../README.md)** - Project overview, building instructions, and documentation index.
- **[AGENTS.md](../AGENTS.md)** - Additional agent-specific instructions.

## Coding Conventions

- **Language**: Zig. Follow idiomatic Zig formatting (`zig fmt`).
- **Error handling**: Prefer Zig error unions. User-facing issues should report via the diagnostics pipeline rather than panicking.
- **Data ownership**: Respect the arena/allocation model (arenas for AST/HIR/MIR nodes; allocator passed explicitly).
- **Naming**: Mirror module and type names from `Architecture.md` when adding new files (e.g., `frontend/parser.zig`, `mir/passes/const_fold.zig`).

## Building and Testing

```bash
# Build the compiler
zig build

# Run tests
zig build test

# Run the compiler
zig build run -- <compiler-args>

# Format code
zig fmt src/
```

## Key Directories

- `src/main.zig` and `src/driver.zig` - Entry point and compiler driver
- `src/frontend/` - Tokens, lexer, AST, and parser
- `src/hir/` - HIR representation, name resolution, and type checking
- `src/mir/` - MIR representation, builder, and optimization passes
- `src/backend/` - x86-64 backend components
- `src/diag/` - Source mapping and diagnostics utilities
- `src/all_tests.zig` - Aggregated test runner

## When Making Changes

1. Keep `Architecture.md`, `Features.md`, and `grammar.ebnf` in sync with behavior changes.
2. Run `zig fmt` before committing Zig source changes.
3. Add or update tests in `src/all_tests.zig` when adding new modules.
4. Use descriptive commit messages referencing the affected compiler stage (e.g., "frontend: improve parser recovery").
