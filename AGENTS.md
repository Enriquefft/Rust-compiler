# AGENTS Instructions

## Scope
These instructions apply to the entire repository unless a more specific `AGENTS.md` is added in a subdirectory.

## Project Overview
- This repository implements a Rust-subset compiler in Zig. High-level architecture, stages, and module expectations are detailed in `Architecture.md`. Use it as the authoritative guide for responsibilities and data flows between lexer, parser, HIR/MIR, optimization passes, and x86-64 backend components.
- Supported language features and semantic boundaries are outlined in `Features.md`. Align new grammar or semantic changes with this document to stay consistent with the intended subset (e.g., no break/continue, macros parsed but treated as calls).
- The canonical grammar is defined in `grammar.ebnf`. Parser changes should trace back to this file; update both the grammar and feature descriptions together when behavior changes.

## Coding and Design Conventions
- Language: Zig. Follow idiomatic Zig formatting (`zig fmt`) and avoid wrapping imports with try/catch-style constructs (imports should fail loudly during build).
- Error handling: prefer Zig error unions; user-facing issues should report via the diagnostics pipeline rather than panicking.
- Data ownership: respect the arena/allocation model sketched in `Architecture.md` (arenas for AST/HIR/MIR nodes; allocator passed explicitly).
- Naming: mirror module and type names from `Architecture.md` when adding new files (e.g., `frontend/parser.zig`, `mir/passes/const_fold.zig`). Keep enums/IDs consistent with documented names.

## Testing and Tooling
- Default builds use Zig. When adding or modifying code, run `zig test` or `zig build test` if tests exist. If the repository introduces a specific build target, prefer `zig build`.
- Add regression tests alongside new features when possible (use `tests/` once it exists). If no automated tests apply, describe manual validation steps in commits/PRs.
- Apply `zig fmt` before committing Zig source changes.

## Documentation Expectations
- Keep `Architecture.md`, `Features.md`, and `grammar.ebnf` in sync with behavior changes. Update relevant sections when altering the language, pipeline stages, or runtime assumptions.
- Include concise doc comments for public APIs in new modules, especially for compiler stages and shared utilities.

## Git and PR Guidance
- Keep commits focused and descriptive. Reference the affected compiler stage (e.g., "frontend: improve parser recovery").
- When preparing a PR message, summarize the compiler-stage impact, mention updated docs, and list tests executed. Use bullet points and keep the tone concise.

## Placeholders / Open Items
- Add a dedicated testing section once automated test harnesses or sample programs are introduced.
- Document allocator choices and diagnostic formatting once implemented to ensure contributors follow the chosen conventions.
