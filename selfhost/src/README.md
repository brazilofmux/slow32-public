# src/ — Canonical live sources for the active compiler generation

This directory holds the single copy of the shared compiler frontend and
common HIR infrastructure used by:

- `stage08/` (the current self-hosting SLOW-32 native compiler)
- `stage08-cross-x64/`
- `stage08-cross-a64/`

## What lives here

- Lexer + parser + sema + AST (`c_lexer*`, `ast.h`, `parser.h`, `sema.h`, ...)
- Preprocessor (`pp.h`, `optimize.h`)
- Common HIR/SSA/optimizer pieces (`hir*.h` except target-specific backends)
- Generation script for the Ragel lexer (`gen_lexer.sh`, `c_lexer.rl`)

## What does *not* live here (per-target or per-stage)

- Target-specific code generators and register allocators:
  - SLOW-32: `hir_regalloc.h`, `hir_codegen.h` (live in the stage dir)
  - x86-64: `hir_*_x64.h` + `hir_burg_x64.h`
  - AArch64: `hir_*_a64.h`
- The compiler driver (`s12cc.c`, `cc-x64.c`, `cc-a64.c`)
- Runtime / libc pieces that are environment specific (`crt0`, `syscalls`, `mmio_ring_*`)
- Stage-specific tests, builtins, build scripts, and Makefiles

## How sharing works

The consuming trees contain symlinks:

    stage08/ast.h             -> ../src/ast.h
    stage08-cross-x64/hir.h   -> ../src/hir.h
    ...

Build systems list the symlinks (or the src/ paths) in their dependency
variables (CC_DEPS). Make and the compilers see the content through the links.

When the frontend evolves, edit the file in `src/`. All live consumers see it
immediately.

## Historical stages

`stage03/` through `stage07/` contain full, self-contained copies of the sources
as they existed for that bootstrap step. This preserves exact reproducibility:
the bytes in `stage07/parser.h` are the ones that were fed to stage06's
compiler to produce stage07's toolchain.

Do not replace historical stage contents with symlinks.

## Creating a new stage snapshot

When cutting a new stage (e.g. stage09 from stage08):

1. `mkdir -p selfhost/stage09`
2. Either:
   - Copy the thin view (`cp -R stage08/* stage09/`) — symlinks are preserved, or
   - Materialize for a fully standalone historical tree:
     `cp -RL src stage09/src` (or just rely on the symlinks under stage09/
      pointing at the then-current src/, or freeze by dereferencing).

3. Adjust `BOOTSTRAP_CC = ../stage08/cc.s32x` etc. in the new stage's Makefile.
4. Commit the new stage dir (with whatever materialization policy the project
   chooses for auditability).

## Why this reduces duplication

Before: the large shared sources (~15-20k lines of C + headers) lived in full
in every recent `stage0N/`, plus being redundantly symlinked into the two cross
trees.

After: one canonical copy under `src/`. The active development trees
(stage08 + crosses) stay small and stay in sync automatically. Historical
snapshots remain full for the record.
