# src/ — Canonical live sources for the active compiler generation

This directory holds the single copy of the shared compiler frontend and
common HIR infrastructure used by:

- `stage08/` (the current self-hosting SLOW-32 native compiler)
- `stage08-cross-x64/`
- `stage08-cross-a64/`

## What lives here

- Lexer + parser + sema + AST (`c_lexer*`, `ast.h`, `parser.h`, `sema.h`, ...)
- Preprocessor (`pp.h`) and the AST-level optimizer (`optimize.h`)
- Common HIR/SSA/optimizer pieces (`hir*.h` except target-specific backends)
- Generation script for the Ragel lexer (`gen_lexer.sh`, `c_lexer.rl`)
- `libc/`: the target-independent stage08 libc sources (`stdio.c`, `malloc.c`,
  `convert.c`, ...), linked file by file from `stage08/libc/`
- `tools/`: the kit's `s32-as`, `s32-ar`, `s32-ld`, `slow32dis`, `slow32dump`
  sources; `stage08/tools` is a symlink to this directory

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

## No new stage snapshots

Stages are repaired in place: a fix to the compiler lands in `src/` (and so
in stage08 and the cross trees), and an older stage is changed only where
it is itself wrong.  There is no stage09 cut from stage08.  Each stage
records what it had to overcome.

## Why this reduces duplication

Before: the large shared sources (~15-20k lines of C + headers) lived in full
in every recent `stage0N/`, plus being redundantly symlinked into the two cross
trees.

After: one canonical copy under `src/`. The active development trees
(stage08 + crosses) stay small and stay in sync automatically. Historical
snapshots remain full for the record.
