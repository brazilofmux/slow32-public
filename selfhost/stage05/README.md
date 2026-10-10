# Stage 05: HIR, SSA, BURG and a Register Allocator

The stage05 compiler is stage04's front end with the code-quality cycle
behind it: the AST is lowered to a high-level IR (`hir.h`, `hir_lower.h`),
put into SSA (`hir_ssa.h`), optimized (`hir_opt.h`: folding, dead code,
copy propagation; `hir_licm.h`: loop-invariant code motion), selected with
BURG tree patterns (`hir_burg.h`, audit and batch plan in
`BURG_MIGRATION.md`), register-allocated by graph coloring
(`hir_regalloc.h`) and emitted by `hir_codegen.h`.  The tree-walk
`codegen.h` remains as the bootstrap path.

The ABI changes here: r11-r28 are callee-saved under the HIR/SSA code
generator, so a program compiled by this compiler must link against a libc
compiled by it, not stage04's.  `build-s12cc.sh` is therefore two-phase:
stage04's `cc.s32x` builds gen1 (tree-walk ABI inside, HIR/SSA ABI in what it
emits), and gen1 recompiles the libc.

## Artifacts

- `cc.s32x` -- the stage05 compiler
- `s32-as.s32x`, `s32-ar.s32x`, `s32-ld.s32x`, `slow32dump.s32x`,
  `slow32dis.s32x` -- the tools (`tools/`), compiled by stage04's `cc.s32x`
  against this stage's libc (`build-tools.sh`)
- `libc/`, `crt0.s`, `mmio_no_start.s`, `builtins64.s` (64-bit integer
  helpers)

## Bootstrap

- stage04 `cc.s32x`, `s32-as.s32x`, `s32-ld.s32x` build gen1 and the tools
- gen1 recompiles the libc (HIR/SSA ABI)

## Tests

`run-tests.sh [--emu <path>] [--keep-artifacts] [--fixed-point] [--strict-shape]`:

1. bootstrap: runtime and libc
2. gen1: this stage's `s12cc.c` compiled by the checked-in `cc.s32x` (a
   self-rebuild), then the libc recompiled by gen1
3. the phase tests (`tests/test_phase2.c` .. `test_phase24.c`, `test_short.c`,
   `test_corpus.c`) compiled by gen1, run and checked; a semantic-equivalence
   smoke and an optimizer regression pack, checked-in `cc.s32x` against gen1
   (`--fixed-point` adds gen1 -> gen2 -> gen3, gen2 == gen3)
4. the tools rebuilt, then an end-to-end toolchain run

`run-abi-conformance.sh` -- stage03's calling-convention checks built with
this compiler and this stage's assembler and linker.
`benchmark.sh` -- compiler throughput on synthetic inputs (O(n) check) and
self-compilation timing.  `investigate-gen3.sh` -- the probe used when gen3
hung; kept as the recipe.
