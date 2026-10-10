# Stage 06: The Fixed-Point Cycle

stage06 is stage05 run through one more self-hosting cycle: the same
compiler shape (front end, HIR/SSA, BURG, graph-coloring allocation; see
`../stage05/README.md`), seeded from stage05 and built by stage05's tools,
with the `--fixed-point` gate -- gen1 -> gen2 -> gen3, gen2 == gen3 --
available in its test runner.  `../docs/STAGE6-FIXEDPOINT.md` is the
rationale.

What is new over stage05: `include/` (the stage's own `<ctype.h>`,
`<stdbool.h>`, `<stddef.h>`, `<stdint.h>`, `<stdlib.h>`, `<string.h>`),
`builtins_fp64.s` (64-bit floating-point helpers on register pairs, the
first floating point in the chain; `../docs/FLOATING-POINT.md`), and
`compare-llvm-selfhost.sh`, which compiles the programs in
`tests/codegen_compare.list` with this compiler and with clang
(`-O2` by default) and compares what they print.  stage07 grows the headers
further; stage08 is the active head.

## Artifacts

- `cc.s32x` -- the stage06 compiler
- `s32-as.s32x`, `s32-ar.s32x`, `s32-ld.s32x`, `slow32dump.s32x`,
  `slow32dis.s32x` -- the tools (`tools/`), compiled by stage05's `cc.s32x`
  against this stage's libc
- `libc/`, `include/`, `crt0.s`, `mmio_no_start.s`, `builtins64.s`,
  `builtins_fp64.s`

## Bootstrap

- stage05 `cc.s32x`, `s32-as.s32x`, `s32-ld.s32x` build gen1 and the tools
  (`build-s12cc.sh`, `build-tools.sh`)
- gen1 recompiles the libc (HIR/SSA ABI, r11-r28 callee-saved)

## Tests

`run-tests.sh [--emu <path>] [--keep-artifacts] [--fixed-point] [--strict-shape]`
-- the same five steps as stage05's (bootstrap; gen1 and its libc; the phase
tests through `test_phase26.c` plus `compare_switch_dense.c` and
`compare_branch_mix.c`, the equivalence smoke and the optimizer pack against
stage05; tools; end-to-end), with `--fixed-point` for gen2 == gen3.

`run-abi-conformance.sh` -- stage03's calling-convention checks built with
this compiler and this stage's assembler and linker.
`compare-llvm-selfhost.sh [--keep] [tests...]` -- clang vs this compiler on
the codegen list (needs `~/llvm-project/build/bin/clang`, or `LLVM_BIN`).
`investigate-gen3.sh` -- the gen3-hang probe recipe, as in stage05.
