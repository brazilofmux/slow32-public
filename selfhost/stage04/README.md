# Stage 04: Compiler with a Real Front End

The stage04 compiler (`cc.s32x`, source `s12cc.c` and the headers beside it)
replaces stage03's single-pass emit-as-you-go design with separate passes:
a Ragel `-G2` scanner (`c_lexer.rl`, generated into `c_lexer_gen.c` by
`gen_lexer.sh`), a recursive-descent parser building an AST (`parser.h`,
`ast.h`), a preprocessor (`pp.h`), semantic analysis and type computation over
the tree (`sema.h`), and a tree-walk code generator (`codegen.h`,
`optimize.h`).  `PLAN.md` is the architecture it was built against and the
phase list; the HIR/SSA half of that plan lands in stage05.

Compiled by stage03's `cc.s32x`, assembled and linked by stage03's tools
(`build-s12cc.sh`); its own tools are then compiled by itself
(`build-tools.sh`).  Self-hosting is proven in the stage walk.

## Artifacts

- `cc.s32x` -- the stage04 compiler
- `s32-as.s32x`, `s32-ar.s32x`, `s32-ld.s32x` -- assembler, archiver, linker
  (`s32-as.c`, `s32-ar.c`, `s32-ld.c`, compiled by stage04's `cc.s32x`)
- `libc/`, `crt0.s`, `mmio_no_start.s` -- the stage's own runtime

## Features (over stage03)

- Table-driven scanner, 68 token types (Ragel `-G2`); the parser no longer
  interleaves lexing with emission
- AST with a semantic pass: types, sizes and offsets computed once, then used
- Preprocessor as its own layer (`#include`, `#define`, conditionals)
- The same C subset as stage03 as the floor, grown through `tests/test_phase*.c`

## Bootstrap

- stage03 `cc.s32x` compiles `s12cc.c` and the libc; stage03 `s32-as.s32x` and
  `s32-ld.s32x` assemble and link
- stage04 `cc.s32x` then compiles the three tools in this directory

## Tests

`run-tests.sh [--emu <path>] [--keep-artifacts]` -- the stage04 gate the stage
walk runs:

1. bootstrap: runtime and libc built with this compiler
2. Duff's device (`tests/test_duff.c`): switch into a loop body
3. Ragel lexer smoke, then the full token tests (`tests/lex_test.c`)
4. compiler build and the phase tests `tests/test_phase2.c` ..
   `test_phase14.c`, each compiled, run and checked

`run-abi-conformance.sh` -- the SLOW-32 calling-convention checks
(stage03's script) built with this stage's compiler and stage03's assembler
and linker (run-stages: "[stage04] ABI conformance").

The shared front end that stage08 and the cross compilers use today lives in
`../src/`; stage04 keeps its own snapshot, as every historical stage does.
