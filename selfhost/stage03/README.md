# Stage 03: Compiler + Tools

The stage03 compiler is a restructured C compiler with separate lexer and parser passes,
evolved from cc-min (stage02). Compiled by cc-min, self-hosting proven.

Includes BNE+JAL long-branch switch codegen and increased limits
(P_MXCASE/P_MXLBL 512, P_LBNBUF 24576).

Also builds self-contained tools (assembler, archiver, linker) and includes
its own libc, crt0, and mmio runtime — making this stage a complete
self-sufficient toolchain that later stages can depend on without needing
stage02.

## Artifacts

- cc.s32x — stage03 compiler executable
- s32-as.s32x — assembler (compiled by stage03 compiler)
- s32-ar.s32x — archiver with symbol index (compiled by stage03 compiler)
- s32-ld.s32x — linker (compiled by stage03 compiler)

## Features (over cc-min)

- Split lexer (s32cc_lex.h) and parser (s32cc_parse.h) for cleaner code
- Same C subset as cc-min: structs, typedefs, sizeof, casts, #define, enum
- switch/case with long-branch safe codegen (BNE+JAL, not BEQ)

## Bootstrap

Uses pre-built tools from stage02:
- cc-min.s32x (stage02) to compile stage03 compiler
- s32-as.s32x (stage02) to assemble
- s32-ld.s32x (stage02) to link

Self-hosting: stage03 compiler compiles itself, gen2.s == gen3.s.

## Tests

- Lexer tests: keyword, identifier, number, string, char, operator, comment, mixed
- Parser tests: smoke, expression, control flow, pointer, mixed
- Self-hosting fixed-point proof
