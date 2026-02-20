# Stage 03: s32-cc C Compiler + Tools

s32-cc: a restructured C compiler with separate lexer and parser passes,
evolved from cc-min (stage02). Compiled by cc-min, self-hosting proven.

Includes BNE+JAL long-branch switch codegen and increased limits
(P_MXCASE/P_MXLBL 512, P_LBNBUF 24576).

Also builds self-contained tools (assembler, archiver, linker) and includes
its own libc, crt0, and mmio runtime — making this stage a complete
self-sufficient toolchain that later stages can depend on without needing
stage02.

## Artifacts

- s32cc.s32x — s32-cc compiler (compiled by stage02 cc-min)
- s32-as.s32x — assembler (compiled by s32cc)
- s32-ar.s32x — archiver with symbol index (compiled by s32cc)
- s32-ld.s32x — linker (compiled by s32cc)

## Features (over cc-min)

- Split lexer (s32cc_lex.h) and parser (s32cc_parse.h) for cleaner code
- Same C subset as cc-min: structs, typedefs, sizeof, casts, #define, enum
- switch/case with long-branch safe codegen (BNE+JAL, not BEQ)

## Bootstrap

Uses pre-built tools from stage02:
- cc-min.s32x (stage02) to compile s32-cc
- s32-as.s32x (stage02) to assemble
- s32-ld.s32x (stage02) to link

Self-hosting: s32-cc compiles itself, gen2.s == gen3.s.

## Tests

- Lexer tests: keyword, identifier, number, string, char, operator, comment, mixed
- Parser tests: smoke, expression, control flow, pointer, mixed
- Self-hosting fixed-point proof
