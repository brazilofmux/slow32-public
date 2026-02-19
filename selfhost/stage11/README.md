# Stage 10: s32-cc C Compiler

s32-cc: a restructured C compiler with separate lexer and parser passes,
evolved from cc-min (stage08/09). Compiled by cc-min, self-hosting proven.

## Features (over cc-min)

- Split lexer (s32cc_lex.h) and parser (s32cc_parse.h) for cleaner code
- Same C subset as cc-min: structs, typedefs, sizeof, casts, #define, enum

## Bootstrap

Uses pre-built tools from earlier stages:
- cc-min.s32x (stage09) to compile s32-cc
- s32-as.s32x (stage05) to assemble
- s32-ld.s32x (stage07) to link

Self-hosting: s32-cc compiles itself, gen2.s == gen3.s.

## Tests

- Lexer tests: keyword, identifier, number, string, char, operator, comment, mixed
- Parser tests: smoke, expression, control flow, pointer, mixed
- Self-hosting fixed-point proof
