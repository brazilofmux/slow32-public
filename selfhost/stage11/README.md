# Stage 11: s32-cc Evolved

s32-cc with new C features added on top of stage10. Self-hosting proven.
Also builds a new assembler from s32-as.c using the evolved compiler.

## New Features (over stage10)

- `goto` and labels
- `#include "file.h"` (resolves relative to source directory)
- `#ifdef` / `#ifndef` / `#else` / `#endif`
- `static const int x = N` treated as compile-time constant
- Larger buffers: P_MAX_OUT=1MB, 512 globals/functions/defines

## Bootstrap

Two-phase build required because stage10's 524KB output buffer can't hold
stage11's ~559KB asm output:
1. Build a "boot" compiler (stage10 code with P_MAX_OUT bumped to 1MB)
2. Boot compiler compiles stage11 source

Self-hosting: gen2.s == gen3.s (559KB asm).

## Assembler

Builds s32-as.c (port of stage05 assembler) using the evolved compiler.
Parity proven: byte-identical .s32o output vs stage05 assembler.
