# SLOW-32 Improvement Suggestions

This document consolidates feedback and improvement suggestions for the SLOW-32 toolchain components.

## Resolved Issues

### Assembler (all resolved)

- **Register names**: Case-insensitive via `tolower()` in `parse_register()` ✅
- **`.word symbol`**: REL_32 relocations for jump tables/data pointers ✅
- **Auto-align in `.text`**: `bump_size()` for uniform section accounting ✅
- **S-format parsing**: Explicit `rs2` handling with multiple addressing modes ✅
- **`%lo(symbol)`**: Correctly marked and applied in loads/stores ✅

### LLVM Backend (all resolved)

- **Switch/jump tables**: Custom `LowerJumpTable()`, assembler REL_32, linker support ✅
- **Address formation / GEP scaling**: `SelectAddr()` handles global+offset, 12-bit constraints ✅
- **Long branches**: R2 reserved for materialization, pseudo instructions expanded ✅

### Toolchain (all resolved)

1. **64-bit Division** ✅ — `UMUL_LOHI` custom lowering verified correct.
2. **Linker Error Handling** ✅ — Hard errors on unresolved symbols (exit code 1).
3. **REL_CALL Implementation** ✅ — Implemented in `s32-ld.c`.
4. **Linker Range Checks** ✅ — LO12 signed ±2048, BRANCH +4094, JAL +1,048,574.
5. **Printf Formatting** ✅ — `%llu` and other formats verified.
6. **Runtime Safety & Optimization** ✅ — `memset` beq-only loops, segregated free list malloc.
7. **Emulator Safety** ✅ — W^X unified across all emulators, respects header flag.
8. **Shared Loader** ✅ — Callback-based `s32x_loader.h` used by all emulators.
9. **IEEE 754 Floating-Point** ✅ — Native f32/f64 instructions across entire toolchain.

### Runtime Libraries (all resolved)

1. **String-float conversions** ✅ — dtoa/strtod implemented.
2. **Math library transcendentals** ✅ — sin, cos, exp, log, etc. implemented via DBT/QEMU runtime interception.

## Open Items

### Cross-compiler (cc-x64) — code layout / alignment robustness

**#1 ANSWERED (Jun 2026, Cascade Lake): gcc is CLEVER, not lucky.** Two sweeps
at REPS=15 (`selfhost/tools/{layout-sweep.sh,gcc-layout-sweep.sh}`):

| build              | spread | range / median        |
|--------------------|--------|-----------------------|
| cc-x64 s32fast-hir | 19.6%  | 1.07–1.28s, med ~1.23 |
| gcc slow32-fast    |  1.0%  | 1.05–1.06s, med ~1.06 |

Walked through all 16 alignment phases, gcc stays ~1.06s — robustly well-packed.
cc-x64 swings 20% and only equals gcc at its lucky phases (pad ≡48 mod 64 →
1.08s). So cc-x64's instruction *selection* can match gcc, but it lacks gcc's
alignment *robustness*; the real median gap (~1.23 vs ~1.06) is alignment.
Mechanism: DSB (µop-cache) packing of the hot `di->handler()` dispatch loop;
gcc's `-falign-functions/-loops/-jumps` (default at -O2/O3) keep it well-packed
regardless of `.text` base, cc-x64 emits no such padding.
(Harness note: gcc -O3 -freorder-functions puts `main` + the inlined loop in
`.text.startup`; gcc-layout-sweep.sh pads that section so the loop actually
moves. Always measure layout changes by the sweep MEDIAN, never one build.)

**RETRACTED:** the earlier "hand-emitting alignment in cc-x64 is a dead end"
(loop-align +16B and branch if-conversion reverts) was judged by SINGLE-BUILD
A/B — the exact contaminated measurement the sweep exists to replace. gcc's 1.0%
spread proves alignment works; the verdict is void until re-tested via sweep.

1. **Function / hot-loop alignment in cc-x64 (do first — now the primary lever).**
   Replicate gcc's `-falign-*`: align hot-loop heads / function entries so the
   dispatch loop lands in a good DSB phase deterministically. Validate by running
   `layout-sweep.sh stage08-cross-x64` on the aligned build and checking the
   MEDIAN drops toward gcc's ~1.06 and the spread collapses toward ~1% — NOT by a
   single build (that's what mis-killed it before). Cheap if it works; bounded.

2. **Jcc-erratum-aware branch padding (if #1 underperforms).** Targeted: pad
   individual branches off 32B boundaries (`-mbranches-within-32B-boundaries`
   analogue). gcc's assembler does this on affected -march.

3. **Post-link BOLT-style layout pass (large, fully-robust fallback).** Captures
   placement deterministically for all future codegen work; heavier than #1/#2.

## Testing Recommendations

1. Test multi-file linking with global symbols ✅ (`feature-multifile`)
2. Test negative offsets in data access ✅ (`feature-negative-offset`)
3. Test loops with complex control flow ✅ (`feature-control-flow`)
4. Test all intrinsics with edge cases (0 size, odd sizes, etc.) ✅ (`stdlib-string-edge`)
5. Add string function regression tests (strlen, strcpy, memcpy, etc.) ✅ (`stdlib-string-funcs`)
6. Add stdlib regression tests (qsort, strtol, realloc) ✅ (`stdlib-qsort`, `stdlib-strtol`)
