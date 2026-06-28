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

### Cross-compiler (cc-x64) — code layout / alignment (QUEUED, not started)

Established Jun 2026 (Cascade Lake): cc-x64's *codegen* already matches gcc on
`benchmark_core` — at its best `.text` alignment phase it hits 1.08s == the gcc
slow32-fast baseline. The residual benchmark difference is an **alignment-phase
lottery worth ~20%** (1.08s..1.30s across offsets; layout:noise ≈ 25:1),
mechanism = DSB (µop-cache) packing of the hot `di->handler()` dispatch loop.
Measure anything in this area with `selfhost/tools/layout-sweep.sh` and compare
MEDIANS across offsets — single-build A/B is dominated by layout luck. Three
data points prove hand-emitting alignment in cc-x64 is a dead end (16B
loop-align and branch if-conversion both measured net-negative and were
reverted).

1. **Is gcc clever or lucky? (cheap, do first)** The "we match gcc" claim
   compares our sweep BEST to gcc's SINGLE build. Sweep gcc's slow32-fast too
   (pad its `.text`, e.g. via a linker offset) and compare gcc's spread/median
   to ours. If gcc's spread ≈ ours (~20%), gcc just rolled well and we genuinely
   match; if gcc's spread is small, gcc has real alignment robustness to learn
   from — most likely from `-falign-functions`/`-falign-loops` + the GNU
   assembler's **Jcc-erratum mitigation** (pad so branches don't straddle 32B
   boundaries; `-mbranches-within-32B-boundaries`). cc-x64 does neither beyond
   16B function alignment.

2. **Jcc-erratum-aware branch padding (mid effort, if #1 says gcc is robust).**
   A *targeted* transform — pad individual branches off 32B boundaries — is
   bounded and principled, unlike the blanket loop-top padding that failed.
   Potentially most of the win for far less than a full layout pass.

3. **Post-link BOLT-style layout pass (large, the robust answer).** The only
   thing that captures the ~20% deterministically instead of by alignment luck,
   and "stops the dice" for all future codegen work. Now the single biggest
   lever on the benchmark. The `layout-sweep.sh` harness already validates such
   work.

## Testing Recommendations

1. Test multi-file linking with global symbols ✅ (`feature-multifile`)
2. Test negative offsets in data access ✅ (`feature-negative-offset`)
3. Test loops with complex control flow ✅ (`feature-control-flow`)
4. Test all intrinsics with edge cases (0 size, odd sizes, etc.) ✅ (`stdlib-string-edge`)
5. Add string function regression tests (strlen, strcpy, memcpy, etc.) ✅ (`stdlib-string-funcs`)
6. Add stdlib regression tests (qsort, strtol, realloc) ✅ (`stdlib-qsort`, `stdlib-strtol`)
