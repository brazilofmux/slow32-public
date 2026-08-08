# SLOW32 Backend Status

Last Reviewed: 2026-08-08

## Overview
- Backend tracks LLVM main; `llc` and `clang` build with SLOW32 as the default
  triple (`slow32-unknown-none`).
- SelectionDAG, MC, frame lowering, and the clang driver toolchain are exercised
  by lit tests under `llvm/test/CodeGen/SLOW32/` plus
  `clang/test/Driver/slow32-toolchain.c`.
- Integration patches and a backend mirror live in the `slow-32` repo
  (`llvm-backend/`); regenerate with `scripts/generate-patches.sh` +
  `scripts/backup.sh` after rebases or backend edits.

## Architecture
- 32-bit little-endian RISC, 32 GPRs: `r0`=zero, `r2` reserved (long-branch
  scratch / MC relaxation), `r29`=sp, `r30`=fp, `r31`=lr.
- Base + signed 12-bit offset addressing; stack grows down.
- Data layout (clang + backend):  
  `e-m:e-p:32:32-i8:8:32-i16:16:32-i32:32:32-i64:32:32-f32:32:32-f64:32:32-n8:16:32-S32`
- Frame lowering requests 16-byte stack alignment; layout string still says `S32`
  (safe over-align; unify later if ABI tooling cares).
- Single-hart only: no interrupts, no threads. Control leaves the guest only via
  voluntary `YIELD` to the host. Atomics expand to plain loads/stores.

## Working
- Core SelectionDAG + MC plumbing, prologue/epilogue: LR saved when the
  function has calls; FP only when required (var-sized objects, frameaddress,
  realignment, etc.). Neither is in the CSR list.
- Integer/logic/shift/memory, mul/mulh, native signed/unsigned branches.
- Call/return ABI with r1/r2 and r3–r10 pairs; sret demotion for oversized
  returns; varargs including f64 register/stack straddle.
- Globals/JT/CPI/blockaddr via `LOAD_ADDR` → `%hi`/`%lo` (LUI+ADDI).
- Long-branch relaxation (machine + MC); PostRA expansion stamps MO_HI/MO_LO
  on MBB operands; MC lowering wraps `%hi`/`%lo` for the external assembler.
- f32/f64 in GPRs / GPRPair; NaN-correct compares; i64↔fp via FCVT_* pairs.
- Emulated TLS (`__emutls_get_address`); single-hart atomics (NotAtomic).
- C23 `_BitInt` with wide div/rem/fp-convert expand limits at 64 bits.
- Clang target + driver toolchain (slow32asm / s32-ld / mmio vs debug-io).

## Partially Working / Soft Spots
- Switches: chained compares work; jump tables expand (`BR_JT` → Expand) but
  lack dedicated optimisations / tests.
- Integer `SELECT` is always branchless (`mask = -cond`); no size-tuned path.
- Feature flags (`+m`/`+f`/`+a`) exist but do not gate isel legality yet.
- Dual libcall registration (TargetLowering + Subtarget) kept in sync via a
  shared helper; revisit when upstream drops one of the APIs.

## Known Pitfalls (fixed or documented)
- ~~MBB long-branch ops dropped `%hi`/`%lo`~~ — fixed 2026-08-08.
- ~~`(shl imm, 16) → LUI`~~ — removed (LUI is `<<12`).
- ~~Duplicate signed `extload` patterns~~ — removed.
- ~~R30/R31 double-spilled via CSR + prologue~~ — CSR list no longer includes them.
- ~~Dead ADDC/ADDE/SUBC/SUBE custom-inserter stubs~~ — removed.
- Handwritten assembly that triggers MC branch relaxation must treat `r2` as
  clobbered (relaxation uses it as scratch).

## Regression Tests
```
./build/bin/llvm-lit -v llvm/test/CodeGen/SLOW32/
./build/bin/llvm-lit -v clang/test/Driver/slow32-toolchain.c
```
Coverage includes addressing, branches (PC+4 fixups), long-branch `%hi`/`%lo`,
varargs/f64 straddle, atomics+emutls, bitint, large frames, extload zext,
elf relocs, and fp object encoding.

## Future Opportunities
- Size-tuned SELECT under `-Os` (branchless mask is always used today).
- Gate mul/div/FP isel on subtarget features for a real `slow32-minimal`.
- Align data-layout stack alignment (`S32` vs 16-byte frame preference).
- Jump-table / computed-goto stress tests; CFI smoke if debugging soft-core code.
