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
  `e-m:e-p:32:32-i8:8:32-i16:16:32-i32:32:32-i64:32:32-f32:32:32-f64:32:32-n8:16:32-S128`
  (`S128` = 16-byte stack alignment, matching `SLOW32FrameLowering`).
- Single-hart only: no interrupts, no threads. Control leaves the guest only via
  voluntary `YIELD` to the host. Atomics expand to plain loads/stores.
- Subtarget features: `+m` (mul/div), `+f` (native soft-float in GPRs) on by
  default; `slow32-minimal` / `-mattr=-m,-f` force libcall soft paths.

## Working
- Core SelectionDAG + MC plumbing; optional FP (LR on calls; FP only when
  required). Neither FP nor LR is in the CSR list.
- Real `ADJCALLSTACK*` expansion (no reserved call frame) so stack args sit
  below LR/FP saves.
- Integer/logic/shift/memory; mul/mulh when `+m`; native signed/unsigned branches.
- Call/return ABI with r1/r2 and r3–r10 pairs; sret demotion; varargs f64 straddle.
- Tail calls (`SLOW32ISD::TAIL`): a call in tail position is `jal r0, sym`
  (or `jalr r0, r2, 0` through a pointer) after the epilogue, when every
  argument is in a register and there is no byval, no struct return, and
  the caller is not variadic.  A function whose only call is a tail call
  has no frame.  `musttail` is honoured or is a fatal error.
- Globals/JT/CPI/blockaddr via `LOAD_ADDR` → `%hi`/`%lo`; jump tables via
  Expand → load + `BRIND` (`jalr`).
- Long-branch relaxation PostRA-only (AsmPrinter asserts if a long-branch
  pseudo leaks through).
- f32/f64 when `+f`; NaN-correct compares; i64↔fp via FCVT_* pairs.
- Emulated TLS; single-hart atomics; C23 `_BitInt` (wide expand at 64 bits).
- Load/store via `SLOW32Addr` / `SelectAddr` (FI-as-base supported).
- Integer SELECT: branchless mask by default; branchy `SELECT_PSEUDO` under
  `optsize`/`minsize`.
- Inline asm: `r`, `f` (f32 in GPR), `i`/`n` (simm12), `m`; f64 pairs unsupported.
- Clang target + driver (slow32asm / s32-ld / mmio vs debug-io).

## Soft Spots
- Dual libcall registration (TargetLowering + Subtarget) via shared helper —
  revisit when upstream finishes RuntimeLibcalls consolidation.
- `SLOW32Schedule.td` exists for TableGen; not heavily tuned against real RTL.
- f64 inline-asm operands still unsupported (pair printing).

## Known Pitfalls (fixed or documented)
- ~~MBB long-branch ops dropped `%hi`/`%lo`~~ — fixed.
- ~~`(shl imm, 16) → LUI`~~ — removed.
- ~~Duplicate signed `extload` / inverted LoadStorePat / ADDC stubs~~ — removed.
- ~~R30/R31 double-spilled~~ — omitted from CSR; prologue owns them.
- Handwritten asm that triggers MC branch relaxation must treat `r2` as clobbered.
- `r2` has two jobs: the long-branch scratch, and the carrier of an
  indirect tail call's target across the epilogue.  They cannot meet -- the
  copy into `r2` is glued to the jump that ends its block -- but anything
  new that wants `r2` has to know about both.

## Regression Tests
```
./build/bin/llvm-lit -v llvm/test/CodeGen/SLOW32/
./build/bin/llvm-lit -v clang/test/Driver/slow32-toolchain.c
```
Coverage: addressing, branches (PC+4), long-branch `%hi`/`%lo`, jump tables,
varargs/f64 straddle, atomics+emutls, bitint, large frames, extload zext,
elf relocs, fp encoding, optional FP, SELECT optsize, memcpy/memmove/memset
names, i64/i32 udiv libcalls, `+m` feature, CFI smoke, tail calls
(`tail-call.ll`: what is one and what is not, the epilogue before the jump,
a kept pointer moved to `r2`, a frame past the 12-bit offset).

## Future Opportunities
- Fold `%lo` into the load or store that uses the address: a word of a
  global is `lui` + `addi` + `ldw` today, three instructions for two.
  Loads can take the existing LO12 relocation; a store's immediate is
  split, and needs a relocation kind the assembler and linker do not have.
- Shrink-wrapping: a function with a call on some path saves the return
  address on every path.  Tail calls remove the commonest case (a short
  path, and "otherwise the general routine"); the rest wants the
  prologue placed where it is needed.
- Callee-saved registers are spilled at the top of the frame: in a frame
  past 2 KB each save and restore is three instructions of address
  arithmetic.  Nearer the stack pointer they would be one.
- Soft-float libcall completeness when `-mattr=-f` (ensure full RTLIB map).
- f64 / GPRPair inline-asm constraint if external asm needs it.
- Schedule model tuning against the soft-core pipeline.
- Computed-goto / blockaddress stress beyond basic JT coverage.
