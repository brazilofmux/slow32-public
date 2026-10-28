# SLOW32 Backend Status

Last Reviewed: 2026-10-28

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
- Shrink-wrapping: the prologue and epilogue go where ShrinkWrap puts them,
  so an early exit that makes no call builds no frame.  Functions with
  unwind tables included (CFIFixup, `resetCFIToInitialState`).  Off for
  `optnone` and for any function that reads or writes `r31` other than by
  a call or return (`r31` is not in the CSR list, so ShrinkWrap cannot see
  it).  `__builtin_return_address(0)` reads `r31` as a function live-in.
- The constant 0 is `r0` (a CopyFromReg, as RISC-V's `x0`): no register
  is spent on compares against zero or on zero stores.
- Globals/JT/CPI/blockaddr via `LOAD_ADDR` → `%hi`/`%lo`; jump tables via
  Expand → load + `BRIND` (`jalr`).  `SLOW32LoadAddrOpt` folds the `%lo`
  into the loads and stores when every use of the address is a memory
  base at one offset (`lui` + `ldw sym+%lo`): a store's `%lo` is the
  S-type split, which the MC layer and `s32-ld` both apply.  Uses at
  different offsets keep `lui` + `addi`, since the carry out of `%lo` is
  only known at link time.
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

## Verification
- `-verify-machineinstrs` is clean over every C library clang builds for
  SLOW-32 (runtime, libcob, libf77, SQLite):
  `~/slow-32/regression/run-llvm-verify.sh`.  Run it after any change here;
  turned on 2026-10-27, it flagged the GPRPair spill bug and two liveness
  defects at once.
- `~/slow-32/regression/run-stage08-fuzz.sh` compiles random C with clang
  and with stage08 cc at -O2 and -Os, llc under `-verify-machineinstrs`;
  a case where stage08 agrees with the host compiler is a backend bug.

## Known Pitfalls (fixed or documented)
- ~~MBB long-branch ops dropped `%hi`/`%lo`~~ — fixed.
- ~~`(shl imm, 16) → LUI`~~ — removed.
- ~~Duplicate signed `extload` / inverted LoadStorePat / ADDC stubs~~ — removed.
- ~~R30/R31 double-spilled~~ — omitted from CSR; prologue owns them.
- ~~Branchless SELECT computed the discarded poison arm~~ — both arms frozen.
- ~~Virtual GPRPair spill reloaded as two undef halves~~ — one
  `LDW64_FI`/`STW64_FI` split in `eliminateFrameIndex`.
- ~~`__builtin_return_address(0)` after a call read the clobbered `r31`~~ —
  `r31` is a function live-in.
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
a kept pointer moved to `r2`, a frame past the 12-bit offset), GPRPair
spills, SELECT poison arms, shrink-wrapping (`shrink-wrap.ll`, with and
without unwind tables), `returnaddr-after-call.ll`, `zero-register.ll`,
and the `%lo` fold (`global-lo-fold.ll`, `load-addr-fold.mir`).

## Future Opportunities
- Callee-saved registers are spilled at the top of the frame: in a frame
  past 2 KB each save and restore is three instructions of address
  arithmetic.  Nearer the stack pointer they would be one.
- The `%lo` fold takes one offset per address; a global struct read at
  several offsets still pays `lui` + `addi` once and a load per field.
- Soft-float libcall completeness when `-mattr=-f` (ensure full RTLIB map).
- f64 / GPRPair inline-asm constraint if external asm needs it.
- Schedule model tuning against the soft-core pipeline.
- Computed-goto / blockaddress stress beyond basic JT coverage.
