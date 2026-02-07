# SLOW32 Backend Status

Last Reviewed: 2025-09-29  
Next Review Due: 2025-11-15 (archive this file if the date passes)

## Overview
- Backend is active on LLVM main; llc and clang build with assertions on.
- SelectionDAG, MC, and pass config are exercised by medium-sized C workloads.
- Treat this snapshot as living documentation; drop it after the next review if it is still stale.

## Architecture Reminder
- 32-bit little-endian RISC with 32 GPRs; `r0`=zero, `r29`=sp, `r30`=fp, `r31`=lr.
- Base + signed 12-bit offset addressing; stack grows down from `0x0FFFFFF0`.
- Data layout mirrors clang: `e-m:e-p:32:32-i8:8:32-i16:16:32-i32:32:32-i64:32:32-f32:32:32-f64:32:32-n8:16:32-S32`.

## ✅ Working
- Core SelectionDAG + MC plumbing, prologue/epilogue, and register classes.
- Integer, logical, shift, and memory instructions (immediate and register forms).
- Multiply + high-multiply (`MUL`, `MULH`) opcodes in instruction selection.
- Call/return path with glue-managed CopyToReg, `%hi`/`%lo` global materialisation, and stack argument spill.
- 64-bit scalar ABI now routes through the r1/r2 and r3/r4 register pairs without gratuitous spill slots.
- Varargs fast path: `va_start`, register spill to the shadow area, and `va_arg` for integers/pointers.
- Inline asm constraints `'r'`, `'i'`, `'n'`, `'m'` with register aliases.
- Signed/unsigned conditional branches select native BLE/BGT/BLTU/BGEU/BGTU/BLEU sequences via dedicated opcodes.

## ⚠️ Partially Working
- Switches: chained compares succeed; jump tables and tail-dup guards are still TODO.
- Varargs: heavy mixes beyond eight register args need more stress and clang `va_list` layout cleanup.
- Machine verifier: `-O0` occasionally flags missing barriers around synthesised compare/branch sequences.

## ❌ Still Todo
- Wire up an `isMBBSafeToTailDuplicate` guard in `SLOW32InstrInfo` to keep RET+BR blocks sane.
- Map `llvm.memcpy/memmove/memset` intrinsics to the correct libcall name instead of hard-wiring `memcpy`.
- Debug info, TLS, atomics, and FP remain out of scope for now.

## 🐛 Known Issues
- Constant folding for `(C + reg)` in truncated contexts can produce incorrect low bits.
- Some libcall lowers from intrinsics emit `memcpy` even for memset/memmove.
- MachineVerifier occasionally reports fallthrough after conditionals when branch barriers are rewritten.

## Regression Tests
- Run `ninja -C build check-llvm-codegen-SLOW32` and `check-clang` before merging.
- Current suite covers arithmetic/control/varargs/addressing plus native branch opcode selection (`llvm/test/CodeGen/SLOW32/branches.ll`); extend it for jump tables and inline asm clobbers.
- f32/f64 arithmetic, compare, and convert lower to native FP opcodes in GPRs (f64 uses even/odd register pairs). Transcendentals such as `fmodf`/`fmod` remain libcalls.
- Refresh this section with concrete pass/fail counts on each review cycle; archive the document if it is not updated by the next review date.
