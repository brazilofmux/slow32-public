# SLOW32 Backend Status

Last Updated: 2025-09-19

## Overview
The SLOW32 LLVM backend is fully functional for C programs at all optimization levels (-O0, -O1, -O2).
Regression suite passes 14/14 tests. Native clang target (`-target slow32-unknown-none`) is working.

**Note**: No floating-point support yet (soft-float not implemented).

## Architecture Reminder
- 32-bit little-endian RISC with 32 GPRs; `r0`=zero, `r29`=sp, `r30`=fp, `r31`=lr.
- Base + signed 12-bit offset addressing; stack grows down from `0x0FFFFFF0`.
- Data layout mirrors clang: `e-m:e-p:32:32-i8:8:32-i16:16:32-i32:32:32-i64:32:32-f32:32:32-f64:32:32-n8:16:32-S32`.

## ✅ Working
- Core SelectionDAG + MC plumbing, prologue/epilogue, and register classes
- Integer, logical, shift, and memory instructions (immediate and register forms)
- Call/return path with glue-managed CopyToReg, `%hi`/`%lo` global materialisation, and stack argument spill
- **64-bit integers**: Full i64 support via custom lowering + libcalls (__divdi3, __udivdi3, __moddi3, __umoddi3)
- **Varargs**: Fully working with clang native target
- **Jump tables**: Implemented and working for switch statements
- **LLVM intrinsics**: memcpy, memset, lifetime, smax/smin
- Inline asm constraints `'r'`, `'i'`, `'n'`, `'m'` with register aliases
- Signed/unsigned conditional branches select native BLE/BGT/BLTU/BGEU/BGTU/BLEU sequences

## ⚠️ Areas for Future Improvement
- Branch analysis optimization could be enhanced for better codegen
- Machine verifier occasionally reports warnings (not errors) at -O0

## ❌ Not Implemented
- **Floating-point**: No FP support (soft-float not implemented)
- Debug info, TLS, atomics remain out of scope for now

## 🐛 Known Issues
- MachineVerifier occasionally reports warnings at -O0 (does not affect correctness)
- See `docs/IMPROVEMENTS.md` for detailed list

## Regression Tests
- **All 14/14 tests passing** in `~/slow-32/regression/`
- Test coverage includes:
  - Arithmetic operations (including 64-bit)
  - Control flow and jump tables
  - Varargs
  - Memory addressing
  - Stack arguments
  - MMIO support
- Run `cd ~/slow-32/regression && ./run-tests.sh` before committing backend changes