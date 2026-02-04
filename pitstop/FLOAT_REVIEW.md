# Code Review: Floating-Point Implementation (f32/f64)

**Date:** 2026-02-02
**Review Target:** Commits `f432799` to HEAD (Floating-point support)

## Overview
A review was conducted on the recent addition of IEEE 754 floating-point support to the SLOW-32 toolchain. The implementation covers the LLVM backend, assembler, emulator, and runtime. The core functionality (arithmetic, comparisons, conversions) is well-implemented and verified by regression tests. However, a few gaps and potential issues were identified.

## Findings (Updated 2026-02-02)

### 1. LLVM Backend: Hardcoded Alignment in f64 Lowering -- FIXED
In commit `0d3870eb`, the hardcoded `Align(4)` in `LowerLOAD` and `LowerSTORE` was replaced with proper alignment propagation from the `LoadSDNode`/`StoreSDNode`.

```cpp
// SLOW32ISelLowering.cpp
Align BaseAlign = Load->getAlign();
SDValue Lo = DAG.getLoad(MVT::i32, DL, Chain, Addr, MPI, BaseAlign);
// ...
SDValue Hi = DAG.getLoad(MVT::i32, DL, Chain, AddrHi, MPI.getWithOffset(4),
                         commonAlignment(BaseAlign, 4));
```

**Status:** Resolved. LLVM now correctly handles unaligned `f64` access by decomposing it into byte/halfword operations when necessary.

### 2. Runtime: Missing fmod/fmodf Implementation -- DECISION: HOST INTERCEPT
The `FREM` operation maps to `fmod` and `fmodf`. While these are stubs in the runtime library, the project uses a host-intercept strategy for complex math functions.

**Architectural Decision:**
*   **Emulators:** `slow32-dbt` and QEMU-based emulators intercept these symbols via the `.s32x` symbol table and execute them natively on the host.
*   **Reference Emulators:** `slow32` and `slow32-fast` will execute the stubs (returning 0).
*   **Rationale:** Avoids the significant complexity of implementing soft-float transcendentals/complex operations (like CORDIC) in assembly, leveraging host performance for high-speed emulators.

**Status:** Resolved (by policy).

### 3. Runtime: Missing f64 Intrinsics -- RESOLVED (DEAD CODE REMOVED)
The LLVM backend emits native instructions directly for `f64` arithmetic and comparisons. Commit `0d3870eb` removed over 150 lines of dead soft-float code from `float_intrinsics.s` to keep the runtime lean.

**Status:** Resolved.

### 4. LLVM Backend: FCOPYSIGN Expansion for f64 -- FIXED
Commit `0d3870eb` implemented custom lowering for `f64` `FCOPYSIGN`. It now correctly splits the register pair and performs bitwise operations on the high word.

```cpp
// SLOW32ISelLowering.cpp
SDValue MagHi = MagSplit.getValue(1);
SDValue HiMasked = DAG.getNode(ISD::AND, DL, MVT::i32, MagHi, MagMask);
SDValue NewHi = DAG.getNode(ISD::OR, DL, MVT::i32, HiMasked, SignMasked);
return DAG.getNode(SLOW32ISD::BuildPairF64, DL, MVT::f64, MagLo, NewHi);
```

**Status:** Resolved. Supports `f32`/`f64` mixed-type sign copying as well.

## Recommendations & Next Steps (Updated)

1.  **Verification:** Add a regression test for `copysign(double, double)` and unaligned `f64` (packed structs) to confirm the fixes in commit `0d3870eb` are robust.
2.  **String Conversions:** Note that `dtoa`/`strtod` are still missing and remain the next major gap for high-level language support.
