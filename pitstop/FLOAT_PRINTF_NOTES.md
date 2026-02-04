# Float printf + dtoa integration notes (2026-02-02)

This document captures what we learned while attempting to integrate David M. Gay's `dtoa.c` to enable `printf` `%f/%e/%g` on SLOW-32. Work is **paused**; the goal is to document compiler/toolchain gaps discovered so a separate agent can address them.

## Summary

We were able to compile `dtoa.c` and wire a first-pass float formatting path in `printf_enhanced.c`, but this surfaced multiple issues:

1. **Varargs + f64 in `printf`**: floating varargs are not retrieved correctly (all zeros), and certain forms of `va_arg(ap, double)` can **crash llc** during legalization.
2. **LLVM register allocator crash** (Greedy RA) on `dtoa.c` at `-O2`.
3. **Assembler limitations** encountered due to the size/shape of generated `.s`:
   - `Too many labels` (MAX_LABELS too low)
   - `Invalid string literal` (assembler cannot parse long `.ascii` with octal escapes or long lines)
   - `MAX_LINE`/`MAX_TOKEN_LEN` too small for large `.ascii` lines

We mitigated the assembler issues locally, but the compiler/ABI issues remain.

## Varargs + f64: observed behavior

### Symptoms

- `feature-printf-float` test produced **all zeros** for `%f/%e/%g` output.
- `printf` formatting uses `va_arg(ap, double)` for `%f/%e/%g`.
- Output zeros indicates `va_arg` is pulling **zeroed values** from the varargs area.

### Notes

The SLOW-32 varargs lowering currently:

- Saves **remaining integer registers** to the varargs save area (`LowerFormalArguments`).
- `va_arg` (`LowerVAARG`) loads from the varargs pointer and advances by 4 or 8 bytes for integer/fp.

However, for **vararg doubles**, the ABI likely passes in **register pairs** when available, and the varargs save area is only saving **single 32-bit registers**, not **paired f64 reg slots** (or not saving the *correct* registers after f64 assignment).

This mismatch would make `va_arg(ap, double)` read garbage/zeros.

### Additional failure: llc assertion

When `printf_enhanced.c` uses `va_arg(ap, double)` directly, `llc` can assert in SelectionDAG Legalize:

```
Assertion "Unexpected illegal type!" in LegalizeDAG
```

This occurred at `-O2` in `vsnprintf_enhanced`. The crash went away when **avoid**ed using `va_arg` on `double` and instead pulled raw words, but that approach produced **zeros** (ABI mismatch).

**Conclusion:** the float varargs ABI + `va_arg` lowering is incomplete or inconsistent for f64.

## LLVM backend crash: dtoa.c @ -O2

### Symptom

`llc` crashes in Greedy Register Allocator:

```
MCRegisterInfo::operator[]: invalid register number
Stack: SLOW32InstrInfo::storeRegToStackSlot -> InlineSpiller -> RAGreedy
```

This happens on `dtoa.c` at `-O2` in function `dtoa_strtod` (very large function with high register pressure). `-O0` avoids the regalloc crash but produces enormous `.s` files that stress the assembler.

### Likely gap

A register spill path for **GPRPair** or a subreg mapping is producing an invalid register index. This is likely a backend bug in:

- `SLOW32InstrInfo::storeRegToStackSlot` / spill logic for f64
- GPRPair subregister handling
- RegAlloc (Greedy) + spill folding using pair regs

## Assembler gaps exposed by dtoa.s

`dtoa.c` generates very large assembly with long `.ascii` lines containing octal escapes and non-printable bytes. The assembler failed with:

- `Too many labels` (MAX_LABELS = 1024)
- `Invalid string literal` on `.ascii "\000..."` (octal escapes not supported)
- Large `.ascii` lines truncated due to `MAX_LINE = 256` / `MAX_TOKEN_LEN = 512`

### Local mitigations applied

To get dtoa.s to assemble, the following changes were needed:

- Increase `MAX_LABELS` (to 8192)
- Increase `MAX_LINE` (to 65536)
- Increase `MAX_TOKEN_LEN` (to 16384)
- Add **octal escape** support in `process_string_literal` (e.g., `\000`)

These changes were enough to assemble dtoa.s at `-O0`.

## Current integration scaffolding (paused)

The following was added (not finalized):

- `runtime/dtoa.c` (copied from `pitstop/dtoa.c`)
- `runtime/include/errno.h`, `runtime/include/float.h`, and `runtime/errno.c`
- `runtime/Makefile` rules to compile `dtoa.c` with `-O0` and include it in libc
- `printf_enhanced.c` prototype formatting path using dtoa (modes 2/3)
- `regression/tests/feature-printf-float` test

All of this is **paused** pending compiler/backend fixes.

## Recommendations / next steps

1. **Fix f64 varargs ABI**:
   - Confirm how f64 varargs are passed (register pairs vs stack)
   - Ensure varargs save area includes **both halves** of f64 reg pairs
   - Ensure `va_arg` for f64 reconstructs the proper pair

2. **Fix llc legalize crash on `va_arg(ap, double)`**:
   - Identify illegal type in LegalizeDAG for f64 varargs
   - Add proper custom lowering if needed

3. **Fix regalloc crash for large f64-heavy functions**:
   - Investigate invalid register number during spill of GPRPair
   - Check `storeRegToStackSlot` subreg mapping

4. **Assembler robustness**:
   - Keep octal escape support
   - Keep larger line/token limits (or implement multi-line `.ascii` parsing)

Once (1)-(3) are addressed, `%f/%e/%g` should work reliably.

## Toolchain gaps checklist

- [ ] f64 varargs ABI: confirm register/stack layout for doubles in varargs
- [ ] varargs save area: ensure both halves of f64 register pairs are saved
- [ ] `va_arg` lowering: validate f64 handling and removal of illegal types
- [ ] llc LegalizeDAG crash with `va_arg(ap, double)`
- [ ] RegAlloc crash in `dtoa_strtod` (`-O2`) due to invalid register id
- [ ] Spill/fold logic for GPRPair in `SLOW32InstrInfo::storeRegToStackSlot`
- [ ] Assembler robustness: octal escapes, label capacity, long lines/tokens

## Runtime/printf changes touched (paused)

- `runtime/dtoa.c` (copied from `pitstop/dtoa.c`)
- `runtime/printf_enhanced.c` float formatting path (modes 2/3 via dtoa)
- `runtime/include/float.h`, `runtime/include/errno.h`, `runtime/errno.c`
- `runtime/Makefile` dtoa build rule (`-O0`, `-DIEEE_8087`) and libc include
- `regression/tests/feature-printf-float` test case
