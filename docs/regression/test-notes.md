# Regression Test Notes

This document centralizes descriptions of regression scenarios that were previously documented alongside individual tests. Each section summarises the purpose of the test, expected output, and any historical context that remains relevant when extending coverage.

## Feature Suites

### Feature: Basic Arithmetic (`regression/tests/feature-arithmetic`)
Verifies addition, subtraction, multiplication, division, and remainder operations using small constants. The executable prints `PASS` when all checks succeed.

### Feature: Branch Instructions (`regression/tests/feature-branches`)
Exercises each conditional branch (BLT, BGE, BEQ, BNE and their inversions). The expected console output is `123456`, proving the scheduler can fall through correctly.

### Feature: Function Calls (`regression/tests/feature-function-calls`)
Covers calling conventions with 2-, 8-, and 10-argument functions to ensure register and stack argument passing behave. The program reports `OK` on success.

### Feature: Logical Operations (`regression/tests/feature-logical`)
Confirms AND/OR/XOR logic for both register and immediate forms. Example cases include `0x55 & 0x33 = 0x11` and `0x55 ^ 0xFF = 0xAA`. Output should be `OK`.

### Feature: Loop Constructs (`regression/tests/feature-loops`)
Validates `for`, `while`, and `do-while` loops. The combined result should print `012345678`.

### Feature: Shift Operations (`regression/tests/feature-shifts`)
Checks SLL, SRL, and SRA with both register and immediate shift amounts. Pass condition is a `PASS` banner.

## Bug Reproductions

### Bug #001 – Varargs Argument Clobber (Fixed)
Location: `regression/tests/bug001-varargs-clobbered`

- **Original issue**: LLVM Branch Folder pass reordered conditional/unconditional branches, clobbering the loop that consumes varargs arguments.
- **Impact**: Variadic functions miscounted arguments; sample sum test returned the wrong value.
- **Fix summary**: Updated `SLOW32InstrInfo` TableGen flags so conditional branches are not treated as barriers, rewrote `analyzeBranch`, `removeBranch`, and `insertBranch` to emit branches in the correct order.
- **Status**: The regression now prints `OK` and protects against regressions in branch scheduling.

### Bug #005 – getelementptr / memcpy Expansion
Location: `regression/tests/bug005-getelementptr`

- **Original failure**: At `-O0`, LLVM lowered `llvm.memcpy` calls to repeated byte loads from offset `0`, so copying "ABC\0" resulted in `"   "`. Array and struct accesses were effectively broken.
- **Investigations (2025-09-11)**: Implemented `SLOW32Addr` ComplexPattern, `SelectAddr`, and frame index fixes. Assembly now uses incrementing offsets (`ldbu r2, r1+3`, etc.).
- **Workarounds applied**: Runtime `memcpy` switched to byte-wise copying in `intrinsics.s`; backend adjustments ensure %hi/%lo pairs for `GlobalAddress+offset`.
- **Current risk**: Runtime still hit a memory fault (0xFFFFFFF4) after ~154K instructions, suggesting remaining frame or stack issues when the optimizer is disabled.
- **Reproduction**: Compile `test.c` with `-O0`, assemble, link with runtime archives, and run the emulator to observe the faulty output; `-O1` and above no longer exhibit the bug.
- **Next steps noted**: Debug the memory fault, request assembler support for `symbol+offset`, and re-validate at all optimization levels.

