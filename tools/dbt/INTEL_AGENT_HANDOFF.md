# ARM/Intel DBT Handoff Notes

## Current State
- ARM DBT now passes:
  - `lua` smoke matrix (`tools/dbt/lua-triage.sh`)
  - `sbasic/tests` (`29/29`)
  - `forth/tests` (`22/22` via DBT smoke)
- Key ARM fix already applied:
  - `tools/dbt/emit_a64.c`: corrected AArch64 FP 1-source encodings (`fneg/fabs/fsqrt`).
  - Symptom fixed: negative values losing sign (for example `-7.30 -> 7.30`).

## Important Findings
- A previous ARM-only symptom was traced to wrong `fneg` emission.
- Remaining memory-fault signatures around `free` path (`PC=0x0001A1E8`) were observed in non-DBT `slow32` and `slow32-fast` too, with different reporting text.
- `sbasic/tests/run-tests.sh` now filters DBT memory-fault text the same way it already filtered fast-emulator OOB lines, so harness output aligns for regression runs.

## Repro Commands
- Lua matrix:
```bash
tools/dbt/lua-triage.sh
```
- SLOW BASIC:
```bash
cd sbasic/tests && EMU=../../tools/dbt/slow32-dbt bash ./run-tests.sh
cd sbasic/tests && EMU=../../tools/emulator/slow32-fast bash ./run-tests.sh
```
- Forth DBT sanity:
```bash
cat forth/prelude.fth forth/tests/test-core-arith.fth | ./tools/dbt/slow32-dbt forth/kernel.s32x
```

## Intel Agent Priorities
1. Validate parity on Intel host:
   - Compare `slow32-fast`, `slow32`, and DBT outputs for the same workloads.
2. UB / aliasing sweep:
   - Build with sanitizers and strict-aliasing toggles.
3. Toolchain variance:
   - Compare GCC vs Clang builds (especially optimized builds).
4. Code review targets:
   - Float/int bit reinterpretation, sign/rounding conversions, varargs call boundaries, and memory allocator metadata manipulation.

## Suggested Diagnostic Builds
```bash
make clean
make CFLAGS="-O0 -g -fsanitize=address,undefined -fno-omit-frame-pointer"
```
```bash
make clean
make CFLAGS="-O2 -g -fno-strict-aliasing"
```

## High-Value Cross-Checks
- If an issue appears only on one architecture/compiler:
  - suspect UB or codegen-sensitive C patterns first.
- If issue reproduces in both DBT and interpreter:
  - suspect shared runtime/program behavior rather than DBT translation.
