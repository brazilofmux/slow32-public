# LLVM Backend Bug: Incorrect i64 Division/Modulo by Constants

## Problem Summary
The SLOW32 LLVM backend miscompiles 64-bit `/` and `%` operations when the divisor is a constant (e.g., 10). Instead of emitting a call to `__udivdi3`/`__umoddi3`, the SelectionDAG legalization step replaces the operation with a multiply-by-magic-constant sequence. The expanded sequence assumes a working 64-bit multiply-high implementation, but the current SLOW32 lowering (`UMUL_LOHI` split into 16-bit parts) produces incorrect results. As a result, `uint64_t` division/modulo return wrong values across all optimization levels.

## Impact
- Any code that performs `uint64_t` or `int64_t` division/modulo by constants generates bogus values. This breaks fundamental routines such as time calculations and decimal formatting.
- The enhanced `printf` implementation (`runtime/printf_enhanced.c`) had to switch to a double-dabble BCD converter for `%llu` to avoid triggering the compiler bug, adding complexity and making the formatter slower than it needs to be.
- Libraries that need correct `time_t` (seconds since epoch) math or filesystem helpers (`stat`, `fstat`) cannot rely on `value / 10`, `/ 60`, etc.

## Reproduction
Save the following test program as `test_i64_div.c`:

```c
#include <stdint.h>
#include <stdio.h>

int main(void) {
    uint64_t value = 18446744073709551615ull;
    char buf[32];
    int len = 0;

    while (value) {
        uint64_t q = value / 10;
        uint64_t r = value % 10;
        buf[len++] = '0' + (int)r;
        value = q;
    }

    for (int i = len - 1; i >= 0; --i) putchar(buf[i]);
    putchar('\n');
    return 0;
}
```

Build and run it with the native toolchain:

```bash
~/llvm-project/build/bin/clang -target slow32-unknown-none -S -emit-llvm -O2 \
    -Iruntime/include test_i64_div.c -o test_i64_div.ll
~/llvm-project/build/bin/llc -mtriple=slow32-unknown-none test_i64_div.ll -o test_i64_div.s
./tools/assembler/slow32asm test_i64_div.s test_i64_div.s32o
./tools/linker/s32-ld -o test_i64_div.s32x runtime/crt0.s32o test_i64_div.s32o \
    runtime/libs32.s32a runtime/libc_debug.s32a
./tools/emulator/slow32 test_i64_div.s32x
```

## Observed vs. Expected Output

```
Expected: 18446744073709551615
Actual:   32421741531949858815
```

The incorrect value appears at both `-O0` and `-O2`, and it is independent of the C library (DEBUG vs. MMIO).

## Assembly Evidence
The generated `test_i64_div.s` never calls `__udivdi3`/`__umoddi3`. Instead, LLVM emits a hand-written division-by-10 sequence built from magic constants (`0xCCCC`, `0xCCCD`, …):

```asm
lui r6, 13
addi r1, r6, -819      # 0x0000CCCD
lui r4, 16
addi r4, r4, -1        # 0x0000FFFF
...
mul r17, r16, r1       # part of magic-multiply divide-by-10
mul r19, r18, r6
...
```

Because SLOW32 only has 32-bit `mul`/`mulh` implemented via software splitting (`llvm-backend/SLOW32/SLOW32ISelLowering.cpp:1700`), the synthesized sequence fails to preserve the high 32 bits needed for the algorithm, so the quotient and remainder are wrong.

## Root-Cause Hypothesis
1. `SLOW32TargetLowering` calls `setOperationAction(ISD::UDIV, MVT::i64, Expand)` intending to force libcalls, but the target-independent legalization still performs strength reduction for constant divisors before the libcall decision is made.
2. The legalization relies on correct `i64` multiply-high support. Our current lowering for `ISD::UMUL_LOHI` (splitting into four 16×16 products) likely misses carries, so the synthesized division/remainder sequence is mathematically incorrect.

## Workarounds
- Avoid emitting `/` or `%` on 64-bit values in the runtime. `runtime/printf_enhanced.c` currently uses a double-dabble converter for `%llu` to stay clear of the miscompiled path.
- Manually call `__udivdi3`/`__umoddi3` from C and mark them `__attribute__((used))` so LLVM cannot strength-reduce the operation (not practical for general code).

## Next Steps for the LLVM Backend
1. Decide whether SLOW32 should **always** lower `i64` division/modulo to libcalls, even for constant divisors. If so, add a target hook (e.g., override `isIntDivCheap`/`isIntDivRemCheap`) or custom DAG combine to block `DivRemByConstant` transforms.
2. Alternatively, fix the custom multiply-high lowering in `SLOW32ISelLowering.cpp` so that the generated code preserves carries correctly when used by constant-division expansions.
3. Add an LLVM regression test (e.g., `llvm/test/CodeGen/SLOW32/i64-div-const.ll`) that verifies `udiv i64` by 10 lowers to a libcall or otherwise produces the correct quotient.
4. Once fixed, simplify `%llu` printing paths in `runtime/printf_enhanced.c` back to straightforward div/mod loops.

Until the backend is fixed, downstream projects should continue to avoid `uint64_t` division/modulo in performance-critical code.
