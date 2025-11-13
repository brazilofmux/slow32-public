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

## Status Update (2024-11-11)
- Reimplemented the custom `UMUL_LOHI` lowering so every 16-bit partial product is masked, shifted, and merged with explicit carry propagation. This fixes the bogus multiply-high results that constant-division relied on.
- Added `llvm/test/CodeGen/SLOW32/umul-lohi.ll` to pin the new lowering strategy so future tweaks cannot drop those carries silently.
- Rebuilt the toolchain and reran the original `test_i64_div.c` repro through the assembler/linker/emulator pipeline; the program now prints the expected `18446744073709551615`.

## New Regression (2025-03-04)
Although constant divisors now behave correctly when the compiler expands them itself, directly calling the runtime libcalls still produces corrupt high halves:

```c
#include <stdint.h>
#include <stdio.h>

extern uint64_t __udivdi3(uint64_t, uint64_t);
extern uint64_t __umoddi3(uint64_t, uint64_t);

int main(void) {
    uint64_t value = 1234ULL;
    uint64_t q = __udivdi3(value, 10ULL);
    uint64_t r = __umoddi3(value, 10ULL);
    printf("q=%llu r=%llu\n", (unsigned long long)q, (unsigned long long)r);
    return 0;
}
```

Build and run with the `slow32cc` driver:

```bash
./slow32cc test_small_div.c -O2 -o test_small_div.s32x
tools/emulator/slow32 test_small_div.s32x
```

Observed output:

```
q=4294967419 r=4
```

Expected output:

```
q=123 r=4
```

The remainder is correct but the upper 32 bits of the quotient are garbage (here `0x00000001`). Any formatting code that inspects the high word—such as the `%llu` converter—believes the quotient is still ≥2³² and keeps iterating, producing the `491322686312992511615` string discovered while testing the restored division-based formatter.

**Hypothesis:** the backend never zero-extends the high 32-bit register when returning from the `__udivdi3` libcall. The handwritten long-division logic in `runtime/builtins.c` only ever writes the low word once the quotient drops below 2³², leaving the return register pair `(r2:r1)` with an old high value. When clang synthesizes division-by-constant loops it does not emit the libcall, so the issue only shows up when code explicitly calls `__udivdi3`/`__umoddi3`.

## Fix (2025-03-05)
- Reworked the runtime’s `__udivdi3` so the quotient is accumulated via explicit 32-bit halves instead of relying on `uint64_t` bit shifts (`slow-32/runtime/builtins.c`). Each loop iteration now toggles either the low or high word directly, which guarantees that the callee clears `r2` whenever the true result fits in 32 bits.
- Rebuilt `libs32.s32a` and reran the failing repros with the existing `test_small_div.c` sample plus the union-based sanity check below (`./slow32cc <file>.c -O2 -o <file>.s32x && tools/emulator/slow32 <file>.s32x`).
- Both programs now report `hi=0` and `q=123 r=4`, so direct libcalls line up with the constant-division expansion again.

The extra sanity harness used to observe the raw register halves:

```c
#include <stdint.h>
#include <stdio.h>

extern uint64_t __udivdi3(uint64_t, uint64_t);

int main(void) {
    union {
        uint64_t ll;
        struct {
            uint32_t lo;
            uint32_t hi;
        } parts;
    } q = {0};

    q.ll = __udivdi3(1234ULL, 10ULL);
    printf("lo=%u hi=%u\n", q.parts.lo, q.parts.hi);
    return 0;
}
```

## Formatter Follow-up (2025-03-05)
- `runtime/printf_enhanced.c` now routes `%llu` through the standard `__udivdi3`/`__umoddi3` helpers again. The converter processes the number in base-100 chunks so it still leverages the `Digits100` table without needing the temporary double-dabble BCD buffer.
- The shared `udivmoddi3_core` helper drives both `__udivdi3` (for quotients) and `__umoddi3` (which now derives the remainder as `n - q * d`), so the libcall pair always agrees on the high word before handing results back to the caller.
- Rebuilt both libc variants and verified that printing `18446744073709551615ULL` via `printf("value=%llu\n", v);` produces the expected decimal string under the emulator using the current toolchain. A new regression (`regression/tests/feature-printf-llu`) covers this end-to-end.

**Next steps:**

1. Landed `llvm/test/CodeGen/SLOW32/udiv-libcall.ll`, which issues an explicit `@__udivdi3` call and verifies the caller copies both halves of the returned quotient (guarding the ABI contract).
2. Monitor downstream consumers to ensure the reinstated `%llu` path covers their needs; add more printf regression tests if issues pop up.

## Workarounds
- Older toolchain drops should keep the double-dabble `%llu` path if they cannot pick up the updated backend/runtime pair; current builds no longer require it.
- Manually call `__udivdi3`/`__umoddi3` from C and mark them `__attribute__((used))` if you must stay on an older compiler where constant-folding still tries to synthesize division sequences.

## Next Steps for the LLVM Backend
- ✅ Preserve correctness in the custom multiply-high lowering used by constant-division (done via the new `UMUL_LOHI` expansion).
- ✅ Land a regression test that exercises the carry paths in the new lowering (`llvm/test/CodeGen/SLOW32/umul-lohi.ll`).
- ☐ Simplify `%llu` printing in `runtime/printf_enhanced.c` now that the backend can safely emit div/mod loops again (needs runtime-side follow-up once the formatter is retested).

Until the runtime-side cleanups land everywhere, downstream projects should continue to avoid `uint64_t` division/modulo in performance-critical code unless they can pick up the updated backend.
