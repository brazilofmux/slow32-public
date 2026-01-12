# Critical Issue: 64-bit Division/Modulo Bug (RESOLVED)

**Status**: ✅ Resolved (2025-12-27)
**Resolution**: The LLVM backend's `UMUL_LOHI` custom lowering, previously suspected of being broken, has been verified to produce correct results. Regression tests (including `test_i64_div.c`) now pass with correct output values. The original issue description below is preserved for historical context.

## Original Issue Description

The SLOW32 LLVM backend miscompiles 64-bit `/` and `%` operations when the divisor is a constant (e.g., 10). Instead of emitting a call to `__udivdi3`/`__umoddi3`, the SelectionDAG legalization step replaces the operation with a multiply-by-magic-constant sequence. The expanded sequence assumes a working 64-bit multiply-high implementation, but the current SLOW32 lowering (`UMUL_LOHI` split into 16-bit parts) produces incorrect results. As a result, `uint64_t` division/modulo return wrong values across all optimization levels.

### Symptoms
- `printf("%llu", val)` prints garbage (because `printf` uses `/ 10` and `% 10`).
- `uint64_t` division by constant returns incorrect values.
- `__udivdi3` is NOT called even when `-O2` is used.

### Root Cause Analysis
LLVM's `TargetLowering` defaults to expanding division-by-constant into a sequence of multiplies and shifts (using "magic constants"). This requires a working `MULHU` (unsigned multiply high) or equivalent 64-bit multiply.

SLOW32 does not have a 64-bit multiply instruction, nor a `mulh` instruction. The backend implements `ISD::UMUL_LOHI` custom lowering (in `SLOW32ISelLowering.cpp`) to synthesize a 64-bit result from 32-bit inputs.

**The Bug:** The manual 16-bit split implementation of `UMUL_LOHI` was incorrect, specifically in how it handled carries between the partial products.

### Affected Versions
- All versions prior to the fix.

### Workaround
1.  **Disable Optimization**: Compile with `-O0` (though the backend might still try to optimize DAGs).
2.  **Force Libcalls**: Use `__udivdi3(a, b)` explicitly in C code and ensure it is marked `optnone` or compiled separately.
3.  **Volatile**: Make the divisor `volatile` to force a real division instruction (which lowers to a libcall).

### Fix Implementation
The fix involves correcting the `UMUL_LOHI` lowering logic in `SLOW32ISelLowering.cpp`.
Alternatively, we can tell LLVM *not* to use the division-by-constant optimization by overriding `BuildSDIV`/`BuildUDIV` or setting relevant `TargetLowering` flags, forcing it to always emit `__udivdi3`.

## Verification
Compile and run the following test case:

```c
#include <stdint.h>
#include <stdio.h>

int main(void) {
    uint64_t value = 18446744073709551615ull;
    printf("Testing: value / 10 = %llu\n", value / 10);
    printf("Testing: value %% 10 = %llu\n", value % 10);
    return 0;
}
```

**Expected Output:**
```
Testing: value / 10 = 1844674407370955161
Testing: value % 10 = 5
```