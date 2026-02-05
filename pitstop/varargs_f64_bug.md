# SLOW-32 LLVM Backend Bug: varargs f64 stack arguments

## Symptom
When a printf call has 4 or more f64 (double) arguments, the 4th and subsequent
arguments are read incorrectly (showing as 0.0 instead of their actual values).

## Test Case
```c
#include <stdio.h>
int main(void) {
    double a = 1.0, b = 2.0, c = 3.0, d = 4.0;
    printf("4 args: %.1f %.1f %.1f %.1f\n", a, b, c, d);
    // Output: 4 args: 1.0 2.0 3.0 0.0
    // Expected: 4 args: 1.0 2.0 3.0 4.0
    return 0;
}
```

## Root Cause (IDENTIFIED)

### The Problem
The varargs save area saves ALL 7 argument registers (r4-r10 = 28 bytes), but when
doubles are passed, r10 is left UNUSED by the caller because doubles need register
pairs and r10 is an odd register with no pair.

### Memory Layout Issue

**Caller passes 4 doubles:**
- r4:r5 = 1.0 (1st double)
- r6:r7 = 2.0 (2nd double)
- r8:r9 = 3.0 (3rd double)
- r10 = UNUSED (no pair for double)
- stack+0 to stack+7 = 4.0 (4th double)

**Callee (printf) creates 28-byte save area and saves r4-r10:**
```
fp-28: r4  (1st double lo)
fp-24: r5  (1st double hi)
fp-20: r6  (2nd double lo)
fp-16: r7  (2nd double hi)
fp-12: r8  (3rd double lo)
fp-8:  r9  (3rd double hi)
fp-4:  r10 (GARBAGE - unused by caller!)
fp+0:  4th double lo (on stack)
fp+4:  4th double hi (on stack)
```

**What happens with va_arg:**
1. va_start returns fp-28
2. 1st va_arg(double): reads fp-28, fp-24 (r4, r5) ✓ → AP = fp-20
3. 2nd va_arg(double): reads fp-20, fp-16 (r6, r7) ✓ → AP = fp-12
4. 3rd va_arg(double): reads fp-12, fp-8 (r8, r9) ✓ → AP = fp-4
5. 4th va_arg(double): reads fp-4, fp+0 **BUG!**
   - Lo word = r10 (garbage)
   - Hi word = 4th double's lo word (wrong!)

The 4th double should read {fp+0, fp+4} but instead reads {fp-4, fp+0}.

### Why 0.0 Appears
The 4th double (4.0) has IEEE754 representation:
- Low word: 0x00000000
- High word: 0x40100000

When we read {r10, fp+0} = {garbage?, 0x00000000}, if r10 happens to be 0,
we get 0.0.

## The Fix

### Option 1: Reduce varargs save area to 6 registers (RECOMMENDED)
Only save r4-r9 for varargs functions. This ensures the save area is exactly
24 bytes and is contiguous with stack arguments.

In `SLOW32ISelLowering.cpp`, change the varargs setup (around line 1056):

```cpp
// BEFORE:
unsigned NumRegsLeft = FirstVAReg < NumArgRegs ? NumArgRegs - FirstVAReg : 0;

// AFTER:
// For varargs, only use r4-r9 (6 regs). r10 can't hold a double alone.
const unsigned NumVarArgRegs = 6;
unsigned NumRegsLeft = FirstVAReg < NumVarArgRegs ? NumVarArgRegs - FirstVAReg : 0;
```

**Trade-off:** A varargs call with 7 int arguments would have the 7th int on
stack instead of in r10. Minor performance impact, rare use case.

### Option 2: Position save area to account for gap
Calculate save area position so that unused register slots don't create a gap.
More complex, but preserves r10 for int varargs.

## Files to Modify

- `llvm-backend/SLOW32/SLOW32ISelLowering.cpp` lines 1053-1105
  - Specifically the `NumRegsLeft` calculation for varargs
  - The `VarArgsSaveSize` calculation

## Impact
This bug affects any varargs function (printf, sprintf, etc.) when more than
3 double arguments are passed. Breaks real-world programs like the regal
valuation code.

## Related Code

### Calling Convention (Caller Side - CORRECT)
```asm
addi sp, sp, -8
lui r18, 262400              ; 0x40100000 = high word of 4.0
stw sp+4, r18                ; high word at sp+4
addi r17, r0, 0
stw sp+0, r17                ; low word (0) at sp+0
jal r31, printf
addi sp, sp, 8
```

### Varargs Setup (lines 1053-1105 in SLOW32ISelLowering.cpp)
```cpp
if (isVarArg) {
    unsigned FirstVAReg = CCInfo.getFirstUnallocated(SLOW32ArgRegs);
    unsigned NumArgRegs = NumSLOW32ArgRegs;  // Currently 7
    unsigned NumRegsLeft = FirstVAReg < NumArgRegs ? NumArgRegs - FirstVAReg : 0;
    // ... saves NumRegsLeft registers (currently 7 = 28 bytes)
}
```

### LowerVAARG (in SLOW32ISelLoweringVarargs.cpp)
The va_arg implementation is correct - it simply reads from AP and bumps by
8 bytes for doubles. The problem is where VA_START points and the save area size.
