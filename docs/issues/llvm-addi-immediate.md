# LLVM Backend Bug: ADDI Immediate Sign Extension

## Problem Summary
The SLOW32 LLVM backend is incorrectly printing immediate values for ADDI instructions. It's printing the raw 12-bit immediate value without sign-extending negative values, causing incorrect assembly output.

## Root Cause
In `/home/sdennis/llvm-project/llvm/lib/Target/SLOW32/MCTargetDesc/SLOW32InstPrinter.cpp`, line 45 prints immediates directly:
```cpp
O << Op.getImm();
```

For ADDI instructions with 12-bit signed immediates, negative values need to be sign-extended when printed.

## Specific Example
When compiling division-by-10 optimization code at -O1 or -O2, LLVM generates:

- `addi r8, r1, 3277` (should be `addi r8, r1, -819`)
- `addi r17, r1, 3276` (should be `addi r17, r1, -820`)

These are magic constants for division optimization:

- 3277 (0xccd) is the low 12 bits of -819 (0xfffffccd)
- 3276 (0xccc) is the low 12 bits of -820 (0xfffffccc)

## Impact
This breaks printf() decimal integer formatting (%d, %i) completely - no digits are printed for any integer values.

## Test Case
```c
// test_division.c
#include <stdio.h>
int main() {
    printf("Value: %d\n", 42);  // Prints "Value: " (missing 42)
    return 0;
}
```

Compile with:
```bash
~/llvm-project/build/bin/clang -target slow32-unknown-none -S -emit-llvm -O1 test_division.c -o test.ll
~/llvm-project/build/bin/llc -mtriple=slow32-unknown-none test.ll -o test.s
grep "addi.*327[67]" test.s  # Will show the incorrect immediates
```

## Fix Required
The SLOW32InstPrinter needs to check if an instruction is ADDI and sign-extend 12-bit immediates before printing them. The immediate values are stored correctly internally (as 12-bit values), they just need proper sign extension when printed to assembly.
