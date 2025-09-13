# Bug #001: Branch Folder Optimization Bug (FIXED)

## Problem (Historical)
The LLVM backend's Branch Folder optimization was incorrectly generating branch sequences,
causing unconditional branches to be placed before conditional branches, making the 
conditional branches unreachable.

## Test Description
This test calls a varargs sum function with 3 arguments (10, 20, 30).
- Expected: Returns 60 (sum of 10+20+30)
- Bug behavior: Loop would exit after first iteration due to branch ordering bug

## Status
**FIXED** - The test now passes correctly.

## Fix Applied
The bug was fixed with changes to the SLOW32 backend:

1. **TableGen flags** (SLOW32InstrInfo.td):
   - Added `isBarrier = 0` and `hasSideEffects = 0` to conditional branches
   - This correctly marks them as branches that can fall through

2. **analyzeBranch** (SLOW32InstrInfo.cpp):
   - Removed incorrect treatment of BR as a special case like RET
   - Now properly analyzes BR instructions as branches

3. **removeBranch** (SLOW32InstrInfo.cpp):
   - Only treats RET as a special case, not BR

4. **insertBranch** (SLOW32InstrInfo.cpp):
   - Fixed branch emission order: conditional first, unconditional second
   - Removed problematic getFirstTerminator() logic
   - Now always appends branches at the end in correct order

## Result
The test now correctly outputs "OK" and all 14 regression tests pass.