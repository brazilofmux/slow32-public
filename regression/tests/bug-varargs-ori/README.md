# bug-varargs-ori

Regression test for the LLVM backend varargs ORI bug (FIXED).

## The Bug

The backend was using `ORI` (bitwise OR) instead of `ADDI` when computing stack slot addresses for spilling multiple vararg registers. This corrupted data when the base address had certain bit patterns.

## Fix

Fixed in `SLOW32ISelDAGToDAG.cpp` by intercepting `ISD::OR` nodes with the `disjoint` flag and converting them to `ADDI` instructions.

## Expected Output

```
ABC:123456789
123456789,987654321
123456789:ABC:987654321
```
