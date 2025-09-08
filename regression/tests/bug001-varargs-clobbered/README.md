# Bug #001: Varargs Arguments Clobbered

## Problem
The LLVM backend incorrectly reuses argument registers (r3-r10) for llvm.lifetime.start/end 
intrinsic calls without first preserving the original argument values.

## Test Description
This test calls a varargs sum function with 3 arguments (10, 20, 30).
- Expected: Returns 60 (sum of 10+20+30)
- Bug behavior: r3 (count) gets overwritten with 24, causing wrong iteration count

## Status
Currently FAILS - r3 is clobbered by llvm.lifetime intrinsic

## When Fixed
The test should output "OK" when the backend properly preserves argument registers.