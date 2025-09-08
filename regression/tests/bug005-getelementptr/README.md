# Bug #005: Getelementptr Not Generating Pointer Arithmetic

## Problem  
The LLVM backend is not generating ADD instructions for getelementptr operations.
This breaks ALL pointer arithmetic, array indexing, and struct field access.

## Test Description
Simple pointer increment test:
- Creates string "ABC"
- Uses pointer arithmetic to iterate through characters
- Expected: Outputs "ABC"
- Bug behavior: Outputs "AAA" (pointer never increments)

## Status
Currently FAILS - getelementptr doesn't generate arithmetic instructions

## Impact
This is CRITICAL - breaks:
- String operations
- Array indexing
- Loop iteration over arrays
- Struct field access