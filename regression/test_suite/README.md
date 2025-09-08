# SLOW32 Backend Verification Test Suite

This directory contains a comprehensive test suite for validating SLOW32 backend features that are claimed to be working.

## Test Coverage

### test_arithmetic.ll
- Basic arithmetic operations (ADD, SUB, MUL, DIV)
- Remainder operation (SREM)
- Complex expressions combining multiple operations
- Expected result: PASS

### test_shifts.ll
- Shift left (SLL) with immediate and register operands
- Logical shift right (SRL) with immediate and register operands  
- Arithmetic shift right (SRA) with immediate and register operands
- Tests both positive and negative values for arithmetic shifts
- Expected result: PASS

### test_function_calls.ll
- Simple function calls with JAL/JALR
- Recursive functions (factorial)
- Functions with >8 arguments (tests stack passing)
- Deeply nested function calls (validates stack frame preservation)
- Expected result: PASS

### test_globals.ll
- Global variable read/write access
- Global constants
- Global arrays with getelementptr
- Read-modify-write patterns
- Expected result: PASS (may fail due to getelementptr bug)

### test_varargs.ll
- Variadic functions with SLOW32's char* va_list
- Multiple varargs counts (3, 5, 10 arguments)
- Tests both register and stack-passed varargs
- Expected result: PASS (requires clang with SLOW32 target)

### test_branches.ll
- Comparison operations (EQ, NE, LT, GT, LE, GE)
- Branch instructions (BEQ, BNE, BLT, BGE)
- Complex control flow with multiple branches
- Loops with conditional logic
- PHI nodes in branch convergence
- Expected result: PASS

## Running Tests

After the build completes:

```bash
cd /ztank/secret/sdennis/slow-32/regression/test_suite
./run_tests.sh
```

The script will:
1. Compile each .ll file to SLOW32 assembly using llc
2. Assemble with slow32asm
3. Link with runtime libraries
4. Execute in the emulator
5. Check for "PASS" output

## Expected Results

Based on STATUS.md, these tests should all pass except:
- test_globals.ll may fail due to the getelementptr bug (bug005)
- Some tests may produce incorrect output due to character generation issues

## Notes

These tests validate that previously working features continue to work after changes to the backend. They serve as regression tests to catch any breakage during development.