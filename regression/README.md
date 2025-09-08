# SLOW32 Regression Test Suite

## Overview
This regression test suite helps catch regressions as the SLOW32 LLVM backend evolves.

## Running Tests
```bash
# Run all tests
./run-tests.sh

# Run specific test(s)
./run-tests.sh feature-arithmetic
./run-tests.sh bug*

# Verbose output
VERBOSE=1 ./run-tests.sh
```

## Test Structure
```
regression/
├── run-tests.sh          # Main test runner
├── config.sh            # Configuration (paths, tools)
├── tests/               # Test cases
│   ├── bug001-*/        # Known bug tests
│   └── feature-*/       # Feature tests
└── results/             # Test outputs for debugging
```

## Test Files
Each test directory contains:
- `test.c` - The C source code to test
- `expected.txt` - Expected output (optional)
- `README.md` - Description of what's being tested
- `optimize` - Optimization level (0-3, default 0)
- `expect-fail` - Mark test as expected to fail
- `SKIP` - Skip this test with optional reason

## Current Status

### Working Features
- Basic arithmetic (add, sub, mul, div, rem)
- Function calls (register and stack args)
- Some optimized code patterns

### Known Issues
- Bug #001: Varargs arguments clobbered by lifetime intrinsics
- Bug #005: Volatile/unoptimized pointer arithmetic crashes LLC
- Cannot handle volatile loads with sign extension

## Adding Tests
1. Create a directory under `tests/`
2. Add `test.c` with your test code
3. Add `expected.txt` with expected output
4. Add `README.md` explaining the test
5. Run `./run-tests.sh your-test-name`

## Philosophy
- Test what works to catch regressions
- Document known failures for tracking
- Keep tests simple and focused
- Default to -O0 to test actual codegen, not optimizer