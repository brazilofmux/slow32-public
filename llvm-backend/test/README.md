# SLOW32 LLVM Backend Tests

This directory contains LLVM backend tests for the SLOW32 target. These tests are meant to be run with LLVM's `lit` test runner in the `~/llvm-project` tree.

## Test Files

- **addressing.ll** - Tests various addressing modes and memory operations
- **branches.ll** - Tests branch instructions and control flow
- **branch-folder.mir** - Tests branch folding optimization pass
- **branch-successors.mir** - Tests branch successor handling
- **i64-call-return.ll** - Tests 64-bit integer calling convention
- **immediates.ll** - Tests immediate value handling
- **jal-pseudo-expansion.mir** - Tests JAL pseudo-instruction expansion
- **large-frame.ll** - Tests stack frame handling for large frames
- **load-addr-fold.mir** - Tests load-address folding optimization
- **umul-lohi.ll** - Tests unsigned multiply-low-high (critical for 64-bit division)
- **varargs.ll** - Tests variadic function calling convention

## Running Tests

From `~/llvm-project/build`:

```bash
# Run all SLOW32 tests
./bin/llvm-lit ../llvm/test/CodeGen/SLOW32

# Run specific test
./bin/llvm-lit ../llvm/test/CodeGen/SLOW32/umul-lohi.ll

# Run with verbose output
./bin/llvm-lit -v ../llvm/test/CodeGen/SLOW32
```

## Important Notes

- These tests are synced from `~/llvm-project/llvm/test/CodeGen/SLOW32/` via the backup script
- The `umul-lohi.ll` test is critical - it validates the fix for the i64 division bug
- Tests use FileCheck patterns to verify correct assembly generation
- `.mir` tests use MIR (Machine IR) format for testing specific optimization passes
