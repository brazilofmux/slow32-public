# Cleanup Summary - September 10, 2025

## What We Cleaned Up

### 1. Repository Organization

- Removed temporary build files from root directory (*.ll, *.s, *.s32o, *.s32x)
- Updated .gitignore to better exclude temporary files
- Enhanced Makefile clean target for more thorough cleanup

### 2. Helper Scripts Created

- `scripts/compile.sh` - Easy compilation of C programs to SLOW-32 executables
- `scripts/test-quick.sh` - Quick test to verify toolchain is working

### 3. Runtime Library Fix

- Added missing `yield` function stub to runtime
- Rebuilt libc.s32a archive with all necessary object files
- This fixes linking errors that were occurring with stdio functions

### 4. Documentation Updates

- Updated CLAUDE.md with:
  - New helper scripts documentation
  - Fixed Docker container command paths (clang/llc in /usr/local/bin)
  - Added quick test command

### 5. Current State

- All 14 regression tests pass (5 skipped due to missing test files)
- Toolchain builds cleanly with `make`
- Quick test script works correctly
- Docker containers are built and available (slow32-toolchain, slow32-emulator)

## Known Issues

### Printf Format String Issue
The printf function with format strings appears to halt early. Simple string printing works:

- ✅ `printf("Hello World\n")` - Works
- ⚠️ `printf("Value: %d\n", 42)` - May halt after first string

This appears to be a runtime library issue, not a compiler/backend problem.

## Repository Structure

- `~/slow-32` - Main development repository (private)
- `~/llvm-project` - LLVM with SLOW-32 backend (can't push upstream)
- `~/slow32-public` - Public-facing repository with clean presentation

## Next Steps When Needed

1. Investigate printf format string handling in runtime
2. Consider updating Docker containers with latest fixes
3. Sync improvements to slow32-public repository
4. Keep regression tests updated as new features/fixes are added
