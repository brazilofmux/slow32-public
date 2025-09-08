#!/bin/bash

# SLOW32 Regression Test Configuration
# This file defines paths and settings for the regression test suite

# Base directory for SLOW32 project
SLOW32_BASE="/ztank/secret/sdennis/slow-32"

# LLVM build directory (where llc lives)
LLVM_BUILD="/ztank/secret/sdennis/llvm-project/build"

# Tool paths
LLC="$LLVM_BUILD/bin/llc"
CLANG="$LLVM_BUILD/bin/clang"
SLOW32_COMPILE="$SLOW32_BASE/compiler/slow32-compile"
SLOW32_ASM="$SLOW32_BASE/assembler/slow32asm"
SLOW32_LD="$SLOW32_BASE/linker/s32-ld"
SLOW32_EMU="$SLOW32_BASE/emulator/slow32"

# Runtime components
CRT0="$SLOW32_BASE/runtime/crt0.s32o"
DEBUG_CHAR="$SLOW32_BASE/runtime/debug_char.s32o"
INTRINSICS="$SLOW32_BASE/runtime/intrinsics.s32o"
BUILTINS="$SLOW32_BASE/runtime/builtins.s32o"

# Test settings
TEST_TIMEOUT=2  # seconds before considering a test hung
VERBOSE=${VERBOSE:-0}  # Set VERBOSE=1 for detailed output

# Colors for output (if terminal supports it)
if [ -t 1 ]; then
    RED='\033[0;31m'
    GREEN='\033[0;32m'
    YELLOW='\033[1;33m'
    NC='\033[0m' # No Color
else
    RED=''
    GREEN=''
    YELLOW=''
    NC=''
fi