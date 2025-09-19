#!/bin/bash
# Simple script to compile a C file to SLOW-32 executable

if [ $# -lt 1 ]; then
    echo "Usage: $0 <input.c> [output] [--mmio SIZE]"
    echo "  Compiles a C file to SLOW-32 executable"
    echo "  Output defaults to input basename with .s32x extension"
    echo "  --mmio SIZE enables MMIO with specified size (e.g., 64K)"
    exit 1
fi

INPUT="$1"
BASE=$(basename "$INPUT" .c)
OUTPUT="${2:-$BASE.s32x}"

# Check for --mmio flag
MMIO_FLAG=""
LIBC_ARCHIVE="runtime/libc_debug.s32a"
if [ "$3" = "--mmio" ] && [ -n "$4" ]; then
    MMIO_FLAG="--mmio $4"
    LIBC_ARCHIVE="runtime/libc_mmio.s32a"
fi

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
NC='\033[0m' # No Color

echo "Compiling $INPUT -> $OUTPUT"

# Step 1: C to LLVM IR
echo -n "  Generating LLVM IR... "
~/llvm-project/build/bin/clang -target slow32-unknown-none -S -emit-llvm -O2 -Iruntime/include "$INPUT" -o "$BASE.ll"
if [ $? -ne 0 ]; then
    echo -e "${RED}FAILED${NC}"
    exit 1
fi
echo -e "${GREEN}OK${NC}"

# Step 2: LLVM IR to Assembly
echo -n "  Generating assembly... "
~/llvm-project/build/bin/llc -mtriple=slow32-unknown-none "$BASE.ll" -o "$BASE.s"
if [ $? -ne 0 ]; then
    echo -e "${RED}FAILED${NC}"
    exit 1
fi
echo -e "${GREEN}OK${NC}"

# Step 3: Assembly to Object
echo -n "  Assembling... "
./tools/assembler/slow32asm "$BASE.s" "$BASE.s32o"
if [ $? -ne 0 ]; then
    echo -e "${RED}FAILED${NC}"
    exit 1
fi
echo -e "${GREEN}OK${NC}"

# Step 4: Link
echo -n "  Linking... "
./tools/linker/s32-ld $MMIO_FLAG -o "$OUTPUT" runtime/crt0.s32o "$BASE.s32o" "$LIBC_ARCHIVE" runtime/libs32.s32a
if [ $? -ne 0 ]; then
    echo -e "${RED}FAILED${NC}"
    exit 1
fi
echo -e "${GREEN}OK${NC}"

echo "Success! Run with: ./tools/emulator/slow32 $OUTPUT"