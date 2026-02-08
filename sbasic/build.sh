#!/bin/bash
set -e

# Paths (relative to sbasic/)
LLVM_BIN=~/llvm-project/build/bin
CLANG=$LLVM_BIN/clang
LLC=$LLVM_BIN/llc
ASM=../tools/assembler/slow32asm
LD=../tools/linker/s32-ld
TARGET=slow32-unknown-none
CFLAGS="-target $TARGET -S -emit-llvm -nostdinc -fno-builtin -I../runtime/include"

# Optimization level
OPT="${OPT:--O1}"

echo "=== Building SLOW BASIC ==="

# Auto-discover all .c files in src/
OBJECTS=""
for src in src/*.c; do
    base=$(basename "$src" .c)
    echo -n "  Compiling $base.c... "
    $CLANG $CFLAGS $OPT -DNDEBUG "$src" -o "src/$base.ll" 2>&1
    echo "OK"

    echo -n "  Generating assembly ($base)... "
    $LLC -mtriple=$TARGET "src/$base.ll" -o "src/$base.s" 2>&1
    echo "OK"

    echo -n "  Assembling $base... "
    $ASM "src/$base.s" "src/$base.s32o" 2>&1
    echo "OK"

    OBJECTS="$OBJECTS src/$base.s32o"
done

# Link with MMIO libc
echo -n "  Linking... "
$LD --mmio 64K -o sbasic.s32x \
    ../runtime/crt0.s32o \
    $OBJECTS \
    ../runtime/libc_mmio.s32a \
    ../runtime/libs32.s32a 2>&1
echo "OK"

echo ""
echo "Success! Run with:"
echo "  echo 'PRINT \"hello\"' | ../tools/emulator/slow32-fast sbasic.s32x"
