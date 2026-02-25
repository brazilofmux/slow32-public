#!/bin/bash
set -e

# Paths (relative to nano/)
LLVM_BIN=~/llvm-project/build/bin
CLANG=$LLVM_BIN/clang
LLC=$LLVM_BIN/llc
ASM=../tools/assembler/slow32asm
LD=../tools/linker/s32-ld
TARGET=slow32-unknown-none
CFLAGS="-target $TARGET -S -emit-llvm -nostdinc -fno-builtin -I../runtime/include"

# Optimization level
OPT="${OPT:--O1}"

echo "=== Building nano (Text Editor) ==="

echo -n "  Compiling nano.c... "
$CLANG $CFLAGS $OPT src/nano.c -o src/nano.ll 2>&1
echo "OK"

echo -n "  Generating assembly... "
$LLC -mtriple=$TARGET src/nano.ll -o src/nano.s 2>&1
echo "OK"

echo -n "  Assembling... "
$ASM src/nano.s src/nano.s32o 2>&1
echo "OK"

echo -n "  Linking... "
$LD --mmio 64K --stack-size 128K --data-size 2M -o nano.s32x \
    ../runtime/crt0.s32o \
    src/nano.s32o \
    ../runtime/libc_mmio.s32a \
    ../runtime/libs32.s32a 2>&1
echo "OK"

echo ""
echo "Success! Run with:"
echo "  ../tools/emulator/slow32-fast nano.s32x [filename]"
