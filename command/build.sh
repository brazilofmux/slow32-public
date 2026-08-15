#!/bin/bash
set -e

LLVM_BIN="${LLVM_BIN:-$HOME/llvm-project/build/bin}"
CLANG=$LLVM_BIN/clang
LLC=$LLVM_BIN/llc
ASM=../tools/assembler/slow32asm
LD=../tools/linker/s32-ld
TARGET=slow32-unknown-none
CFLAGS="-target $TARGET -S -emit-llvm -nostdinc -fno-builtin -Wall -Wextra -I../runtime/include"
OPT="${OPT:--O1}"

echo "=== Building COMMAND.COM ==="

echo -n "  Compiling command.c... "
$CLANG $CFLAGS $OPT src/command.c -o src/command.ll
echo "OK"

echo -n "  Generating assembly... "
$LLC -mtriple=$TARGET src/command.ll -o src/command.s
echo "OK"

echo -n "  Assembling... "
$ASM src/command.s src/command.s32o
echo "OK"

echo -n "  Linking... "
$LD --mmio 64K --stack-size 64K --data-size 2M -o command.s32x \
    ../runtime/crt0.s32o \
    src/command.s32o \
    ../runtime/libc_mmio.s32a \
    ../runtime/libs32.s32a
echo "OK"

echo ""
echo "Success! Run with:"
echo "  ../tools/emulator/slow32-fast command.s32x"
