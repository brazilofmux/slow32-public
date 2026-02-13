#!/bin/bash
set -e

# Paths (relative to zork/)
LLVM_BIN=~/llvm-project/build/bin
CLANG=$LLVM_BIN/clang
LLC=$LLVM_BIN/llc
ASM=../tools/assembler/slow32asm
LD=../tools/linker/s32-ld
TARGET=slow32-unknown-none
CFLAGS="-target $TARGET -S -emit-llvm -nostdinc -fno-builtin -I../runtime/include"

# Optimization level
OPT="${OPT:--O1}"

echo "=== Building MojoZork (Z-Machine Interpreter) ==="

echo -n "  Compiling mojozork.c... "
$CLANG $CFLAGS $OPT src/mojozork.c -o src/mojozork.ll 2>&1
echo "OK"

echo -n "  Generating assembly... "
$LLC -mtriple=$TARGET src/mojozork.ll -o src/mojozork.s 2>&1
echo "OK"

echo -n "  Assembling... "
$ASM src/mojozork.s src/mojozork.s32o 2>&1
echo "OK"

echo -n "  Linking... "
$LD --mmio 64K --stack-size 128K --data-size 2M -o zork.s32x \
    ../runtime/crt0.s32o \
    src/mojozork.s32o \
    ../runtime/libc_mmio.s32a \
    ../runtime/libs32.s32a 2>&1
echo "OK"

echo ""
echo "Success! Run with:"
echo "  printf 'look\nquit\ny\n' | ../tools/emulator/slow32-fast zork.s32x stories/minizork.z3"
