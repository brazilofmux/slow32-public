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

echo "=== Building SLOW-32 Kermit ==="

echo -n "  Compiling kermit.c... "
$CLANG $CFLAGS $OPT src/kermit.c -o src/kermit.ll
$LLC -mtriple=$TARGET src/kermit.ll -o src/kermit.s
$ASM src/kermit.s src/kermit.s32o
echo "OK"

echo -n "  Linking... "
$LD --mmio 64K --stack-size 64K --data-size 1M -o kermit.s32x \
    ../runtime/crt0.s32o \
    src/kermit.s32o \
    ../runtime/libc_mmio.s32a \
    ../runtime/libs32.s32a
echo "OK"

echo ""
echo "Success! Receiver:  ../tools/emulator/slow32-fast kermit.s32x -r"
echo "         Sender:    ../tools/emulator/slow32-fast kermit.s32x -s PORT FILE..."
