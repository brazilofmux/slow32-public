#!/bin/bash
set -e

LLVM_BIN="${LLVM_BIN:-$HOME/llvm-project/build/bin}"
CLANG=$LLVM_BIN/clang
LLC=$LLVM_BIN/llc
ASM=../tools/assembler/slow32asm
LD=../tools/linker/s32-ld
TARGET=slow32-unknown-none
CFLAGS="-target $TARGET -S -emit-llvm -nostdinc -fno-builtin -Wall -Wextra -I../runtime/include -Isrc"
OPT="${OPT:--O1}"

echo "=== Building SLOW-32 Rogue ==="

OBJECTS=""
for src in src/dungeon.c src/game.c src/save.c src/ui.c src/main.c; do
    base=$(basename "$src" .c)
    echo -n "  Compiling $base.c... "
    $CLANG $CFLAGS $OPT "$src" -o "src/$base.ll"
    $LLC -mtriple=$TARGET "src/$base.ll" -o "src/$base.s"
    $ASM "src/$base.s" "src/$base.s32o"
    OBJECTS="$OBJECTS src/$base.s32o"
    echo "OK"
done

echo -n "  Linking... "
$LD --mmio 64K --stack-size 128K --data-size 1M -o rogue.s32x \
    ../runtime/crt0.s32o \
    $OBJECTS \
    ../runtime/libc_mmio.s32a \
    ../runtime/libs32.s32a
echo "OK"

echo ""
echo "Success! Run with:"
echo "  ../tools/emulator/slow32-fast rogue.s32x"
