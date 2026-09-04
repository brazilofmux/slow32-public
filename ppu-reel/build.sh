#!/bin/bash
# Build the ppu conformance reel.
set -e
cd "$(dirname "$0")"

LLVM_BIN="${LLVM_BIN:-$HOME/llvm-project/build/bin}"
TARGET=slow32-unknown-none
CFLAGS="-target $TARGET -S -emit-llvm -nostdinc -fno-builtin -Wall -Wextra -I../runtime/include"
OPT="${OPT:--O1}"

echo -n "  reel... "
"$LLVM_BIN/clang" $CFLAGS $OPT src/reel.c -o src/reel.ll
"$LLVM_BIN/llc" -mtriple=$TARGET src/reel.ll -o src/reel.s
../tools/assembler/slow32asm src/reel.s src/reel.s32o >/dev/null
../tools/linker/s32-ld --mmio 64K -o reel.s32x \
    ../runtime/crt0.s32o src/reel.s32o \
    ../runtime/libc_mmio.s32a ../runtime/libs32.s32a
rm -f src/reel.ll src/reel.s src/reel.s32o
echo OK
echo "Golden test:   bash tests/run-tests.sh"
echo "On the glass:  ../tools/emulator/slow32-fast reel.s32x --show   then ../tools/s32-crt-mac"
