#!/bin/bash
# Build the three tube demos: vecscope (vec), fire (fb), sprites (ppu).
set -e
cd "$(dirname "$0")"

LLVM_BIN="${LLVM_BIN:-$HOME/llvm-project/build/bin}"
TARGET=slow32-unknown-none
CFLAGS="-target $TARGET -S -emit-llvm -nostdinc -fno-builtin -Wall -Wextra -I../runtime/include"
OPT="${OPT:--O1}"

for name in vecscope fire sprites; do
    echo -n "  $name... "
    "$LLVM_BIN/clang" $CFLAGS $OPT "$name.c" -o "$name.ll"
    "$LLVM_BIN/llc" -mtriple=$TARGET "$name.ll" -o "$name.s"
    ../tools/assembler/slow32asm "$name.s" "$name.s32o" >/dev/null
    ../tools/linker/s32-ld --mmio 64K -o "$name.s32x" \
        ../runtime/crt0.s32o "$name.s32o" \
        ../runtime/libc_mmio.s32a ../runtime/libs32.s32a
    rm -f "$name.ll" "$name.s" "$name.s32o"
    echo OK
done
echo "Run one, then attach the glass:"
echo "  ../tools/emulator/slow32-fast fire.s32x &   ../tools/s32-crt/s32-crt"
