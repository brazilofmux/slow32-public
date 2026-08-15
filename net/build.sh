#!/bin/bash
set -e

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
LLVM_BIN="${LLVM_BIN:-$HOME/llvm-project/build/bin}"
CLANG="$LLVM_BIN/clang"
LLC="$LLVM_BIN/llc"
ASM="$ROOT/tools/assembler/slow32asm"
LD="$ROOT/tools/linker/s32-ld"
TARGET=slow32-unknown-none
CFLAGS="-target $TARGET -S -emit-llvm -I$ROOT/runtime/include -O1"

compile() {
    local src="$1"
    local base
    base="$(basename "$src" .c)"
    echo -n "  Compiling $base.c... "
    "$CLANG" $CFLAGS "$src" -o "$base.ll"
    "$LLC" -mtriple=$TARGET "$base.ll" -o "$base.s"
    "$ASM" "$base.s" "$base.s32o"
    "$LD" --mmio 64K -o "$base.s32x" \
        "$ROOT/runtime/crt0.s32o" "$base.s32o" \
        "$ROOT/runtime/libc_mmio.s32a" "$ROOT/runtime/libs32.s32a"
    echo "OK"
}

echo "=== Building SLOW-32 net examples ==="
cd "$(dirname "$0")"
compile echo_server.c
compile echo_client.c
echo ""
echo "Run with: ./run-tests.sh"
