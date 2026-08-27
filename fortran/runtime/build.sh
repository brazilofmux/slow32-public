#!/bin/bash
# Build the Fortran runtime for SLOW-32.
#
# fortran/ is in the tree's ordinary universe, so the host toolchain
# builds guest code -- the same arrangement every other app here uses.
set -eu
HERE="$(cd "$(dirname "$0")" && pwd)"
ROOT="$(cd "$HERE/../.." && pwd)"
LLVM_BIN="${LLVM_BIN:-$HOME/llvm-project/build/bin}"
OPT="${OPT:--O1}"

"$LLVM_BIN/clang" -target slow32-unknown-none -S -emit-llvm $OPT \
    -nostdinc -fno-builtin -I"$ROOT/runtime/include" \
    "$HERE/libf77.c" -o "$HERE/libf77.ll"
"$LLVM_BIN/llc" -mtriple=slow32-unknown-none "$HERE/libf77.ll" -o "$HERE/libf77.s"
"$ROOT/tools/assembler/slow32asm" "$HERE/libf77.s" "$HERE/libf77.s32o" >/dev/null
echo "built: $HERE/libf77.s32o"
