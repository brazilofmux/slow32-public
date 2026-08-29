#!/bin/bash
# Build the COBOL runtime for SLOW-32.
#
# cobol/ is in the tree's ordinary universe, so the host toolchain builds
# guest code -- the same arrangement fortran/ and every other app use.
set -eu
HERE="$(cd "$(dirname "$0")" && pwd)"
ROOT="$(cd "$HERE/../.." && pwd)"
LLVM_BIN="${LLVM_BIN:-$HOME/llvm-project/build/bin}"
OPT="${OPT:--O1}"

"$LLVM_BIN/clang" -target slow32-unknown-none -S -emit-llvm $OPT \
    -nostdinc -fno-builtin -I"$ROOT/runtime/include" \
    "$HERE/libcob.c" -o "$HERE/libcob.ll"
"$LLVM_BIN/llc" -mtriple=slow32-unknown-none "$HERE/libcob.ll" -o "$HERE/libcob.s"
"$ROOT/tools/assembler/slow32asm" "$HERE/libcob.s" "$HERE/libcob.s32o" >/dev/null
echo "built: $HERE/libcob.s32o"
