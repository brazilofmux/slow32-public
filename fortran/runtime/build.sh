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
# S32_AS / S32_RT_INCLUDE: the assembler and runtime headers when they are
# not at their tree paths (the slow32:fortran image builds under /opt/slow32).
S32_AS="${S32_AS:-$ROOT/tools/assembler/slow32asm}"
S32_RT_INCLUDE="${S32_RT_INCLUDE:-$ROOT/runtime/include}"

"$LLVM_BIN/clang" -target slow32-unknown-none -S -emit-llvm $OPT \
    -nostdinc -fno-builtin -I"$S32_RT_INCLUDE" \
    "$HERE/libf77.c" -o "$HERE/libf77.ll"
"$LLVM_BIN/llc" -mtriple=slow32-unknown-none "$HERE/libf77.ll" -o "$HERE/libf77.s"
"$S32_AS" "$HERE/libf77.s" "$HERE/libf77.s32o" >/dev/null
echo "built: $HERE/libf77.s32o"
