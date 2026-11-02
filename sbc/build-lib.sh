#!/bin/bash
# libsbasic.s32a: the interpreter's objects minus main.s32o (its REPL and
# command line), and sbcrt -- what every compiled program links, so an
# installed compiler (the slow32:basic image) never rebuilds the
# interpreter.  compile-bas.sh takes it as SBASIC_LIB.
#
#   build-lib.sh [out.s32a]
#
# The interpreter's objects are the ones sbasic/build.sh made (clang);
# sbcrt.c is compiled here with clang.  LLVM_BIN, S32_AS, S32_AR,
# S32_RT_INCLUDE as in compile-bas.sh.
set -euo pipefail
SBC_DIR="$(cd "$(dirname "$0")" && pwd)"
ROOT="$(dirname "$SBC_DIR")"
LLVM_BIN="${LLVM_BIN:-$HOME/llvm-project/build/bin}"
ASM="${S32_AS:-$ROOT/tools/assembler/slow32asm}"
AR="${S32_AR:-$ROOT/tools/utilities/s32-ar}"
RT_INCLUDE="${S32_RT_INCLUDE:-$ROOT/runtime/include}"
SB="$ROOT/sbasic/src"
OUT="${1:-$SBC_DIR/out/libsbasic.s32a}"
W="$(mktemp -d "${TMPDIR:-/tmp}/sbasiclib.XXXXXX")"
trap 'rm -rf "${W:?}"' EXIT
CFLAGS="-target slow32-unknown-none -S -emit-llvm -nostdinc -fno-builtin -I$RT_INCLUDE -I$SB -I$SBC_DIR/runtime -O2"
"$LLVM_BIN/clang" $CFLAGS "$SBC_DIR/runtime/sbcrt.c" -o "$W/sbcrt.ll"
"$LLVM_BIN/llc" -mtriple=slow32-unknown-none "$W/sbcrt.ll" -o "$W/sbcrt.s"
"$ASM" "$W/sbcrt.s" "$W/sbcrt.s32o" >/dev/null
objs=("$W/sbcrt.s32o")
for o in "$SB"/*.s32o; do
    [ "$(basename "$o")" = main.s32o ] || objs+=("$o")
done
[ ${#objs[@]} -gt 1 ] || { echo "build-lib.sh: no interpreter objects in $SB (run sbasic/build.sh)" >&2; exit 1; }
mkdir -p "$(dirname "$OUT")"
rm -f "$OUT"
"$AR" rc "$OUT" "${objs[@]}"
echo "OK  $OUT (${#objs[@]} objects)"
