#!/bin/bash
# .bas -> C -> .s32x: s32-sbc writes C, clang compiles it for SLOW-32,
# and it links with the interpreter's runtime (sbasic's objects but
# main.s32o, and runtime/sbcrt.c).
#
#   compile-bas.sh input.bas [output.s32x]
#
#   LLVM_BIN   clang and llc (default ~/llvm-project/build/bin)
#   SBC_WORK   where the generated C and objects go (default out/)
#
# The interpreter's objects are the ones sbasic/build.sh made; run it
# first after changing sbasic/src.
set -euo pipefail

SBC_DIR="$(cd "$(dirname "$0")" && pwd)"
ROOT="$(dirname "$SBC_DIR")"
LLVM_BIN="${LLVM_BIN:-$HOME/llvm-project/build/bin}"
CLANG="$LLVM_BIN/clang"
LLC="$LLVM_BIN/llc"
ASM="$ROOT/tools/assembler/slow32asm"
LD="$ROOT/tools/linker/s32-ld"
RT="$ROOT/runtime"
SB="$ROOT/sbasic/src"
WORK="${SBC_WORK:-$SBC_DIR/out}"
TARGET=slow32-unknown-none
CFLAGS="-target $TARGET -S -emit-llvm -nostdinc -fno-builtin -I$RT/include -I$SB -I$SBC_DIR/runtime"
OPT="${OPT:--O1}"

if [ $# -lt 1 ]; then
    echo "Usage: $0 input.bas [output.s32x]"
    exit 1
fi
BAS="$1"
[ -f "$BAS" ] || { echo "not found: $BAS"; exit 1; }
BASE="$(basename "$BAS")"
BASE="${BASE%.*}"
OUTX="${2:-$WORK/$BASE.s32x}"
mkdir -p "$WORK"

bash "$SBC_DIR/build.sh" >/dev/null

# the runtime's own piece, rebuilt when stale
RTO="$WORK/sbcrt.s32o"
if [ ! -f "$RTO" ] || [ "$SBC_DIR/runtime/sbcrt.c" -nt "$RTO" ] || [ -n "$(find $SB/*.h $SBC_DIR/runtime/sbcrt.h -newer "$RTO" 2>/dev/null)" ]; then
    "$CLANG" $CFLAGS $OPT "$SBC_DIR/runtime/sbcrt.c" -o "$WORK/sbcrt.ll"
    "$LLC" -mtriple=$TARGET "$WORK/sbcrt.ll" -o "$WORK/sbcrt.s"
    "$ASM" "$WORK/sbcrt.s" "$RTO" >/dev/null
fi

"$SBC_DIR/s32-sbc" "$BAS" -o "$WORK/$BASE.c"
"$CLANG" $CFLAGS $OPT "$WORK/$BASE.c" -o "$WORK/$BASE.ll"
"$LLC" -mtriple=$TARGET "$WORK/$BASE.ll" -o "$WORK/$BASE.s"
"$ASM" "$WORK/$BASE.s" "$WORK/$BASE.s32o" >/dev/null

OBJS=()
for o in "$SB"/*.s32o; do
    [ "$(basename "$o")" = main.s32o ] || OBJS+=("$o")
done
"$LD" --mmio 64K --stack-size 1M --heap-size 16M -o "$OUTX" \
    "$RT/crt0.s32o" "$WORK/$BASE.s32o" "$RTO" "${OBJS[@]}" \
    "$RT/libc_mmio.s32a" "$RT/libs32.s32a"
