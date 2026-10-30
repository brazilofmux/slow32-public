#!/bin/bash
# libxbase.s32a: the interpreter's sources minus main.c (its REPL) and
# cliprt, compiled once -- what every compiled program links, so an
# installed compiler (the slow32:xbase image) never rebuilds the
# interpreter.  compile-prg.sh takes it as XBASE_LIB.
#
#   build-lib.sh [out.s32a]
#
# XBC_CC (clang, the default, or stage08), LLVM_BIN, S32_CC_S32X,
# S32_CC_EMU, S32_AS, S32_AR, S32_RT_INCLUDE as in compile-prg.sh.
set -euo pipefail
CLIP_DIR="$(cd "$(dirname "$0")" && pwd)"
ROOT="$(dirname "$CLIP_DIR")"
XBC_CC="${XBC_CC:-clang}"
LLVM_BIN="${LLVM_BIN:-$HOME/llvm-project/build/bin}"
CC_S32X="${S32_CC_S32X:-$ROOT/selfhost/stage08/cc.s32x}"
CC_EMU="${S32_CC_EMU:-$ROOT/tools/emulator/slow32-fast}"
ASM="${S32_AS:-$ROOT/tools/assembler/slow32asm}"
AR="${S32_AR:-$ROOT/tools/utilities/s32-ar}"
RT_INCLUDE="${S32_RT_INCLUDE:-$ROOT/runtime/include}"
OUT="${1:-$CLIP_DIR/out/libxbase.s32a}"
W="$(mktemp -d "${TMPDIR:-/tmp}/xbaselib.XXXXXX")"
trap 'rm -rf "$W"' EXIT
INCS="-I$RT_INCLUDE -I$ROOT/xbase/src -I$CLIP_DIR/runtime"
CFLAGS="-target slow32-unknown-none -S -emit-llvm -nostdinc -fno-builtin $INCS ${OPT:--O1} -DNDEBUG"
objs=()
for src in "$ROOT"/xbase/src/*.c "$CLIP_DIR/runtime/cliprt.c"; do
    b="$(basename "$src" .c)"
    [ "$b" = "main" ] && continue
    case "$XBC_CC" in
        clang)
            "$LLVM_BIN/clang" $CFLAGS "$src" -o "$W/$b.ll"
            "$LLVM_BIN/llc" -mtriple=slow32-unknown-none "$W/$b.ll" -o "$W/$b.s" ;;
        stage08)
            if ! "$CC_EMU" "$CC_S32X" $INCS -DNDEBUG "$src" "$W/$b.s" >/dev/null 2>"$W/$b.cclog"; then
                cat "$W/$b.cclog" >&2; exit 1
            fi ;;
        *) echo "XBC_CC: clang or stage08, not $XBC_CC" >&2; exit 2 ;;
    esac
    "$ASM" "$W/$b.s" "$W/$b.s32o" >/dev/null
    objs+=("$W/$b.s32o")
done
mkdir -p "$(dirname "$OUT")"
rm -f "$OUT"
"$AR" rcs "$OUT" "${objs[@]}"
echo "$OUT (${#objs[@]} objects, $XBC_CC)"
