#!/bin/bash
# .prg -> C -> .s32x: s32-xbc writes C, a SLOW-32 C compiler compiles it,
# and it links with the interpreter's runtime (everything but its REPL).
#
#   compile-prg.sh input.prg [output.s32x] [more.prg ...]
#
# In the tree everything comes from the tree; each piece can be pointed
# elsewhere, which is how an installed copy (the slow32:xbase image) runs it:
#   XBC_CC        clang (default) or stage08: the compiler for the program's
#                 own C (stage08 = selfhost/stage08/cc.s32x on the emulator)
#   LLVM_BIN      clang and llc (default ~/llvm-project/build/bin)
#   S32_CC_S32X   stage08's cc.s32x;  S32_CC_EMU  the engine it runs on
#   S32_XBC       s32-xbc (default: this directory's, rebuilt when stale)
#   S32_AS S32_LD the assembler and linker
#   S32_RT        crt0.s32o, libc_mmio.s32a, libs32.s32a;  S32_RT_INCLUDE  their headers
#   XBASE_INCLUDE the interpreter's and cliprt's headers (the generated C
#                 includes cliprt.h)
#   XBASE_LIB     the interpreter's objects minus main.c, and cliprt, as one
#                 archive (build-lib.sh makes it): nothing of the
#                 interpreter is compiled per program
#   XBC_WORK      where the generated C and objects go (default out/)
set -euo pipefail

CLIP_DIR="$(cd "$(dirname "$0")" && pwd)"
ROOT="$(dirname "$CLIP_DIR")"
XBC_CC="${XBC_CC:-clang}"
LLVM_BIN="${LLVM_BIN:-$HOME/llvm-project/build/bin}"
CLANG="$LLVM_BIN/clang"
LLC="$LLVM_BIN/llc"
CC_S32X="${S32_CC_S32X:-$ROOT/selfhost/stage08/cc.s32x}"
CC_EMU="${S32_CC_EMU:-$ROOT/tools/emulator/slow32-fast}"
XBC="${S32_XBC:-$CLIP_DIR/s32-xbc}"
ASM="${S32_AS:-$ROOT/tools/assembler/slow32asm}"
LD="${S32_LD:-$ROOT/tools/linker/s32-ld}"
RT="${S32_RT:-$ROOT/runtime}"
RT_INCLUDE="${S32_RT_INCLUDE:-$ROOT/runtime/include}"
XB_INCS="${XBASE_INCLUDE:-$ROOT/xbase/src:$CLIP_DIR/runtime}"
WORK="${XBC_WORK:-$CLIP_DIR/out}"
TARGET=slow32-unknown-none
INCS="-I$RT_INCLUDE"
IFS=: read -r -a _xi <<< "$XB_INCS"
for d in "${_xi[@]}"; do INCS="$INCS -I$d"; done
CFLAGS="-target $TARGET -S -emit-llvm -nostdinc -fno-builtin $INCS"
OPT="${OPT:--O1}"

if [ $# -lt 1 ]; then
    echo "Usage: $0 input.prg [output.s32x] [more.prg ...]"
    exit 1
fi

PRG="$1"
shift
if [ ! -f "$PRG" ]; then
    echo "not found: $PRG"
    exit 1
fi
BASE="$(basename "$PRG" .prg)"
BASE="$(basename "$BASE" .PRG)"
OUT=""
if [ $# -ge 1 ]; then
    case "$1" in
        *.prg|*.PRG) ;;
        *) OUT="$1"; shift ;;
    esac
fi
OUT="${OUT:-$CLIP_DIR/$BASE.s32x}"
GEN="$WORK/${BASE}.c"
mkdir -p "$WORK"

[ -n "${S32_XBC:-}" ] || bash "$CLIP_DIR/build.sh" >/dev/null
"$XBC" "$PRG" "$@" -o "$GEN"

# One C file to an object, by the chosen compiler
cc_one() {   # cc_one src.c obj.s32o
    local src=$1 obj=$2 s="${2%.s32o}.s"
    case "$XBC_CC" in
        clang)
            "$CLANG" $CFLAGS $OPT -DNDEBUG "$src" -o "${2%.s32o}.ll"
            "$LLC" -mtriple=$TARGET "${2%.s32o}.ll" -o "$s" ;;
        stage08)
            # cc.s32x narrates its optimiser on stderr: kept for a failure
            if ! "$CC_EMU" "$CC_S32X" $INCS "$src" "$s" >/dev/null 2>"${2%.s32o}.cclog"; then
                cat "${2%.s32o}.cclog" >&2; return 1
            fi ;;
        *) echo "XBC_CC: clang or stage08, not $XBC_CC" >&2; return 2 ;;
    esac
    "$ASM" "$s" "$obj" >/dev/null
}

if [ -n "${XBASE_LIB:-}" ]; then
    RUNTIME="$XBASE_LIB"
else
    # the interpreter's objects (everything except the REPL), by clang:
    # an object older than its source or the newest header is rebuilt
    RUNTIME=""
    NEWEST_H=$(ls -t "$ROOT/xbase/src"/*.h | head -1)
    for src in "$ROOT/xbase/src"/*.c; do
        b="$(basename "$src" .c)"
        [ "$b" = "main" ] && continue
        o="$ROOT/xbase/src/$b.s32o"
        if [ ! -f "$o" ] || [ "$src" -nt "$o" ] || [ "$NEWEST_H" -nt "$o" ]; then
            echo "  compile xbase $b.c"
            XBC_CC=clang cc_one "$src" "$o"
        fi
        RUNTIME="$RUNTIME $o"
    done
    echo "  compile cliprt"
    XBC_CC=clang cc_one "$CLIP_DIR/runtime/cliprt.c" "$WORK/cliprt.s32o"
    RUNTIME="$WORK/cliprt.s32o $RUNTIME"
fi

echo "  compile ${BASE}.c ($XBC_CC)"
cc_one "$GEN" "$WORK/${BASE}.s32o"

echo "  link $OUT"
# A Clipper-style program nests deep (a command through the interpreter,
# its expressions through the AST, a user function back into the program
# ...): DBU's main screen overflowed 128K.
"$LD" --mmio 64K --stack-size "${STACK_SIZE:-1M}" --data-size 8M --heap-size 64M -o "$OUT" \
    "$RT/crt0.s32o" \
    "$WORK/${BASE}.s32o" \
    $RUNTIME \
    "$RT/libc_mmio.s32a" \
    "$RT/libs32.s32a"
echo "OK"
