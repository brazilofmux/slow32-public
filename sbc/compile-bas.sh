#!/bin/bash
# .bas -> C -> .s32x: s32-sbc writes C, a C compiler for SLOW-32 compiles
# it, and it links with the interpreter's runtime (sbasic's objects but
# main.s32o, and runtime/sbcrt.c).
#
#   compile-bas.sh input.bas [output.s32x]
#
#   SBC_CC        clang (default) or stage08: the compiler for the
#                 program's C (stage08 = selfhost/stage08/cc.s32x on the
#                 emulator, how an installed copy -- the slow32:basic
#                 image -- compiles, with no LLVM)
#   LLVM_BIN      clang and llc (default ~/llvm-project/build/bin)
#   S32_CC_S32X   stage08's cc.s32x;  S32_CC_EMU  the engine it runs on
#   S32_SBC       s32-sbc (default: this directory's, rebuilt when stale)
#   S32_AS S32_LD the assembler and linker
#   S32_RT        crt0.s32o, libc_mmio.s32a, libs32.s32a;  S32_RT_INCLUDE  their headers
#   SBASIC_INCLUDE  the interpreter's and sbcrt's headers, colon-separated
#                 (the generated C includes sbcrt.h)
#   SBASIC_LIB    the interpreter's objects minus main.c, and sbcrt, as
#                 one archive (build-lib.sh makes it): nothing of the
#                 interpreter is compiled per program.  Unset: sbasic's
#                 own objects (sbasic/build.sh made them) and sbcrt here
#   SBC_WORK      where the generated C and objects go (default out/)
set -euo pipefail

SBC_DIR="$(cd "$(dirname "$0")" && pwd)"
ROOT="$(dirname "$SBC_DIR")"
SBC_CC="${SBC_CC:-clang}"
LLVM_BIN="${LLVM_BIN:-$HOME/llvm-project/build/bin}"
CLANG="$LLVM_BIN/clang"
LLC="$LLVM_BIN/llc"
CC_S32X="${S32_CC_S32X:-$ROOT/selfhost/stage08/cc.s32x}"
CC_EMU="${S32_CC_EMU:-$ROOT/tools/emulator/slow32-fast}"
SBC="${S32_SBC:-$SBC_DIR/s32-sbc}"
ASM="${S32_AS:-$ROOT/tools/assembler/slow32asm}"
LD="${S32_LD:-$ROOT/tools/linker/s32-ld}"
RT="${S32_RT:-$ROOT/runtime}"
RT_INCLUDE="${S32_RT_INCLUDE:-$ROOT/runtime/include}"
SB="$ROOT/sbasic/src"
SB_INCS="${SBASIC_INCLUDE:-$SB:$SBC_DIR/runtime}"
WORK="${SBC_WORK:-$SBC_DIR/out}"
TARGET=slow32-unknown-none
INCS="-I$RT_INCLUDE"
IFS=: read -r -a _si <<< "$SB_INCS"
for d in "${_si[@]}"; do INCS="$INCS -I$d"; done
CFLAGS="-target $TARGET -S -emit-llvm -nostdinc -fno-builtin $INCS"
OPT="${OPT:--O2}"

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

# src.c -> obj.s32o with the chosen compiler
cc_one() {
    local src="$1" obj="$2" s="${2%.s32o}.s"
    case "$SBC_CC" in
        clang)
            "$CLANG" $CFLAGS $OPT "$src" -o "${2%.s32o}.ll"
            "$LLC" -mtriple=$TARGET "${2%.s32o}.ll" -o "$s" ;;
        stage08)
            # cc.s32x narrates its optimiser on stderr: kept for a failure
            if ! "$CC_EMU" "$CC_S32X" $INCS "$src" "$s" >/dev/null 2>"${2%.s32o}.cclog" || [ ! -s "$s" ]; then
                cat "${2%.s32o}.cclog" >&2; return 1
            fi ;;
        *) echo "SBC_CC: clang or stage08, not $SBC_CC" >&2; return 2 ;;
    esac
    "$ASM" "$s" "$obj" >/dev/null
}

[ -z "${S32_SBC:-}" ] && bash "$SBC_DIR/build.sh" >/dev/null

if [ -n "${SBASIC_LIB:-}" ]; then
    RUNTIME=("$SBASIC_LIB")
else
    # the runtime's own piece, rebuilt when stale; the interpreter's objects
    RTO="$WORK/sbcrt.s32o"
    if [ ! -f "$RTO" ] || [ "$SBC_DIR/runtime/sbcrt.c" -nt "$RTO" ] || [ -n "$(find $SB/*.h $SBC_DIR/runtime/sbcrt.h -newer "$RTO" 2>/dev/null)" ]; then
        "$CLANG" $CFLAGS $OPT "$SBC_DIR/runtime/sbcrt.c" -o "$WORK/sbcrt.ll"
        "$LLC" -mtriple=$TARGET "$WORK/sbcrt.ll" -o "$WORK/sbcrt.s"
        "$ASM" "$WORK/sbcrt.s" "$RTO" >/dev/null
    fi
    RUNTIME=("$RTO")
    for o in "$SB"/*.s32o; do
        [ "$(basename "$o")" = main.s32o ] || RUNTIME+=("$o")
    done
fi

"$SBC" "$BAS" -o "$WORK/$BASE.c"
cc_one "$WORK/$BASE.c" "$WORK/$BASE.s32o"

"$LD" --mmio 64K --stack-size 1M --heap-size 16M -o "$OUTX" \
    "$RT/crt0.s32o" "$WORK/$BASE.s32o" "${RUNTIME[@]}" \
    "$RT/libc_mmio.s32a" "$RT/libs32.s32a"
