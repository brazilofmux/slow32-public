#!/bin/bash
# compile.sh -- a fixed-form Fortran 77 source to a SLOW-32 executable.
#   ./compile.sh prog.f [-o prog.s32x]
# f77 -> .s -> slow32asm -> s32-ld with libf77 and the MMIO libc (--mmio
# is what carries STOP n out as the exit status).  Run the result with
# tools/dbt/slow32-dbt or tools/emulator/slow32-fast.
#
# Where the pieces are: the tree by default; the slow32:fortran image sets
# S32_F77/S32_LIBF77/S32_AS/S32_LD/S32_RT to its /opt/slow32 install (the
# same knobs tests/run-tests.sh honours).
set -eu
HERE="$(cd "$(dirname "$0")" && pwd)"
ROOT="$(cd "$HERE/.." && pwd)"
: "${S32_F77:=$HERE/out/f77}"
: "${S32_LIBF77:=$HERE/runtime/libf77.s32o}"
: "${S32_AS:=$ROOT/tools/assembler/slow32asm}"
: "${S32_LD:=$ROOT/tools/linker/s32-ld}"
: "${S32_RT:=$ROOT/runtime}"
src=""; out=""
while [ $# -gt 0 ]; do
    case "$1" in
        -o) out="$2"; shift ;;
        *.f|*.for|*.f77) src="$1" ;;
        *) echo "compile.sh: what is $1?" >&2; exit 2 ;;
    esac
    shift
done
[ -n "$src" ] || { echo "usage: compile.sh prog.f [-o prog.s32x]" >&2; exit 2; }
[ -n "$out" ] || out="${src%.*}.s32x"
[ -x "$S32_F77" ] && [ -f "$S32_LIBF77" ] || "$HERE/build.sh" >/dev/null
base="${out%.s32x}"
"$S32_F77" "$src" "$base.s"
"$S32_AS" "$base.s" "$base.s32o" >/dev/null
"$S32_LD" -o "$out" --mmio 64K "$S32_RT/crt0.s32o" "$base.s32o" "$S32_LIBF77" \
    "$S32_RT/libc_mmio.s32a" "$S32_RT/libs32.s32a" >/dev/null
echo "$out"
