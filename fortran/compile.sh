#!/bin/bash
# compile.sh -- Fortran (77, 90, 95; fixed or free form by the file's
# extension) to a SLOW-32 executable.
#   ./compile.sh prog.f [more.f90 ...] [-o prog.s32x]
# Each source: f77 -> .s -> slow32asm; then s32-ld with libf77 and the MMIO
# libc (--mmio is what carries STOP n out as the exit status).  Sources
# compile in the order given, so a module comes before what uses it; f77
# writes a module's .mod in the current directory and reads it from there,
# as gfortran does.  The output is named for the first source unless -o
# names it; nothing else is left behind.  Run the result with
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
USAGE="usage: compile.sh prog.f [more.f90 ...] [-o prog.s32x]"
srcs=(); out=""
while [ $# -gt 0 ]; do
    case "$1" in
        -o) [ $# -ge 2 ] || { echo "$USAGE" >&2; exit 2; }; out="$2"; shift ;;
        *.f|*.for|*.f77|*.F|*.f90|*.F90|*.f95|*.F95) srcs+=("$1") ;;
        *) echo "compile.sh: what is $1?" >&2; exit 2 ;;
    esac
    shift
done
[ ${#srcs[@]} -gt 0 ] || { echo "$USAGE" >&2; exit 2; }
[ -n "$out" ] || out="${srcs[0]%.*}.s32x"
[ -x "$S32_F77" ] && [ -f "$S32_LIBF77" ] || "$HERE/build.sh" >/dev/null
W="$(mktemp -d "${TMPDIR:-/tmp}/s32f77.XXXXXX")"
trap 'rm -rf "${W:?}"' EXIT
objs=(); i=0
for src in "${srcs[@]}"; do
    o="$W/$i"; i=$((i + 1))
    "$S32_F77" "$src" "$o.s"
    # slow32asm and s32-ld narrate on stdout: keep it for a failure
    "$S32_AS" "$o.s" "$o.s32o" >"$o.log" 2>&1 || { cat "$o.log" >&2; exit 1; }
    objs+=("$o.s32o")
done
"$S32_LD" -o "$out" --mmio 64K "$S32_RT/crt0.s32o" "${objs[@]}" "$S32_LIBF77" \
    "$S32_RT/libc_mmio.s32a" "$S32_RT/libs32.s32a" >"$W/ld.log" 2>&1 || { cat "$W/ld.log" >&2; exit 1; }
echo "$out"
