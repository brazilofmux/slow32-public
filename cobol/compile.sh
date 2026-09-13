#!/bin/bash
# compile.sh -- COBOL source(s), C source(s) and objects to a SLOW-32 executable.
#   ./compile.sh [-free|-fixed] main.cbl [sub.cbl ...] [x.c ...] [x.s32o ...] [-I dir]... [-o prog.s32x]
# The first .cbl is the main program; further .cbl are subprogram
# modules (-m); .c files are built by the SLOW-32 C toolchain, which is
# how dateutil.c joins gl030 (docs/lowering.md: one convention, the C
# ABI, so COBOL, C and Fortran link with no glue).  Run the result with
# tools/emulator/slow32-fast or tools/dbt/slow32-dbt.
set -eu
HERE="$(cd "$(dirname "$0")" && pwd)"
ROOT="$(cd "$HERE/.." && pwd)"
fmt="-fixed"; out=""; incs=""
mains=(); subs=(); cs=(); objs=()
while [ $# -gt 0 ]; do
    case "$1" in
        -free|-fixed) fmt="$1" ;;
        -o) out="$2"; shift ;;
        -I) incs="$incs -I$2"; shift ;;
        -I*) incs="$incs $1" ;;
        *.cbl) if [ ${#mains[@]} -eq 0 ]; then mains+=("$1"); else subs+=("$1"); fi ;;
        *.c) cs+=("$1") ;;
        *.s32o) objs+=("$1") ;;
        *) echo "compile.sh: what is $1?" >&2; exit 2 ;;
    esac
    shift
done
[ ${#mains[@]} -eq 1 ] || { echo "usage: compile.sh [-free|-fixed] main.cbl [sub.cbl ...] [x.c ...] [x.s32o ...] [-o prog.s32x]" >&2; exit 2; }
main="${mains[0]}"
[ -n "$out" ] || out="${main%.cbl}.s32x"
# Where the pieces are.  Defaults are the tree; the slow32:cobol image sets
# S32_COBC/S32_LIBCOB/S32_AS/S32_LD/S32_RT to its /opt/slow32 install (the
# same knobs cctool.sh and tests/run-tests.sh honour).
: "${S32_COBC:=$HERE/out/s32-cobc}"
: "${S32_LIBCOB:=$HERE/libcob/libcob.s32o}"
: "${S32_AS:=$ROOT/tools/assembler/slow32asm}"
: "${S32_LD:=$ROOT/tools/linker/s32-ld}"
: "${S32_RT:=$ROOT/runtime}"
[ -x "$S32_COBC" ] && [ -f "$S32_LIBCOB" ] || "$HERE/build.sh" >/dev/null
. "$HERE/cctool.sh"
base="${out%.s32x}"
link=()
"$S32_COBC" $fmt $incs -o "$base.s" "$main"
"$S32_AS" "$base.s" "$base.s32o" >/dev/null
link+=("$base.s32o")
i=0
for f in "${subs[@]+"${subs[@]}"}"; do
    i=$((i+1))
    "$S32_COBC" $fmt $incs -m -o "$base-$i.s" "$f"
    "$S32_AS" "$base-$i.s" "$base-$i.s32o" >/dev/null
    link+=("$base-$i.s32o")
done
for f in "${cs[@]+"${cs[@]}"}"; do
    i=$((i+1))
    s32_cc_obj "$base-$i.s32o" "$f" $incs
    link+=("$base-$i.s32o")
done
# __muldi3 when the self-hosted cc compiled any of this; empty otherwise
builtins=$(s32_cc_builtins)
if [ -n "$builtins" ]; then link+=("$builtins"); fi
for f in "${objs[@]+"${objs[@]}"}"; do link+=("$f"); done
# The MMIO libc: files (fopen and friends) live only there, and the
# linker's --mmio gives the emulator the ring buffers to serve them.
"$S32_LD" --mmio 64K --stack-size 256K --heap-size 64M -o "$out" "$S32_RT/crt0.s32o" "${link[@]}" \
    "$S32_LIBCOB" "$S32_RT/libc_mmio.s32a" "$S32_RT/libs32.s32a" >/dev/null
echo "$out"
