#!/bin/sh
# Sweep COPY_INLINE_MAX -- the size below which s32-cobc copies inline
# instead of calling memcpy (src/s32-cobc.c, GitHub #27).
#
#   ./sweep.sh [thresholds...]      default: 0 8 16 24 40
#   SRC=b0big.cbl ./sweep.sh ...    a different bench source
#
# Measure on BOTH engine families and do not tune on one of them.  The
# interpreters execute every instruction an inline copy runs, so they always
# want a bigger threshold; slow32-dbt recognises the memcpy entry point by
# name and substitutes a native stub, so past a handful of bytes the call is
# the cheaper one there.  The shipped value is the compromise, and the DBT is
# what runs the corpus.
#
# b3big (twelve MOVEs x 2.2M iterations) is the source that separates them;
# b0big (the bare PERFORM VARYING) isolates the compare and truncation paths.
set -eu
SRC=${SRC:-b3big.cbl}
HERE="$(cd "$(dirname "$0")" && pwd)"
CDIR="$(cd "$HERE/.." && pwd)"
ROOT="$(cd "$CDIR/.." && pwd)"
DBT=${S32_DBT:-$ROOT/tools/dbt/slow32-dbt}
FAST=${S32_FAST:-$ROOT/tools/emulator/slow32-fast}
CC=${CC:-cc}
REPS=${REPS:-7}
base="${SRC%.cbl}"
cd "$HERE"
med() { sort -n | awk '{v[NR]=$1} END{printf "%.3f", v[int((NR+1)/2)]}'; }
printf '%-8s %14s %10s %10s\n' thresh 'fast insns' 'fast s' 'dbt s'
for n in "${@:-0 8 16 24 40}"; do
    $CC -std=c99 -O1 -w -DCOPY_INLINE_MAX="$n" -I"$CDIR/src" -o "$CDIR/out/s32-cobc-sweep" \
        "$CDIR/src/s32-cobc.c" "$CDIR/src/picture.c" "$CDIR/src/picture_scan.c"
    "$CDIR/out/s32-cobc-sweep" -free -o "$base-$n.s" "$SRC"
    "$ROOT/tools/assembler/slow32asm" "$base-$n.s" "$base-$n.s32o" >/dev/null
    # builtins64.s32o supplies __muldi3 when libcob was built by the
    # self-hosted cc (no clang on the host); harmless when it was not.
    extra=""
    [ -f "$CDIR/out/builtins64.s32o" ] && extra="$CDIR/out/builtins64.s32o"
    "$ROOT/tools/linker/s32-ld" --mmio 64K --stack-size 128K --heap-size 8M -o "$base-$n.s32x" \
        "$ROOT/runtime/crt0.s32o" "$base-$n.s32o" "$CDIR/libcob/libcob.s32o" $extra \
        "$ROOT/runtime/libc_mmio.s32a" "$ROOT/runtime/libs32.s32a" >/dev/null
    insns=$("$FAST" "$base-$n.s32x" | awk '/Instructions executed/{print $3}')
    ft=$(i=0; while [ $i -lt "$REPS" ]; do /usr/bin/time -f %e "$FAST" "$base-$n.s32x" 2>&1 >/dev/null | tail -1; i=$((i+1)); done | med)
    dt=$(i=0; while [ $i -lt "$REPS" ]; do /usr/bin/time -f %e "$DBT"  "$base-$n.s32x" 2>&1 >/dev/null | tail -1; i=$((i+1)); done | med)
    printf '%-8s %14s %10s %10s\n' "$n" "$insns" "$ft" "$dt"
done
rm -f "$CDIR/out/s32-cobc-sweep"
