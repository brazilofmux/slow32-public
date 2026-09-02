#!/bin/sh
# Cost of the #29 comparison shapes, before and after.
set -eu
R=/home/sdennis/slow-32; B=$R/cobol
FAST=$R/tools/emulator/slow32-fast; DBT=$R/tools/dbt/slow32-dbt
build() {
  "$B/out/$1" -free -o "$2.s" "$3.cbl"
  "$R/tools/assembler/slow32asm" "$2.s" "$2.s32o" >/dev/null
  "$R/tools/linker/s32-ld" --mmio 64K --stack-size 128K --heap-size 8M -o "$2.s32x" \
    "$R/runtime/crt0.s32o" "$2.s32o" "$B/libcob/libcob.s32o" "$B/out/builtins64.s32o" \
    "$R/runtime/libc_mmio.s32a" "$R/runtime/libs32.s32a" >/dev/null
}
for v in ${VARIANTS:-s32-cobc-pre29 s32-cobc}; do
  build "$v" "b5-$v" b5
  build "$v" "b0-$v" b0
  i5=$("$FAST" b5-$v.s32x | awk '/Instructions executed/{print $3}')
  i0=$("$FAST" b0-$v.s32x | awk '/Instructions executed/{print $3}')
  awk -v v="$v" -v a="$i5" -v b="$i0" 'BEGIN{printf "%-10s b5 %12d   per compare %6.0f\n", v, a, (a-b)/56164/4}'
done
echo "--- b5big wall clock, 5 reps median:"
med() { sort -n | awk '{v[NR]=$1} END{printf "%.3f", v[int((NR+1)/2)]}'; }
now() { python3 -c 'import time;print(time.time())'; }
el() { t0=$(now); "$@" >/dev/null 2>&1; t1=$(now); awk -v a=$t0 -v b=$t1 'BEGIN{printf "%.3f",b-a}'; }
for v in ${VARIANTS:-s32-cobc-pre29 s32-cobc}; do
  build "$v" "b5big-$v" b5big
  f=$(i=0; while [ $i -lt 5 ]; do el "$FAST" b5big-$v.s32x; echo; i=$((i+1)); done | med)
  d=$(i=0; while [ $i -lt 5 ]; do el "$DBT"  b5big-$v.s32x; echo; i=$((i+1)); done | med)
  printf '%-16s fast %8s s   dbt %8s s\n' "$v" "$f" "$d"
done
