#!/bin/sh
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
med() { sort -n | awk '{v[NR]=$1} END{printf "%.3f", v[int((NR+1)/2)]}'; }
now() { python3 -c 'import time;print(time.time())'; }
el() { t0=$(now); "$@" >/dev/null 2>&1; t1=$(now); awk -v a=$t0 -v b=$t1 'BEGIN{printf "%.3f",b-a}'; }
for v in s32-cobc-pre2 s32-cobc; do
  build "$v" "b6-$v" b6; build "$v" "b0-$v" b0; build "$v" "b6big-$v" b6big
  i6=$("$FAST" b6-$v.s32x | awk '/Instructions executed/{print $3}')
  i0=$("$FAST" b0-$v.s32x | awk '/Instructions executed/{print $3}')
  d=$(i=0; while [ $i -lt 5 ]; do el "$DBT" b6big-$v.s32x; echo; i=$((i+1)); done | med)
  awk -v v="$v" -v a="$i6" -v b="$i0" -v d="$d" 'BEGIN{printf "%-16s per compare %6.0f   b6big dbt %s s\n", v, (a-b)/56164/4, d}'
done
