#!/bin/bash
# kern-differential.sh -- the hooks over libcob/kern.h against the guest
# code they replace (docs/dbt-hooks.md): tests/kern_diff.c drives
# cob_put_num_x and cob_get_num over random descriptors and values, and
# cob_sort_run over random runs, under
# slow32-fast, which never hooks, and slow32-dbt, which does; the outputs
# must be identical, end in the sentinel, and the DBT must have called
# both hooks (a hook that never ran proved nothing).
set -u
HERE="$(cd "$(dirname "$0")" && pwd)"; C="$HERE/.."; ROOT="$C/.."
: "${S32_FAST:=$ROOT/tools/emulator/slow32-fast}"
: "${S32_DBT:=$ROOT/tools/dbt/slow32-dbt}"
W="$C/out/kerndiff"; rm -rf "$W"; mkdir -p "$W"
"$C/libcob/build.sh" >/dev/null || exit 1
. "$C/cctool.sh"
s32_cc_obj "$W/kern_diff.s32o" "$HERE/kern_diff.c" -I"$C/libcob" || exit 1
"$ROOT/tools/linker/s32-ld" --mmio 64K -o "$W/kern_diff.s32x" "$ROOT/runtime/crt0.s32o" "$W/kern_diff.s32o" \
    "$C/libcob/libcob.s32o" $(s32_cc_builtins) "$ROOT/runtime/libc_mmio.s32a" "$ROOT/runtime/libs32.s32a" >/dev/null || exit 1
guest() { grep -E '^(put |sort |kern_diff: )'; }
"$S32_FAST" "$W/kern_diff.s32x" 2>/dev/null | guest > "$W/fast.txt"
"$S32_DBT" -s "$W/kern_diff.s32x" 2>"$W/dbt.stats" | guest > "$W/dbt.txt"
bad=0
grep -q '^kern_diff: done$' "$W/fast.txt" || { echo "kern-differential: no sentinel under slow32-fast"; bad=1; }
cmp -s "$W/fast.txt" "$W/dbt.txt" || { echo "kern-differential: slow32-dbt differs:"; diff "$W/fast.txt" "$W/dbt.txt" | head; bad=1; }
for f in cob_get_num cob_put_num_x cob_sort_run; do
    calls=$(awk -v f="$f" '$1 == f { print $4 }' "$W/dbt.stats")
    [ "${calls:-0}" -gt 0 ] || { echo "kern-differential: the $f hook was never called"; bad=1; }
done
[ $bad = 0 ] && echo "kern-differential: $(grep hash "$W/fast.txt" | sed 's/kern_diff: //'), hooks called: $(awk '$1 ~ /^cob_/ { printf "%s %s/%s declined  ", $1, $4, $6 }' "$W/dbt.stats")"
exit $bad
