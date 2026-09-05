#!/usr/bin/env bash
# dfsort/tests/run.sh -- every deck on the host build and on SLOW-32, bytes compared,
# and against the checked-in .expected.  A deck is NAME.ctl + NAME.in (text) or
# NAME.bin (fixed) [+ NAME.in2] [+ NAME.args].  UPDATE=1 rewrites .expected from the host.
set -u
HERE="$(cd "$(dirname "$0")" && pwd)"; ROOT="$(cd "$HERE/../.." && pwd)"; D="$HERE/decks"; W="$HERE/tmp"; mkdir -p "$W"
EMU="${EMU:-$ROOT/tools/dbt/slow32-dbt}"
cc -std=c99 -O1 -w -o "$W/s32sort_host" "$HERE/../s32sort.c" || { echo "host build failed"; exit 1; }
"$ROOT/slow32cc" --libc=mmio -O2 "$HERE/../s32sort.c" -o "$W/s32sort.s32x" >/dev/null 2>&1 || { echo "guest build failed"; exit 1; }
pass=0; fail=0
for ctl in "$D"/*.ctl; do
    n="$(basename "$ctl" .ctl)"; ins=""
    for f in "$D/$n.in" "$D/$n.bin" "$D/$n.in2"; do [ -e "$f" ] && ins="$ins SORTIN=$f"; done
    args=""; [ -f "$D/$n.args" ] && args="$(cat "$D/$n.args")"
    "$W/s32sort_host" $ins "SORTOUT=$W/$n.host" "SYSIN=$ctl" $args 2>"$W/$n.hlog"; hrc=$?
    (cd "$W" && "$EMU" ./s32sort.s32x $ins "SORTOUT=$W/$n.guest" "SYSIN=$ctl" $args >"$W/$n.glog" 2>&1); grc=$?
    [ "${UPDATE:-0}" = 1 ] && cp "$W/$n.host" "$D/$n.expected"
    if [ $hrc -ne 0 ]; then echo "  $n: FAIL host rc=$hrc: $(tail -1 "$W/$n.hlog")"; fail=$((fail+1))
    elif ! cmp -s "$W/$n.host" "$W/$n.guest"; then echo "  $n: FAIL guest differs from host"; cmp "$W/$n.host" "$W/$n.guest" | head -1; grep -E 'fault|Error|s32sort:' "$W/$n.glog" | head -2; fail=$((fail+1))
    elif [ ! -f "$D/$n.expected" ]; then echo "  $n: FAIL no .expected (UPDATE=1 to write it)"; fail=$((fail+1))
    elif ! cmp -s "$W/$n.host" "$D/$n.expected"; then echo "  $n: FAIL differs from .expected"; fail=$((fail+1))
    else echo "  $n: PASS ($(grep -o '[0-9]* records in.*' "$W/$n.hlog"))"; pass=$((pass+1)); fi
done
echo "dfsort: $pass passed, $fail failed"; [ $fail = 0 ]
