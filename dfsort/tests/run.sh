#!/usr/bin/env bash
# dfsort/tests/run.sh -- every deck on the host build and on SLOW-32, bytes compared,
# and against the checked-in .expected files.  A deck is NAME.ctl + NAME.in (text) or
# NAME.bin (fixed) [+ NAME.in2] [+ NAME.args].  In NAME.args, @OUT@ stands for the run's
# output stem, so extra data sets (OUTFIL FNAMES, JOINKEYS F1/F2 outputs) are
# `DD=@OUT@.dd`; SORTOUT is only given when the deck does not say NOSORTOUT.  Every
# output file NAME.host* is compared with NAME.guest* and with decks/NAME.expected*.
# UPDATE=1 rewrites the .expected files from the host.
set -u
HERE="$(cd "$(dirname "$0")" && pwd)"; ROOT="$(cd "$HERE/../.." && pwd)"; D="$HERE/decks"; W="$HERE/tmp"; mkdir -p "$W"
EMU="${EMU:-$ROOT/tools/dbt/slow32-dbt}"
cc -std=c99 -O1 -w -o "$W/s32sort_host" "$HERE/../s32sort.c" || { echo "host build failed"; exit 1; }
"$ROOT/slow32cc" --libc=mmio -O2 "$HERE/../s32sort.c" -o "$W/s32sort.s32x" >/dev/null 2>&1 || { echo "guest build failed"; exit 1; }
pass=0; fail=0
for ctl in "$D"/*.ctl; do
    n="$(basename "$ctl" .ctl)"; ins=""
    for f in "$D/$n.in" "$D/$n.bin" "$D/$n.in2"; do [ -e "$f" ] && ins="$ins SORTIN=$f"; done
    rm -f "$W/$n.host"* "$W/$n.guest"*
    run() {  # run STEM LOGFILE -> rc ; host or guest by $1's tool
        local tool="$1" stem="$2" log="$3" args="" so
        so="SORTOUT=$stem"
        if [ -f "$D/$n.args" ]; then args="$(sed "s|@OUT@|$stem|g; s|@D@|$D|g" "$D/$n.args")"; case "$args" in *NOSORTOUT*) so=""; args="${args//NOSORTOUT/}";; esac; fi
        (cd "$W" && $tool $ins $so "SYSIN=$ctl" $args) >"$log" 2>&1
    }
    run "$W/s32sort_host" "$W/$n.host" "$W/$n.hlog"; hrc=$?
    run "$EMU ./s32sort.s32x" "$W/$n.guest" "$W/$n.glog"; grc=$?
    if [ $hrc -ne 0 ]; then echo "  $n: FAIL host rc=$hrc: $(tail -1 "$W/$n.hlog")"; fail=$((fail+1)); continue; fi
    ok=1; outs=0
    for h in "$W/$n.host"*; do
        [ -e "$h" ] || continue; outs=$((outs+1)); suf="${h#$W/$n.host}"; g="$W/$n.guest$suf"; e="$D/$n.expected$suf"
        [ "${UPDATE:-0}" = 1 ] && cp "$h" "$e"
        if ! cmp -s "$h" "$g"; then echo "  $n$suf: guest differs from host"; grep -E 'fault|Error|s32sort:' "$W/$n.glog" | head -2; ok=0
        elif [ ! -f "$e" ]; then echo "  $n$suf: no .expected (UPDATE=1 to write it)"; ok=0
        elif ! cmp -s "$h" "$e"; then echo "  $n$suf: differs from .expected"; ok=0; fi
    done
    [ $outs -eq 0 ] && { echo "  $n: no output files"; ok=0; }
    if [ $ok = 1 ]; then echo "  $n: PASS ($outs file(s); $(grep -o '[0-9]* records in.*' "$W/$n.hlog"))"; pass=$((pass+1)); else echo "  $n: FAIL"; fail=$((fail+1)); fi
done
echo "dfsort: $pass passed, $fail failed"; [ $fail = 0 ]
