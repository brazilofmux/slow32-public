#!/bin/bash
# wide-differential.sh [seeds] [statements] -- 31-digit arithmetic against
# GnuCOBOL (docs/wide.md): tests/wide-gen.py writes COMPUTE statements
# over random items of 1-31 digits (DISPLAY, BINARY, PACKED-DECIMAL,
# random scales and signs; + - * and one level of parentheses) into random
# receivers, ROUNDED or not, each reporting its value or its size error;
# both compilers run it and every line is compared.  Division is left out
# (the intermediate precision is the implementor's), and so are BINARY
# receivers past 18 digits: GnuCOBOL checks those against the sixteen-byte
# field, not the PICTURE, and reports no size error where the value
# overflows the PICTURE (ISSUES-117).  Needs podman and the
# gnucobol:4.0-builder image.
HERE="$(cd "$(dirname "$0")" && pwd)"; C="$HERE/.."
N=${1:-10}; M=${2:-80}
W="$C/out/widediff"; rm -rf "$W"; mkdir -p "$W"
tot=0; bad=0
for seed in $(seq 1 "$N"); do
    python3 "$HERE/wide-gen.py" "$seed" "$M" > "$W/wd$seed.cbl"
    "$C/compile.sh" -free -std=2002 "$W/wd$seed.cbl" -o "$W/wd$seed.s32x" >/dev/null 2>&1 || { echo "seed $seed: compile failed"; bad=1; continue; }
    (cd "$W" && ~/slow-32/tools/emulator/slow32 wd$seed.s32x 2>/dev/null | awk '/^Starting execution/{c=1;next} /^HALT at|^Program halted|^Exit code/{c=0} c && NF') > "$W/us$seed.txt"
    podman run --rm -v "$W:/w" -w /w gnucobol:4.0-builder sh -c "cobc -x -free -std=cobol2002 wd$seed.cbl -o wd$seed && ./wd$seed" > "$W/them$seed.txt" 2>&1
    d=$(diff "$W/us$seed.txt" "$W/them$seed.txt" | grep -c '^<')
    tot=$((tot + $(wc -l < "$W/us$seed.txt"))); [ "$d" = 0 ] || { echo "seed $seed: $d lines differ"; bad=1; }
done
echo "wide-differential: $tot statements over $N seeds, $([ $bad = 0 ] && echo "all agree with GnuCOBOL" || echo "DIFFERENCES above")"
exit $bad
