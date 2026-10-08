#!/bin/bash
# run-ref.sh FIRST COUNT [STATEMENTS] -- generated programs against their own
# reference: gen-$GEN.py (GEN=national, the default) writes the program and,
# on stderr, the output the text's model gives it; the program is built
# (compile.sh, -std=2002) and run (slow32-fast) and must print exactly that.
# No oracle: for national data GnuCOBOL has no UTF-16, so the reference is
# the model written out from the text (gen-national.py's docstring).  A
# reference line ending in " split" marks a case where the model's
# code-unit truncation parts a surrogate pair; the program's line must
# match it without that word, and the count of such cases is reported.
# Keeps the work directory when anything disagrees.
set -eu
HERE="$(cd "$(dirname "$0")" && pwd)"
CDIR="$(cd "$HERE/../.." && pwd)"
ROOT="$(cd "$CDIR/.." && pwd)"
FIRST=${1:?usage: run-ref.sh FIRST COUNT [STATEMENTS]}
COUNT=${2:?usage: run-ref.sh FIRST COUNT [STATEMENTS]}
NSTMT=${3:-40}
GEN=${GEN:-national}
STD=${STD:-2014}   # zero-length literals
EMU="${EMU:-$ROOT/tools/emulator/slow32-fast}"
mkdir -p "$CDIR/out"
W="$(mktemp -d "$CDIR/out/ref.XXXXXX")"
last=$((FIRST + COUNT - 1)); bad=0; splits=0
for s in $(seq "$FIRST" "$last"); do
    if [ -n "${GENDIR:-}" ] && [ -f "$GENDIR/$GEN-$s.cbl" ]; then cp "$GENDIR/$GEN-$s.cbl" "$W/g$s.cbl"; cp "$GENDIR/$GEN-$s.ref" "$W/g$s.ref";   # pre-generated, as run-flag.sh names them (the fleet has no python3)
    elif [ -n "${GENDIR:-}" ]; then echo "seed $s: NOT GENERATED ($GENDIR/$GEN-$s.cbl)"; bad=$((bad + 1)); continue
    else python3 "$HERE/gen-$GEN.py" "$s" "$NSTMT" > "$W/g$s.cbl" 2> "$W/g$s.ref"; fi
    mkdir -p "$W/r$s"
    if "$CDIR/compile.sh" -free -std=$STD "$W/g$s.cbl" -o "$W/r$s/g.s32x" > "$W/r$s/cc.log" 2>&1; then
        (cd "$W/r$s" && "$EMU" -q -c 2000000000 g.s32x > out.txt 2>/dev/null) || true
    else
        echo "seed $s: BUILD FAILED ($(grep -m1 error "$W/r$s/cc.log" | cut -c1-120))"; bad=$((bad + 1)); continue
    fi
    sp=$(grep -c " split$" "$W/g$s.ref" || true); splits=$((splits + sp))
    LC_ALL=C sed 's/ split$//' "$W/g$s.ref" > "$W/r$s/ref.txt"
    if cmp -s "$W/r$s/out.txt" "$W/r$s/ref.txt"; then
        echo "seed $s: agrees ($(wc -l < "$W/r$s/out.txt" | tr -d ' ') lines, $sp split)"
        rm -rf "$W/r$s" "$W/g$s.cbl" "$W/g$s.ref"
    else
        echo "seed $s: DIFFERS ($(LC_ALL=C diff "$W/r$s/ref.txt" "$W/r$s/out.txt" | grep -c '^<') lines; see $W/r$s)"; bad=$((bad + 1))
    fi
done
echo "$((COUNT - bad)) of $COUNT agree, $splits reference lines part a surrogate pair"
[ $bad -eq 0 ] && rmdir "$W" 2>/dev/null
exit $((bad > 0))
