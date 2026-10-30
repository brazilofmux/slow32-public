#!/bin/bash
# Clipper 5.2 survey: each probe (probes/<area>-<slug>.prg, an original
# program exercising one feature) compiled by s32-clip and run, and run
# by Harbour (slow32:harbour-oracle, Dockerfile.harbour-oracle).  The
# CA-Clipper 5.2 guide decides where Harbour departs from Clipper.
#
#   tests/clipper/run-probes.sh [probes/x.prg ...]
#
# Verdicts: PASS (same output); DIFFER; NOCOMPILE (s32-clip refuses it);
# ORACLE (Harbour refuses it: the probe is wrong).
set -u
HERE="$(cd "$(dirname "$0")" && pwd)"
CLIP="$(cd "$HERE/../.." && pwd)"
EMU="${EMU:-$CLIP/../tools/emulator/slow32-fast}"
OUT="$CLIP/out/clipper"
mkdir -p "$OUT"
bash "$CLIP/build.sh" >/dev/null || exit 2
if [ $# -gt 0 ]; then PROBES=("$@"); else PROBES=("$HERE"/probes/*.prg); fi

pass=0; differ=0; nocomp=0; oracle=0
for prg in "${PROBES[@]}"; do
    n=$(basename "$prg" .prg)
    w=$(mktemp -d "$OUT/w.XXXX")
    cp "$prg" "$w/"
    podman run --rm -v "$w":/work slow32:harbour-oracle "$n.prg" > "$OUT/$n.hb" 2>&1
    rc=$?
    if [ $rc -eq 2 ] && grep -q ORACLE-COMPILE-FAIL "$OUT/$n.hb"; then
        printf '%-36s ORACLE (Harbour refuses it)\n' "$n"; oracle=$((oracle+1)); rm -rf "$w"; continue
    fi
    if ! bash "$CLIP/compile-prg.sh" "$prg" "$OUT/$n.s32x" > "$OUT/$n.cc.log" 2>&1; then
        printf '%-36s NOCOMPILE %s\n' "$n" "$(grep -m1 -E ': ' "$OUT/$n.cc.log" | sed 's/^[^:]*:[0-9]*: //' | cut -c1-50)"
        nocomp=$((nocomp+1)); rm -rf "$w"; continue
    fi
    (cd "$w" && timeout 30 "$EMU" -q --deny term "$OUT/$n.s32x" </dev/null > "$OUT/$n.clip" 2>&1)
    rm -rf "$w"
    if cmp -s "$OUT/$n.hb" "$OUT/$n.clip"; then
        printf '%-36s PASS\n' "$n"; pass=$((pass+1))
    else
        printf '%-36s DIFFER (diff %s/%s.{hb,clip})\n' "$n" "$OUT" "$n"; differ=$((differ+1))
    fi
done
echo "clipper probes: $pass pass, $differ differ, $nocomp do not compile, $oracle refused by the oracle (of ${#PROBES[@]})"
