#!/bin/bash
# run-gen.sh FIRST COUNT [STATEMENTS] -- differential testing on generated
# programs: gen-arith.py seeds FIRST..FIRST+COUNT-1, each built and run
# here (compile.sh, slow32-fast) and under GnuCOBOL -std=cobol85 (the
# harness's oracle images, one container for the whole batch), output
# compared line by line.  Prints one line per seed and keeps the work
# directory (under cobol/out, which the container can mount) only when
# something disagreed.
set -eu
HERE="$(cd "$(dirname "$0")" && pwd)"
CDIR="$(cd "$HERE/../.." && pwd)"
ROOT="$(cd "$CDIR/.." && pwd)"
FIRST=${1:?usage: run-gen.sh FIRST COUNT [STATEMENTS]}
COUNT=${2:?usage: run-gen.sh FIRST COUNT [STATEMENTS]}
NSTMT=${3:-60}
ENGINE=$(command -v podman || command -v docker)
EMU="$ROOT/tools/emulator/slow32-fast"

mkdir -p "$CDIR/out"
W="$(mktemp -d "$CDIR/out/gen.XXXXXX")"
last=$((FIRST + COUNT - 1))

for s in $(seq "$FIRST" "$last"); do
    python3 "$HERE/gen-arith.py" "$s" "$NSTMT" > "$W/g$s.cbl"
    if "$CDIR/compile.sh" -free -std=85 "$W/g$s.cbl" -o "$W/g$s.s32x" > "$W/g$s.cclog" 2>&1; then
        "$EMU" "$W/g$s.s32x" 2>/dev/null | sed '/^Starting execution at PC/,$d' > "$W/g$s.out" || true
    else
        echo "BUILD-FAILED" > "$W/g$s.out"
    fi
done

# the oracle: one container builds and runs the whole batch
"$ENGINE" run --rm -v "$ROOT:$ROOT" -w "$W" gnucobol:4.0-builder sh -c "
for s in \$(seq $FIRST $last); do
    if cobc -x -std=cobol85 -free g\$s.cbl -o g\$s.orc > g\$s.orclog 2>&1; then
        ./g\$s.orc > g\$s.orcout 2>/dev/null || true
    else
        echo BUILD-FAILED > g\$s.orcout
    fi
done"

# Known oracle defects, recorded in docs/oracles.md, are counted apart:
# - a REMAINDER after its own quotient's SIZE ERROR: GnuCOBOL stores one,
#   X3.23-1985 VI-81 DIVIDE general rule 8a leaves it unchanged
#   (tests/free/divremse).
classify() {  # classify ours oracle: prints "<real> <known>"
    python3 - "$1" "$2" <<'PY'
import sys
a = open(sys.argv[1]).read().splitlines()
b = open(sys.argv[2]).read().splitlines()
real = known = 0
if len(a) != len(b):
    print(max(len(a), len(b)), 0); sys.exit()
se = {l.split()[0] for l in a if " divrem SIZE ERROR " in l}
for x, y in zip(a, b):
    if x == y:
        continue
    f = x.split()
    if len(f) >= 3 and f[1] == "divrem" and f[2] == "rem" and f[0] in se:
        known += 1
    else:
        real += 1
print(real, known)
PY
}
bad=0
for s in $(seq "$FIRST" "$last"); do
    read -r real known < <(classify "$W/g$s.out" "$W/g$s.orcout")
    lines=$(wc -l < "$W/g$s.out" | tr -d ' ')
    if [ "$real" = 0 ]; then
        echo "seed $s: agree ($lines lines${known:+; $known known oracle defects})" | sed 's/; 0 known oracle defects//'
    else
        echo "seed $s: DISAGREE on $real lines (and $known known oracle defects)"
        bad=$((bad + 1))
    fi
done
if [ "$bad" = 0 ]; then
    rm -rf -- "$W"
    echo "all $COUNT agree"
else
    echo "$bad of $COUNT disagree; kept $W"
fi
