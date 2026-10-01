#!/bin/bash
# run-gen.sh FIRST COUNT [STATEMENTS] -- differential testing on generated
# programs: gen-$GEN.py (GEN=arith, the default, or edit) seeds
# FIRST..FIRST+COUNT-1, each built and run
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
GEN=${GEN:-arith}
ENGINE=$(command -v podman || command -v docker)
EMU="$ROOT/tools/emulator/slow32-fast"

mkdir -p "$CDIR/out"
W="$(mktemp -d "$CDIR/out/gen.XXXXXX")"
last=$((FIRST + COUNT - 1))

for s in $(seq "$FIRST" "$last"); do
    python3 "$HERE/gen-$GEN.py" "$s" "$NSTMT" > "$W/g$s.cbl" 2> "$W/g$s.ref"
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
#   (tests/free/divremse);
# - an edited item's simple insertion characters: the 85 editing rules
#   (VI-34, VI-35, rules 7 and 8) replace one embedded in or immediately
#   right of a suppression or floating string while suppression lasts,
#   and leave a B outside a * string a space; GnuCOBOL keeps the
#   character, and prints such a B as '*' (tests/free/editins).  Only
#   those two shapes are counted apart: our keeping a character the rules
#   replace is still a disagreement;
# - a relation against a negative literal with more integer digits than
#   its subject, labelled neglit by gen-cond.py: the oracle reads the
#   literal unsigned (X3.23-1985 VI-55; tests/free/negcmp);
# - INSPECT, where a generator writes the expected line from the text
#   (gen-string.py, with inspect85.py) to g<seed>.ref: that reference
#   decides, and ours must equal it whatever the oracle says.
classify() {  # classify ours oracle [reference]: prints "<real> <known>"
    python3 - "$1" "$2" "${3:-}" <<'PY'
import sys, os
a = open(sys.argv[1]).read().splitlines()
b = open(sys.argv[2]).read().splitlines()
# a reference written out from the text (tests/gen/inspect85.py) decides
# the lines it covers: ours must be it -- even where the oracle agrees
# with us -- and an oracle that differs from it is counted apart
ref = {}                    # label -> its expected lines, in order (a DIVIDE REMAINDER shows two)
if sys.argv[3] and os.path.exists(sys.argv[3]):
    for l in open(sys.argv[3]).read().splitlines():
        ref.setdefault(l.split(" ", 1)[0], []).append(l)
real = known = 0
if len(a) != len(b):
    print(max(len(a), len(b)), 0); sys.exit()
se = {l.split()[0] for l in a if " divrem SIZE ERROR " in l}

def positions(pic):
    """one picture symbol per output position (CR and DB are two)"""
    out, i = [], 0
    while i < len(pic):
        if pic[i:i+2] in ("CR", "DB"):
            out += ["S", "S"]; i += 2
        else:
            out.append(pic[i]); i += 1
    return out

def insertion_known(x, y):
    """an edit line ("k PIC [...]") whose every difference is an insertion
    character the rules replace (ours: the fill, the reference: the
    character itself), or a B the reference prints as '*'"""
    fx, fy = x.split(" ", 2), y.split(" ", 2)
    if len(fx) < 3 or fx[:2] != fy[:2] or not fx[2].startswith("["):
        return False
    pos = positions(fx[1])
    vx, vy = fx[2][1:-1], fy[2][1:-1]
    if len(vx) != len(vy) or len(vx) != len(pos):
        return False
    fill = "*" if "*" in fx[1] else " "
    for i, (cx, cy) in enumerate(zip(vx, vy)):
        if cx == cy:
            continue
        sym = pos[i]
        if sym in ",0/" and cx == fill and cy == sym:
            continue                    # rules 7-8: replaced while suppression lasts
        if sym == "B" and cx == " " and cy == "*":
            continue                    # a B outside the * string stays a space
        if sym == "B" and cx == "*" and cy == " ":
            continue                    # a B in the * string takes the fill
        return False
    return True

def neglit_known(x, y):
    """a condition labelled neglit=T|F, the algebraic truth gen-cond.py
    computed: the oracle reads a negative literal with more integer digits
    than the subject as if unsigned.  Known only when ours is the truth."""
    fx, fy = x.split(), y.split()
    if len(fx) != 3 or len(fy) != 3 or fx[0] != fy[0] or not fx[2].startswith("neglit="):
        return False
    return fx[1] == fx[2][-1] and fy[1] != fx[1]

for x, y in zip(a, b):
    lab = x.split(" ", 1)[0]
    if ref.get(lab):
        if x != ref[lab].pop(0):
            real += 1
        elif y != x:
            known += 1
        continue
    if x == y:
        continue
    f = x.split()
    if len(f) >= 3 and f[1] == "divrem" and f[2] == "rem" and f[0] in se:
        known += 1
    elif insertion_known(x, y):
        known += 1
    elif neglit_known(x, y):
        known += 1
    else:
        real += 1
print(real, known)
PY
}
bad=0
for s in $(seq "$FIRST" "$last"); do
    read -r real known < <(classify "$W/g$s.out" "$W/g$s.orcout" "$W/g$s.ref")
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
