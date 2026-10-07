#!/bin/bash
# asm-snapshot.sh OUTDIR -- every program we have, compiled to assembly:
# the harness's own (fixed/, free/, 2002/, warn/, with its flags), the
# Open Systems suite (~/open), majesty (~/majesty/src/cobol), CCVS-85
# (through ccvs-run.sh), and the X-COBOL programs that compile (through
# xcobol-survey.py, with and without -dialect=mf).  Two snapshots diff to
# nothing when a change leaves the generated code alone -- the check for
# a refactor of the compiler (docs/plans/frontend-pass.md).
#
#   asm-snapshot.sh /tmp/before;  (change);  asm-snapshot.sh /tmp/after
#   diff -r /tmp/before /tmp/after
#   tests/asm-equiv.py /tmp/before /tmp/after   (the same code, labels and data order aside)
#
#   SNAP_SKIP="ccvs xcobol"   leave those corpora out (they are the slow ones)
set -u
HERE="$(cd "$(dirname "$0")" && pwd)"; C="$HERE/.."
OUT="${1:?usage: asm-snapshot.sh OUTDIR}"
COBC="$C/out/s32-cobc"
SKIP=" ${SNAP_SKIP:-} "
export SOURCE_DATE_EPOCH=1767225600     # WHEN-COMPILED's time: one fixed instant (2026-01-01)
mkdir -p "$OUT"
case "$OUT" in /*) ;; *) OUT="$PWD/$OUT" ;; esac

one() {   # one out flags... -- source
    local o="$1"; shift
    "$COBC" "$@" -o "$o.s" >"$o.err" 2>&1 || rm -f "$o.s"
}

# the harness's programs, with the flags run-tests.sh gives them
for d in fixed free 2002 2014 warn; do
    mkdir -p "$OUT/tests-$d"
    for src in "$HERE/$d"/*.cbl; do
        n=$(basename "$src" .cbl)
        flags="-$d"; [ "$d" = 2002 ] && flags="-free -std=2002"; [ "$d" = 2014 ] && flags="-free -std=2014"
        if [ "$d" = warn ]; then
            flags="-fixed"; grep -q "^identification division" "$src" && flags="-free"
            case "$n" in *std2002*) flags="$flags -std=2002" ;; *std2014*) flags="$flags -std=2014" ;; esac
        fi
        case "$n" in mf-*|*-mf-*|ext-mf*) flags="$flags -dialect=mf" ;; esac
        one "$OUT/tests-$d/$n" $flags -I "$HERE/copy" "$src"
    done
done

# the Open Systems suite, as bi2 compiles it
if [ -d "$HOME/open" ]; then
    mkdir -p "$OUT/open"
    for src in "$HOME"/open/*/*.COB; do
        d=$(dirname "$src"); n=$(basename "$d")_$(basename "$src" .COB)
        one "$OUT/open/$n" -fixed -std=85 -I "$d" "$src"
    done
fi

# majesty, as its s32x/build.sh compiles it (-std=2002 where 85 refuses)
M="$HOME/majesty/src"
if [ -d "$M/cobol" ]; then
    mkdir -p "$OUT/majesty"
    for src in "$M"/cobol/*.cbl; do
        n=$(basename "$src" .cbl)
        one "$OUT/majesty/$n" -free -I "$M/copy" -I "$M/h" "$src"
        [ -f "$OUT/majesty/$n.s" ] || one "$OUT/majesty/$n" -free -std=2002 -I "$M/copy" -I "$M/h" "$src"
    done
fi

# CCVS-85: its programs exist only as ccvs-run.sh extracts them
case "$SKIP" in *" ccvs "*) ;; *)
    mkdir -p "$OUT/ccvs"
    w=$(CCVS_KEEP=1 "$HERE/ccvs-run.sh" 2>&1 | sed -n 's/^.*work directory: //p' | head -1)
    [ -z "$w" ] && w=$(ls -dt "$C"/out/ccvsrun.* 2>/dev/null | head -1)
    if [ -n "$w" ] && [ -d "$w" ]; then
        find "$w" -name '*.s' | while read -r s; do cp "$s" "$OUT/ccvs/$(basename "$(dirname "$s")")_$(basename "$s")"; done
        rm -rf "$w"
    fi ;;
esac

# X-COBOL: the programs that compile, standard and -dialect=mf
case "$SKIP" in *" xcobol "*) ;; *)
    for v in std mf; do
        mkdir -p "$OUT/xcobol-$v"
        f=""; [ $v = mf ] && f=-dialect=mf
        XCOBOL_FLAGS="$f" XCOBOL_ASM="$OUT/xcobol-$v" python3 "$HERE/xcobol-survey.py" "$OUT/xcobol-$v.tsv" >/dev/null 2>&1
    done ;;
esac

# the .file line names the source's path, and CCVS's sources sit in a
# fresh directory each run: it is taken out
find "$OUT" -name '*.s' -print0 | xargs -0 sed -i.bak '/^[[:space:]]*\.file[[:space:]]/d'
find "$OUT" -name '*.s.bak' -delete
echo "$(find "$OUT" -name '*.s' | wc -l | tr -d ' ') programs compiled into $OUT"
