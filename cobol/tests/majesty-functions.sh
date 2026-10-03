#!/bin/bash
# majesty-functions.sh -- COBOL 2002 user-defined functions (cobol ISSUES-50)
# against real code: majesty's date family as it was written, with
# FUNCTION-ID, REPOSITORY and bare invocations, before majesty rewrote it
# to COBOL 85 subprograms (majesty e69e98b).  The originals are taken from
# majesty's history, unchanged, and built twice -- s32-cobc -std=2002 and
# the GnuCOBOL oracle -- and each pair must print the same bytes:
#
#   jerm       the family's driver: every day from 1601 to 200,000 after today
#   the trio   crgltrans, ldgltrans (COBOL 85, majesty's current source)
#              and the original exgltrans, over 3,000 SYNTHETIC
#              transactions generated here; each build keeps its own
#              indexed file format, so the three run as a set
#
# Needs ~/majesty, the gnucobol:4.0-builder/-runtime images, podman or
# docker, and the DBT (tools/dbt/slow32-dbt).  Nothing it makes is kept.
set -u
HERE="$(cd "$(dirname "$0")" && pwd)"; CDIR="$(cd "$HERE/.." && pwd)"; ROOT="$(cd "$CDIR/.." && pwd)"
MAJ="${MAJESTY:-$HOME/majesty}"; M="$MAJ/src"
ENG=""; for e in podman docker; do command -v $e >/dev/null && { ENG=$e; break; }; done
[ -n "$ENG" ] || { echo "majesty-functions: no podman or docker"; exit 2; }
DBT="$ROOT/tools/dbt/slow32-dbt"
W="$(mktemp -d)"; [ -n "${KEEP:-}" ] && echo "work: $W" >&2 || trap 'rm -rf "$W"' EXIT
fail=0

# the originals, from the commit before the rewrite
O="$W/orig"; mkdir -p "$O"
for f in $(git -C "$MAJ" show --name-only --format= e69e98b); do
    git -C "$MAJ" show "e69e98b^:$f" > "$O/$(basename "$f")" 2>/dev/null
done
# the original driver, with today's range: it sweeps from 1601 since the
# date routines became the COBOL intrinsics (majesty c0ae220, whose jerm
# CALLs the rewritten family; this one is the FUNCTION family's).  The
# old sweep began in the 1400s and stopped itself at its first year-end
# check: 302 lines on both sides, byte-identical, a pass that covered
# nothing until the line count below was required.
python3 - "$O/jerm.cbl" <<'PY'
import sys
p = sys.argv[1]; s = open(p).read()
a = "    subtract 200000 from ld_today giving ld_lower.\n"
assert s.count(a) == 1 and s.count("        if 0 < ld\n") == 1, "jerm.cbl is not the original the clamp expects"
s = s.replace(a, a + "    if ld_lower < 0 move 0 to ld_lower end-if.\n").replace("        if 0 < ld\n", "        if 0 <= ld\n")
open(p, "w").write(s)
PY
FAM="fielded_to_linear floor-div floor-divmod holidays isleapyear isvaliddate linear_to_fielded"

# the oracle's tree: sources, copybooks, the C bridge
G="$W/gnu"; mkdir -p "$G/copy" "$G/data"
cp "$O"/*.cbl "$M/cobol/crgltrans.cbl" "$M/cobol/ldgltrans.cbl" "$M/cobol/clinkages.cbl" "$M/c/dateutil.c" "$M/h/dateutil.h" "$G/"
cp "$M/copy/sgltrans" "$M/copy/stransaction" "$G/copy/"
S="$W/ours"; mkdir -p "$S/data"

# synthetic transactions: a fixed seed, dates across 1901-2065
python3 - "$S/data/transactions-in.txt" <<'PY'
import sys, random, datetime
random.seed(50); d0 = datetime.date(1901, 1, 1); out = []
for k in range(1, 3001):
    d = d0 + datetime.timedelta(days=random.randint(0, 60000))
    out.append("%010d%05d%02d%02d%-10s%-8s%-80s" % (k, d.year, d.month, d.day, "R%07d" % random.randint(0, 9999999),
               random.choice(["CD", "CR", "GJ", "SJ", "PJ"]), "SYNTHETIC ENTRY %d %s" % (k, random.choice(["ALPHA", "BETA", "GAMMA"]))))
open(sys.argv[1], "w").write("\n".join(out) + "\n")
PY
cp "$S/data/transactions-in.txt" "$G/data/"

fam=(); for f in $FAM; do fam+=("$O/$f.cbl"); done
"$CDIR/compile.sh" -free -std=2002 "$O/jerm.cbl" "${fam[@]}" "$M/cobol/clinkages.cbl" "$M/c/dateutil.c" -I "$M/h" -o "$S/jerm.s32x" >/dev/null || { echo "FAIL jerm: build"; exit 1; }
"$CDIR/compile.sh" -free "$M/cobol/crgltrans.cbl" -I "$M/copy" -o "$S/crgltrans.s32x" >/dev/null || { echo "FAIL crgltrans: build"; exit 1; }
"$CDIR/compile.sh" -free "$M/cobol/ldgltrans.cbl" "$M/cobol/clinkages.cbl" "$M/c/dateutil.c" -I "$M/copy" -I "$M/h" -o "$S/ldgltrans.s32x" >/dev/null || { echo "FAIL ldgltrans: build"; exit 1; }
"$CDIR/compile.sh" -free -std=2002 "$O/exgltrans.cbl" "$O/linear_to_fielded.cbl" "$O/fielded_to_linear.cbl" "$O/floor-div.cbl" "$O/floor-divmod.cbl" "$O/isleapyear.cbl" -I "$M/copy" -o "$S/exgltrans.s32x" >/dev/null || { echo "FAIL exgltrans: build"; exit 1; }

"$ENG" run --rm -v "$G:/w" -w /w gnucobol:4.0-builder sh -c "
    cobc -x -free -o jerm jerm.cbl $(for f in $FAM; do printf '%s.cbl ' $f; done) clinkages.cbl dateutil.c &&
    cobc -x -free -I copy -o crgltrans crgltrans.cbl &&
    cobc -x -free -I copy -o ldgltrans ldgltrans.cbl clinkages.cbl dateutil.c &&
    cobc -x -free -I copy -o exgltrans exgltrans.cbl linear_to_fielded.cbl fielded_to_linear.cbl floor-div.cbl floor-divmod.cbl isleapyear.cbl" >/dev/null 2>&1 \
    || { echo "FAIL oracle build"; exit 1; }

# the program's own output: the DBT's lines are not the program's
# jerm dates its lines around today: the oracle's container runs in UTC,
# so ours does too, or the two disagree on "today" for the hours when the
# local date and UTC's differ (cobol ISSUES-82)
ours() { (cd "$S" && TZ=UTC "$DBT" "$1.s32x" 2>/dev/null) | grep -v '^\[DBT\]\|^$' ; }
gnu()  { "$ENG" run --rm -v "$G:/w" -w /w gnucobol:4.0-runtime "./$1" 2>&1; }

ours jerm > "$S/jerm.out"; gnu jerm > "$G/jerm.out"
jl=$(wc -l < "$S/jerm.txt" | tr -d ' ')
if [ "$jl" -lt 300000 ]; then echo "FAIL jerm: $jl lines (the sweep is 1601 to today + 200,000 days: well over 300,000)"; fail=1;
elif cmp -s "$S/jerm.txt" "$G/jerm.txt" && cmp -s "$S/jerm.out" "$G/jerm.out"; then
    echo "PASS jerm: $jl lines, byte-identical"
else echo "FAIL jerm: output differs"; fail=1; fi
for p in crgltrans ldgltrans exgltrans; do ours $p > "$S/$p.out"; gnu $p > "$G/$p.out"; done
if cmp -s "$S/exgltrans.out" "$G/exgltrans.out" && cmp -s "$S/ldgltrans.out" "$G/ldgltrans.out"; then
    echo "PASS gltrans trio: $(grep -c SYNTHETIC "$S/exgltrans.out") records, byte-identical"
else echo "FAIL gltrans trio: output differs"; diff "$G/exgltrans.out" "$S/exgltrans.out" | head -4; fail=1; fi
exit $fail
