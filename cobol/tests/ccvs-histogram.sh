#!/bin/bash
# Run the NIST CCVS-85 modules through s32-cobc and rank what stops them.
#
# Not a score: almost nothing here is expected to compile at first, and the
# useful output is a histogram -- which single missing thing blocks the most
# programs -- the same exercise cobc370/bin/cobc-ccvs does for the 74
# compiler.  A program counts as compiling when s32-cobc produces assembly;
# assembling, linking and running are later gates (docs/oracles.md).
#
#   tests/ccvs-histogram.sh [module ...]     default: the 1985 modules we claim
#
# Prerequisite: GnuCOBOL's harness has split newcob.val into per-module
# directories of .CBL files (in ~/gnucobol-svn/tests/cobol85: "make NC",
# or "make modules").  The X-cards are already substituted there.
set -u
HERE="$(cd "$(dirname "$0")" && pwd)"
CDIR="$(cd "$HERE/.." && pwd)"
CCVS=${CCVS85:-$HOME/gnucobol-svn/tests/cobol85}
COBC="$CDIR/out/s32-cobc"
[ -d "$CCVS" ] || { echo "no CCVS-85 tree at $CCVS (set CCVS85)" >&2; exit 1; }
[ -x "$COBC" ] || "$CDIR/build.sh" >/dev/null || exit 1

# NC nucleus, SQ sequential, RL relative, IX indexed, ST sort-merge, SM source
# text manipulation (COPY/REPLACE), IC inter-program, RW report writer, IF the
# 1989 intrinsic functions; SG segmentation, OB obsolete, DB debug and CM
# communication are deliberately out (dialect.md)
MODULES=${*:-NC SQ RL IX ST SM IC RW IF}
W="$(mktemp -d "$CDIR/out/ccvs.XXXXXX")"
trap 'rm -rf "$W"' EXIT
total=0; built=0
for m in $MODULES; do
    d="$CCVS/$m"
    if [ ! -d "$d" ]; then printf '  %-3s not extracted (run "make %s" in %s)\n' "$m" "$m" "$CCVS"; continue; fi
    ok=0; n=0
    for f in "$d"/*.CBL; do
        [ -e "$f" ] || continue
        n=$((n+1))
        if "$COBC" -fixed -I "$CCVS/copy" -o "$W/out.s" "$f" >/dev/null 2>"$W/err"; then
            ok=$((ok+1)); echo "$m $(basename "$f" .CBL)" >> "$W/compiled"
        else
            # the first diagnostic, with the position and the names it quotes
            # taken out, so identical causes fall into one bin
            r="$(grep -m1 "error" "$W/err" | sed "s/^[^:]*:[0-9]*: *error: *//; s/'[^']*'/'…'/g")"
            [ -n "$r" ] || r="(no diagnostic: $(tail -1 "$W/err" | cut -c1-60))"
            printf '%s\t%s\t%s\n' "$m" "$(basename "$f" .CBL)" "$r" >> "$W/reasons"
        fi
    done
    printf '  %-3s %3d of %3d compile\n' "$m" "$ok" "$n"
    total=$((total+n)); built=$((built+ok))
done
echo
echo "$built of $total compile"
echo
echo "what stops the rest, most common first:"
[ -f "$W/reasons" ] && cut -f3 "$W/reasons" | sort | uniq -c | sort -rn | head -${TOP:-25} | sed 's/^/  /'
if [ -n "${CCVS_DETAIL:-}" ]; then
    echo; echo "per program:"; sort "$W/reasons" | sed 's/^/  /'
fi
