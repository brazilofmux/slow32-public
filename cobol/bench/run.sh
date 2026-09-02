#!/bin/sh
# Guest instruction counts for the COBOL statement-cost benches (GitHub #27).
#   ./run.sh      s32-cobol / slow32-fast off PATH, else $SLOW32 (~/slow-32)
# bgen writes the synthetic input; b9 is the startup floor the rest subtract.
# Nothing private is needed: bgen writes its own 4.27 MB / 56164-record input.
set -eu
HERE="$(cd "$(dirname "$0")" && pwd)"
SLOW32="${SLOW32:-$HOME/slow-32}"
CC="${S32_COMPILE:-$(command -v s32-cobol || echo "$SLOW32/cobol/compile.sh")}"
EMU="${S32_EMU:-$SLOW32/tools/emulator/slow32-fast}"
N=56164
cd "$HERE"
for b in bgen b9 b0 b1 b2 b3 b4; do "$CC" -free "$b.cbl" -o "$b.s32x" >/dev/null; done
"$EMU" bgen.s32x >/dev/null
count() { "$EMU" "$1.s32x" | awk '/Instructions executed/{print $3}'; }
b9=$(count b9); b0=$(count b0); b1=$(count b1); b2=$(count b2); b3=$(count b3); b4=$(count b4)
# awk, not bc: a bare Debian has no bc.
row() { awk -v l="$1" -v t="$2" -v a="$3" -v b="$4" -v n="$5" \
        'BEGIN{printf "%-28s %12d %10.0f\n", l, t, (a-b)/n}'; }
printf '%-28s %12s %10s\n' bench instructions per-unit
printf '%-28s %12s %10s\n' '--------------------------' ------------ ----------
printf '%-28s %12d %10s\n' 'b9  startup floor'       "$b9" -
row 'b0  loop iteration'       "$b0" "$b0" "$b9" "$N"
row 'b1  READ'                 "$b1" "$b1" "$b9" "$N"
row 'b2-b1  group MOVE+WRITE'  "$b2" "$b2" "$b1" "$N"
row 'b4-b0  numeric MOVE (x7)' "$b4" "$b4" "$b0" "$((N * 7))"
row 'b3-b4  PIC X MOVE (x5)'   "$b3" "$b3" "$b4" "$((N * 5))"
