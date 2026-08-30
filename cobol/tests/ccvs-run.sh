#!/bin/bash
# Run the NIST CCVS-85 programs that compile, and score them the way the
# suite scores itself: each program prints its own report, ending in
#   nnn OF nnn  TESTS WERE EXECUTED SUCCESSFULLY / nnn TEST(S) FAILED /
#   ... DELETED / ... REQUIRE INSPECTION
# (the same lines GnuCOBOL's report.pl reads).  Output: one line per
# program -- pass/total, failed, deleted, and how that compares with
# GnuCOBOL's own tally in <module>.txt -- then totals per module.
#
#   tests/ccvs-run.sh [module ...]        default: NC SQ RL IX ST SM IC RW IF
#   CCVS_KEEP=1                           keep the work directory
#   CCVS_ONLY=NC101A                      one program, with its report shown
set -u
HERE="$(cd "$(dirname "$0")" && pwd)"
CDIR="$(cd "$HERE/.." && pwd)"
ROOT="$(cd "$CDIR/.." && pwd)"
CCVS=${CCVS85:-$HOME/gnucobol-svn/tests/cobol85}
EMU=${EMU:-$ROOT/tools/dbt/slow32-dbt}
[ -x "$EMU" ] || EMU=$ROOT/tools/emulator/slow32-fast
[ -d "$CCVS" ] || { echo "no CCVS-85 tree at $CCVS (set CCVS85)" >&2; exit 1; }
[ -x "$CDIR/out/s32-cobc" ] || "$CDIR/build.sh" >/dev/null || exit 1
MODULES=${*:-NC SQ RL IX ST SM IC RW IF}
W="$(mktemp -d "$CDIR/out/ccvsrun.XXXXXX")"
[ -n "${CCVS_KEEP:-}" ] || trap 'rm -rf "$W"' EXIT
gt_all=0; gt_pass=0; gt_fail=0; gt_nocomp=0; gt_crash=0; gt_prog=0; gt_progok=0
for m in $MODULES; do
    d="$CCVS/$m"
    [ -d "$d" ] || { printf '  %-3s not extracted\n' "$m"; continue; }
    t_all=0; t_pass=0; t_fail=0; t_nocomp=0; t_crash=0; t_prog=0; t_progok=0
    for f in "$d"/*.CBL; do
        [ -e "$f" ] || continue
        name="$(basename "$f" .CBL)"
        [ -z "${CCVS_ONLY:-}" ] || [ "$name" = "$CCVS_ONLY" ] || continue
        lc="$(echo "$name" | tr 'A-Z' 'a-z')"
        t_prog=$((t_prog+1))
        run="$W/$m/$name"; mkdir -p "$run"
        cp "$f" "$run/$lc.cbl"
        # GnuCOBOL's tally for this program: total pass fail deleted
        exp="$(grep "^$name.CBL" "$CCVS/$m.txt" 2>/dev/null | head -1 | awk '{print $2, $3, $4, $5}')"
        if ! "$CDIR/compile.sh" -fixed -I "$CCVS/copy" "$run/$lc.cbl" -o "$run/$lc.s32x" >"$run/compile.log" 2>&1; then
            t_nocomp=$((t_nocomp+1))
            printf '  %-7s %-24s (%s)\n' "$name" "does not compile" "$(grep -m1 -i error "$run/compile.log" | sed 's/^[^:]*:[0-9]*: *error: *//' | cut -c1-60)"
            continue
        fi
        # the program's own data, if the suite ships it, on standard input
        inp=/dev/null; [ -f "$d/$name.DAT" ] && inp="$d/$name.DAT"
        # the same X-card files the suite's harness makes available
        for x in "$d"/*.DAT; do [ -e "$x" ] && cp "$x" "$run/" 2>/dev/null; done
        ( cd "$run" && timeout 120 "$EMU" "$lc.s32x" < "$inp" > stdout.txt 2> stderr.txt ); rc=$?
        rep="$run/REPORT"
        if [ ! -f "$rep" ] && [ -f "$run/stdout.txt" ]; then rep="$run/stdout.txt"; fi
        pass=0; total=0; fail=0; del=0
        if [ -f "$rep" ]; then
            eval "$(awk '
                /^ *[0-9]+ *OF *[0-9]+ *TESTS WERE/ { pass += $1; total += $3 }
                /^ *[0-9NO]+ *TEST\(S\) FAILED/ { if ($1 != "NO") fail += $1 }
                /^ *[0-9NO]+ *TEST\(S\) DELETED/ { if ($1 != "NO") del += $1 }
                END { printf "pass=%d total=%d fail=%d del=%d\n", pass, total, fail, del }' "$rep")"
        fi
        status=""
        if [ "$rc" != 0 ] || grep -q "fatal\|HALT at\|Memory fault\|file error" "$run/stderr.txt" "$run/stdout.txt" 2>/dev/null && [ "$total" = 0 ]; then
            status="CRASH rc=$rc: $(grep -m1 -h "fatal\|error\|fault" "$run/stderr.txt" "$run/stdout.txt" 2>/dev/null | cut -c1-50)"
            t_crash=$((t_crash+1))
        elif [ "$total" = 0 ]; then
            status="no summary (rc=$rc)"; t_crash=$((t_crash+1))
        else
            set -- $exp; e_total=${1:-?}; e_pass=${2:-?}; e_fail=${3:-?}; e_del=${4:-?}
            if [ "$pass" = "$e_pass" ] && [ "$fail" = "$e_fail" ]; then status="= GnuCOBOL"; t_progok=$((t_progok+1))
            else status="GnuCOBOL $e_pass/$e_total fail $e_fail del $e_del"; fi
        fi
        printf '  %-7s %3d/%3d  fail %2d  del %2d   %s\n' "$name" "$pass" "$total" "$fail" "$del" "$status"
        [ -n "${CCVS_ONLY:-}" ] && grep -n "FAIL\*" "$rep" | head -20 | cut -c1-110
        t_all=$((t_all+total)); t_pass=$((t_pass+pass)); t_fail=$((t_fail+fail))
    done
    printf '%s: %d programs, %d compile, %d run to a summary; tests %d of %d pass, %d fail; %d programs match GnuCOBOL exactly\n' \
        "$m" "$t_prog" "$((t_prog-t_nocomp))" "$((t_prog-t_nocomp-t_crash))" "$t_pass" "$t_all" "$t_fail" "$t_progok"
    gt_all=$((gt_all+t_all)); gt_pass=$((gt_pass+t_pass)); gt_fail=$((gt_fail+t_fail)); gt_nocomp=$((gt_nocomp+t_nocomp)); gt_crash=$((gt_crash+t_crash)); gt_prog=$((gt_prog+t_prog)); gt_progok=$((gt_progok+t_progok))
done
echo
printf 'CCVS-85: %d programs, %d compile, %d run to a summary; tests %d of %d pass (%d fail); %d programs match GnuCOBOL exactly\n' \
    "$gt_prog" "$((gt_prog-gt_nocomp))" "$((gt_prog-gt_nocomp-gt_crash))" "$gt_pass" "$gt_all" "$gt_fail" "$gt_progok"
