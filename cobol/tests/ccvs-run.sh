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
    # one working directory per module, in the suite's order: the *M programs
    # read files the programs before them wrote (the X-card files)
    run="$W/$m"; mkdir -p "$run"
    for x in "$d"/*.DAT; do [ -e "$x" ] && cp "$x" "$run/" 2>/dev/null; done
    # the module's library programs (IC: the CALLed subprograms), compiled
    # once as modules and linked into every main, as report.pl's compile_lib
    libobjs=()
    for x in "$d"/lib/*.CBL; do
        [ -e "$x" ] || continue
        ln="$(basename "$x" .CBL | tr 'A-Z' 'a-z')"
        if "$CDIR/out/s32-cobc" -fixed -I "$CCVS/copy" -m -o "$run/lib-$ln.s" "$x" >"$run/lib-$ln.compile.log" 2>&1 &&
           "$ROOT/tools/assembler/slow32asm" "$run/lib-$ln.s" "$run/lib-$ln.s32o" >/dev/null 2>&1; then
            libobjs+=("$run/lib-$ln.s32o")
        else
            printf '  lib/%-7s does not compile   (%s)\n' "$(basename "$x" .CBL)" "$(grep -m1 -i error "$run/lib-$ln.compile.log" | sed 's/^[^:]*:[0-9]*: *error: *//' | cut -c1-60)"
        fi
    done
    for f in "$d"/*.CBL; do
        [ -e "$f" ] || continue
        name="$(basename "$f" .CBL)"
        [ -z "${CCVS_ONLY:-}" ] || [ "$name" = "$CCVS_ONLY" ] || continue
        lc="$(echo "$name" | tr 'A-Z' 'a-z')"
        t_prog=$((t_prog+1))
        cp "$f" "$run/$lc.cbl"
        # GnuCOBOL's tally for this program: total pass fail deleted
        exp="$(grep "^$name.CBL" "$CCVS/$m.txt" 2>/dev/null | head -1 | awk '{print $2, $3, $4, $5}')"
        if ! "$CDIR/compile.sh" -fixed -I "$CCVS/copy" "$run/$lc.cbl" "${libobjs[@]+"${libobjs[@]}"}" -o "$run/$lc.s32x" >"$run/$name.compile.log" 2>&1; then
            t_nocomp=$((t_nocomp+1))
            printf '  %-7s %-24s (%s)\n' "$name" "does not compile" "$(grep -m1 -i error "$run/$name.compile.log" | sed 's/^[^:]*:[0-9]*: *error: *//' | cut -c1-60)"
            continue
        fi
        # compile-only programs (report.pl's comp_only): a compile is the pass
        case "$name" in NC401M|RL301M|RL401M|IC401M|IX301M|IX401M|SQ401M|ST301M|SM401M|OB401M|DB301M|DB302M|DB303M|DB304M|DB305M|SG301M|CM301M|CM401M)
            printf '  %-7s %3d/%3d  fail %2d  del %2d   %s\n' "$name" 1 1 0 0 "= GnuCOBOL (compile only)"
            t_all=$((t_all+1)); t_pass=$((t_pass+1)); t_progok=$((t_progok+1)); continue ;;
        esac
        # the program's own data, if the suite ships it, on standard input
        inp=/dev/null; [ -f "$d/$name.DAT" ] && inp="$d/$name.DAT"
        # the X-card files start afresh for every program, as report.pl does
        rm -f "$run"/XXXXX* "$run/REPORT"
        ( cd "$run" && timeout 120 "$EMU" "$lc.s32x" < "$inp" > "$name.out" 2> "$name.err" ); rc=$?
        rep="$run/REPORT"
        # a few programs report on the console instead (report.pl reads NC121M's
        # and NC220M's .out); two write nothing and count as one pass when they exit 0

        pass=0; total=0; fail=0; del=0; summary=0
        if [ -f "$rep" ]; then
            eval "$(awk '
                /^ *[0-9]+ *OF *[0-9]+ *TESTS WERE/ { pass += $1; total += $3; summary = 1 }
                /^ *[0-9NO]+ *TEST\(S\) FAILED/ { if ($1 != "NO") fail += $1 }
                /^ *[0-9NO]+ *TEST\(S\) DELETED/ { if ($1 != "NO") del += $1 }
                END { printf "pass=%d total=%d fail=%d del=%d summary=%d\n", pass, total, fail, del, summary }' "$rep")"
        fi
        case "$name" in NC110M|NC214M) [ "$rc" = 0 ] && { pass=1; total=1; summary=1; } ;; esac
        [ "$summary" = 1 ] && [ "$total" = 0 ] && { pass=1; total=1; }     # "000 OF 000 TESTS": the run itself is the test
        status=""
        # a program with no summary that GnuCOBOL's tally also scores 0 of 0 is fine when it exits 0
        if [ "$summary" = 0 ] && [ "$rc" = 0 ] && [ "${exp% *}" = "0 0 0" ]; then summary=1; fi
        if [ "$summary" = 0 ]; then
            if [ "$rc" != 0 ] || grep -q "fatal\|HALT at\|Memory fault\|file error" "$run/$name.err" "$run/$name.out" 2>/dev/null; then
                status="CRASH rc=$rc: $(grep -m1 -h "fatal\|error\|fault" "$run/$name.err" "$run/$name.out" 2>/dev/null | cut -c1-50)"
            else status="no summary (rc=$rc)"; fi
            t_crash=$((t_crash+1))
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
