#!/bin/bash
# run-memcheck.sh -- the reference interpreter's memcheck mode (slow32 -m;
# tools/emulator/memcheck.h says what it sees), as a gate.
#
# The gates beside this one compare answers.  A read of a dead frame, of a
# freed block or through a null pointer changes an answer only when the
# bytes it finds happen to be wrong, and on SLOW-32 they seldom are: the
# stack still holds what the last call left, and address 0 is readable.
# Its first runs (2026-11-11), with every other gate green, found a read
# through a null pointer in f77's code for an absent OPTIONAL argument and
# another in libcob's UNSTRING, printf("%.*s") reading past its precision,
# libcob's report writer branching on a buffer it had not filled, and a
# bug fixed in stage07 a month before (selfhost ISSUES-76: a tail call
# that hands its callee a pointer into the frame it has popped) alive in
# the compiler one stage down, in the assembler stage07 is built with.
#
# Two parts:
#
#   1. The instrument.  regression/memcheck/seeded.c has one error for each
#      argument and none for 0.  Built by clang against runtime/ and, when
#      there is a kit, by stage08 cc against its libc, each case must be
#      reported for what it is, case 0 must print nothing, and the
#      program's own output must be the same bytes with -m and without.
#
#   2. The sweeps.  Whole suites run under S32_MEMCHECK=1, which changes
#      nothing they compare: each must pass as it does without, and no
#      program may report unless regression/memcheck/known.txt lists it.
#
#   run-memcheck.sh              the instrument, and the quick sweeps (~8 min):
#                                regression, forth, fortran, fcvs, kit
#   run-memcheck.sh --all        and the long ones (~40 min): cobol, ccvs,
#                                corpus, xbase, xbc
#   run-memcheck.sh NAME ...     the instrument and the sweeps named
#   run-memcheck.sh --none       the instrument alone (seconds)
#
# islisp's suite is not among them: it cannot pass on the interpreter, with
# the mode or without -- its stage08 build's soft libm differs from the
# clang build's in last digits (runtime ISSUES-29), which the DBT's host
# libm hides.  Its collector's reports are in known.txt for a run by hand.
#
# After a change to the interpreter or memcheck.h: all of it.  After a
# change to a compiler or a run-time library: the sweep of its suite.
# Exit 0 clean, 1 a failure, 2 something it needs is missing.
# KIT (default ~/s32x) is the kit; without one the kit parts are skipped.
set -u
HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"; ROOT="$(dirname "$HERE")"
EMU="$ROOT/tools/emulator/slow32"
KIT="${KIT:-$HOME/s32x}"
CLANG="${CLANG:-$HOME/llvm-project/build/bin/clang}"
KNOWN="$HERE/memcheck/known.txt"
QUICK="regression forth fortran fcvs kit"
LONG="cobol ccvs corpus xbase xbc"
[ -x "$EMU" ] || { echo "run-memcheck: no $EMU" >&2; exit 2; }

case "${1:-}" in
    "")      SWEEPS="$QUICK" ;;
    --all)   SWEEPS="$QUICK $LONG" ;;
    --none)  SWEEPS="" ;;
    *)       SWEEPS="$*" ;;
esac

W="$(mktemp -d "${TMPDIR:-/tmp}/memcheck.XXXXXX")" || exit 2
fail=0
say() { printf '  %-28s %s\n' "$1" "$2"; }

# ---------------------------------------------------------------- 1. the instrument
# case | what its report must say (each ;-separated piece, a grep -E pattern)
CASES='1|conditional branch on an uninitialised value;from the stack frame made at
2|store of 1 byte at 0x[0-9A-F]+, which is not addressable;is 0 bytes after a block of 10 bytes
3|load of 1 byte at 0x[0-9A-F]+, which is not addressable;is 1 bytes inside a block of 10 bytes;and freed at
4|free of 0x[0-9A-F]+, which is not a live block;and freed at
5|a null pointer
6|3 uninitialised bytes written to file descriptor 1 \(first at byte 1 of 5\);from a block of 10 bytes
7|uninitialised;from a block of 40 bytes
8|uninitialised;from a block of 100 bytes
9|which is not addressable;bytes below the stack pointer
10|free of 0x[0-9A-F]+, which is not a live block;is on the stack
11|load of 1 byte at 0x[0-9A-F]+, which is not addressable;is 4 bytes before a block of 10 bytes'

instrument() {      # name program
    local name="$1" x="$2" bad="" n pats pat
    # nothing wrong: nothing said, and the same bytes out
    "$EMU" -q "$x" 0 > "$W/plain.out" 2> "$W/plain.err"; local prc=$?
    "$EMU" -q -m "$x" 0 > "$W/mc.out" 2> "$W/mc.err"; local mrc=$?
    [ -s "$W/mc.err" ] && bad="$bad 0:reports"
    { cmp -s "$W/plain.out" "$W/mc.out" && [ "$prc" = "$mrc" ] && grep -q '^clean ok' "$W/mc.out"; } || bad="$bad 0:output"
    while IFS='|' read -r n pats; do
        "$EMU" -q "$x" "$n" > "$W/plain.out" 2>/dev/null; prc=$?
        "$EMU" -q -m "$x" "$n" > "$W/mc.out" 2> "$W/mc.err"; mrc=$?
        { cmp -s "$W/plain.out" "$W/mc.out" && [ "$prc" = "$mrc" ]; } || bad="$bad $n:output"
        while [ -n "$pats" ]; do
            pat="${pats%%;*}"
            grep -aEq -- "$pat" "$W/mc.err" || { bad="$bad $n"; break; }
            [ "$pat" = "$pats" ] && break
            pats="${pats#*;}"
        done
    done <<< "$CASES"
    # the log, and the status asked for
    rm -f "$W/log"
    S32_MEMCHECK=1 S32_MEMCHECK_LOG="$W/log" S32_MEMCHECK_EXIT=99 "$EMU" -q "$x" 2 > /dev/null 2> "$W/mc.err"
    { [ $? = 99 ] && [ ! -s "$W/mc.err" ] && grep -q "^memcheck: 1 error at 1 place in .*$(basename "$x")" "$W/log"; } || bad="$bad log"
    if [ -z "$bad" ]; then say "instrument/$name:" "PASS (11 seeded errors told apart, a clean run silent)"
    else say "instrument/$name:" "FAIL (cases:$bad)"; fail=1; fi
}

echo "memcheck: the instrument"
if [ -x "$CLANG" ]; then
    if SLOW32_HOME="$ROOT" "$CLANG" --target=slow32-unknown-none -O0 "$HERE/memcheck/seeded.c" -o "$W/seeded.s32x" > "$W/build.log" 2>&1
    then instrument clang "$W/seeded.s32x"
    else say "instrument/clang:" "FAIL (does not build: $W/build.log)"; fail=1; fi
else
    say "instrument/clang:" "SKIP (no $CLANG)"
fi
if [ -f "$KIT/cc.s32x" ] && [ -f "$KIT/selfhost/libc.s32a" ]; then
    F="$ROOT/tools/emulator/slow32-fast"
    if "$F" "$KIT/cc.s32x" -I"$KIT/selfhost/include" "$HERE/memcheck/seeded.c" "$W/k.s" > "$W/build.log" 2>&1 &&
       "$F" "$KIT/s32-as.s32x" "$W/k.s" "$W/k.s32o" >> "$W/build.log" 2>&1 &&
       "$F" "$KIT/s32-ld.s32x" --mmio 64K -o "$W/seeded-kit.s32x" "$KIT/selfhost/crt0.s32o" "$W/k.s32o" "$KIT/selfhost/libc.s32a" >> "$W/build.log" 2>&1
    then instrument kit "$W/seeded-kit.s32x"
    else say "instrument/kit:" "FAIL (does not build: $W/build.log)"; fail=1; fi
else
    say "instrument/kit:" "SKIP (no kit at $KIT)"
fi

# ---------------------------------------------------------------- 2. the sweeps
# An engine for harnesses that take the interpreter's banner for output
printf '#!/bin/sh\nexec "%s" -q "$@"\n' "$EMU" > "$W/slow32-q"; chmod +x "$W/slow32-q"

sweep_cmd() {
    case "$1" in
        regression) echo "cd '$ROOT/regression' && TIMEOUT=30 ./run-tests.sh" ;;
        forth)      echo "cd '$ROOT/forth' && EMU='$EMU' ./tests/run-tests.sh" ;;
        fortran)    echo "EMU='$EMU' '$ROOT/fortran/tests/run-tests.sh'" ;;
        fcvs)       echo "EMU='$W/slow32-q' TIMEOUT=600 '$ROOT/fortran/tests/run-fcvs.sh'" ;;
        kit)        echo "cd '$ROOT/regression' && KIT='$KIT' TIMEOUT=60 ./run-kit-differential.sh" ;;
        # CCVS apart: the suite's both-paths check compares console output byte
        # for byte, and the interpreter's own closing lines carry the wall time
        cobol)      echo "CCVS=0 EMU='$EMU' '$ROOT/cobol/tests/run-tests.sh'" ;;
        ccvs)       echo "EMU='$W/slow32-q' '$ROOT/cobol/tests/ccvs-run.sh' | tail -1 | diff - '$ROOT/cobol/tests/ccvs-baseline.txt'" ;;
        corpus)     echo "S32_ENGINE='$W/slow32-q' '$ROOT/regression/run-f77-corpus.sh'" ;;
        xbase)      echo "EMU='$EMU' bash '$ROOT/xbase/tests/run-tests.sh'" ;;
        xbc)        echo "EMU='$EMU' bash '$ROOT/xbc/tests/run-tests.sh'" ;;
        *)          return 1 ;;
    esac
}

known() {       # is this program, in this sweep, on the list?
    local sweep="$1" path="$2" sw pat why
    while IFS=$'\t' read -r sw pat why; do
        case "$sw" in ""|\#*) continue ;; esac
        [ "$sw" = "$sweep" ] || continue
        # shellcheck disable=SC2254
        case "$path" in $pat) return 0 ;; esac
    done < "$KNOWN"
    return 1
}

[ -n "$SWEEPS" ] && echo "memcheck: the sweeps"
for s in $SWEEPS; do
    cmd="$(sweep_cmd "$s")" || { say "sweep/$s:" "FAIL (no such sweep; they are: $QUICK $LONG)"; fail=1; continue; }
    if [ "$s" = kit ] && [ ! -f "$KIT/cc.s32x" ]; then say "sweep/$s:" "SKIP (no kit at $KIT)"; continue; fi
    log="$W/$s.log"; : > "$log"
    S32_MEMCHECK=1 S32_MEMCHECK_LOG="$log" bash -c "$cmd" > "$W/$s.out" 2>&1; rc=$?
    # the programs that reported, each once
    grep -a '^memcheck: [0-9]* errors\{0,1\} at [0-9]* places\{0,1\} in ' "$log" | sed 's/^memcheck: .* in //' | sort -u > "$W/$s.progs"
    new=0; seen_known=0
    while IFS= read -r p; do
        [ -n "$p" ] || continue
        if known "$s" "$p"; then seen_known=$((seen_known + 1)); else new=$((new + 1)); echo "    reports: $p"; fi
    done < "$W/$s.progs"
    if [ "$rc" -ne 0 ]; then say "sweep/$s:" "FAIL (the suite itself: exit $rc, $W/$s.out)"; fail=1
    elif [ "$new" -ne 0 ]; then say "sweep/$s:" "FAIL ($new programs report: $log)"; fail=1
    elif [ "$seen_known" -ne 0 ]; then say "sweep/$s:" "PASS (the suite passes; $seen_known programs report, as memcheck/known.txt says they do)"
    else say "sweep/$s:" "PASS (the suite passes, no reports)"; fi
done

# a listed program that no longer reports comes off the list (when its sweep ran)
while IFS=$'\t' read -r sw pat why; do
    case "$sw" in ""|\#*) continue ;; esac
    [ -f "$W/$sw.progs" ] || continue
    hit=0
    # shellcheck disable=SC2254
    while IFS= read -r p; do case "$p" in $pat) hit=1 ;; esac; done < "$W/$sw.progs"
    [ "$hit" = 1 ] || { say "known:" "FAIL ($pat no longer reports: take it off memcheck/known.txt)"; fail=1; }
done < "$KNOWN"

if [ "$fail" = 0 ]; then echo "memcheck: clean"; rm -rf "$W"; exit 0; fi
echo "memcheck: FAILED (work kept in $W)"
exit 1
