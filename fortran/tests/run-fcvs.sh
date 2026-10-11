#!/bin/bash
# run-fcvs.sh -- NIST's Fortran 77 Compiler Validation System (FCVS 2.1)
# against f77 + libf77 on SLOW-32.
#
# The suite is 192 self-checking programs FM001..FM923 (public domain,
# ~1.4 MB as fcvs21.tar.Z).  It is NOT in this tree: fetch it to
# ~/refs/fcvs/fcvs21 (or point FCVS at it).  Each program prints a report
# and a run summary ("N ERRORS ENCOUNTERED / N TESTS PASSED / N TESTS
# DELETED"); this script compiles each one with compile.sh's pipeline,
# runs it on an emulator, and reads that summary.  Unit 5 is the card
# reader: a sibling FMxxx.DAT is the program's stdin.
#
# Only 53 programs carry that summary.  The other 139 (FM005, FM109, and
# all of the full-language level) print values for inspection, so for
# those the harness runs gfortran through tests/oracle.sh (the
# slow32:fortran-oracle container) and diffs the two outputs with
# trailing blanks removed.  Oracle outputs are cached in $OUT/oracle;
# ORACLE=0 skips that leg (inspection programs then report INSPECT).
#
#   tests/run-fcvs.sh [-v] [--limit N] [FM001 FM110 ...]
#
# Outcomes:
#   PASS           summary present, 0 errors (FM001 is special: it must
#                  report exactly 1 error, 1 pass, 1 deleted)
#   FAIL(n)        summary present, n tests failed; the note lists them
#   DIFF(n)        inspection program: n lines differ from gfortran's output
#   INSPECT        inspection program ran, oracle leg off or unavailable
#   COMPILE-FAIL / ASM-FAIL / LINK-FAIL
#   RUN-FAULT      emulator fault (memory/alignment/illegal)
#   RUN-FAIL(rc)   exited without a summary; note has the first stderr line
#   TIMEOUT        exceeded $TIMEOUT seconds (default 60)
#
# Every program's full output is kept in $OUT (default /tmp/fcvs-slow32)
# as FMxxx.out, with results.tsv beside them.
set -u
HERE="$(cd "$(dirname "$0")" && pwd)"
FDIR="$(cd "$HERE/.." && pwd)"
ROOT="$(cd "$FDIR/.." && pwd)"
FCVS="${FCVS:-$HOME/refs/fcvs/fcvs21}"
OUT="${OUT:-/tmp/fcvs-slow32}"
F77="${S32_F77:-$FDIR/out/f77}"
LIBF77="${S32_LIBF77:-$FDIR/runtime/libf77.s32o}"
AS="${S32_AS:-$ROOT/tools/assembler/slow32asm}"
LD="${S32_LD:-$ROOT/tools/linker/s32-ld}"
RT="${S32_RT:-$ROOT/runtime}"
# The DBT prints nothing of its own, which the output diff relies on; the
# C++ emulators' banner/statistics lines are filtered just in case.
EMU="${EMU:-$ROOT/tools/dbt/slow32-dbt}"
[ -x "$EMU" ] || EMU="$ROOT/tools/emulator/slow32-fast"
TIMEOUT="${TIMEOUT:-60}"
ORACLE="${ORACLE:-1}"
BANNER='^Starting execution|^HALT at|^Program halted|^Instructions executed|^Simulated cycles|^Cycles|^Wall time|^Performance|^MMIO enabled|^Exit code|instructions/second'

VERBOSE=0; LIMIT=0; NAMES=()
while [ $# -gt 0 ]; do
    case "$1" in
        -v) VERBOSE=1 ;;
        --limit) LIMIT="$2"; shift ;;
        -h|--help) sed -n '2,30p' "$0"; exit 0 ;;
        *) NAMES+=("${1%.FOR}") ;;
    esac
    shift
done
[ -d "$FCVS" ] || { echo "run-fcvs.sh: no suite at $FCVS (fetch fcvs21.tar.Z there)" >&2; exit 2; }
[ -x "$F77" ] && [ -f "$LIBF77" ] || "$FDIR/build.sh" >/dev/null
if command -v timeout >/dev/null; then TO=timeout; elif command -v gtimeout >/dev/null; then TO=gtimeout; else TO=""; fi

mkdir -p "$OUT/oracle"
W="$(mktemp -d "${TMPDIR:-/tmp}/fcvs.XXXXXX")"
# The oracle container mounts the source's directory, which must be under
# $HOME on macOS (podman's VM does not share /tmp).
OW="$(mktemp -d "$HOME/.fcvs-oracle.XXXXXX")"
trap 'rm -rf "$W" "$OW"' EXIT
: > "$OUT/results.tsv"

if [ ${#NAMES[@]} -eq 0 ]; then
    for f in "$FCVS"/FM*.FOR; do NAMES+=("$(basename "$f" .FOR)"); done
fi

N_PASS=0; N_FAIL=0; N_DIFF=0; N_INSPECT=0; N_COMPILE_FAIL=0; N_ASM_FAIL=0; N_LINK_FAIL=0; N_RUN_FAULT=0; N_RUN_FAIL=0; N_TIMEOUT=0
TOTAL=0
for name in "${NAMES[@]}"; do
    [ "$LIMIT" -gt 0 ] && [ "$TOTAL" -ge "$LIMIT" ] && break
    TOTAL=$((TOTAL+1))
    src="$FCVS/$name.FOR"
    [ -f "$src" ] || { echo "$name: no such program" >&2; continue; }
    d="$W/$name"; mkdir -p "$d"
    # 139 of the files carry a PROGRAM card prepended to a program that
    # already has one; gfortran rejects the pair, so the first is
    # dropped here (f77 reads PROGRAM as documentation either way).
    if [ "$(grep -c '^      PROGRAM ' "$src")" -gt 1 ]; then
        awk 'BEGIN { seen = 0 } /^      PROGRAM / && !seen { seen = 1; next } { print }' "$src" > "$d/$name.f"
    else
        cp "$src" "$d/$name.f"
    fi
    src="$d/$name.f"
    log="$OUT/$name.out"; : > "$log"
    outcome=""; note=""
    if ! "$F77" "$d/$name.f" "$d/$name.s" >"$d/cc.err" 2>&1; then
        outcome="COMPILE-FAIL"; note="$(head -1 "$d/cc.err" | cut -c1-100)"
        cat "$d/cc.err" >> "$log"
    elif ! "$AS" "$d/$name.s" "$d/$name.s32o" >"$d/as.err" 2>&1; then
        outcome="ASM-FAIL"; note="$(grep -m1 -i 'error' "$d/as.err" | cut -c1-100)"
        cat "$d/as.err" >> "$log"
    elif ! "$LD" -o "$d/$name.s32x" --mmio 64K "$RT/crt0.s32o" "$d/$name.s32o" "$LIBF77" \
            "$RT/libc_mmio.s32a" "$RT/libs32.s32a" >"$d/ld.err" 2>&1; then
        outcome="LINK-FAIL"; note="$(grep -m1 -i 'undefined\|error' "$d/ld.err" | cut -c1-100)"
        cat "$d/ld.err" >> "$log"
    else
        dat=/dev/null; [ -f "$FCVS/$name.DAT" ] && dat="$FCVS/$name.DAT"
        # gfortran's PAUSE reads stdin and ends the job at EOF; feed it
        # "go" so the oracle runs on past each PAUSE as f77 does.
        if [ "$dat" = /dev/null ] && grep -q '^ .....PAUSE' "$src"; then
            yes go | head -50 > "$d/pause.in"; dat="$d/pause.in"
        fi
        # Programs OPEN and REWIND scratch files: run inside their own dir.
        (cd "$d" && $TO ${TO:+$TIMEOUT} "$EMU" "$d/$name.s32x" < "$dat" > "$d/stdout" 2> "$d/stderr")
        rc=$?
        cat "$d/stdout" >> "$log"; [ -s "$d/stderr" ] && { echo "--- stderr:"; cat "$d/stderr"; } >> "$log"
        errs="$(grep -m1 'ERRORS ENCOUNTERED' "$d/stdout" | sed 's/ *ERRORS.*//; s/ //g')"
        pass="$(grep -m1 'TESTS PASSED' "$d/stdout" | sed 's/ *TESTS.*//; s/ //g')"
        dele="$(grep -m1 'TESTS DELETED' "$d/stdout" | sed 's/ *TESTS.*//; s/ //g')"
        if [ "$rc" = 124 ]; then
            outcome="TIMEOUT"
        elif grep -qi 'fault\|exception\|illegal\|misaligned' "$d/stderr"; then
            outcome="RUN-FAULT"; note="$(grep -m1 -i 'fault\|exception\|illegal\|misaligned' "$d/stderr" | cut -c1-100)"
        elif ! grep -q 'ERRORS ENCOUNTERED' "$src"; then
            # Inspection program: no summary by design; compare with gfortran.
            if [ "$rc" != 0 ] && [ "$rc" != 1 ]; then
                outcome="RUN-FAIL($rc)"; note="$(head -1 "$d/stderr" | cut -c1-100)"
            elif [ "$ORACLE" != 1 ]; then
                outcome="INSPECT"
            else
                want="$OUT/oracle/$name.want"
                if [ ! -s "$want" ]; then
                    mkdir -p "$OW/$name"; cp "$src" "$OW/$name/$name.f"
                    # gfortran's warnings come back on the same stream as
                    # the program's output; drop each 5-line block.
                    (cd "$OW/$name" && "$HERE/oracle.sh" "$OW/$name/$name.f" < "$dat" 2>/dev/null) \
                        | awk '/^\/work\/[^ ]*\.f:[0-9]+:/ { skip = 1; next }
                               skip { if ($0 ~ /^(Warning|Error|note):/) skip = 0; next }
                               { print }' \
                        | sed 's/[[:space:]]*$//' > "$want.tmp"
                    if [ -s "$want.tmp" ]; then mv "$want.tmp" "$want"; else rm -f "$want.tmp"; fi
                fi
                if [ ! -s "$want" ]; then
                    outcome="INSPECT"; note="oracle produced nothing"
                else
                    grep -vE "$BANNER" "$d/stdout" | sed 's/[[:space:]]*$//' > "$d/got"
                    ndiff="$(diff "$want" "$d/got" | grep -c '^[<>]')"
                    if [ "$ndiff" = 0 ]; then
                        outcome="PASS"; note="matches gfortran, $(wc -l < "$d/got" | tr -d ' ') lines"
                    else
                        outcome="DIFF($ndiff)"
                        note="first: $(diff "$want" "$d/got" | grep -m1 '^[<>]' | cut -c1-70)"
                        diff "$want" "$d/got" > "$OUT/$name.diff"
                    fi
                fi
            fi
        elif [ -z "$errs" ]; then
            outcome="RUN-FAIL($rc)"; note="$(head -1 "$d/stderr" | cut -c1-100)"
        elif [ "$name" = FM001 ]; then
            if [ "$errs" = 1 ] && [ "$pass" = 1 ] && [ "$dele" = 1 ]; then outcome="PASS"; else outcome="FAIL($errs)"; fi
            note="self-test: $errs/$pass/$dele (want 1/1/1)"
        elif [ "$errs" = 0 ]; then
            outcome="PASS"; note="$pass passed, $dele deleted"
        else
            outcome="FAIL($errs)"
            note="tests $(grep -E '^ +[0-9]+ +FAIL' "$d/stdout" | awk '{print $1}' | head -12 | tr '\n' ' ')"
            [ -s "$d/stderr" ] && note="$note; $(head -1 "$d/stderr" | cut -c1-60)"
        fi
    fi
    key="N_$(echo "${outcome%%(*}" | tr '-' '_')"
    eval "$key=\$(( $key + 1 ))"
    printf '%s\t%s\t%s\n' "$name" "$outcome" "$note" >> "$OUT/results.tsv"
    if [ "$VERBOSE" = 1 ] || [ "$outcome" != PASS ]; then
        printf '  %-7s %-14s %s\n' "$name" "$outcome" "$note"
    fi
done
echo "----"
for k in PASS FAIL DIFF INSPECT COMPILE-FAIL ASM-FAIL LINK-FAIL RUN-FAULT RUN-FAIL TIMEOUT; do
    v="N_$(echo "$k" | tr '-' '_')"; eval "n=\$$v"
    [ "$n" -gt 0 ] && printf '  %-13s %4d\n' "$k" "$n"
done
echo "  of $TOTAL programs; outputs in $OUT"
[ "$N_PASS" = "$TOTAL" ]
