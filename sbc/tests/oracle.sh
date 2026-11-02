#!/bin/bash
# The interpreter is the oracle, in bash alone (run-oracle.py's rules, no
# python3): every program is run by sbasic.s32x and compiled, with the
# same keys on stdin, and the two must print the same bytes.  For inside
# the slow32:basic image, where s32sbc compiles; in the tree it uses
# compile-bas.sh.
#
#   sbc/tests/oracle.sh [DIR_OR_FILE ...]      (default: sbasic/tests)
#
#   SBASIC    the interpreter (default: /opt/slow32/basic/sbasic.s32x
#             when there, else sbasic/sbasic.s32x)
#   EMU       the engine (default slow32-fast on PATH, else the tree's)
#   COMPILE   "COMPILE prog.bas out.s32x" compiles (default: s32sbc on
#             PATH, else bash sbc/compile-bas.sh)
#   TIMEOUT   seconds a run may take (default 60)
#
# A program's keys are its sibling .in or .input.  Verdicts as
# run-oracle.py's: AGREE; AGREE-REJECT (refused by the compiler and the
# interpreter alike); DIFFER, NOCOMPILE, TIMEOUT fail.  The last line:
#   sbc oracle: N agree, M agree-reject, 0 differ, 0 nocompile, 0 timeout (of T)
set -u
HERE="$(cd "$(dirname "$0")" && pwd)"
SBC_DIR="$(dirname "$HERE")"
ROOT="$(dirname "$SBC_DIR")"
if [ -z "${SBASIC:-}" ]; then
    if [ -f /opt/slow32/basic/sbasic.s32x ]; then SBASIC=/opt/slow32/basic/sbasic.s32x
    else SBASIC="$ROOT/sbasic/sbasic.s32x"; fi
fi
if [ -z "${EMU:-}" ]; then
    EMU=$(command -v slow32-fast || echo "$ROOT/tools/emulator/slow32-fast")
fi
if [ -z "${COMPILE:-}" ]; then
    if command -v s32sbc >/dev/null; then COMPILE=s32sbc
    else COMPILE="bash $SBC_DIR/compile-bas.sh"; fi
fi
TLIMIT="${TIMEOUT:-60}"
[ $# -eq 0 ] && set -- "$ROOT/sbasic/tests"

W=$(mktemp -d "${TMPDIR:-/tmp}/sbcoracle.XXXXXX") || exit 2
trap 'rm -rf "${W:?}"' EXIT

files=()
for a in "$@"; do
    if [ -d "$a" ]; then
        for f in "$a"/*.bas "$a"/*.BAS; do [ -f "$f" ] && files+=("$(cd "$(dirname "$f")" && pwd)/$(basename "$f")"); done
    elif [ -f "$a" ]; then
        files+=("$(cd "$(dirname "$a")" && pwd)/$(basename "$a")")
    fi
done

agree=0 reject=0 differ=0 nocomp=0 tmo=0
for f in "${files[@]}"; do
    base="${f%.*}"
    keys=/dev/null
    [ -f "$base.in" ] && keys="$base.in"
    [ -f "$base.input" ] && keys="$base.input"
    # where a program runs (and is compiled: $INCLUDE paths are relative):
    # its directory, but sbasic's tests from sbasic/, as run-tests.sh does
    cwd="$(dirname "$f")"
    if [ "$(basename "$cwd")" = tests ] && [ "$(basename "$(dirname "$cwd")")" = sbasic ]; then
        cwd="$(dirname "$cwd")"
    fi
    name="${f#"$ROOT"/}"
    rm -f "$W/p.s32x" "$W/i.raw" "$W/i.out" "$W/c.out" "$W/cc.err"
    (cd "$cwd" && $COMPILE "$f" "$W/p.s32x") >/dev/null 2>"$W/cc.err"
    crc=$?
    (cd "$cwd" && timeout "$TLIMIT" "$EMU" -q "$SBASIC" "$f" < "$keys" > "$W/i.raw" 2>/dev/null)
    irc=$?
    # the interpreter's "Loaded N lines from F" (a compiled program has no
    # loading to report), cut out byte for byte
    k=$(head -n 3 "$W/i.raw" | grep -n -m1 '^Loaded .* lines from ' | cut -d: -f1)
    if [ -n "$k" ]; then
        { [ "$k" -gt 1 ] && head -n $((k - 1)) "$W/i.raw"; tail -n +$((k + 1)) "$W/i.raw"; } > "$W/i.out"
    else
        cp "$W/i.raw" "$W/i.out"
    fi
    if [ $crc -ne 0 ] || [ ! -s "$W/p.s32x" ]; then
        msg=$(grep -v '^$' "$W/cc.err" | tail -n 1)
        # the interpreter refuses it too: a parse error, or the same error
        # before running a line (FOR without NEXT is found by lowering)
        if [ $irc -ne 124 ] && { head -c 13 "$W/i.out" | grep -q '^Error in line' ||
             [ "$(tr -d '\n' < "$W/i.out")" = "Error: ${msg##*: }" ]; }; then
            reject=$((reject + 1))
        else
            echo "$name NOCOMPILE $msg"
            nocomp=$((nocomp + 1))
        fi
        continue
    fi
    (cd "$cwd" && timeout "$TLIMIT" "$EMU" -q "$W/p.s32x" < "$keys" > "$W/c.out" 2>/dev/null)
    prc=$?
    if [ $irc -eq 124 ] || [ $prc -eq 124 ]; then
        echo "$name TIMEOUT"
        tmo=$((tmo + 1))
    elif cmp -s "$W/i.out" "$W/c.out"; then
        agree=$((agree + 1))
    else
        echo "$name DIFFER"
        diff "$W/i.out" "$W/c.out" | head -n 6 | sed 's/^/    /'
        differ=$((differ + 1))
    fi
done
echo "sbc oracle: $agree agree, $reject agree-reject, $differ differ, $nocomp nocompile, $tmo timeout (of ${#files[@]})"
[ $((differ + nocomp + tmo)) -eq 0 ]
