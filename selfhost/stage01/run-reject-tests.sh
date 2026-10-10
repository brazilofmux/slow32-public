#!/usr/bin/env bash
# Stage01 cc.fth: programs OUTSIDE the Subset-C contract must be rejected with
# a diagnostic -- "Compilation failed" within the time limit -- never hang or
# compile silently (selfhost ISSUES-54, 55, 56). Each tests/conformance/reject/*.c
# is one such program.
set -euo pipefail
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
SELFHOST_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"
ROOT_DIR="$(cd "$SELFHOST_DIR/.." && pwd)"
EMU="${STAGE01_CC_EMU:-$SELFHOST_DIR/stage00/s32-emu}"
KERNEL="${STAGE01_CC_KERNEL:-$ROOT_DIR/forth/kernel.s32x}"
PRELUDE="${STAGE01_CC_PRELUDE:-$ROOT_DIR/forth/prelude.fth}"
CC_FTH="${STAGE01_CC_CC:-$SCRIPT_DIR/cc.fth}"
REJECT_DIR="${STAGE01_CC_REJECT_DIR:-$SCRIPT_DIR/tests/conformance/reject}"
LIMIT="${SELFHOST_TIMEOUT:-60}"
while [[ $# -gt 0 ]]; do
    case "$1" in
        --emu) EMU="$2"; shift 2 ;;
        -h|--help) echo "Usage: $0 [--emu <path>]"; exit 0 ;;
        *) echo "unknown option: $1" >&2; exit 2 ;;
    esac
done
for f in "$EMU" "$KERNEL" "$PRELUDE" "$CC_FTH"; do
    [[ -e "$f" ]] || { echo "missing: $f" >&2; exit 1; }
done
WORK="$(mktemp -d)"
trap 'rm -rf "$WORK"' EXIT
fail=0; n=0
for src in "$REJECT_DIR"/*.c; do
    n=$((n+1))
    name="$(basename "$src" .c)"
    log="$WORK/$name.log"
    set +e
    cat "$PRELUDE" "$CC_FTH" - <<FTH | timeout "$LIMIT" "$EMU" "$KERNEL" >"$log" 2>&1
S" $src" S" $WORK/$name.s" COMPILE-FILE BYE
FTH
    rc=$?
    set -e
    if [[ $rc -eq 124 ]]; then
        echo "HANG: $name (no verdict in ${LIMIT}s)"; fail=1
    elif grep -q 'Compilation failed' "$log"; then
        echo "OK:   $name -- $(grep -m1 'Error line' "$log" | sed 's/ok> //g')"
    elif grep -q 'Compilation successful' "$log"; then
        echo "ACCEPTED: $name (should have been rejected)"; fail=1
    else
        echo "FAIL: $name (rc=$rc, no verdict)"; tail -n 5 "$log" | sed 's/^/    /'; fail=1
    fi
done
echo "reject tests: $n cases, $([[ $fail -eq 0 ]] && echo all rejected || echo FAILURES)"
exit $fail
