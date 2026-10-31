#!/bin/bash
set -euo pipefail

CLIP_DIR="$(cd "$(dirname "$0")/.." && pwd)"
ROOT="$(dirname "$CLIP_DIR")"
EMU="${EMU:-$ROOT/tools/emulator/slow32-fast}"

echo "Building s32-xbc..."
(cd "$CLIP_DIR" && bash build.sh)

run_one() {   # run_one NAME [RUNDIR [MORE.prg ...]]: MORE named on the command line too
    local name="$1"
    local rundir="${2:-$CLIP_DIR/out}"
    local more=() f
    shift; [ $# -gt 0 ] && shift
    for f in "$@"; do more+=("$CLIP_DIR/tests/$f"); done
    echo "=== $name ==="
    rm -f "$CLIP_DIR/out/${name}.s32x"   # never the last build's program
    if ! bash "$CLIP_DIR/compile-prg.sh" "$CLIP_DIR/tests/${name}.prg" "$CLIP_DIR/out/${name}.s32x" ${more[@]+"${more[@]}"}; then
        echo "  FAIL $name (does not compile)"
        return 1
    fi
    (cd "$rundir" && "$EMU" -q "$CLIP_DIR/out/${name}.s32x") \
        > "$CLIP_DIR/out/${name}.out" 2>"$CLIP_DIR/out/${name}.err" || true
    if diff -u "$CLIP_DIR/tests/${name}.expected" "$CLIP_DIR/out/${name}.out"; then
        echo "  OK  $name"
        return 0
    fi
    echo "  FAIL $name"
    echo "--- stderr ---"
    cat "$CLIP_DIR/out/${name}.err"
    return 1
}

fail=0
run_one hello || fail=1
run_one loop || fail=1
run_one case || fail=1
run_one hybrid || fail=1
run_one libskip || fail=1
run_one dofile "$CLIP_DIR/tests" || fail=1
run_one doproc || fail=1
run_one setproc || fail=1
run_one listed "$CLIP_DIR/out" listlib.prg listsub.prg || fail=1
run_one func || fail=1
run_one funcpriv || fail=1
run_one funclocal || fail=1
run_one ifnum || fail=1
run_one funccase || fail=1
run_one money || fail=1

# The default handler with no key to come: the terminal service on, input
# closed.  It must quit (status 1), not wait (the timeout's 124).
echo "=== errquit ==="
rm -f "$CLIP_DIR/out/errquit.s32x"
if bash "$CLIP_DIR/compile-prg.sh" "$CLIP_DIR/tests/errquit.prg" "$CLIP_DIR/out/errquit.s32x" >/dev/null; then
    rc=0
    (cd "$CLIP_DIR/out" && timeout 30 "$EMU" -q "$CLIP_DIR/out/errquit.s32x" </dev/null >/dev/null 2>&1) || rc=$?
    if [ "$rc" -eq 1 ]; then echo "  OK  errquit"; else echo "  FAIL errquit (status $rc)"; fail=1; fi
else
    echo "  FAIL errquit (does not compile)"; fail=1
fi

if [ "$fail" -ne 0 ]; then
    echo "=== TESTS FAILED ==="
    exit 1
fi
echo "=== All xbc tests passed ==="
