#!/bin/bash
set -euo pipefail

CLIP_DIR="$(cd "$(dirname "$0")/.." && pwd)"
ROOT="$(dirname "$CLIP_DIR")"
EMU="${EMU:-$ROOT/tools/emulator/slow32-fast}"

echo "Building s32-clip..."
(cd "$CLIP_DIR" && bash build.sh)

run_one() {
    local name="$1"
    local rundir="${2:-$CLIP_DIR/out}"
    echo "=== $name ==="
    bash "$CLIP_DIR/compile-prg.sh" "$CLIP_DIR/tests/${name}.prg" "$CLIP_DIR/out/${name}.s32x"
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
run_one func || fail=1
run_one funcpriv || fail=1
run_one funccase || fail=1
run_one money || fail=1

if [ "$fail" -ne 0 ]; then
    echo "=== TESTS FAILED ==="
    exit 1
fi
echo "=== All clip tests passed ==="
