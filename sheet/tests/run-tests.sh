#!/bin/bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
SHEET_DIR="$(dirname "$SCRIPT_DIR")"
ROOT="$(dirname "$SHEET_DIR")"
EMU="${EMU:-$ROOT/tools/emulator/slow32-fast}"

echo "Building sheet..."
(cd "$SHEET_DIR" && bash build.sh)

if [ ! -x "$EMU" ]; then
    echo "emulator not found: $EMU"
    exit 1
fi

run_script() {
    local name="$1"
    shift
    local work
    work="$(mktemp -d "${TMPDIR:-/tmp}/s32-sheet.XXXXXX")"
    (cd "$work" && "$EMU" "$SHEET_DIR/sheet.s32x") \
        < "$SCRIPT_DIR/$name.cmd" > "$work/out.txt" 2>"$work/err.txt" || true
    local fail=0
    local pat
    for pat in "$@"; do
        if grep -q "$pat" "$work/out.txt"; then
            echo "  OK  $name: $pat"
        else
            echo "  FAIL $name: $pat"
            fail=1
        fi
    done
    if [ "$fail" -ne 0 ]; then
        echo "=== $name output ---"
        cat "$work/out.txt"
        rm -rf "$work"
        return 1
    fi
    rm -rf "$work"
    return 0
}

echo "=== Sheet tests ==="
fail=0
run_script arithmetic "A3: 50" "A4: 60" || fail=1
run_script sum "A4: 6" "A5: 2" "A6: 1" "A7: 3" || fail=1
run_script cycle "#CYCLE!" "#DIV/0!" || fail=1
run_script saveload "A1: 42" "B1: hello" "C1: 43" || fail=1

if [ "$fail" -ne 0 ]; then
    echo "=== TESTS FAILED ==="
    exit 1
fi
echo "=== All tests passed ==="
