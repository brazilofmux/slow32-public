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
    (cd "$work" && "$EMU" "$SHEET_DIR/sheet.s32x" --line) \
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
# Formulas flatten to cached values in WK1. C1 was A1+1.
run_script wk1 "A1: 42" "B1: hello" "C1: 43" "D1: 3.5" || fail=1
run_script dbf "A1: 10" "B1: hello" "A2: 20" "B2: world" || fail=1

if command -v python3 >/dev/null 2>&1; then
    work="$(mktemp -d "${TMPDIR:-/tmp}/s32-sheet.XXXXXX")"
    python3 - "$work/foreign.wk1" <<'PY'
import struct, sys
def rec(op, data):
    return struct.pack('<HH', op, len(data)) + data
out = rec(0x0000, struct.pack('<H', 0x0406))
out += rec(0x000D, struct.pack('<BHH', 0xFF, 0, 0) + struct.pack('<h', 99))
out += rec(0x000E, struct.pack('<BHH', 0xFF, 1, 0) + struct.pack('<d', 1.25))
out += rec(0x000F, struct.pack('<BHH', 0xFF, 2, 0) + b"'lotus\x00")
out += rec(0x0001, b'')
open(sys.argv[1], 'wb').write(out)
PY
    printf 'LOAD foreign.wk1\nA1\nB1\nC1\nQUIT\n' > "$work/in.cmd"
    (cd "$work" && "$EMU" "$SHEET_DIR/sheet.s32x" --line) \
        < "$work/in.cmd" > "$work/out.txt" 2>"$work/err.txt" || true
    f2=0
    for pat in "A1: 99" "B1: 1.25" "C1: lotus"; do
        if grep -q "$pat" "$work/out.txt"; then
            echo "  OK  foreign-wk1: $pat"
        else
            echo "  FAIL foreign-wk1: $pat"
            f2=1
        fi
    done
    if [ "$f2" -ne 0 ]; then
        echo "=== foreign-wk1 output ---"
        cat "$work/out.txt"
        fail=1
    fi
    rm -rf "$work"
else
    echo "  SKIP foreign-wk1 (no python3)"
fi

if [ "$fail" -ne 0 ]; then
    echo "=== TESTS FAILED ==="
    exit 1
fi
echo "=== All tests passed ==="
