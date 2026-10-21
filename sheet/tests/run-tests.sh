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
    # A round-trip script CLEARs between SAVE and LOAD.  Only what follows
    # the last "cleared" counts, or the echo of each assignment before the
    # SAVE would satisfy the check and the load could print anything.
    if grep -q '^>cleared' "$work/out.txt"; then
        awk '/^>cleared/ { n = 0 } { l[n++] = $0 } END { for (i = 0; i < n; i++) print l[i] }' \
            "$work/out.txt" > "$work/after.txt"
    else
        cp "$work/out.txt" "$work/after.txt"
    fi
    for pat in "$@"; do
        if grep -q "$pat" "$work/after.txt"; then
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
run_script sumexpr "A3: 30" "A4: 30" "A5: 30" \
    "C1: 0" "C2: 0" "C3: 0" "C4: 0" "C5: 10" || fail=1
run_script cycle "#CYCLE!" "#DIV/0!" || fail=1
run_script saveload "A1: 42" "B1: hello" "C1: 43" || fail=1
# Formulas flatten to cached values in WK1. C1 was A1+1.  E1 has eleven
# digits, past what %.10g kept; Y1 magnifies the digits of pi in F1 that
# %.10g dropped (GitHub issue 115).
run_script wk1 "A1: 42" "B1: hello" "C1: 43" "D1: 3.5" "E1: 12345678901" "Y1: 3.58979" || fail=1
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

    # A DBF written by something else (GitHub issue 117): a C field whose
    # text looks numeric must come back a label, N fields numbers, and a
    # deleted record must not take up a row.
    python3 - "$work/foreign.dbf" <<'PY'
import struct, sys
hdr = bytes([3, 0, 0, 0]) + struct.pack('<IHH', 3, 32 + 2 * 32 + 1, 11) + bytes(20)
f1 = b'A'.ljust(11, b'\0') + b'C' + bytes(4) + bytes([5, 0]) + bytes(14)
f2 = b'B'.ljust(11, b'\0') + b'N' + bytes(4) + bytes([5, 0]) + bytes(14)
recs = b' ' + b'00123' + b'   42' + b'*' + b'gone ' + b'    0' + b' ' + b'zz   ' + b'    7'
open(sys.argv[1], 'wb').write(hdr + f1 + f2 + b'\x0d' + recs + b'\x1a')
PY
    printf 'LOAD foreign.dbf\nA1\nB1\nA2\nB2\nA3\nX1==A1+1\nQUIT\n' > "$work/in2.cmd"
    (cd "$work" && "$EMU" "$SHEET_DIR/sheet.s32x" --line) \
        < "$work/in2.cmd" > "$work/out2.txt" 2>"$work/err2.txt" || true
    f3=0
    # X1: a label is worth 0 in arithmetic, a number 123 would give 124
    for pat in "A1: 00123" "B1: 42" "A2: zz" "B2: 7" "A3: (empty)" "X1: 1 "; do
        if grep -q "$pat" "$work/out2.txt"; then
            echo "  OK  foreign-dbf: $pat"
        else
            echo "  FAIL foreign-dbf: $pat"
            f3=1
        fi
    done
    if [ "$f3" -ne 0 ]; then
        echo "=== foreign-dbf output ---"
        cat "$work/out2.txt" "$work/err2.txt"
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
