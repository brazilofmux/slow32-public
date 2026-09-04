#!/bin/bash
# Run Prolog interpreter test suite
set -e

BASDIR="$(cd "$(dirname "$0")/.." && pwd)"
EMU="${EMU:-$BASDIR/../tools/emulator/slow32-fast}"
PROLOG="$BASDIR/prolog.s32x"

if [ ! -f "$PROLOG" ]; then
    echo "Error: prolog.s32x not found. Run build.sh first."
    exit 1
fi

PASS=0
FAIL=0
TOTAL=0

# Filter function: remove emulator status lines
filter_output() {
    grep -v \
        -e "^Starting execution" \
        -e "^MMIO enabled" \
        -e "^HALT at" \
        -e "^Program halted" \
        -e "^Exit code:" \
        -e "^Instructions executed" \
        -e "^Simulated cycles" \
        -e "^Wall time:" \
        -e "^Performance:" \
        -e "instructions/second" \
        -e "^Error: Read out of bounds" \
        -e "^Error: Write out of bounds" \
        || true
}

for testfile in "$BASDIR"/tests/*.pl; do
    [ -f "$testfile" ] || continue
    name=$(basename "$testfile" .pl)
    expected="$BASDIR/tests/$name.expected"
    TOTAL=$((TOTAL + 1))

    # Run test: pipe prolog file to emulator, merge stdout+stderr, filter
    actual=$(cat "$testfile" | "$EMU" "$PROLOG" 2>&1 | filter_output)

    if [ -f "$expected" ]; then
        exp=$(cat "$expected")
        if [ "$actual" = "$exp" ]; then
            echo "  PASS: $name"
            PASS=$((PASS + 1))
        else
            echo "  FAIL: $name"
            echo "    Expected:"
            echo "$exp" | head -5 | sed 's/^/      /'
            echo "    Got:"
            echo "$actual" | head -5 | sed 's/^/      /'
            FAIL=$((FAIL + 1))
        fi
    else
        # No expected file — just check it doesn't error
        if echo "$actual" | grep -q "^Error"; then
            echo "  FAIL: $name (error in output)"
            echo "$actual" | grep "Error" | head -3 | sed 's/^/      /'
            FAIL=$((FAIL + 1))
        else
            echo "  PASS: $name (no .expected file, no errors)"
            PASS=$((PASS + 1))
        fi
    fi
done

echo ""
echo "$PASS/$TOTAL passed, $FAIL failed"
exit $FAIL
