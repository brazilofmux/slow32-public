#!/bin/bash
# Run SLOW BASIC test suite
set -e

BASDIR="$(cd "$(dirname "$0")/.." && pwd)"
EMU="${EMU:-$BASDIR/../tools/emulator/slow32-fast}"
SBASIC="$BASDIR/sbasic.s32x"

if [ ! -f "$SBASIC" ]; then
    echo "Error: sbasic.s32x not found. Run build.sh first."
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
        || true
}

for testfile in "$BASDIR"/tests/*.bas; do
    name=$(basename "$testfile" .bas)
    expected="$BASDIR/tests/$name.expected"
    TOTAL=$((TOTAL + 1))

    # Run test: merge stdout+stderr, filter emulator lines
    actual=$( (cat "$testfile"; echo "RUN") | "$EMU" "$SBASIC" 2>&1 | filter_output )

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
