#!/bin/bash
# Run dBASE III clone test suite
set -e

BASDIR="$(cd "$(dirname "$0")/.." && pwd)"
EMU="${EMU:-$BASDIR/../tools/emulator/slow32-fast}"
EMU_ARGS="--deny term"
DBASE="$BASDIR/dbase.s32x"

if [ ! -f "$DBASE" ]; then
    echo "Error: dbase.s32x not found. Run build.sh first."
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

# Clean up any leftover files from previous runs
rm -f "$BASDIR"/tests/*.DBF "$BASDIR"/tests/*.DBT "$BASDIR"/tests/*.FRM "$BASDIR"/tests/*.LBL "$BASDIR"/tests/*.NDX \
    "$BASDIR"/tests/testfile.txt

for testfile in "$BASDIR"/tests/*.txt; do
    [ -f "$testfile" ] || continue
    name=$(basename "$testfile" .txt)
    expected="$BASDIR/tests/expected/$name.expected"
    TOTAL=$((TOTAL + 1))

    # Clean files before each test
    rm -f "$BASDIR"/tests/*.DBF "$BASDIR"/tests/*.FRM "$BASDIR"/tests/*.LBL "$BASDIR"/tests/*.NDX \
        "$BASDIR"/tests/testfile.txt

    # Run test from tests/ directory so .DBF files are created there
    actual=$( cd "$BASDIR/tests" && cat "$testfile" | "$EMU" $EMU_ARGS "$DBASE" 2>&1 | filter_output )

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
        if echo "$actual" | grep -qi "^error"; then
            echo "  FAIL: $name (error in output)"
            echo "$actual" | grep -i "error" | head -3 | sed 's/^/      /'
            FAIL=$((FAIL + 1))
        else
            echo "  PASS: $name (no .expected file, no errors)"
            PASS=$((PASS + 1))
        fi
    fi
done

# Final cleanup
rm -f "$BASDIR"/tests/*.DBF "$BASDIR"/tests/*.DBT "$BASDIR"/tests/*.FRM "$BASDIR"/tests/*.LBL "$BASDIR"/tests/*.NDX \
    "$BASDIR"/tests/testfile.txt

echo ""
echo "$PASS/$TOTAL passed, $FAIL failed"
exit $FAIL
