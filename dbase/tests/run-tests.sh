#!/bin/bash
# Run dBASE III clone test suite
set -e

BASDIR="$(cd "$(dirname "$0")/.." && pwd)"
EMU="${EMU:-$BASDIR/../tools/emulator/slow32-fast}"
EMU_ARGS="${EMU_ARGS---deny term}"
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
        -e "Starting execution" \
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
    "$BASDIR"/tests/testfile.txt "$BASDIR"/tests/ALT*.TXT "$BASDIR"/tests/RESULT*.TXT

for testfile in "$BASDIR"/tests/*.txt; do
    [ -f "$testfile" ] || continue
    name=$(basename "$testfile" .txt)
    expected="$BASDIR/tests/expected/$name.expected"
    TOTAL=$((TOTAL + 1))

    # Clean files before each test
    rm -f "$BASDIR"/tests/*.DBF "$BASDIR"/tests/*.DBT "$BASDIR"/tests/*.FRM "$BASDIR"/tests/*.LBL "$BASDIR"/tests/*.NDX \
        "$BASDIR"/tests/testfile.txt "$BASDIR"/tests/RESULT*.TXT

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

# Timed INKEY needs the term service (wait-for-any on stdin). The rest of
# the suite denies term so @SAY stays line-mode. DO a .PRG so the program
# text is a file; S32_STDIN_PREFIX is only the DO line; keys are real stdin.
run_inkey() {
    local name=$1
    local prg=$2
    local keys=$3
    local want=$4
    TOTAL=$((TOTAL + 1))
    local prefix="$BASDIR/tests/.$name.dot"
    printf 'DO %s\n' "$prg" > "$prefix"
    local actual
    if [ -n "$keys" ]; then
        actual=$( cd "$BASDIR/tests" && env S32_STDIN_PREFIX="$prefix" \
            timeout 5 "$EMU" "$DBASE" < <(sleep 0.3; printf '%s' "$keys") 2>&1 | filter_output )
    else
        actual=$( cd "$BASDIR/tests" && env S32_STDIN_PREFIX="$prefix" \
            timeout 5 "$EMU" "$DBASE" </dev/null 2>&1 | filter_output )
    fi
    rm -f "$prefix"
    if echo "$actual" | grep -q "$want"; then
        echo "  PASS: $name"
        PASS=$((PASS + 1))
    else
        echo "  FAIL: $name (want /$want/)"
        echo "$actual" | head -8 | sed 's/^/      /'
        FAIL=$((FAIL + 1))
    fi
}
run_inkey test_inkey_poll INKPOLL.PRG "" $'^\. 0$'
run_inkey test_inkey_timeout INKWAIT.PRG "" $'^\. 0$'
run_inkey test_inkey_key INKWAIT.PRG "A" $'^\. 65$'

# Final cleanup
rm -f "$BASDIR"/tests/*.DBF "$BASDIR"/tests/*.DBT "$BASDIR"/tests/*.FRM "$BASDIR"/tests/*.LBL "$BASDIR"/tests/*.NDX \
    "$BASDIR"/tests/testfile.txt "$BASDIR"/tests/ALT*.TXT "$BASDIR"/tests/RESULT*.TXT

echo ""
echo "$PASS/$TOTAL passed, $FAIL failed"
exit $FAIL
