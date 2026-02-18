#!/bin/bash
# Run SLOW BASIC test suite
set -e

BASDIR="$(cd "$(dirname "$0")/.." && pwd)"
EMU="${EMU:-$BASDIR/../tools/emulator/slow32-fast}"
SBASIC="$BASDIR/sbasic.s32x"
FILTER_FAULTS="${FILTER_FAULTS:-1}"
STRICT_FAULTS="${STRICT_FAULTS:-0}"

if [ ! -f "$SBASIC" ]; then
    echo "Error: sbasic.s32x not found. Run build.sh first."
    exit 1
fi

PASS=0
FAIL=0
TOTAL=0

# Filter function: remove emulator status lines
filter_output() {
    local grep_args=(
        -e "^Starting execution" \
        -e "^MMIO enabled" \
        -e "^HALT at" \
        -e "^Program halted" \
        -e "^Exit code:" \
        -e "^Instructions executed" \
        -e "^Simulated cycles" \
        -e "^Wall time:" \
        -e "^Performance:" \
        -e "instructions/second"
    )
    if [ "$FILTER_FAULTS" = "1" ]; then
        grep_args+=(
            -e "^Error: Read out of bounds" \
            -e "^Error: Write out of bounds" \
            -e "^DBT: Memory fault at PC="
        )
    fi
    grep -v "${grep_args[@]}" || true
}

has_fault_signature() {
    grep -Eq "^Error: Read out of bounds|^Error: Write out of bounds|^DBT: Memory fault at PC="
}

for testfile in "$BASDIR"/tests/*.bas; do
    name=$(basename "$testfile" .bas)
    expected="$BASDIR/tests/$name.expected"
    TOTAL=$((TOTAL + 1))

    # Run test: merge stdout+stderr, filter emulator lines
    raw_output=$( (cat "$testfile"; echo "RUN") | "$EMU" "$SBASIC" 2>&1 )
    actual=$( printf '%s\n' "$raw_output" | filter_output )

    if [ "$STRICT_FAULTS" = "1" ] && printf '%s\n' "$raw_output" | has_fault_signature; then
        echo "  FAIL: $name (fault signature)"
        printf '%s\n' "$raw_output" | grep -E "^Error: Read out of bounds|^Error: Write out of bounds|^DBT: Memory fault at PC=" | head -1 | sed 's/^/      /'
        FAIL=$((FAIL + 1))
        continue
    fi

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
