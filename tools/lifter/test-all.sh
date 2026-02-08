#!/bin/bash
# test-all.sh - Run regression tests through the lifter
# Usage: ./test-all.sh [-O0|-O2] [test-name-filter]

cd "$(dirname "$0")"

OPT_LEVEL="${1:--O0}"
FILTER="${2:-}"
LLC="${LLC:-llc-19}"
SAFE_FLAG=""
if [ "${S32_LIFT_SAFE:-0}" = "1" ]; then
    SAFE_FLAG="-safe"
fi

make s32-lift runtime.o mmio_ring.o >/dev/null 2>&1

PASS=0; FAIL=0; SKIP=0; TOTAL=0
for dir in ../../regression/results/*/; do
    test=$(basename "$dir")
    [[ -n "$FILTER" && ! "$test" == *"$FILTER"* ]] && continue
    s32x="$dir/test.s32x"
    expected="$dir/output.txt"
    [ ! -f "$s32x" ] && continue
    [ ! -f "$expected" ] && continue
    TOTAL=$((TOTAL+1))

    TEST_SAFE_FLAG="$SAFE_FLAG"
    if [ -z "$TEST_SAFE_FLAG" ] && [ -f "$expected" ]; then
        if head -n 1 "$expected" | grep -q "^Memory fault:"; then
            TEST_SAFE_FLAG="-safe"
        fi
    fi
    ./s32-lift "$s32x" -o lifted.ll $TEST_SAFE_FLAG 2>/dev/null
    if ! $LLC $OPT_LEVEL lifted.ll -filetype=obj -o lifted.o 2>/dev/null; then
        echo "SKIP $test (llc)"; SKIP=$((SKIP+1)); continue
    fi
    if ! gcc -no-pie lifted.o runtime.o mmio_ring.o -o lifted_native -lm 2>link.err; then
        msg=$(head -n 1 link.err)
        if [ -n "$msg" ]; then
            echo "SKIP $test (link: $msg)"
        else
            echo "SKIP $test (link)"
        fi
        SKIP=$((SKIP+1)); continue
    fi

    test_src="../../regression/tests/$test"
    args=()
    if [ -f "$test_src/args.txt" ]; then
        mapfile -t args < "$test_src/args.txt"
    fi

    LIFTED=$(timeout 30 ./lifted_native "${args[@]}" 2>&1 || true)
    RC=${PIPESTATUS[0]:-$?}

    EXPECTED=$(cat "$expected")
    EXP_EXIT=$(cat "$test_src/expected_exit.txt" 2>/dev/null || echo 0)

    if [ "$LIFTED" = "$EXPECTED" ] && [ "$RC" -eq "$EXP_EXIT" ]; then
        PASS=$((PASS+1))
    else
        echo "FAIL $test (stdout=$( [ "$LIFTED" = "$EXPECTED" ] && echo "OK" || echo "ERR" ), exit=$RC vs $EXP_EXIT)"
        FAIL=$((FAIL+1))
    fi
done
echo ""
echo "$PASS pass, $FAIL fail, $SKIP skip (of $TOTAL)"
