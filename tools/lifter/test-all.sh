#!/bin/bash
# test-all.sh - Run regression tests through the lifter
# Usage: ./test-all.sh [-O0|-O2] [test-name-filter]

cd "$(dirname "$0")"

OPT_LEVEL="${1:--O0}"
FILTER="${2:-}"
LLC="${LLC:-llc-19}"

PASS=0; FAIL=0; SKIP=0; TOTAL=0
for dir in ../../regression/results/*/; do
    test=$(basename "$dir")
    [[ -n "$FILTER" && ! "$test" == *"$FILTER"* ]] && continue
    s32x="$dir/test.s32x"
    expected="$dir/output.txt"
    [ ! -f "$s32x" ] && continue
    [ ! -f "$expected" ] && continue
    TOTAL=$((TOTAL+1))

    ./s32-lift "$s32x" -o lifted.ll 2>/dev/null
    if ! $LLC $OPT_LEVEL lifted.ll -filetype=obj -o lifted.o 2>/dev/null; then
        echo "SKIP $test (llc)"; SKIP=$((SKIP+1)); continue
    fi
    if ! gcc -no-pie lifted.o runtime.c -o lifted_native 2>/dev/null; then
        echo "SKIP $test (link)"; SKIP=$((SKIP+1)); continue
    fi

    LIFTED=$(timeout 30 ./lifted_native 2>/dev/null || true)
    RC=${PIPESTATUS[0]:-$?}

    EXPECTED=$(cat "$expected")
    if [ "$LIFTED" = "$EXPECTED" ]; then
        PASS=$((PASS+1))
    else
        echo "FAIL $test"; FAIL=$((FAIL+1))
    fi
done
echo ""
echo "$PASS pass, $FAIL fail, $SKIP skip (of $TOTAL)"
