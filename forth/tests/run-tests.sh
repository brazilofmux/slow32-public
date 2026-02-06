#!/bin/bash
# Forth Kernel Regression Tests
# Pipes prelude + test through emulator, compares output against expected.

set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
FORTH_DIR="$(dirname "$SCRIPT_DIR")"
ROOT_DIR="$(dirname "$FORTH_DIR")"
EMULATOR="$ROOT_DIR/tools/emulator/slow32"
KERNEL="$FORTH_DIR/kernel.s32x"
PRELUDE="$FORTH_DIR/prelude.fth"

RED='\033[0;31m'
GREEN='\033[0;32m'
NC='\033[0m'

TOTAL=0
PASSED=0
FAILED=0

# Strip emulator chrome and Forth prompts, leaving only test output
filter_output() {
    sed 's/ok> //g' | \
    grep -v '^Starting execution$' | \
    grep -v '^SLOW-32 Forth$' | \
    grep -v '^HALT ' | \
    grep -v '^Program halted' | \
    grep -v '^Instructions executed' | \
    grep -v '^Cycles' | \
    grep -v '^Wall time' | \
    grep -v '^Performance' | \
    sed 's/[[:space:]]*$//' | \
    grep -v '^$'
}

echo "Forth Kernel Regression Tests"
echo "============================="
echo ""

for test_file in "$SCRIPT_DIR"/test-*.fth; do
    [ -f "$test_file" ] || continue
    test_name=$(basename "$test_file" .fth)
    expected="$SCRIPT_DIR/expected/${test_name}.expected"

    if [ ! -f "$expected" ]; then
        printf "%-30s ${RED}SKIP${NC} (no expected file)\n" "$test_name:"
        continue
    fi

    TOTAL=$((TOTAL + 1))

    actual=$(cat "$PRELUDE" "$test_file" | timeout 5 "$EMULATOR" "$KERNEL" 2>/dev/null | filter_output || true)
    expected_content=$(cat "$expected")

    if [ "$actual" = "$expected_content" ]; then
        printf "%-30s ${GREEN}PASS${NC}\n" "$test_name:"
        PASSED=$((PASSED + 1))
    else
        printf "%-30s ${RED}FAIL${NC}\n" "$test_name:"
        FAILED=$((FAILED + 1))
        diff --color=auto <(echo "$actual") <(echo "$expected_content") | head -20
        echo ""
    fi
done

echo ""
echo "============================="
echo "Results: $PASSED/$TOTAL passed, $FAILED failed"
if [ $FAILED -eq 0 ]; then
    echo -e "${GREEN}All tests passed!${NC}"
else
    echo -e "${RED}Some tests failed.${NC}"
    exit 1
fi
