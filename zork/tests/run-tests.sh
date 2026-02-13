#!/bin/bash
set -e

# Run MojoZork tests
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
ZORK_DIR="$(dirname "$SCRIPT_DIR")"
ROOT_DIR="$(dirname "$ZORK_DIR")"
EMULATOR="${EMULATOR:-$ROOT_DIR/tools/emulator/slow32-fast}"
ZORK="$ZORK_DIR/zork.s32x"
STORY="$ZORK_DIR/stories/minizork.z3"

if [ ! -f "$ZORK" ]; then
    echo "ERROR: zork.s32x not found. Run build.sh first."
    exit 1
fi

if [ ! -f "$STORY" ]; then
    echo "ERROR: minizork.z3 not found in stories/"
    exit 1
fi

PASS=0
FAIL=0
TOTAL=0

run_test() {
    local name="$1"
    local input="$2"
    local expected_file="$SCRIPT_DIR/$name.expected"
    TOTAL=$((TOTAL + 1))

    if [ ! -f "$expected_file" ]; then
        echo "SKIP: $name (no expected file)"
        return
    fi

    # Run and capture only program output (filter emulator status lines)
    local actual
    actual=$(printf '%s' "$input" | $EMULATOR "$ZORK" "$STORY" 2>/dev/null | grep -v "^Starting execution\|^MMIO enabled\|^HALT at\|^Program halted\|^Exit code:\|^Instructions executed\|^Simulated cycles\|^Wall time:\|^Performance:\|instructions/second")

    local expected
    expected=$(cat "$expected_file")

    if [ "$actual" = "$expected" ]; then
        echo "PASS: $name"
        PASS=$((PASS + 1))
    else
        echo "FAIL: $name"
        echo "  Expected:"
        echo "$expected" | head -5 | sed 's/^/    /'
        echo "  Got:"
        echo "$actual" | head -5 | sed 's/^/    /'
        FAIL=$((FAIL + 1))
    fi
}

echo "=== MojoZork Tests ==="

# Test 1: Startup and quit
run_test "startup" "$(printf 'quit\ny\n')"

# Test 2: Look around
run_test "look" "$(printf 'look\nquit\ny\n')"

# Test 3: Explore
run_test "explore" "$(printf 'open mailbox\nread leaflet\ngo north\nquit\ny\n')"

echo ""
echo "Results: $PASS/$TOTAL passed, $FAIL failed"

if [ $FAIL -gt 0 ]; then
    exit 1
fi
