#!/bin/bash
# Lua test runner for SLOW-32
# Usage: bash lua/tests/run-tests.sh

set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
LUA_DIR="$(dirname "$SCRIPT_DIR")"
ROOT_DIR="$(dirname "$LUA_DIR")"
EMULATOR="$ROOT_DIR/tools/emulator/slow32-fast"
BINARY="$LUA_DIR/lua.s32x"

if [ ! -f "$BINARY" ]; then
    echo "ERROR: $BINARY not found. Run build.sh first."
    exit 1
fi

if [ ! -f "$EMULATOR" ]; then
    echo "ERROR: $EMULATOR not found."
    exit 1
fi

PASS=0
FAIL=0
ERRORS=""

for testfile in "$SCRIPT_DIR"/*.lua; do
    testname=$(basename "$testfile" .lua)
    expected="$SCRIPT_DIR/$testname.expected"

    if [ ! -f "$expected" ]; then
        echo "  SKIP  $testname (no .expected file)"
        continue
    fi

    # Run test: pipe .lua file to emulator, capture stdout only
    # Filter out all emulator status lines (starting with known prefixes or containing MIPS/instructions)
    actual=$("$EMULATOR" "$BINARY" < "$testfile" 2>/dev/null | sed '/^Starting execution/d; /^MMIO enabled/d; /^HALT /d; /^Program halted/d; /^Exit code/d; /^Instructions executed/d; /^Simulated cycles/d; /^Wall time/d; /^Performance/d; /instructions\/second$/d')

    expected_content=$(cat "$expected")

    if [ "$actual" = "$expected_content" ]; then
        echo "  PASS  $testname"
        PASS=$((PASS + 1))
    else
        echo "  FAIL  $testname"
        FAIL=$((FAIL + 1))
        ERRORS="$ERRORS\n--- $testname ---\nExpected:\n$expected_content\nActual:\n$actual\n"
    fi
done

echo ""
echo "Results: $PASS passed, $FAIL failed out of $((PASS + FAIL)) tests"

if [ $FAIL -gt 0 ]; then
    echo ""
    echo "Failures:"
    echo -e "$ERRORS"
    exit 1
fi
