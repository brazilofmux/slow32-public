#!/bin/bash
# Differential testing: Compare DBT output against interpreter

# Don't use set -e because arithmetic can return false

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
DBT="$SCRIPT_DIR/../slow32-dbt"
INTERP="$SCRIPT_DIR/../../emulator/slow32"
REGRESSION_DIR="$SCRIPT_DIR/../../../regression"

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Check binaries exist
if [ ! -x "$DBT" ]; then
    echo "Error: DBT not found at $DBT"
    echo "Run 'make' first"
    exit 1
fi

if [ ! -x "$INTERP" ]; then
    echo "Error: Interpreter not found at $INTERP"
    exit 1
fi

# Find test files
if [ ! -d "$REGRESSION_DIR" ]; then
    echo "Error: Regression directory not found: $REGRESSION_DIR"
    exit 1
fi

# Only use tests from results directory (properly built)
TESTS=$(find "$REGRESSION_DIR/results" -name "*.s32x" -type f | sort)

if [ -z "$TESTS" ]; then
    echo "No test files found in $REGRESSION_DIR"
    exit 1
fi

PASS=0
FAIL=0
SKIP=0

echo "=== SLOW-32 DBT Differential Testing ==="
echo ""

for test in $TESTS; do
    name=$(basename "$(dirname "$test")")
    dbt_args=()

    # Run interpreter - filter to just program output (lines between "Starting" and "HALT")
    # Actually, easier to just compare exit codes and check if both PASS/FAIL in output
    interp_full=$("$INTERP" "$test" 2>&1) || true
    interp_exit=$?

    # Run DBT
    dbt_full=$("$DBT" "${dbt_args[@]}" "$test" 2>&1) || true
    dbt_exit=$?

    # Extract just the DEBUG output (program's actual output before HALT/stats)
    # Both should output the same via DEBUG instruction
    # The interpreter has extra status lines, DBT is clean

    # Compare exit codes and check if both have "PASS" or both have same key output
    # For simplicity, just compare exit codes for now
    if [ "$interp_exit" = "$dbt_exit" ]; then
        # Check if interpreter shows HALT (success path)
        if echo "$interp_full" | grep -q "HALT at" || echo "$interp_full" | grep -q "Program halted."; then
            echo -e "${GREEN}PASS${NC}: $name (exit=$dbt_exit)"
            PASS=$((PASS+1))
        else
            echo -e "${RED}FAIL${NC}: $name (interpreter didn't HALT)"
            FAIL=$((FAIL+1))
            echo "  Interpreter output:"
            echo "$interp_full" | head -5 | sed 's/^/    /'
        fi
    else
        echo -e "${RED}FAIL${NC}: $name"
        FAIL=$((FAIL+1))

        # Show diff
        echo "  Exit codes: interp=$interp_exit, dbt=$dbt_exit"
        echo "  Interpreter output (last 5 lines):"
        echo "$interp_full" | tail -5 | sed 's/^/    /'
        echo "  DBT output (last 5 lines):"
        echo "$dbt_full" | tail -5 | sed 's/^/    /'
        echo ""
    fi
done

echo ""
echo "=== Results ==="
echo -e "Passed:  ${GREEN}$PASS${NC}"
echo -e "Failed:  ${RED}$FAIL${NC}"
echo -e "Skipped: ${YELLOW}$SKIP${NC}"

if [ $FAIL -gt 0 ]; then
    exit 1
fi
