#!/bin/bash
# Forth Benchmark Runner
# Times bench.fth across all available execution engines.

set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
ROOT_DIR="$(dirname "$SCRIPT_DIR")"

KERNEL="$SCRIPT_DIR/kernel.s32x"
PRELUDE="$SCRIPT_DIR/prelude.fth"
BENCH="$SCRIPT_DIR/bench.fth"

EMU="$ROOT_DIR/tools/emulator/slow32"
EMU_FAST="$ROOT_DIR/tools/emulator/slow32-fast"
DBT="$ROOT_DIR/tools/dbt/slow32-dbt"

RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m'

EXPECTED="317811
1899
1000000"

# Strip emulator chrome, leaving only program output
filter_output() {
    sed 's/ok> //g' | \
    grep -v '^Starting execution' | \
    grep -v '^SLOW-32 Forth$' | \
    grep -v '^MMIO enabled' | \
    grep -v '^HALT ' | \
    grep -v '^Program halted' | \
    grep -v '^Exit code' | \
    grep -v '^Instructions executed' | \
    grep -v '^Simulated cycles' | \
    grep -v '^Cycles' | \
    grep -v '^Wall time' | \
    grep -v '^Performance' | \
    grep -v 'instructions/second' | \
    grep -v '^Time:' | \
    sed 's/[[:space:]]*$//' | \
    grep -v '^$'
}

run_bench() {
    local label="$1"
    shift
    local engine=("$@")

    if [ ! -x "${engine[0]}" ]; then
        printf "  %-28s ${YELLOW}SKIP${NC} (not found)\n" "$label:"
        return
    fi

    local out
    local wall
    local start end

    start=$(date +%s%N)
    out=$(cat "$PRELUDE" "$BENCH" | timeout 60 "${engine[@]}" "$KERNEL" 2>/dev/null | filter_output || true)
    end=$(date +%s%N)

    wall=$(echo "scale=3; ($end - $start) / 1000000000" | bc)

    if [ "$out" = "$EXPECTED" ]; then
        printf "  %-28s ${GREEN}%6ss${NC}  (correct)\n" "$label:" "$wall"
    else
        printf "  %-28s ${RED}%6ss${NC}  (WRONG OUTPUT)\n" "$label:" "$wall"
        echo "    Expected: $(echo "$EXPECTED" | tr '\n' ' ')"
        echo "    Got:      $(echo "$out" | tr '\n' ' ')"
    fi
}

if [ ! -f "$KERNEL" ]; then
    echo "Error: kernel.s32x not found. Run forth/build.sh first."
    exit 1
fi

echo "========================================"
echo "SLOW-32 Forth Benchmark Suite"
echo "========================================"
echo
echo "Benchmark: bench.fth (fib(28), sieve x10, nested 100^3)"
echo

run_bench "Interpreter"              "$EMU"
run_bench "Interpreter (fast)"       "$EMU_FAST"
run_bench "DBT Stage 3"             "$DBT" -3
run_bench "DBT Stage 4"             "$DBT" -4
run_bench "DBT Stage 4 (no regcache)" "$DBT" -4 -R

echo
echo "========================================"
