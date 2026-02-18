#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
ROOT_DIR="$(cd "$SCRIPT_DIR/../.." && pwd)"

EMU_SLOW="${EMU_SLOW:-$ROOT_DIR/tools/emulator/slow32}"
EMU_FAST="${EMU_FAST:-$ROOT_DIR/tools/emulator/slow32-fast}"
EMU_DBT="${EMU_DBT:-$ROOT_DIR/tools/dbt/slow32-dbt}"

TEST_S32X="${1:-$ROOT_DIR/sbasic/sbasic.s32x}"
TEST_STDIN_FILE="${2:-$ROOT_DIR/sbasic/tests/hello.bas}"
TEST_STDIN_APPEND_RUN="${TEST_STDIN_APPEND_RUN:-1}"

for exe in "$EMU_SLOW" "$EMU_FAST" "$EMU_DBT"; do
    if [[ ! -x "$exe" ]]; then
        echo "Missing emulator: $exe" >&2
        exit 1
    fi
done

if [[ ! -f "$TEST_S32X" ]]; then
    echo "Missing test binary: $TEST_S32X" >&2
    exit 1
fi

if [[ ! -f "$TEST_STDIN_FILE" ]]; then
    echo "Missing stdin file: $TEST_STDIN_FILE" >&2
    exit 1
fi

WORKDIR="$(mktemp -d /tmp/intel-parity.XXXXXX)"
trap 'rm -rf "$WORKDIR"' EXIT

STDIN_FILE="$WORKDIR/stdin.txt"
cp "$TEST_STDIN_FILE" "$STDIN_FILE"
if [[ "$TEST_STDIN_APPEND_RUN" == "1" ]]; then
    printf '\nRUN\n' >> "$STDIN_FILE"
fi

run_one() {
    local name="$1"
    local emu="$2"
    local out="$WORKDIR/$name.out"
    local rc_file="$WORKDIR/$name.rc"

    set +e
    "$emu" "$TEST_S32X" < "$STDIN_FILE" >"$out" 2>&1
    local rc=$?
    set -e
    printf '%s\n' "$rc" >"$rc_file"
}

normalize_output() {
    sed \
        -e '/^Starting execution/d' \
        -e '/^MMIO enabled/d' \
        -e '/^HALT at/d' \
        -e '/^Program halted/d' \
        -e '/^Exit code:/d' \
        -e '/^Instructions executed/d' \
        -e '/^Simulated cycles:/d' \
        -e '/^Wall time:/d' \
        -e '/^Performance:/d' \
        -e '/^Cycles:/d' \
        -e '/instructions\/second/d' \
    | awk '
        { lines[NR] = $0; if ($0 ~ /[^[:space:]]/) last = NR }
        END { for (i = 1; i <= last; i++) print lines[i] }
    '
}

extract_fault() {
    grep -E '^DBT: Memory fault at PC=|^Error: Read out of bounds|^Error: Write out of bounds' | head -1 || true
}

run_one "slow" "$EMU_SLOW"
run_one "fast" "$EMU_FAST"
run_one "dbt" "$EMU_DBT"

for n in slow fast dbt; do
    normalize_output < "$WORKDIR/$n.out" > "$WORKDIR/$n.norm"
done

slow_rc="$(cat "$WORKDIR/slow.rc")"
fast_rc="$(cat "$WORKDIR/fast.rc")"
dbt_rc="$(cat "$WORKDIR/dbt.rc")"

echo "Test binary: $TEST_S32X"
echo "Stdin file:  $TEST_STDIN_FILE"
echo "Workdir:     $WORKDIR"
echo
echo "Exit codes:"
echo "  slow32      rc=$slow_rc"
echo "  slow32-fast rc=$fast_rc"
echo "  slow32-dbt  rc=$dbt_rc"
echo
echo "First fault signatures:"
echo "  slow32:      $(extract_fault < "$WORKDIR/slow.out")"
echo "  slow32-fast: $(extract_fault < "$WORKDIR/fast.out")"
echo "  slow32-dbt:  $(extract_fault < "$WORKDIR/dbt.out")"
echo

parity_ok=1
if [[ "$slow_rc" != "$fast_rc" || "$slow_rc" != "$dbt_rc" ]]; then
    parity_ok=0
    echo "Exit-code mismatch detected."
fi

if ! cmp -s "$WORKDIR/slow.norm" "$WORKDIR/fast.norm"; then
    parity_ok=0
    echo "Output mismatch: slow32 vs slow32-fast"
    diff -u "$WORKDIR/slow.norm" "$WORKDIR/fast.norm" | sed -n '1,80p'
fi

if ! cmp -s "$WORKDIR/slow.norm" "$WORKDIR/dbt.norm"; then
    parity_ok=0
    echo "Output mismatch: slow32 vs slow32-dbt"
    diff -u "$WORKDIR/slow.norm" "$WORKDIR/dbt.norm" | sed -n '1,80p'
fi

if [[ "$parity_ok" -eq 1 ]]; then
    echo "OK: emulator parity (normalized output + rc)"
    exit 0
fi

echo "FAIL: emulator parity diverged"
exit 1
