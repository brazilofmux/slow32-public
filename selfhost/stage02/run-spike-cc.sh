#!/usr/bin/env bash
set -euo pipefail

# Stage 08 smoke test: validate cc-min.s32x (the C compiler)
# Uses pre-built artifacts from earlier stages.
# Tests:
#   1. Compile a trivial C program, assemble, link, run
#   2. Compile a program with function calls
#   3. Compile a program with control flow

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
SELFHOST_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"
ROOT_DIR="$(cd "$SELFHOST_DIR/../.." && pwd 2>/dev/null || cd "$SELFHOST_DIR/.." && pwd)"
if git -C "$SCRIPT_DIR" rev-parse --show-toplevel >/dev/null 2>&1; then
    ROOT_DIR="$(git -C "$SCRIPT_DIR" rev-parse --show-toplevel)"
fi

EMU="${SELFHOST_EMU:-$SELFHOST_DIR/stage00/s32-emu}"
STAGE2_AS="$SELFHOST_DIR/stage02/s32-as.s32x"
STAGE2_LD="$SELFHOST_DIR/stage02/s32-ld.s32x"
STAGE8_CC="$SCRIPT_DIR/cc-min.s32x"

for f in "$EMU" "$STAGE2_AS" "$STAGE2_LD" "$STAGE8_CC"; do
    [[ -f "$f" ]] || { echo "Missing: $f" >&2; exit 1; }
done

WORKDIR="$(mktemp -d /tmp/stage02-cc-spike.XXXXXX)"
trap 'rm -rf "$WORKDIR"' EXIT

cd "$ROOT_DIR"

# Build a minimal crt0 that calls main and halts (for bare test programs)
cat > "$WORKDIR/minicrt0.s" <<'ASM'
.text
.global _start
_start:
    jal r31, main
    halt
ASM
set +e
timeout 30 "$EMU" "$STAGE2_AS" "$WORKDIR/minicrt0.s" "$WORKDIR/minicrt0.s32o" >/dev/null 2>&1
set -e
[[ -s "$WORKDIR/minicrt0.s32o" ]] || { echo "Failed to assemble minicrt0" >&2; exit 1; }

run_emu() {
    local exe="$1" log="$2"
    shift 2
    set +e
    timeout "${EXEC_TIMEOUT:-60}" "$EMU" "$exe" "$@" >"$log" 2>&1
    local rc=$?
    set -e
    echo "$rc"
}

compile_cc() {
    local src="$1" asm="$2" log="$3"
    set +e
    timeout "${EXEC_TIMEOUT:-60}" "$EMU" "$STAGE8_CC" "$src" "$asm" >"$log" 2>&1
    local rc=$?
    set -e
    if [[ "$rc" -ne 0 && "$rc" -ne 96 ]]; then
        echo "cc-min failed (rc=$rc): $src" >&2
        tail -n 20 "$log" >&2
        return 1
    fi
    [[ -s "$asm" ]] || { echo "cc-min produced no output: $src" >&2; return 1; }
}

assemble() {
    local src="$1" obj="$2" log="$3"
    set +e
    timeout 120 "$EMU" "$STAGE2_AS" "$src" "$obj" >"$log" 2>&1
    local rc=$?
    set -e
    if [[ "$rc" -ne 0 && "$rc" -ne 96 ]]; then
        echo "assemble failed (rc=$rc): $src" >&2
        tail -n 20 "$log" >&2
        return 1
    fi
    [[ -s "$obj" ]] || { echo "assemble produced no output: $src" >&2; return 1; }
}

link_exe() {
    local log="$1"
    shift
    set +e
    timeout 120 "$EMU" "$STAGE2_LD" "$@" >"$log" 2>&1
    local rc=$?
    set -e
    if [[ "$rc" -ne 0 && "$rc" -ne 96 ]]; then
        echo "link failed (rc=$rc)" >&2
        tail -n 20 "$log" >&2
        return 1
    fi
}

# compile_and_run: compile C source, assemble, link (bare _start), run, return exit code
compile_and_run() {
    local name="$1" src="$2" expected="$3"
    compile_cc "$src" "$WORKDIR/${name}.s" "$WORKDIR/${name}.cc.log"
    assemble "$WORKDIR/${name}.s" "$WORKDIR/${name}.s32o" "$WORKDIR/${name}.as.log"
    link_exe "$WORKDIR/${name}.ld.log" -o "$WORKDIR/${name}.s32x" "$WORKDIR/minicrt0.s32o" "$WORKDIR/${name}.s32o"
    [[ -s "$WORKDIR/${name}.s32x" ]] || { echo "FAIL ($name): no output"; return 1; }
    local rc
    rc=$(run_emu "$WORKDIR/${name}.s32x" "$WORKDIR/${name}.run.log")
    if [[ "$rc" -eq "$expected" ]]; then
        echo "  $name: PASS (rc=$rc)"
        return 0
    else
        echo "  $name: FAIL (expected $expected, got $rc)"
        return 1
    fi
}

PASS=0
FAIL=0

run_test() {
    if compile_and_run "$@"; then
        PASS=$((PASS+1))
    else
        FAIL=$((FAIL+1))
    fi
}

# --- Test 1: trivial return ---
echo "=== Test 1: trivial return ==="
run_test "ret42" "$SCRIPT_DIR/tests/min_ret7.c" 7

# --- Test 2: expression (2 + 3*4 = 14) ---
echo "=== Test 2: expression ==="
run_test "expr" "$SCRIPT_DIR/tests/min_ret_expr.c" 14

# --- Test 3: if/else (1+2==3 → return 9) ---
echo "=== Test 3: if/else ==="
run_test "if_true" "$SCRIPT_DIR/tests/min_if_true.c" 9

# --- Test 4: while loop ---
echo "=== Test 4: while loop ==="
run_test "while" "$SCRIPT_DIR/tests/min_while_countdown.c" 0

# --- Test 5: function call (helper returns 11) ---
echo "=== Test 5: function call ==="
run_test "helper" "$SCRIPT_DIR/tests/min_helper_call.c" 11

# --- Test 6: function with args (10+3=13) ---
echo "=== Test 6: function args ==="
run_test "helper_arg" "$SCRIPT_DIR/tests/min_helper_arg.c" 13

# --- Summary ---
echo ""
echo "Stage 08 smoke: $PASS passed, $FAIL failed"
if [[ "$FAIL" -gt 0 ]]; then
    exit 1
fi
