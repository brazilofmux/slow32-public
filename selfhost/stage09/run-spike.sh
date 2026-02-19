#!/usr/bin/env bash
set -euo pipefail

# Stage 09: Self-Compilation Fixed-Point Proof
#
# Gen1 (cc-min compiled by cc.fth, stage08) is already built.
# Gen2 (cc-min compiled by Gen1, stage09) is already built.
# This script:
#   1. Gen2 compiles cc-min → Gen3 assembly
#   2. Gen1 compiles cc-min → Gen2 assembly (for comparison)
#   3. Fixed-point check: gen2.s == gen3.s
#   4. Smoke test: Gen2 compiles test_smoke.c → rc=42

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
SELFHOST_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"
ROOT_DIR="$(cd "$SELFHOST_DIR/../.." && pwd 2>/dev/null || cd "$SELFHOST_DIR/.." && pwd)"
if git -C "$SCRIPT_DIR" rev-parse --show-toplevel >/dev/null 2>&1; then
    ROOT_DIR="$(git -C "$SCRIPT_DIR" rev-parse --show-toplevel)"
fi

EMU="${SELFHOST_EMU:-$SELFHOST_DIR/stage00/s32-emu}"
STAGE2_AS="$SELFHOST_DIR/stage02/s32-as.s32x"
STAGE7_LD="$SELFHOST_DIR/stage07/s32-ld.s32x"
GEN1_CC="$SELFHOST_DIR/stage08/cc-min.s32x"
GEN2_CC="$SCRIPT_DIR/cc-min.s32x"
SMOKE_SRC="$SCRIPT_DIR/test_smoke.c"

STAGE08_DIR="$SELFHOST_DIR/stage08"
CCMIN_PASS1="$STAGE08_DIR/cc-min-pass1.c"
CCMIN_PASS2="$STAGE08_DIR/cc-min-pass2.c"
CCMIN_PASS3="$STAGE08_DIR/cc-min-pass3.c"
CCMIN_MAIN="$STAGE08_DIR/cc-min.c"

for f in "$EMU" "$STAGE2_AS" "$STAGE7_LD" "$GEN1_CC" "$GEN2_CC" "$SMOKE_SRC" \
         "$CCMIN_PASS1" "$CCMIN_PASS2" "$CCMIN_PASS3" "$CCMIN_MAIN"; do
    [[ -f "$f" ]] || { echo "Missing: $f" >&2; exit 1; }
done

WORKDIR="$(mktemp -d /tmp/stage09-spike.XXXXXX)"
trap 'rm -rf "$WORKDIR"' EXIT

cd "$ROOT_DIR"

run_emu() {
    local exe="$1" log="$2"
    shift 2
    set +e
    timeout "${EXEC_TIMEOUT:-180}" "$EMU" "$exe" "$@" >"$log" 2>&1
    local rc=$?
    set -e
    echo "$rc"
}

run_cc() {
    local cc="$1" src="$2" asm="$3" log="$4"
    set +e
    timeout "${EXEC_TIMEOUT:-180}" "$EMU" "$cc" "$src" "$asm" >"$log" 2>&1
    local rc=$?
    set -e
    if [[ "$rc" -ne 0 && "$rc" -ne 96 ]]; then
        echo "compile failed (rc=$rc): $src" >&2
        tail -n 20 "$log" >&2
        return 1
    fi
    [[ -s "$asm" ]] || { echo "compile produced no output: $src" >&2; return 1; }
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
    timeout 120 "$EMU" "$STAGE7_LD" "$@" >"$log" 2>&1
    local rc=$?
    set -e
    if [[ "$rc" -ne 0 && "$rc" -ne 96 ]]; then
        echo "link failed (rc=$rc)" >&2
        tail -n 20 "$log" >&2
        return 1
    fi
}

PASS=0
FAIL=0

# --- Prepare merged source ---
MERGED_SRC="$WORKDIR/cc-min.merged.c"
cat "$CCMIN_PASS1" "$CCMIN_PASS2" "$CCMIN_PASS3" "$CCMIN_MAIN" > "$MERGED_SRC"
echo "Merged source: $(wc -c < "$MERGED_SRC") bytes"

# --- Step 1: Gen1 compiles cc-min → gen2.s ---
echo ""
echo "=== Step 1: Gen1 compiles cc-min ==="
run_cc "$GEN1_CC" "$MERGED_SRC" "$WORKDIR/gen2.s" "$WORKDIR/gen1.log"
echo "  gen2.s: $(wc -c < "$WORKDIR/gen2.s") bytes"

# --- Step 2: Gen2 compiles cc-min → gen3.s ---
echo "=== Step 2: Gen2 compiles cc-min ==="
run_cc "$GEN2_CC" "$MERGED_SRC" "$WORKDIR/gen3.s" "$WORKDIR/gen2.log"
echo "  gen3.s: $(wc -c < "$WORKDIR/gen3.s") bytes"

# --- Step 3: Fixed-point check ---
echo "=== Step 3: Fixed-point check ==="
if diff "$WORKDIR/gen2.s" "$WORKDIR/gen3.s" >/dev/null 2>&1; then
    echo "  FIXED POINT PROVEN: gen2.s == gen3.s"
    PASS=$((PASS+1))
else
    echo "  FIXED POINT FAILED: gen2.s != gen3.s"
    diff "$WORKDIR/gen2.s" "$WORKDIR/gen3.s" | head -n 20 >&2
    FAIL=$((FAIL+1))
fi

# --- Step 4: Smoke test — Gen2 compiles test_smoke.c → rc=42 ---
echo "=== Step 4: Smoke test ==="

# Build minicrt0
cat > "$WORKDIR/minicrt0.s" <<'ASM'
.text
.global _start
_start:
    jal r31, main
    halt
ASM
assemble "$WORKDIR/minicrt0.s" "$WORKDIR/minicrt0.s32o" "$WORKDIR/minicrt0.as.log"

run_cc "$GEN2_CC" "$SMOKE_SRC" "$WORKDIR/smoke.s" "$WORKDIR/smoke.cc.log"
assemble "$WORKDIR/smoke.s" "$WORKDIR/smoke.s32o" "$WORKDIR/smoke.as.log"
link_exe "$WORKDIR/smoke.ld.log" -o "$WORKDIR/smoke.s32x" "$WORKDIR/minicrt0.s32o" "$WORKDIR/smoke.s32o"
[[ -s "$WORKDIR/smoke.s32x" ]] || { echo "FAIL: no output"; FAIL=$((FAIL+1)); }

RC=$(run_emu "$WORKDIR/smoke.s32x" "$WORKDIR/smoke.run.log")
if [[ "$RC" -eq 42 ]]; then
    echo "  Smoke test: PASS (rc=42)"
    PASS=$((PASS+1))
else
    echo "  Smoke test: FAIL (expected 42, got $RC)"
    FAIL=$((FAIL+1))
fi

# --- Summary ---
echo ""
echo "Stage 09 fixed-point: $PASS passed, $FAIL failed"
if [[ "$FAIL" -gt 0 ]]; then
    exit 1
fi
