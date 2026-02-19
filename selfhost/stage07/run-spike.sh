#!/usr/bin/env bash
set -euo pipefail

# Stage 07 smoke test: validate s32-ld.s32x (the C linker)
# Uses pre-built artifacts from earlier stages.
# Tests:
#   1. Old-style CLI: input.s32o output.s32x
#   2. New-style CLI: -o output input.s32o
#   3. Multi-object with libc + MMIO

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
SELFHOST_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"
ROOT_DIR="$(cd "$SELFHOST_DIR/../.." && pwd 2>/dev/null || cd "$SELFHOST_DIR/.." && pwd)"
if git -C "$SCRIPT_DIR" rev-parse --show-toplevel >/dev/null 2>&1; then
    ROOT_DIR="$(git -C "$SCRIPT_DIR" rev-parse --show-toplevel)"
fi

EMU="${SELFHOST_EMU:-$SELFHOST_DIR/stage00/s32-emu}"
STAGE5_AS="$SELFHOST_DIR/stage05/s32-as.s32x"
STAGE7_LD="$SCRIPT_DIR/s32-ld.s32x"
CC_FTH="$SELFHOST_DIR/stage04/cc.fth"
KERNEL="${SELFHOST_KERNEL:-$ROOT_DIR/forth/kernel.s32x}"
PRELUDE="${SELFHOST_PRELUDE:-$ROOT_DIR/forth/prelude.fth}"

CRT0_SRC="$SELFHOST_DIR/stage05/crt0.s"
MMIO_NO_START_SRC="$SELFHOST_DIR/stage05/mmio_no_start.s"
LIBC_DIR="$SELFHOST_DIR/stage05/libc"

for f in "$EMU" "$STAGE5_AS" "$STAGE7_LD" "$CC_FTH" "$KERNEL" "$PRELUDE" \
         "$CRT0_SRC" "$MMIO_NO_START_SRC"; do
    [[ -f "$f" ]] || { echo "Missing: $f" >&2; exit 1; }
done

WORKDIR="$(mktemp -d /tmp/stage07-spike.XXXXXX)"
trap 'rm -rf "$WORKDIR"' EXIT

cd "$ROOT_DIR"

run_emu() {
    local exe="$1" log="$2"
    shift 2
    set +e
    timeout "${EXEC_TIMEOUT:-60}" "$EMU" "$exe" "$@" >"$log" 2>&1
    local rc=$?
    set -e
    echo "$rc"
}

assemble() {
    local src="$1" obj="$2" log="$3"
    set +e
    timeout 120 "$EMU" "$STAGE5_AS" "$src" "$obj" >"$log" 2>&1
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

compile() {
    local src="$1" asm="$2" log="$3"
    set +e
    cat "$PRELUDE" "$CC_FTH" - <<FTH | timeout 300 "$EMU" "$KERNEL" >"$log" 2>&1
S" $src" S" $asm" COMPILE-FILE
BYE
FTH
    local rc=$?
    set -e
    if [[ "$rc" -ne 0 && "$rc" -ne 96 ]]; then
        echo "compile failed (rc=$rc): $src" >&2
        tail -n 20 "$log" >&2
        return 1
    fi
    [[ -s "$asm" ]] || { echo "compile produced no output: $src" >&2; return 1; }
}

PASS=0
FAIL=0

# --- Test 1: Old-style CLI ---
echo "=== Test 1: Old-style CLI ==="
cat > "$WORKDIR/halt42.s" <<'ASM'
.text
.global _start
_start:
    addi r1, r0, 42
    halt
ASM

assemble "$WORKDIR/halt42.s" "$WORKDIR/halt42.s32o" "$WORKDIR/halt42.as.log"
link_exe "$WORKDIR/ld1.log" "$WORKDIR/halt42.s32o" "$WORKDIR/halt42.s32x"
[[ -s "$WORKDIR/halt42.s32x" ]] || { echo "FAIL: no output"; FAIL=$((FAIL+1)); }
RC=$(run_emu "$WORKDIR/halt42.s32x" "$WORKDIR/halt42.run.log")
if [[ "$RC" -eq 42 ]]; then
    echo "  Old-style single object: PASS (rc=42)"
    PASS=$((PASS+1))
else
    echo "  Old-style single object: FAIL (expected 42, got $RC)"
    FAIL=$((FAIL+1))
fi

# --- Test 2: New-style CLI ---
echo "=== Test 2: New-style CLI ==="
link_exe "$WORKDIR/ld2.log" -o "$WORKDIR/halt42_new.s32x" "$WORKDIR/halt42.s32o"
[[ -s "$WORKDIR/halt42_new.s32x" ]] || { echo "FAIL: no output"; FAIL=$((FAIL+1)); }
RC=$(run_emu "$WORKDIR/halt42_new.s32x" "$WORKDIR/halt42_new.run.log")
if [[ "$RC" -eq 42 ]]; then
    echo "  New-style single object: PASS (rc=42)"
    PASS=$((PASS+1))
else
    echo "  New-style single object: FAIL (expected 42, got $RC)"
    FAIL=$((FAIL+1))
fi

# --- Test 3: Multi-object with libc + MMIO ---
echo "=== Test 3: Multi-object with libc + MMIO ==="

# Build runtime
assemble "$CRT0_SRC" "$WORKDIR/crt0.s32o" "$WORKDIR/crt0.log"
assemble "$MMIO_NO_START_SRC" "$WORKDIR/mmio_no_start.s32o" "$WORKDIR/mmio_no_start.log"

# Build libc
for name in string_extra convert stdio; do
    compile "$LIBC_DIR/${name}.c" "$WORKDIR/${name}.s" "$WORKDIR/${name}.cc.log"
    assemble "$WORKDIR/${name}.s" "$WORKDIR/${name}.s32o" "$WORKDIR/${name}.as.log"
done
compile "$LIBC_DIR/start.c" "$WORKDIR/start.s" "$WORKDIR/start.cc.log"
assemble "$WORKDIR/start.s" "$WORKDIR/start.s32o" "$WORKDIR/start.as.log"

# Simple main() that returns 42
cat > "$WORKDIR/main42.s" <<'ASM'
.text
.global main
main:
    addi r1, r0, 42
    jalr r0, r31, 0
ASM
assemble "$WORKDIR/main42.s" "$WORKDIR/main42.s32o" "$WORKDIR/main42.as.log"

link_exe "$WORKDIR/ld3.log" -o "$WORKDIR/multi.s32x" --mmio 64K \
    "$WORKDIR/crt0.s32o" "$WORKDIR/main42.s32o" "$WORKDIR/start.s32o" \
    "$WORKDIR/mmio_no_start.s32o" \
    "$WORKDIR/string_extra.s32o" "$WORKDIR/convert.s32o" "$WORKDIR/stdio.s32o"
[[ -s "$WORKDIR/multi.s32x" ]] || { echo "FAIL: no output"; FAIL=$((FAIL+1)); }
RC=$(run_emu "$WORKDIR/multi.s32x" "$WORKDIR/multi.run.log")
if [[ "$RC" -eq 42 ]]; then
    echo "  Multi-object with libc: PASS (rc=42)"
    PASS=$((PASS+1))
else
    echo "  Multi-object with libc: FAIL (expected 42, got $RC)"
    tail -n 10 "$WORKDIR/multi.run.log" >&2
    FAIL=$((FAIL+1))
fi

# --- Summary ---
echo ""
echo "Stage 07 smoke: $PASS passed, $FAIL failed"
if [[ "$FAIL" -gt 0 ]]; then
    exit 1
fi
