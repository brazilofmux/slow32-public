#!/usr/bin/env bash
set -euo pipefail

# Stage 11: Compile the standard assembler (s32-as) with cc-min
#
# 1) Bootstrap via stage09 (obtains Gen2 cc-min, stage05 assembler, runtime)
# 2) cc-min compiles s32-as-port.c → s32-as-port.s
# 3) Stage05 assembler assembles → s32-as-port.s32o
# 4) Forth linker links → s32-as-port.s32x
# 5) Parity test: both assemblers assemble the same inputs, compare .s32o
# 6) Smoke test: cc-min-compiled assembler assembles a test, link + run

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
ROOT_DIR="${SELFHOST_ROOT:-$(cd "$SCRIPT_DIR/../.." && pwd)}"
if git -C "$SCRIPT_DIR" rev-parse --show-toplevel >/dev/null 2>&1; then
    ROOT_DIR="$(git -C "$SCRIPT_DIR" rev-parse --show-toplevel)"
fi

EMU="${STAGE11_EMU:-}"
EMU_EXPLICIT=0
KERNEL="${STAGE11_KERNEL:-$ROOT_DIR/forth/kernel.s32x}"
PRELUDE="${STAGE11_PRELUDE:-$ROOT_DIR/forth/prelude.fth}"
LINK_FTH="${STAGE11_LINK_FTH:-$ROOT_DIR/selfhost/stage03/link.fth}"

STAGE09_DIR="$ROOT_DIR/selfhost/stage09"
AS_PORT_SRC="$SCRIPT_DIR/s32-as-port.c"
SMOKE_SRC="$STAGE09_DIR/test_smoke.c"

CRT0_SRC="$ROOT_DIR/selfhost/stage01/crt0_minimal.s"
MMIO_SRC="$ROOT_DIR/selfhost/stage01/mmio_minimal.s"

KEEP_ARTIFACTS=0

choose_default_emu() {
    if [[ -x "$ROOT_DIR/tools/dbt/slow32-dbt" ]]; then
        printf '%s\n' "$ROOT_DIR/tools/dbt/slow32-dbt"
        return
    fi
    if [[ -x "$ROOT_DIR/tools/dbt/slow32-dbg" ]]; then
        printf '%s\n' "$ROOT_DIR/tools/dbt/slow32-dbg"
        return
    fi
    if [[ -x "$ROOT_DIR/tools/emulator/slow32-fast" ]]; then
        printf '%s\n' "$ROOT_DIR/tools/emulator/slow32-fast"
        return
    fi
    if [[ -x "$ROOT_DIR/tools/emulator/slow32" ]]; then
        printf '%s\n' "$ROOT_DIR/tools/emulator/slow32"
        return
    fi
    printf '%s\n' "$ROOT_DIR/tools/emulator/slow32-fast"
}

usage() {
    cat <<USAGE
Usage: $0 [--emu <path>] [--keep-artifacts]

Stage11 cc-min-compiled assembler:
  1) bootstrap Gen2 cc-min via stage09
  2) compile s32-as-port.c with cc-min
  3) assemble + link → s32-as-port.s32x
  4) parity test: compare .s32o output against stage05 assembler
  5) smoke test: assemble + link + run a test program
USAGE
}

while [[ $# -gt 0 ]]; do
    case "$1" in
        --emu)
            shift
            [[ $# -gt 0 ]] || { echo "--emu requires a path" >&2; exit 2; }
            EMU="$1"
            EMU_EXPLICIT=1
            ;;
        --keep-artifacts)
            KEEP_ARTIFACTS=1
            ;;
        -h|--help)
            usage
            exit 0
            ;;
        *)
            echo "Unknown option: $1" >&2
            usage
            exit 2
            ;;
    esac
    shift
done

if [[ "$EMU_EXPLICIT" -eq 0 && -z "${STAGE11_EMU:-}" ]]; then
    EMU="$(choose_default_emu)"
fi

if [[ "$EMU" != /* ]]; then
    EMU="$ROOT_DIR/$EMU"
fi

for f in "$EMU" "$KERNEL" "$PRELUDE" "$LINK_FTH" \
         "$AS_PORT_SRC" "$SMOKE_SRC" "$CRT0_SRC" "$MMIO_SRC"; do
    [[ -f "$f" ]] || { echo "Missing required file: $f" >&2; exit 1; }
done

WORKDIR="$(mktemp -d /tmp/selfhost-v2-stage11.XXXXXX)"
if [[ "$KEEP_ARTIFACTS" -eq 0 ]]; then
    trap 'rm -rf "$WORKDIR"' EXIT
fi

# --- Utility functions ---

run_forth() {
    local script_a="$1"
    local script_b="$2"
    local cmd_text="$3"
    local log_file="$4"

    set +e
    cat "$PRELUDE" "$script_a" "$script_b" - <<FTH | timeout 180 "$EMU" "$KERNEL" >"$log_file" 2>&1
$cmd_text
FTH
    local rc=$?
    set -e
    if [[ "$rc" -ne 0 && "$rc" -ne 96 ]]; then
        echo "forth pipeline failed (rc=$rc)" >&2
        tail -n 60 "$log_file" >&2
        return 1
    fi
}

run_exe() {
    local exe="$1"
    local log="$2"
    shift 2

    set +e
    timeout "${EXEC_TIMEOUT:-180}" "$EMU" "$exe" "$@" >"$log" 2>&1
    local rc=$?
    set -e
    if [[ "$rc" -eq 124 ]]; then
        echo "execution timed out: $exe" >&2
        tail -n 60 "$log" >&2
        return 1
    fi
    if grep -Eq "Execute fault|Memory fault|Write out of bounds or to protected memory|Unknown opcode|Unknown instruction|Load fault|Store fault|Execution limit reached" "$log"; then
        echo "execution faulted: $exe" >&2
        tail -n 60 "$log" >&2
        return 1
    fi
    if [[ "$rc" -ne 0 && "$rc" -ne 96 ]]; then
        echo "execution did not halt cleanly: $exe" >&2
        tail -n 60 "$log" >&2
        return 1
    fi
}

run_exe_any_rc() {
    local exe="$1"
    local log="$2"
    shift 2

    set +e
    timeout "${EXEC_TIMEOUT:-180}" "$EMU" "$exe" "$@" >"$log" 2>&1
    local rc=$?
    set -e
    if [[ "$rc" -eq 124 ]]; then
        echo "execution timed out: $exe" >&2
        tail -n 60 "$log" >&2
        return 124
    fi
    if grep -Eq "Execute fault|Memory fault|Write out of bounds or to protected memory|Unknown opcode|Unknown instruction|Load fault|Store fault|Execution limit reached" "$log"; then
        echo "execution faulted: $exe" >&2
        tail -n 60 "$log" >&2
        return 125
    fi
    return "$rc"
}

link_forth_with_libc() {
    local obj="$1"
    local exe="$2"
    local log="$3"

    run_forth "$LINK_FTH" /dev/null "LINK-INIT
S\" $RUNTIME_CRT0\" LINK-OBJ
S\" $obj\" LINK-OBJ
S\" $LIBC_START_OBJ\" LINK-OBJ
S\" $RUNTIME_MMIO_OBJ\" LINK-OBJ
65536 LINK-MMIO
S\" $LIBC_ARCHIVE\" LINK-ARCHIVE
S\" $exe\" LINK-EMIT
BYE" "$log"
    [[ -s "$exe" ]] || { echo "linker produced no output: $obj" >&2; return 1; }
}

# ============================================================
# Step 1: Bootstrap via stage09
# ============================================================
echo "=== Step 1: Bootstrap via stage09 ==="

S9_LOG="$WORKDIR/stage09-build.log"
EMU_FLAG=()
if [[ "$EMU_EXPLICIT" -eq 1 ]]; then
    EMU_FLAG=(--emu "$EMU")
fi
"$STAGE09_DIR/run-spike.sh" "${EMU_FLAG[@]}" --keep-artifacts >"$S9_LOG"

# Extract paths from stage09 output
GEN2_EXE="$(awk -F': ' '/^Gen2 exe:/{print $2}' "$S9_LOG" | tail -n 1)"
S9_ART="$(awk -F': ' '/^Artifacts:/{print $2}' "$S9_LOG" | tail -n 1)"

[[ -n "$GEN2_EXE" && -f "$GEN2_EXE" ]] || { echo "failed to locate Gen2 cc-min exe" >&2; exit 1; }
[[ -n "$S9_ART" && -d "$S9_ART" ]] || { echo "failed to locate stage09 artifacts" >&2; exit 1; }

# Find stage08 artifacts (nested inside stage09 workdir)
S8_LOG="$S9_ART/stage08-build.log"
[[ -f "$S8_LOG" ]] || { echo "missing stage08 build log: $S8_LOG" >&2; exit 1; }
AS_EXE="$(awk -F': ' '/^Assembler exe:/{print $2}' "$S8_LOG" | tail -n 1)"
S8_ART="$(awk -F': ' '/^Artifacts:/{print $2}' "$S8_LOG" | tail -n 1)"
[[ -n "$AS_EXE" && -f "$AS_EXE" ]] || { echo "failed to locate stage05 assembler" >&2; exit 1; }
[[ -n "$S8_ART" && -d "$S8_ART" ]] || { echo "failed to locate stage08 artifacts" >&2; exit 1; }

# Runtime objects from stage05 pipeline (nested inside stage08 workdir)
S5_LOG="$S8_ART/stage5-build.log"
[[ -f "$S5_LOG" ]] || { echo "missing stage05 build log: $S5_LOG" >&2; exit 1; }
S5_ART="$(awk -F': ' '/^Artifacts:/{print $2}' "$S5_LOG" | tail -n 1)"
[[ -n "$S5_ART" && -d "$S5_ART" ]] || { echo "failed to locate stage05 artifacts" >&2; exit 1; }

RUNTIME_CRT0="$S5_ART/crt0_minimal.s32o"
RUNTIME_MMIO_OBJ="$S5_ART/mmio_minimal.s32o"
LIBC_ARCHIVE="$S5_ART/libc_selfhost.s32a"
LIBC_START_OBJ="$S5_ART/libc_start.s32o"

[[ -s "$RUNTIME_CRT0" ]] || { echo "missing runtime crt0: $RUNTIME_CRT0" >&2; exit 1; }
[[ -s "$RUNTIME_MMIO_OBJ" ]] || { echo "missing runtime mmio: $RUNTIME_MMIO_OBJ" >&2; exit 1; }
[[ -s "$LIBC_ARCHIVE" ]] || { echo "missing libc archive: $LIBC_ARCHIVE" >&2; exit 1; }
[[ -s "$LIBC_START_OBJ" ]] || { echo "missing libc start: $LIBC_START_OBJ" >&2; exit 1; }

echo "Gen2 cc-min: $GEN2_EXE"
echo "Stage05 assembler: $AS_EXE"

# ============================================================
# Step 2: Compile s32-as-port.c with cc-min
# ============================================================
echo ""
echo "=== Step 2: Compile s32-as-port.c with cc-min ==="

# cc-min needs the include file alongside the source
cp "$SCRIPT_DIR/s32_formats_min.h" "$WORKDIR/s32_formats_min.h"
cp "$AS_PORT_SRC" "$WORKDIR/s32-as-port.c"

PORT_ASM="$WORKDIR/s32-as-port.s"
run_exe "$GEN2_EXE" "$WORKDIR/compile-as-port.log" "$WORKDIR/s32-as-port.c" "$PORT_ASM"
[[ -s "$PORT_ASM" ]] || { echo "cc-min produced no assembly for s32-as-port.c" >&2; cat "$WORKDIR/compile-as-port.log" >&2; exit 1; }

PORT_ASM_SZ="$(wc -c < "$PORT_ASM")"
echo "Assembly size: $PORT_ASM_SZ bytes"

# ============================================================
# Step 3: Assemble + link → s32-as-port.s32x
# ============================================================
echo ""
echo "=== Step 3: Assemble + link ==="

PORT_OBJ="$WORKDIR/s32-as-port.s32o"
PORT_EXE="$WORKDIR/s32-as-port.s32x"

run_exe "$AS_EXE" "$WORKDIR/assemble-as-port.log" "$PORT_ASM" "$PORT_OBJ"
[[ -s "$PORT_OBJ" ]] || { echo "assembler produced no output for s32-as-port" >&2; exit 1; }

link_forth_with_libc "$PORT_OBJ" "$PORT_EXE" "$WORKDIR/link-as-port.log"
echo "cc-min-compiled assembler: $PORT_EXE"

# ============================================================
# Step 4: Parity test — assemble cc-min output with both assemblers
# ============================================================
echo ""
echo "=== Step 4: Parity test ==="

PARITY_PASS=1

# Compile test_smoke.c with cc-min to get assembly with rN-style registers
SMOKE_ASM="$WORKDIR/smoke.s"
run_exe "$GEN2_EXE" "$WORKDIR/smoke-compile.log" "$SMOKE_SRC" "$SMOKE_ASM"
[[ -s "$SMOKE_ASM" ]] || { echo "cc-min produced no smoke assembly" >&2; exit 1; }

# Assemble smoke.s with both assemblers and compare
REF_SMOKE_OBJ="$WORKDIR/parity-smoke-ref.s32o"
NEW_SMOKE_OBJ="$WORKDIR/parity-smoke-new.s32o"
run_exe "$AS_EXE" "$WORKDIR/parity-smoke-ref.log" "$SMOKE_ASM" "$REF_SMOKE_OBJ"
run_exe "$PORT_EXE" "$WORKDIR/parity-smoke-new.log" "$SMOKE_ASM" "$NEW_SMOKE_OBJ"

if diff "$REF_SMOKE_OBJ" "$NEW_SMOKE_OBJ" >/dev/null 2>&1; then
    echo "  smoke.s: MATCH"
else
    echo "  smoke.s: MISMATCH" >&2
    PARITY_PASS=0
fi

# Also assemble s32-as-port.s (large file) with both assemblers
REF_PORT_OBJ="$WORKDIR/parity-port-ref.s32o"
NEW_PORT_OBJ="$WORKDIR/parity-port-new.s32o"
run_exe "$AS_EXE" "$WORKDIR/parity-port-ref.log" "$PORT_ASM" "$REF_PORT_OBJ"
run_exe "$PORT_EXE" "$WORKDIR/parity-port-new.log" "$PORT_ASM" "$NEW_PORT_OBJ"

if diff "$REF_PORT_OBJ" "$NEW_PORT_OBJ" >/dev/null 2>&1; then
    echo "  s32-as-port.s: MATCH"
else
    echo "  s32-as-port.s: MISMATCH" >&2
    PARITY_PASS=0
fi

if [[ "$PARITY_PASS" -eq 1 ]]; then
    echo "Parity: PASS"
else
    echo "Parity: FAIL" >&2
fi

# ============================================================
# Step 5: Smoke test — link + run using new assembler's output
# ============================================================
echo ""
echo "=== Step 5: Smoke test ==="

SMOKE_OBJ="$NEW_SMOKE_OBJ"
SMOKE_EXE="$WORKDIR/smoke.s32x"

# Link and run using the new assembler's .s32o
link_forth_with_libc "$SMOKE_OBJ" "$SMOKE_EXE" "$WORKDIR/smoke-link.log"

set +e
timeout "${EXEC_TIMEOUT:-180}" "$EMU" "$SMOKE_EXE" >"$WORKDIR/smoke-run.log" 2>&1
SMOKE_RC=$?
set -e

if [[ "$SMOKE_RC" -eq 42 ]]; then
    echo "Smoke test PASSED (rc=42)"
else
    echo "Smoke test FAILED: expected rc=42, got rc=$SMOKE_RC" >&2
fi

# ============================================================
# Final report
# ============================================================
echo ""
if [[ "$PARITY_PASS" -eq 1 && "$SMOKE_RC" -eq 42 ]]; then
    echo "OK: stage11 cc-min-compiled assembler"
else
    echo "FAIL: stage11 cc-min-compiled assembler" >&2
    [[ "$PARITY_PASS" -eq 1 ]] || echo "  - parity test failed" >&2
    [[ "$SMOKE_RC" -eq 42 ]] || echo "  - smoke test failed (rc=$SMOKE_RC)" >&2
    exit 1
fi

echo "Assembler exe: $PORT_EXE"
echo "Parity: PASS"
echo "Emulator: $EMU"
echo "Artifacts: $WORKDIR"
