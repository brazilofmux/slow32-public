#!/usr/bin/env bash
set -euo pipefail

# Stage 09: Self-Compilation Fixed-Point Proof
#
# Gen1 (cc-min compiled by cc.fth) compiles cc-min → Gen2
# Gen2 compiles cc-min → Gen3
# If gen2.s == gen3.s, the compiler has reached a fixed point.

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
ROOT_DIR="${SELFHOST_ROOT:-$(cd "$SCRIPT_DIR/../.." && pwd)}"
if git -C "$SCRIPT_DIR" rev-parse --show-toplevel >/dev/null 2>&1; then
    ROOT_DIR="$(git -C "$SCRIPT_DIR" rev-parse --show-toplevel)"
fi

EMU="${STAGE9_EMU:-}"
EMU_EXPLICIT=0
KERNEL="${STAGE9_KERNEL:-$ROOT_DIR/forth/kernel.s32x}"
PRELUDE="${STAGE9_PRELUDE:-$ROOT_DIR/forth/prelude.fth}"
LINK_FTH="${STAGE9_LINK_FTH:-$ROOT_DIR/selfhost/stage03/link.fth}"

STAGE08_DIR="$ROOT_DIR/selfhost/stage08"
SRC_PASS1="$STAGE08_DIR/cc-min-pass1.c"
SRC_PASS2="$STAGE08_DIR/cc-min-pass2.c"
SRC_PASS3="$STAGE08_DIR/cc-min-pass3.c"
SRC_MAIN="$STAGE08_DIR/cc-min.c"
SMOKE_SRC="$SCRIPT_DIR/test_smoke.c"
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

Stage09 self-compilation fixed-point proof:
  1) bootstrap Gen1 cc-min via stage08
  2) Gen1 compiles cc-min → Gen2 assembly
  3) assemble + link Gen2 → gen2-cc-min.s32x
  4) Gen2 compiles cc-min → Gen3 assembly
  5) diff gen2.s gen3.s → must be identical (fixed point)
  6) smoke test: Gen2 compiles test_smoke.c, run → rc=42
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

if [[ "$EMU_EXPLICIT" -eq 0 && -z "${STAGE9_EMU:-}" ]]; then
    EMU="$(choose_default_emu)"
fi

if [[ "$EMU" != /* ]]; then
    EMU="$ROOT_DIR/$EMU"
fi

for f in "$EMU" "$KERNEL" "$PRELUDE" "$LINK_FTH" \
         "$SRC_PASS1" "$SRC_PASS2" "$SRC_PASS3" "$SRC_MAIN" "$SMOKE_SRC"; do
    [[ -f "$f" ]] || { echo "Missing required file: $f" >&2; exit 1; }
done

WORKDIR="$(mktemp -d /tmp/selfhost-v2-stage09.XXXXXX)"
if [[ "$KEEP_ARTIFACTS" -eq 0 ]]; then
    trap 'rm -rf "$WORKDIR"' EXIT
fi

# --- Utility functions (same pattern as stage08) ---

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
# Step 1: Bootstrap Gen1 via stage08
# ============================================================
echo "=== Step 1: Bootstrap Gen1 cc-min via stage08 ==="

S8_LOG="$WORKDIR/stage08-build.log"
EMU_FLAG=()
if [[ "$EMU_EXPLICIT" -eq 1 ]]; then
    EMU_FLAG=(--emu "$EMU")
fi
"$STAGE08_DIR/run-cc-spike.sh" "${EMU_FLAG[@]}" --keep-artifacts >"$S8_LOG"

# Extract paths from stage08 output
GEN1_EXE="$(awk -F': ' '/^Compiler exe:/{print $2}' "$S8_LOG" | tail -n 1)"
AS_EXE="$(awk -F': ' '/^Assembler exe:/{print $2}' "$S8_LOG" | tail -n 1)"
S8_ART="$(awk -F': ' '/^Artifacts:/{print $2}' "$S8_LOG" | tail -n 1)"

[[ -n "$GEN1_EXE" && -f "$GEN1_EXE" ]] || { echo "failed to locate Gen1 cc-min exe" >&2; exit 1; }
[[ -n "$AS_EXE" && -f "$AS_EXE" ]] || { echo "failed to locate stage05 assembler" >&2; exit 1; }
[[ -n "$S8_ART" && -d "$S8_ART" ]] || { echo "failed to locate stage08 artifacts" >&2; exit 1; }

# Runtime objects live in the stage05 pipeline artifacts (nested inside stage08 workdir)
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

echo "Gen1 cc-min: $GEN1_EXE"
echo "Assembler:   $AS_EXE"

# ============================================================
# Step 2: Gen1 compiles cc-min → Gen2 assembly
# ============================================================
echo ""
echo "=== Step 2: Gen1 compiles cc-min → Gen2 ==="

MERGED_SRC="$WORKDIR/cc-min.merged.c"
cat "$SRC_PASS1" "$SRC_PASS2" "$SRC_PASS3" "$SRC_MAIN" > "$MERGED_SRC"

MERGED_SZ="$(wc -c < "$MERGED_SRC")"
echo "Merged source size: $MERGED_SZ bytes"
if [[ "$MERGED_SZ" -ge 65536 ]]; then
    echo "ERROR: merged source ($MERGED_SZ bytes) exceeds MAX_SRC (65536)" >&2
    exit 1
fi

GEN2_ASM="$WORKDIR/gen2.s"
run_exe "$GEN1_EXE" "$WORKDIR/gen1-compile.log" "$MERGED_SRC" "$GEN2_ASM"
[[ -s "$GEN2_ASM" ]] || { echo "Gen1 produced no assembly output" >&2; exit 1; }

GEN2_ASM_SZ="$(wc -c < "$GEN2_ASM")"
echo "Gen2 assembly size: $GEN2_ASM_SZ bytes"

# ============================================================
# Step 3: Assemble + link Gen2 → gen2-cc-min.s32x
# ============================================================
echo ""
echo "=== Step 3: Assemble + link Gen2 ==="

GEN2_OBJ="$WORKDIR/gen2.s32o"
GEN2_EXE="$WORKDIR/gen2-cc-min.s32x"

run_exe "$AS_EXE" "$WORKDIR/gen2-assemble.log" "$GEN2_ASM" "$GEN2_OBJ"
[[ -s "$GEN2_OBJ" ]] || { echo "assembler produced no output for gen2" >&2; exit 1; }

link_forth_with_libc "$GEN2_OBJ" "$GEN2_EXE" "$WORKDIR/gen2-link.log"
echo "Gen2 exe: $GEN2_EXE"

# ============================================================
# Step 4: Gen2 compiles cc-min → Gen3 assembly
# ============================================================
echo ""
echo "=== Step 4: Gen2 compiles cc-min → Gen3 ==="

GEN3_ASM="$WORKDIR/gen3.s"
run_exe "$GEN2_EXE" "$WORKDIR/gen2-compile.log" "$MERGED_SRC" "$GEN3_ASM"
[[ -s "$GEN3_ASM" ]] || { echo "Gen2 produced no assembly output" >&2; exit 1; }

GEN3_ASM_SZ="$(wc -c < "$GEN3_ASM")"
echo "Gen3 assembly size: $GEN3_ASM_SZ bytes"

# ============================================================
# Step 5: Fixed-point check
# ============================================================
echo ""
echo "=== Step 5: Fixed-point check ==="

if diff "$GEN2_ASM" "$GEN3_ASM" >/dev/null 2>&1; then
    echo "FIXED POINT PROVEN: gen2.s and gen3.s are byte-identical"
    FIXED_POINT=1
else
    echo "FIXED POINT FAILED: gen2.s and gen3.s differ" >&2
    diff "$GEN2_ASM" "$GEN3_ASM" | head -n 40 >&2
    FIXED_POINT=0
fi

# ============================================================
# Step 6: Smoke test — Gen2 compiles test_smoke.c
# ============================================================
echo ""
echo "=== Step 6: Smoke test ==="

SMOKE_ASM="$WORKDIR/smoke.s"
SMOKE_OBJ="$WORKDIR/smoke.s32o"
SMOKE_EXE="$WORKDIR/smoke.s32x"

run_exe "$GEN2_EXE" "$WORKDIR/smoke-compile.log" "$SMOKE_SRC" "$SMOKE_ASM"
[[ -s "$SMOKE_ASM" ]] || { echo "Gen2 produced no smoke assembly output" >&2; exit 1; }

run_exe "$AS_EXE" "$WORKDIR/smoke-assemble.log" "$SMOKE_ASM" "$SMOKE_OBJ"
[[ -s "$SMOKE_OBJ" ]] || { echo "assembler produced no output for smoke test" >&2; exit 1; }

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
if [[ "$FIXED_POINT" -eq 1 && "$SMOKE_RC" -eq 42 ]]; then
    echo "OK: stage09 self-compilation fixed-point"
else
    echo "FAIL: stage09 self-compilation" >&2
    [[ "$FIXED_POINT" -eq 1 ]] || echo "  - fixed point check failed" >&2
    [[ "$SMOKE_RC" -eq 42 ]] || echo "  - smoke test failed (rc=$SMOKE_RC)" >&2
    exit 1
fi

echo "Gen1 exe: $GEN1_EXE"
echo "Gen2 exe: $GEN2_EXE"
echo "Gen2 assembly: $GEN2_ASM"
echo "Gen3 assembly: $GEN3_ASM"
echo "Emulator: $EMU"
echo "Artifacts: $WORKDIR"
