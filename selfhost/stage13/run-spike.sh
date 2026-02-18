#!/usr/bin/env bash
set -euo pipefail

# Stage 13: Compile the linker (s32-ld) with cc-min
#
# Bootstrap chain:
# 1) Bootstrap via stage12 (obtains Gen2 cc-min, assembler, archiver, runtime)
#    — also produces the stage07 C linker via stage08
# 2) cc-min compiles s32-ld-port.c → s32-ld-port.s
# 3) Stage05 assembler assembles → s32-ld-port.s32o
# 4) Stage07 C linker links → s32-ld-port.s32x  (no Forth linker!)
# 5) Smoke test: C linker links a test program, run → rc=42
# 6) Archive test: C linker links against .s32a → rc=42

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
SELFHOST_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"
ROOT_DIR="$(cd "$SELFHOST_DIR/.." && pwd)"

EMU="${STAGE13_EMU:-}"
EMU_EXPLICIT=0

STAGE12_DIR="$SELFHOST_DIR/stage12"
LD_PORT_SRC="$SCRIPT_DIR/s32-ld-port.c"
SMOKE_SRC="$SELFHOST_DIR/stage09/test_smoke.c"

KEEP_ARTIFACTS=0

choose_default_emu() {
    printf '%s\n' "$SELFHOST_DIR/stage00/s32-emu"
}

usage() {
    cat <<USAGE
Usage: $0 [--emu <path>] [--keep-artifacts]

Stage13 cc-min-compiled linker:
  1) bootstrap Gen2 cc-min + assembler + archiver + C linker via stage12
  2) compile s32-ld-port.c with cc-min
  3) assemble + link (stage07 C linker) → s32-ld-port.s32x
  4) smoke test: C linker links test program → rc=42
  5) archive test: C linker links against .s32a → rc=42
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

if [[ "$EMU_EXPLICIT" -eq 0 && -z "${STAGE13_EMU:-}" ]]; then
    EMU="$(choose_default_emu)"
fi

for f in "$EMU" "$LD_PORT_SRC" "$SMOKE_SRC"; do
    [[ -f "$f" ]] || { echo "Missing required file: $f" >&2; exit 1; }
done

WORKDIR="$(mktemp -d /tmp/selfhost-v2-stage13.XXXXXX)"
if [[ "$KEEP_ARTIFACTS" -eq 0 ]]; then
    trap 'rm -rf "$WORKDIR"' EXIT
fi

# --- Utility functions ---

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

link_with_c_linker() {
    local obj="$1"
    local exe="$2"
    local log="$3"

    run_exe "$LD_EXE" "$log" \
        -o "$exe" --mmio 64K \
        "$RUNTIME_CRT0" "$obj" "$LIBC_START_OBJ" "$RUNTIME_MMIO_NO_START_OBJ" \
        "$LIBC_ARCHIVE"
    [[ -s "$exe" ]] || { echo "C linker produced no output: $obj" >&2; return 1; }
}

# ============================================================
# Step 1: Bootstrap via stage12
# ============================================================
echo "=== Step 1: Bootstrap via stage12 ==="

S12_LOG="$WORKDIR/stage12-build.log"
EMU_FLAG=()
if [[ "$EMU_EXPLICIT" -eq 1 ]]; then
    EMU_FLAG=(--emu "$EMU")
fi
"$STAGE12_DIR/run-spike.sh" "${EMU_FLAG[@]}" --keep-artifacts >"$S12_LOG"

# Extract paths from stage12 output
S12_ART="$(awk -F': ' '/^Artifacts:/{print $2}' "$S12_LOG" | tail -n 1)"
[[ -n "$S12_ART" && -d "$S12_ART" ]] || { echo "failed to locate stage12 artifacts" >&2; exit 1; }

# Find Gen2 cc-min via stage09 artifacts (nested inside stage12→stage11→stage09)
S11_LOG="$S12_ART/stage11-build.log"
[[ -f "$S11_LOG" ]] || { echo "missing stage11 build log: $S11_LOG" >&2; exit 1; }
S11_ART="$(awk -F': ' '/^Artifacts:/{print $2}' "$S11_LOG" | tail -n 1)"
[[ -n "$S11_ART" && -d "$S11_ART" ]] || { echo "failed to locate stage11 artifacts" >&2; exit 1; }

S9_LOG="$S11_ART/stage09-build.log"
[[ -f "$S9_LOG" ]] || { echo "missing stage09 build log: $S9_LOG" >&2; exit 1; }
GEN2_EXE="$(awk -F': ' '/^Gen2 exe:/{print $2}' "$S9_LOG" | tail -n 1)"
S9_ART="$(awk -F': ' '/^Artifacts:/{print $2}' "$S9_LOG" | tail -n 1)"
[[ -n "$GEN2_EXE" && -f "$GEN2_EXE" ]] || { echo "failed to locate Gen2 cc-min exe" >&2; exit 1; }

# Find assembler from stage08 (nested inside stage09)
S8_LOG="$S9_ART/stage08-build.log"
[[ -f "$S8_LOG" ]] || { echo "missing stage08 build log: $S8_LOG" >&2; exit 1; }
AS_EXE="$(awk -F': ' '/^Assembler exe:/{print $2}' "$S8_LOG" | tail -n 1)"
S8_ART="$(awk -F': ' '/^Artifacts:/{print $2}' "$S8_LOG" | tail -n 1)"
[[ -n "$AS_EXE" && -f "$AS_EXE" ]] || { echo "failed to locate stage05 assembler" >&2; exit 1; }

# Find stage07 C linker from stage08
LD_EXE="$(awk -F': ' '/^Linker exe:/{print $2}' "$S8_LOG" | tail -n 1)"
[[ -n "$LD_EXE" && -f "$LD_EXE" ]] || { echo "failed to locate stage07 C linker" >&2; exit 1; }

# Runtime objects from stage05 pipeline
S5_LOG="$S8_ART/stage5-build.log"
[[ -f "$S5_LOG" ]] || { echo "missing stage05 build log: $S5_LOG" >&2; exit 1; }
S5_ART="$(awk -F': ' '/^Artifacts:/{print $2}' "$S5_LOG" | tail -n 1)"
[[ -n "$S5_ART" && -d "$S5_ART" ]] || { echo "failed to locate stage05 artifacts" >&2; exit 1; }

RUNTIME_CRT0="$S5_ART/crt0_minimal.s32o"
RUNTIME_MMIO_NO_START_OBJ="$S5_ART/mmio_no_start.s32o"
LIBC_ARCHIVE="$S5_ART/libc_selfhost.s32a"
LIBC_START_OBJ="$S5_ART/libc_start.s32o"

[[ -s "$RUNTIME_CRT0" ]] || { echo "missing runtime crt0: $RUNTIME_CRT0" >&2; exit 1; }
[[ -s "$RUNTIME_MMIO_NO_START_OBJ" ]] || { echo "missing runtime mmio (no start): $RUNTIME_MMIO_NO_START_OBJ" >&2; exit 1; }
[[ -s "$LIBC_ARCHIVE" ]] || { echo "missing libc archive: $LIBC_ARCHIVE" >&2; exit 1; }
[[ -s "$LIBC_START_OBJ" ]] || { echo "missing libc start: $LIBC_START_OBJ" >&2; exit 1; }

echo "Gen2 cc-min: $GEN2_EXE"
echo "Stage05 assembler: $AS_EXE"
echo "Stage07 C linker: $LD_EXE"

# ============================================================
# Step 2: Compile s32-ld-port.c with cc-min
# ============================================================
echo ""
echo "=== Step 2: Compile s32-ld-port.c with cc-min ==="

cp "$LD_PORT_SRC" "$WORKDIR/s32-ld-port.c"
if [[ -f "$SCRIPT_DIR/s32ld_min.h" ]]; then
    cp "$SCRIPT_DIR/s32ld_min.h" "$WORKDIR/s32ld_min.h"
fi

PORT_ASM="$WORKDIR/s32-ld-port.s"
run_exe "$GEN2_EXE" "$WORKDIR/compile-ld-port.log" "$WORKDIR/s32-ld-port.c" "$PORT_ASM"
[[ -s "$PORT_ASM" ]] || { echo "cc-min produced no assembly for s32-ld-port.c" >&2; cat "$WORKDIR/compile-ld-port.log" >&2; exit 1; }

PORT_ASM_SZ="$(wc -c < "$PORT_ASM")"
echo "Assembly size: $PORT_ASM_SZ bytes"

# ============================================================
# Step 3: Assemble + link → s32-ld-port.s32x
# ============================================================
echo ""
echo "=== Step 3: Assemble + link (using stage07 C linker) ==="

PORT_OBJ="$WORKDIR/s32-ld-port.s32o"
PORT_EXE="$WORKDIR/s32-ld-port.s32x"

run_exe "$AS_EXE" "$WORKDIR/assemble-ld-port.log" "$PORT_ASM" "$PORT_OBJ"
[[ -s "$PORT_OBJ" ]] || { echo "assembler produced no output for s32-ld-port" >&2; exit 1; }

link_with_c_linker "$PORT_OBJ" "$PORT_EXE" "$WORKDIR/link-ld-port.log"
echo "cc-min-compiled linker: $PORT_EXE"

# ============================================================
# Step 4: Compile test program for smoke/archive tests
# ============================================================
echo ""
echo "=== Step 4: Prepare test program ==="

SMOKE_ASM="$WORKDIR/smoke.s"
run_exe "$GEN2_EXE" "$WORKDIR/smoke-compile.log" "$SMOKE_SRC" "$SMOKE_ASM"
[[ -s "$SMOKE_ASM" ]] || { echo "cc-min produced no smoke assembly" >&2; exit 1; }

SMOKE_OBJ="$WORKDIR/smoke.s32o"
run_exe "$AS_EXE" "$WORKDIR/smoke-assemble.log" "$SMOKE_ASM" "$SMOKE_OBJ"
[[ -s "$SMOKE_OBJ" ]] || { echo "assembler produced no smoke .s32o" >&2; exit 1; }

echo "Test program compiled: $SMOKE_OBJ"

# ============================================================
# Step 5: Smoke test — C linker links test program, run → rc=42
# ============================================================
echo ""
echo "=== Step 5: Smoke test ==="

SMOKE_PASS=1

C_EXE="$WORKDIR/smoke-c.s32x"
run_exe "$PORT_EXE" "$WORKDIR/smoke-c-link.log" \
    -o "$C_EXE" --mmio 64K \
    "$RUNTIME_CRT0" "$SMOKE_OBJ" "$LIBC_START_OBJ" "$RUNTIME_MMIO_NO_START_OBJ" \
    "$LIBC_ARCHIVE"

[[ -s "$C_EXE" ]] || { echo "C linker produced no output" >&2; SMOKE_PASS=0; }

if [[ "$SMOKE_PASS" -eq 1 ]]; then
    set +e
    timeout "${EXEC_TIMEOUT:-180}" "$EMU" "$C_EXE" >"$WORKDIR/smoke-run.log" 2>&1
    SMOKE_RC=$?
    set -e

    if [[ "$SMOKE_RC" -eq 42 ]]; then
        echo "  Smoke test: PASS (rc=42)"
    else
        echo "  Smoke test: FAIL (expected rc=42, got rc=$SMOKE_RC)" >&2
        tail -n 20 "$WORKDIR/smoke-run.log" >&2
        SMOKE_PASS=0
    fi
fi

if [[ "$SMOKE_PASS" -eq 1 ]]; then
    echo "Smoke test: PASS"
else
    echo "Smoke test: FAIL" >&2
fi

# ============================================================
# Step 6: Archive test — create archive and link against it
# ============================================================
echo ""
echo "=== Step 6: Archive test ==="

ARCHIVE_PASS=1

# Find archiver from stage12
AR_EXE="$(awk -F': ' '/^Archiver exe:/{print $2}' "$S12_LOG" | tail -n 1)"
if [[ -z "$AR_EXE" || ! -f "$AR_EXE" ]]; then
    AR_EXE="$S12_ART/s32-ar-port.s32x"
fi

if [[ -f "$AR_EXE" ]]; then
    # Gather libc .s32o files
    LIBC_OBJS=()
    for f in "$S5_ART"/*.s32o; do
        bn="$(basename "$f")"
        case "$bn" in
            crt0_minimal.s32o|libc_start.s32o|mmio_minimal.s32o|mmio_no_start.s32o) continue ;;
        esac
        LIBC_OBJS+=("$f")
    done

    if [[ "${#LIBC_OBJS[@]}" -gt 0 ]]; then
        # Create archive using cc-min archiver
        TEST_ARCHIVE="$WORKDIR/libc_test.s32a"
        run_exe "$AR_EXE" "$WORKDIR/ar-create.log" cs "$TEST_ARCHIVE" "${LIBC_OBJS[@]}"
        [[ -s "$TEST_ARCHIVE" ]] || { echo "archiver produced no output" >&2; ARCHIVE_PASS=0; }

        if [[ "$ARCHIVE_PASS" -eq 1 ]]; then
            # Link smoke test against our archive using cc-min linker
            AR_EXE_OUT="$WORKDIR/smoke-archive.s32x"
            run_exe "$PORT_EXE" "$WORKDIR/archive-link.log" \
                -o "$AR_EXE_OUT" --mmio 64K \
                "$RUNTIME_CRT0" "$SMOKE_OBJ" "$LIBC_START_OBJ" "$RUNTIME_MMIO_NO_START_OBJ" \
                "$TEST_ARCHIVE"

            [[ -s "$AR_EXE_OUT" ]] || { echo "C linker produced no archive output" >&2; ARCHIVE_PASS=0; }

            if [[ "$ARCHIVE_PASS" -eq 1 ]]; then
                set +e
                timeout "${EXEC_TIMEOUT:-180}" "$EMU" "$AR_EXE_OUT" >"$WORKDIR/archive-run.log" 2>&1
                AR_RC=$?
                set -e

                if [[ "$AR_RC" -eq 42 ]]; then
                    echo "  Archive link+run: PASS (rc=42)"
                else
                    echo "  Archive link+run: FAIL (expected rc=42, got rc=$AR_RC)" >&2
                    ARCHIVE_PASS=0
                fi
            fi
        fi
    else
        echo "  No libc .s32o files found for archive test" >&2
        ARCHIVE_PASS=0
    fi
else
    echo "  Archiver not found, skipping archive test" >&2
    ARCHIVE_PASS=0
fi

if [[ "$ARCHIVE_PASS" -eq 1 ]]; then
    echo "Archive test: PASS"
else
    echo "Archive test: FAIL" >&2
fi

# ============================================================
# Final report
# ============================================================
echo ""
if [[ "$SMOKE_PASS" -eq 1 && "$ARCHIVE_PASS" -eq 1 ]]; then
    echo "OK: stage13 cc-min-compiled linker"
else
    echo "FAIL: stage13 cc-min-compiled linker" >&2
    [[ "$SMOKE_PASS" -eq 1 ]] || echo "  - smoke test failed" >&2
    [[ "$ARCHIVE_PASS" -eq 1 ]] || echo "  - archive test failed" >&2
    exit 1
fi

echo "Linker exe: $PORT_EXE"
echo "Emulator: $EMU"
echo "Artifacts: $WORKDIR"
