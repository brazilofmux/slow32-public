#!/usr/bin/env bash
set -euo pipefail

# Stage 12: Compile the archiver (s32-ar) with cc-min
#
# 1) Bootstrap via stage11 (obtains Gen2 cc-min, assembler, runtime)
# 2) cc-min compiles s32-ar-port.c → s32-ar-port.s
# 3) Stage05 assembler assembles → s32-ar-port.s32o
# 4) Stage07 C linker links → s32-ar-port.s32x
# 5) Functional test: create archive, list contents
# 6) Link test: create libc archive with cc-min archiver, link a test program

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
SELFHOST_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"

EMU="${STAGE12_EMU:-}"
EMU_EXPLICIT=0
STAGE11_DIR="$SELFHOST_DIR/stage11"
AR_PORT_SRC="$SCRIPT_DIR/s32-ar-port.c"
SMOKE_SRC="$SELFHOST_DIR/stage09/test_smoke.c"

KEEP_ARTIFACTS=0

choose_default_emu() {
    printf '%s\n' "$SELFHOST_DIR/stage00/s32-emu"
}

usage() {
    cat <<USAGE
Usage: $0 [--emu <path>] [--keep-artifacts]

Stage12 cc-min-compiled archiver:
  1) bootstrap Gen2 cc-min + assembler via stage11
  2) compile s32-ar-port.c with cc-min
  3) assemble + link → s32-ar-port.s32x
  4) functional test: create archive, list contents
  5) link test: create libc archive, link test program, run → rc=42
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

if [[ "$EMU_EXPLICIT" -eq 0 && -z "${STAGE12_EMU:-}" ]]; then
    EMU="$(choose_default_emu)"
fi

for f in "$EMU" "$AR_PORT_SRC" "$SMOKE_SRC"; do
    [[ -f "$f" ]] || { echo "Missing required file: $f" >&2; exit 1; }
done

WORKDIR="$(mktemp -d /tmp/selfhost-v2-stage12.XXXXXX)"
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

link_with_c_linker_custom_archive() {
    local obj="$1"
    local archive="$2"
    local exe="$3"
    local log="$4"

    run_exe "$LD_EXE" "$log" \
        -o "$exe" --mmio 64K \
        "$RUNTIME_CRT0" "$obj" "$LIBC_START_OBJ" "$RUNTIME_MMIO_NO_START_OBJ" \
        "$archive"
    [[ -s "$exe" ]] || { echo "C linker produced no output: $obj" >&2; return 1; }
}

# ============================================================
# Step 1: Bootstrap via stage11
# ============================================================
echo "=== Step 1: Bootstrap via stage11 ==="

S11_LOG="$WORKDIR/stage11-build.log"
EMU_FLAG=()
if [[ "$EMU_EXPLICIT" -eq 1 ]]; then
    EMU_FLAG=(--emu "$EMU")
fi
"$STAGE11_DIR/run-spike.sh" "${EMU_FLAG[@]}" --keep-artifacts >"$S11_LOG"

# Extract paths from stage11 output
S11_ART="$(awk -F': ' '/^Artifacts:/{print $2}' "$S11_LOG" | tail -n 1)"
[[ -n "$S11_ART" && -d "$S11_ART" ]] || { echo "failed to locate stage11 artifacts" >&2; exit 1; }

# Find Gen2 cc-min via stage09 artifacts (nested inside stage11 workdir)
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
# Step 2: Compile s32-ar-port.c with cc-min
# ============================================================
echo ""
echo "=== Step 2: Compile s32-ar-port.c with cc-min ==="

# cc-min needs the include file alongside the source
cp "$SCRIPT_DIR/s32ar_min.h" "$WORKDIR/s32ar_min.h"
cp "$AR_PORT_SRC" "$WORKDIR/s32-ar-port.c"

PORT_ASM="$WORKDIR/s32-ar-port.s"
run_exe "$GEN2_EXE" "$WORKDIR/compile-ar-port.log" "$WORKDIR/s32-ar-port.c" "$PORT_ASM"
[[ -s "$PORT_ASM" ]] || { echo "cc-min produced no assembly for s32-ar-port.c" >&2; cat "$WORKDIR/compile-ar-port.log" >&2; exit 1; }

PORT_ASM_SZ="$(wc -c < "$PORT_ASM")"
echo "Assembly size: $PORT_ASM_SZ bytes"

# ============================================================
# Step 3: Assemble + link → s32-ar-port.s32x
# ============================================================
echo ""
echo "=== Step 3: Assemble + link ==="

PORT_OBJ="$WORKDIR/s32-ar-port.s32o"
PORT_EXE="$WORKDIR/s32-ar-port.s32x"

run_exe "$AS_EXE" "$WORKDIR/assemble-ar-port.log" "$PORT_ASM" "$PORT_OBJ"
[[ -s "$PORT_OBJ" ]] || { echo "assembler produced no output for s32-ar-port" >&2; exit 1; }

link_with_c_linker "$PORT_OBJ" "$PORT_EXE" "$WORKDIR/link-ar-port.log"
echo "cc-min-compiled archiver: $PORT_EXE"

# ============================================================
# Step 4: Functional test — create archive and list contents
# ============================================================
echo ""
echo "=== Step 4: Functional test ==="

FUNC_PASS=1

# Gather some .s32o files from the libc build to archive
LIBC_OBJS=()
for f in "$S5_ART"/*.s32o; do
    bn="$(basename "$f")"
    # Skip crt0 and start (those are linked as objects, not archived)
    case "$bn" in
        crt0_minimal.s32o|libc_start.s32o|mmio_minimal.s32o) continue ;;
    esac
    LIBC_OBJS+=("$f")
done

if [[ "${#LIBC_OBJS[@]}" -eq 0 ]]; then
    echo "  No libc .s32o files found for functional test" >&2
    FUNC_PASS=0
else
    TEST_ARCHIVE="$WORKDIR/test-func.s32a"

    # Create archive with cc-min archiver
    run_exe "$PORT_EXE" "$WORKDIR/ar-create.log" cs "$TEST_ARCHIVE" "${LIBC_OBJS[@]}"
    [[ -s "$TEST_ARCHIVE" ]] || { echo "archiver produced no output" >&2; FUNC_PASS=0; }

    if [[ "$FUNC_PASS" -eq 1 ]]; then
        # List archive contents
        run_exe "$PORT_EXE" "$WORKDIR/ar-list.log" t "$TEST_ARCHIVE"

        # Verify we got the right number of members
        LIST_LINES="$(wc -l < "$WORKDIR/ar-list.log")"
        EXPECTED="${#LIBC_OBJS[@]}"
        if [[ "$LIST_LINES" -eq "$EXPECTED" ]]; then
            echo "  Archive create+list: PASS ($EXPECTED members)"
        else
            echo "  Archive create+list: FAIL (expected $EXPECTED members, got $LIST_LINES)" >&2
            FUNC_PASS=0
        fi
    fi
fi

if [[ "$FUNC_PASS" -eq 1 ]]; then
    echo "Functional test: PASS"
else
    echo "Functional test: FAIL" >&2
fi

# ============================================================
# Step 5: Link test — archive works with C linker
# ============================================================
echo ""
echo "=== Step 5: Link test ==="

LINK_PASS=1

# Compile test_smoke.c with cc-min
SMOKE_ASM="$WORKDIR/smoke.s"
run_exe "$GEN2_EXE" "$WORKDIR/smoke-compile.log" "$SMOKE_SRC" "$SMOKE_ASM"
[[ -s "$SMOKE_ASM" ]] || { echo "cc-min produced no smoke assembly" >&2; LINK_PASS=0; }

if [[ "$LINK_PASS" -eq 1 ]]; then
    # Assemble smoke.s
    SMOKE_OBJ="$WORKDIR/smoke.s32o"
    run_exe "$AS_EXE" "$WORKDIR/smoke-assemble.log" "$SMOKE_ASM" "$SMOKE_OBJ"
    [[ -s "$SMOKE_OBJ" ]] || { echo "assembler produced no smoke .s32o" >&2; LINK_PASS=0; }
fi

if [[ "$LINK_PASS" -eq 1 ]]; then
    # Create libc archive using cc-min archiver (with symbol index)
    CCMIN_LIBC_ARCHIVE="$WORKDIR/libc_ccmin.s32a"
    run_exe "$PORT_EXE" "$WORKDIR/ar-libc-create.log" cs "$CCMIN_LIBC_ARCHIVE" "${LIBC_OBJS[@]}"
    [[ -s "$CCMIN_LIBC_ARCHIVE" ]] || { echo "archiver produced no libc archive" >&2; LINK_PASS=0; }
fi

if [[ "$LINK_PASS" -eq 1 ]]; then
    # Link smoke.s32o against cc-min-created archive using C linker
    SMOKE_EXE="$WORKDIR/smoke.s32x"
    link_with_c_linker_custom_archive "$SMOKE_OBJ" "$CCMIN_LIBC_ARCHIVE" "$SMOKE_EXE" "$WORKDIR/smoke-link.log"

    # Run the linked executable
    set +e
    timeout "${EXEC_TIMEOUT:-180}" "$EMU" "$SMOKE_EXE" >"$WORKDIR/smoke-run.log" 2>&1
    SMOKE_RC=$?
    set -e

    if [[ "$SMOKE_RC" -eq 42 ]]; then
        echo "  Smoke test: PASS (rc=42)"
    else
        echo "  Smoke test: FAIL (expected rc=42, got rc=$SMOKE_RC)" >&2
        LINK_PASS=0
    fi
fi

if [[ "$LINK_PASS" -eq 1 ]]; then
    echo "Link test: PASS"
else
    echo "Link test: FAIL" >&2
fi

# ============================================================
# Final report
# ============================================================
echo ""
if [[ "$FUNC_PASS" -eq 1 && "$LINK_PASS" -eq 1 ]]; then
    echo "OK: stage12 cc-min-compiled archiver"
else
    echo "FAIL: stage12 cc-min-compiled archiver" >&2
    [[ "$FUNC_PASS" -eq 1 ]] || echo "  - functional test failed" >&2
    [[ "$LINK_PASS" -eq 1 ]] || echo "  - link test failed" >&2
    exit 1
fi

echo "Archiver exe: $PORT_EXE"
echo "Link test: PASS"
echo "Emulator: $EMU"
echo "Artifacts: $WORKDIR"
