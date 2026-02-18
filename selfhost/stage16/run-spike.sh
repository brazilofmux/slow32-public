#!/usr/bin/env bash
set -euo pipefail

# Stage 16: Recompile archiver (s32-ar) with s32-cc
#
# 1) Bootstrap via stage15 (obtains s32-cc, new assembler, C linker, runtime)
# 2) Compile s32-ar.c with s32-cc + new assembler + link
# 3) Functional test: create archive, list contents
# 4) Link test: create libc archive with new archiver, link test program, run

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
SELFHOST_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"

EMU="${STAGE16_EMU:-}"
EMU_EXPLICIT=0
STAGE15_DIR="$SELFHOST_DIR/stage15"
KEEP_ARTIFACTS=0

choose_default_emu() {
    printf '%s\n' "$SELFHOST_DIR/stage00/s32-emu"
}

usage() {
    cat <<USAGE
Usage: $0 [--emu <path>] [--keep-artifacts]

Stage16 s32-cc-compiled archiver:
  1) bootstrap via stage15 (s32-cc, assembler, linker, runtime)
  2) compile s32-ar.c with s32-cc
  3) functional test: create archive, list contents
  4) link test: create libc archive, link test program, run
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

if [[ "$EMU_EXPLICIT" -eq 0 && -z "${STAGE16_EMU:-}" ]]; then
    EMU="$(choose_default_emu)"
fi

[[ -f "$EMU" ]] || { echo "Missing emulator: $EMU" >&2; exit 1; }

WORKDIR="$(mktemp -d /tmp/selfhost-v2-stage16.XXXXXX)"
if [[ "$KEEP_ARTIFACTS" -eq 0 ]]; then
    trap 'rm -rf "$WORKDIR"' EXIT
fi

# --- Utility functions ---

run_exe() {
    local exe="$1"
    local log="$2"
    shift 2

    local rc=0
    timeout "${EXEC_TIMEOUT:-180}" "$EMU" "$exe" "$@" >"$log" 2>&1 || rc=$?
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

run_exe_rc() {
    local exe="$1"
    local log="$2"
    shift 2

    local rc=0
    timeout "${EXEC_TIMEOUT:-180}" "$EMU" "$exe" "$@" >"$log" 2>&1 || rc=$?
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

PASS=0
FAIL=0
TOTAL=0

# ============================================================
# Step 1: Bootstrap via stage15
# ============================================================
echo "=== Step 1: Bootstrap via stage15 ==="

S15_LOG="$WORKDIR/stage15-build.log"
EMU_FLAG=()
if [[ "$EMU_EXPLICIT" -eq 1 ]]; then
    EMU_FLAG=(--emu "$EMU")
fi
"$STAGE15_DIR/run-spike.sh" "${EMU_FLAG[@]}" --keep-artifacts >"$S15_LOG"

# Extract artifacts from stage15 output
S15_ART="$(awk -F': ' '/^Artifacts:/{print $2}' "$S15_LOG" | tail -n 1)"
[[ -n "$S15_ART" && -d "$S15_ART" ]] || { echo "failed to locate stage15 artifacts" >&2; exit 1; }

S32CC_EXE="$(awk -F': ' '/^Compiler exe:/{print $2}' "$S15_LOG" | tail -n 1)"
AS_EXE="$(awk -F': ' '/^New assembler:/{print $2}' "$S15_LOG" | tail -n 1)"
LD_EXE="$(awk -F': ' '/^Linker exe:/{print $2}' "$S15_LOG" | tail -n 1)"
RUNTIME_CRT0="$(awk -F': ' '/^Runtime crt0:/{print $2}' "$S15_LOG" | tail -n 1)"
RUNTIME_MMIO_NO_START_OBJ="$(awk -F': ' '/^Runtime mmio:/{print $2}' "$S15_LOG" | tail -n 1)"
LIBC_ARCHIVE="$(awk -F': ' '/^Libc archive:/{print $2}' "$S15_LOG" | tail -n 1)"
LIBC_START_OBJ="$(awk -F': ' '/^Libc start:/{print $2}' "$S15_LOG" | tail -n 1)"

for f in "$S32CC_EXE" "$AS_EXE" "$LD_EXE" "$RUNTIME_CRT0" "$RUNTIME_MMIO_NO_START_OBJ" "$LIBC_ARCHIVE" "$LIBC_START_OBJ"; do
    [[ -s "$f" ]] || { echo "Missing artifact: $f" >&2; exit 1; }
done

echo "Compiler: $S32CC_EXE"
echo "Assembler: $AS_EXE"
echo "Linker: $LD_EXE"

# ============================================================
# Step 2: Compile s32-ar.c with s32-cc
# ============================================================
echo ""
echo "=== Step 2: Compile s32-ar.c ==="

# Copy source to workdir for #include resolution
cp "$SCRIPT_DIR/s32-ar.c" "$WORKDIR/s32-ar.c"
cp "$SCRIPT_DIR/s32ar_min.h" "$WORKDIR/s32ar_min.h"

AR_ASM="$WORKDIR/s32-ar.s"
AR_OBJ="$WORKDIR/s32-ar.s32o"
AR_EXE="$WORKDIR/s32-ar.s32x"

SAVED_TIMEOUT="${EXEC_TIMEOUT:-180}"
EXEC_TIMEOUT=300

TOTAL=$((TOTAL + 1))

run_exe "$S32CC_EXE" "$WORKDIR/ar-compile.log" "$WORKDIR/s32-ar.c" "$AR_ASM"
if [[ ! -s "$AR_ASM" ]]; then
    echo "s32-cc produced no assembly for s32-ar" >&2
    cat "$WORKDIR/ar-compile.log" >&2
    exit 1
fi
echo "  Compiled OK ($(wc -c < "$AR_ASM") bytes asm)"

run_exe "$AS_EXE" "$WORKDIR/ar-assemble.log" "$AR_ASM" "$AR_OBJ"
if [[ ! -s "$AR_OBJ" ]]; then
    echo "assembler produced no output for s32-ar" >&2
    cat "$WORKDIR/ar-assemble.log" >&2
    exit 1
fi

run_exe "$LD_EXE" "$WORKDIR/ar-link.log" \
    -o "$AR_EXE" --mmio 64K \
    "$RUNTIME_CRT0" "$AR_OBJ" "$LIBC_START_OBJ" "$RUNTIME_MMIO_NO_START_OBJ" \
    "$LIBC_ARCHIVE"
if [[ ! -s "$AR_EXE" ]]; then
    echo "linker produced no output for s32-ar" >&2
    cat "$WORKDIR/ar-link.log" >&2
    exit 1
fi

EXEC_TIMEOUT="$SAVED_TIMEOUT"
echo "  New archiver: $AR_EXE ($(wc -c < "$AR_EXE") bytes)"
PASS=$((PASS + 1))

# ============================================================
# Step 3: Functional test — create archive, list contents
# ============================================================
echo ""
echo "=== Step 3: Functional test ==="

# Gather libc .s32o files to archive
LIBC_DIR="$(dirname "$LIBC_ARCHIVE")"
LIBC_OBJS=()
for f in "$LIBC_DIR"/*.s32o; do
    bn="$(basename "$f")"
    case "$bn" in
        crt0_minimal.s32o|libc_start.s32o|mmio_minimal.s32o) continue ;;
    esac
    LIBC_OBJS+=("$f")
done

TOTAL=$((TOTAL + 1))
if [[ "${#LIBC_OBJS[@]}" -eq 0 ]]; then
    echo "  No libc .s32o files found" >&2
    FAIL=$((FAIL + 1))
else
    TEST_ARCHIVE="$WORKDIR/test-func.s32a"
    run_exe "$AR_EXE" "$WORKDIR/ar-create.log" cs "$TEST_ARCHIVE" "${LIBC_OBJS[@]}"

    if [[ ! -s "$TEST_ARCHIVE" ]]; then
        echo "  Archive create: FAIL" >&2
        FAIL=$((FAIL + 1))
    else
        run_exe "$AR_EXE" "$WORKDIR/ar-list.log" t "$TEST_ARCHIVE"
        LIST_LINES="$(wc -l < "$WORKDIR/ar-list.log")"
        EXPECTED="${#LIBC_OBJS[@]}"

        if [[ "$LIST_LINES" -eq "$EXPECTED" ]]; then
            printf "  %-24s PASS (%d members)\n" "create+list:" "$EXPECTED"
            PASS=$((PASS + 1))
        else
            printf "  %-24s FAIL (expected %d, got %d)\n" "create+list:" "$EXPECTED" "$LIST_LINES"
            FAIL=$((FAIL + 1))
        fi
    fi
fi

# ============================================================
# Step 4: Link test — archive works with linker
# ============================================================
echo ""
echo "=== Step 4: Link test ==="

SMOKE_SRC="$SELFHOST_DIR/stage14/tests/test_parse_smoke.c"
SMOKE_ASM="$WORKDIR/smoke.s"
SMOKE_OBJ="$WORKDIR/smoke.s32o"
SMOKE_EXE="$WORKDIR/smoke.s32x"

TOTAL=$((TOTAL + 1))

# Compile test with s32-cc
run_exe "$S32CC_EXE" "$WORKDIR/smoke-compile.log" "$SMOKE_SRC" "$SMOKE_ASM"
if [[ ! -s "$SMOKE_ASM" ]]; then
    echo "  Link test: FAIL (compile)" >&2
    FAIL=$((FAIL + 1))
else
    # Assemble with new assembler
    run_exe "$AS_EXE" "$WORKDIR/smoke-assemble.log" "$SMOKE_ASM" "$SMOKE_OBJ"
    if [[ ! -s "$SMOKE_OBJ" ]]; then
        echo "  Link test: FAIL (assemble)" >&2
        FAIL=$((FAIL + 1))
    else
        # Create libc archive with new archiver
        NEW_LIBC="$WORKDIR/libc_new.s32a"
        run_exe "$AR_EXE" "$WORKDIR/ar-libc.log" cs "$NEW_LIBC" "${LIBC_OBJS[@]}"

        if [[ ! -s "$NEW_LIBC" ]]; then
            echo "  Link test: FAIL (archive)" >&2
            FAIL=$((FAIL + 1))
        else
            # Link against new archive
            run_exe "$LD_EXE" "$WORKDIR/smoke-link.log" \
                -o "$SMOKE_EXE" --mmio 64K \
                "$RUNTIME_CRT0" "$SMOKE_OBJ" "$LIBC_START_OBJ" "$RUNTIME_MMIO_NO_START_OBJ" \
                "$NEW_LIBC"

            if [[ ! -s "$SMOKE_EXE" ]]; then
                echo "  Link test: FAIL (link)" >&2
                FAIL=$((FAIL + 1))
            else
                set +e
                run_exe_rc "$SMOKE_EXE" "$WORKDIR/smoke-run.log"
                SMOKE_RC=$?
                set -e

                if [[ "$SMOKE_RC" -eq 0 ]]; then
                    printf "  %-24s PASS (rc=0)\n" "link+run:"
                    PASS=$((PASS + 1))
                else
                    printf "  %-24s FAIL (rc=%d)\n" "link+run:" "$SMOKE_RC"
                    FAIL=$((FAIL + 1))
                fi
            fi
        fi
    fi
fi

# ============================================================
# Final report
# ============================================================
echo ""
if [[ "$FAIL" -eq 0 ]]; then
    echo "OK: stage16 ($PASS/$TOTAL tests passed)"
else
    echo "FAIL: stage16 ($PASS/$TOTAL tests passed, $FAIL failed)" >&2
    exit 1
fi

echo "Emulator: $EMU"
echo "Compiler exe: $S32CC_EXE"
echo "Assembler exe: $AS_EXE"
echo "Linker exe: $LD_EXE"
echo "Archiver exe: $AR_EXE"
echo "Runtime crt0: $RUNTIME_CRT0"
echo "Runtime mmio: $RUNTIME_MMIO_NO_START_OBJ"
echo "Libc archive: $LIBC_ARCHIVE"
echo "Libc start: $LIBC_START_OBJ"
echo "Artifacts: $WORKDIR"
