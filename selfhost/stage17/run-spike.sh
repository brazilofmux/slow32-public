#!/usr/bin/env bash
set -euo pipefail

# Stage 17: Recompile linker (s32-ld) with s32-cc
#
# 1) Bootstrap via stage16 (obtains s32-cc, assembler, archiver, old linker, runtime)
# 2) Compile s32-ld.c with s32-cc + new assembler + old linker
# 3) Parity test: new linker produces byte-identical .s32x vs old linker
# 4) Functional test: new linker links test program, run → rc=0

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
SELFHOST_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"

EMU="${STAGE17_EMU:-}"
EMU_EXPLICIT=0
STAGE16_DIR="$SELFHOST_DIR/stage16"
KEEP_ARTIFACTS=0

choose_default_emu() {
    printf '%s\n' "$SELFHOST_DIR/stage00/s32-emu"
}

usage() {
    cat <<USAGE
Usage: $0 [--emu <path>] [--keep-artifacts]

Stage17 s32-cc-compiled linker:
  1) bootstrap via stage16 (s32-cc, assembler, archiver, linker, runtime)
  2) compile s32-ld.c with s32-cc (bumped limits: 1MB text/data)
  3) parity test: byte-identical .s32x output vs old linker
  4) functional test: new linker links test program, run
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

if [[ "$EMU_EXPLICIT" -eq 0 && -z "${STAGE17_EMU:-}" ]]; then
    EMU="$(choose_default_emu)"
fi

[[ -f "$EMU" ]] || { echo "Missing emulator: $EMU" >&2; exit 1; }

WORKDIR="$(mktemp -d /tmp/selfhost-v2-stage17.XXXXXX)"
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
# Step 1: Bootstrap via stage16
# ============================================================
echo "=== Step 1: Bootstrap via stage16 ==="

S16_LOG="$WORKDIR/stage16-build.log"
EMU_FLAG=()
if [[ "$EMU_EXPLICIT" -eq 1 ]]; then
    EMU_FLAG=(--emu "$EMU")
fi
"$STAGE16_DIR/run-spike.sh" "${EMU_FLAG[@]}" --keep-artifacts >"$S16_LOG"

# Extract artifacts from stage16 output
S16_ART="$(awk -F': ' '/^Artifacts:/{print $2}' "$S16_LOG" | tail -n 1)"
[[ -n "$S16_ART" && -d "$S16_ART" ]] || { echo "failed to locate stage16 artifacts" >&2; exit 1; }

S32CC_EXE="$(awk -F': ' '/^Compiler exe:/{print $2}' "$S16_LOG" | tail -n 1)"
AS_EXE="$(awk -F': ' '/^Assembler exe:/{print $2}' "$S16_LOG" | tail -n 1)"
OLD_LD_EXE="$(awk -F': ' '/^Linker exe:/{print $2}' "$S16_LOG" | tail -n 1)"
AR_EXE="$(awk -F': ' '/^Archiver exe:/{print $2}' "$S16_LOG" | tail -n 1)"
RUNTIME_CRT0="$(awk -F': ' '/^Runtime crt0:/{print $2}' "$S16_LOG" | tail -n 1)"
RUNTIME_MMIO_NO_START_OBJ="$(awk -F': ' '/^Runtime mmio:/{print $2}' "$S16_LOG" | tail -n 1)"
LIBC_ARCHIVE="$(awk -F': ' '/^Libc archive:/{print $2}' "$S16_LOG" | tail -n 1)"
LIBC_START_OBJ="$(awk -F': ' '/^Libc start:/{print $2}' "$S16_LOG" | tail -n 1)"

for f in "$S32CC_EXE" "$AS_EXE" "$OLD_LD_EXE" "$AR_EXE" "$RUNTIME_CRT0" "$RUNTIME_MMIO_NO_START_OBJ" "$LIBC_ARCHIVE" "$LIBC_START_OBJ"; do
    [[ -s "$f" ]] || { echo "Missing artifact: $f" >&2; exit 1; }
done

echo "Compiler: $S32CC_EXE"
echo "Assembler: $AS_EXE"
echo "Old linker: $OLD_LD_EXE"
echo "Archiver: $AR_EXE"

# ============================================================
# Step 2: Compile s32-ld.c with s32-cc
# ============================================================
echo ""
echo "=== Step 2: Compile s32-ld.c ==="

# Linker has inlined header, no #include needed — just copy source
cp "$SCRIPT_DIR/s32-ld.c" "$WORKDIR/s32-ld.c"

LD_ASM="$WORKDIR/s32-ld.s"
LD_OBJ="$WORKDIR/s32-ld.s32o"
NEW_LD_EXE="$WORKDIR/s32-ld.s32x"

SAVED_TIMEOUT="${EXEC_TIMEOUT:-180}"
EXEC_TIMEOUT=300

TOTAL=$((TOTAL + 1))

run_exe "$S32CC_EXE" "$WORKDIR/ld-compile.log" "$WORKDIR/s32-ld.c" "$LD_ASM"
if [[ ! -s "$LD_ASM" ]]; then
    echo "s32-cc produced no assembly for s32-ld" >&2
    cat "$WORKDIR/ld-compile.log" >&2
    exit 1
fi
echo "  Compiled OK ($(wc -c < "$LD_ASM") bytes asm)"

run_exe "$AS_EXE" "$WORKDIR/ld-assemble.log" "$LD_ASM" "$LD_OBJ"
if [[ ! -s "$LD_OBJ" ]]; then
    echo "assembler produced no output for s32-ld" >&2
    cat "$WORKDIR/ld-assemble.log" >&2
    exit 1
fi

# Link the new linker using the OLD linker
run_exe "$OLD_LD_EXE" "$WORKDIR/ld-link.log" \
    -o "$NEW_LD_EXE" --mmio 64K \
    "$RUNTIME_CRT0" "$LD_OBJ" "$LIBC_START_OBJ" "$RUNTIME_MMIO_NO_START_OBJ" \
    "$LIBC_ARCHIVE"
if [[ ! -s "$NEW_LD_EXE" ]]; then
    echo "old linker produced no output for new linker" >&2
    cat "$WORKDIR/ld-link.log" >&2
    exit 1
fi

EXEC_TIMEOUT="$SAVED_TIMEOUT"
echo "  New linker: $NEW_LD_EXE ($(wc -c < "$NEW_LD_EXE") bytes)"
PASS=$((PASS + 1))

# ============================================================
# Step 3: Parity test — new linker vs old linker
# ============================================================
echo ""
echo "=== Step 3: Parity test ==="

# Compile a test program, assemble it, then link with both linkers
PARITY_SRC="$SELFHOST_DIR/stage14/tests/test_parse_smoke.c"
PARITY_ASM="$WORKDIR/parity.s"
PARITY_OBJ="$WORKDIR/parity.s32o"
PARITY_OLD_EXE="$WORKDIR/parity-old.s32x"
PARITY_NEW_EXE="$WORKDIR/parity-new.s32x"

SAVED_TIMEOUT="${EXEC_TIMEOUT:-180}"
EXEC_TIMEOUT=300

run_exe "$S32CC_EXE" "$WORKDIR/parity-compile.log" "$PARITY_SRC" "$PARITY_ASM"
run_exe "$AS_EXE" "$WORKDIR/parity-assemble.log" "$PARITY_ASM" "$PARITY_OBJ"

# Link with old linker
run_exe "$OLD_LD_EXE" "$WORKDIR/parity-old-link.log" \
    -o "$PARITY_OLD_EXE" --mmio 64K \
    "$RUNTIME_CRT0" "$PARITY_OBJ" "$LIBC_START_OBJ" "$RUNTIME_MMIO_NO_START_OBJ" \
    "$LIBC_ARCHIVE"

# Link with new linker
run_exe "$NEW_LD_EXE" "$WORKDIR/parity-new-link.log" \
    -o "$PARITY_NEW_EXE" --mmio 64K \
    "$RUNTIME_CRT0" "$PARITY_OBJ" "$LIBC_START_OBJ" "$RUNTIME_MMIO_NO_START_OBJ" \
    "$LIBC_ARCHIVE"

EXEC_TIMEOUT="$SAVED_TIMEOUT"

TOTAL=$((TOTAL + 1))
if diff "$PARITY_OLD_EXE" "$PARITY_NEW_EXE" >/dev/null 2>&1; then
    echo "  PARITY PROVEN: old.s32x == new.s32x ($(wc -c < "$PARITY_OLD_EXE") bytes)"
    PASS=$((PASS + 1))
else
    echo "  PARITY FAILED: old and new linker outputs differ" >&2
    echo "  old: $(wc -c < "$PARITY_OLD_EXE") bytes, new: $(wc -c < "$PARITY_NEW_EXE") bytes" >&2
    FAIL=$((FAIL + 1))
fi

# ============================================================
# Step 4: Functional test — new linker in full pipeline
# ============================================================
echo ""
echo "=== Step 4: Functional test ==="

FUNC_EXE="$WORKDIR/func-test.s32x"

TOTAL=$((TOTAL + 1))

# Link test program with new linker
SAVED_TIMEOUT="${EXEC_TIMEOUT:-180}"
EXEC_TIMEOUT=300

run_exe "$NEW_LD_EXE" "$WORKDIR/func-link.log" \
    -o "$FUNC_EXE" --mmio 64K \
    "$RUNTIME_CRT0" "$PARITY_OBJ" "$LIBC_START_OBJ" "$RUNTIME_MMIO_NO_START_OBJ" \
    "$LIBC_ARCHIVE"

EXEC_TIMEOUT="$SAVED_TIMEOUT"

if [[ ! -s "$FUNC_EXE" ]]; then
    echo "  Functional test: FAIL (link)" >&2
    FAIL=$((FAIL + 1))
else
    set +e
    run_exe_rc "$FUNC_EXE" "$WORKDIR/func-run.log"
    FUNC_RC=$?
    set -e

    if [[ "$FUNC_RC" -eq 0 ]]; then
        printf "  %-24s PASS (rc=0)\n" "link+run:"
        PASS=$((PASS + 1))
    else
        printf "  %-24s FAIL (rc=%d)\n" "link+run:" "$FUNC_RC"
        FAIL=$((FAIL + 1))
    fi
fi

# ============================================================
# Final report
# ============================================================
echo ""
if [[ "$FAIL" -eq 0 ]]; then
    echo "OK: stage17 ($PASS/$TOTAL tests passed)"
else
    echo "FAIL: stage17 ($PASS/$TOTAL tests passed, $FAIL failed)" >&2
    exit 1
fi

echo "Emulator: $EMU"
echo "Compiler exe: $S32CC_EXE"
echo "Assembler exe: $AS_EXE"
echo "Archiver exe: $AR_EXE"
echo "Linker exe: $NEW_LD_EXE"
echo "Runtime crt0: $RUNTIME_CRT0"
echo "Runtime mmio: $RUNTIME_MMIO_NO_START_OBJ"
echo "Libc archive: $LIBC_ARCHIVE"
echo "Libc start: $LIBC_START_OBJ"
echo "Artifacts: $WORKDIR"
