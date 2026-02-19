#!/usr/bin/env bash
set -euo pipefail

# Stage 18: Ragel -G2 C lexer for new compiler
#
# 1) Bootstrap via stage17 (obtains s32-cc, assembler, archiver, linker, runtime)
# 2) Build boot s32-cc with P_MXLBL=512 (Ragel generates ~128+ labels)
# 3) Generate Ragel lexer (if ragel available) or use pre-generated c_lexer_gen.c
# 4) Compile lexer tests with boot s32-cc, run all tests

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
SELFHOST_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"
TESTS_DIR="$SCRIPT_DIR/tests"

EMU="${STAGE18_EMU:-}"
EMU_EXPLICIT=0
STAGE17_DIR="$SELFHOST_DIR/stage17"
KEEP_ARTIFACTS=0

choose_default_emu() {
    printf '%s\n' "$SELFHOST_DIR/stage00/s32-emu"
}

usage() {
    cat <<USAGE
Usage: $0 [--emu <path>] [--keep-artifacts]

Stage18 Ragel -G2 C lexer:
  1) bootstrap via stage17 (s32-cc, assembler, archiver, linker, runtime)
  2) build boot s32-cc with P_MXLBL=512 for Ragel goto labels
  3) generate/verify Ragel lexer
  4) compile and run lexer tests
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

if [[ "$EMU_EXPLICIT" -eq 0 && -z "${STAGE18_EMU:-}" ]]; then
    EMU="$(choose_default_emu)"
fi

[[ -f "$EMU" ]] || { echo "Missing emulator: $EMU" >&2; exit 1; }

WORKDIR="$(mktemp -d /tmp/selfhost-v2-stage18.XXXXXX)"
if [[ "$KEEP_ARTIFACTS" -eq 0 ]]; then
    trap 'rm -rf "$WORKDIR"' EXIT
fi

# --- Utility functions ---

compile_and_link() {
    local name="$1"
    local src="$2"
    local asm="$WORKDIR/${name}.s"
    local obj="$WORKDIR/${name}.s32o"
    local exe="$WORKDIR/${name}.s32x"

    run_exe "$BOOT_EXE" "$WORKDIR/${name}-compile.log" "$src" "$asm"
    if [[ ! -s "$asm" ]]; then
        echo "  $name: FAIL (compile)" >&2
        tail -n 20 "$WORKDIR/${name}-compile.log" >&2
        return 1
    fi
    echo "    Compiled: $(wc -c < "$asm") bytes asm" >&2

    run_exe "$AS_EXE" "$WORKDIR/${name}-assemble.log" "$asm" "$obj"
    if [[ ! -s "$obj" ]]; then
        echo "  $name: FAIL (assemble)" >&2
        tail -n 20 "$WORKDIR/${name}-assemble.log" >&2
        return 1
    fi

    run_exe "$LD_EXE" "$WORKDIR/${name}-link.log" \
        -o "$exe" --mmio 64K \
        "$RUNTIME_CRT0" "$obj" "$LIBC_START_OBJ" "$RUNTIME_MMIO_NO_START_OBJ" \
        "$LIBC_ARCHIVE"
    if [[ ! -s "$exe" ]]; then
        echo "  $name: FAIL (link)" >&2
        tail -n 20 "$WORKDIR/${name}-link.log" >&2
        return 1
    fi
    printf '%s' "$exe"
}

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
# Step 1: Bootstrap via stage17
# ============================================================
echo "=== Step 1: Bootstrap via stage17 ==="

S17_LOG="$WORKDIR/stage17-build.log"
EMU_FLAG=()
if [[ "$EMU_EXPLICIT" -eq 1 ]]; then
    EMU_FLAG=(--emu "$EMU")
fi
"$STAGE17_DIR/run-spike.sh" "${EMU_FLAG[@]}" --keep-artifacts >"$S17_LOG"

# Extract artifacts
S17_ART="$(awk -F': ' '/^Artifacts:/{print $2}' "$S17_LOG" | tail -n 1)"
[[ -n "$S17_ART" && -d "$S17_ART" ]] || { echo "failed to locate stage17 artifacts" >&2; exit 1; }

S32CC_EXE="$(awk -F': ' '/^Compiler exe:/{print $2}' "$S17_LOG" | tail -n 1)"
AS_EXE="$(awk -F': ' '/^Assembler exe:/{print $2}' "$S17_LOG" | tail -n 1)"
LD_EXE="$(awk -F': ' '/^Linker exe:/{print $2}' "$S17_LOG" | tail -n 1)"
AR_EXE="$(awk -F': ' '/^Archiver exe:/{print $2}' "$S17_LOG" | tail -n 1)"
RUNTIME_CRT0="$(awk -F': ' '/^Runtime crt0:/{print $2}' "$S17_LOG" | tail -n 1)"
RUNTIME_MMIO_NO_START_OBJ="$(awk -F': ' '/^Runtime mmio:/{print $2}' "$S17_LOG" | tail -n 1)"
LIBC_ARCHIVE="$(awk -F': ' '/^Libc archive:/{print $2}' "$S17_LOG" | tail -n 1)"
LIBC_START_OBJ="$(awk -F': ' '/^Libc start:/{print $2}' "$S17_LOG" | tail -n 1)"

for f in "$S32CC_EXE" "$AS_EXE" "$LD_EXE" "$RUNTIME_CRT0" "$RUNTIME_MMIO_NO_START_OBJ" "$LIBC_ARCHIVE" "$LIBC_START_OBJ"; do
    [[ -s "$f" ]] || { echo "Missing artifact: $f" >&2; exit 1; }
done

echo "Compiler: $S32CC_EXE"
echo "Assembler: $AS_EXE"
echo "Linker: $LD_EXE"

# ============================================================
# Step 2: Build boot s32-cc with P_MXLBL=512
# ============================================================
echo ""
echo "=== Step 2: Build boot s32-cc (P_MXLBL=512) ==="

# Create merged source from stage15 with bumped label limit
BOOT_MERGED="$WORKDIR/boot_merged.c"
S15_LEX="$SELFHOST_DIR/stage15/s32cc_lex.h"
S15_PARSE="$SELFHOST_DIR/stage15/s32cc_parse.h"
S15_MAIN="$SELFHOST_DIR/stage15/s32cc.c"

cat "$S15_LEX" "$S15_PARSE" > "$BOOT_MERGED"
tail -n +2 "$S15_MAIN" >> "$BOOT_MERGED"

# Bump P_MXLBL from 64 to 512, and scale name buffer
sed -i 's/#define P_MXLBL 64/#define P_MXLBL 512/' "$BOOT_MERGED"
sed -i 's/#define P_LBNBUF 3072/#define P_LBNBUF 24576/' "$BOOT_MERGED"

BOOT_SZ="$(wc -c < "$BOOT_MERGED")"
echo "  Boot merged: $BOOT_SZ bytes"

SAVED_TIMEOUT="${EXEC_TIMEOUT:-180}"
EXEC_TIMEOUT=300

BOOT_ASM="$WORKDIR/boot.s"
BOOT_OBJ="$WORKDIR/boot.s32o"
BOOT_EXE="$WORKDIR/boot-s32cc.s32x"

run_exe "$S32CC_EXE" "$WORKDIR/boot-compile.log" "$BOOT_MERGED" "$BOOT_ASM"
if [[ ! -s "$BOOT_ASM" ]]; then
    echo "s32-cc produced no assembly for boot" >&2
    cat "$WORKDIR/boot-compile.log" >&2
    exit 1
fi
echo "  Compiled OK ($(wc -c < "$BOOT_ASM") bytes asm)"

run_exe "$AS_EXE" "$WORKDIR/boot-assemble.log" "$BOOT_ASM" "$BOOT_OBJ"
run_exe "$LD_EXE" "$WORKDIR/boot-link.log" \
    -o "$BOOT_EXE" --mmio 64K \
    "$RUNTIME_CRT0" "$BOOT_OBJ" "$LIBC_START_OBJ" "$RUNTIME_MMIO_NO_START_OBJ" \
    "$LIBC_ARCHIVE"
if [[ ! -s "$BOOT_EXE" ]]; then
    echo "link failed for boot s32-cc" >&2
    cat "$WORKDIR/boot-link.log" >&2
    exit 1
fi
echo "  Boot s32-cc: $BOOT_EXE ($(wc -c < "$BOOT_EXE") bytes)"

EXEC_TIMEOUT="$SAVED_TIMEOUT"
TOTAL=$((TOTAL + 1))
PASS=$((PASS + 1))

# ============================================================
# Step 3: Generate Ragel lexer (or use pre-generated)
# ============================================================
echo ""
echo "=== Step 3: Ragel lexer ==="

LEXER_GEN="$SCRIPT_DIR/c_lexer_gen.c"

if command -v ragel >/dev/null 2>&1; then
    echo "  Regenerating with ragel -G2 + post-processing..."
    bash "$SCRIPT_DIR/gen_lexer.sh"
elif [[ -s "$LEXER_GEN" ]]; then
    echo "  Using pre-generated c_lexer_gen.c ($(wc -c < "$LEXER_GEN") bytes)"
else
    echo "ERROR: ragel not found and no pre-generated c_lexer_gen.c" >&2
    exit 1
fi

[[ -s "$LEXER_GEN" ]] || { echo "c_lexer_gen.c is empty" >&2; exit 1; }
echo "  Lexer: $(wc -l < "$LEXER_GEN") lines, $(wc -c < "$LEXER_GEN") bytes"

# Verify Duff's device was defused (no bare "case N:" in outer switch)
BARE_CASES=$(grep -c '^case [0-9]*:$' "$LEXER_GEN" || true)
if [[ "$BARE_CASES" -gt 0 ]]; then
    echo "WARNING: $BARE_CASES bare case labels remain — Duff's device not defused!" >&2
    echo "  Re-run gen_lexer.sh to apply the post-processing." >&2
    exit 1
fi
echo "  Duff's device defused: 0 bare case labels"

# ============================================================
# Step 4: Compile and run lexer tests
# ============================================================
echo ""
echo "=== Step 4: Lexer tests ==="

SAVED_TIMEOUT="${EXEC_TIMEOUT:-180}"
EXEC_TIMEOUT=300

# --- 4a: Stub test (lexer globals + helpers, stub lex_next → TK_EOF) ---
TOTAL=$((TOTAL + 1))
echo "  4a: Stub test (globals + helpers only)..."
STUB_EXE=""
STUB_EXE=$(compile_and_link "stub" "$TESTS_DIR/lex_stub.c") || true
if [[ -n "$STUB_EXE" && -s "$STUB_EXE" ]]; then
    set +e
    run_exe_rc "$STUB_EXE" "$WORKDIR/stub-run.log"
    STUB_RC=$?
    set -e
    if [[ "$STUB_RC" -eq 0 ]]; then
        printf "  %-24s PASS\n" "lex-stub:"
        PASS=$((PASS + 1))
    else
        printf "  %-24s FAIL (rc=%d)\n" "lex-stub:" "$STUB_RC"
        cat "$WORKDIR/stub-run.log" >&2
        FAIL=$((FAIL + 1))
    fi
else
    printf "  %-24s FAIL (build)\n" "lex-stub:"
    FAIL=$((FAIL + 1))
fi

# --- 4b: Smoke compile (full lexer + minimal main, no test logic) ---
TOTAL=$((TOTAL + 1))
SMOKE_SRC="$WORKDIR/lex_smoke.c"
cp "$LEXER_GEN" "$SMOKE_SRC"
printf '\nint main(void) {\n    char *src;\n    src = "int x;";\n    lex_init(src, strlen(src));\n    lex_next();\n    return 0;\n}\n' >> "$SMOKE_SRC"

echo "  4b: Smoke compile ($(wc -c < "$SMOKE_SRC") bytes)..."
SMOKE_EXE=""
SMOKE_EXE=$(compile_and_link "smoke" "$SMOKE_SRC") || true
if [[ -n "$SMOKE_EXE" && -s "$SMOKE_EXE" ]]; then
    set +e
    run_exe_rc "$SMOKE_EXE" "$WORKDIR/smoke-run.log"
    SMOKE_RC=$?
    set -e
    if [[ "$SMOKE_RC" -eq 0 ]]; then
        printf "  %-24s PASS\n" "lex-smoke:"
        PASS=$((PASS + 1))
    else
        printf "  %-24s FAIL (rc=%d)\n" "lex-smoke:" "$SMOKE_RC"
        cat "$WORKDIR/smoke-run.log" >&2
        FAIL=$((FAIL + 1))
    fi
else
    printf "  %-24s FAIL (build)\n" "lex-smoke:"
    FAIL=$((FAIL + 1))
fi

# --- 4c: Full lexer test (merge lexer + test harness) ---
TOTAL=$((TOTAL + 1))
MERGED_TEST="$WORKDIR/lex_test_merged.c"
cp "$LEXER_GEN" "$MERGED_TEST"
grep -v '^#include "c_lexer_gen.c"' "$TESTS_DIR/lex_test.c" >> "$MERGED_TEST"

echo "  4c: Full test ($(wc -c < "$MERGED_TEST") bytes)..."
TEST_EXE=""
TEST_EXE=$(compile_and_link "lex_test" "$MERGED_TEST") || true
if [[ -n "$TEST_EXE" && -s "$TEST_EXE" ]]; then
    EXEC_TIMEOUT="$SAVED_TIMEOUT"
    set +e
    run_exe_rc "$TEST_EXE" "$WORKDIR/lex-test-run.log"
    TEST_RC=$?
    set -e
    if [[ "$TEST_RC" -eq 0 ]]; then
        printf "  %-24s PASS\n" "lex-test:"
        PASS=$((PASS + 1))
        cat "$WORKDIR/lex-test-run.log"
    else
        printf "  %-24s FAIL (rc=%d)\n" "lex-test:" "$TEST_RC"
        cat "$WORKDIR/lex-test-run.log" >&2
        FAIL=$((FAIL + 1))
    fi
else
    printf "  %-24s FAIL (build)\n" "lex-test:"
    FAIL=$((FAIL + 1))
fi

EXEC_TIMEOUT="$SAVED_TIMEOUT"

# ============================================================
# Final report
# ============================================================
echo ""
if [[ "$FAIL" -eq 0 ]]; then
    echo "OK: stage18 ($PASS/$TOTAL tests passed)"
else
    echo "FAIL: stage18 ($PASS/$TOTAL tests passed, $FAIL failed)" >&2
    exit 1
fi

echo "Emulator: $EMU"
echo "Compiler exe: $BOOT_EXE"
echo "Assembler exe: $AS_EXE"
echo "Linker exe: $LD_EXE"
echo "Archiver exe: $AR_EXE"
echo "Runtime crt0: $RUNTIME_CRT0"
echo "Runtime mmio: $RUNTIME_MMIO_NO_START_OBJ"
echo "Libc archive: $LIBC_ARCHIVE"
echo "Libc start: $LIBC_START_OBJ"
echo "Artifacts: $WORKDIR"
