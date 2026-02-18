#!/usr/bin/env bash
set -euo pipefail

# Stage 14: s32-cc C compiler (Pass 1 lexer + Pass 2 parser)
#
# Bootstrap chain:
# 1) Bootstrap via stage13 (obtains Gen2 cc-min, assembler, C linker, runtime)
# 2) Lexer tests: compile each test_lex_*.c with cc-min, run
# 3) Build s32-cc: concatenate lex+parse headers, compile s32cc.c with cc-min
# 4) Parser tests: compile each test_parse_*.c with s32-cc, assemble, link, run

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
SELFHOST_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"
ROOT_DIR="$(cd "$SELFHOST_DIR/.." && pwd)"
TESTS_DIR="$SCRIPT_DIR/tests"

EMU="${STAGE14_EMU:-}"
EMU_EXPLICIT=0

STAGE13_DIR="$SELFHOST_DIR/stage13"

KEEP_ARTIFACTS=0

choose_default_emu() {
    printf '%s\n' "$SELFHOST_DIR/stage00/s32-emu"
}

usage() {
    cat <<USAGE
Usage: $0 [--emu <path>] [--keep-artifacts]

Stage14 s32-cc compiler tests:
  1) bootstrap Gen2 cc-min + assembler + C linker via stage13
  2) compile each test_lex_*.c with cc-min, run
  3) build s32-cc compiler from s32cc.c with cc-min
  4) compile each test_parse_*.c with s32-cc, assemble, link, run
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

if [[ "$EMU_EXPLICIT" -eq 0 && -z "${STAGE14_EMU:-}" ]]; then
    EMU="$(choose_default_emu)"
fi

[[ -f "$EMU" ]] || { echo "Missing emulator: $EMU" >&2; exit 1; }

WORKDIR="$(mktemp -d /tmp/selfhost-v2-stage14.XXXXXX)"
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

compile_and_link() {
    local src="$1"
    local name="$2"
    local exe="$3"

    local asm_out="$WORKDIR/${name}.s"
    local obj_out="$WORKDIR/${name}.s32o"

    # Copy source and header into workdir
    cp "$src" "$WORKDIR/${name}.c"
    cp "$SCRIPT_DIR/s32cc_lex.h" "$WORKDIR/s32cc_lex.h"

    # Compile with cc-min
    run_exe "$GEN2_EXE" "$WORKDIR/${name}-compile.log" "$WORKDIR/${name}.c" "$asm_out"
    if [[ ! -s "$asm_out" ]]; then
        echo "cc-min produced no assembly for $name" >&2
        cat "$WORKDIR/${name}-compile.log" >&2
        return 1
    fi

    # Assemble
    run_exe "$AS_EXE" "$WORKDIR/${name}-assemble.log" "$asm_out" "$obj_out"
    if [[ ! -s "$obj_out" ]]; then
        echo "assembler produced no output for $name" >&2
        cat "$WORKDIR/${name}-assemble.log" >&2
        return 1
    fi

    # Link
    run_exe "$LD_EXE" "$WORKDIR/${name}-link.log" \
        -o "$exe" --mmio 64K \
        "$RUNTIME_CRT0" "$obj_out" "$LIBC_START_OBJ" "$RUNTIME_MMIO_NO_START_OBJ" \
        "$LIBC_ARCHIVE"
    if [[ ! -s "$exe" ]]; then
        echo "linker produced no output for $name" >&2
        cat "$WORKDIR/${name}-link.log" >&2
        return 1
    fi
}

# ============================================================
# Step 1: Bootstrap via stage13
# ============================================================
echo "=== Step 1: Bootstrap via stage13 ==="

S13_LOG="$WORKDIR/stage13-build.log"
EMU_FLAG=()
if [[ "$EMU_EXPLICIT" -eq 1 ]]; then
    EMU_FLAG=(--emu "$EMU")
fi
"$STAGE13_DIR/run-spike.sh" "${EMU_FLAG[@]}" --keep-artifacts >"$S13_LOG"

# Extract paths from stage13 output
S13_ART="$(awk -F': ' '/^Artifacts:/{print $2}' "$S13_LOG" | tail -n 1)"
[[ -n "$S13_ART" && -d "$S13_ART" ]] || { echo "failed to locate stage13 artifacts" >&2; exit 1; }

# Find Gen2 cc-min via stage12→stage11→stage09
S12_LOG="$S13_ART/stage12-build.log"
[[ -f "$S12_LOG" ]] || { echo "missing stage12 build log" >&2; exit 1; }
S12_ART="$(awk -F': ' '/^Artifacts:/{print $2}' "$S12_LOG" | tail -n 1)"
[[ -n "$S12_ART" && -d "$S12_ART" ]] || { echo "failed to locate stage12 artifacts" >&2; exit 1; }

S11_LOG="$S12_ART/stage11-build.log"
[[ -f "$S11_LOG" ]] || { echo "missing stage11 build log" >&2; exit 1; }
S11_ART="$(awk -F': ' '/^Artifacts:/{print $2}' "$S11_LOG" | tail -n 1)"
[[ -n "$S11_ART" && -d "$S11_ART" ]] || { echo "failed to locate stage11 artifacts" >&2; exit 1; }

S9_LOG="$S11_ART/stage09-build.log"
[[ -f "$S9_LOG" ]] || { echo "missing stage09 build log" >&2; exit 1; }
GEN2_EXE="$(awk -F': ' '/^Gen2 exe:/{print $2}' "$S9_LOG" | tail -n 1)"
S9_ART="$(awk -F': ' '/^Artifacts:/{print $2}' "$S9_LOG" | tail -n 1)"
[[ -n "$GEN2_EXE" && -f "$GEN2_EXE" ]] || { echo "failed to locate Gen2 cc-min exe" >&2; exit 1; }

# Find assembler from stage08
S8_LOG="$S9_ART/stage08-build.log"
[[ -f "$S8_LOG" ]] || { echo "missing stage08 build log" >&2; exit 1; }
AS_EXE="$(awk -F': ' '/^Assembler exe:/{print $2}' "$S8_LOG" | tail -n 1)"
S8_ART="$(awk -F': ' '/^Artifacts:/{print $2}' "$S8_LOG" | tail -n 1)"
[[ -n "$AS_EXE" && -f "$AS_EXE" ]] || { echo "failed to locate assembler" >&2; exit 1; }

# Find C linker from stage08
LD_EXE="$(awk -F': ' '/^Linker exe:/{print $2}' "$S8_LOG" | tail -n 1)"
[[ -n "$LD_EXE" && -f "$LD_EXE" ]] || { echo "failed to locate C linker" >&2; exit 1; }

# Runtime objects from stage05
S5_LOG="$S8_ART/stage5-build.log"
[[ -f "$S5_LOG" ]] || { echo "missing stage05 build log" >&2; exit 1; }
S5_ART="$(awk -F': ' '/^Artifacts:/{print $2}' "$S5_LOG" | tail -n 1)"
[[ -n "$S5_ART" && -d "$S5_ART" ]] || { echo "failed to locate stage05 artifacts" >&2; exit 1; }

RUNTIME_CRT0="$S5_ART/crt0_minimal.s32o"
RUNTIME_MMIO_NO_START_OBJ="$S5_ART/mmio_no_start.s32o"
LIBC_ARCHIVE="$S5_ART/libc_selfhost.s32a"
LIBC_START_OBJ="$S5_ART/libc_start.s32o"

[[ -s "$RUNTIME_CRT0" ]] || { echo "missing runtime crt0" >&2; exit 1; }
[[ -s "$RUNTIME_MMIO_NO_START_OBJ" ]] || { echo "missing runtime mmio" >&2; exit 1; }
[[ -s "$LIBC_ARCHIVE" ]] || { echo "missing libc archive" >&2; exit 1; }
[[ -s "$LIBC_START_OBJ" ]] || { echo "missing libc start" >&2; exit 1; }

echo "Gen2 cc-min: $GEN2_EXE"
echo "Assembler: $AS_EXE"
echo "C linker: $LD_EXE"

# ============================================================
# Step 2: Lexer tests
# ============================================================
echo ""
echo "=== Step 2: Lexer tests ==="

TESTS="test_lex_kw test_lex_ident test_lex_num test_lex_str test_lex_char test_lex_ops test_lex_comment test_lex_mixed"
PASS=0
FAIL=0
TOTAL=0

for test_name in $TESTS; do
    TOTAL=$((TOTAL + 1))
    SRC="$TESTS_DIR/${test_name}.c"
    EXE="$WORKDIR/${test_name}.s32x"

    if [[ ! -f "$SRC" ]]; then
        printf "  %-20s SKIP (source not found)\n" "${test_name}:"
        FAIL=$((FAIL + 1))
        continue
    fi

    # Compile, assemble, link
    if ! compile_and_link "$SRC" "$test_name" "$EXE" 2>"$WORKDIR/${test_name}-build-err.log"; then
        printf "  %-20s FAIL (build)\n" "${test_name}:"
        cat "$WORKDIR/${test_name}-build-err.log" >&2
        FAIL=$((FAIL + 1))
        continue
    fi

    # Run test
    set +e
    run_exe_rc "$EXE" "$WORKDIR/${test_name}-run.log"
    TEST_RC=$?
    set -e

    if [[ "$TEST_RC" -eq 0 ]]; then
        printf "  %-20s PASS (rc=0)\n" "${test_name}:"
        PASS=$((PASS + 1))
    else
        printf "  %-20s FAIL (rc=%d)\n" "${test_name}:" "$TEST_RC"
        FAIL=$((FAIL + 1))
    fi
done

# ============================================================
# Step 3: Build s32-cc compiler
# ============================================================
echo ""
echo "=== Step 3: Build s32-cc ==="

# Concatenate lexer + parser headers
cat "$SCRIPT_DIR/s32cc_lex.h" "$SCRIPT_DIR/s32cc_parse.h" > "$WORKDIR/s32cc_combined.h"

# Copy driver
cp "$SCRIPT_DIR/s32cc.c" "$WORKDIR/s32cc.c"

# Compile with cc-min
S32CC_ASM="$WORKDIR/s32cc.s"
S32CC_OBJ="$WORKDIR/s32cc.s32o"
S32CC_EXE="$WORKDIR/s32cc.s32x"

run_exe "$GEN2_EXE" "$WORKDIR/s32cc-compile.log" "$WORKDIR/s32cc.c" "$S32CC_ASM"
if [[ ! -s "$S32CC_ASM" ]]; then
    echo "cc-min produced no assembly for s32cc" >&2
    cat "$WORKDIR/s32cc-compile.log" >&2
    exit 1
fi
echo "  s32cc.c compiled OK ($(wc -c < "$S32CC_ASM") bytes asm)"

# Assemble
run_exe "$AS_EXE" "$WORKDIR/s32cc-assemble.log" "$S32CC_ASM" "$S32CC_OBJ"
if [[ ! -s "$S32CC_OBJ" ]]; then
    echo "assembler produced no output for s32cc" >&2
    cat "$WORKDIR/s32cc-assemble.log" >&2
    exit 1
fi

# Link
run_exe "$LD_EXE" "$WORKDIR/s32cc-link.log" \
    -o "$S32CC_EXE" --mmio 64K \
    "$RUNTIME_CRT0" "$S32CC_OBJ" "$LIBC_START_OBJ" "$RUNTIME_MMIO_NO_START_OBJ" \
    "$LIBC_ARCHIVE"
if [[ ! -s "$S32CC_EXE" ]]; then
    echo "linker produced no output for s32cc" >&2
    cat "$WORKDIR/s32cc-link.log" >&2
    exit 1
fi
echo "  s32cc.s32x linked OK ($(wc -c < "$S32CC_EXE") bytes)"
TOTAL=$((TOTAL + 1))
PASS=$((PASS + 1))

# ============================================================
# Step 4: Parser tests (compile with s32-cc, assemble, link, run)
# ============================================================
echo ""
echo "=== Step 4: Parser tests ==="

PARSE_TESTS="test_parse_smoke test_parse_expr test_parse_ctrl test_parse_ptr test_parse_mixed"

for test_name in $PARSE_TESTS; do
    TOTAL=$((TOTAL + 1))
    SRC="$TESTS_DIR/${test_name}.c"
    PASM="$WORKDIR/${test_name}.s"
    POBJ="$WORKDIR/${test_name}.s32o"
    EXE="$WORKDIR/${test_name}.s32x"

    if [[ ! -f "$SRC" ]]; then
        printf "  %-24s SKIP (source not found)\n" "${test_name}:"
        FAIL=$((FAIL + 1))
        continue
    fi

    # Compile with s32-cc
    if ! run_exe "$S32CC_EXE" "$WORKDIR/${test_name}-s32cc.log" "$SRC" "$PASM" 2>"$WORKDIR/${test_name}-s32cc-err.log"; then
        printf "  %-24s FAIL (s32cc compile)\n" "${test_name}:"
        cat "$WORKDIR/${test_name}-s32cc.log" >&2
        cat "$WORKDIR/${test_name}-s32cc-err.log" >&2
        FAIL=$((FAIL + 1))
        continue
    fi
    if [[ ! -s "$PASM" ]]; then
        printf "  %-24s FAIL (no asm output)\n" "${test_name}:"
        cat "$WORKDIR/${test_name}-s32cc.log" >&2
        FAIL=$((FAIL + 1))
        continue
    fi

    # Assemble
    if ! run_exe "$AS_EXE" "$WORKDIR/${test_name}-assemble.log" "$PASM" "$POBJ" 2>"$WORKDIR/${test_name}-as-err.log"; then
        printf "  %-24s FAIL (assemble)\n" "${test_name}:"
        cat "$WORKDIR/${test_name}-assemble.log" >&2
        cat "$WORKDIR/${test_name}-as-err.log" >&2
        FAIL=$((FAIL + 1))
        continue
    fi
    if [[ ! -s "$POBJ" ]]; then
        printf "  %-24s FAIL (no obj output)\n" "${test_name}:"
        FAIL=$((FAIL + 1))
        continue
    fi

    # Link (needs libc_start for __slow32_start which calls main)
    if ! run_exe "$LD_EXE" "$WORKDIR/${test_name}-link.log" \
        -o "$EXE" --mmio 64K \
        "$RUNTIME_CRT0" "$POBJ" "$LIBC_START_OBJ" "$RUNTIME_MMIO_NO_START_OBJ" \
        "$LIBC_ARCHIVE" 2>"$WORKDIR/${test_name}-ld-err.log"; then
        printf "  %-24s FAIL (link)\n" "${test_name}:"
        cat "$WORKDIR/${test_name}-link.log" >&2
        cat "$WORKDIR/${test_name}-ld-err.log" >&2
        FAIL=$((FAIL + 1))
        continue
    fi
    if [[ ! -s "$EXE" ]]; then
        printf "  %-24s FAIL (no exe output)\n" "${test_name}:"
        FAIL=$((FAIL + 1))
        continue
    fi

    # Run test
    set +e
    run_exe_rc "$EXE" "$WORKDIR/${test_name}-run.log"
    TEST_RC=$?
    set -e

    if [[ "$TEST_RC" -eq 0 ]]; then
        printf "  %-24s PASS (rc=0)\n" "${test_name}:"
        PASS=$((PASS + 1))
    else
        printf "  %-24s FAIL (rc=%d)\n" "${test_name}:" "$TEST_RC"
        # Show assembly for debugging
        echo "  --- Assembly output (first 40 lines) ---" >&2
        head -n 40 "$PASM" >&2
        FAIL=$((FAIL + 1))
    fi
done

# ============================================================
# Final report
# ============================================================
echo ""
if [[ "$FAIL" -eq 0 ]]; then
    echo "OK: stage14 ($PASS/$TOTAL tests passed)"
else
    echo "FAIL: stage14 ($PASS/$TOTAL tests passed, $FAIL failed)" >&2
    exit 1
fi

echo "Emulator: $EMU"
echo "Artifacts: $WORKDIR"
