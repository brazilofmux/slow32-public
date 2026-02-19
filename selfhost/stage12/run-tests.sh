#!/usr/bin/env bash
set -euo pipefail

# Stage 12: New compiler (Ragel lexer + parser)
#
# Bootstrap chain:
#   stage11/s32cc → compile stage12 sources
#   stage05/s32-as → assemble
#   stage07/s32-ld → link
#
# Tests:
#   1) Duff's device test (nested switch + goto labels)
#   2) Ragel lexer smoke test (compile + basic tokenize)
#   3) Full lexer regression test (10 token test groups)

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
SELFHOST_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"
TESTS_DIR="$SCRIPT_DIR/tests"

EMU="${SELFHOST_EMU:-}"
EMU_EXPLICIT=0
KEEP_ARTIFACTS=0

choose_default_emu() {
    local dbt="$SELFHOST_DIR/../tools/dbt/slow32-dbt"
    if [[ -x "$dbt" ]]; then
        printf '%s\n' "$dbt"
    else
        printf '%s\n' "$SELFHOST_DIR/stage00/s32-emu"
    fi
}

usage() {
    cat <<USAGE
Usage: $0 [--emu <path>] [--keep-artifacts]

Stage12 tests: Ragel lexer + parser tests
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

if [[ "$EMU_EXPLICIT" -eq 0 && -z "$EMU" ]]; then
    EMU="$(choose_default_emu)"
fi

[[ -f "$EMU" ]] || { echo "Missing emulator: $EMU" >&2; exit 1; }

WORKDIR="$(mktemp -d /tmp/selfhost-v2-stage12.XXXXXX)"
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
    if grep -Eq "Execute fault|Memory fault|Write out of bounds|Unknown opcode|Unknown instruction|Load fault|Store fault|Execution limit reached" "$log"; then
        echo "execution faulted: $exe" >&2
        tail -n 60 "$log" >&2
        return 1
    fi
    if [[ "$rc" -ne 0 && "$rc" -ne 96 ]]; then
        echo "execution did not halt cleanly: $exe (rc=$rc)" >&2
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
    if grep -Eq "Execute fault|Memory fault|Write out of bounds|Unknown opcode|Unknown instruction|Load fault|Store fault|Execution limit reached" "$log"; then
        echo "execution faulted: $exe" >&2
        tail -n 60 "$log" >&2
        return 125
    fi
    return "$rc"
}

compile_and_link() {
    local name="$1"
    local src="$2"
    local exe="$WORKDIR/${name}.s32x"
    local asm="$WORKDIR/${name}.s"
    local obj="$WORKDIR/${name}.s32o"

    run_exe "$CC_EXE" "$WORKDIR/${name}-compile.log" "$src" "$asm"
    if [[ ! -s "$asm" ]]; then
        echo "  $name: FAIL (compile)" >&2
        tail -n 20 "$WORKDIR/${name}-compile.log" >&2
        return 1
    fi

    run_exe "$AS_EXE" "$WORKDIR/${name}-assemble.log" "$asm" "$obj"
    if [[ ! -s "$obj" ]]; then
        echo "  $name: FAIL (assemble)" >&2
        tail -n 20 "$WORKDIR/${name}-assemble.log" >&2
        return 1
    fi

    run_exe "$LD_EXE" "$WORKDIR/${name}-link.log" \
        -o "$exe" --mmio 64K \
        "$RUNTIME_CRT0" "$obj" "$LIBC_START_OBJ" "$RUNTIME_MMIO_NO_START_OBJ" \
        $LIBC_OBJS
    if [[ ! -s "$exe" ]]; then
        echo "  $name: FAIL (link)" >&2
        tail -n 20 "$WORKDIR/${name}-link.log" >&2
        return 1
    fi
    printf '%s' "$exe"
}

# ============================================================
# Step 1: Bootstrap — build runtime/libc
# ============================================================
echo "=== Step 1: Bootstrap ==="

CC_EXE="$SELFHOST_DIR/stage11/s32cc.s32x"
AS_EXE="$SELFHOST_DIR/stage05/s32-as.s32x"
LD_EXE="$SELFHOST_DIR/stage07/s32-ld.s32x"

[[ -f "$CC_EXE" ]] || { echo "Missing s32cc (stage11): $CC_EXE" >&2; exit 1; }
[[ -f "$AS_EXE" ]] || { echo "Missing assembler: $AS_EXE" >&2; exit 1; }
[[ -f "$LD_EXE" ]] || { echo "Missing linker: $LD_EXE" >&2; exit 1; }

LIBC_DIR="$SELFHOST_DIR/stage05/libc"
CRT0_SRC="$SELFHOST_DIR/stage05/crt0.s"
MMIO_NO_START_SRC="$SELFHOST_DIR/stage05/mmio_no_start.s"

run_exe "$AS_EXE" "$WORKDIR/crt0.log" "$CRT0_SRC" "$WORKDIR/crt0.s32o"
[[ -s "$WORKDIR/crt0.s32o" ]] || { echo "failed to assemble crt0" >&2; exit 1; }

run_exe "$AS_EXE" "$WORKDIR/mmio_no_start.log" "$MMIO_NO_START_SRC" "$WORKDIR/mmio_no_start.s32o"
[[ -s "$WORKDIR/mmio_no_start.s32o" ]] || { echo "failed to assemble mmio_no_start" >&2; exit 1; }

LIBC_OBJS=""
for name in string_extra string_more ctype convert stdio malloc; do
    run_exe "$CC_EXE" "$WORKDIR/${name}.cc.log" "$LIBC_DIR/${name}.c" "$WORKDIR/${name}.s"
    [[ -s "$WORKDIR/${name}.s" ]] || { echo "failed to compile ${name}.c" >&2; exit 1; }
    run_exe "$AS_EXE" "$WORKDIR/${name}.as.log" "$WORKDIR/${name}.s" "$WORKDIR/${name}.s32o"
    [[ -s "$WORKDIR/${name}.s32o" ]] || { echo "failed to assemble ${name}.s" >&2; exit 1; }
    LIBC_OBJS="$LIBC_OBJS $WORKDIR/${name}.s32o"
done

run_exe "$CC_EXE" "$WORKDIR/start.cc.log" "$LIBC_DIR/start.c" "$WORKDIR/start.s"
[[ -s "$WORKDIR/start.s" ]] || { echo "failed to compile start.c" >&2; exit 1; }
run_exe "$AS_EXE" "$WORKDIR/start.as.log" "$WORKDIR/start.s" "$WORKDIR/start.s32o"
[[ -s "$WORKDIR/start.s32o" ]] || { echo "failed to assemble start.s" >&2; exit 1; }

RUNTIME_CRT0="$WORKDIR/crt0.s32o"
RUNTIME_MMIO_NO_START_OBJ="$WORKDIR/mmio_no_start.s32o"
LIBC_START_OBJ="$WORKDIR/start.s32o"

echo "Compiler (stage11): $CC_EXE"
echo "Assembler: $AS_EXE"
echo "Linker: $LD_EXE"

PASS=0
FAIL=0
TOTAL=0

# ============================================================
# Step 2: Duff's device test
# ============================================================
echo ""
echo "=== Step 2: Duff's device test ==="
TOTAL=$((TOTAL + 1))
DUFF_EXE=""
DUFF_EXE=$(compile_and_link "test_duff" "$TESTS_DIR/test_duff.c") || true
if [[ -n "$DUFF_EXE" && -s "$DUFF_EXE" ]]; then
    set +e
    run_exe_rc "$DUFF_EXE" "$WORKDIR/test_duff-run.log"
    DUFF_RC=$?
    set -e
    if [[ "$DUFF_RC" -eq 0 ]]; then
        printf "  %-30s PASS\n" "test_duff:"
        PASS=$((PASS + 1))
    else
        printf "  %-30s FAIL (rc=%d)\n" "test_duff:" "$DUFF_RC"
        cat "$WORKDIR/test_duff-run.log" >&2
        FAIL=$((FAIL + 1))
    fi
else
    printf "  %-30s FAIL (build)\n" "test_duff:"
    FAIL=$((FAIL + 1))
fi

# ============================================================
# Step 3: Ragel lexer smoke test
# ============================================================
echo ""
echo "=== Step 3: Ragel lexer smoke test ==="

LEXER_GEN="$SCRIPT_DIR/c_lexer_gen.c"
[[ -s "$LEXER_GEN" ]] || { echo "ERROR: c_lexer_gen.c not found (run gen_lexer.sh)" >&2; exit 1; }
echo "  Lexer: $(wc -l < "$LEXER_GEN") lines, $(wc -c < "$LEXER_GEN") bytes"

TOTAL=$((TOTAL + 1))
SMOKE_SRC="$WORKDIR/lex_smoke.c"
cp "$LEXER_GEN" "$SMOKE_SRC"
printf '\nint main(void) {\n    char *src;\n    src = "int x;";\n    lex_init(src, strlen(src));\n    lex_next();\n    if (lex_tok != TK_INT) return 1;\n    lex_next();\n    if (lex_tok != TK_IDENT) return 2;\n    lex_next();\n    if (lex_tok != TK_SEMI) return 3;\n    lex_next();\n    if (lex_tok != TK_EOF) return 4;\n    return 0;\n}\n' >> "$SMOKE_SRC"

echo "  Smoke source: $(wc -c < "$SMOKE_SRC") bytes"
SMOKE_EXE=""
SAVED_TIMEOUT="${EXEC_TIMEOUT:-180}"
EXEC_TIMEOUT=300
SMOKE_EXE=$(compile_and_link "lex_smoke" "$SMOKE_SRC") || true
EXEC_TIMEOUT="$SAVED_TIMEOUT"
if [[ -n "$SMOKE_EXE" && -s "$SMOKE_EXE" ]]; then
    set +e
    run_exe_rc "$SMOKE_EXE" "$WORKDIR/lex_smoke-run.log"
    SMOKE_RC=$?
    set -e
    if [[ "$SMOKE_RC" -eq 0 ]]; then
        printf "  %-30s PASS\n" "lex-smoke:"
        PASS=$((PASS + 1))
    else
        printf "  %-30s FAIL (rc=%d)\n" "lex-smoke:" "$SMOKE_RC"
        cat "$WORKDIR/lex_smoke-run.log" >&2
        FAIL=$((FAIL + 1))
    fi
else
    printf "  %-30s FAIL (build)\n" "lex-smoke:"
    FAIL=$((FAIL + 1))
fi

# ============================================================
# Step 4: Full lexer token tests
# ============================================================
echo ""
echo "=== Step 4: Full lexer token tests ==="

TOTAL=$((TOTAL + 1))
MERGED_TEST="$WORKDIR/lex_test_merged.c"
cp "$LEXER_GEN" "$MERGED_TEST"
grep -v '^#include "c_lexer_gen.c"' "$TESTS_DIR/lex_test.c" >> "$MERGED_TEST"

echo "  Test source: $(wc -c < "$MERGED_TEST") bytes"
TEST_EXE=""
SAVED_TIMEOUT="${EXEC_TIMEOUT:-180}"
EXEC_TIMEOUT=300
TEST_EXE=$(compile_and_link "lex_test" "$MERGED_TEST") || true
EXEC_TIMEOUT="$SAVED_TIMEOUT"
if [[ -n "$TEST_EXE" && -s "$TEST_EXE" ]]; then
    set +e
    run_exe_rc "$TEST_EXE" "$WORKDIR/lex_test-run.log"
    TEST_RC=$?
    set -e
    if [[ "$TEST_RC" -eq 0 ]]; then
        printf "  %-30s PASS\n" "lex-test:"
        PASS=$((PASS + 1))
        cat "$WORKDIR/lex_test-run.log"
    else
        printf "  %-30s FAIL (rc=%d)\n" "lex-test:" "$TEST_RC"
        cat "$WORKDIR/lex_test-run.log" >&2
        FAIL=$((FAIL + 1))
    fi
else
    printf "  %-30s FAIL (build)\n" "lex-test:"
    FAIL=$((FAIL + 1))
fi

# ============================================================
# Step 5: s12cc compiler build + Phase 1 tests
# ============================================================
echo ""
echo "=== Step 5: s12cc compiler tests ==="

# Build s12cc
S12CC_SRC="$SCRIPT_DIR/s12cc.c"
[[ -s "$S12CC_SRC" ]] || { echo "ERROR: s12cc.c not found" >&2; exit 1; }

TOTAL=$((TOTAL + 1))
SAVED_TIMEOUT="${EXEC_TIMEOUT:-180}"
EXEC_TIMEOUT=300
S12CC_EXE=$(compile_and_link "s12cc" "$S12CC_SRC") || true
EXEC_TIMEOUT="$SAVED_TIMEOUT"

if [[ -n "$S12CC_EXE" && -s "$S12CC_EXE" ]]; then
    printf "  %-30s PASS\n" "s12cc-build:"
    PASS=$((PASS + 1))
else
    printf "  %-30s FAIL (build)\n" "s12cc-build:"
    FAIL=$((FAIL + 1))
fi

# Run Phase 1 test programs using s12cc
if [[ -n "$S12CC_EXE" && -s "$S12CC_EXE" ]]; then
    for tst in "$TESTS_DIR"/test_spike.c "$TESTS_DIR"/test_phase2.c; do
        [[ -f "$tst" ]] || continue
        tname="$(basename "$tst" .c)"
        TOTAL=$((TOTAL + 1))

        # Compile with s12cc
        run_exe "$S12CC_EXE" "$WORKDIR/${tname}-s12cc.log" "$tst" "$WORKDIR/${tname}.s"
        if [[ ! -s "$WORKDIR/${tname}.s" ]]; then
            printf "  %-30s FAIL (compile)\n" "$tname:"
            tail -n 20 "$WORKDIR/${tname}-s12cc.log" >&2
            FAIL=$((FAIL + 1))
            continue
        fi

        # Assemble
        run_exe "$AS_EXE" "$WORKDIR/${tname}-as.log" "$WORKDIR/${tname}.s" "$WORKDIR/${tname}.s32o"
        if [[ ! -s "$WORKDIR/${tname}.s32o" ]]; then
            printf "  %-30s FAIL (assemble)\n" "$tname:"
            tail -n 20 "$WORKDIR/${tname}-as.log" >&2
            FAIL=$((FAIL + 1))
            continue
        fi

        # Link
        run_exe "$LD_EXE" "$WORKDIR/${tname}-ld.log" \
            -o "$WORKDIR/${tname}.s32x" --mmio 64K \
            "$RUNTIME_CRT0" "$WORKDIR/${tname}.s32o" "$LIBC_START_OBJ" "$RUNTIME_MMIO_NO_START_OBJ" \
            $LIBC_OBJS
        if [[ ! -s "$WORKDIR/${tname}.s32x" ]]; then
            printf "  %-30s FAIL (link)\n" "$tname:"
            tail -n 20 "$WORKDIR/${tname}-ld.log" >&2
            FAIL=$((FAIL + 1))
            continue
        fi

        # Run
        set +e
        run_exe_rc "$WORKDIR/${tname}.s32x" "$WORKDIR/${tname}-run.log"
        RUN_RC=$?
        set -e
        if [[ "$RUN_RC" -eq 0 ]]; then
            printf "  %-30s PASS\n" "$tname:"
            PASS=$((PASS + 1))
        else
            printf "  %-30s FAIL (rc=%d)\n" "$tname:" "$RUN_RC"
            cat "$WORKDIR/${tname}-run.log" >&2
            FAIL=$((FAIL + 1))
        fi
    done
fi

# ============================================================
# Final report
# ============================================================
echo ""
if [[ "$FAIL" -eq 0 ]]; then
    echo "OK: stage12 ($PASS/$TOTAL tests passed)"
else
    echo "FAIL: stage12 ($PASS/$TOTAL tests passed, $FAIL failed)" >&2
    exit 1
fi

echo "Artifacts: $WORKDIR"
