#!/usr/bin/env bash
set -euo pipefail

# Stage 10: s32-cc C compiler (Pass 1 lexer + Pass 2 parser)
#
# Bootstrap chain:
# 1) Bootstrap via stage09 (obtains Gen2 cc-min, assembler, C linker, runtime)
# 2) Lexer tests: compile each test_lex_*.c with cc-min, run
# 3) Build s32-cc: concatenate lex+parse headers, compile s32cc.c with cc-min
# 4) Parser tests: compile each test_parse_*.c with s32-cc, assemble, link, run
# 5) Self-hosting: s32-cc compiles itself → fixed point proof (gen2.s == gen3.s)

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
SELFHOST_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"
ROOT_DIR="$(cd "$SELFHOST_DIR/.." && pwd)"
TESTS_DIR="$SCRIPT_DIR/tests"

EMU="${STAGE10_EMU:-}"
EMU_EXPLICIT=0

KEEP_ARTIFACTS=0

choose_default_emu() {
    printf '%s\n' "$SELFHOST_DIR/stage00/s32-emu"
}

usage() {
    cat <<USAGE
Usage: $0 [--emu <path>] [--keep-artifacts]

Stage10 s32-cc compiler tests:
  1) build runtime/libc from source using pre-built tools
  2) compile each test_lex_*.c with cc-min, run
  3) build s32-cc compiler from s32cc.c with cc-min
  4) compile each test_parse_*.c with s32-cc, assemble, link, run
  5) self-hosting: s32-cc compiles itself, fixed point + smoke test
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

if [[ "$EMU_EXPLICIT" -eq 0 && -z "${STAGE10_EMU:-}" ]]; then
    EMU="$(choose_default_emu)"
fi

[[ -f "$EMU" ]] || { echo "Missing emulator: $EMU" >&2; exit 1; }

WORKDIR="$(mktemp -d /tmp/selfhost-v2-stage10.XXXXXX)"
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
        $LIBC_OBJS
    if [[ ! -s "$exe" ]]; then
        echo "linker produced no output for $name" >&2
        cat "$WORKDIR/${name}-link.log" >&2
        return 1
    fi
}

# ============================================================
# Step 1: Bootstrap — use pre-built tools, build runtime/libc
# ============================================================
echo "=== Step 1: Bootstrap ==="

# Pre-built tools from earlier stages
GEN2_EXE="$SELFHOST_DIR/stage09/cc-min.s32x"
AS_EXE="$SELFHOST_DIR/stage05/s32-as.s32x"
LD_EXE="$SELFHOST_DIR/stage07/s32-ld.s32x"

[[ -f "$GEN2_EXE" ]] || { echo "missing cc-min: $GEN2_EXE" >&2; exit 1; }
[[ -f "$AS_EXE" ]] || { echo "missing assembler: $AS_EXE" >&2; exit 1; }
[[ -f "$LD_EXE" ]] || { echo "missing linker: $LD_EXE" >&2; exit 1; }

# Build runtime objects from source
LIBC_DIR="$SELFHOST_DIR/stage05/libc"
CRT0_SRC="$SELFHOST_DIR/stage05/crt0.s"
MMIO_NO_START_SRC="$SELFHOST_DIR/stage05/mmio_no_start.s"

run_exe "$AS_EXE" "$WORKDIR/crt0.log" "$CRT0_SRC" "$WORKDIR/crt0.s32o"
[[ -s "$WORKDIR/crt0.s32o" ]] || { echo "failed to assemble crt0" >&2; exit 1; }

run_exe "$AS_EXE" "$WORKDIR/mmio_no_start.log" "$MMIO_NO_START_SRC" "$WORKDIR/mmio_no_start.s32o"
[[ -s "$WORKDIR/mmio_no_start.s32o" ]] || { echo "failed to assemble mmio_no_start" >&2; exit 1; }

# Build libc (compiled by cc-min)
LIBC_OBJS=""
for name in string_extra string_more ctype convert stdio malloc; do
    run_exe "$GEN2_EXE" "$WORKDIR/${name}.cc.log" "$LIBC_DIR/${name}.c" "$WORKDIR/${name}.s"
    [[ -s "$WORKDIR/${name}.s" ]] || { echo "failed to compile ${name}.c" >&2; exit 1; }
    run_exe "$AS_EXE" "$WORKDIR/${name}.as.log" "$WORKDIR/${name}.s" "$WORKDIR/${name}.s32o"
    [[ -s "$WORKDIR/${name}.s32o" ]] || { echo "failed to assemble ${name}.s" >&2; exit 1; }
    LIBC_OBJS="$LIBC_OBJS $WORKDIR/${name}.s32o"
done

# Build start.c (libc startup)
run_exe "$GEN2_EXE" "$WORKDIR/start.cc.log" "$LIBC_DIR/start.c" "$WORKDIR/start.s"
[[ -s "$WORKDIR/start.s" ]] || { echo "failed to compile start.c" >&2; exit 1; }
run_exe "$AS_EXE" "$WORKDIR/start.as.log" "$WORKDIR/start.s" "$WORKDIR/start.s32o"
[[ -s "$WORKDIR/start.s32o" ]] || { echo "failed to assemble start.s" >&2; exit 1; }

RUNTIME_CRT0="$WORKDIR/crt0.s32o"
RUNTIME_MMIO_NO_START_OBJ="$WORKDIR/mmio_no_start.s32o"
LIBC_START_OBJ="$WORKDIR/start.s32o"

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
    $LIBC_OBJS
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

PARSE_TESTS="test_parse_smoke test_parse_expr test_parse_ctrl test_parse_ptr test_parse_mixed test_malloc test_libc"

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
        $LIBC_OBJS 2>"$WORKDIR/${test_name}-ld-err.log"; then
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
# Step 5: Self-hosting — s32-cc compiles itself
# ============================================================
echo ""
echo "=== Step 5: Self-hosting ==="

# Create merged source: lex header + parse header + driver (minus #include line)
S32CC_MERGED="$WORKDIR/s32cc_merged.c"
cat "$SCRIPT_DIR/s32cc_lex.h" "$SCRIPT_DIR/s32cc_parse.h" > "$S32CC_MERGED"
tail -n +2 "$SCRIPT_DIR/s32cc.c" >> "$S32CC_MERGED"

MERGED_SZ="$(wc -c < "$S32CC_MERGED")"
echo "  Merged source: $MERGED_SZ bytes (limit 131072)"
if [[ "$MERGED_SZ" -ge 131072 ]]; then
    echo "ERROR: merged source ($MERGED_SZ bytes) exceeds LEX_SRC_SZ (131072)" >&2
    exit 1
fi

# Save and bump timeout for self-compilation (compiling 62KB source is slow)
SAVED_TIMEOUT="${EXEC_TIMEOUT:-180}"
EXEC_TIMEOUT=300

# Gen1 compile: s32cc.s32x (compiled by cc-min) compiles merged → gen2-s32cc.s
GEN2_S32CC_ASM="$WORKDIR/gen2-s32cc.s"
run_exe "$S32CC_EXE" "$WORKDIR/gen1-s32cc-compile.log" "$S32CC_MERGED" "$GEN2_S32CC_ASM"
if [[ ! -s "$GEN2_S32CC_ASM" ]]; then
    echo "Gen1 (s32cc) produced no assembly output" >&2
    cat "$WORKDIR/gen1-s32cc-compile.log" >&2
    exit 1
fi
GEN2_ASM_SZ="$(wc -c < "$GEN2_S32CC_ASM")"
echo "  Gen1 → Gen2: OK ($GEN2_ASM_SZ bytes asm)"
TOTAL=$((TOTAL + 1))
PASS=$((PASS + 1))

# Assemble + link Gen2
GEN2_S32CC_OBJ="$WORKDIR/gen2-s32cc.s32o"
GEN2_S32CC_EXE="$WORKDIR/gen2-s32cc.s32x"

run_exe "$AS_EXE" "$WORKDIR/gen2-s32cc-assemble.log" "$GEN2_S32CC_ASM" "$GEN2_S32CC_OBJ"
if [[ ! -s "$GEN2_S32CC_OBJ" ]]; then
    echo "assembler produced no output for gen2-s32cc" >&2
    cat "$WORKDIR/gen2-s32cc-assemble.log" >&2
    exit 1
fi

run_exe "$LD_EXE" "$WORKDIR/gen2-s32cc-link.log" \
    -o "$GEN2_S32CC_EXE" --mmio 64K \
    "$RUNTIME_CRT0" "$GEN2_S32CC_OBJ" "$LIBC_START_OBJ" "$RUNTIME_MMIO_NO_START_OBJ" \
    $LIBC_OBJS
if [[ ! -s "$GEN2_S32CC_EXE" ]]; then
    echo "linker produced no output for gen2-s32cc" >&2
    cat "$WORKDIR/gen2-s32cc-link.log" >&2
    exit 1
fi
echo "  Gen2 assembled + linked OK"
TOTAL=$((TOTAL + 1))
PASS=$((PASS + 1))

# Gen2 compile: gen2-s32cc.s32x compiles merged → gen3-s32cc.s
GEN3_S32CC_ASM="$WORKDIR/gen3-s32cc.s"
run_exe "$GEN2_S32CC_EXE" "$WORKDIR/gen2-s32cc-compile.log" "$S32CC_MERGED" "$GEN3_S32CC_ASM"
if [[ ! -s "$GEN3_S32CC_ASM" ]]; then
    echo "Gen2 (s32cc) produced no assembly output" >&2
    cat "$WORKDIR/gen2-s32cc-compile.log" >&2
    exit 1
fi
GEN3_ASM_SZ="$(wc -c < "$GEN3_S32CC_ASM")"
echo "  Gen2 → Gen3: OK ($GEN3_ASM_SZ bytes asm)"

# Restore timeout
EXEC_TIMEOUT="$SAVED_TIMEOUT"

# Fixed-point check: gen2.s must equal gen3.s
TOTAL=$((TOTAL + 1))
if diff "$GEN2_S32CC_ASM" "$GEN3_S32CC_ASM" >/dev/null 2>&1; then
    echo "  FIXED POINT PROVEN: gen2.s == gen3.s"
    PASS=$((PASS + 1))
else
    echo "  FIXED POINT FAILED: gen2.s and gen3.s differ" >&2
    diff "$GEN2_S32CC_ASM" "$GEN3_S32CC_ASM" | head -n 40 >&2
    FAIL=$((FAIL + 1))
fi

# Smoke test: Gen2 compiles test_parse_smoke.c → assemble → link → run
SMOKE_SRC="$TESTS_DIR/test_parse_smoke.c"
SMOKE_ASM="$WORKDIR/selfhost-smoke.s"
SMOKE_OBJ="$WORKDIR/selfhost-smoke.s32o"
SMOKE_EXE="$WORKDIR/selfhost-smoke.s32x"

TOTAL=$((TOTAL + 1))
run_exe "$GEN2_S32CC_EXE" "$WORKDIR/selfhost-smoke-compile.log" "$SMOKE_SRC" "$SMOKE_ASM"
if [[ ! -s "$SMOKE_ASM" ]]; then
    echo "  Smoke test: FAIL (Gen2 produced no assembly)" >&2
    FAIL=$((FAIL + 1))
else
    run_exe "$AS_EXE" "$WORKDIR/selfhost-smoke-assemble.log" "$SMOKE_ASM" "$SMOKE_OBJ"
    if [[ ! -s "$SMOKE_OBJ" ]]; then
        echo "  Smoke test: FAIL (assemble)" >&2
        FAIL=$((FAIL + 1))
    else
        run_exe "$LD_EXE" "$WORKDIR/selfhost-smoke-link.log" \
            -o "$SMOKE_EXE" --mmio 64K \
            "$RUNTIME_CRT0" "$SMOKE_OBJ" "$LIBC_START_OBJ" "$RUNTIME_MMIO_NO_START_OBJ" \
            $LIBC_OBJS
        if [[ ! -s "$SMOKE_EXE" ]]; then
            echo "  Smoke test: FAIL (link)" >&2
            FAIL=$((FAIL + 1))
        else
            set +e
            run_exe_rc "$SMOKE_EXE" "$WORKDIR/selfhost-smoke-run.log"
            SMOKE_RC=$?
            set -e

            if [[ "$SMOKE_RC" -eq 0 ]]; then
                echo "  Smoke test: PASS (rc=0)"
                PASS=$((PASS + 1))
            else
                echo "  Smoke test: FAIL (rc=$SMOKE_RC)" >&2
                FAIL=$((FAIL + 1))
            fi
        fi
    fi
fi

# ============================================================
# Final report
# ============================================================
echo ""
if [[ "$FAIL" -eq 0 ]]; then
    echo "OK: stage10 ($PASS/$TOTAL tests passed)"
else
    echo "FAIL: stage10 ($PASS/$TOTAL tests passed, $FAIL failed)" >&2
    exit 1
fi

echo "Emulator: $EMU"
echo "Artifacts: $WORKDIR"
