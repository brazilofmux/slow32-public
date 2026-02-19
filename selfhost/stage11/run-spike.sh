#!/usr/bin/env bash
set -euo pipefail

# Stage 11: s32-cc evolved + new assembler with bumped limits
#
# Bootstrap chain:
# 1) Build runtime/libc, then build stage10 s32-cc from source
# 2) Build stage11 s32-cc: stage10 s32-cc compiles stage11 merged source
# 3) Feature tests: compile each test with stage11 s32-cc
# 4) Regression tests: run stage10 parser tests through stage11 compiler
# 5) Self-hosting: s32-cc compiles itself → fixed point proof (gen2.s == gen3.s)
# 6) Build new assembler: s32-cc compiles s32-as.c with bumped limits
# 7) Assembler parity: new assembler produces byte-identical .s32o vs old
# 8) Assembler functional test: new assembler in full pipeline

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
SELFHOST_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"
ROOT_DIR="$(cd "$SELFHOST_DIR/.." && pwd)"
TESTS_DIR="$SCRIPT_DIR/tests"

EMU="${STAGE11_EMU:-}"
EMU_EXPLICIT=0

KEEP_ARTIFACTS=0

choose_default_emu() {
    printf '%s\n' "$SELFHOST_DIR/stage00/s32-emu"
}

usage() {
    cat <<USAGE
Usage: $0 [--emu <path>] [--keep-artifacts]

Stage11 s32-cc compiler + new assembler:
  1) build runtime/libc, then build stage10 s32-cc from source
  2) build stage11 s32-cc with stage10 s32-cc
  3) feature tests: goto, #include, #ifdef, static const int
  4) regression tests: stage10 parser tests via stage11 compiler
  5) self-hosting: s32-cc compiles itself, fixed point + smoke test
  6) build new assembler with s32-cc (bumped limits: 1MB text/data)
  7) assembler parity test (byte-identical .s32o output)
  8) assembler functional test (new assembler in full pipeline)
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

[[ -f "$EMU" ]] || { echo "Missing emulator: $EMU" >&2; exit 1; }

WORKDIR="$(mktemp -d /tmp/selfhost-v2-stage11.XXXXXX)"
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

compile_s32cc() {
    local src="$1"
    local name="$2"
    local exe="$3"

    local asm_out="$WORKDIR/${name}.s"
    local obj_out="$WORKDIR/${name}.s32o"

    # Compile with s32-cc
    run_exe "$S32CC_EXE" "$WORKDIR/${name}-compile.log" "$src" "$asm_out"
    if [[ ! -s "$asm_out" ]]; then
        echo "s32-cc produced no assembly for $name" >&2
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
# Step 1: Bootstrap — build runtime/libc, then stage10 s32-cc
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
for name in string_extra convert stdio; do
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

# Build stage10 s32-cc from source (using cc-min)
STAGE10_DIR="$SELFHOST_DIR/stage10"
S10_MERGED="$WORKDIR/s10_merged.c"
cat "$STAGE10_DIR/s32cc_lex.h" "$STAGE10_DIR/s32cc_parse.h" > "$S10_MERGED"
tail -n +2 "$STAGE10_DIR/s32cc.c" >> "$S10_MERGED"

run_exe "$GEN2_EXE" "$WORKDIR/s10-compile.log" "$S10_MERGED" "$WORKDIR/s10.s"
[[ -s "$WORKDIR/s10.s" ]] || { echo "failed to compile stage10 s32cc" >&2; exit 1; }

run_exe "$AS_EXE" "$WORKDIR/s10-assemble.log" "$WORKDIR/s10.s" "$WORKDIR/s10.s32o"
[[ -s "$WORKDIR/s10.s32o" ]] || { echo "failed to assemble stage10 s32cc" >&2; exit 1; }

S14_S32CC_EXE="$WORKDIR/s10-s32cc.s32x"
run_exe "$LD_EXE" "$WORKDIR/s10-link.log" \
    -o "$S14_S32CC_EXE" --mmio 64K \
    "$RUNTIME_CRT0" "$WORKDIR/s10.s32o" "$LIBC_START_OBJ" "$RUNTIME_MMIO_NO_START_OBJ" \
    $LIBC_OBJS
[[ -s "$S14_S32CC_EXE" ]] || { echo "failed to link stage10 s32cc" >&2; exit 1; }

echo "Stage10 s32-cc: $S14_S32CC_EXE"
echo "Assembler: $AS_EXE"
echo "C linker: $LD_EXE"

PASS=0
FAIL=0
TOTAL=0

# ============================================================
# Step 2: Build stage11 s32-cc (two-phase bootstrap)
# ============================================================
echo ""
echo "=== Step 2: Build stage11 s32-cc ==="

# Create full merged source: lex + parse + driver (minus #include line)
S15_MERGED="$WORKDIR/s32cc_merged.c"
cat "$SCRIPT_DIR/s32cc_lex.h" "$SCRIPT_DIR/s32cc_parse.h" > "$S15_MERGED"
tail -n +2 "$SCRIPT_DIR/s32cc.c" >> "$S15_MERGED"

MERGED_SZ="$(wc -c < "$S15_MERGED")"
echo "  Stage15 merged source: $MERGED_SZ bytes"

# Phase 1: Build boot compiler — stage10 code + P_MAX_OUT=1048576.
# Stage15's new features generate > 524KB asm, exceeding stage10's output
# buffer. So we first build a boot compiler with a 1MB output buffer
# (same code as stage10 otherwise) that stage10 can compile.
BOOT_MERGED="$WORKDIR/boot_merged.c"
S14_LEX="$SELFHOST_DIR/stage10/s32cc_lex.h"
S14_PARSE="$SELFHOST_DIR/stage10/s32cc_parse.h"
S14_MAIN="$SELFHOST_DIR/stage10/s32cc.c"

cat "$S14_LEX" "$S14_PARSE" > "$BOOT_MERGED"
tail -n +2 "$S14_MAIN" >> "$BOOT_MERGED"

# Patch P_MAX_OUT in boot source from 524288 to 1048576
sed -i 's/#define P_MAX_OUT 524288/#define P_MAX_OUT 1048576/' "$BOOT_MERGED"

BOOT_SZ="$(wc -c < "$BOOT_MERGED")"
echo "  Boot merged source: $BOOT_SZ bytes (limit 65536)"
if [[ "$BOOT_SZ" -ge 65536 ]]; then
    echo "ERROR: boot merged source ($BOOT_SZ bytes) exceeds LEX_SRC_SZ (65536)" >&2
    exit 1
fi

# Bump timeout for compiling large source
SAVED_TIMEOUT="${EXEC_TIMEOUT:-180}"
EXEC_TIMEOUT=300

BOOT_ASM="$WORKDIR/boot.s"
BOOT_OBJ="$WORKDIR/boot.s32o"
BOOT_EXE="$WORKDIR/boot.s32x"

run_exe "$S14_S32CC_EXE" "$WORKDIR/boot-compile.log" "$BOOT_MERGED" "$BOOT_ASM"
if [[ ! -s "$BOOT_ASM" ]]; then
    echo "stage10 s32-cc produced no assembly for boot" >&2
    cat "$WORKDIR/boot-compile.log" >&2
    exit 1
fi
echo "  Phase 1: boot compiled OK ($(wc -c < "$BOOT_ASM") bytes asm)"

run_exe "$AS_EXE" "$WORKDIR/boot-assemble.log" "$BOOT_ASM" "$BOOT_OBJ"
if [[ ! -s "$BOOT_OBJ" ]]; then
    echo "assembler produced no output for boot" >&2
    cat "$WORKDIR/boot-assemble.log" >&2
    exit 1
fi

run_exe "$LD_EXE" "$WORKDIR/boot-link.log" \
    -o "$BOOT_EXE" --mmio 64K \
    "$RUNTIME_CRT0" "$BOOT_OBJ" "$LIBC_START_OBJ" "$RUNTIME_MMIO_NO_START_OBJ" \
    $LIBC_OBJS
if [[ ! -s "$BOOT_EXE" ]]; then
    echo "linker produced no output for boot" >&2
    cat "$WORKDIR/boot-link.log" >&2
    exit 1
fi
echo "  Phase 1: boot.s32x linked OK ($(wc -c < "$BOOT_EXE") bytes)"

# Phase 2: Build stage11 with boot compiler (1MB output buffer)
if [[ "$MERGED_SZ" -ge 65536 ]]; then
    echo "ERROR: stage11 source ($MERGED_SZ bytes) exceeds LEX_SRC_SZ (65536)" >&2
    exit 1
fi

S32CC_ASM="$WORKDIR/s32cc.s"
S32CC_OBJ="$WORKDIR/s32cc.s32o"
S32CC_EXE="$WORKDIR/s32cc.s32x"

run_exe "$BOOT_EXE" "$WORKDIR/s32cc-compile.log" "$S15_MERGED" "$S32CC_ASM"
if [[ ! -s "$S32CC_ASM" ]]; then
    echo "boot produced no assembly for stage11" >&2
    cat "$WORKDIR/s32cc-compile.log" >&2
    exit 1
fi
echo "  Phase 2: s32cc compiled OK ($(wc -c < "$S32CC_ASM") bytes asm)"

run_exe "$AS_EXE" "$WORKDIR/s32cc-assemble.log" "$S32CC_ASM" "$S32CC_OBJ"
if [[ ! -s "$S32CC_OBJ" ]]; then
    echo "assembler produced no output for s32cc" >&2
    cat "$WORKDIR/s32cc-assemble.log" >&2
    exit 1
fi

run_exe "$LD_EXE" "$WORKDIR/s32cc-link.log" \
    -o "$S32CC_EXE" --mmio 64K \
    "$RUNTIME_CRT0" "$S32CC_OBJ" "$LIBC_START_OBJ" "$RUNTIME_MMIO_NO_START_OBJ" \
    $LIBC_OBJS
if [[ ! -s "$S32CC_EXE" ]]; then
    echo "linker produced no output for s32cc" >&2
    cat "$WORKDIR/s32cc-link.log" >&2
    exit 1
fi
echo "  Phase 2: s32cc.s32x linked OK ($(wc -c < "$S32CC_EXE") bytes)"
EXEC_TIMEOUT="$SAVED_TIMEOUT"

TOTAL=$((TOTAL + 1))
PASS=$((PASS + 1))

# ============================================================
# Step 3: Feature tests (compile with stage11 s32-cc)
# ============================================================
echo ""
echo "=== Step 3: Feature tests ==="

FEATURE_TESTS="test_goto test_include test_ifdef test_static_const"

for test_name in $FEATURE_TESTS; do
    TOTAL=$((TOTAL + 1))
    SRC="$TESTS_DIR/${test_name}.c"
    EXE="$WORKDIR/${test_name}.s32x"

    if [[ ! -f "$SRC" ]]; then
        printf "  %-24s SKIP (source not found)\n" "${test_name}:"
        FAIL=$((FAIL + 1))
        continue
    fi

    # For #include test, copy the header into workdir
    if [[ "$test_name" == "test_include" ]]; then
        cp "$TESTS_DIR/test_include.h" "$WORKDIR/test_include.h"
        cp "$SRC" "$WORKDIR/${test_name}.c"
        SRC="$WORKDIR/${test_name}.c"
    fi

    if ! compile_s32cc "$SRC" "$test_name" "$EXE" 2>"$WORKDIR/${test_name}-build-err.log"; then
        printf "  %-24s FAIL (build)\n" "${test_name}:"
        cat "$WORKDIR/${test_name}-build-err.log" >&2
        FAIL=$((FAIL + 1))
        continue
    fi

    set +e
    run_exe_rc "$EXE" "$WORKDIR/${test_name}-run.log"
    TEST_RC=$?
    set -e

    if [[ "$TEST_RC" -eq 0 ]]; then
        printf "  %-24s PASS (rc=0)\n" "${test_name}:"
        PASS=$((PASS + 1))
    else
        printf "  %-24s FAIL (rc=%d)\n" "${test_name}:" "$TEST_RC"
        FAIL=$((FAIL + 1))
    fi
done

# ============================================================
# Step 4: Regression tests (stage10 parser tests through stage11 compiler)
# ============================================================
echo ""
echo "=== Step 4: Regression tests ==="

S14_TESTS_DIR="$SELFHOST_DIR/stage10/tests"
REGR_TESTS="test_parse_smoke test_parse_expr test_parse_ctrl test_parse_ptr test_parse_mixed"

for test_name in $REGR_TESTS; do
    TOTAL=$((TOTAL + 1))
    SRC="$S14_TESTS_DIR/${test_name}.c"
    EXE="$WORKDIR/regr-${test_name}.s32x"

    if [[ ! -f "$SRC" ]]; then
        printf "  %-24s SKIP (source not found)\n" "${test_name}:"
        FAIL=$((FAIL + 1))
        continue
    fi

    if ! compile_s32cc "$SRC" "regr-${test_name}" "$EXE" 2>"$WORKDIR/regr-${test_name}-build-err.log"; then
        printf "  %-24s FAIL (build)\n" "${test_name}:"
        cat "$WORKDIR/regr-${test_name}-build-err.log" >&2
        FAIL=$((FAIL + 1))
        continue
    fi

    set +e
    run_exe_rc "$EXE" "$WORKDIR/regr-${test_name}-run.log"
    TEST_RC=$?
    set -e

    if [[ "$TEST_RC" -eq 0 ]]; then
        printf "  %-24s PASS (rc=0)\n" "${test_name}:"
        PASS=$((PASS + 1))
    else
        printf "  %-24s FAIL (rc=%d)\n" "${test_name}:" "$TEST_RC"
        FAIL=$((FAIL + 1))
    fi
done

# ============================================================
# Step 5: Self-hosting — stage11 s32-cc compiles itself
# ============================================================
echo ""
echo "=== Step 5: Self-hosting ==="

echo "  Merged source: $MERGED_SZ bytes (limit 262144)"

SAVED_TIMEOUT="${EXEC_TIMEOUT:-180}"
EXEC_TIMEOUT=300

# Gen1 compile: stage11 s32cc (compiled by stage10) compiles merged → gen2.s
GEN2_S32CC_ASM="$WORKDIR/gen2-s32cc.s"
run_exe "$S32CC_EXE" "$WORKDIR/gen1-compile.log" "$S15_MERGED" "$GEN2_S32CC_ASM"
if [[ ! -s "$GEN2_S32CC_ASM" ]]; then
    echo "Gen1 produced no assembly output" >&2
    cat "$WORKDIR/gen1-compile.log" >&2
    exit 1
fi
GEN2_ASM_SZ="$(wc -c < "$GEN2_S32CC_ASM")"
echo "  Gen1 → Gen2: OK ($GEN2_ASM_SZ bytes asm)"
TOTAL=$((TOTAL + 1))
PASS=$((PASS + 1))

# Assemble + link Gen2
GEN2_S32CC_OBJ="$WORKDIR/gen2-s32cc.s32o"
GEN2_S32CC_EXE="$WORKDIR/gen2-s32cc.s32x"

run_exe "$AS_EXE" "$WORKDIR/gen2-assemble.log" "$GEN2_S32CC_ASM" "$GEN2_S32CC_OBJ"
if [[ ! -s "$GEN2_S32CC_OBJ" ]]; then
    echo "assembler produced no output for gen2" >&2
    cat "$WORKDIR/gen2-assemble.log" >&2
    exit 1
fi

run_exe "$LD_EXE" "$WORKDIR/gen2-link.log" \
    -o "$GEN2_S32CC_EXE" --mmio 64K \
    "$RUNTIME_CRT0" "$GEN2_S32CC_OBJ" "$LIBC_START_OBJ" "$RUNTIME_MMIO_NO_START_OBJ" \
    $LIBC_OBJS
if [[ ! -s "$GEN2_S32CC_EXE" ]]; then
    echo "linker produced no output for gen2" >&2
    cat "$WORKDIR/gen2-link.log" >&2
    exit 1
fi
echo "  Gen2 assembled + linked OK"
TOTAL=$((TOTAL + 1))
PASS=$((PASS + 1))

# Gen2 compile: gen2-s32cc compiles merged → gen3.s
GEN3_S32CC_ASM="$WORKDIR/gen3-s32cc.s"
run_exe "$GEN2_S32CC_EXE" "$WORKDIR/gen2-compile.log" "$S15_MERGED" "$GEN3_S32CC_ASM"
if [[ ! -s "$GEN3_S32CC_ASM" ]]; then
    echo "Gen2 produced no assembly output" >&2
    cat "$WORKDIR/gen2-compile.log" >&2
    exit 1
fi
GEN3_ASM_SZ="$(wc -c < "$GEN3_S32CC_ASM")"
echo "  Gen2 → Gen3: OK ($GEN3_ASM_SZ bytes asm)"

EXEC_TIMEOUT="$SAVED_TIMEOUT"

# Fixed-point check
TOTAL=$((TOTAL + 1))
if diff "$GEN2_S32CC_ASM" "$GEN3_S32CC_ASM" >/dev/null 2>&1; then
    echo "  FIXED POINT PROVEN: gen2.s == gen3.s"
    PASS=$((PASS + 1))
else
    echo "  FIXED POINT FAILED: gen2.s and gen3.s differ" >&2
    diff "$GEN2_S32CC_ASM" "$GEN3_S32CC_ASM" | head -n 40 >&2
    FAIL=$((FAIL + 1))
fi

# Smoke test: Gen2 compiles test_parse_smoke.c
SMOKE_SRC="$SELFHOST_DIR/stage10/tests/test_parse_smoke.c"
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
# Step 6: Build new assembler with s32-cc
# ============================================================
echo ""
echo "=== Step 6: Build new assembler ==="

# Copy assembler source to workdir (s32-cc needs files in same dir for #include)
cp "$SCRIPT_DIR/s32-as.c" "$WORKDIR/s32-as.c"
cp "$SCRIPT_DIR/s32_formats_min.h" "$WORKDIR/s32_formats_min.h"

SAVED_TIMEOUT="${EXEC_TIMEOUT:-180}"
EXEC_TIMEOUT=300

NEW_AS_ASM="$WORKDIR/s32-as.s"
NEW_AS_OBJ="$WORKDIR/s32-as.s32o"
NEW_AS_EXE="$WORKDIR/s32-as.s32x"

TOTAL=$((TOTAL + 1))

run_exe "$S32CC_EXE" "$WORKDIR/s32-as-compile.log" "$WORKDIR/s32-as.c" "$NEW_AS_ASM"
if [[ ! -s "$NEW_AS_ASM" ]]; then
    echo "s32-cc produced no assembly for s32-as" >&2
    cat "$WORKDIR/s32-as-compile.log" >&2
    exit 1
fi
echo "  Compiled s32-as.c OK ($(wc -c < "$NEW_AS_ASM") bytes asm)"

run_exe "$AS_EXE" "$WORKDIR/s32-as-assemble.log" "$NEW_AS_ASM" "$NEW_AS_OBJ"
if [[ ! -s "$NEW_AS_OBJ" ]]; then
    echo "assembler produced no output for s32-as" >&2
    cat "$WORKDIR/s32-as-assemble.log" >&2
    exit 1
fi

run_exe "$LD_EXE" "$WORKDIR/s32-as-link.log" \
    -o "$NEW_AS_EXE" --mmio 64K \
    "$RUNTIME_CRT0" "$NEW_AS_OBJ" "$LIBC_START_OBJ" "$RUNTIME_MMIO_NO_START_OBJ" \
    $LIBC_OBJS
if [[ ! -s "$NEW_AS_EXE" ]]; then
    echo "linker produced no output for s32-as" >&2
    cat "$WORKDIR/s32-as-link.log" >&2
    exit 1
fi

EXEC_TIMEOUT="$SAVED_TIMEOUT"
echo "  New assembler: $NEW_AS_EXE ($(wc -c < "$NEW_AS_EXE") bytes)"
PASS=$((PASS + 1))

# ============================================================
# Step 7: Assembler parity test
# ============================================================
echo ""
echo "=== Step 7: Assembler parity test ==="

# Use gen2 assembly from self-hosting as parity test input
PARITY_INPUT="$GEN2_S32CC_ASM"

SAVED_TIMEOUT="${EXEC_TIMEOUT:-180}"
EXEC_TIMEOUT=300

# Assemble with old assembler
OLD_PARITY_OBJ="$WORKDIR/parity-old.s32o"
run_exe "$AS_EXE" "$WORKDIR/parity-old.log" "$PARITY_INPUT" "$OLD_PARITY_OBJ"
if [[ ! -s "$OLD_PARITY_OBJ" ]]; then
    echo "old assembler failed on parity input" >&2
    cat "$WORKDIR/parity-old.log" >&2
    exit 1
fi

# Assemble with new assembler
NEW_PARITY_OBJ="$WORKDIR/parity-new.s32o"
run_exe "$NEW_AS_EXE" "$WORKDIR/parity-new.log" "$PARITY_INPUT" "$NEW_PARITY_OBJ"
if [[ ! -s "$NEW_PARITY_OBJ" ]]; then
    echo "new assembler failed on parity input" >&2
    cat "$WORKDIR/parity-new.log" >&2
    exit 1
fi

EXEC_TIMEOUT="$SAVED_TIMEOUT"

TOTAL=$((TOTAL + 1))
if diff "$OLD_PARITY_OBJ" "$NEW_PARITY_OBJ" >/dev/null 2>&1; then
    echo "  PARITY PROVEN: old.s32o == new.s32o ($(wc -c < "$OLD_PARITY_OBJ") bytes)"
    PASS=$((PASS + 1))
else
    echo "  PARITY FAILED: old and new assembler outputs differ" >&2
    echo "  old: $(wc -c < "$OLD_PARITY_OBJ") bytes, new: $(wc -c < "$NEW_PARITY_OBJ") bytes" >&2
    FAIL=$((FAIL + 1))
fi

# ============================================================
# Step 8: Assembler functional test (new assembler in pipeline)
# ============================================================
echo ""
echo "=== Step 8: New assembler functional test ==="

FUNC_SRC="$SELFHOST_DIR/stage10/tests/test_parse_smoke.c"
FUNC_ASM="$WORKDIR/func-test.s"
FUNC_OBJ="$WORKDIR/func-test.s32o"
FUNC_EXE="$WORKDIR/func-test.s32x"

TOTAL=$((TOTAL + 1))
run_exe "$S32CC_EXE" "$WORKDIR/func-test-compile.log" "$FUNC_SRC" "$FUNC_ASM"
if [[ ! -s "$FUNC_ASM" ]]; then
    echo "  Functional test: FAIL (compile)" >&2
    FAIL=$((FAIL + 1))
else
    # Assemble with NEW assembler
    run_exe "$NEW_AS_EXE" "$WORKDIR/func-test-assemble.log" "$FUNC_ASM" "$FUNC_OBJ"
    if [[ ! -s "$FUNC_OBJ" ]]; then
        echo "  Functional test: FAIL (assemble with new AS)" >&2
        FAIL=$((FAIL + 1))
    else
        run_exe "$LD_EXE" "$WORKDIR/func-test-link.log" \
            -o "$FUNC_EXE" --mmio 64K \
            "$RUNTIME_CRT0" "$FUNC_OBJ" "$LIBC_START_OBJ" "$RUNTIME_MMIO_NO_START_OBJ" \
            $LIBC_OBJS
        if [[ ! -s "$FUNC_EXE" ]]; then
            echo "  Functional test: FAIL (link)" >&2
            FAIL=$((FAIL + 1))
        else
            set +e
            run_exe_rc "$FUNC_EXE" "$WORKDIR/func-test-run.log"
            FUNC_RC=$?
            set -e

            if [[ "$FUNC_RC" -eq 0 ]]; then
                echo "  Functional test: PASS (rc=0)"
                PASS=$((PASS + 1))
            else
                echo "  Functional test: FAIL (rc=$FUNC_RC)" >&2
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
    echo "OK: stage11 ($PASS/$TOTAL tests passed)"
else
    echo "FAIL: stage11 ($PASS/$TOTAL tests passed, $FAIL failed)" >&2
    exit 1
fi

echo "Emulator: $EMU"
echo "Compiler exe: $S32CC_EXE"
echo "New assembler: $NEW_AS_EXE"
echo "Linker exe: $LD_EXE"
echo "Runtime crt0: $RUNTIME_CRT0"
echo "Runtime mmio: $RUNTIME_MMIO_NO_START_OBJ"
echo "Libc objects: $LIBC_OBJS"
echo "Libc start: $LIBC_START_OBJ"
echo "Artifacts: $WORKDIR"
