#!/usr/bin/env bash
set -euo pipefail

# Stage 15: s32-cc evolved + new assembler with bumped limits
#
# Bootstrap chain:
# 1) Bootstrap via stage14 (obtains s32-cc, cc-min, assembler, C linker, runtime)
# 2) Build stage15 s32-cc: stage14 s32-cc compiles stage15 merged source
# 3) Feature tests: compile each test with stage15 s32-cc
# 4) Regression tests: run stage14 parser tests through stage15 compiler
# 5) Self-hosting: s32-cc compiles itself → fixed point proof (gen2.s == gen3.s)
# 6) Build new assembler: s32-cc compiles s32-as.c with bumped limits
# 7) Assembler parity: new assembler produces byte-identical .s32o vs old
# 8) Assembler functional test: new assembler in full pipeline

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
SELFHOST_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"
ROOT_DIR="$(cd "$SELFHOST_DIR/.." && pwd)"
TESTS_DIR="$SCRIPT_DIR/tests"

EMU="${STAGE15_EMU:-}"
EMU_EXPLICIT=0

STAGE14_DIR="$SELFHOST_DIR/stage14"

KEEP_ARTIFACTS=0

choose_default_emu() {
    printf '%s\n' "$SELFHOST_DIR/stage00/s32-emu"
}

usage() {
    cat <<USAGE
Usage: $0 [--emu <path>] [--keep-artifacts]

Stage15 s32-cc compiler + new assembler:
  1) bootstrap s32-cc + assembler + C linker via stage14
  2) build stage15 s32-cc with stage14 s32-cc
  3) feature tests: goto, #include, #ifdef, static const int
  4) regression tests: stage14 parser tests via stage15 compiler
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

if [[ "$EMU_EXPLICIT" -eq 0 && -z "${STAGE15_EMU:-}" ]]; then
    EMU="$(choose_default_emu)"
fi

[[ -f "$EMU" ]] || { echo "Missing emulator: $EMU" >&2; exit 1; }

WORKDIR="$(mktemp -d /tmp/selfhost-v2-stage15.XXXXXX)"
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
        "$LIBC_ARCHIVE"
    if [[ ! -s "$exe" ]]; then
        echo "linker produced no output for $name" >&2
        cat "$WORKDIR/${name}-link.log" >&2
        return 1
    fi
}

# ============================================================
# Step 1: Bootstrap via stage14
# ============================================================
echo "=== Step 1: Bootstrap via stage14 ==="

S14_LOG="$WORKDIR/stage14-build.log"
EMU_FLAG=()
if [[ "$EMU_EXPLICIT" -eq 1 ]]; then
    EMU_FLAG=(--emu "$EMU")
fi
"$STAGE14_DIR/run-spike.sh" "${EMU_FLAG[@]}" --keep-artifacts >"$S14_LOG"

# Extract artifacts from stage14
S14_ART="$(awk -F': ' '/^Artifacts:/{print $2}' "$S14_LOG" | tail -n 1)"
[[ -n "$S14_ART" && -d "$S14_ART" ]] || { echo "failed to locate stage14 artifacts" >&2; exit 1; }

# stage14 builds s32cc.s32x in its workdir
S14_S32CC_EXE="$S14_ART/s32cc.s32x"
[[ -f "$S14_S32CC_EXE" ]] || { echo "missing stage14 s32cc.s32x" >&2; exit 1; }

# Chase back to find assembler, linker, runtime from stage14's stage13
S13_LOG="$S14_ART/stage13-build.log"
[[ -f "$S13_LOG" ]] || { echo "missing stage13 build log in stage14 artifacts" >&2; exit 1; }
S13_ART="$(awk -F': ' '/^Artifacts:/{print $2}' "$S13_LOG" | tail -n 1)"
[[ -n "$S13_ART" && -d "$S13_ART" ]] || { echo "failed to locate stage13 artifacts" >&2; exit 1; }

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

S8_LOG="$S9_ART/stage08-build.log"
[[ -f "$S8_LOG" ]] || { echo "missing stage08 build log" >&2; exit 1; }
AS_EXE="$(awk -F': ' '/^Assembler exe:/{print $2}' "$S8_LOG" | tail -n 1)"
S8_ART="$(awk -F': ' '/^Artifacts:/{print $2}' "$S8_LOG" | tail -n 1)"
[[ -n "$AS_EXE" && -f "$AS_EXE" ]] || { echo "failed to locate assembler" >&2; exit 1; }

LD_EXE="$(awk -F': ' '/^Linker exe:/{print $2}' "$S8_LOG" | tail -n 1)"
[[ -n "$LD_EXE" && -f "$LD_EXE" ]] || { echo "failed to locate C linker" >&2; exit 1; }

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

echo "Stage14 s32-cc: $S14_S32CC_EXE"
echo "Assembler: $AS_EXE"
echo "C linker: $LD_EXE"

PASS=0
FAIL=0
TOTAL=0

# ============================================================
# Step 2: Build stage15 s32-cc (two-phase bootstrap)
# ============================================================
echo ""
echo "=== Step 2: Build stage15 s32-cc ==="

# Create full merged source: lex + parse + driver (minus #include line)
S15_MERGED="$WORKDIR/s32cc_merged.c"
cat "$SCRIPT_DIR/s32cc_lex.h" "$SCRIPT_DIR/s32cc_parse.h" > "$S15_MERGED"
tail -n +2 "$SCRIPT_DIR/s32cc.c" >> "$S15_MERGED"

MERGED_SZ="$(wc -c < "$S15_MERGED")"
echo "  Stage15 merged source: $MERGED_SZ bytes"

# Phase 1: Build boot compiler — stage14 code + P_MAX_OUT=1048576.
# Stage15's new features generate > 524KB asm, exceeding stage14's output
# buffer. So we first build a boot compiler with a 1MB output buffer
# (same code as stage14 otherwise) that stage14 can compile.
BOOT_MERGED="$WORKDIR/boot_merged.c"
S14_LEX="$SELFHOST_DIR/stage14/s32cc_lex.h"
S14_PARSE="$SELFHOST_DIR/stage14/s32cc_parse.h"
S14_MAIN="$SELFHOST_DIR/stage14/s32cc.c"

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
    echo "stage14 s32-cc produced no assembly for boot" >&2
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
    "$LIBC_ARCHIVE"
if [[ ! -s "$BOOT_EXE" ]]; then
    echo "linker produced no output for boot" >&2
    cat "$WORKDIR/boot-link.log" >&2
    exit 1
fi
echo "  Phase 1: boot.s32x linked OK ($(wc -c < "$BOOT_EXE") bytes)"

# Phase 2: Build stage15 with boot compiler (1MB output buffer)
if [[ "$MERGED_SZ" -ge 65536 ]]; then
    echo "ERROR: stage15 source ($MERGED_SZ bytes) exceeds LEX_SRC_SZ (65536)" >&2
    exit 1
fi

S32CC_ASM="$WORKDIR/s32cc.s"
S32CC_OBJ="$WORKDIR/s32cc.s32o"
S32CC_EXE="$WORKDIR/s32cc.s32x"

run_exe "$BOOT_EXE" "$WORKDIR/s32cc-compile.log" "$S15_MERGED" "$S32CC_ASM"
if [[ ! -s "$S32CC_ASM" ]]; then
    echo "boot produced no assembly for stage15" >&2
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
    "$LIBC_ARCHIVE"
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
# Step 3: Feature tests (compile with stage15 s32-cc)
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
# Step 4: Regression tests (stage14 parser tests through stage15 compiler)
# ============================================================
echo ""
echo "=== Step 4: Regression tests ==="

S14_TESTS_DIR="$SELFHOST_DIR/stage14/tests"
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
# Step 5: Self-hosting — stage15 s32-cc compiles itself
# ============================================================
echo ""
echo "=== Step 5: Self-hosting ==="

echo "  Merged source: $MERGED_SZ bytes (limit 262144)"

SAVED_TIMEOUT="${EXEC_TIMEOUT:-180}"
EXEC_TIMEOUT=300

# Gen1 compile: stage15 s32cc (compiled by stage14) compiles merged → gen2.s
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
    "$LIBC_ARCHIVE"
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
SMOKE_SRC="$SELFHOST_DIR/stage14/tests/test_parse_smoke.c"
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
            "$LIBC_ARCHIVE"
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
    "$LIBC_ARCHIVE"
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

FUNC_SRC="$SELFHOST_DIR/stage14/tests/test_parse_smoke.c"
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
            "$LIBC_ARCHIVE"
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
    echo "OK: stage15 ($PASS/$TOTAL tests passed)"
else
    echo "FAIL: stage15 ($PASS/$TOTAL tests passed, $FAIL failed)" >&2
    exit 1
fi

echo "Emulator: $EMU"
echo "Compiler exe: $S32CC_EXE"
echo "New assembler: $NEW_AS_EXE"
echo "Linker exe: $LD_EXE"
echo "Runtime crt0: $RUNTIME_CRT0"
echo "Runtime mmio: $RUNTIME_MMIO_NO_START_OBJ"
echo "Libc archive: $LIBC_ARCHIVE"
echo "Libc start: $LIBC_START_OBJ"
echo "Artifacts: $WORKDIR"
