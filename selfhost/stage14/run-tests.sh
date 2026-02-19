#!/usr/bin/env bash
set -euo pipefail

# Stage 14: Compiler playground (copy of stage13, bootstrapped by stage13)

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

Stage14 tests: s12cc compiler tests (bootstrapped from stage13)
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

echo "Compiler (stage11 for libc): $CC_EXE"
echo "Assembler: $AS_EXE"
echo "Linker: $LD_EXE"

PASS=0
FAIL=0
TOTAL=0

# ============================================================
# Step 2: Build s14cc using stage13's s12cc
# ============================================================
echo ""
echo "=== Step 2: Build s14cc (compiled by stage13 s12cc) ==="

S13CC_EXE="$SELFHOST_DIR/stage13/s12cc.s32x"
[[ -f "$S13CC_EXE" ]] || { echo "Missing s12cc (stage13): $S13CC_EXE" >&2; exit 1; }

S14CC_SRC="$SCRIPT_DIR/s12cc.c"
[[ -s "$S14CC_SRC" ]] || { echo "ERROR: s12cc.c not found" >&2; exit 1; }

TOTAL=$((TOTAL + 1))
SAVED_TIMEOUT="${EXEC_TIMEOUT:-180}"
EXEC_TIMEOUT=300

# Compile stage14 source with stage13 compiler
run_exe "$S13CC_EXE" "$WORKDIR/s14cc-compile.log" "$S14CC_SRC" "$WORKDIR/s14cc.s"
if [[ ! -s "$WORKDIR/s14cc.s" ]]; then
    printf "  %-30s FAIL (compile)\n" "s14cc-build:"
    tail -n 20 "$WORKDIR/s14cc-compile.log" >&2
    FAIL=$((FAIL + 1))
else
    run_exe "$AS_EXE" "$WORKDIR/s14cc-assemble.log" "$WORKDIR/s14cc.s" "$WORKDIR/s14cc.s32o"
    if [[ ! -s "$WORKDIR/s14cc.s32o" ]]; then
        printf "  %-30s FAIL (assemble)\n" "s14cc-build:"
        FAIL=$((FAIL + 1))
    else
        run_exe "$LD_EXE" "$WORKDIR/s14cc-link.log" \
            -o "$WORKDIR/s14cc.s32x" --mmio 64K \
            "$RUNTIME_CRT0" "$WORKDIR/s14cc.s32o" "$LIBC_START_OBJ" "$RUNTIME_MMIO_NO_START_OBJ" \
            $LIBC_OBJS
        if [[ ! -s "$WORKDIR/s14cc.s32x" ]]; then
            printf "  %-30s FAIL (link)\n" "s14cc-build:"
            FAIL=$((FAIL + 1))
        else
            printf "  %-30s PASS\n" "s14cc-build:"
            PASS=$((PASS + 1))
        fi
    fi
fi
EXEC_TIMEOUT="$SAVED_TIMEOUT"

# ============================================================
# Step 3: Test s14cc by compiling and running test programs
# ============================================================
S14CC_EXE="$WORKDIR/s14cc.s32x"

if [[ -s "$S14CC_EXE" ]]; then
    CC_EXE="$S14CC_EXE"
    echo ""
    echo "=== Step 3: s14cc compiler tests ==="

    for tst in "$TESTS_DIR"/test_spike.c "$TESTS_DIR"/test_phase2.c "$TESTS_DIR"/test_phase3.c "$TESTS_DIR"/test_phase4.c "$TESTS_DIR"/test_phase5.c "$TESTS_DIR"/test_phase6.c "$TESTS_DIR"/test_phase7.c "$TESTS_DIR"/test_phase8.c "$TESTS_DIR"/test_phase9.c "$TESTS_DIR"/test_phase10.c "$TESTS_DIR"/test_phase11.c "$TESTS_DIR"/test_phase12.c; do
        [[ -f "$tst" ]] || continue
        tname="$(basename "$tst" .c)"
        TOTAL=$((TOTAL + 1))

        # Compile with s14cc
        run_exe "$CC_EXE" "$WORKDIR/${tname}-s14cc.log" "$tst" "$WORKDIR/${tname}.s"
        if [[ ! -s "$WORKDIR/${tname}.s" ]]; then
            printf "  %-30s FAIL (compile)\n" "$tname:"
            tail -n 20 "$WORKDIR/${tname}-s14cc.log" >&2
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
    echo "OK: stage14 ($PASS/$TOTAL tests passed)"
else
    echo "FAIL: stage14 ($PASS/$TOTAL tests passed, $FAIL failed)" >&2
    exit 1
fi

echo "Artifacts: $WORKDIR"
