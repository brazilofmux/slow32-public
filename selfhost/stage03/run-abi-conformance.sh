#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
SELFHOST_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"
ROOT_DIR="$(cd "$SELFHOST_DIR/.." && pwd)"
TEST_DIR="$SCRIPT_DIR/tests/abi"

EMU="${SELFHOST_EMU:-}"
EMU_EXPLICIT=0
KEEP_ARTIFACTS=0

CC_EXE="$SCRIPT_DIR/cc.s32x"
AS_EXE="$SCRIPT_DIR/s32-as.s32x"
LD_EXE="$SCRIPT_DIR/s32-ld.s32x"
RUNTIME_DIR="$SCRIPT_DIR"
LIBC_DIR="$SCRIPT_DIR/libc"

choose_default_emu() {
    local dbt="$ROOT_DIR/tools/dbt/slow32-dbt"
    if [[ -x "$dbt" ]]; then
        printf '%s\n' "$dbt"
    else
        printf '%s\n' "$SELFHOST_DIR/stage00/s32-emu"
    fi
}

usage() {
    cat <<USAGE
Usage: $0 [options]

Run ABI conformance tests against a compiler/toolchain tuple.

Options:
  --cc <path>            Compiler executable (default: stage03/cc.s32x, required)
  --as <path>            Assembler executable (default: stage03/s32-as.s32x)
  --ld <path>            Linker executable (default: stage03/s32-ld.s32x)
  --runtime-dir <path>   Directory containing crt0.s and mmio_no_start.s
  --libc-dir <path>      Directory containing libc sources
  --emu <path>           Emulator (default: dbt if present, else stage00/s32-emu)
  --keep-artifacts       Keep workdir
  -h, --help             Show this help
USAGE
}

while [[ $# -gt 0 ]]; do
    case "$1" in
        --cc) shift; CC_EXE="$1" ;;
        --as) shift; AS_EXE="$1" ;;
        --ld) shift; LD_EXE="$1" ;;
        --runtime-dir) shift; RUNTIME_DIR="$1" ;;
        --libc-dir) shift; LIBC_DIR="$1" ;;
        --emu) shift; EMU="$1"; EMU_EXPLICIT=1 ;;
        --keep-artifacts) KEEP_ARTIFACTS=1 ;;
        -h|--help) usage; exit 0 ;;
        *) echo "Unknown option: $1" >&2; usage; exit 2 ;;
    esac
    shift
done

if [[ "$EMU_EXPLICIT" -eq 0 && -z "$EMU" ]]; then
    EMU="$(choose_default_emu)"
fi

[[ -f "$EMU" ]] || { echo "Missing emulator: $EMU" >&2; exit 1; }
[[ -f "$CC_EXE" ]] || { echo "Missing compiler: $CC_EXE" >&2; exit 1; }
[[ -f "$AS_EXE" ]] || { echo "Missing assembler: $AS_EXE" >&2; exit 1; }
[[ -f "$LD_EXE" ]] || { echo "Missing linker: $LD_EXE" >&2; exit 1; }
[[ -f "$RUNTIME_DIR/crt0.s" ]] || { echo "Missing runtime source: $RUNTIME_DIR/crt0.s" >&2; exit 1; }
[[ -f "$RUNTIME_DIR/mmio_no_start.s" ]] || { echo "Missing runtime source: $RUNTIME_DIR/mmio_no_start.s" >&2; exit 1; }
[[ -f "$LIBC_DIR/start.c" ]] || { echo "Missing libc source: $LIBC_DIR/start.c" >&2; exit 1; }

for f in \
    "$TEST_DIR/callee_save_target.c" \
    "$TEST_DIR/callee_save_main.s" \
    "$TEST_DIR/arg_caller.c" \
    "$TEST_DIR/arg_probe.s" \
    "$TEST_DIR/arg_probe_main.s"; do
    [[ -f "$f" ]] || { echo "Missing ABI test input: $f" >&2; exit 1; }
done

WORKDIR="$(mktemp -d /tmp/selfhost-abi.XXXXXX)"
if [[ "$KEEP_ARTIFACTS" -eq 0 ]]; then
    trap 'rm -rf "$WORKDIR"' EXIT
fi

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

compile_c() {
    local src="$1"
    local out_asm="$2"
    local log="$3"
    run_exe "$CC_EXE" "$log" "$src" "$out_asm"
    [[ -s "$out_asm" ]] || { echo "compiler produced no output: $src" >&2; return 1; }
}

assemble_s() {
    local src="$1"
    local out_obj="$2"
    local log="$3"
    run_exe "$AS_EXE" "$log" "$src" "$out_obj"
    [[ -s "$out_obj" ]] || { echo "assembler produced no output: $src" >&2; return 1; }
}

echo "=== ABI setup ==="
echo "Compiler: $CC_EXE"
echo "Assembler: $AS_EXE"
echo "Linker: $LD_EXE"
echo "Runtime: $RUNTIME_DIR"
echo "Libc: $LIBC_DIR"

assemble_s "$RUNTIME_DIR/crt0.s" "$WORKDIR/crt0.s32o" "$WORKDIR/crt0.log"
assemble_s "$RUNTIME_DIR/mmio_no_start.s" "$WORKDIR/mmio_no_start.s32o" "$WORKDIR/mmio_no_start.log"

LIBC_OBJS=""
for name in string_extra string_more ctype convert stdio malloc; do
    compile_c "$LIBC_DIR/${name}.c" "$WORKDIR/${name}.s" "$WORKDIR/${name}.cc.log"
    assemble_s "$WORKDIR/${name}.s" "$WORKDIR/${name}.s32o" "$WORKDIR/${name}.as.log"
    LIBC_OBJS="$LIBC_OBJS $WORKDIR/${name}.s32o"
done
compile_c "$LIBC_DIR/start.c" "$WORKDIR/start.s" "$WORKDIR/start.cc.log"
assemble_s "$WORKDIR/start.s" "$WORKDIR/start.s32o" "$WORKDIR/start.as.log"

PASS=0
FAIL=0
TOTAL=0

link_and_run_expect_zero() {
    local name="$1"
    shift
    local exe="$WORKDIR/${name}.s32x"

    TOTAL=$((TOTAL + 1))
    if ! run_exe "$LD_EXE" "$WORKDIR/${name}.ld.log" \
        -o "$exe" --mmio 64K \
        "$WORKDIR/crt0.s32o" "$@" "$WORKDIR/start.s32o" "$WORKDIR/mmio_no_start.s32o" \
        $LIBC_OBJS; then
        printf "  %-26s FAIL (link)\n" "${name}:"
        FAIL=$((FAIL + 1))
        return
    fi
    if [[ ! -s "$exe" ]]; then
        printf "  %-26s FAIL (no exe)\n" "${name}:"
        FAIL=$((FAIL + 1))
        return
    fi

    set +e
    run_exe_rc "$exe" "$WORKDIR/${name}.run.log"
    local rc=$?
    set -e
    if [[ "$rc" -eq 0 ]]; then
        printf "  %-26s PASS\n" "${name}:"
        PASS=$((PASS + 1))
    else
        printf "  %-26s FAIL (rc=%d)\n" "${name}:" "$rc"
        tail -n 20 "$WORKDIR/${name}.run.log" >&2
        FAIL=$((FAIL + 1))
    fi
}

echo ""
echo "=== ABI tests ==="

# Test 1: compiled callee must preserve callee-saved regs and compute expected return.
compile_c "$TEST_DIR/callee_save_target.c" "$WORKDIR/callee_save_target.s" "$WORKDIR/callee_save_target.cc.log"
assemble_s "$WORKDIR/callee_save_target.s" "$WORKDIR/callee_save_target.s32o" "$WORKDIR/callee_save_target.as.log"
assemble_s "$TEST_DIR/callee_save_main.s" "$WORKDIR/callee_save_main.s32o" "$WORKDIR/callee_save_main.as.log"
link_and_run_expect_zero "abi-callee-save" \
    "$WORKDIR/callee_save_main.s32o" "$WORKDIR/callee_save_target.s32o"

# Test 2: compiled caller must pass 10 args correctly (register + stack).
compile_c "$TEST_DIR/arg_caller.c" "$WORKDIR/arg_caller.s" "$WORKDIR/arg_caller.cc.log"
assemble_s "$WORKDIR/arg_caller.s" "$WORKDIR/arg_caller.s32o" "$WORKDIR/arg_caller.as.log"
assemble_s "$TEST_DIR/arg_probe.s" "$WORKDIR/arg_probe.s32o" "$WORKDIR/arg_probe.as.log"
assemble_s "$TEST_DIR/arg_probe_main.s" "$WORKDIR/arg_probe_main.s32o" "$WORKDIR/arg_probe_main.as.log"
link_and_run_expect_zero "abi-arg-pass-10" \
    "$WORKDIR/arg_probe_main.s32o" "$WORKDIR/arg_caller.s32o" "$WORKDIR/arg_probe.s32o"

echo ""
if [[ "$FAIL" -eq 0 ]]; then
    echo "OK: ABI conformance ($PASS/$TOTAL tests passed)"
else
    echo "FAIL: ABI conformance ($PASS/$TOTAL tests passed, $FAIL failed)" >&2
    [[ "$KEEP_ARTIFACTS" -eq 1 ]] && echo "Artifacts: $WORKDIR" >&2
    exit 1
fi

if [[ "$KEEP_ARTIFACTS" -eq 1 ]]; then
    echo "Artifacts: $WORKDIR"
fi
