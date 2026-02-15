#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
ROOT_DIR="${STAGE4_ROOT:-$(cd "$SCRIPT_DIR/../.." && pwd)}"
if git -C "$SCRIPT_DIR" rev-parse --show-toplevel >/dev/null 2>&1; then
    ROOT_DIR="$(git -C "$SCRIPT_DIR" rev-parse --show-toplevel)"
fi

EMU="${STAGE4_EMU:-$ROOT_DIR/tools/emulator/slow32}"
KERNEL="$ROOT_DIR/forth/kernel.s32x"
PRELUDE="$ROOT_DIR/forth/prelude.fth"
CC_FTH="${STAGE4_CC:-$SCRIPT_DIR/cc.fth}"
ASM_FTH="${STAGE4_ASM:-$ROOT_DIR/selfhost/stage2/asm.fth}"
LINK_FTH="${STAGE4_LINK:-$ROOT_DIR/selfhost/stage3/link.fth}"
TEST_DIR="${STAGE4_TEST_DIR:-$SCRIPT_DIR/tests}"
VALIDATION_DIR="${STAGE4_VALIDATION_DIR:-$SCRIPT_DIR/validation}"

SLOW32DUMP=0
KEEP_ARTIFACTS=0

usage() {
    cat <<USAGE
Usage: $0 [--slow32dump] [--keep-artifacts] [--emu <path>]

Runs Stage 4 compiler regression in a staged work-up:
  Stage A: test1.c, test2.c
  Stage B: test3.c, test4.c, test5.c, test6.c
  Stage C: test7.c, test8.c, test9.c
  Stage D (optional): validation/slow32dump.c
    - on minimal emulator (selfhost/stage0/s32-emu): smoke-run (--help)
    - on full emulators: disassemble test3.s32x and validate output

Defaults:
  Emulator: \$STAGE4_EMU or $ROOT_DIR/tools/emulator/slow32
  Minimal emulator example: --emu $ROOT_DIR/selfhost/stage0/s32-emu
USAGE
}

while [[ $# -gt 0 ]]; do
    case "$1" in
        --slow32dump) SLOW32DUMP=1 ;;
        --keep-artifacts) KEEP_ARTIFACTS=1 ;;
        --emu)
            shift
            [[ $# -gt 0 ]] || { echo "--emu requires a path" >&2; exit 2; }
            EMU="$1"
            ;;
        -h|--help) usage; exit 0 ;;
        *)
            echo "Unknown option: $1" >&2
            usage
            exit 2
            ;;
    esac
    shift
done

for f in "$EMU" "$KERNEL" "$PRELUDE" "$CC_FTH" "$ASM_FTH" "$LINK_FTH"; do
    [[ -f "$f" ]] || { echo "Missing required file: $f" >&2; exit 1; }
done

WORKDIR="$(mktemp -d /tmp/stage4-regression.XXXXXX)"
if [[ "$KEEP_ARTIFACTS" -eq 0 ]]; then
    trap 'rm -rf "$WORKDIR"' EXIT
fi

run_forth() {
    local script_a="$1"
    local script_b="$2"
    local cmd_text="$3"
    local log_file="$4"

    set +e
    cat "$PRELUDE" "$script_a" "$script_b" - <<FTH | timeout 180 "$EMU" "$KERNEL" >"$log_file" 2>&1
$cmd_text
FTH
    local rc=$?
    set -e
    if [[ "$rc" -ne 0 && "$rc" -ne 96 ]]; then
        echo "forth pipeline failed (rc=$rc)" >&2
        tail -n 40 "$log_file" >&2
        return 1
    fi
}

compile_c() {
    local src="$1"
    local asm="$2"
    local log="$3"

    run_forth "$CC_FTH" /dev/null "S\" $src\" S\" $asm\" COMPILE-FILE
BYE" "$log"
    [[ -s "$asm" ]] || { echo "compile produced no output: $src" >&2; return 1; }
    grep -q "Compilation successful" "$log" || {
        echo "compile failed: $src" >&2
        tail -n 40 "$log" >&2
        return 1
    }
}

assemble_s() {
    local asm="$1"
    local obj="$2"
    local log="$3"

    run_forth "$ASM_FTH" /dev/null "S\" $asm\" S\" $obj\" ASSEMBLE
BYE" "$log"
    [[ -s "$obj" ]] || { echo "assembler produced no output: $asm" >&2; return 1; }
    if grep -q "FAILED:" "$log"; then
        echo "assembler failed: $asm" >&2
        tail -n 40 "$log" >&2
        return 1
    fi
}

link_obj() {
    local obj="$1"
    local exe="$2"
    local log="$3"

    run_forth "$LINK_FTH" /dev/null "LINK-INIT
S\" $ROOT_DIR/runtime/crt0.s32o\" LINK-OBJ
S\" $obj\" LINK-OBJ
65536 LINK-MMIO
S\" $ROOT_DIR/runtime/libc_mmio.s32a\" LINK-ARCHIVE
S\" $ROOT_DIR/runtime/libs32.s32a\" LINK-ARCHIVE
S\" $exe\" LINK-EMIT
BYE" "$log"
    [[ -s "$exe" ]] || { echo "linker produced no output: $obj" >&2; return 1; }
}

run_exe() {
    local exe="$1"
    local log="$2"
    shift 2

    set +e
    timeout 30 "$EMU" "$exe" "$@" >"$log" 2>&1
    local rc=$?
    set -e
    if [[ "$rc" -eq 124 ]]; then
        echo "execution timed out: $exe" >&2
        tail -n 40 "$log" >&2
        return 1
    fi
    if grep -Eq "Execute fault|Memory fault|Unknown opcode|Unknown instruction" "$log"; then
        echo "execution faulted: $exe" >&2
        tail -n 40 "$log" >&2
        return 1
    fi
    if [[ "$rc" -ne 0 && "$rc" -ne 96 ]]; then
        echo "execution did not halt cleanly: $exe" >&2
        tail -n 40 "$log" >&2
        return 1
    fi
}

build_and_run_test() {
    local test_name="$1"
    local src="$TEST_DIR/${test_name}.c"
    local asm="$WORKDIR/${test_name}.s"
    local obj="$WORKDIR/${test_name}.s32o"
    local exe="$WORKDIR/${test_name}.s32x"

    echo "  - $test_name"
    compile_c "$src" "$asm" "$WORKDIR/${test_name}.cc.log"
    assemble_s "$asm" "$obj" "$WORKDIR/${test_name}.as.log"
    link_obj "$obj" "$exe" "$WORKDIR/${test_name}.ld.log"
    run_exe "$exe" "$WORKDIR/${test_name}.run.log"
}

run_stage() {
    local stage_name="$1"
    shift
    echo "[$stage_name]"
    for t in "$@"; do
        build_and_run_test "$t"
    done
}

run_stage "Stage A" test1 test2
run_stage "Stage B" test3 test4 test5 test6
run_stage "Stage C" test7 test8 test9

if [[ "$SLOW32DUMP" -eq 1 ]]; then
    echo "[Stage D]"
    echo "  - slow32dump"
    slow_src="$VALIDATION_DIR/slow32dump.c"
    slow_asm="$WORKDIR/slow32dump.s"
    slow_obj="$WORKDIR/slow32dump.s32o"
    slow_exe="$WORKDIR/slow32dump.s32x"

    compile_c "$slow_src" "$slow_asm" "$WORKDIR/slow32dump.cc.log"
    assemble_s "$slow_asm" "$slow_obj" "$WORKDIR/slow32dump.as.log"
    link_obj "$slow_obj" "$slow_exe" "$WORKDIR/slow32dump.ld.log"
    if [[ "$EMU" == *"/selfhost/stage0/s32-emu" ]]; then
        run_exe "$slow_exe" "$WORKDIR/slow32dump.run.log" "--help"
        if ! grep -q "Usage:" "$WORKDIR/slow32dump.run.log"; then
            echo "slow32dump minimal-emulator smoke validation failed" >&2
            tail -n 40 "$WORKDIR/slow32dump.run.log" >&2
            exit 1
        fi
    else
        run_exe "$slow_exe" "$WORKDIR/slow32dump.run.log" "$WORKDIR/test3.s32x"
        if ! grep -q "file format s32x-slow32" "$WORKDIR/slow32dump.run.log"; then
            echo "slow32dump ran but output validation failed" >&2
            tail -n 40 "$WORKDIR/slow32dump.run.log" >&2
            exit 1
        fi
    fi
fi

echo "OK: Stage 4 regression passed"
echo "Artifacts: $WORKDIR"
