#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
ROOT_DIR="${SELFHOST_ROOT:-$(cd "$SCRIPT_DIR/../../.." && pwd)}"
if git -C "$SCRIPT_DIR" rev-parse --show-toplevel >/dev/null 2>&1; then
    ROOT_DIR="$(git -C "$SCRIPT_DIR" rev-parse --show-toplevel)"
fi

EMU="${SELFHOST_EMU:-$ROOT_DIR/tools/emulator/slow32}"
KERNEL="${SELFHOST_KERNEL:-$ROOT_DIR/forth/kernel.s32x}"
PRELUDE="${SELFHOST_PRELUDE:-$ROOT_DIR/forth/prelude.fth}"

CC_FTH="${SELFHOST_CC_FTH:-$ROOT_DIR/selfhost/v2/stage04/cc.fth}"
ASM_FTH="${SELFHOST_ASM_FTH:-$ROOT_DIR/selfhost/v2/stage01/asm.fth}"
LINK_FTH="${SELFHOST_LINK_FTH:-$ROOT_DIR/selfhost/v2/stage03/link.fth}"

TEST_DIR="${SELFHOST_TEST_DIR:-$ROOT_DIR/selfhost/v2/stage04/tests}"
VALIDATION_DIR="${SELFHOST_VALIDATION_DIR:-$ROOT_DIR/selfhost/v2/stage04/validation}"

MODE="baseline"
TEST_NAME="test3"
KEEP_ARTIFACTS=0

usage() {
    cat <<USAGE
Usage: $0 [--mode baseline|progressive-as|progressive-as-ar] [--test <name>] [--emu <path>] [--keep-artifacts]

Modes:
  baseline          Stage4 cc.fth + Stage1 asm.fth + Stage3 link.fth
  progressive-as    Stage4 cc.fth + Stage5 s32-as.c (for .s -> .s32o) + Stage3 link.fth
  progressive-as-ar Stage4 cc.fth + Stage5 s32-as.c + Stage6 s32-ar.c smoke-check + Stage3 link.fth

Env overrides:
  SELFHOST_ROOT SELFHOST_EMU SELFHOST_KERNEL SELFHOST_PRELUDE
  SELFHOST_CC_FTH SELFHOST_ASM_FTH SELFHOST_LINK_FTH
  SELFHOST_TEST_DIR SELFHOST_VALIDATION_DIR
USAGE
}

while [[ $# -gt 0 ]]; do
    case "$1" in
        --mode)
            shift
            [[ $# -gt 0 ]] || { echo "--mode requires a value" >&2; exit 2; }
            MODE="$1"
            ;;
        --test)
            shift
            [[ $# -gt 0 ]] || { echo "--test requires a value" >&2; exit 2; }
            TEST_NAME="$1"
            ;;
        --emu)
            shift
            [[ $# -gt 0 ]] || { echo "--emu requires a path" >&2; exit 2; }
            EMU="$1"
            ;;
        --keep-artifacts) KEEP_ARTIFACTS=1 ;;
        -h|--help) usage; exit 0 ;;
        *)
            echo "Unknown option: $1" >&2
            usage
            exit 2
            ;;
    esac
    shift
done

case "$MODE" in
    baseline|progressive-as|progressive-as-ar) ;;
    *)
        echo "Unknown mode: $MODE" >&2
        usage
        exit 2
        ;;
esac

for f in "$EMU" "$KERNEL" "$PRELUDE" "$CC_FTH" "$ASM_FTH" "$LINK_FTH"; do
    [[ -f "$f" ]] || { echo "Missing required file: $f" >&2; exit 1; }
done

WORKDIR="$(mktemp -d /tmp/selfhost-v2-stage05.XXXXXX)"
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
        tail -n 60 "$log_file" >&2
        return 1
    fi
}

run_exe() {
    local exe="$1"
    local log="$2"
    shift 2

    set +e
    timeout 60 "$EMU" "$exe" "$@" >"$log" 2>&1
    local rc=$?
    set -e
    if [[ "$rc" -eq 124 ]]; then
        echo "execution timed out: $exe" >&2
        tail -n 60 "$log" >&2
        return 1
    fi
    if grep -Eq "Execute fault|Memory fault|Unknown opcode|Unknown instruction" "$log"; then
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

compile_c_stage4() {
    local src="$1"
    local asm="$2"
    local log="$3"

    run_forth "$CC_FTH" /dev/null "S\" $src\" S\" $asm\" COMPILE-FILE
BYE" "$log"
    [[ -s "$asm" ]] || { echo "compile produced no output: $src" >&2; return 1; }
    grep -q "Compilation successful" "$log" || {
        echo "compile failed: $src" >&2
        tail -n 60 "$log" >&2
        return 1
    }
}

assemble_forth() {
    local asm="$1"
    local obj="$2"
    local log="$3"

    run_forth "$ASM_FTH" /dev/null "S\" $asm\" S\" $obj\" ASSEMBLE
BYE" "$log"
    [[ -s "$obj" ]] || { echo "assembler produced no output: $asm" >&2; return 1; }
    if grep -q "FAILED:" "$log"; then
        echo "assembler failed: $asm" >&2
        tail -n 60 "$log" >&2
        return 1
    fi
}

link_forth() {
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

build_stage5_assembler() {
    local src="$VALIDATION_DIR/s32-as.c"
    local asm="$WORKDIR/s32-as.s"
    local obj="$WORKDIR/s32-as.s32o"
    local exe="$WORKDIR/s32-as.s32x"

    [[ -f "$src" ]] || { echo "Missing source: $src" >&2; return 1; }
    compile_c_stage4 "$src" "$asm" "$WORKDIR/s32-as.cc.log"
    assemble_forth "$asm" "$obj" "$WORKDIR/s32-as.as.log"
    link_forth "$obj" "$exe" "$WORKDIR/s32-as.ld.log"
    STAGE5_AS_EXE="$exe"
}

assemble_with_stage5() {
    local asm="$1"
    local obj="$2"
    local log="$3"

    [[ -n "${STAGE5_AS_EXE:-}" ]] || { echo "stage5 assembler is not built" >&2; return 1; }
    run_exe "$STAGE5_AS_EXE" "$log" "$asm" "$obj"
    [[ -s "$obj" ]] || { echo "stage5 assembler produced no output: $asm" >&2; return 1; }
}

build_stage6_archiver() {
    local src="$VALIDATION_DIR/s32-ar.c"
    local asm="$WORKDIR/s32-ar.s"
    local obj="$WORKDIR/s32-ar.s32o"
    local exe="$WORKDIR/s32-ar.s32x"

    [[ -f "$src" ]] || { echo "Missing source: $src" >&2; return 1; }
    compile_c_stage4 "$src" "$asm" "$WORKDIR/s32-ar.cc.log"
    assemble_with_stage5 "$asm" "$obj" "$WORKDIR/s32-ar.as.log"
    link_forth "$obj" "$exe" "$WORKDIR/s32-ar.ld.log"
    STAGE6_AR_EXE="$exe"
}

stage6_archive_smoke() {
    local archive="$WORKDIR/smoke.s32a"
    local log="$WORKDIR/s32-ar.run.log"

    [[ -n "${STAGE6_AR_EXE:-}" ]] || { echo "stage6 archiver is not built" >&2; return 1; }
    run_exe "$STAGE6_AR_EXE" "$log" c "$archive" "$TARGET_OBJ"
    [[ -s "$archive" ]] || { echo "stage6 archiver produced no output" >&2; return 1; }
}

if [[ "$TEST_NAME" = /* ]] || [[ "$TEST_NAME" == *.c ]]; then
    TARGET_SRC="$TEST_NAME"
else
    TARGET_SRC="$TEST_DIR/${TEST_NAME}.c"
fi
[[ -f "$TARGET_SRC" ]] || { echo "Missing test source: $TARGET_SRC" >&2; exit 1; }

TARGET_ASM="$WORKDIR/target.s"
TARGET_OBJ="$WORKDIR/target.s32o"
TARGET_EXE="$WORKDIR/target.s32x"

compile_c_stage4 "$TARGET_SRC" "$TARGET_ASM" "$WORKDIR/target.cc.log"

case "$MODE" in
    baseline)
        assemble_forth "$TARGET_ASM" "$TARGET_OBJ" "$WORKDIR/target.as.log"
        ;;
    progressive-as|progressive-as-ar)
        build_stage5_assembler
        assemble_with_stage5 "$TARGET_ASM" "$TARGET_OBJ" "$WORKDIR/target.as.log"
        if [[ "$MODE" == "progressive-as-ar" ]]; then
            build_stage6_archiver
            stage6_archive_smoke
        fi
        ;;
esac

link_forth "$TARGET_OBJ" "$TARGET_EXE" "$WORKDIR/target.ld.log"
run_exe "$TARGET_EXE" "$WORKDIR/target.run.log"

echo "OK: stage05 pipeline ($MODE)"
echo "Input: $TARGET_SRC"
echo "Assembler path: $([[ "$MODE" == baseline ]] && echo "forth(stage01)" || echo "c(stage05)")"
echo "Linker path: forth(stage03)"
if [[ "$MODE" == "progressive-as-ar" ]]; then
    echo "Archiver smoke: c(stage06)"
fi
echo "Artifacts: $WORKDIR"
