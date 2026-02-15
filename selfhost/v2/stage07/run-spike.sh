#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
ROOT_DIR="${SELFHOST_ROOT:-$(cd "$SCRIPT_DIR/../../.." && pwd)}"
if git -C "$SCRIPT_DIR" rev-parse --show-toplevel >/dev/null 2>&1; then
    ROOT_DIR="$(git -C "$SCRIPT_DIR" rev-parse --show-toplevel)"
fi

EMU="${STAGE7_EMU:-$ROOT_DIR/tools/emulator/slow32-fast}"
KERNEL="${STAGE7_KERNEL:-$ROOT_DIR/forth/kernel.s32x}"
PRELUDE="${STAGE7_PRELUDE:-$ROOT_DIR/forth/prelude.fth}"
CC_FTH="${STAGE7_CC:-$ROOT_DIR/selfhost/v2/stage04/cc.fth}"
ASM_FTH="${STAGE7_ASM:-$ROOT_DIR/selfhost/v2/stage01/asm.fth}"
LINK_FTH="${STAGE7_LINK:-$ROOT_DIR/selfhost/v2/stage03/link.fth}"
SRC="${STAGE7_SRC:-$SCRIPT_DIR/validation/s32-ld.c}"
KEEP_ARTIFACTS=0

usage() {
    cat <<USAGE
Usage: $0 [--emu <path>] [--keep-artifacts]

Builds and spikes Stage07 linker candidate with current selfhost pipeline:
  stage04 cc.fth -> stage01 asm.fth -> stage03 link.fth
Then validates linker output by linking/running a tiny halt object.
USAGE
}

while [[ $# -gt 0 ]]; do
    case "$1" in
        --emu)
            shift
            [[ $# -gt 0 ]] || { echo "--emu requires a path" >&2; exit 2; }
            EMU="$1"
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

if [[ "$EMU" != /* ]]; then
    EMU="$ROOT_DIR/$EMU"
fi

for f in "$EMU" "$KERNEL" "$PRELUDE" "$CC_FTH" "$ASM_FTH" "$LINK_FTH" "$SRC"; do
    [[ -f "$f" ]] || { echo "Missing required file: $f" >&2; exit 1; }
done

WORKDIR="$(mktemp -d /tmp/selfhost-v2-stage07.XXXXXX)"
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
    timeout "${EXEC_TIMEOUT:-60}" "$EMU" "$exe" "$@" >"$log" 2>&1
    local rc=$?
    set -e
    if [[ "$rc" -eq 124 ]]; then
        echo "execution timed out: $exe" >&2
        tail -n 60 "$log" >&2
        return 1
    fi
    if grep -Eq "Execute fault|Memory fault|Write out of bounds or to protected memory|Unknown opcode|Unknown instruction|Load fault" "$log"; then
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

STAGE7_ASM="$WORKDIR/s32-ld.s"
STAGE7_OBJ="$WORKDIR/s32-ld.s32o"
STAGE7_EXE="$WORKDIR/s32-ld.s32x"

compile_c_stage4 "$SRC" "$STAGE7_ASM" "$WORKDIR/s32-ld.cc.log"
assemble_forth "$STAGE7_ASM" "$STAGE7_OBJ" "$WORKDIR/s32-ld.as.log"
link_forth "$STAGE7_OBJ" "$STAGE7_EXE" "$WORKDIR/s32-ld.ld.log"

cat > "$WORKDIR/main_halt.s" <<'ASM'
.text
.global main
main:
    halt
ASM

MINI_OBJ="$WORKDIR/main_halt.s32o"
MINI_EXE="$WORKDIR/main_halt.s32x"

assemble_forth "$WORKDIR/main_halt.s" "$MINI_OBJ" "$WORKDIR/main_halt.as.log"
run_exe "$STAGE7_EXE" "$WORKDIR/s32-ld.run.log" "$MINI_OBJ" "$MINI_EXE"
[[ -s "$MINI_EXE" ]] || { echo "stage07 linker produced no output" >&2; exit 1; }
run_exe "$MINI_EXE" "$WORKDIR/main_halt.run.log"

echo "OK: stage07 linker spike"
echo "Linker source: $SRC"
echo "Linker exe: $STAGE7_EXE"
echo "Spike object: $MINI_OBJ"
echo "Spike output: $MINI_EXE"
echo "Emulator: $EMU"
echo "Artifacts: $WORKDIR"
