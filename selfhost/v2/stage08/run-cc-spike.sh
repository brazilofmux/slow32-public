#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
ROOT_DIR="${SELFHOST_ROOT:-$(cd "$SCRIPT_DIR/../../.." && pwd)}"
if git -C "$SCRIPT_DIR" rev-parse --show-toplevel >/dev/null 2>&1; then
    ROOT_DIR="$(git -C "$SCRIPT_DIR" rev-parse --show-toplevel)"
fi

EMU="${STAGE8_EMU:-$ROOT_DIR/tools/emulator/slow32-fast}"
KERNEL="${STAGE8_KERNEL:-$ROOT_DIR/forth/kernel.s32x}"
PRELUDE="${STAGE8_PRELUDE:-$ROOT_DIR/forth/prelude.fth}"
CC_FTH="${STAGE8_CC_FTH:-$ROOT_DIR/selfhost/v2/stage04/cc.fth}"
ASM_FTH="${STAGE8_ASM_FTH:-$ROOT_DIR/selfhost/v2/stage01/asm.fth}"
LINK_FTH="${STAGE8_LINK_FTH:-$ROOT_DIR/selfhost/v2/stage03/link.fth}"
SRC="${STAGE8_CC_MIN_SRC:-$SCRIPT_DIR/validation/cc-min.c}"
TEST_IN="${STAGE8_TEST_IN:-$SCRIPT_DIR/tests/min_main.c}"
TEST_RET_IN="${STAGE8_TEST_RET_IN:-$SCRIPT_DIR/tests/min_ret7.c}"
TEST_EXPR_IN="${STAGE8_TEST_EXPR_IN:-$SCRIPT_DIR/tests/min_ret_expr.c}"
TEST_LOCAL_IN="${STAGE8_TEST_LOCAL_IN:-$SCRIPT_DIR/tests/min_local_ret_expr.c}"
TEST_REL_IN="${STAGE8_TEST_REL_IN:-$SCRIPT_DIR/tests/min_ret_rel.c}"
TEST_IF_TRUE_IN="${STAGE8_TEST_IF_TRUE_IN:-$SCRIPT_DIR/tests/min_if_true.c}"
TEST_IF_FALSE_IN="${STAGE8_TEST_IF_FALSE_IN:-$SCRIPT_DIR/tests/min_if_false.c}"
KEEP_ARTIFACTS=0

usage() {
    cat <<USAGE
Usage: $0 [--emu <path>] [--keep-artifacts]

Stage08 compiler spike:
  1) build cc-min.s32x via stage04->stage01->stage03
  2) build stage05 assembler (s32-as.s32x) and stage07 linker (s32-ld.s32x)
  3) compile min_main, min_ret7, min_ret_expr, min_local_ret_expr, min_ret_rel, and min_if_{true,false} with cc-min.s32x
  4) assemble with stage05; produce raw link via stage07; run via stage03 runtime link
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

for f in "$EMU" "$KERNEL" "$PRELUDE" "$CC_FTH" "$ASM_FTH" "$LINK_FTH" "$SRC" "$TEST_IN" "$TEST_RET_IN" "$TEST_EXPR_IN" "$TEST_LOCAL_IN" "$TEST_REL_IN" "$TEST_IF_TRUE_IN" "$TEST_IF_FALSE_IN"; do
    [[ -f "$f" ]] || { echo "Missing required file: $f" >&2; exit 1; }
done

WORKDIR="$(mktemp -d /tmp/selfhost-v2-stage08-cc.XXXXXX)"
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
    if grep -Eq "Execute fault|Memory fault|Write out of bounds or to protected memory|Unknown opcode|Unknown instruction|Load fault|Store fault" "$log"; then
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

run_exe_any_rc() {
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
        return 124
    fi
    if grep -Eq "Execute fault|Memory fault|Write out of bounds or to protected memory|Unknown opcode|Unknown instruction|Load fault|Store fault" "$log"; then
        echo "execution faulted: $exe" >&2
        tail -n 60 "$log" >&2
        return 125
    fi
    return "$rc"
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

# 1) Build cc-min compiler executable.
CCMIN_ASM="$WORKDIR/cc-min.s"
CCMIN_OBJ="$WORKDIR/cc-min.s32o"
CCMIN_EXE="$WORKDIR/cc-min.s32x"
compile_c_stage4 "$SRC" "$CCMIN_ASM" "$WORKDIR/cc-min.cc.log"
assemble_forth "$CCMIN_ASM" "$CCMIN_OBJ" "$WORKDIR/cc-min.as.log"
link_forth "$CCMIN_OBJ" "$CCMIN_EXE" "$WORKDIR/cc-min.ld.log"

# 2) Build Stage05 assembler and Stage07 linker executables.
PIPE_LOG="$WORKDIR/stage5-build.log"
"$ROOT_DIR/selfhost/v2/stage05/run-pipeline.sh" --mode stage6-ar-smoke --emu "$EMU" --keep-artifacts >"$PIPE_LOG"
PIPE_ART="$(awk -F': ' '/^Artifacts:/{print $2}' "$PIPE_LOG" | tail -n 1)"
[[ -n "$PIPE_ART" && -d "$PIPE_ART" ]] || { echo "failed to locate stage05 artifacts dir" >&2; exit 1; }
AS_EXE="$PIPE_ART/s32-as.s32x"
[[ -f "$AS_EXE" ]] || { echo "missing stage05 assembler exe: $AS_EXE" >&2; exit 1; }

LD_LOG="$WORKDIR/stage7-build.log"
"$ROOT_DIR/selfhost/v2/stage07/run-spike.sh" --emu "$EMU" --keep-artifacts >"$LD_LOG"
LD_EXE="$(awk -F': ' '/^Linker exe:/{print $2}' "$LD_LOG" | tail -n 1)"
[[ -n "$LD_EXE" && -f "$LD_EXE" ]] || { echo "failed to locate stage07 linker exe" >&2; exit 1; }

# 3) Use cc-min to compile minimal inputs.
GEN_ASM="$WORKDIR/min_main.generated.s"
GEN_RET_ASM="$WORKDIR/min_ret7.generated.s"
GEN_EXPR_ASM="$WORKDIR/min_ret_expr.generated.s"
GEN_LOCAL_ASM="$WORKDIR/min_local_ret_expr.generated.s"
GEN_REL_ASM="$WORKDIR/min_ret_rel.generated.s"
GEN_IF_TRUE_ASM="$WORKDIR/min_if_true.generated.s"
GEN_IF_FALSE_ASM="$WORKDIR/min_if_false.generated.s"
run_exe "$CCMIN_EXE" "$WORKDIR/cc-min.run.log" "$TEST_IN" "$GEN_ASM"
run_exe "$CCMIN_EXE" "$WORKDIR/cc-min-ret.run.log" "$TEST_RET_IN" "$GEN_RET_ASM"
run_exe "$CCMIN_EXE" "$WORKDIR/cc-min-expr.run.log" "$TEST_EXPR_IN" "$GEN_EXPR_ASM"
run_exe "$CCMIN_EXE" "$WORKDIR/cc-min-local.run.log" "$TEST_LOCAL_IN" "$GEN_LOCAL_ASM"
run_exe "$CCMIN_EXE" "$WORKDIR/cc-min-rel.run.log" "$TEST_REL_IN" "$GEN_REL_ASM"
run_exe "$CCMIN_EXE" "$WORKDIR/cc-min-if-true.run.log" "$TEST_IF_TRUE_IN" "$GEN_IF_TRUE_ASM"
run_exe "$CCMIN_EXE" "$WORKDIR/cc-min-if-false.run.log" "$TEST_IF_FALSE_IN" "$GEN_IF_FALSE_ASM"
[[ -s "$GEN_ASM" ]] || { echo "cc-min produced no assembly output" >&2; exit 1; }
[[ -s "$GEN_RET_ASM" ]] || { echo "cc-min produced no return-test assembly output" >&2; exit 1; }
[[ -s "$GEN_EXPR_ASM" ]] || { echo "cc-min produced no expr-test assembly output" >&2; exit 1; }
[[ -s "$GEN_LOCAL_ASM" ]] || { echo "cc-min produced no local-test assembly output" >&2; exit 1; }
[[ -s "$GEN_REL_ASM" ]] || { echo "cc-min produced no relational-test assembly output" >&2; exit 1; }
[[ -s "$GEN_IF_TRUE_ASM" ]] || { echo "cc-min produced no if-true assembly output" >&2; exit 1; }
[[ -s "$GEN_IF_FALSE_ASM" ]] || { echo "cc-min produced no if-false assembly output" >&2; exit 1; }
grep -q '^main:' "$GEN_ASM" || { echo "generated assembly missing main label" >&2; exit 1; }
grep -q 'addi r1, r0, 7' "$GEN_RET_ASM" || { echo "generated return-test assembly missing return immediate" >&2; exit 1; }
grep -q 'addi r1, r0, 14' "$GEN_EXPR_ASM" || { echo "generated expr-test assembly missing expected immediate" >&2; exit 1; }
grep -q 'addi r1, r0, 14' "$GEN_LOCAL_ASM" || { echo "generated local-test assembly missing expected immediate" >&2; exit 1; }
grep -q 'addi r1, r0, 4' "$GEN_REL_ASM" || { echo "generated relational-test assembly missing expected immediate" >&2; exit 1; }
grep -q 'addi r1, r0, 9' "$GEN_IF_TRUE_ASM" || { echo "generated if-true assembly missing expected immediate" >&2; exit 1; }
grep -q 'addi r1, r0, 4' "$GEN_IF_FALSE_ASM" || { echo "generated if-false assembly missing expected immediate" >&2; exit 1; }

# 4) Assemble, link with stage07 (artifact), then link/run with stage03 runtime.
GEN_OBJ="$WORKDIR/min_main.generated.s32o"
GEN_RAW_EXE="$WORKDIR/min_main.generated.raw.s32x"
GEN_EXE="$WORKDIR/min_main.generated.s32x"
GEN_RET_OBJ="$WORKDIR/min_ret7.generated.s32o"
GEN_RET_RAW_EXE="$WORKDIR/min_ret7.generated.raw.s32x"
GEN_RET_EXE="$WORKDIR/min_ret7.generated.s32x"
GEN_EXPR_OBJ="$WORKDIR/min_ret_expr.generated.s32o"
GEN_EXPR_RAW_EXE="$WORKDIR/min_ret_expr.generated.raw.s32x"
GEN_EXPR_EXE="$WORKDIR/min_ret_expr.generated.s32x"
GEN_LOCAL_OBJ="$WORKDIR/min_local_ret_expr.generated.s32o"
GEN_LOCAL_RAW_EXE="$WORKDIR/min_local_ret_expr.generated.raw.s32x"
GEN_LOCAL_EXE="$WORKDIR/min_local_ret_expr.generated.s32x"
GEN_REL_OBJ="$WORKDIR/min_ret_rel.generated.s32o"
GEN_REL_RAW_EXE="$WORKDIR/min_ret_rel.generated.raw.s32x"
GEN_REL_EXE="$WORKDIR/min_ret_rel.generated.s32x"
GEN_IF_TRUE_OBJ="$WORKDIR/min_if_true.generated.s32o"
GEN_IF_TRUE_RAW_EXE="$WORKDIR/min_if_true.generated.raw.s32x"
GEN_IF_TRUE_EXE="$WORKDIR/min_if_true.generated.s32x"
GEN_IF_FALSE_OBJ="$WORKDIR/min_if_false.generated.s32o"
GEN_IF_FALSE_RAW_EXE="$WORKDIR/min_if_false.generated.raw.s32x"
GEN_IF_FALSE_EXE="$WORKDIR/min_if_false.generated.s32x"
run_exe "$AS_EXE" "$WORKDIR/stage5-as.run.log" "$GEN_ASM" "$GEN_OBJ"
run_exe "$AS_EXE" "$WORKDIR/stage5-as-ret.run.log" "$GEN_RET_ASM" "$GEN_RET_OBJ"
run_exe "$AS_EXE" "$WORKDIR/stage5-as-expr.run.log" "$GEN_EXPR_ASM" "$GEN_EXPR_OBJ"
run_exe "$AS_EXE" "$WORKDIR/stage5-as-local.run.log" "$GEN_LOCAL_ASM" "$GEN_LOCAL_OBJ"
run_exe "$AS_EXE" "$WORKDIR/stage5-as-rel.run.log" "$GEN_REL_ASM" "$GEN_REL_OBJ"
run_exe "$AS_EXE" "$WORKDIR/stage5-as-if-true.run.log" "$GEN_IF_TRUE_ASM" "$GEN_IF_TRUE_OBJ"
run_exe "$AS_EXE" "$WORKDIR/stage5-as-if-false.run.log" "$GEN_IF_FALSE_ASM" "$GEN_IF_FALSE_OBJ"
[[ -s "$GEN_OBJ" ]] || { echo "stage05 assembler produced no object output" >&2; exit 1; }
[[ -s "$GEN_RET_OBJ" ]] || { echo "stage05 assembler produced no return-test object output" >&2; exit 1; }
[[ -s "$GEN_EXPR_OBJ" ]] || { echo "stage05 assembler produced no expr-test object output" >&2; exit 1; }
[[ -s "$GEN_LOCAL_OBJ" ]] || { echo "stage05 assembler produced no local-test object output" >&2; exit 1; }
[[ -s "$GEN_REL_OBJ" ]] || { echo "stage05 assembler produced no relational-test object output" >&2; exit 1; }
[[ -s "$GEN_IF_TRUE_OBJ" ]] || { echo "stage05 assembler produced no if-true object output" >&2; exit 1; }
[[ -s "$GEN_IF_FALSE_OBJ" ]] || { echo "stage05 assembler produced no if-false object output" >&2; exit 1; }
run_exe "$LD_EXE" "$WORKDIR/stage7-ld.run.log" "$GEN_OBJ" "$GEN_RAW_EXE"
run_exe "$LD_EXE" "$WORKDIR/stage7-ld-ret.run.log" "$GEN_RET_OBJ" "$GEN_RET_RAW_EXE"
run_exe "$LD_EXE" "$WORKDIR/stage7-ld-expr.run.log" "$GEN_EXPR_OBJ" "$GEN_EXPR_RAW_EXE"
run_exe "$LD_EXE" "$WORKDIR/stage7-ld-local.run.log" "$GEN_LOCAL_OBJ" "$GEN_LOCAL_RAW_EXE"
run_exe "$LD_EXE" "$WORKDIR/stage7-ld-rel.run.log" "$GEN_REL_OBJ" "$GEN_REL_RAW_EXE"
run_exe "$LD_EXE" "$WORKDIR/stage7-ld-if-true.run.log" "$GEN_IF_TRUE_OBJ" "$GEN_IF_TRUE_RAW_EXE"
run_exe "$LD_EXE" "$WORKDIR/stage7-ld-if-false.run.log" "$GEN_IF_FALSE_OBJ" "$GEN_IF_FALSE_RAW_EXE"
[[ -s "$GEN_RAW_EXE" ]] || { echo "stage07 linker produced no executable output" >&2; exit 1; }
[[ -s "$GEN_RET_RAW_EXE" ]] || { echo "stage07 linker produced no return-test executable output" >&2; exit 1; }
[[ -s "$GEN_EXPR_RAW_EXE" ]] || { echo "stage07 linker produced no expr-test executable output" >&2; exit 1; }
[[ -s "$GEN_LOCAL_RAW_EXE" ]] || { echo "stage07 linker produced no local-test executable output" >&2; exit 1; }
[[ -s "$GEN_REL_RAW_EXE" ]] || { echo "stage07 linker produced no relational-test executable output" >&2; exit 1; }
[[ -s "$GEN_IF_TRUE_RAW_EXE" ]] || { echo "stage07 linker produced no if-true executable output" >&2; exit 1; }
[[ -s "$GEN_IF_FALSE_RAW_EXE" ]] || { echo "stage07 linker produced no if-false executable output" >&2; exit 1; }
link_forth "$GEN_OBJ" "$GEN_EXE" "$WORKDIR/stage3-link.run.log"
link_forth "$GEN_RET_OBJ" "$GEN_RET_EXE" "$WORKDIR/stage3-link-ret.run.log"
link_forth "$GEN_EXPR_OBJ" "$GEN_EXPR_EXE" "$WORKDIR/stage3-link-expr.run.log"
link_forth "$GEN_LOCAL_OBJ" "$GEN_LOCAL_EXE" "$WORKDIR/stage3-link-local.run.log"
link_forth "$GEN_REL_OBJ" "$GEN_REL_EXE" "$WORKDIR/stage3-link-rel.run.log"
link_forth "$GEN_IF_TRUE_OBJ" "$GEN_IF_TRUE_EXE" "$WORKDIR/stage3-link-if-true.run.log"
link_forth "$GEN_IF_FALSE_OBJ" "$GEN_IF_FALSE_EXE" "$WORKDIR/stage3-link-if-false.run.log"
run_exe "$GEN_EXE" "$WORKDIR/gen.run.log"
RET_RC=0
run_exe_any_rc "$GEN_RET_EXE" "$WORKDIR/gen-ret.run.log" || RET_RC=$?
if [[ "$RET_RC" -ne 7 ]]; then
    echo "return-test executable had unexpected exit code: $RET_RC (expected 7)" >&2
    tail -n 60 "$WORKDIR/gen-ret.run.log" >&2
    exit 1
fi
EXPR_RC=0
run_exe_any_rc "$GEN_EXPR_EXE" "$WORKDIR/gen-expr.run.log" || EXPR_RC=$?
if [[ "$EXPR_RC" -ne 14 ]]; then
    echo "expr-test executable had unexpected exit code: $EXPR_RC (expected 14)" >&2
    tail -n 60 "$WORKDIR/gen-expr.run.log" >&2
    exit 1
fi
LOCAL_RC=0
run_exe_any_rc "$GEN_LOCAL_EXE" "$WORKDIR/gen-local.run.log" || LOCAL_RC=$?
if [[ "$LOCAL_RC" -ne 14 ]]; then
    echo "local-test executable had unexpected exit code: $LOCAL_RC (expected 14)" >&2
    tail -n 60 "$WORKDIR/gen-local.run.log" >&2
    exit 1
fi
REL_RC=0
run_exe_any_rc "$GEN_REL_EXE" "$WORKDIR/gen-rel.run.log" || REL_RC=$?
if [[ "$REL_RC" -ne 4 ]]; then
    echo "relational-test executable had unexpected exit code: $REL_RC (expected 4)" >&2
    tail -n 60 "$WORKDIR/gen-rel.run.log" >&2
    exit 1
fi
IF_TRUE_RC=0
run_exe_any_rc "$GEN_IF_TRUE_EXE" "$WORKDIR/gen-if-true.run.log" || IF_TRUE_RC=$?
if [[ "$IF_TRUE_RC" -ne 9 ]]; then
    echo "if-true test executable had unexpected exit code: $IF_TRUE_RC (expected 9)" >&2
    tail -n 60 "$WORKDIR/gen-if-true.run.log" >&2
    exit 1
fi
IF_FALSE_RC=0
run_exe_any_rc "$GEN_IF_FALSE_EXE" "$WORKDIR/gen-if-false.run.log" || IF_FALSE_RC=$?
if [[ "$IF_FALSE_RC" -ne 4 ]]; then
    echo "if-false test executable had unexpected exit code: $IF_FALSE_RC (expected 4)" >&2
    tail -n 60 "$WORKDIR/gen-if-false.run.log" >&2
    exit 1
fi

echo "OK: stage08 cc-min spike"
echo "Compiler source: $SRC"
echo "Compiler exe: $CCMIN_EXE"
echo "Assembler exe: $AS_EXE"
echo "Linker exe: $LD_EXE"
echo "Input C: $TEST_IN"
echo "Return-test C: $TEST_RET_IN"
echo "Expr-test C: $TEST_EXPR_IN"
echo "Local-test C: $TEST_LOCAL_IN"
echo "Relational-test C: $TEST_REL_IN"
echo "If-true C: $TEST_IF_TRUE_IN"
echo "If-false C: $TEST_IF_FALSE_IN"
echo "Generated asm: $GEN_ASM"
echo "Generated return asm: $GEN_RET_ASM"
echo "Generated expr asm: $GEN_EXPR_ASM"
echo "Generated local asm: $GEN_LOCAL_ASM"
echo "Generated relational asm: $GEN_REL_ASM"
echo "Generated if-true asm: $GEN_IF_TRUE_ASM"
echo "Generated if-false asm: $GEN_IF_FALSE_ASM"
echo "Generated raw exe (stage07): $GEN_RAW_EXE"
echo "Generated return raw exe (stage07): $GEN_RET_RAW_EXE"
echo "Generated expr raw exe (stage07): $GEN_EXPR_RAW_EXE"
echo "Generated local raw exe (stage07): $GEN_LOCAL_RAW_EXE"
echo "Generated relational raw exe (stage07): $GEN_REL_RAW_EXE"
echo "Generated if-true raw exe (stage07): $GEN_IF_TRUE_RAW_EXE"
echo "Generated if-false raw exe (stage07): $GEN_IF_FALSE_RAW_EXE"
echo "Generated exe: $GEN_EXE"
echo "Generated return exe: $GEN_RET_EXE"
echo "Generated expr exe: $GEN_EXPR_EXE"
echo "Generated local exe: $GEN_LOCAL_EXE"
echo "Generated relational exe: $GEN_REL_EXE"
echo "Generated if-true exe: $GEN_IF_TRUE_EXE"
echo "Generated if-false exe: $GEN_IF_FALSE_EXE"
echo "Emulator: $EMU"
echo "Artifacts: $WORKDIR"
