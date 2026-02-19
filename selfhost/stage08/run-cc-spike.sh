#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
SELFHOST_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"
ROOT_DIR="$(cd "$SELFHOST_DIR/.." && pwd)"

EMU="${STAGE8_EMU:-}"
EMU_EXPLICIT=0
KERNEL="${STAGE8_KERNEL:-$ROOT_DIR/forth/kernel.s32x}"
PRELUDE="${STAGE8_PRELUDE:-$ROOT_DIR/forth/prelude.fth}"
CC_FTH="${STAGE8_CC_FTH:-$SELFHOST_DIR/stage01/cc.fth}"
LINK_FTH="${STAGE8_LINK_FTH:-$SELFHOST_DIR/stage01/link.fth}"

CRT0_SRC="$SELFHOST_DIR/stage05/crt0.s"
MMIO_SRC="$SELFHOST_DIR/stage05/mmio.s"
LIBC_DIR="$SELFHOST_DIR/stage05/libc"
SRC="${STAGE8_CC_MIN_SRC:-$SCRIPT_DIR/cc-min.c}"
SRC_PASS1="${STAGE8_CC_MIN_PASS1_SRC:-$SCRIPT_DIR/cc-min-pass1.c}"
SRC_PASS2="${STAGE8_CC_MIN_PASS2_SRC:-$SCRIPT_DIR/cc-min-pass2.c}"
SRC_PASS3="${STAGE8_CC_MIN_PASS3_SRC:-$SCRIPT_DIR/cc-min-pass3.c}"
TEST_IN="${STAGE8_TEST_IN:-$SCRIPT_DIR/tests/min_main.c}"
TEST_RET_IN="${STAGE8_TEST_RET_IN:-$SCRIPT_DIR/tests/min_ret7.c}"
TEST_EXPR_IN="${STAGE8_TEST_EXPR_IN:-$SCRIPT_DIR/tests/min_ret_expr.c}"
TEST_LOCAL_IN="${STAGE8_TEST_LOCAL_IN:-$SCRIPT_DIR/tests/min_local_ret_expr.c}"
TEST_REL_IN="${STAGE8_TEST_REL_IN:-$SCRIPT_DIR/tests/min_ret_rel.c}"
TEST_IF_TRUE_IN="${STAGE8_TEST_IF_TRUE_IN:-$SCRIPT_DIR/tests/min_if_true.c}"
TEST_IF_FALSE_IN="${STAGE8_TEST_IF_FALSE_IN:-$SCRIPT_DIR/tests/min_if_false.c}"
TEST_WHILE_IN="${STAGE8_TEST_WHILE_IN:-$SCRIPT_DIR/tests/min_while_countdown.c}"
TEST_TWO_LOCALS_IN="${STAGE8_TEST_TWO_LOCALS_IN:-$SCRIPT_DIR/tests/min_two_locals.c}"
TEST_HELPER_IN="${STAGE8_TEST_HELPER_IN:-$SCRIPT_DIR/tests/min_helper_call.c}"
TEST_HELPER_ARG_IN="${STAGE8_TEST_HELPER_ARG_IN:-$SCRIPT_DIR/tests/min_helper_arg.c}"
TEST_HELPER_LOCAL_IN="${STAGE8_TEST_HELPER_LOCAL_IN:-$SCRIPT_DIR/tests/min_helper_local.c}"
TEST_MAIN_LOCAL_HELPER_IN="${STAGE8_TEST_MAIN_LOCAL_HELPER_IN:-$SCRIPT_DIR/tests/min_main_local_helper.c}"
TEST_HELPER_TWO_ARGS_IN="${STAGE8_TEST_HELPER_TWO_ARGS_IN:-$SCRIPT_DIR/tests/min_helper_two_args.c}"
TEST_HELPER_TWO_ARGS_IF_IN="${STAGE8_TEST_HELPER_TWO_ARGS_IF_IN:-$SCRIPT_DIR/tests/min_helper_two_args_if.c}"
TEST_MULTI_FUNC_IN="$SCRIPT_DIR/tests/min_multi_func.c"
TEST_FOR_LOOP_IN="$SCRIPT_DIR/tests/min_for_loop.c"
TEST_NESTED_IF_IN="$SCRIPT_DIR/tests/min_nested_if.c"
TEST_BREAK_CONTINUE_IN="$SCRIPT_DIR/tests/min_break_continue.c"
TEST_GENERAL_NAMES_IN="$SCRIPT_DIR/tests/min_general_names.c"
TEST_COMPLEX_EXPR_IN="$SCRIPT_DIR/tests/min_complex_expr.c"
TEST_CHAR_TYPE_IN="$SCRIPT_DIR/tests/min_char_type.c"
TEST_CHAR_LITERAL_IN="$SCRIPT_DIR/tests/min_char_literal.c"
TEST_LOCAL_ARRAY_IN="$SCRIPT_DIR/tests/min_local_array.c"
TEST_CHAR_ARRAY_IN="$SCRIPT_DIR/tests/min_char_array.c"
TEST_STRING_LIT_IN="$SCRIPT_DIR/tests/min_string_lit.c"
TEST_POINTER_IN="$SCRIPT_DIR/tests/min_pointer.c"
TEST_GLOBAL_IN="$SCRIPT_DIR/tests/min_global.c"
TEST_GLOBAL_ARRAY_IN="$SCRIPT_DIR/tests/min_global_array.c"
TEST_PTR_ARITH_IN="$SCRIPT_DIR/tests/min_ptr_arith.c"
TEST_SHORT_CIRCUIT_IN="$SCRIPT_DIR/tests/min_short_circuit.c"
TEST_TYPEDEF_IN="$SCRIPT_DIR/tests/min_typedef.c"
TEST_TYPEDEF_STRUCT_IN="$SCRIPT_DIR/tests/min_typedef_struct.c"
TEST_STRUCT_BASIC_IN="$SCRIPT_DIR/tests/min_struct_basic.c"
TEST_STRUCT_ARROW_IN="$SCRIPT_DIR/tests/min_struct_arrow.c"
TEST_STRUCT_ARRAY_IN="$SCRIPT_DIR/tests/min_struct_array.c"
TEST_SIZEOF_IN="$SCRIPT_DIR/tests/min_sizeof.c"
TEST_SIZEOF_STRUCT_IN="$SCRIPT_DIR/tests/min_sizeof_struct.c"
TEST_CAST_IN="$SCRIPT_DIR/tests/min_cast.c"
TEST_DEFINE_IN="$SCRIPT_DIR/tests/min_define.c"
TEST_DEFINE_HEX_IN="$SCRIPT_DIR/tests/min_define_hex.c"
TEST_ENUM_IN="$SCRIPT_DIR/tests/min_enum.c"
TEST_PROTOTYPE_IN="$SCRIPT_DIR/tests/min_prototype.c"
TEST_INCLUDE_IN="$SCRIPT_DIR/tests/min_include.c"
TEST_INCLUDE_HELPER_IN="$SCRIPT_DIR/tests/min_include_helper.h"
KEEP_ARTIFACTS=0
REBUILD_LIBC="${STAGE8_REBUILD_LIBC:-0}"

choose_default_emu() {
    printf '%s\n' "$SELFHOST_DIR/stage00/s32-emu"
}

usage() {
    cat <<USAGE
Usage: $0 [--emu <path>] [--keep-artifacts]

Stage08 compiler spike:
  1) bootstrap stage05 assembler + stage06 archiver + stage07 linker
  2) build cc-min.s32x via stage01->stage05->stage01
  3) compile min_main, min_ret7, min_ret_expr, min_local_ret_expr, min_ret_rel, min_if_{true,false}, min_while_countdown, min_two_locals, min_helper_call, min_helper_arg, min_helper_local, min_main_local_helper, min_helper_two_args, and min_helper_two_args_if with cc-min.s32x
  4) assemble with stage05; produce raw link via stage07; run via stage01 runtime link
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

if [[ "$EMU_EXPLICIT" -eq 0 && -z "${STAGE8_EMU:-}" ]]; then
    EMU="$(choose_default_emu)"
fi

for f in "$EMU" "$KERNEL" "$PRELUDE" "$CC_FTH" "$LINK_FTH" \
         "$SRC_PASS1" "$SRC_PASS2" "$SRC_PASS3" "$TEST_IN" "$TEST_RET_IN" "$TEST_EXPR_IN" "$TEST_LOCAL_IN" "$TEST_REL_IN" "$TEST_IF_TRUE_IN" "$TEST_IF_FALSE_IN" "$TEST_WHILE_IN" "$TEST_TWO_LOCALS_IN" "$TEST_HELPER_IN" "$TEST_HELPER_ARG_IN" "$TEST_HELPER_LOCAL_IN" "$TEST_MAIN_LOCAL_HELPER_IN" "$TEST_HELPER_TWO_ARGS_IN" "$TEST_HELPER_TWO_ARGS_IF_IN" \
         "$TEST_MULTI_FUNC_IN" "$TEST_FOR_LOOP_IN" "$TEST_NESTED_IF_IN" "$TEST_BREAK_CONTINUE_IN" "$TEST_GENERAL_NAMES_IN" "$TEST_COMPLEX_EXPR_IN" \
         "$TEST_CHAR_TYPE_IN" "$TEST_CHAR_LITERAL_IN" "$TEST_LOCAL_ARRAY_IN" "$TEST_CHAR_ARRAY_IN" "$TEST_STRING_LIT_IN" "$TEST_POINTER_IN" "$TEST_GLOBAL_IN" "$TEST_GLOBAL_ARRAY_IN" "$TEST_PTR_ARITH_IN" "$TEST_SHORT_CIRCUIT_IN" \
         "$TEST_TYPEDEF_IN" "$TEST_TYPEDEF_STRUCT_IN" "$TEST_STRUCT_BASIC_IN" "$TEST_STRUCT_ARROW_IN" "$TEST_STRUCT_ARRAY_IN" "$TEST_SIZEOF_IN" "$TEST_SIZEOF_STRUCT_IN" "$TEST_CAST_IN" \
         "$TEST_DEFINE_IN" "$TEST_DEFINE_HEX_IN" "$TEST_ENUM_IN" "$TEST_PROTOTYPE_IN" "$TEST_INCLUDE_IN" "$TEST_INCLUDE_HELPER_IN"; do
    [[ -f "$f" ]] || { echo "Missing required file: $f" >&2; exit 1; }
done

if [[ ! -f "$SRC" ]]; then
    # Backward-compatible fallback during path transition.
    SRC="$SCRIPT_DIR/validation/cc-min.c"
fi
[[ -f "$SRC" ]] || { echo "Missing required file: $SRC" >&2; exit 1; }

WORKDIR="$(mktemp -d /tmp/selfhost-v2-stage08-cc.XXXXXX)"
if [[ "$KEEP_ARTIFACTS" -eq 0 ]]; then
    trap 'rm -rf "$WORKDIR"' EXIT
fi

# cc.fth uses relative include path "selfhost/stage01/include/" — run from repo root
cd "$ROOT_DIR"

# Bootstrap strict C-toolchain stages first. Stage08 then uses these outputs.
PIPE_LOG="$WORKDIR/stage5-build.log"
"$SELFHOST_DIR/stage05/run-pipeline.sh" --mode stage6-ar-smoke --emu "$EMU" --keep-artifacts >"$PIPE_LOG"
PIPE_ART="$(awk -F': ' '/^Artifacts:/{print $2}' "$PIPE_LOG" | tail -n 1)"
[[ -n "$PIPE_ART" && -d "$PIPE_ART" ]] || { echo "failed to locate stage05 artifacts dir" >&2; exit 1; }
AS_EXE="$PIPE_ART/s32-as.s32x"
AR_EXE="$PIPE_ART/s32-ar.s32x"
[[ -f "$AS_EXE" ]] || { echo "missing stage05 assembler exe: $AS_EXE" >&2; exit 1; }
[[ -f "$AR_EXE" ]] || { echo "missing stage06 archiver exe: $AR_EXE" >&2; exit 1; }

LD_LOG="$WORKDIR/stage7-build.log"
"$SELFHOST_DIR/stage07/run-spike.sh" --emu "$EMU" --keep-artifacts >"$LD_LOG"
LD_EXE="$(awk -F': ' '/^Linker exe:/{print $2}' "$LD_LOG" | tail -n 1)"
[[ -n "$LD_EXE" && -f "$LD_EXE" ]] || { echo "failed to locate stage07 linker exe" >&2; exit 1; }

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
    if grep -Eq "Execute fault|Memory fault|Write out of bounds or to protected memory|Unknown opcode|Unknown instruction|Load fault|Store fault|Execution limit reached" "$log"; then
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

assemble_with_stage5() {
    local asm="$1"
    local obj="$2"
    local log="$3"

    run_exe "$AS_EXE" "$log" "$asm" "$obj"
    [[ -s "$obj" ]] || { echo "stage05 assembler produced no output: $asm" >&2; return 1; }
}

# --- Runtime objects come from stage05 bootstrap artifacts ---
RUNTIME_CRT0="$PIPE_ART/crt0_minimal.s32o"
RUNTIME_MMIO_OBJ="$PIPE_ART/mmio_minimal.s32o"
RUNTIME_MMIO_NO_START_OBJ="$PIPE_ART/mmio_no_start.s32o"
[[ -s "$RUNTIME_CRT0" ]] || { echo "missing runtime crt0 object: $RUNTIME_CRT0" >&2; exit 1; }
[[ -s "$RUNTIME_MMIO_OBJ" ]] || { echo "missing runtime mmio object: $RUNTIME_MMIO_OBJ" >&2; exit 1; }
[[ -s "$RUNTIME_MMIO_NO_START_OBJ" ]] || { echo "missing runtime mmio (no start) object: $RUNTIME_MMIO_NO_START_OBJ" >&2; exit 1; }

# --- Build selfhost libc ---
LIBC_ARCHIVE="$PIPE_ART/libc_selfhost.s32a"
LIBC_START_OBJ="$PIPE_ART/libc_start.s32o"
[[ -s "$LIBC_ARCHIVE" ]] || { echo "missing bootstrap libc archive: $LIBC_ARCHIVE" >&2; exit 1; }
[[ -s "$LIBC_START_OBJ" ]] || { echo "missing bootstrap libc start object: $LIBC_START_OBJ" >&2; exit 1; }

build_selfhost_libc() {
    local libc_c_files="string_extra convert stdio start"
    local name src asm obj
    local ar_objs=()

    echo "Building selfhost libc..."

    for name in $libc_c_files; do
        src="$LIBC_DIR/${name}.c"
        asm="$WORKDIR/libc_${name}.s"
        obj="$WORKDIR/libc_${name}.s32o"

        [[ -f "$src" ]] || { echo "Missing libc source: $src" >&2; return 1; }
        compile_c_stage4 "$src" "$asm" "$WORKDIR/libc_${name}.cc.log"
        assemble_with_stage5 "$asm" "$obj" "$WORKDIR/libc_${name}.as.log"

        if [[ "$name" == "start" ]]; then
            LIBC_START_OBJ="$obj"
        else
            ar_objs+=("$obj")
        fi
    done

    run_exe "$AR_EXE" "$WORKDIR/libc.ar.log" "c" "$LIBC_ARCHIVE" "${ar_objs[@]}"
    [[ -s "$LIBC_ARCHIVE" ]] || { echo "failed to build libc archive" >&2; return 1; }
    echo "Libc archive: $LIBC_ARCHIVE"
}

if [[ "$REBUILD_LIBC" -eq 1 ]]; then
    LIBC_ARCHIVE="$WORKDIR/libc_selfhost.s32a"
    LIBC_START_OBJ=""
    build_selfhost_libc
fi

link_forth_with_libc() {
    local obj="$1"
    local exe="$2"
    local log="$3"

    run_forth "$LINK_FTH" /dev/null "LINK-INIT
S\" $RUNTIME_CRT0\" LINK-OBJ
S\" $obj\" LINK-OBJ
S\" $LIBC_START_OBJ\" LINK-OBJ
S\" $RUNTIME_MMIO_NO_START_OBJ\" LINK-OBJ
65536 LINK-MMIO
S\" $LIBC_ARCHIVE\" LINK-ARCHIVE
S\" $exe\" LINK-EMIT
BYE" "$log"
    [[ -s "$exe" ]] || { echo "linker produced no output: $obj" >&2; return 1; }
}

# 1) Build cc-min compiler executable.
# Keep pass sources split on disk, but concatenate for now so Stage08 can
# run on the current subset-C toolchain without multi-TU linkage pitfalls.
CCMIN_MERGED_SRC="$WORKDIR/cc-min.merged.c"
cat "$SRC_PASS1" "$SRC_PASS2" "$SRC_PASS3" "$SRC" > "$CCMIN_MERGED_SRC"

CCMIN_ASM="$WORKDIR/cc-min.s"
CCMIN_OBJ="$WORKDIR/cc-min-main.s32o"
CCMIN_EXE="$WORKDIR/cc-min.s32x"
compile_c_stage4 "$CCMIN_MERGED_SRC" "$CCMIN_ASM" "$WORKDIR/cc-min.cc.log"
assemble_with_stage5 "$CCMIN_ASM" "$CCMIN_OBJ" "$WORKDIR/cc-min.as.log"
link_forth_with_libc "$CCMIN_OBJ" "$CCMIN_EXE" "$WORKDIR/cc-min.ld.log"

# 3) Use cc-min to compile minimal inputs.
GEN_ASM="$WORKDIR/min_main.generated.s"
GEN_RET_ASM="$WORKDIR/min_ret7.generated.s"
GEN_EXPR_ASM="$WORKDIR/min_ret_expr.generated.s"
GEN_LOCAL_ASM="$WORKDIR/min_local_ret_expr.generated.s"
GEN_REL_ASM="$WORKDIR/min_ret_rel.generated.s"
GEN_IF_TRUE_ASM="$WORKDIR/min_if_true.generated.s"
GEN_IF_FALSE_ASM="$WORKDIR/min_if_false.generated.s"
GEN_WHILE_ASM="$WORKDIR/min_while_countdown.generated.s"
GEN_TWO_LOCALS_ASM="$WORKDIR/min_two_locals.generated.s"
GEN_HELPER_ASM="$WORKDIR/min_helper_call.generated.s"
GEN_HELPER_ARG_ASM="$WORKDIR/min_helper_arg.generated.s"
GEN_HELPER_LOCAL_ASM="$WORKDIR/min_helper_local.generated.s"
GEN_MAIN_LOCAL_HELPER_ASM="$WORKDIR/min_main_local_helper.generated.s"
GEN_HELPER_TWO_ARGS_ASM="$WORKDIR/min_helper_two_args.generated.s"
GEN_HELPER_TWO_ARGS_IF_ASM="$WORKDIR/min_helper_two_args_if.generated.s"
run_exe "$CCMIN_EXE" "$WORKDIR/cc-min.run.log" "$TEST_IN" "$GEN_ASM"
run_exe "$CCMIN_EXE" "$WORKDIR/cc-min-ret.run.log" "$TEST_RET_IN" "$GEN_RET_ASM"
run_exe "$CCMIN_EXE" "$WORKDIR/cc-min-expr.run.log" "$TEST_EXPR_IN" "$GEN_EXPR_ASM"
run_exe "$CCMIN_EXE" "$WORKDIR/cc-min-local.run.log" "$TEST_LOCAL_IN" "$GEN_LOCAL_ASM"
run_exe "$CCMIN_EXE" "$WORKDIR/cc-min-rel.run.log" "$TEST_REL_IN" "$GEN_REL_ASM"
run_exe "$CCMIN_EXE" "$WORKDIR/cc-min-if-true.run.log" "$TEST_IF_TRUE_IN" "$GEN_IF_TRUE_ASM"
run_exe "$CCMIN_EXE" "$WORKDIR/cc-min-if-false.run.log" "$TEST_IF_FALSE_IN" "$GEN_IF_FALSE_ASM"
run_exe "$CCMIN_EXE" "$WORKDIR/cc-min-while.run.log" "$TEST_WHILE_IN" "$GEN_WHILE_ASM"
run_exe "$CCMIN_EXE" "$WORKDIR/cc-min-two-locals.run.log" "$TEST_TWO_LOCALS_IN" "$GEN_TWO_LOCALS_ASM"
run_exe "$CCMIN_EXE" "$WORKDIR/cc-min-helper.run.log" "$TEST_HELPER_IN" "$GEN_HELPER_ASM"
run_exe "$CCMIN_EXE" "$WORKDIR/cc-min-helper-arg.run.log" "$TEST_HELPER_ARG_IN" "$GEN_HELPER_ARG_ASM"
run_exe "$CCMIN_EXE" "$WORKDIR/cc-min-helper-local.run.log" "$TEST_HELPER_LOCAL_IN" "$GEN_HELPER_LOCAL_ASM"
run_exe "$CCMIN_EXE" "$WORKDIR/cc-min-main-local-helper.run.log" "$TEST_MAIN_LOCAL_HELPER_IN" "$GEN_MAIN_LOCAL_HELPER_ASM"
run_exe "$CCMIN_EXE" "$WORKDIR/cc-min-helper-two-args.run.log" "$TEST_HELPER_TWO_ARGS_IN" "$GEN_HELPER_TWO_ARGS_ASM"
run_exe "$CCMIN_EXE" "$WORKDIR/cc-min-helper-two-args-if.run.log" "$TEST_HELPER_TWO_ARGS_IF_IN" "$GEN_HELPER_TWO_ARGS_IF_ASM"
GEN_MULTI_FUNC_ASM="$WORKDIR/min_multi_func.generated.s"
GEN_FOR_LOOP_ASM="$WORKDIR/min_for_loop.generated.s"
GEN_NESTED_IF_ASM="$WORKDIR/min_nested_if.generated.s"
GEN_BREAK_CONTINUE_ASM="$WORKDIR/min_break_continue.generated.s"
GEN_GENERAL_NAMES_ASM="$WORKDIR/min_general_names.generated.s"
GEN_COMPLEX_EXPR_ASM="$WORKDIR/min_complex_expr.generated.s"
run_exe "$CCMIN_EXE" "$WORKDIR/cc-min-multi-func.run.log" "$TEST_MULTI_FUNC_IN" "$GEN_MULTI_FUNC_ASM"
run_exe "$CCMIN_EXE" "$WORKDIR/cc-min-for-loop.run.log" "$TEST_FOR_LOOP_IN" "$GEN_FOR_LOOP_ASM"
run_exe "$CCMIN_EXE" "$WORKDIR/cc-min-nested-if.run.log" "$TEST_NESTED_IF_IN" "$GEN_NESTED_IF_ASM"
run_exe "$CCMIN_EXE" "$WORKDIR/cc-min-break-continue.run.log" "$TEST_BREAK_CONTINUE_IN" "$GEN_BREAK_CONTINUE_ASM"
run_exe "$CCMIN_EXE" "$WORKDIR/cc-min-general-names.run.log" "$TEST_GENERAL_NAMES_IN" "$GEN_GENERAL_NAMES_ASM"
run_exe "$CCMIN_EXE" "$WORKDIR/cc-min-complex-expr.run.log" "$TEST_COMPLEX_EXPR_IN" "$GEN_COMPLEX_EXPR_ASM"
GEN_CHAR_TYPE_ASM="$WORKDIR/min_char_type.generated.s"
GEN_CHAR_LITERAL_ASM="$WORKDIR/min_char_literal.generated.s"
GEN_LOCAL_ARRAY_ASM="$WORKDIR/min_local_array.generated.s"
GEN_CHAR_ARRAY_ASM="$WORKDIR/min_char_array.generated.s"
GEN_STRING_LIT_ASM="$WORKDIR/min_string_lit.generated.s"
GEN_POINTER_ASM="$WORKDIR/min_pointer.generated.s"
GEN_GLOBAL_ASM="$WORKDIR/min_global.generated.s"
GEN_GLOBAL_ARRAY_ASM="$WORKDIR/min_global_array.generated.s"
run_exe "$CCMIN_EXE" "$WORKDIR/cc-min-char-type.run.log" "$TEST_CHAR_TYPE_IN" "$GEN_CHAR_TYPE_ASM"
run_exe "$CCMIN_EXE" "$WORKDIR/cc-min-char-literal.run.log" "$TEST_CHAR_LITERAL_IN" "$GEN_CHAR_LITERAL_ASM"
run_exe "$CCMIN_EXE" "$WORKDIR/cc-min-local-array.run.log" "$TEST_LOCAL_ARRAY_IN" "$GEN_LOCAL_ARRAY_ASM"
run_exe "$CCMIN_EXE" "$WORKDIR/cc-min-char-array.run.log" "$TEST_CHAR_ARRAY_IN" "$GEN_CHAR_ARRAY_ASM"
run_exe "$CCMIN_EXE" "$WORKDIR/cc-min-string-lit.run.log" "$TEST_STRING_LIT_IN" "$GEN_STRING_LIT_ASM"
run_exe "$CCMIN_EXE" "$WORKDIR/cc-min-pointer.run.log" "$TEST_POINTER_IN" "$GEN_POINTER_ASM"
run_exe "$CCMIN_EXE" "$WORKDIR/cc-min-global.run.log" "$TEST_GLOBAL_IN" "$GEN_GLOBAL_ASM"
run_exe "$CCMIN_EXE" "$WORKDIR/cc-min-global-array.run.log" "$TEST_GLOBAL_ARRAY_IN" "$GEN_GLOBAL_ARRAY_ASM"
GEN_PTR_ARITH_ASM="$WORKDIR/min_ptr_arith.generated.s"
run_exe "$CCMIN_EXE" "$WORKDIR/cc-min-ptr-arith.run.log" "$TEST_PTR_ARITH_IN" "$GEN_PTR_ARITH_ASM"
GEN_SHORT_CIRCUIT_ASM="$WORKDIR/min_short_circuit.generated.s"
run_exe "$CCMIN_EXE" "$WORKDIR/cc-min-short-circuit.run.log" "$TEST_SHORT_CIRCUIT_IN" "$GEN_SHORT_CIRCUIT_ASM"
GEN_TYPEDEF_ASM="$WORKDIR/min_typedef.generated.s"
GEN_TYPEDEF_STRUCT_ASM="$WORKDIR/min_typedef_struct.generated.s"
GEN_STRUCT_BASIC_ASM="$WORKDIR/min_struct_basic.generated.s"
GEN_STRUCT_ARROW_ASM="$WORKDIR/min_struct_arrow.generated.s"
GEN_STRUCT_ARRAY_ASM="$WORKDIR/min_struct_array.generated.s"
GEN_SIZEOF_ASM="$WORKDIR/min_sizeof.generated.s"
GEN_SIZEOF_STRUCT_ASM="$WORKDIR/min_sizeof_struct.generated.s"
GEN_CAST_ASM="$WORKDIR/min_cast.generated.s"
run_exe "$CCMIN_EXE" "$WORKDIR/cc-min-typedef.run.log" "$TEST_TYPEDEF_IN" "$GEN_TYPEDEF_ASM"
run_exe "$CCMIN_EXE" "$WORKDIR/cc-min-typedef-struct.run.log" "$TEST_TYPEDEF_STRUCT_IN" "$GEN_TYPEDEF_STRUCT_ASM"
run_exe "$CCMIN_EXE" "$WORKDIR/cc-min-struct-basic.run.log" "$TEST_STRUCT_BASIC_IN" "$GEN_STRUCT_BASIC_ASM"
run_exe "$CCMIN_EXE" "$WORKDIR/cc-min-struct-arrow.run.log" "$TEST_STRUCT_ARROW_IN" "$GEN_STRUCT_ARROW_ASM"
run_exe "$CCMIN_EXE" "$WORKDIR/cc-min-struct-array.run.log" "$TEST_STRUCT_ARRAY_IN" "$GEN_STRUCT_ARRAY_ASM"
run_exe "$CCMIN_EXE" "$WORKDIR/cc-min-sizeof.run.log" "$TEST_SIZEOF_IN" "$GEN_SIZEOF_ASM"
run_exe "$CCMIN_EXE" "$WORKDIR/cc-min-sizeof-struct.run.log" "$TEST_SIZEOF_STRUCT_IN" "$GEN_SIZEOF_STRUCT_ASM"
run_exe "$CCMIN_EXE" "$WORKDIR/cc-min-cast.run.log" "$TEST_CAST_IN" "$GEN_CAST_ASM"
GEN_DEFINE_ASM="$WORKDIR/min_define.generated.s"
GEN_DEFINE_HEX_ASM="$WORKDIR/min_define_hex.generated.s"
GEN_ENUM_ASM="$WORKDIR/min_enum.generated.s"
GEN_PROTOTYPE_ASM="$WORKDIR/min_prototype.generated.s"
GEN_INCLUDE_ASM="$WORKDIR/min_include.generated.s"
run_exe "$CCMIN_EXE" "$WORKDIR/cc-min-define.run.log" "$TEST_DEFINE_IN" "$GEN_DEFINE_ASM"
run_exe "$CCMIN_EXE" "$WORKDIR/cc-min-define-hex.run.log" "$TEST_DEFINE_HEX_IN" "$GEN_DEFINE_HEX_ASM"
run_exe "$CCMIN_EXE" "$WORKDIR/cc-min-enum.run.log" "$TEST_ENUM_IN" "$GEN_ENUM_ASM"
run_exe "$CCMIN_EXE" "$WORKDIR/cc-min-prototype.run.log" "$TEST_PROTOTYPE_IN" "$GEN_PROTOTYPE_ASM"
# Include test: copy helper .h to workdir so cc-min can find it
cp "$TEST_INCLUDE_HELPER_IN" "$WORKDIR/min_include_helper.h"
cp "$TEST_INCLUDE_IN" "$WORKDIR/min_include.c"
pushd "$WORKDIR" >/dev/null
run_exe "$CCMIN_EXE" "$WORKDIR/cc-min-include.run.log" "$WORKDIR/min_include.c" "$GEN_INCLUDE_ASM"
popd >/dev/null
[[ -s "$GEN_ASM" ]] || { echo "cc-min produced no assembly output" >&2; exit 1; }
[[ -s "$GEN_RET_ASM" ]] || { echo "cc-min produced no return-test assembly output" >&2; exit 1; }
[[ -s "$GEN_EXPR_ASM" ]] || { echo "cc-min produced no expr-test assembly output" >&2; exit 1; }
[[ -s "$GEN_LOCAL_ASM" ]] || { echo "cc-min produced no local-test assembly output" >&2; exit 1; }
[[ -s "$GEN_REL_ASM" ]] || { echo "cc-min produced no relational-test assembly output" >&2; exit 1; }
[[ -s "$GEN_IF_TRUE_ASM" ]] || { echo "cc-min produced no if-true assembly output" >&2; exit 1; }
[[ -s "$GEN_IF_FALSE_ASM" ]] || { echo "cc-min produced no if-false assembly output" >&2; exit 1; }
[[ -s "$GEN_WHILE_ASM" ]] || { echo "cc-min produced no while-test assembly output" >&2; exit 1; }
[[ -s "$GEN_TWO_LOCALS_ASM" ]] || { echo "cc-min produced no two-locals assembly output" >&2; exit 1; }
[[ -s "$GEN_HELPER_ASM" ]] || { echo "cc-min produced no helper-call assembly output" >&2; exit 1; }
[[ -s "$GEN_HELPER_ARG_ASM" ]] || { echo "cc-min produced no helper-arg assembly output" >&2; exit 1; }
[[ -s "$GEN_HELPER_LOCAL_ASM" ]] || { echo "cc-min produced no helper-local assembly output" >&2; exit 1; }
[[ -s "$GEN_MAIN_LOCAL_HELPER_ASM" ]] || { echo "cc-min produced no main-local-helper assembly output" >&2; exit 1; }
[[ -s "$GEN_HELPER_TWO_ARGS_ASM" ]] || { echo "cc-min produced no helper-two-args assembly output" >&2; exit 1; }
[[ -s "$GEN_HELPER_TWO_ARGS_IF_ASM" ]] || { echo "cc-min produced no helper-two-args-if assembly output" >&2; exit 1; }
[[ -s "$GEN_MULTI_FUNC_ASM" ]] || { echo "cc-min produced no multi-func assembly output" >&2; exit 1; }
[[ -s "$GEN_FOR_LOOP_ASM" ]] || { echo "cc-min produced no for-loop assembly output" >&2; exit 1; }
[[ -s "$GEN_NESTED_IF_ASM" ]] || { echo "cc-min produced no nested-if assembly output" >&2; exit 1; }
[[ -s "$GEN_BREAK_CONTINUE_ASM" ]] || { echo "cc-min produced no break-continue assembly output" >&2; exit 1; }
[[ -s "$GEN_GENERAL_NAMES_ASM" ]] || { echo "cc-min produced no general-names assembly output" >&2; exit 1; }
[[ -s "$GEN_COMPLEX_EXPR_ASM" ]] || { echo "cc-min produced no complex-expr assembly output" >&2; exit 1; }
[[ -s "$GEN_CHAR_TYPE_ASM" ]] || { echo "cc-min produced no char-type assembly output" >&2; exit 1; }
[[ -s "$GEN_CHAR_LITERAL_ASM" ]] || { echo "cc-min produced no char-literal assembly output" >&2; exit 1; }
[[ -s "$GEN_LOCAL_ARRAY_ASM" ]] || { echo "cc-min produced no local-array assembly output" >&2; exit 1; }
[[ -s "$GEN_CHAR_ARRAY_ASM" ]] || { echo "cc-min produced no char-array assembly output" >&2; exit 1; }
[[ -s "$GEN_STRING_LIT_ASM" ]] || { echo "cc-min produced no string-lit assembly output" >&2; exit 1; }
[[ -s "$GEN_POINTER_ASM" ]] || { echo "cc-min produced no pointer assembly output" >&2; exit 1; }
[[ -s "$GEN_GLOBAL_ASM" ]] || { echo "cc-min produced no global assembly output" >&2; exit 1; }
[[ -s "$GEN_GLOBAL_ARRAY_ASM" ]] || { echo "cc-min produced no global-array assembly output" >&2; exit 1; }
[[ -s "$GEN_PTR_ARITH_ASM" ]] || { echo "cc-min produced no ptr-arith assembly output" >&2; exit 1; }
[[ -s "$GEN_SHORT_CIRCUIT_ASM" ]] || { echo "cc-min produced no short-circuit assembly output" >&2; exit 1; }
[[ -s "$GEN_TYPEDEF_ASM" ]] || { echo "cc-min produced no typedef assembly output" >&2; exit 1; }
[[ -s "$GEN_TYPEDEF_STRUCT_ASM" ]] || { echo "cc-min produced no typedef-struct assembly output" >&2; exit 1; }
[[ -s "$GEN_STRUCT_BASIC_ASM" ]] || { echo "cc-min produced no struct-basic assembly output" >&2; exit 1; }
[[ -s "$GEN_STRUCT_ARROW_ASM" ]] || { echo "cc-min produced no struct-arrow assembly output" >&2; exit 1; }
[[ -s "$GEN_STRUCT_ARRAY_ASM" ]] || { echo "cc-min produced no struct-array assembly output" >&2; exit 1; }
[[ -s "$GEN_SIZEOF_ASM" ]] || { echo "cc-min produced no sizeof assembly output" >&2; exit 1; }
[[ -s "$GEN_SIZEOF_STRUCT_ASM" ]] || { echo "cc-min produced no sizeof-struct assembly output" >&2; exit 1; }
[[ -s "$GEN_CAST_ASM" ]] || { echo "cc-min produced no cast assembly output" >&2; exit 1; }
[[ -s "$GEN_DEFINE_ASM" ]] || { echo "cc-min produced no define assembly output" >&2; exit 1; }
[[ -s "$GEN_DEFINE_HEX_ASM" ]] || { echo "cc-min produced no define-hex assembly output" >&2; exit 1; }
[[ -s "$GEN_ENUM_ASM" ]] || { echo "cc-min produced no enum assembly output" >&2; exit 1; }
[[ -s "$GEN_PROTOTYPE_ASM" ]] || { echo "cc-min produced no prototype assembly output" >&2; exit 1; }
[[ -s "$GEN_INCLUDE_ASM" ]] || { echo "cc-min produced no include assembly output" >&2; exit 1; }
grep -q '^main:' "$GEN_ASM" || { echo "generated assembly missing main label" >&2; exit 1; }

# 4) Assemble, link with stage07 (artifact), then link/run with stage01 runtime.
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
GEN_WHILE_OBJ="$WORKDIR/min_while_countdown.generated.s32o"
GEN_WHILE_RAW_EXE="$WORKDIR/min_while_countdown.generated.raw.s32x"
GEN_WHILE_EXE="$WORKDIR/min_while_countdown.generated.s32x"
GEN_TWO_LOCALS_OBJ="$WORKDIR/min_two_locals.generated.s32o"
GEN_TWO_LOCALS_RAW_EXE="$WORKDIR/min_two_locals.generated.raw.s32x"
GEN_TWO_LOCALS_EXE="$WORKDIR/min_two_locals.generated.s32x"
GEN_HELPER_OBJ="$WORKDIR/min_helper_call.generated.s32o"
GEN_HELPER_RAW_EXE="$WORKDIR/min_helper_call.generated.raw.s32x"
GEN_HELPER_EXE="$WORKDIR/min_helper_call.generated.s32x"
GEN_HELPER_ARG_OBJ="$WORKDIR/min_helper_arg.generated.s32o"
GEN_HELPER_ARG_RAW_EXE="$WORKDIR/min_helper_arg.generated.raw.s32x"
GEN_HELPER_ARG_EXE="$WORKDIR/min_helper_arg.generated.s32x"
GEN_HELPER_LOCAL_OBJ="$WORKDIR/min_helper_local.generated.s32o"
GEN_HELPER_LOCAL_RAW_EXE="$WORKDIR/min_helper_local.generated.raw.s32x"
GEN_HELPER_LOCAL_EXE="$WORKDIR/min_helper_local.generated.s32x"
GEN_MAIN_LOCAL_HELPER_OBJ="$WORKDIR/min_main_local_helper.generated.s32o"
GEN_MAIN_LOCAL_HELPER_RAW_EXE="$WORKDIR/min_main_local_helper.generated.raw.s32x"
GEN_MAIN_LOCAL_HELPER_EXE="$WORKDIR/min_main_local_helper.generated.s32x"
GEN_HELPER_TWO_ARGS_OBJ="$WORKDIR/min_helper_two_args.generated.s32o"
GEN_HELPER_TWO_ARGS_RAW_EXE="$WORKDIR/min_helper_two_args.generated.raw.s32x"
GEN_HELPER_TWO_ARGS_EXE="$WORKDIR/min_helper_two_args.generated.s32x"
GEN_HELPER_TWO_ARGS_IF_OBJ="$WORKDIR/min_helper_two_args_if.generated.s32o"
GEN_HELPER_TWO_ARGS_IF_RAW_EXE="$WORKDIR/min_helper_two_args_if.generated.raw.s32x"
GEN_HELPER_TWO_ARGS_IF_EXE="$WORKDIR/min_helper_two_args_if.generated.s32x"
GEN_MULTI_FUNC_OBJ="$WORKDIR/min_multi_func.generated.s32o"
GEN_MULTI_FUNC_RAW_EXE="$WORKDIR/min_multi_func.generated.raw.s32x"
GEN_MULTI_FUNC_EXE="$WORKDIR/min_multi_func.generated.s32x"
GEN_FOR_LOOP_OBJ="$WORKDIR/min_for_loop.generated.s32o"
GEN_FOR_LOOP_RAW_EXE="$WORKDIR/min_for_loop.generated.raw.s32x"
GEN_FOR_LOOP_EXE="$WORKDIR/min_for_loop.generated.s32x"
GEN_NESTED_IF_OBJ="$WORKDIR/min_nested_if.generated.s32o"
GEN_NESTED_IF_RAW_EXE="$WORKDIR/min_nested_if.generated.raw.s32x"
GEN_NESTED_IF_EXE="$WORKDIR/min_nested_if.generated.s32x"
GEN_BREAK_CONTINUE_OBJ="$WORKDIR/min_break_continue.generated.s32o"
GEN_BREAK_CONTINUE_RAW_EXE="$WORKDIR/min_break_continue.generated.raw.s32x"
GEN_BREAK_CONTINUE_EXE="$WORKDIR/min_break_continue.generated.s32x"
GEN_GENERAL_NAMES_OBJ="$WORKDIR/min_general_names.generated.s32o"
GEN_GENERAL_NAMES_RAW_EXE="$WORKDIR/min_general_names.generated.raw.s32x"
GEN_GENERAL_NAMES_EXE="$WORKDIR/min_general_names.generated.s32x"
GEN_COMPLEX_EXPR_OBJ="$WORKDIR/min_complex_expr.generated.s32o"
GEN_COMPLEX_EXPR_RAW_EXE="$WORKDIR/min_complex_expr.generated.raw.s32x"
GEN_COMPLEX_EXPR_EXE="$WORKDIR/min_complex_expr.generated.s32x"
run_exe "$AS_EXE" "$WORKDIR/stage5-as.run.log" "$GEN_ASM" "$GEN_OBJ"
run_exe "$AS_EXE" "$WORKDIR/stage5-as-ret.run.log" "$GEN_RET_ASM" "$GEN_RET_OBJ"
run_exe "$AS_EXE" "$WORKDIR/stage5-as-expr.run.log" "$GEN_EXPR_ASM" "$GEN_EXPR_OBJ"
run_exe "$AS_EXE" "$WORKDIR/stage5-as-local.run.log" "$GEN_LOCAL_ASM" "$GEN_LOCAL_OBJ"
run_exe "$AS_EXE" "$WORKDIR/stage5-as-rel.run.log" "$GEN_REL_ASM" "$GEN_REL_OBJ"
run_exe "$AS_EXE" "$WORKDIR/stage5-as-if-true.run.log" "$GEN_IF_TRUE_ASM" "$GEN_IF_TRUE_OBJ"
run_exe "$AS_EXE" "$WORKDIR/stage5-as-if-false.run.log" "$GEN_IF_FALSE_ASM" "$GEN_IF_FALSE_OBJ"
run_exe "$AS_EXE" "$WORKDIR/stage5-as-while.run.log" "$GEN_WHILE_ASM" "$GEN_WHILE_OBJ"
run_exe "$AS_EXE" "$WORKDIR/stage5-as-two-locals.run.log" "$GEN_TWO_LOCALS_ASM" "$GEN_TWO_LOCALS_OBJ"
run_exe "$AS_EXE" "$WORKDIR/stage5-as-helper.run.log" "$GEN_HELPER_ASM" "$GEN_HELPER_OBJ"
run_exe "$AS_EXE" "$WORKDIR/stage5-as-helper-arg.run.log" "$GEN_HELPER_ARG_ASM" "$GEN_HELPER_ARG_OBJ"
run_exe "$AS_EXE" "$WORKDIR/stage5-as-helper-local.run.log" "$GEN_HELPER_LOCAL_ASM" "$GEN_HELPER_LOCAL_OBJ"
run_exe "$AS_EXE" "$WORKDIR/stage5-as-main-local-helper.run.log" "$GEN_MAIN_LOCAL_HELPER_ASM" "$GEN_MAIN_LOCAL_HELPER_OBJ"
run_exe "$AS_EXE" "$WORKDIR/stage5-as-helper-two-args.run.log" "$GEN_HELPER_TWO_ARGS_ASM" "$GEN_HELPER_TWO_ARGS_OBJ"
run_exe "$AS_EXE" "$WORKDIR/stage5-as-helper-two-args-if.run.log" "$GEN_HELPER_TWO_ARGS_IF_ASM" "$GEN_HELPER_TWO_ARGS_IF_OBJ"
run_exe "$AS_EXE" "$WORKDIR/stage5-as-multi-func.run.log" "$GEN_MULTI_FUNC_ASM" "$GEN_MULTI_FUNC_OBJ"
run_exe "$AS_EXE" "$WORKDIR/stage5-as-for-loop.run.log" "$GEN_FOR_LOOP_ASM" "$GEN_FOR_LOOP_OBJ"
run_exe "$AS_EXE" "$WORKDIR/stage5-as-nested-if.run.log" "$GEN_NESTED_IF_ASM" "$GEN_NESTED_IF_OBJ"
run_exe "$AS_EXE" "$WORKDIR/stage5-as-break-continue.run.log" "$GEN_BREAK_CONTINUE_ASM" "$GEN_BREAK_CONTINUE_OBJ"
run_exe "$AS_EXE" "$WORKDIR/stage5-as-general-names.run.log" "$GEN_GENERAL_NAMES_ASM" "$GEN_GENERAL_NAMES_OBJ"
run_exe "$AS_EXE" "$WORKDIR/stage5-as-complex-expr.run.log" "$GEN_COMPLEX_EXPR_ASM" "$GEN_COMPLEX_EXPR_OBJ"
GEN_CHAR_TYPE_OBJ="$WORKDIR/min_char_type.generated.s32o"
GEN_CHAR_LITERAL_OBJ="$WORKDIR/min_char_literal.generated.s32o"
GEN_LOCAL_ARRAY_OBJ="$WORKDIR/min_local_array.generated.s32o"
GEN_CHAR_ARRAY_OBJ="$WORKDIR/min_char_array.generated.s32o"
GEN_STRING_LIT_OBJ="$WORKDIR/min_string_lit.generated.s32o"
GEN_POINTER_OBJ="$WORKDIR/min_pointer.generated.s32o"
GEN_GLOBAL_OBJ="$WORKDIR/min_global.generated.s32o"
GEN_GLOBAL_ARRAY_OBJ="$WORKDIR/min_global_array.generated.s32o"
run_exe "$AS_EXE" "$WORKDIR/stage5-as-char-type.run.log" "$GEN_CHAR_TYPE_ASM" "$GEN_CHAR_TYPE_OBJ"
run_exe "$AS_EXE" "$WORKDIR/stage5-as-char-literal.run.log" "$GEN_CHAR_LITERAL_ASM" "$GEN_CHAR_LITERAL_OBJ"
run_exe "$AS_EXE" "$WORKDIR/stage5-as-local-array.run.log" "$GEN_LOCAL_ARRAY_ASM" "$GEN_LOCAL_ARRAY_OBJ"
run_exe "$AS_EXE" "$WORKDIR/stage5-as-char-array.run.log" "$GEN_CHAR_ARRAY_ASM" "$GEN_CHAR_ARRAY_OBJ"
run_exe "$AS_EXE" "$WORKDIR/stage5-as-string-lit.run.log" "$GEN_STRING_LIT_ASM" "$GEN_STRING_LIT_OBJ"
run_exe "$AS_EXE" "$WORKDIR/stage5-as-pointer.run.log" "$GEN_POINTER_ASM" "$GEN_POINTER_OBJ"
run_exe "$AS_EXE" "$WORKDIR/stage5-as-global.run.log" "$GEN_GLOBAL_ASM" "$GEN_GLOBAL_OBJ"
run_exe "$AS_EXE" "$WORKDIR/stage5-as-global-array.run.log" "$GEN_GLOBAL_ARRAY_ASM" "$GEN_GLOBAL_ARRAY_OBJ"
GEN_PTR_ARITH_OBJ="$WORKDIR/min_ptr_arith.generated.s32o"
run_exe "$AS_EXE" "$WORKDIR/stage5-as-ptr-arith.run.log" "$GEN_PTR_ARITH_ASM" "$GEN_PTR_ARITH_OBJ"
GEN_SHORT_CIRCUIT_OBJ="$WORKDIR/min_short_circuit.generated.s32o"
run_exe "$AS_EXE" "$WORKDIR/stage5-as-short-circuit.run.log" "$GEN_SHORT_CIRCUIT_ASM" "$GEN_SHORT_CIRCUIT_OBJ"
GEN_TYPEDEF_OBJ="$WORKDIR/min_typedef.generated.s32o"
GEN_TYPEDEF_STRUCT_OBJ="$WORKDIR/min_typedef_struct.generated.s32o"
GEN_STRUCT_BASIC_OBJ="$WORKDIR/min_struct_basic.generated.s32o"
GEN_STRUCT_ARROW_OBJ="$WORKDIR/min_struct_arrow.generated.s32o"
GEN_STRUCT_ARRAY_OBJ="$WORKDIR/min_struct_array.generated.s32o"
GEN_SIZEOF_OBJ="$WORKDIR/min_sizeof.generated.s32o"
GEN_SIZEOF_STRUCT_OBJ="$WORKDIR/min_sizeof_struct.generated.s32o"
GEN_CAST_OBJ="$WORKDIR/min_cast.generated.s32o"
run_exe "$AS_EXE" "$WORKDIR/stage5-as-typedef.run.log" "$GEN_TYPEDEF_ASM" "$GEN_TYPEDEF_OBJ"
run_exe "$AS_EXE" "$WORKDIR/stage5-as-typedef-struct.run.log" "$GEN_TYPEDEF_STRUCT_ASM" "$GEN_TYPEDEF_STRUCT_OBJ"
run_exe "$AS_EXE" "$WORKDIR/stage5-as-struct-basic.run.log" "$GEN_STRUCT_BASIC_ASM" "$GEN_STRUCT_BASIC_OBJ"
run_exe "$AS_EXE" "$WORKDIR/stage5-as-struct-arrow.run.log" "$GEN_STRUCT_ARROW_ASM" "$GEN_STRUCT_ARROW_OBJ"
run_exe "$AS_EXE" "$WORKDIR/stage5-as-struct-array.run.log" "$GEN_STRUCT_ARRAY_ASM" "$GEN_STRUCT_ARRAY_OBJ"
run_exe "$AS_EXE" "$WORKDIR/stage5-as-sizeof.run.log" "$GEN_SIZEOF_ASM" "$GEN_SIZEOF_OBJ"
run_exe "$AS_EXE" "$WORKDIR/stage5-as-sizeof-struct.run.log" "$GEN_SIZEOF_STRUCT_ASM" "$GEN_SIZEOF_STRUCT_OBJ"
run_exe "$AS_EXE" "$WORKDIR/stage5-as-cast.run.log" "$GEN_CAST_ASM" "$GEN_CAST_OBJ"
GEN_DEFINE_OBJ="$WORKDIR/min_define.generated.s32o"
GEN_DEFINE_HEX_OBJ="$WORKDIR/min_define_hex.generated.s32o"
GEN_ENUM_OBJ="$WORKDIR/min_enum.generated.s32o"
GEN_PROTOTYPE_OBJ="$WORKDIR/min_prototype.generated.s32o"
GEN_INCLUDE_OBJ="$WORKDIR/min_include.generated.s32o"
run_exe "$AS_EXE" "$WORKDIR/stage5-as-define.run.log" "$GEN_DEFINE_ASM" "$GEN_DEFINE_OBJ"
run_exe "$AS_EXE" "$WORKDIR/stage5-as-define-hex.run.log" "$GEN_DEFINE_HEX_ASM" "$GEN_DEFINE_HEX_OBJ"
run_exe "$AS_EXE" "$WORKDIR/stage5-as-enum.run.log" "$GEN_ENUM_ASM" "$GEN_ENUM_OBJ"
run_exe "$AS_EXE" "$WORKDIR/stage5-as-prototype.run.log" "$GEN_PROTOTYPE_ASM" "$GEN_PROTOTYPE_OBJ"
run_exe "$AS_EXE" "$WORKDIR/stage5-as-include.run.log" "$GEN_INCLUDE_ASM" "$GEN_INCLUDE_OBJ"
[[ -s "$GEN_OBJ" ]] || { echo "stage05 assembler produced no object output" >&2; exit 1; }
[[ -s "$GEN_RET_OBJ" ]] || { echo "stage05 assembler produced no return-test object output" >&2; exit 1; }
[[ -s "$GEN_EXPR_OBJ" ]] || { echo "stage05 assembler produced no expr-test object output" >&2; exit 1; }
[[ -s "$GEN_LOCAL_OBJ" ]] || { echo "stage05 assembler produced no local-test object output" >&2; exit 1; }
[[ -s "$GEN_REL_OBJ" ]] || { echo "stage05 assembler produced no relational-test object output" >&2; exit 1; }
[[ -s "$GEN_IF_TRUE_OBJ" ]] || { echo "stage05 assembler produced no if-true object output" >&2; exit 1; }
[[ -s "$GEN_IF_FALSE_OBJ" ]] || { echo "stage05 assembler produced no if-false object output" >&2; exit 1; }
[[ -s "$GEN_WHILE_OBJ" ]] || { echo "stage05 assembler produced no while-test object output" >&2; exit 1; }
[[ -s "$GEN_TWO_LOCALS_OBJ" ]] || { echo "stage05 assembler produced no two-locals object output" >&2; exit 1; }
[[ -s "$GEN_HELPER_OBJ" ]] || { echo "stage05 assembler produced no helper-call object output" >&2; exit 1; }
[[ -s "$GEN_HELPER_ARG_OBJ" ]] || { echo "stage05 assembler produced no helper-arg object output" >&2; exit 1; }
[[ -s "$GEN_HELPER_LOCAL_OBJ" ]] || { echo "stage05 assembler produced no helper-local object output" >&2; exit 1; }
[[ -s "$GEN_MAIN_LOCAL_HELPER_OBJ" ]] || { echo "stage05 assembler produced no main-local-helper object output" >&2; exit 1; }
[[ -s "$GEN_HELPER_TWO_ARGS_OBJ" ]] || { echo "stage05 assembler produced no helper-two-args object output" >&2; exit 1; }
[[ -s "$GEN_HELPER_TWO_ARGS_IF_OBJ" ]] || { echo "stage05 assembler produced no helper-two-args-if object output" >&2; exit 1; }
[[ -s "$GEN_MULTI_FUNC_OBJ" ]] || { echo "stage05 assembler produced no multi-func object output" >&2; exit 1; }
[[ -s "$GEN_FOR_LOOP_OBJ" ]] || { echo "stage05 assembler produced no for-loop object output" >&2; exit 1; }
[[ -s "$GEN_NESTED_IF_OBJ" ]] || { echo "stage05 assembler produced no nested-if object output" >&2; exit 1; }
[[ -s "$GEN_BREAK_CONTINUE_OBJ" ]] || { echo "stage05 assembler produced no break-continue object output" >&2; exit 1; }
[[ -s "$GEN_GENERAL_NAMES_OBJ" ]] || { echo "stage05 assembler produced no general-names object output" >&2; exit 1; }
[[ -s "$GEN_COMPLEX_EXPR_OBJ" ]] || { echo "stage05 assembler produced no complex-expr object output" >&2; exit 1; }
[[ -s "$GEN_CHAR_TYPE_OBJ" ]] || { echo "stage05 assembler produced no char-type object output" >&2; exit 1; }
[[ -s "$GEN_CHAR_LITERAL_OBJ" ]] || { echo "stage05 assembler produced no char-literal object output" >&2; exit 1; }
[[ -s "$GEN_LOCAL_ARRAY_OBJ" ]] || { echo "stage05 assembler produced no local-array object output" >&2; exit 1; }
[[ -s "$GEN_CHAR_ARRAY_OBJ" ]] || { echo "stage05 assembler produced no char-array object output" >&2; exit 1; }
[[ -s "$GEN_STRING_LIT_OBJ" ]] || { echo "stage05 assembler produced no string-lit object output" >&2; exit 1; }
[[ -s "$GEN_POINTER_OBJ" ]] || { echo "stage05 assembler produced no pointer object output" >&2; exit 1; }
[[ -s "$GEN_GLOBAL_OBJ" ]] || { echo "stage05 assembler produced no global object output" >&2; exit 1; }
[[ -s "$GEN_GLOBAL_ARRAY_OBJ" ]] || { echo "stage05 assembler produced no global-array object output" >&2; exit 1; }
[[ -s "$GEN_PTR_ARITH_OBJ" ]] || { echo "stage05 assembler produced no ptr-arith object output" >&2; exit 1; }
[[ -s "$GEN_SHORT_CIRCUIT_OBJ" ]] || { echo "stage05 assembler produced no short-circuit object output" >&2; exit 1; }
[[ -s "$GEN_TYPEDEF_OBJ" ]] || { echo "stage05 assembler produced no typedef object output" >&2; exit 1; }
[[ -s "$GEN_TYPEDEF_STRUCT_OBJ" ]] || { echo "stage05 assembler produced no typedef-struct object output" >&2; exit 1; }
[[ -s "$GEN_STRUCT_BASIC_OBJ" ]] || { echo "stage05 assembler produced no struct-basic object output" >&2; exit 1; }
[[ -s "$GEN_STRUCT_ARROW_OBJ" ]] || { echo "stage05 assembler produced no struct-arrow object output" >&2; exit 1; }
[[ -s "$GEN_STRUCT_ARRAY_OBJ" ]] || { echo "stage05 assembler produced no struct-array object output" >&2; exit 1; }
[[ -s "$GEN_SIZEOF_OBJ" ]] || { echo "stage05 assembler produced no sizeof object output" >&2; exit 1; }
[[ -s "$GEN_SIZEOF_STRUCT_OBJ" ]] || { echo "stage05 assembler produced no sizeof-struct object output" >&2; exit 1; }
[[ -s "$GEN_CAST_OBJ" ]] || { echo "stage05 assembler produced no cast object output" >&2; exit 1; }
[[ -s "$GEN_DEFINE_OBJ" ]] || { echo "stage05 assembler produced no define object output" >&2; exit 1; }
[[ -s "$GEN_DEFINE_HEX_OBJ" ]] || { echo "stage05 assembler produced no define-hex object output" >&2; exit 1; }
[[ -s "$GEN_ENUM_OBJ" ]] || { echo "stage05 assembler produced no enum object output" >&2; exit 1; }
[[ -s "$GEN_PROTOTYPE_OBJ" ]] || { echo "stage05 assembler produced no prototype object output" >&2; exit 1; }
[[ -s "$GEN_INCLUDE_OBJ" ]] || { echo "stage05 assembler produced no include object output" >&2; exit 1; }
run_exe "$LD_EXE" "$WORKDIR/stage7-ld.run.log" "$GEN_OBJ" "$GEN_RAW_EXE"
run_exe "$LD_EXE" "$WORKDIR/stage7-ld-ret.run.log" "$GEN_RET_OBJ" "$GEN_RET_RAW_EXE"
run_exe "$LD_EXE" "$WORKDIR/stage7-ld-expr.run.log" "$GEN_EXPR_OBJ" "$GEN_EXPR_RAW_EXE"
run_exe "$LD_EXE" "$WORKDIR/stage7-ld-local.run.log" "$GEN_LOCAL_OBJ" "$GEN_LOCAL_RAW_EXE"
run_exe "$LD_EXE" "$WORKDIR/stage7-ld-rel.run.log" "$GEN_REL_OBJ" "$GEN_REL_RAW_EXE"
run_exe "$LD_EXE" "$WORKDIR/stage7-ld-if-true.run.log" "$GEN_IF_TRUE_OBJ" "$GEN_IF_TRUE_RAW_EXE"
run_exe "$LD_EXE" "$WORKDIR/stage7-ld-if-false.run.log" "$GEN_IF_FALSE_OBJ" "$GEN_IF_FALSE_RAW_EXE"
run_exe "$LD_EXE" "$WORKDIR/stage7-ld-while.run.log" "$GEN_WHILE_OBJ" "$GEN_WHILE_RAW_EXE"
run_exe "$LD_EXE" "$WORKDIR/stage7-ld-two-locals.run.log" "$GEN_TWO_LOCALS_OBJ" "$GEN_TWO_LOCALS_RAW_EXE"
run_exe "$LD_EXE" "$WORKDIR/stage7-ld-helper.run.log" "$GEN_HELPER_OBJ" "$GEN_HELPER_RAW_EXE"
run_exe "$LD_EXE" "$WORKDIR/stage7-ld-helper-arg.run.log" "$GEN_HELPER_ARG_OBJ" "$GEN_HELPER_ARG_RAW_EXE"
run_exe "$LD_EXE" "$WORKDIR/stage7-ld-helper-local.run.log" "$GEN_HELPER_LOCAL_OBJ" "$GEN_HELPER_LOCAL_RAW_EXE"
run_exe "$LD_EXE" "$WORKDIR/stage7-ld-main-local-helper.run.log" "$GEN_MAIN_LOCAL_HELPER_OBJ" "$GEN_MAIN_LOCAL_HELPER_RAW_EXE"
run_exe "$LD_EXE" "$WORKDIR/stage7-ld-helper-two-args.run.log" "$GEN_HELPER_TWO_ARGS_OBJ" "$GEN_HELPER_TWO_ARGS_RAW_EXE"
run_exe "$LD_EXE" "$WORKDIR/stage7-ld-helper-two-args-if.run.log" "$GEN_HELPER_TWO_ARGS_IF_OBJ" "$GEN_HELPER_TWO_ARGS_IF_RAW_EXE"
run_exe "$LD_EXE" "$WORKDIR/stage7-ld-multi-func.run.log" "$GEN_MULTI_FUNC_OBJ" "$GEN_MULTI_FUNC_RAW_EXE"
run_exe "$LD_EXE" "$WORKDIR/stage7-ld-for-loop.run.log" "$GEN_FOR_LOOP_OBJ" "$GEN_FOR_LOOP_RAW_EXE"
run_exe "$LD_EXE" "$WORKDIR/stage7-ld-nested-if.run.log" "$GEN_NESTED_IF_OBJ" "$GEN_NESTED_IF_RAW_EXE"
run_exe "$LD_EXE" "$WORKDIR/stage7-ld-break-continue.run.log" "$GEN_BREAK_CONTINUE_OBJ" "$GEN_BREAK_CONTINUE_RAW_EXE"
run_exe "$LD_EXE" "$WORKDIR/stage7-ld-general-names.run.log" "$GEN_GENERAL_NAMES_OBJ" "$GEN_GENERAL_NAMES_RAW_EXE"
run_exe "$LD_EXE" "$WORKDIR/stage7-ld-complex-expr.run.log" "$GEN_COMPLEX_EXPR_OBJ" "$GEN_COMPLEX_EXPR_RAW_EXE"
GEN_CHAR_TYPE_RAW_EXE="$WORKDIR/min_char_type.generated.raw.s32x"
GEN_CHAR_LITERAL_RAW_EXE="$WORKDIR/min_char_literal.generated.raw.s32x"
GEN_LOCAL_ARRAY_RAW_EXE="$WORKDIR/min_local_array.generated.raw.s32x"
GEN_CHAR_ARRAY_RAW_EXE="$WORKDIR/min_char_array.generated.raw.s32x"
GEN_STRING_LIT_RAW_EXE="$WORKDIR/min_string_lit.generated.raw.s32x"
GEN_POINTER_RAW_EXE="$WORKDIR/min_pointer.generated.raw.s32x"
GEN_GLOBAL_RAW_EXE="$WORKDIR/min_global.generated.raw.s32x"
GEN_GLOBAL_ARRAY_RAW_EXE="$WORKDIR/min_global_array.generated.raw.s32x"
run_exe "$LD_EXE" "$WORKDIR/stage7-ld-char-type.run.log" "$GEN_CHAR_TYPE_OBJ" "$GEN_CHAR_TYPE_RAW_EXE"
run_exe "$LD_EXE" "$WORKDIR/stage7-ld-char-literal.run.log" "$GEN_CHAR_LITERAL_OBJ" "$GEN_CHAR_LITERAL_RAW_EXE"
run_exe "$LD_EXE" "$WORKDIR/stage7-ld-local-array.run.log" "$GEN_LOCAL_ARRAY_OBJ" "$GEN_LOCAL_ARRAY_RAW_EXE"
run_exe "$LD_EXE" "$WORKDIR/stage7-ld-char-array.run.log" "$GEN_CHAR_ARRAY_OBJ" "$GEN_CHAR_ARRAY_RAW_EXE"
run_exe "$LD_EXE" "$WORKDIR/stage7-ld-string-lit.run.log" "$GEN_STRING_LIT_OBJ" "$GEN_STRING_LIT_RAW_EXE"
run_exe "$LD_EXE" "$WORKDIR/stage7-ld-pointer.run.log" "$GEN_POINTER_OBJ" "$GEN_POINTER_RAW_EXE"
run_exe "$LD_EXE" "$WORKDIR/stage7-ld-global.run.log" "$GEN_GLOBAL_OBJ" "$GEN_GLOBAL_RAW_EXE"
run_exe "$LD_EXE" "$WORKDIR/stage7-ld-global-array.run.log" "$GEN_GLOBAL_ARRAY_OBJ" "$GEN_GLOBAL_ARRAY_RAW_EXE"
GEN_PTR_ARITH_RAW_EXE="$WORKDIR/min_ptr_arith.generated.raw.s32x"
run_exe "$LD_EXE" "$WORKDIR/stage7-ld-ptr-arith.run.log" "$GEN_PTR_ARITH_OBJ" "$GEN_PTR_ARITH_RAW_EXE"
GEN_SHORT_CIRCUIT_RAW_EXE="$WORKDIR/min_short_circuit.generated.raw.s32x"
run_exe "$LD_EXE" "$WORKDIR/stage7-ld-short-circuit.run.log" "$GEN_SHORT_CIRCUIT_OBJ" "$GEN_SHORT_CIRCUIT_RAW_EXE"
GEN_TYPEDEF_RAW_EXE="$WORKDIR/min_typedef.generated.raw.s32x"
GEN_TYPEDEF_STRUCT_RAW_EXE="$WORKDIR/min_typedef_struct.generated.raw.s32x"
GEN_STRUCT_BASIC_RAW_EXE="$WORKDIR/min_struct_basic.generated.raw.s32x"
GEN_STRUCT_ARROW_RAW_EXE="$WORKDIR/min_struct_arrow.generated.raw.s32x"
GEN_STRUCT_ARRAY_RAW_EXE="$WORKDIR/min_struct_array.generated.raw.s32x"
GEN_SIZEOF_RAW_EXE="$WORKDIR/min_sizeof.generated.raw.s32x"
GEN_SIZEOF_STRUCT_RAW_EXE="$WORKDIR/min_sizeof_struct.generated.raw.s32x"
GEN_CAST_RAW_EXE="$WORKDIR/min_cast.generated.raw.s32x"
run_exe "$LD_EXE" "$WORKDIR/stage7-ld-typedef.run.log" "$GEN_TYPEDEF_OBJ" "$GEN_TYPEDEF_RAW_EXE"
run_exe "$LD_EXE" "$WORKDIR/stage7-ld-typedef-struct.run.log" "$GEN_TYPEDEF_STRUCT_OBJ" "$GEN_TYPEDEF_STRUCT_RAW_EXE"
run_exe "$LD_EXE" "$WORKDIR/stage7-ld-struct-basic.run.log" "$GEN_STRUCT_BASIC_OBJ" "$GEN_STRUCT_BASIC_RAW_EXE"
run_exe "$LD_EXE" "$WORKDIR/stage7-ld-struct-arrow.run.log" "$GEN_STRUCT_ARROW_OBJ" "$GEN_STRUCT_ARROW_RAW_EXE"
run_exe "$LD_EXE" "$WORKDIR/stage7-ld-struct-array.run.log" "$GEN_STRUCT_ARRAY_OBJ" "$GEN_STRUCT_ARRAY_RAW_EXE"
run_exe "$LD_EXE" "$WORKDIR/stage7-ld-sizeof.run.log" "$GEN_SIZEOF_OBJ" "$GEN_SIZEOF_RAW_EXE"
run_exe "$LD_EXE" "$WORKDIR/stage7-ld-sizeof-struct.run.log" "$GEN_SIZEOF_STRUCT_OBJ" "$GEN_SIZEOF_STRUCT_RAW_EXE"
run_exe "$LD_EXE" "$WORKDIR/stage7-ld-cast.run.log" "$GEN_CAST_OBJ" "$GEN_CAST_RAW_EXE"
GEN_DEFINE_RAW_EXE="$WORKDIR/min_define.generated.raw.s32x"
GEN_DEFINE_HEX_RAW_EXE="$WORKDIR/min_define_hex.generated.raw.s32x"
GEN_ENUM_RAW_EXE="$WORKDIR/min_enum.generated.raw.s32x"
GEN_PROTOTYPE_RAW_EXE="$WORKDIR/min_prototype.generated.raw.s32x"
GEN_INCLUDE_RAW_EXE="$WORKDIR/min_include.generated.raw.s32x"
run_exe "$LD_EXE" "$WORKDIR/stage7-ld-define.run.log" "$GEN_DEFINE_OBJ" "$GEN_DEFINE_RAW_EXE"
run_exe "$LD_EXE" "$WORKDIR/stage7-ld-define-hex.run.log" "$GEN_DEFINE_HEX_OBJ" "$GEN_DEFINE_HEX_RAW_EXE"
run_exe "$LD_EXE" "$WORKDIR/stage7-ld-enum.run.log" "$GEN_ENUM_OBJ" "$GEN_ENUM_RAW_EXE"
run_exe "$LD_EXE" "$WORKDIR/stage7-ld-prototype.run.log" "$GEN_PROTOTYPE_OBJ" "$GEN_PROTOTYPE_RAW_EXE"
run_exe "$LD_EXE" "$WORKDIR/stage7-ld-include.run.log" "$GEN_INCLUDE_OBJ" "$GEN_INCLUDE_RAW_EXE"
[[ -s "$GEN_RAW_EXE" ]] || { echo "stage07 linker produced no executable output" >&2; exit 1; }
[[ -s "$GEN_RET_RAW_EXE" ]] || { echo "stage07 linker produced no return-test executable output" >&2; exit 1; }
[[ -s "$GEN_EXPR_RAW_EXE" ]] || { echo "stage07 linker produced no expr-test executable output" >&2; exit 1; }
[[ -s "$GEN_LOCAL_RAW_EXE" ]] || { echo "stage07 linker produced no local-test executable output" >&2; exit 1; }
[[ -s "$GEN_REL_RAW_EXE" ]] || { echo "stage07 linker produced no relational-test executable output" >&2; exit 1; }
[[ -s "$GEN_IF_TRUE_RAW_EXE" ]] || { echo "stage07 linker produced no if-true executable output" >&2; exit 1; }
[[ -s "$GEN_IF_FALSE_RAW_EXE" ]] || { echo "stage07 linker produced no if-false executable output" >&2; exit 1; }
[[ -s "$GEN_WHILE_RAW_EXE" ]] || { echo "stage07 linker produced no while executable output" >&2; exit 1; }
[[ -s "$GEN_TWO_LOCALS_RAW_EXE" ]] || { echo "stage07 linker produced no two-locals executable output" >&2; exit 1; }
[[ -s "$GEN_HELPER_RAW_EXE" ]] || { echo "stage07 linker produced no helper-call executable output" >&2; exit 1; }
[[ -s "$GEN_HELPER_ARG_RAW_EXE" ]] || { echo "stage07 linker produced no helper-arg executable output" >&2; exit 1; }
[[ -s "$GEN_HELPER_LOCAL_RAW_EXE" ]] || { echo "stage07 linker produced no helper-local executable output" >&2; exit 1; }
[[ -s "$GEN_MAIN_LOCAL_HELPER_RAW_EXE" ]] || { echo "stage07 linker produced no main-local-helper executable output" >&2; exit 1; }
[[ -s "$GEN_HELPER_TWO_ARGS_RAW_EXE" ]] || { echo "stage07 linker produced no helper-two-args executable output" >&2; exit 1; }
[[ -s "$GEN_HELPER_TWO_ARGS_IF_RAW_EXE" ]] || { echo "stage07 linker produced no helper-two-args-if executable output" >&2; exit 1; }
[[ -s "$GEN_MULTI_FUNC_RAW_EXE" ]] || { echo "stage07 linker produced no multi-func executable output" >&2; exit 1; }
[[ -s "$GEN_FOR_LOOP_RAW_EXE" ]] || { echo "stage07 linker produced no for-loop executable output" >&2; exit 1; }
[[ -s "$GEN_NESTED_IF_RAW_EXE" ]] || { echo "stage07 linker produced no nested-if executable output" >&2; exit 1; }
[[ -s "$GEN_BREAK_CONTINUE_RAW_EXE" ]] || { echo "stage07 linker produced no break-continue executable output" >&2; exit 1; }
[[ -s "$GEN_GENERAL_NAMES_RAW_EXE" ]] || { echo "stage07 linker produced no general-names executable output" >&2; exit 1; }
[[ -s "$GEN_COMPLEX_EXPR_RAW_EXE" ]] || { echo "stage07 linker produced no complex-expr executable output" >&2; exit 1; }
[[ -s "$GEN_CHAR_TYPE_RAW_EXE" ]] || { echo "stage07 linker produced no char-type executable output" >&2; exit 1; }
[[ -s "$GEN_CHAR_LITERAL_RAW_EXE" ]] || { echo "stage07 linker produced no char-literal executable output" >&2; exit 1; }
[[ -s "$GEN_LOCAL_ARRAY_RAW_EXE" ]] || { echo "stage07 linker produced no local-array executable output" >&2; exit 1; }
[[ -s "$GEN_CHAR_ARRAY_RAW_EXE" ]] || { echo "stage07 linker produced no char-array executable output" >&2; exit 1; }
[[ -s "$GEN_STRING_LIT_RAW_EXE" ]] || { echo "stage07 linker produced no string-lit executable output" >&2; exit 1; }
[[ -s "$GEN_POINTER_RAW_EXE" ]] || { echo "stage07 linker produced no pointer executable output" >&2; exit 1; }
[[ -s "$GEN_GLOBAL_RAW_EXE" ]] || { echo "stage07 linker produced no global executable output" >&2; exit 1; }
[[ -s "$GEN_GLOBAL_ARRAY_RAW_EXE" ]] || { echo "stage07 linker produced no global-array executable output" >&2; exit 1; }
[[ -s "$GEN_PTR_ARITH_RAW_EXE" ]] || { echo "stage07 linker produced no ptr-arith executable output" >&2; exit 1; }
[[ -s "$GEN_SHORT_CIRCUIT_RAW_EXE" ]] || { echo "stage07 linker produced no short-circuit executable output" >&2; exit 1; }
[[ -s "$GEN_TYPEDEF_RAW_EXE" ]] || { echo "stage07 linker produced no typedef executable output" >&2; exit 1; }
[[ -s "$GEN_TYPEDEF_STRUCT_RAW_EXE" ]] || { echo "stage07 linker produced no typedef-struct executable output" >&2; exit 1; }
[[ -s "$GEN_STRUCT_BASIC_RAW_EXE" ]] || { echo "stage07 linker produced no struct-basic executable output" >&2; exit 1; }
[[ -s "$GEN_STRUCT_ARROW_RAW_EXE" ]] || { echo "stage07 linker produced no struct-arrow executable output" >&2; exit 1; }
[[ -s "$GEN_STRUCT_ARRAY_RAW_EXE" ]] || { echo "stage07 linker produced no struct-array executable output" >&2; exit 1; }
[[ -s "$GEN_SIZEOF_RAW_EXE" ]] || { echo "stage07 linker produced no sizeof executable output" >&2; exit 1; }
[[ -s "$GEN_SIZEOF_STRUCT_RAW_EXE" ]] || { echo "stage07 linker produced no sizeof-struct executable output" >&2; exit 1; }
[[ -s "$GEN_CAST_RAW_EXE" ]] || { echo "stage07 linker produced no cast executable output" >&2; exit 1; }
[[ -s "$GEN_DEFINE_RAW_EXE" ]] || { echo "stage07 linker produced no define executable output" >&2; exit 1; }
[[ -s "$GEN_DEFINE_HEX_RAW_EXE" ]] || { echo "stage07 linker produced no define-hex executable output" >&2; exit 1; }
[[ -s "$GEN_ENUM_RAW_EXE" ]] || { echo "stage07 linker produced no enum executable output" >&2; exit 1; }
[[ -s "$GEN_PROTOTYPE_RAW_EXE" ]] || { echo "stage07 linker produced no prototype executable output" >&2; exit 1; }
[[ -s "$GEN_INCLUDE_RAW_EXE" ]] || { echo "stage07 linker produced no include executable output" >&2; exit 1; }
link_forth_with_libc "$GEN_OBJ" "$GEN_EXE" "$WORKDIR/stage3-link.run.log"
link_forth_with_libc "$GEN_RET_OBJ" "$GEN_RET_EXE" "$WORKDIR/stage3-link-ret.run.log"
link_forth_with_libc "$GEN_EXPR_OBJ" "$GEN_EXPR_EXE" "$WORKDIR/stage3-link-expr.run.log"
link_forth_with_libc "$GEN_LOCAL_OBJ" "$GEN_LOCAL_EXE" "$WORKDIR/stage3-link-local.run.log"
link_forth_with_libc "$GEN_REL_OBJ" "$GEN_REL_EXE" "$WORKDIR/stage3-link-rel.run.log"
link_forth_with_libc "$GEN_IF_TRUE_OBJ" "$GEN_IF_TRUE_EXE" "$WORKDIR/stage3-link-if-true.run.log"
link_forth_with_libc "$GEN_IF_FALSE_OBJ" "$GEN_IF_FALSE_EXE" "$WORKDIR/stage3-link-if-false.run.log"
link_forth_with_libc "$GEN_WHILE_OBJ" "$GEN_WHILE_EXE" "$WORKDIR/stage3-link-while.run.log"
link_forth_with_libc "$GEN_TWO_LOCALS_OBJ" "$GEN_TWO_LOCALS_EXE" "$WORKDIR/stage3-link-two-locals.run.log"
link_forth_with_libc "$GEN_HELPER_OBJ" "$GEN_HELPER_EXE" "$WORKDIR/stage3-link-helper.run.log"
link_forth_with_libc "$GEN_HELPER_ARG_OBJ" "$GEN_HELPER_ARG_EXE" "$WORKDIR/stage3-link-helper-arg.run.log"
link_forth_with_libc "$GEN_HELPER_LOCAL_OBJ" "$GEN_HELPER_LOCAL_EXE" "$WORKDIR/stage3-link-helper-local.run.log"
link_forth_with_libc "$GEN_MAIN_LOCAL_HELPER_OBJ" "$GEN_MAIN_LOCAL_HELPER_EXE" "$WORKDIR/stage3-link-main-local-helper.run.log"
link_forth_with_libc "$GEN_HELPER_TWO_ARGS_OBJ" "$GEN_HELPER_TWO_ARGS_EXE" "$WORKDIR/stage3-link-helper-two-args.run.log"
link_forth_with_libc "$GEN_HELPER_TWO_ARGS_IF_OBJ" "$GEN_HELPER_TWO_ARGS_IF_EXE" "$WORKDIR/stage3-link-helper-two-args-if.run.log"
link_forth_with_libc "$GEN_MULTI_FUNC_OBJ" "$GEN_MULTI_FUNC_EXE" "$WORKDIR/stage3-link-multi-func.run.log"
link_forth_with_libc "$GEN_FOR_LOOP_OBJ" "$GEN_FOR_LOOP_EXE" "$WORKDIR/stage3-link-for-loop.run.log"
link_forth_with_libc "$GEN_NESTED_IF_OBJ" "$GEN_NESTED_IF_EXE" "$WORKDIR/stage3-link-nested-if.run.log"
link_forth_with_libc "$GEN_BREAK_CONTINUE_OBJ" "$GEN_BREAK_CONTINUE_EXE" "$WORKDIR/stage3-link-break-continue.run.log"
link_forth_with_libc "$GEN_GENERAL_NAMES_OBJ" "$GEN_GENERAL_NAMES_EXE" "$WORKDIR/stage3-link-general-names.run.log"
link_forth_with_libc "$GEN_COMPLEX_EXPR_OBJ" "$GEN_COMPLEX_EXPR_EXE" "$WORKDIR/stage3-link-complex-expr.run.log"
GEN_CHAR_TYPE_EXE="$WORKDIR/min_char_type.generated.s32x"
GEN_CHAR_LITERAL_EXE="$WORKDIR/min_char_literal.generated.s32x"
GEN_LOCAL_ARRAY_EXE="$WORKDIR/min_local_array.generated.s32x"
GEN_CHAR_ARRAY_EXE="$WORKDIR/min_char_array.generated.s32x"
GEN_STRING_LIT_EXE="$WORKDIR/min_string_lit.generated.s32x"
GEN_POINTER_EXE="$WORKDIR/min_pointer.generated.s32x"
GEN_GLOBAL_EXE="$WORKDIR/min_global.generated.s32x"
GEN_GLOBAL_ARRAY_EXE="$WORKDIR/min_global_array.generated.s32x"
link_forth_with_libc "$GEN_CHAR_TYPE_OBJ" "$GEN_CHAR_TYPE_EXE" "$WORKDIR/stage3-link-char-type.run.log"
link_forth_with_libc "$GEN_CHAR_LITERAL_OBJ" "$GEN_CHAR_LITERAL_EXE" "$WORKDIR/stage3-link-char-literal.run.log"
link_forth_with_libc "$GEN_LOCAL_ARRAY_OBJ" "$GEN_LOCAL_ARRAY_EXE" "$WORKDIR/stage3-link-local-array.run.log"
link_forth_with_libc "$GEN_CHAR_ARRAY_OBJ" "$GEN_CHAR_ARRAY_EXE" "$WORKDIR/stage3-link-char-array.run.log"
link_forth_with_libc "$GEN_STRING_LIT_OBJ" "$GEN_STRING_LIT_EXE" "$WORKDIR/stage3-link-string-lit.run.log"
link_forth_with_libc "$GEN_POINTER_OBJ" "$GEN_POINTER_EXE" "$WORKDIR/stage3-link-pointer.run.log"
link_forth_with_libc "$GEN_GLOBAL_OBJ" "$GEN_GLOBAL_EXE" "$WORKDIR/stage3-link-global.run.log"
link_forth_with_libc "$GEN_GLOBAL_ARRAY_OBJ" "$GEN_GLOBAL_ARRAY_EXE" "$WORKDIR/stage3-link-global-array.run.log"
GEN_PTR_ARITH_EXE="$WORKDIR/min_ptr_arith.generated.s32x"
link_forth_with_libc "$GEN_PTR_ARITH_OBJ" "$GEN_PTR_ARITH_EXE" "$WORKDIR/stage3-link-ptr-arith.run.log"
GEN_SHORT_CIRCUIT_EXE="$WORKDIR/min_short_circuit.generated.s32x"
link_forth_with_libc "$GEN_SHORT_CIRCUIT_OBJ" "$GEN_SHORT_CIRCUIT_EXE" "$WORKDIR/stage3-link-short-circuit.run.log"
GEN_TYPEDEF_EXE="$WORKDIR/min_typedef.generated.s32x"
GEN_TYPEDEF_STRUCT_EXE="$WORKDIR/min_typedef_struct.generated.s32x"
GEN_STRUCT_BASIC_EXE="$WORKDIR/min_struct_basic.generated.s32x"
GEN_STRUCT_ARROW_EXE="$WORKDIR/min_struct_arrow.generated.s32x"
GEN_STRUCT_ARRAY_EXE="$WORKDIR/min_struct_array.generated.s32x"
GEN_SIZEOF_EXE="$WORKDIR/min_sizeof.generated.s32x"
GEN_SIZEOF_STRUCT_EXE="$WORKDIR/min_sizeof_struct.generated.s32x"
GEN_CAST_EXE="$WORKDIR/min_cast.generated.s32x"
link_forth_with_libc "$GEN_TYPEDEF_OBJ" "$GEN_TYPEDEF_EXE" "$WORKDIR/stage3-link-typedef.run.log"
link_forth_with_libc "$GEN_TYPEDEF_STRUCT_OBJ" "$GEN_TYPEDEF_STRUCT_EXE" "$WORKDIR/stage3-link-typedef-struct.run.log"
link_forth_with_libc "$GEN_STRUCT_BASIC_OBJ" "$GEN_STRUCT_BASIC_EXE" "$WORKDIR/stage3-link-struct-basic.run.log"
link_forth_with_libc "$GEN_STRUCT_ARROW_OBJ" "$GEN_STRUCT_ARROW_EXE" "$WORKDIR/stage3-link-struct-arrow.run.log"
link_forth_with_libc "$GEN_STRUCT_ARRAY_OBJ" "$GEN_STRUCT_ARRAY_EXE" "$WORKDIR/stage3-link-struct-array.run.log"
link_forth_with_libc "$GEN_SIZEOF_OBJ" "$GEN_SIZEOF_EXE" "$WORKDIR/stage3-link-sizeof.run.log"
link_forth_with_libc "$GEN_SIZEOF_STRUCT_OBJ" "$GEN_SIZEOF_STRUCT_EXE" "$WORKDIR/stage3-link-sizeof-struct.run.log"
link_forth_with_libc "$GEN_CAST_OBJ" "$GEN_CAST_EXE" "$WORKDIR/stage3-link-cast.run.log"
GEN_DEFINE_EXE="$WORKDIR/min_define.generated.s32x"
GEN_DEFINE_HEX_EXE="$WORKDIR/min_define_hex.generated.s32x"
GEN_ENUM_EXE="$WORKDIR/min_enum.generated.s32x"
GEN_PROTOTYPE_EXE="$WORKDIR/min_prototype.generated.s32x"
GEN_INCLUDE_EXE="$WORKDIR/min_include.generated.s32x"
link_forth_with_libc "$GEN_DEFINE_OBJ" "$GEN_DEFINE_EXE" "$WORKDIR/stage3-link-define.run.log"
link_forth_with_libc "$GEN_DEFINE_HEX_OBJ" "$GEN_DEFINE_HEX_EXE" "$WORKDIR/stage3-link-define-hex.run.log"
link_forth_with_libc "$GEN_ENUM_OBJ" "$GEN_ENUM_EXE" "$WORKDIR/stage3-link-enum.run.log"
link_forth_with_libc "$GEN_PROTOTYPE_OBJ" "$GEN_PROTOTYPE_EXE" "$WORKDIR/stage3-link-prototype.run.log"
link_forth_with_libc "$GEN_INCLUDE_OBJ" "$GEN_INCLUDE_EXE" "$WORKDIR/stage3-link-include.run.log"
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
WHILE_RC=0
run_exe_any_rc "$GEN_WHILE_EXE" "$WORKDIR/gen-while.run.log" || WHILE_RC=$?
if [[ "$WHILE_RC" -ne 0 ]]; then
    echo "while test executable had unexpected exit code: $WHILE_RC (expected 0)" >&2
    tail -n 60 "$WORKDIR/gen-while.run.log" >&2
    exit 1
fi
TWO_LOCALS_RC=0
run_exe_any_rc "$GEN_TWO_LOCALS_EXE" "$WORKDIR/gen-two-locals.run.log" || TWO_LOCALS_RC=$?
if [[ "$TWO_LOCALS_RC" -ne 6 ]]; then
    echo "two-locals test executable had unexpected exit code: $TWO_LOCALS_RC (expected 6)" >&2
    tail -n 60 "$WORKDIR/gen-two-locals.run.log" >&2
    exit 1
fi
HELPER_RC=0
run_exe_any_rc "$GEN_HELPER_EXE" "$WORKDIR/gen-helper.run.log" || HELPER_RC=$?
if [[ "$HELPER_RC" -ne 11 ]]; then
    echo "helper-call test executable had unexpected exit code: $HELPER_RC (expected 11)" >&2
    tail -n 60 "$WORKDIR/gen-helper.run.log" >&2
    exit 1
fi
HELPER_ARG_RC=0
run_exe_any_rc "$GEN_HELPER_ARG_EXE" "$WORKDIR/gen-helper-arg.run.log" || HELPER_ARG_RC=$?
if [[ "$HELPER_ARG_RC" -ne 13 ]]; then
    echo "helper-arg test executable had unexpected exit code: $HELPER_ARG_RC (expected 13)" >&2
    tail -n 60 "$WORKDIR/gen-helper-arg.run.log" >&2
    exit 1
fi
HELPER_LOCAL_RC=0
run_exe_any_rc "$GEN_HELPER_LOCAL_EXE" "$WORKDIR/gen-helper-local.run.log" || HELPER_LOCAL_RC=$?
if [[ "$HELPER_LOCAL_RC" -ne 15 ]]; then
    echo "helper-local test executable had unexpected exit code: $HELPER_LOCAL_RC (expected 15)" >&2
    tail -n 60 "$WORKDIR/gen-helper-local.run.log" >&2
    exit 1
fi
MAIN_LOCAL_HELPER_RC=0
run_exe_any_rc "$GEN_MAIN_LOCAL_HELPER_EXE" "$WORKDIR/gen-main-local-helper.run.log" || MAIN_LOCAL_HELPER_RC=$?
if [[ "$MAIN_LOCAL_HELPER_RC" -ne 12 ]]; then
    echo "main-local-helper test executable had unexpected exit code: $MAIN_LOCAL_HELPER_RC (expected 12)" >&2
    tail -n 60 "$WORKDIR/gen-main-local-helper.run.log" >&2
    exit 1
fi
HELPER_TWO_ARGS_RC=0
run_exe_any_rc "$GEN_HELPER_TWO_ARGS_EXE" "$WORKDIR/gen-helper-two-args.run.log" || HELPER_TWO_ARGS_RC=$?
if [[ "$HELPER_TWO_ARGS_RC" -ne 14 ]]; then
    echo "helper-two-args test executable had unexpected exit code: $HELPER_TWO_ARGS_RC (expected 14)" >&2
    tail -n 60 "$WORKDIR/gen-helper-two-args.run.log" >&2
    exit 1
fi
HELPER_TWO_ARGS_IF_RC=0
run_exe_any_rc "$GEN_HELPER_TWO_ARGS_IF_EXE" "$WORKDIR/gen-helper-two-args-if.run.log" || HELPER_TWO_ARGS_IF_RC=$?
if [[ "$HELPER_TWO_ARGS_IF_RC" -ne 5 ]]; then
    echo "helper-two-args-if test executable had unexpected exit code: $HELPER_TWO_ARGS_IF_RC (expected 5)" >&2
    tail -n 60 "$WORKDIR/gen-helper-two-args-if.run.log" >&2
    exit 1
fi
MULTI_FUNC_RC=0
run_exe_any_rc "$GEN_MULTI_FUNC_EXE" "$WORKDIR/gen-multi-func.run.log" || MULTI_FUNC_RC=$?
if [[ "$MULTI_FUNC_RC" -ne 8 ]]; then
    echo "multi-func test executable had unexpected exit code: $MULTI_FUNC_RC (expected 8)" >&2
    tail -n 60 "$WORKDIR/gen-multi-func.run.log" >&2
    exit 1
fi
FOR_LOOP_RC=0
run_exe_any_rc "$GEN_FOR_LOOP_EXE" "$WORKDIR/gen-for-loop.run.log" || FOR_LOOP_RC=$?
if [[ "$FOR_LOOP_RC" -ne 15 ]]; then
    echo "for-loop test executable had unexpected exit code: $FOR_LOOP_RC (expected 15)" >&2
    tail -n 60 "$WORKDIR/gen-for-loop.run.log" >&2
    exit 1
fi
NESTED_IF_RC=0
run_exe_any_rc "$GEN_NESTED_IF_EXE" "$WORKDIR/gen-nested-if.run.log" || NESTED_IF_RC=$?
if [[ "$NESTED_IF_RC" -ne 3 ]]; then
    echo "nested-if test executable had unexpected exit code: $NESTED_IF_RC (expected 3)" >&2
    tail -n 60 "$WORKDIR/gen-nested-if.run.log" >&2
    exit 1
fi
BREAK_CONTINUE_RC=0
run_exe_any_rc "$GEN_BREAK_CONTINUE_EXE" "$WORKDIR/gen-break-continue.run.log" || BREAK_CONTINUE_RC=$?
if [[ "$BREAK_CONTINUE_RC" -ne 9 ]]; then
    echo "break-continue test executable had unexpected exit code: $BREAK_CONTINUE_RC (expected 9)" >&2
    tail -n 60 "$WORKDIR/gen-break-continue.run.log" >&2
    exit 1
fi
GENERAL_NAMES_RC=0
run_exe_any_rc "$GEN_GENERAL_NAMES_EXE" "$WORKDIR/gen-general-names.run.log" || GENERAL_NAMES_RC=$?
if [[ "$GENERAL_NAMES_RC" -ne 9 ]]; then
    echo "general-names test executable had unexpected exit code: $GENERAL_NAMES_RC (expected 9)" >&2
    tail -n 60 "$WORKDIR/gen-general-names.run.log" >&2
    exit 1
fi
COMPLEX_EXPR_RC=0
run_exe_any_rc "$GEN_COMPLEX_EXPR_EXE" "$WORKDIR/gen-complex-expr.run.log" || COMPLEX_EXPR_RC=$?
if [[ "$COMPLEX_EXPR_RC" -ne 15 ]]; then
    echo "complex-expr test executable had unexpected exit code: $COMPLEX_EXPR_RC (expected 15)" >&2
    tail -n 60 "$WORKDIR/gen-complex-expr.run.log" >&2
    exit 1
fi
CHAR_TYPE_RC=0
run_exe_any_rc "$GEN_CHAR_TYPE_EXE" "$WORKDIR/gen-char-type.run.log" || CHAR_TYPE_RC=$?
if [[ "$CHAR_TYPE_RC" -ne 1 ]]; then
    echo "char-type test executable had unexpected exit code: $CHAR_TYPE_RC (expected 1)" >&2
    tail -n 60 "$WORKDIR/gen-char-type.run.log" >&2
    exit 1
fi
CHAR_LITERAL_RC=0
run_exe_any_rc "$GEN_CHAR_LITERAL_EXE" "$WORKDIR/gen-char-literal.run.log" || CHAR_LITERAL_RC=$?
if [[ "$CHAR_LITERAL_RC" -ne 3 ]]; then
    echo "char-literal test executable had unexpected exit code: $CHAR_LITERAL_RC (expected 3)" >&2
    tail -n 60 "$WORKDIR/gen-char-literal.run.log" >&2
    exit 1
fi
LOCAL_ARRAY_RC=0
run_exe_any_rc "$GEN_LOCAL_ARRAY_EXE" "$WORKDIR/gen-local-array.run.log" || LOCAL_ARRAY_RC=$?
if [[ "$LOCAL_ARRAY_RC" -ne 50 ]]; then
    echo "local-array test executable had unexpected exit code: $LOCAL_ARRAY_RC (expected 50)" >&2
    tail -n 60 "$WORKDIR/gen-local-array.run.log" >&2
    exit 1
fi
CHAR_ARRAY_RC=0
run_exe_any_rc "$GEN_CHAR_ARRAY_EXE" "$WORKDIR/gen-char-array.run.log" || CHAR_ARRAY_RC=$?
if [[ "$CHAR_ARRAY_RC" -ne 6 ]]; then
    echo "char-array test executable had unexpected exit code: $CHAR_ARRAY_RC (expected 6)" >&2
    tail -n 60 "$WORKDIR/gen-char-array.run.log" >&2
    exit 1
fi
STRING_LIT_RC=0
run_exe_any_rc "$GEN_STRING_LIT_EXE" "$WORKDIR/gen-string-lit.run.log" || STRING_LIT_RC=$?
if [[ "$STRING_LIT_RC" -ne 5 ]]; then
    echo "string-lit test executable had unexpected exit code: $STRING_LIT_RC (expected 5)" >&2
    tail -n 60 "$WORKDIR/gen-string-lit.run.log" >&2
    exit 1
fi
POINTER_RC=0
run_exe_any_rc "$GEN_POINTER_EXE" "$WORKDIR/gen-pointer.run.log" || POINTER_RC=$?
if [[ "$POINTER_RC" -ne 7 ]]; then
    echo "pointer test executable had unexpected exit code: $POINTER_RC (expected 7)" >&2
    tail -n 60 "$WORKDIR/gen-pointer.run.log" >&2
    exit 1
fi
GLOBAL_RC=0
run_exe_any_rc "$GEN_GLOBAL_EXE" "$WORKDIR/gen-global.run.log" || GLOBAL_RC=$?
if [[ "$GLOBAL_RC" -ne 10 ]]; then
    echo "global test executable had unexpected exit code: $GLOBAL_RC (expected 10)" >&2
    tail -n 60 "$WORKDIR/gen-global.run.log" >&2
    exit 1
fi
GLOBAL_ARRAY_RC=0
run_exe_any_rc "$GEN_GLOBAL_ARRAY_EXE" "$WORKDIR/gen-global-array.run.log" || GLOBAL_ARRAY_RC=$?
if [[ "$GLOBAL_ARRAY_RC" -ne 8 ]]; then
    echo "global-array test executable had unexpected exit code: $GLOBAL_ARRAY_RC (expected 8)" >&2
    tail -n 60 "$WORKDIR/gen-global-array.run.log" >&2
    exit 1
fi
PTR_ARITH_RC=0
run_exe_any_rc "$GEN_PTR_ARITH_EXE" "$WORKDIR/gen-ptr-arith.run.log" || PTR_ARITH_RC=$?
if [[ "$PTR_ARITH_RC" -ne 0 ]]; then
    echo "ptr-arith test executable had unexpected exit code: $PTR_ARITH_RC (expected 0)" >&2
    tail -n 60 "$WORKDIR/gen-ptr-arith.run.log" >&2
    exit 1
fi
SHORT_CIRCUIT_RC=0
run_exe_any_rc "$GEN_SHORT_CIRCUIT_EXE" "$WORKDIR/gen-short-circuit.run.log" || SHORT_CIRCUIT_RC=$?
if [[ "$SHORT_CIRCUIT_RC" -ne 0 ]]; then
    echo "short-circuit test executable had unexpected exit code: $SHORT_CIRCUIT_RC (expected 0)" >&2
    tail -n 60 "$WORKDIR/gen-short-circuit.run.log" >&2
    exit 1
fi
TYPEDEF_RC=0
run_exe_any_rc "$GEN_TYPEDEF_EXE" "$WORKDIR/gen-typedef.run.log" || TYPEDEF_RC=$?
if [[ "$TYPEDEF_RC" -ne 42 ]]; then
    echo "typedef test executable had unexpected exit code: $TYPEDEF_RC (expected 42)" >&2
    tail -n 60 "$WORKDIR/gen-typedef.run.log" >&2
    exit 1
fi
TYPEDEF_STRUCT_RC=0
run_exe_any_rc "$GEN_TYPEDEF_STRUCT_EXE" "$WORKDIR/gen-typedef-struct.run.log" || TYPEDEF_STRUCT_RC=$?
if [[ "$TYPEDEF_STRUCT_RC" -ne 3 ]]; then
    echo "typedef-struct test executable had unexpected exit code: $TYPEDEF_STRUCT_RC (expected 3)" >&2
    tail -n 60 "$WORKDIR/gen-typedef-struct.run.log" >&2
    exit 1
fi
STRUCT_BASIC_RC=0
run_exe_any_rc "$GEN_STRUCT_BASIC_EXE" "$WORKDIR/gen-struct-basic.run.log" || STRUCT_BASIC_RC=$?
if [[ "$STRUCT_BASIC_RC" -ne 7 ]]; then
    echo "struct-basic test executable had unexpected exit code: $STRUCT_BASIC_RC (expected 7)" >&2
    tail -n 60 "$WORKDIR/gen-struct-basic.run.log" >&2
    exit 1
fi
STRUCT_ARROW_RC=0
run_exe_any_rc "$GEN_STRUCT_ARROW_EXE" "$WORKDIR/gen-struct-arrow.run.log" || STRUCT_ARROW_RC=$?
if [[ "$STRUCT_ARROW_RC" -ne 10 ]]; then
    echo "struct-arrow test executable had unexpected exit code: $STRUCT_ARROW_RC (expected 10)" >&2
    tail -n 60 "$WORKDIR/gen-struct-arrow.run.log" >&2
    exit 1
fi
STRUCT_ARRAY_RC=0
run_exe_any_rc "$GEN_STRUCT_ARRAY_EXE" "$WORKDIR/gen-struct-array.run.log" || STRUCT_ARRAY_RC=$?
if [[ "$STRUCT_ARRAY_RC" -ne 99 ]]; then
    echo "struct-array test executable had unexpected exit code: $STRUCT_ARRAY_RC (expected 99)" >&2
    tail -n 60 "$WORKDIR/gen-struct-array.run.log" >&2
    exit 1
fi
SIZEOF_RC=0
run_exe_any_rc "$GEN_SIZEOF_EXE" "$WORKDIR/gen-sizeof.run.log" || SIZEOF_RC=$?
if [[ "$SIZEOF_RC" -ne 4 ]]; then
    echo "sizeof test executable had unexpected exit code: $SIZEOF_RC (expected 4)" >&2
    tail -n 60 "$WORKDIR/gen-sizeof.run.log" >&2
    exit 1
fi
SIZEOF_STRUCT_RC=0
run_exe_any_rc "$GEN_SIZEOF_STRUCT_EXE" "$WORKDIR/gen-sizeof-struct.run.log" || SIZEOF_STRUCT_RC=$?
if [[ "$SIZEOF_STRUCT_RC" -ne 8 ]]; then
    echo "sizeof-struct test executable had unexpected exit code: $SIZEOF_STRUCT_RC (expected 8)" >&2
    tail -n 60 "$WORKDIR/gen-sizeof-struct.run.log" >&2
    exit 1
fi
CAST_RC=0
run_exe_any_rc "$GEN_CAST_EXE" "$WORKDIR/gen-cast.run.log" || CAST_RC=$?
if [[ "$CAST_RC" -ne 1 ]]; then
    echo "cast test executable had unexpected exit code: $CAST_RC (expected 1)" >&2
    tail -n 60 "$WORKDIR/gen-cast.run.log" >&2
    exit 1
fi
DEFINE_RC=0
run_exe_any_rc "$GEN_DEFINE_EXE" "$WORKDIR/gen-define.run.log" || DEFINE_RC=$?
if [[ "$DEFINE_RC" -ne 10 ]]; then
    echo "define test executable had unexpected exit code: $DEFINE_RC (expected 10)" >&2
    tail -n 60 "$WORKDIR/gen-define.run.log" >&2
    exit 1
fi
DEFINE_HEX_RC=0
run_exe_any_rc "$GEN_DEFINE_HEX_EXE" "$WORKDIR/gen-define-hex.run.log" || DEFINE_HEX_RC=$?
if [[ "$DEFINE_HEX_RC" -ne 255 ]]; then
    echo "define-hex test executable had unexpected exit code: $DEFINE_HEX_RC (expected 255)" >&2
    tail -n 60 "$WORKDIR/gen-define-hex.run.log" >&2
    exit 1
fi
ENUM_RC=0
run_exe_any_rc "$GEN_ENUM_EXE" "$WORKDIR/gen-enum.run.log" || ENUM_RC=$?
if [[ "$ENUM_RC" -ne 6 ]]; then
    echo "enum test executable had unexpected exit code: $ENUM_RC (expected 6)" >&2
    tail -n 60 "$WORKDIR/gen-enum.run.log" >&2
    exit 1
fi
PROTOTYPE_RC=0
run_exe_any_rc "$GEN_PROTOTYPE_EXE" "$WORKDIR/gen-prototype.run.log" || PROTOTYPE_RC=$?
if [[ "$PROTOTYPE_RC" -ne 42 ]]; then
    echo "prototype test executable had unexpected exit code: $PROTOTYPE_RC (expected 42)" >&2
    tail -n 60 "$WORKDIR/gen-prototype.run.log" >&2
    exit 1
fi
INCLUDE_RC=0
run_exe_any_rc "$GEN_INCLUDE_EXE" "$WORKDIR/gen-include.run.log" || INCLUDE_RC=$?
if [[ "$INCLUDE_RC" -ne 7 ]]; then
    echo "include test executable had unexpected exit code: $INCLUDE_RC (expected 7)" >&2
    tail -n 60 "$WORKDIR/gen-include.run.log" >&2
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
echo "While-test C: $TEST_WHILE_IN"
echo "Two-locals C: $TEST_TWO_LOCALS_IN"
echo "Helper-call C: $TEST_HELPER_IN"
echo "Helper-arg C: $TEST_HELPER_ARG_IN"
echo "Helper-local C: $TEST_HELPER_LOCAL_IN"
echo "Main-local-helper C: $TEST_MAIN_LOCAL_HELPER_IN"
echo "Helper-two-args C: $TEST_HELPER_TWO_ARGS_IN"
echo "Helper-two-args-if C: $TEST_HELPER_TWO_ARGS_IF_IN"
echo "Generated asm: $GEN_ASM"
echo "Generated return asm: $GEN_RET_ASM"
echo "Generated expr asm: $GEN_EXPR_ASM"
echo "Generated local asm: $GEN_LOCAL_ASM"
echo "Generated relational asm: $GEN_REL_ASM"
echo "Generated if-true asm: $GEN_IF_TRUE_ASM"
echo "Generated if-false asm: $GEN_IF_FALSE_ASM"
echo "Generated while asm: $GEN_WHILE_ASM"
echo "Generated two-locals asm: $GEN_TWO_LOCALS_ASM"
echo "Generated helper-call asm: $GEN_HELPER_ASM"
echo "Generated helper-arg asm: $GEN_HELPER_ARG_ASM"
echo "Generated helper-local asm: $GEN_HELPER_LOCAL_ASM"
echo "Generated main-local-helper asm: $GEN_MAIN_LOCAL_HELPER_ASM"
echo "Generated helper-two-args asm: $GEN_HELPER_TWO_ARGS_ASM"
echo "Generated helper-two-args-if asm: $GEN_HELPER_TWO_ARGS_IF_ASM"
echo "Generated raw exe (stage07): $GEN_RAW_EXE"
echo "Generated return raw exe (stage07): $GEN_RET_RAW_EXE"
echo "Generated expr raw exe (stage07): $GEN_EXPR_RAW_EXE"
echo "Generated local raw exe (stage07): $GEN_LOCAL_RAW_EXE"
echo "Generated relational raw exe (stage07): $GEN_REL_RAW_EXE"
echo "Generated if-true raw exe (stage07): $GEN_IF_TRUE_RAW_EXE"
echo "Generated if-false raw exe (stage07): $GEN_IF_FALSE_RAW_EXE"
echo "Generated while raw exe (stage07): $GEN_WHILE_RAW_EXE"
echo "Generated two-locals raw exe (stage07): $GEN_TWO_LOCALS_RAW_EXE"
echo "Generated helper-call raw exe (stage07): $GEN_HELPER_RAW_EXE"
echo "Generated helper-arg raw exe (stage07): $GEN_HELPER_ARG_RAW_EXE"
echo "Generated helper-local raw exe (stage07): $GEN_HELPER_LOCAL_RAW_EXE"
echo "Generated main-local-helper raw exe (stage07): $GEN_MAIN_LOCAL_HELPER_RAW_EXE"
echo "Generated helper-two-args raw exe (stage07): $GEN_HELPER_TWO_ARGS_RAW_EXE"
echo "Generated helper-two-args-if raw exe (stage07): $GEN_HELPER_TWO_ARGS_IF_RAW_EXE"
echo "Generated exe: $GEN_EXE"
echo "Generated return exe: $GEN_RET_EXE"
echo "Generated expr exe: $GEN_EXPR_EXE"
echo "Generated local exe: $GEN_LOCAL_EXE"
echo "Generated relational exe: $GEN_REL_EXE"
echo "Generated if-true exe: $GEN_IF_TRUE_EXE"
echo "Generated if-false exe: $GEN_IF_FALSE_EXE"
echo "Generated while exe: $GEN_WHILE_EXE"
echo "Generated two-locals exe: $GEN_TWO_LOCALS_EXE"
echo "Generated helper-call exe: $GEN_HELPER_EXE"
echo "Generated helper-arg exe: $GEN_HELPER_ARG_EXE"
echo "Generated helper-local exe: $GEN_HELPER_LOCAL_EXE"
echo "Generated main-local-helper exe: $GEN_MAIN_LOCAL_HELPER_EXE"
echo "Generated helper-two-args exe: $GEN_HELPER_TWO_ARGS_EXE"
echo "Generated helper-two-args-if exe: $GEN_HELPER_TWO_ARGS_IF_EXE"
echo "Generated multi-func exe: $GEN_MULTI_FUNC_EXE"
echo "Generated for-loop exe: $GEN_FOR_LOOP_EXE"
echo "Generated nested-if exe: $GEN_NESTED_IF_EXE"
echo "Generated break-continue exe: $GEN_BREAK_CONTINUE_EXE"
echo "Generated general-names exe: $GEN_GENERAL_NAMES_EXE"
echo "Generated complex-expr exe: $GEN_COMPLEX_EXPR_EXE"
echo "Emulator: $EMU"
echo "Artifacts: $WORKDIR"
