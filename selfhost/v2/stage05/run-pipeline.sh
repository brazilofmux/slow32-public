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
EMU_EXPLICIT=0

CC_FTH="${SELFHOST_CC_FTH:-$ROOT_DIR/selfhost/v2/stage04/cc.fth}"
ASM_FTH="${SELFHOST_ASM_FTH:-$ROOT_DIR/selfhost/v2/stage01/asm.fth}"
LINK_FTH="${SELFHOST_LINK_FTH:-$ROOT_DIR/selfhost/v2/stage03/link.fth}"
AR_FTH="${SELFHOST_AR_FTH:-$ROOT_DIR/selfhost/v2/stage02/ar.fth}"

TEST_DIR="${SELFHOST_TEST_DIR:-$ROOT_DIR/selfhost/v2/stage04/tests}"
VALIDATION_DIR="${SELFHOST_VALIDATION_DIR:-$ROOT_DIR/selfhost/v2/stage04/validation}"

MODE="baseline"
TEST_NAME="test3"
KEEP_ARTIFACTS=0

choose_default_emu() {
    if [[ -x "$ROOT_DIR/tools/emulator/slow32-fast" ]]; then
        printf '%s\n' "$ROOT_DIR/tools/emulator/slow32-fast"
        return
    fi
    if [[ -x "$ROOT_DIR/selfhost/v2/stage00/s32-emu" ]]; then
        printf '%s\n' "$ROOT_DIR/selfhost/v2/stage00/s32-emu"
        return
    fi
    if [[ -x "$ROOT_DIR/tools/emulator/slow32" ]]; then
        printf '%s\n' "$ROOT_DIR/tools/emulator/slow32"
        return
    fi
    printf '%s\n' "$ROOT_DIR/tools/emulator/slow32"
}

usage() {
    cat <<USAGE
Usage: $0 [--mode baseline|progressive-as|progressive-as-ar|progressive-as-ar-scan|stage5-as-bisect|stage6-ar-smoke|stage6-ar-rc-smoke|stage6-ar-tx-smoke|stage6-ar-scan-smoke|stage6-ar-asm-diff|stage6-utility-smoke] [--test <name>] [--emu <path>] [--keep-artifacts]

Modes:
  baseline          Stage4 cc.fth + Stage1 asm.fth + Stage3 link.fth
  progressive-as    Stage4 cc.fth + Stage5 s32-as.c (for .s -> .s32o) + Stage3 link.fth
  progressive-as-ar Stage4 cc.fth + Stage5 s32-as.c + Stage6 s32-ar.c smoke-check + Stage3 link.fth
  progressive-as-ar-scan Same as progressive-as-ar, but runs s32-ar with opt-in symbol scan flag
  stage5-as-bisect  Build stage5 s32-as.c, compile a target .c to .s, and isolate first source line that self-built assembler rejects
  stage6-ar-smoke   Build stage5 assembler, then stage6 s32-ar.c with stage5 assembler; run archive smoke only
  stage6-ar-rc-smoke Build stage5 assembler, then stage6 s32-ar.c with stage5 assembler; verify replace-on-existing (rc) path
  stage6-ar-tx-smoke Build stage5 assembler, then stage6 s32-ar.c with stage5 assembler; verify list/extract (t/x) paths
  stage6-ar-scan-smoke Build stage5 assembler, then stage6 s32-ar-scan.c with stage5 assembler; run archive smoke with cmd=cs only
  stage6-ar-asm-diff Build stage5 assembler, assemble a validation .c with stage5 and forth, link both, and report first .s32x/.s32o byte diff
  stage6-utility-smoke Build a validation utility with stage5 and forth assemblers, require linked .s32x parity, then compare runtime output

Env overrides:
  SELFHOST_ROOT SELFHOST_EMU SELFHOST_KERNEL SELFHOST_PRELUDE
  SELFHOST_CC_FTH SELFHOST_ASM_FTH SELFHOST_LINK_FTH SELFHOST_AR_FTH
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
            EMU_EXPLICIT=1
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

if [[ "$EMU" != /* ]]; then
    EMU="$ROOT_DIR/$EMU"
fi

if [[ "$MODE" == "stage6-utility-smoke" && "$EMU_EXPLICIT" -eq 0 && -z "${SELFHOST_EMU:-}" ]]; then
    EMU="$(choose_default_emu)"
fi

case "$MODE" in
    baseline|progressive-as|progressive-as-ar|progressive-as-ar-scan|stage5-as-bisect|stage6-ar-smoke|stage6-ar-rc-smoke|stage6-ar-tx-smoke|stage6-ar-scan-smoke|stage6-ar-asm-diff|stage6-utility-smoke) ;;
    *)
        echo "Unknown mode: $MODE" >&2
        usage
        exit 2
        ;;
esac

for f in "$EMU" "$KERNEL" "$PRELUDE" "$CC_FTH" "$ASM_FTH" "$LINK_FTH" "$AR_FTH"; do
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
    timeout "${EXEC_TIMEOUT:-60}" "$EMU" "$exe" "$@" >"$log" 2>&1
    local rc=$?
    set -e
    if [[ "$rc" -eq 124 ]]; then
        echo "execution timed out: $exe" >&2
        tail -n 60 "$log" >&2
        return 1
    fi
    if grep -Eq "Execute fault|Memory fault|Write out of bounds or to protected memory|Unknown opcode|Unknown instruction" "$log"; then
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
    if grep -Eq "Execute fault|Memory fault|Write out of bounds or to protected memory|Unknown opcode|Unknown instruction" "$log"; then
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
    local src="${1:-$VALIDATION_DIR/s32-ar.c}"
    local asm_mode="${2:-stage5}"
    local asm="$WORKDIR/s32-ar.s"
    local obj="$WORKDIR/s32-ar.s32o"
    local exe="$WORKDIR/s32-ar.s32x"

    [[ -f "$src" ]] || { echo "Missing source: $src" >&2; return 1; }
    compile_c_stage4 "$src" "$asm" "$WORKDIR/s32-ar.cc.log"
    if [[ "$asm_mode" == "forth" ]]; then
        assemble_forth "$asm" "$obj" "$WORKDIR/s32-ar.as.log"
    else
        assemble_with_stage5 "$asm" "$obj" "$WORKDIR/s32-ar.as.log"
    fi
    link_forth "$obj" "$exe" "$WORKDIR/s32-ar.ld.log"
    STAGE6_AR_EXE="$exe"
}

stage6_archive_smoke() {
    local cmd="${1:-c}"
    local archive="$WORKDIR/smoke.s32a"
    local log="$WORKDIR/s32-ar.run.log"

    [[ -n "${STAGE6_AR_EXE:-}" ]] || { echo "stage6 archiver is not built" >&2; return 1; }
    run_exe "$STAGE6_AR_EXE" "$log" "$cmd" "$archive" "$TARGET_OBJ"
    [[ -s "$archive" ]] || { echo "stage6 archiver produced no output" >&2; return 1; }
}

stage6_archive_rc_smoke() {
    local archive="$WORKDIR/smoke-rc.s32a"
    local base_obj="$ROOT_DIR/runtime/divsi3.s32o"
    local keep_obj="$ROOT_DIR/runtime/crt0.s32o"
    local repl_src="$ROOT_DIR/runtime/builtins.s32o"
    local repl_obj="$WORKDIR/divsi3.s32o"
    local xdir="$WORKDIR/extract"
    local xlog="$WORKDIR/s32-ar.extract.log"
    local rc=0

    [[ -f "$base_obj" && -f "$keep_obj" && -f "$repl_src" ]] || {
        echo "Missing runtime object required for rc smoke" >&2
        return 1
    }
    cp "$repl_src" "$repl_obj"

    run_exe "$STAGE6_AR_EXE" "$WORKDIR/s32-ar.rc-create.log" "c" "$archive" "$base_obj" "$keep_obj"
    run_exe "$STAGE6_AR_EXE" "$WORKDIR/s32-ar.rc-replace.log" "rc" "$archive" "$repl_obj"
    [[ -s "$archive" ]] || { echo "stage6 archiver produced no output" >&2; return 1; }

    mkdir -p "$xdir"
    set +e
    (
        cd "$xdir"
        cat "$PRELUDE" "$AR_FTH" - <<FTH | timeout 120 "$EMU" "$KERNEL" >"$xlog" 2>&1
S" $archive" S" divsi3.s32o" AR-X1
BYE
FTH
    )
    rc=$?
    set -e
    if [[ "$rc" -ne 0 && "$rc" -ne 96 ]]; then
        echo "forth extraction failed after stage6 rc replace (rc=$rc)" >&2
        tail -n 40 "$xlog" >&2
        return 1
    fi
    [[ -s "$xdir/divsi3.s32o" ]] || { echo "replaced member missing after extract" >&2; return 1; }
    cmp -s "$repl_obj" "$xdir/divsi3.s32o" || {
        echo "replaced member content mismatch after rc" >&2
        return 1
    }
}

stage6_archive_tx_smoke() {
    local archive="$WORKDIR/smoke-tx.s32a"
    local obj_a="$WORKDIR/member-a.src"
    local obj_b="$WORKDIR/member-b.src"
    local obj_a_expected="$WORKDIR/member-a.expected"
    local xdir="$WORKDIR/extract-tx"
    local rc=0

    printf 'alpha-stage6-tx\n' > "$obj_a"
    printf 'beta-stage6-tx\n' > "$obj_b"
    cp "$obj_a" "$obj_a_expected"

    run_exe "$STAGE6_AR_EXE" "$WORKDIR/s32-ar.tx-create.log" "c" "$archive" "$obj_a" "$obj_b"
    run_exe "$STAGE6_AR_EXE" "$WORKDIR/s32-ar.tx-list.log" "t" "$archive"
    rm -f "$obj_a" "$obj_b"

    mkdir -p "$xdir"
    set +e
    (
        cd "$xdir"
        run_exe "$STAGE6_AR_EXE" "$WORKDIR/s32-ar.tx-extract.log" "x" "$archive" "member-a.src"
    )
    rc=$?
    set -e
    if [[ "$rc" -ne 0 ]]; then
        echo "stage6 x path failed" >&2
        return 1
    fi
    [[ -s "$xdir/member-a.src" ]] || { echo "stage6 x path did not extract member-a.src" >&2; return 1; }
    cmp -s "$obj_a_expected" "$xdir/member-a.src" || {
        echo "stage6 x extracted bytes mismatch" >&2
        return 1
    }
}

stage5_assemble_bisect() {
    local asm="$1"
    local out_obj="$2"
    local run_log="$3"
    local total first_bad lo hi mid tmp_asm tmp_obj

    if run_exe "$STAGE5_AS_EXE" "$run_log" "$asm" "$out_obj"; then
        [[ -s "$out_obj" ]] || { echo "stage5 assembler produced no output: $asm" >&2; return 1; }
        echo "stage5 assembler accepted full assembly"
        return 0
    fi

    total="$(wc -l < "$asm")"
    if [[ -z "$total" || "$total" -le 0 ]]; then
        echo "cannot bisect empty assembly file: $asm" >&2
        return 1
    fi

    lo=1
    hi="$total"
    first_bad="$total"
    while (( lo <= hi )); do
        mid=$(( (lo + hi) / 2 ))
        tmp_asm="$WORKDIR/bisect-$mid.s"
        tmp_obj="$WORKDIR/bisect-$mid.s32o"
        head -n "$mid" "$asm" > "$tmp_asm"
        if run_exe "$STAGE5_AS_EXE" "$WORKDIR/bisect-$mid.log" "$tmp_asm" "$tmp_obj"; then
            lo=$(( mid + 1 ))
        else
            first_bad="$mid"
            hi=$(( mid - 1 ))
        fi
    done

    echo "stage5 assembler first failing line: $first_bad"
    echo "---- failing line context ----"
    sed -n "$(( first_bad > 2 ? first_bad - 2 : 1 )),$(( first_bad + 2 ))p" "$asm"
    echo "------------------------------"
    return 1
}

resolve_validation_source() {
    local name="$1"
    if [[ "$name" = /* ]] || [[ "$name" == *.c ]]; then
        printf '%s\n' "$name"
    else
        printf '%s/%s.c\n' "$VALIDATION_DIR" "$name"
    fi
}

case "$MODE" in
    stage6-ar-smoke)
        build_stage5_assembler
        TARGET_OBJ="$ROOT_DIR/runtime/crt0.s32o"
        build_stage6_archiver "$VALIDATION_DIR/s32-ar.c" stage5
        stage6_archive_smoke
        ;;
    stage6-ar-rc-smoke)
        build_stage5_assembler
        build_stage6_archiver "$VALIDATION_DIR/s32-ar.c" stage5
        stage6_archive_rc_smoke
        ;;
    stage6-ar-tx-smoke)
        build_stage5_assembler
        build_stage6_archiver "$VALIDATION_DIR/s32-ar.c" stage5
        stage6_archive_tx_smoke
        ;;
    stage6-ar-scan-smoke)
        build_stage5_assembler
        TARGET_OBJ="$ROOT_DIR/runtime/crt0.s32o"
        build_stage6_archiver "$VALIDATION_DIR/s32-ar-scan.c" stage5
        stage6_archive_smoke "cs"
        ;;
    stage6-ar-asm-diff)
        build_stage5_assembler
        if [[ "$TEST_NAME" == "test3" ]]; then
            TARGET_SRC="$VALIDATION_DIR/s32-ar.c"
        else
            TARGET_SRC="$(resolve_validation_source "$TEST_NAME")"
        fi
        [[ -f "$TARGET_SRC" ]] || { echo "Missing test source: $TARGET_SRC" >&2; exit 1; }
        TARGET_ASM="$WORKDIR/s32-ar.s"
        TARGET_STAGE5_OBJ="$WORKDIR/s32-ar.stage5.s32o"
        TARGET_FORTH_OBJ="$WORKDIR/s32-ar.forth.s32o"
        TARGET_STAGE5_EXE="$WORKDIR/s32-ar.stage5.s32x"
        TARGET_FORTH_EXE="$WORKDIR/s32-ar.forth.s32x"
        compile_c_stage4 "$TARGET_SRC" "$TARGET_ASM" "$WORKDIR/s32-ar.cc.log"
        assemble_with_stage5 "$TARGET_ASM" "$TARGET_STAGE5_OBJ" "$WORKDIR/s32-ar.stage5.as.log"
        assemble_forth "$TARGET_ASM" "$TARGET_FORTH_OBJ" "$WORKDIR/s32-ar.forth.as.log"
        link_forth "$TARGET_STAGE5_OBJ" "$TARGET_STAGE5_EXE" "$WORKDIR/s32-ar.stage5.ld.log"
        link_forth "$TARGET_FORTH_OBJ" "$TARGET_FORTH_EXE" "$WORKDIR/s32-ar.forth.ld.log"

        if cmp -s "$TARGET_FORTH_EXE" "$TARGET_STAGE5_EXE"; then
            if cmp -s "$TARGET_FORTH_OBJ" "$TARGET_STAGE5_OBJ"; then
                echo "OK: stage5 and forth assemblers produced identical .s32o/.s32x"
            else
                echo "OK: linked .s32x is identical (object metadata differs)"
            fi
        else
            FIRST_EXE_DIFF="$(cmp -l "$TARGET_FORTH_EXE" "$TARGET_STAGE5_EXE" 2>/dev/null | head -n 1 || true)"
            FIRST_OBJ_DIFF="$(cmp -l "$TARGET_FORTH_OBJ" "$TARGET_STAGE5_OBJ" 2>/dev/null | head -n 1 || true)"
            echo "FAIL: stage5 and forth linked outputs differ"
            if [[ -n "$FIRST_EXE_DIFF" ]]; then
                echo "First .s32x byte diff (1-based offset, forth, stage5): $FIRST_EXE_DIFF"
                OFF_DEC="$(printf '%s\n' "$FIRST_EXE_DIFF" | awk '{print $1}')"
                if [[ -n "$OFF_DEC" ]]; then
                    OFF_HEX="$(printf '0x%X' "$((OFF_DEC - 1))")"
                    echo "First .s32x differing zero-based offset: $OFF_HEX"
                fi
            fi
            if [[ -n "$FIRST_OBJ_DIFF" ]]; then
                echo "First .s32o byte diff (1-based offset, forth, stage5): $FIRST_OBJ_DIFF"
            fi
            exit 1
        fi
        ;;
    stage6-utility-smoke)
        build_stage5_assembler
        if [[ "$TEST_NAME" == "test3" ]]; then
            TARGET_SRC="$VALIDATION_DIR/slow32dump.c"
        else
            TARGET_SRC="$(resolve_validation_source "$TEST_NAME")"
        fi
        [[ -f "$TARGET_SRC" ]] || { echo "Missing test source: $TARGET_SRC" >&2; exit 1; }
        TARGET_ASM="$WORKDIR/utility.s"
        TARGET_STAGE5_OBJ="$WORKDIR/utility.stage5.s32o"
        TARGET_FORTH_OBJ="$WORKDIR/utility.forth.s32o"
        TARGET_STAGE5_EXE="$WORKDIR/utility.stage5.s32x"
        TARGET_FORTH_EXE="$WORKDIR/utility.forth.s32x"
        UTILITY_ARGS_RAW="${UTILITY_ARGS:--h}"
        read -r -a UTILITY_ARGS_VEC <<<"$UTILITY_ARGS_RAW"
        EXEC_TIMEOUT="${UTILITY_TIMEOUT:-60}"
        if [[ -n "${UTILITY_INPUT:-}" ]]; then
            TARGET_INPUT="$UTILITY_INPUT"
            [[ -f "$TARGET_INPUT" ]] || { echo "Missing utility input: $TARGET_INPUT" >&2; exit 1; }
            UTILITY_ARGS_VEC+=("$TARGET_INPUT")
        fi

        compile_c_stage4 "$TARGET_SRC" "$TARGET_ASM" "$WORKDIR/utility.cc.log"
        assemble_with_stage5 "$TARGET_ASM" "$TARGET_STAGE5_OBJ" "$WORKDIR/utility.stage5.as.log"
        assemble_forth "$TARGET_ASM" "$TARGET_FORTH_OBJ" "$WORKDIR/utility.forth.as.log"
        link_forth "$TARGET_STAGE5_OBJ" "$TARGET_STAGE5_EXE" "$WORKDIR/utility.stage5.ld.log"
        link_forth "$TARGET_FORTH_OBJ" "$TARGET_FORTH_EXE" "$WORKDIR/utility.forth.ld.log"

        if ! cmp -s "$TARGET_FORTH_EXE" "$TARGET_STAGE5_EXE"; then
            FIRST_EXE_DIFF="$(cmp -l "$TARGET_FORTH_EXE" "$TARGET_STAGE5_EXE" 2>/dev/null | head -n 1 || true)"
            echo "FAIL: utility linked outputs differ"
            if [[ -n "$FIRST_EXE_DIFF" ]]; then
                echo "First .s32x byte diff (1-based offset, forth, stage5): $FIRST_EXE_DIFF"
            fi
            exit 1
        fi

        TARGET_RUN_EXE="$WORKDIR/utility.run.s32x"
        cp "$TARGET_STAGE5_EXE" "$TARGET_RUN_EXE"
        RC_STAGE5=0
        run_exe_any_rc "$TARGET_RUN_EXE" "$WORKDIR/utility.stage5.run.log" "${UTILITY_ARGS_VEC[@]}" || RC_STAGE5=$?
        cp "$TARGET_FORTH_EXE" "$TARGET_RUN_EXE"
        RC_FORTH=0
        run_exe_any_rc "$TARGET_RUN_EXE" "$WORKDIR/utility.forth.run.log" "${UTILITY_ARGS_VEC[@]}" || RC_FORTH=$?
        if [[ "$RC_STAGE5" -eq 124 || "$RC_STAGE5" -eq 125 || "$RC_FORTH" -eq 124 || "$RC_FORTH" -eq 125 ]]; then
            echo "FAIL: utility runtime fault/timeout" >&2
            exit 1
        fi
        if [[ "$RC_STAGE5" -ne "$RC_FORTH" ]]; then
            echo "FAIL: utility exit code differs (stage5=$RC_STAGE5 forth=$RC_FORTH)" >&2
            exit 1
        fi
        sed -E '/^Wall time: /d;/^Performance: /d;/instructions\/second$/d' \
            "$WORKDIR/utility.stage5.run.log" > "$WORKDIR/utility.stage5.run.norm.log"
        sed -E '/^Wall time: /d;/^Performance: /d;/instructions\/second$/d' \
            "$WORKDIR/utility.forth.run.log" > "$WORKDIR/utility.forth.run.norm.log"
        if ! cmp -s "$WORKDIR/utility.forth.run.norm.log" "$WORKDIR/utility.stage5.run.norm.log"; then
            echo "FAIL: utility runtime output differs"
            exit 1
        fi
        ;;
    stage5-as-bisect)
        if [[ "$TEST_NAME" = /* ]] || [[ "$TEST_NAME" == *.c ]]; then
            TARGET_SRC="$TEST_NAME"
        else
            TARGET_SRC="$TEST_DIR/${TEST_NAME}.c"
        fi
        [[ -f "$TARGET_SRC" ]] || { echo "Missing test source: $TARGET_SRC" >&2; exit 1; }
        TARGET_ASM="$WORKDIR/target.s"
        TARGET_OBJ="$WORKDIR/target.s32o"
        compile_c_stage4 "$TARGET_SRC" "$TARGET_ASM" "$WORKDIR/target.cc.log"
        build_stage5_assembler
        stage5_assemble_bisect "$TARGET_ASM" "$TARGET_OBJ" "$WORKDIR/target.as.log"
        ;;
    baseline)
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
        assemble_forth "$TARGET_ASM" "$TARGET_OBJ" "$WORKDIR/target.as.log"
        ;;
    progressive-as|progressive-as-ar|progressive-as-ar-scan)
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
        build_stage5_assembler
        assemble_with_stage5 "$TARGET_ASM" "$TARGET_OBJ" "$WORKDIR/target.as.log"
        if [[ "$MODE" == "progressive-as-ar" ]]; then
            build_stage6_archiver "$VALIDATION_DIR/s32-ar.c" stage5
            stage6_archive_smoke
        elif [[ "$MODE" == "progressive-as-ar-scan" ]]; then
            build_stage6_archiver "$VALIDATION_DIR/s32-ar-scan.c" stage5
            stage6_archive_smoke "cs"
        fi
        ;;
esac

if [[ "$MODE" != "stage6-ar-smoke" && "$MODE" != "stage6-ar-rc-smoke" && "$MODE" != "stage6-ar-tx-smoke" && "$MODE" != "stage6-ar-scan-smoke" && "$MODE" != "stage6-ar-asm-diff" && "$MODE" != "stage6-utility-smoke" && "$MODE" != "stage5-as-bisect" ]]; then
    link_forth "$TARGET_OBJ" "$TARGET_EXE" "$WORKDIR/target.ld.log"
    run_exe "$TARGET_EXE" "$WORKDIR/target.run.log"
fi

echo "OK: stage05 pipeline ($MODE)"
if [[ "$MODE" == "stage6-ar-smoke" || "$MODE" == "stage6-ar-scan-smoke" ]]; then
    echo "Input member: $TARGET_OBJ"
    echo "Assembler path: c(stage05) for s32-as and stage6 smoke asm"
    echo "Linker path: forth(stage03)"
elif [[ "$MODE" == "stage6-ar-rc-smoke" ]]; then
    echo "Input members: runtime/divsi3.s32o runtime/crt0.s32o (replace divsi3.s32o)"
    echo "Assembler path: c(stage05) for s32-as and stage6 smoke asm"
    echo "Linker path: forth(stage03)"
elif [[ "$MODE" == "stage6-ar-tx-smoke" ]]; then
    echo "Input members: runtime/divsi3.s32o runtime/crt0.s32o"
    echo "Assembler path: c(stage05) for s32-as and stage6 smoke asm"
    echo "Linker path: forth(stage03)"
elif [[ "$MODE" == "stage6-ar-asm-diff" ]]; then
    echo "Input: $TARGET_SRC"
    echo "Assembler path: c(stage05) vs forth(stage01) object-compare"
    echo "Linker path: skipped"
elif [[ "$MODE" == "stage6-utility-smoke" ]]; then
    echo "Input: $TARGET_SRC"
    echo "Utility args: $UTILITY_ARGS_RAW"
    if [[ -n "${TARGET_INPUT:-}" ]]; then
        echo "Utility input: $TARGET_INPUT"
    fi
    echo "Assembler path: c(stage05) vs forth(stage01) parity"
    echo "Linker path: forth(stage03)"
elif [[ "$MODE" == "stage5-as-bisect" ]]; then
    echo "Input: $TARGET_SRC"
    echo "Assembler path: c(stage05) first-failing-line bisect"
    echo "Linker path: skipped"
elif [[ "$MODE" == "baseline" ]]; then
    echo "Input: $TARGET_SRC"
    echo "Assembler path: forth(stage01)"
    echo "Linker path: forth(stage03)"
else
    echo "Input: $TARGET_SRC"
    echo "Assembler path: c(stage05)"
    echo "Linker path: forth(stage03)"
fi
if [[ "$MODE" == "progressive-as-ar" || "$MODE" == "stage6-ar-smoke" ]]; then
    echo "Archiver smoke: c(stage06)"
elif [[ "$MODE" == "stage6-ar-rc-smoke" ]]; then
    echo "Archiver smoke: c(stage06, cmd=rc replace)"
elif [[ "$MODE" == "stage6-ar-tx-smoke" ]]; then
    echo "Archiver smoke: c(stage06, cmd=t/x list+extract)"
elif [[ "$MODE" == "progressive-as-ar-scan" || "$MODE" == "stage6-ar-scan-smoke" ]]; then
    echo "Archiver smoke: c(stage06, cmd=cs)"
elif [[ "$MODE" == "stage6-ar-asm-diff" ]]; then
    echo "Archiver smoke: skipped (assembler comparison only)"
elif [[ "$MODE" == "stage6-utility-smoke" ]]; then
    echo "Utility smoke: runtime output parity"
elif [[ "$MODE" == "stage5-as-bisect" ]]; then
    echo "Archiver smoke: skipped (assembler failure isolation only)"
fi
echo "Emulator: $EMU"
echo "Artifacts: $WORKDIR"
