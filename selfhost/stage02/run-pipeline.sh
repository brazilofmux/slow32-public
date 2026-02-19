#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
SELFHOST_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"
ROOT_DIR="$(cd "$SELFHOST_DIR/.." && pwd)"

EMU="${SELFHOST_EMU:-$SELFHOST_DIR/stage00/s32-emu}"
KERNEL="${SELFHOST_KERNEL:-$ROOT_DIR/forth/kernel.s32x}"
PRELUDE="${SELFHOST_PRELUDE:-$ROOT_DIR/forth/prelude.fth}"
EMU_EXPLICIT=0

CC_FTH="${SELFHOST_CC_FTH:-$SELFHOST_DIR/stage01/cc.fth}"
ASM_FTH="${SELFHOST_ASM_FTH:-$SELFHOST_DIR/stage01/asm.fth}"
LINK_FTH="${SELFHOST_LINK_FTH:-$SELFHOST_DIR/stage01/link.fth}"
AR_FTH="${SELFHOST_AR_FTH:-$SELFHOST_DIR/stage01/ar.fth}"

TEST_DIR="${SELFHOST_TEST_DIR:-$SELFHOST_DIR/stage01/tests}"
VALIDATION_DIR="${SELFHOST_VALIDATION_DIR:-$SELFHOST_DIR/stage01/validation}"
STAGE6_AR_SRC="${SELFHOST_STAGE6_AR_SRC:-$SCRIPT_DIR/s32-ar.c}"
STAGE6_AR_SCAN_SRC="${SELFHOST_STAGE6_AR_SCAN_SRC:-$SCRIPT_DIR/s32-ar-scan.c}"
LIBC_DIR="$SCRIPT_DIR/libc"
CRT0_SRC="$SCRIPT_DIR/crt0.s"
MMIO_SRC="$SCRIPT_DIR/mmio.s"
MMIO_NO_START_SRC="$SCRIPT_DIR/mmio_no_start.s"
STAGE2_AS_READY=0

MODE="baseline"
TEST_NAME="test3"
KEEP_ARTIFACTS=0

choose_default_emu() {
    printf '%s\n' "$SELFHOST_DIR/stage00/s32-emu"
}

usage() {
    cat <<USAGE
Usage: $0 [--mode baseline|progressive-as|progressive-as-ar|progressive-as-ar-scan|stage2-as-bisect|stage6-ar-smoke|stage6-ar-rc-smoke|stage6-ar-tx-smoke|stage6-ar-d-smoke|stage6-ar-m-smoke|stage6-ar-vp-smoke|stage6-ar-scan-smoke|stage6-ar-asm-diff|stage6-utility-smoke] [--test <name>] [--emu <path>] [--keep-artifacts]

Modes:
  baseline          Stage4 cc.fth + Stage1 asm.fth + Stage3 link.fth
  progressive-as    Stage4 cc.fth + Stage2 s32-as.c (for .s -> .s32o) + Stage3 link.fth
  progressive-as-ar Stage4 cc.fth + Stage2 s32-as.c + Stage6 s32-ar.c smoke-check + Stage3 link.fth
  progressive-as-ar-scan Same as progressive-as-ar, but runs s32-ar with opt-in symbol scan flag
  stage2-as-bisect  Build stage2 s32-as.c, compile a target .c to .s, and isolate first source line that self-built assembler rejects
  stage6-ar-smoke   Build stage2 assembler, then stage6 s32-ar.c with stage2 assembler; run archive smoke only
  stage6-ar-rc-smoke Build stage2 assembler, then stage6 s32-ar.c with stage2 assembler; verify replace-on-existing (rc) path
  stage6-ar-tx-smoke Build stage2 assembler, then stage6 s32-ar.c with stage2 assembler; verify list/extract (t/x) paths
  stage6-ar-d-smoke Build stage2 assembler, then stage6 s32-ar.c with stage2 assembler; verify delete (d) path
  stage6-ar-m-smoke Build stage2 assembler, then stage6 s32-ar.c with stage2 assembler; verify move-to-end (m) path
  stage6-ar-vp-smoke Build stage2 assembler, then stage6 s32-ar.c with stage2 assembler; verify verbose/list + print-member (v/p) paths
  stage6-ar-scan-smoke Build stage2 assembler, then stage6 s32-ar-scan.c with stage2 assembler; run archive smoke with cmd=cs only
  stage6-ar-asm-diff Build stage2 assembler, assemble a validation .c with stage2 and forth, link both, and report first .s32x/.s32o byte diff
  stage6-utility-smoke Build a validation utility with stage2 and forth assemblers, require linked .s32x parity, then compare runtime output

Env overrides:
  SELFHOST_ROOT SELFHOST_EMU SELFHOST_KERNEL SELFHOST_PRELUDE
  SELFHOST_CC_FTH SELFHOST_ASM_FTH SELFHOST_LINK_FTH SELFHOST_AR_FTH
  SELFHOST_TEST_DIR SELFHOST_VALIDATION_DIR
  SELFHOST_STAGE6_AR_SRC SELFHOST_STAGE6_AR_SCAN_SRC
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

if [[ "$MODE" == "stage6-utility-smoke" && "$EMU_EXPLICIT" -eq 0 && -z "${SELFHOST_EMU:-}" ]]; then
    EMU="$(choose_default_emu)"
fi

case "$MODE" in
    baseline|progressive-as|progressive-as-ar|progressive-as-ar-scan|stage2-as-bisect|stage6-ar-smoke|stage6-ar-rc-smoke|stage6-ar-tx-smoke|stage6-ar-d-smoke|stage6-ar-m-smoke|stage6-ar-vp-smoke|stage6-ar-scan-smoke|stage6-ar-asm-diff|stage6-utility-smoke) ;;
    *)
        echo "Unknown mode: $MODE" >&2
        usage
        exit 2
        ;;
esac

for f in "$EMU" "$KERNEL" "$PRELUDE" "$CC_FTH" "$ASM_FTH" "$LINK_FTH" "$AR_FTH" \
         "$CRT0_SRC" "$MMIO_SRC" "$MMIO_NO_START_SRC"; do
    [[ -f "$f" ]] || { echo "Missing required file: $f" >&2; exit 1; }
done

WORKDIR="$(mktemp -d /tmp/selfhost-v2-stage02.XXXXXX)"
if [[ "$KEEP_ARTIFACTS" -eq 0 ]]; then
    trap 'rm -rf "$WORKDIR"' EXIT
fi

# cc.fth uses relative include path "selfhost/stage01/include/" — run from repo root
cd "$ROOT_DIR"

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

# --- Build selfhost runtime from bundled stage02 sources ---
RUNTIME_CRT0="$WORKDIR/crt0_minimal.s32o"
RUNTIME_MMIO_OBJ="$WORKDIR/mmio_minimal.s32o"
RUNTIME_MMIO_NO_START_OBJ="$WORKDIR/mmio_no_start.s32o"
run_forth "$ASM_FTH" /dev/null "S\" $CRT0_SRC\" S\" $RUNTIME_CRT0\" ASSEMBLE
BYE" "$WORKDIR/crt0.as.log"
[[ -s "$RUNTIME_CRT0" ]] || { echo "failed to assemble crt0_minimal.s" >&2; exit 1; }
run_forth "$ASM_FTH" /dev/null "S\" $MMIO_SRC\" S\" $RUNTIME_MMIO_OBJ\" ASSEMBLE
BYE" "$WORKDIR/mmio.as.log"
[[ -s "$RUNTIME_MMIO_OBJ" ]] || { echo "failed to assemble mmio_minimal.s" >&2; exit 1; }
run_forth "$ASM_FTH" /dev/null "S\" $MMIO_NO_START_SRC\" S\" $RUNTIME_MMIO_NO_START_OBJ\" ASSEMBLE
BYE" "$WORKDIR/mmio_no_start.as.log"
[[ -s "$RUNTIME_MMIO_NO_START_OBJ" ]] || { echo "failed to assemble mmio.s" >&2; exit 1; }
RUNTIME_BUILD_MODE="forth"

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
S\" $RUNTIME_CRT0\" LINK-OBJ
S\" $obj\" LINK-OBJ
S\" $RUNTIME_MMIO_OBJ\" LINK-OBJ
65536 LINK-MMIO
S\" $exe\" LINK-EMIT
BYE" "$log"
    [[ -s "$exe" ]] || { echo "linker produced no output: $obj" >&2; return 1; }
}

# --- Selfhost libc build ---
LIBC_ARCHIVE=""
LIBC_START_OBJ=""
LIBC_BUILT=0
LIBC_BUILD_MODE=""

build_selfhost_libc() {
    local mode="${1:-forth}"
    if [[ "$LIBC_BUILT" -eq 1 && "${LIBC_BUILD_MODE:-}" == "$mode" ]]; then return 0; fi
    LIBC_BUILD_MODE="$mode"
    local libc_c_files="string_extra convert stdio start"
    local name src asm obj
    local ar_objs=""

    echo "Building selfhost libc (assembler: $mode)..."

    for name in $libc_c_files; do
        src="$LIBC_DIR/${name}.c"
        asm="$WORKDIR/libc_${name}.s"
        obj="$WORKDIR/libc_${name}.s32o"

        [[ -f "$src" ]] || { echo "Missing libc source: $src" >&2; return 1; }
        compile_c_stage4 "$src" "$asm" "$WORKDIR/libc_${name}.cc.log"
        if [[ "$mode" == "stage2" ]]; then
            assemble_with_stage2 "$asm" "$obj" "$WORKDIR/libc_${name}.as.log"
        else
            assemble_forth "$asm" "$obj" "$WORKDIR/libc_${name}.as.log"
        fi

        if [[ "$name" == "start" ]]; then
            LIBC_START_OBJ="$obj"
        else
            ar_objs="$ar_objs $obj"
        fi
    done

    # Archive the libc objects (start.s32o is linked directly, not archived)
    LIBC_ARCHIVE="$WORKDIR/libc_selfhost.s32a"
    local ar_cmd
    ar_cmd="S\" $LIBC_ARCHIVE\" AR-C-BEGIN"
    for obj in $ar_objs; do
        ar_cmd="${ar_cmd}
S\" $obj\" AR-ADD"
    done
    ar_cmd="${ar_cmd}
AR-C-END
BYE"
    run_forth "$AR_FTH" /dev/null "$ar_cmd" "$WORKDIR/libc.ar.log"
    [[ -s "$LIBC_ARCHIVE" ]] || { echo "failed to build libc archive" >&2; return 1; }
    echo "Libc archive: $LIBC_ARCHIVE"
    LIBC_BUILT=1
}

rebuild_runtime_with_stage2() {
    if [[ "${RUNTIME_BUILD_MODE:-}" == "stage2" ]]; then
        return 0
    fi
    [[ -n "${STAGE2_AS_EXE:-}" ]] || { echo "stage2 assembler is not built" >&2; return 1; }
    echo "Rebuilding runtime objects with stage2 assembler..."
    assemble_with_stage2 "$CRT0_SRC" "$RUNTIME_CRT0" "$WORKDIR/crt0.stage2.as.log"
    assemble_with_stage2 "$MMIO_SRC" "$RUNTIME_MMIO_OBJ" "$WORKDIR/mmio.stage2.as.log"
    assemble_with_stage2 "$MMIO_NO_START_SRC" "$RUNTIME_MMIO_NO_START_OBJ" "$WORKDIR/mmio_no_start.stage2.as.log"
    RUNTIME_BUILD_MODE="stage2"
}

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

build_stage2_assembler() {
    if [[ "${STAGE2_AS_READY:-0}" -eq 1 ]]; then
        return 0
    fi
    local exe="$SCRIPT_DIR/s32-as.s32x"
    [[ -f "$exe" ]] || { echo "Missing artifact: $exe (run 'make' in stage02 first)" >&2; return 1; }
    STAGE2_AS_EXE="$exe"
    STAGE2_AS_READY=1
    rebuild_runtime_with_stage2
    build_selfhost_libc stage2
}

assemble_with_stage2() {
    local asm="$1"
    local obj="$2"
    local log="$3"

    [[ -n "${STAGE2_AS_EXE:-}" ]] || { echo "stage2 assembler is not built" >&2; return 1; }
    run_exe "$STAGE2_AS_EXE" "$log" "$asm" "$obj"
    [[ -s "$obj" ]] || { echo "stage2 assembler produced no output: $asm" >&2; return 1; }
}

build_stage6_archiver() {
    local src="${1:-$STAGE6_AR_SRC}"
    local asm_mode="${2:-stage2}"
    local asm="$WORKDIR/s32-ar.s"
    local obj="$WORKDIR/s32-ar.s32o"
    local exe="$WORKDIR/s32-ar.s32x"

    [[ -f "$src" ]] || { echo "Missing source: $src" >&2; return 1; }
    if [[ "$asm_mode" == "stage2" ]]; then
        build_selfhost_libc stage2
    else
        build_selfhost_libc forth
    fi
    compile_c_stage4 "$src" "$asm" "$WORKDIR/s32-ar.cc.log"
    if [[ "$asm_mode" == "forth" ]]; then
        assemble_forth "$asm" "$obj" "$WORKDIR/s32-ar.as.log"
    else
        assemble_with_stage2 "$asm" "$obj" "$WORKDIR/s32-ar.as.log"
    fi
    link_forth_with_libc "$obj" "$exe" "$WORKDIR/s32-ar.ld.log"
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
    local obj_a="$WORKDIR/rc-alpha.dat"
    local obj_b="$WORKDIR/rc-beta.dat"
    local repl_obj="$WORKDIR/rc-repl/rc-alpha.dat"
    local xdir="$WORKDIR/extract"
    local xlog="$WORKDIR/s32-ar.extract.log"
    local rc=0

    printf 'original-alpha-content-for-rc-smoke\n' > "$obj_a"
    printf 'original-beta-content-for-rc-smoke\n' > "$obj_b"
    mkdir -p "$WORKDIR/rc-repl"
    printf 'replaced-alpha-content-for-rc-smoke\n' > "$repl_obj"

    run_exe "$STAGE6_AR_EXE" "$WORKDIR/s32-ar.rc-create.log" "c" "$archive" "$obj_a" "$obj_b"
    run_exe "$STAGE6_AR_EXE" "$WORKDIR/s32-ar.rc-replace.log" "rc" "$archive" "$repl_obj"
    [[ -s "$archive" ]] || { echo "stage6 archiver produced no output" >&2; return 1; }

    mkdir -p "$xdir"
    set +e
    (
        cd "$xdir"
        cat "$PRELUDE" "$AR_FTH" - <<FTH | timeout 120 "$EMU" "$KERNEL" >"$xlog" 2>&1
S" $archive" S" rc-alpha.dat" AR-X1
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
    [[ -s "$xdir/rc-alpha.dat" ]] || { echo "replaced member missing after extract" >&2; return 1; }
    cmp -s "$repl_obj" "$xdir/rc-alpha.dat" || {
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

stage6_archive_d_smoke() {
    local archive="$WORKDIR/smoke-d.s32a"
    local obj_a="$WORKDIR/member-a.src"
    local obj_b="$WORKDIR/member-b.src"
    local obj_b_expected="$WORKDIR/member-b.expected"
    local xdir="$WORKDIR/extract-d"
    local rc=0

    printf 'alpha-stage6-d\n' > "$obj_a"
    printf 'beta-stage6-d\n' > "$obj_b"
    cp "$obj_b" "$obj_b_expected"

    run_exe "$STAGE6_AR_EXE" "$WORKDIR/s32-ar.d-create.log" "c" "$archive" "$obj_a" "$obj_b"
    run_exe "$STAGE6_AR_EXE" "$WORKDIR/s32-ar.d-delete.log" "d" "$archive" "member-a.src"
    mkdir -p "$xdir"

    run_exe_any_rc "$STAGE6_AR_EXE" "$WORKDIR/s32-ar.d-extract.log" "x" "$archive" "member-a.src" || rc=$?
    if [[ "$rc" -eq 124 || "$rc" -eq 125 ]]; then
        echo "stage6 d path failed: extract fault/timeout" >&2
        return 1
    fi
    if [[ "$rc" -eq 0 || "$rc" -eq 96 ]]; then
        echo "stage6 d path failed: deleted member unexpectedly extracted" >&2
        return 1
    fi

    rc=0
    set +e
    (
        cd "$xdir"
        run_exe "$STAGE6_AR_EXE" "$WORKDIR/s32-ar.d-extract-keep.log" "x" "$archive" "member-b.src"
    )
    rc=$?
    set -e
    if [[ "$rc" -ne 0 ]]; then
        echo "stage6 d path failed: remaining member extract failed" >&2
        return 1
    fi
    [[ -s "$xdir/member-b.src" ]] || { echo "stage6 d path failed: remaining member missing after extract" >&2; return 1; }
    cmp -s "$obj_b_expected" "$xdir/member-b.src" || {
        echo "stage6 d path failed: remaining member bytes mismatch" >&2
        return 1
    }
}

stage6_archive_m_smoke() {
    local archive="$WORKDIR/smoke-m.s32a"
    local obj_a="$WORKDIR/member-a.src"
    local obj_b="$WORKDIR/member-b.src"
    local obj_c="$WORKDIR/member-c.src"
    local list_log="$WORKDIR/s32-ar.m-list.log"
    local order_after="$WORKDIR/s32-ar.m-after.names"
    local expected_after="$WORKDIR/s32-ar.m-expected.names"

    printf 'alpha-stage6-m\n' > "$obj_a"
    printf 'beta-stage6-m\n' > "$obj_b"
    printf 'gamma-stage6-m\n' > "$obj_c"

    run_exe "$STAGE6_AR_EXE" "$WORKDIR/s32-ar.m-create.log" "c" "$archive" "$obj_a" "$obj_b" "$obj_c"
    run_exe "$STAGE6_AR_EXE" "$WORKDIR/s32-ar.m-move.log" "m" "$archive" "member-a.src"
    run_exe "$STAGE6_AR_EXE" "$list_log" "t" "$archive"
    sed -E '/^Wall time: /d;/^Performance: /d;/instructions\/second$/d;/^Starting execution/d;/^MMIO enabled/d;/^HALT at /d;/^$/d;/^Program halted/d;/^Exit code:/d;/^Instructions executed:/d;/^Simulated cycles:/d' \
        "$list_log" | awk '{print $2}' > "$order_after"
    printf "member-b.src\nmember-c.src\nmember-a.src\n" > "$expected_after"
    cmp -s "$order_after" "$expected_after" || {
        echo "stage6 m path failed: member order mismatch after move" >&2
        return 1
    }
}

stage6_archive_vp_smoke() {
    local archive="$WORKDIR/smoke-vp.s32a"
    local obj_a="$WORKDIR/member-a.src"
    local obj_b="$WORKDIR/member-b.src"
    local rc=0

    printf 'alpha-stage6-vp\n' > "$obj_a"
    printf 'beta-stage6-vp\n' > "$obj_b"

    run_exe "$STAGE6_AR_EXE" "$WORKDIR/s32-ar.vp-create.log" "cv" "$archive" "$obj_a" "$obj_b"
    run_exe "$STAGE6_AR_EXE" "$WORKDIR/s32-ar.vp-print-a.log" "p" "$archive" "member-a.src"
    run_exe "$STAGE6_AR_EXE" "$WORKDIR/s32-ar.vp-list.log" "tv" "$archive"

    rc=0
    run_exe_any_rc "$STAGE6_AR_EXE" "$WORKDIR/s32-ar.vp-print-missing.log" "p" "$archive" "missing.src" || rc=$?
    if [[ "$rc" -eq 0 || "$rc" -eq 96 ]]; then
        echo "stage6 p path failed: missing member unexpectedly succeeded" >&2
        return 1
    fi

    run_exe "$STAGE6_AR_EXE" "$WORKDIR/s32-ar.vp-print-all.log" "p" "$archive"
}

stage2_assemble_bisect() {
    local asm="$1"
    local out_obj="$2"
    local run_log="$3"
    local total first_bad lo hi mid tmp_asm tmp_obj

    if run_exe "$STAGE2_AS_EXE" "$run_log" "$asm" "$out_obj"; then
        [[ -s "$out_obj" ]] || { echo "stage2 assembler produced no output: $asm" >&2; return 1; }
        echo "stage2 assembler accepted full assembly"
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
        if run_exe "$STAGE2_AS_EXE" "$WORKDIR/bisect-$mid.log" "$tmp_asm" "$tmp_obj"; then
            lo=$(( mid + 1 ))
        else
            first_bad="$mid"
            hi=$(( mid - 1 ))
        fi
    done

    echo "stage2 assembler first failing line: $first_bad"
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
        build_stage2_assembler
        TARGET_OBJ="$RUNTIME_CRT0"
        build_stage6_archiver "$STAGE6_AR_SRC" stage2
        stage6_archive_smoke
        ;;
    stage6-ar-rc-smoke)
        build_stage2_assembler
        build_stage6_archiver "$STAGE6_AR_SRC" stage2
        stage6_archive_rc_smoke
        ;;
    stage6-ar-tx-smoke)
        build_stage2_assembler
        build_stage6_archiver "$STAGE6_AR_SRC" stage2
        stage6_archive_tx_smoke
        ;;
    stage6-ar-d-smoke)
        build_stage2_assembler
        build_stage6_archiver "$STAGE6_AR_SRC" stage2
        stage6_archive_d_smoke
        ;;
    stage6-ar-m-smoke)
        build_stage2_assembler
        build_stage6_archiver "$STAGE6_AR_SRC" stage2
        stage6_archive_m_smoke
        ;;
    stage6-ar-vp-smoke)
        build_stage2_assembler
        build_stage6_archiver "$STAGE6_AR_SRC" stage2
        stage6_archive_vp_smoke
        ;;
    stage6-ar-scan-smoke)
        build_stage2_assembler
        TARGET_OBJ="$RUNTIME_CRT0"
        build_stage6_archiver "$STAGE6_AR_SCAN_SRC" stage2
        stage6_archive_smoke "cs"
        ;;
    stage6-ar-asm-diff)
        build_stage2_assembler
        if [[ "$TEST_NAME" == "test3" ]]; then
            TARGET_SRC="$STAGE6_AR_SRC"
        else
            TARGET_SRC="$(resolve_validation_source "$TEST_NAME")"
        fi
        [[ -f "$TARGET_SRC" ]] || { echo "Missing test source: $TARGET_SRC" >&2; exit 1; }
        TARGET_ASM="$WORKDIR/s32-ar.s"
        TARGET_STAGE2_OBJ="$WORKDIR/s32-ar.stage2.s32o"
        TARGET_FORTH_OBJ="$WORKDIR/s32-ar.forth.s32o"
        TARGET_STAGE2_EXE="$WORKDIR/s32-ar.stage2.s32x"
        TARGET_FORTH_EXE="$WORKDIR/s32-ar.forth.s32x"
        compile_c_stage4 "$TARGET_SRC" "$TARGET_ASM" "$WORKDIR/s32-ar.cc.log"
        assemble_with_stage2 "$TARGET_ASM" "$TARGET_STAGE2_OBJ" "$WORKDIR/s32-ar.stage2.as.log"
        assemble_forth "$TARGET_ASM" "$TARGET_FORTH_OBJ" "$WORKDIR/s32-ar.forth.as.log"
        link_forth_with_libc "$TARGET_STAGE2_OBJ" "$TARGET_STAGE2_EXE" "$WORKDIR/s32-ar.stage2.ld.log"
        link_forth_with_libc "$TARGET_FORTH_OBJ" "$TARGET_FORTH_EXE" "$WORKDIR/s32-ar.forth.ld.log"

        if cmp -s "$TARGET_FORTH_EXE" "$TARGET_STAGE2_EXE"; then
            if cmp -s "$TARGET_FORTH_OBJ" "$TARGET_STAGE2_OBJ"; then
                echo "OK: stage2 and forth assemblers produced identical .s32o/.s32x"
            else
                echo "OK: linked .s32x is identical (object metadata differs)"
            fi
        else
            FIRST_EXE_DIFF="$(cmp -l "$TARGET_FORTH_EXE" "$TARGET_STAGE2_EXE" 2>/dev/null | head -n 1 || true)"
            FIRST_OBJ_DIFF="$(cmp -l "$TARGET_FORTH_OBJ" "$TARGET_STAGE2_OBJ" 2>/dev/null | head -n 1 || true)"
            echo "FAIL: stage2 and forth linked outputs differ"
            if [[ -n "$FIRST_EXE_DIFF" ]]; then
                echo "First .s32x byte diff (1-based offset, forth, stage2): $FIRST_EXE_DIFF"
                OFF_DEC="$(printf '%s\n' "$FIRST_EXE_DIFF" | awk '{print $1}')"
                if [[ -n "$OFF_DEC" ]]; then
                    OFF_HEX="$(printf '0x%X' "$((OFF_DEC - 1))")"
                    echo "First .s32x differing zero-based offset: $OFF_HEX"
                fi
            fi
            if [[ -n "$FIRST_OBJ_DIFF" ]]; then
                echo "First .s32o byte diff (1-based offset, forth, stage2): $FIRST_OBJ_DIFF"
            fi
            exit 1
        fi
        ;;
    stage6-utility-smoke)
        build_stage2_assembler
        if [[ "$TEST_NAME" == "test3" ]]; then
            TARGET_SRC="$VALIDATION_DIR/slow32dump.c"
        else
            TARGET_SRC="$(resolve_validation_source "$TEST_NAME")"
        fi
        [[ -f "$TARGET_SRC" ]] || { echo "Missing test source: $TARGET_SRC" >&2; exit 1; }
        TARGET_ASM="$WORKDIR/utility.s"
        TARGET_STAGE2_OBJ="$WORKDIR/utility.stage2.s32o"
        TARGET_FORTH_OBJ="$WORKDIR/utility.forth.s32o"
        TARGET_STAGE2_EXE="$WORKDIR/utility.stage2.s32x"
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
        assemble_with_stage2 "$TARGET_ASM" "$TARGET_STAGE2_OBJ" "$WORKDIR/utility.stage2.as.log"
        assemble_forth "$TARGET_ASM" "$TARGET_FORTH_OBJ" "$WORKDIR/utility.forth.as.log"
        link_forth_with_libc "$TARGET_STAGE2_OBJ" "$TARGET_STAGE2_EXE" "$WORKDIR/utility.stage2.ld.log"
        link_forth_with_libc "$TARGET_FORTH_OBJ" "$TARGET_FORTH_EXE" "$WORKDIR/utility.forth.ld.log"

        if ! cmp -s "$TARGET_FORTH_EXE" "$TARGET_STAGE2_EXE"; then
            FIRST_EXE_DIFF="$(cmp -l "$TARGET_FORTH_EXE" "$TARGET_STAGE2_EXE" 2>/dev/null | head -n 1 || true)"
            echo "FAIL: utility linked outputs differ"
            if [[ -n "$FIRST_EXE_DIFF" ]]; then
                echo "First .s32x byte diff (1-based offset, forth, stage2): $FIRST_EXE_DIFF"
            fi
            exit 1
        fi

        TARGET_RUN_EXE="$WORKDIR/utility.run.s32x"
        cp "$TARGET_STAGE2_EXE" "$TARGET_RUN_EXE"
        RC_STAGE2=0
        run_exe_any_rc "$TARGET_RUN_EXE" "$WORKDIR/utility.stage2.run.log" "${UTILITY_ARGS_VEC[@]}" || RC_STAGE2=$?
        cp "$TARGET_FORTH_EXE" "$TARGET_RUN_EXE"
        RC_FORTH=0
        run_exe_any_rc "$TARGET_RUN_EXE" "$WORKDIR/utility.forth.run.log" "${UTILITY_ARGS_VEC[@]}" || RC_FORTH=$?
        if [[ "$RC_STAGE2" -eq 124 || "$RC_STAGE2" -eq 125 || "$RC_FORTH" -eq 124 || "$RC_FORTH" -eq 125 ]]; then
            echo "FAIL: utility runtime fault/timeout" >&2
            exit 1
        fi
        if [[ "$RC_STAGE2" -ne "$RC_FORTH" ]]; then
            echo "FAIL: utility exit code differs (stage2=$RC_STAGE2 forth=$RC_FORTH)" >&2
            exit 1
        fi
        sed -E '/^Wall time: /d;/^Performance: /d;/instructions\/second$/d' \
            "$WORKDIR/utility.stage2.run.log" > "$WORKDIR/utility.stage2.run.norm.log"
        sed -E '/^Wall time: /d;/^Performance: /d;/instructions\/second$/d' \
            "$WORKDIR/utility.forth.run.log" > "$WORKDIR/utility.forth.run.norm.log"
        if ! cmp -s "$WORKDIR/utility.forth.run.norm.log" "$WORKDIR/utility.stage2.run.norm.log"; then
            echo "FAIL: utility runtime output differs"
            exit 1
        fi
        ;;
    stage2-as-bisect)
        if [[ "$TEST_NAME" = /* ]] || [[ "$TEST_NAME" == *.c ]]; then
            TARGET_SRC="$TEST_NAME"
        else
            TARGET_SRC="$TEST_DIR/${TEST_NAME}.c"
        fi
        [[ -f "$TARGET_SRC" ]] || { echo "Missing test source: $TARGET_SRC" >&2; exit 1; }
        TARGET_ASM="$WORKDIR/target.s"
        TARGET_OBJ="$WORKDIR/target.s32o"
        compile_c_stage4 "$TARGET_SRC" "$TARGET_ASM" "$WORKDIR/target.cc.log"
        build_stage2_assembler
        stage2_assemble_bisect "$TARGET_ASM" "$TARGET_OBJ" "$WORKDIR/target.as.log"
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
        build_stage2_assembler
        assemble_with_stage2 "$TARGET_ASM" "$TARGET_OBJ" "$WORKDIR/target.as.log"
        if [[ "$MODE" == "progressive-as-ar" ]]; then
            build_stage6_archiver "$STAGE6_AR_SRC" stage2
            stage6_archive_smoke
        elif [[ "$MODE" == "progressive-as-ar-scan" ]]; then
            build_stage6_archiver "$STAGE6_AR_SCAN_SRC" stage2
            stage6_archive_smoke "cs"
        fi
        ;;
esac

if [[ "$MODE" != "stage6-ar-smoke" && "$MODE" != "stage6-ar-rc-smoke" && "$MODE" != "stage6-ar-tx-smoke" && "$MODE" != "stage6-ar-d-smoke" && "$MODE" != "stage6-ar-m-smoke" && "$MODE" != "stage6-ar-vp-smoke" && "$MODE" != "stage6-ar-scan-smoke" && "$MODE" != "stage6-ar-asm-diff" && "$MODE" != "stage6-utility-smoke" && "$MODE" != "stage2-as-bisect" ]]; then
    link_forth "$TARGET_OBJ" "$TARGET_EXE" "$WORKDIR/target.ld.log"
    run_exe "$TARGET_EXE" "$WORKDIR/target.run.log"
fi

echo "OK: stage02 pipeline ($MODE)"
if [[ "$MODE" == "stage6-ar-smoke" || "$MODE" == "stage6-ar-scan-smoke" ]]; then
    echo "Input member: $TARGET_OBJ"
    echo "Assembler path: c(stage02) for s32-as and stage6 smoke asm"
    echo "Linker path: forth(stage01)"
elif [[ "$MODE" == "stage6-ar-rc-smoke" ]]; then
    echo "Input members: synthetic rc-alpha.dat rc-beta.dat (replace rc-alpha.dat)"
    echo "Assembler path: c(stage02) for s32-as and stage6 smoke asm"
    echo "Linker path: forth(stage01)"
elif [[ "$MODE" == "stage6-ar-tx-smoke" ]]; then
    echo "Input members: synthetic member-a.src member-b.src"
    echo "Assembler path: c(stage02) for s32-as and stage6 smoke asm"
    echo "Linker path: forth(stage01)"
elif [[ "$MODE" == "stage6-ar-d-smoke" ]]; then
    echo "Input members: synthetic member-a.src member-b.src (delete member-a.src)"
    echo "Assembler path: c(stage02) for s32-as and stage6 smoke asm"
    echo "Linker path: forth(stage01)"
elif [[ "$MODE" == "stage6-ar-m-smoke" ]]; then
    echo "Input members: synthetic member-a.src member-b.src member-c.src (move member-a.src to end)"
    echo "Assembler path: c(stage02) for s32-as and stage6 smoke asm"
    echo "Linker path: forth(stage01)"
elif [[ "$MODE" == "stage6-ar-vp-smoke" ]]; then
    echo "Input members: synthetic member-a.src member-b.src (list/print)"
    echo "Assembler path: c(stage02) for s32-as and stage6 smoke asm"
    echo "Linker path: forth(stage01)"
elif [[ "$MODE" == "stage6-ar-asm-diff" ]]; then
    echo "Input: $TARGET_SRC"
    echo "Assembler path: c(stage02) vs forth(stage01) object-compare"
    echo "Linker path: skipped"
elif [[ "$MODE" == "stage6-utility-smoke" ]]; then
    echo "Input: $TARGET_SRC"
    echo "Utility args: $UTILITY_ARGS_RAW"
    if [[ -n "${TARGET_INPUT:-}" ]]; then
        echo "Utility input: $TARGET_INPUT"
    fi
    echo "Assembler path: c(stage02) vs forth(stage01) parity"
    echo "Linker path: forth(stage01)"
elif [[ "$MODE" == "stage2-as-bisect" ]]; then
    echo "Input: $TARGET_SRC"
    echo "Assembler path: c(stage02) first-failing-line bisect"
    echo "Linker path: skipped"
elif [[ "$MODE" == "baseline" ]]; then
    echo "Input: $TARGET_SRC"
    echo "Assembler path: forth(stage01)"
    echo "Linker path: forth(stage01)"
else
    echo "Input: $TARGET_SRC"
    echo "Assembler path: c(stage02)"
    echo "Linker path: forth(stage01)"
fi
if [[ "$MODE" == "progressive-as-ar" || "$MODE" == "stage6-ar-smoke" ]]; then
    echo "Archiver smoke: c(stage02)"
elif [[ "$MODE" == "stage6-ar-rc-smoke" ]]; then
    echo "Archiver smoke: c(stage02, cmd=rc replace)"
elif [[ "$MODE" == "stage6-ar-tx-smoke" ]]; then
    echo "Archiver smoke: c(stage02, cmd=t/x list+extract)"
elif [[ "$MODE" == "stage6-ar-d-smoke" ]]; then
    echo "Archiver smoke: c(stage02, cmd=d delete)"
elif [[ "$MODE" == "stage6-ar-m-smoke" ]]; then
    echo "Archiver smoke: c(stage02, cmd=m move-to-end)"
elif [[ "$MODE" == "stage6-ar-vp-smoke" ]]; then
    echo "Archiver smoke: c(stage02, cmd=v/p list+print)"
elif [[ "$MODE" == "progressive-as-ar-scan" || "$MODE" == "stage6-ar-scan-smoke" ]]; then
    echo "Archiver smoke: c(stage02, cmd=cs)"
elif [[ "$MODE" == "stage6-ar-asm-diff" ]]; then
    echo "Archiver smoke: skipped (assembler comparison only)"
elif [[ "$MODE" == "stage6-utility-smoke" ]]; then
    echo "Utility smoke: runtime output parity"
elif [[ "$MODE" == "stage2-as-bisect" ]]; then
    echo "Archiver smoke: skipped (assembler failure isolation only)"
fi
echo "Emulator: $EMU"
echo "Artifacts: $WORKDIR"
