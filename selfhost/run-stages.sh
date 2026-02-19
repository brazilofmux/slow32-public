#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
ROOT_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"
if git -C "$SCRIPT_DIR" rev-parse --show-toplevel >/dev/null 2>&1; then
    ROOT_DIR="$(git -C "$SCRIPT_DIR" rev-parse --show-toplevel)"
fi

choose_default_emu() {
    if [[ -x "$ROOT_DIR/tools/emulator/slow32-fast" ]]; then
        printf '%s\n' "$ROOT_DIR/tools/emulator/slow32-fast"
        return
    fi
    if [[ -x "$ROOT_DIR/selfhost/stage00/s32-emu" ]]; then
        printf '%s\n' "$ROOT_DIR/selfhost/stage00/s32-emu"
        return
    fi
    if [[ -x "$ROOT_DIR/tools/emulator/slow32" ]]; then
        printf '%s\n' "$ROOT_DIR/tools/emulator/slow32"
        return
    fi
    printf '%s\n' "$ROOT_DIR/tools/emulator/slow32"
}

EMU_DEFAULT="$(choose_default_emu)"
EMU="$EMU_DEFAULT"

FROM="stage00"
TO="stage08"
SKIP_SELFHOST_KERNEL=0
SKIP_PURITY_GUARD=0
QUICK=0
KEEP_LOGS=0
TAIL_LINES="${TAIL_LINES:-80}"
LOG_DIR="$(mktemp -d /tmp/selfhost-stage-walk.XXXXXX)"

usage() {
    cat <<USAGE
Usage: $0 [--from stage00] [--to stage08] [--emu <path>] [--skip-selfhost-kernel] [--skip-purity-guard] [--quick] [--keep-logs]

Runs ordered stage checks so a clean checkout can be validated end-to-end.

Default sequence:
  stage00 -> stage01 -> stage02 -> stage08

Options:
  --from stageNN   Start stage (stage00..stage08)
  --to stageNN     End stage (stage00..stage08)
  --emu path       Emulator for stage01..stage08 (default: slow32-fast, then stage00 s32-emu, then slow32)
  --skip-selfhost-kernel
                   Skip stage01 selfhost-kernel regeneration/boot gate (dev fast-path)
  --skip-purity-guard
                   Skip bootstrap purity guard for stage00..02 (debug only)
  --quick          Run reduced smoke checks for faster iteration
  --keep-logs      Keep log directory on success (always kept on failure)
USAGE
}

while [[ $# -gt 0 ]]; do
    case "$1" in
        --from)
            shift
            [[ $# -gt 0 ]] || { echo "--from requires a value" >&2; exit 2; }
            FROM="$1"
            ;;
        --to)
            shift
            [[ $# -gt 0 ]] || { echo "--to requires a value" >&2; exit 2; }
            TO="$1"
            ;;
        --emu)
            shift
            [[ $# -gt 0 ]] || { echo "--emu requires a path" >&2; exit 2; }
            EMU="$1"
            ;;
        --skip-selfhost-kernel)
            SKIP_SELFHOST_KERNEL=1
            ;;
        --skip-purity-guard)
            SKIP_PURITY_GUARD=1
            ;;
        --quick)
            QUICK=1
            ;;
        --keep-logs)
            KEEP_LOGS=1
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

on_exit() {
    local rc=$?
    if [[ "$rc" -ne 0 ]]; then
        echo "Stage walk failed. Logs kept at: $LOG_DIR" >&2
        return
    fi
    if [[ "$KEEP_LOGS" -eq 1 ]]; then
        echo "Logs kept at: $LOG_DIR"
    else
        rm -rf "$LOG_DIR"
    fi
}
trap on_exit EXIT

run_logged() {
    local label="$1"
    local log="$2"
    shift 2

    if ! "$@" >"$log" 2>&1; then
        echo "FAIL: $label" >&2
        echo "Log: $log" >&2
        tail -n "$TAIL_LINES" "$log" >&2 || true
        return 1
    fi
}

stage_num() {
    case "$1" in
        stage00) echo 0 ;;
        stage01) echo 1 ;;
        stage02) echo 2 ;;
        stage08) echo 8 ;;
        *) return 1 ;;
    esac
}

FROM_N="$(stage_num "$FROM")" || { echo "Unknown --from stage: $FROM" >&2; exit 2; }
TO_N="$(stage_num "$TO")" || { echo "Unknown --to stage: $TO" >&2; exit 2; }
if (( FROM_N > TO_N )); then
    echo "--from must be <= --to" >&2
    exit 2
fi

for req in "$ROOT_DIR/forth/kernel.s32x" "$ROOT_DIR/forth/prelude.fth"; do
    [[ -f "$req" ]] || { echo "Missing required file: $req" >&2; exit 1; }
done

if (( TO_N >= 0 && FROM_N <= 6 )); then
    if [[ "$SKIP_PURITY_GUARD" -eq 0 ]]; then
        echo "[guard] bootstrap purity (stage00..06)"
        "$ROOT_DIR/selfhost/check-bootstrap-purity.sh"
    else
        echo "[guard] skipping bootstrap purity check (--skip-purity-guard)"
    fi
fi

run_stage00() {
    echo "[stage00] build + smoke"
    run_logged "stage00 build" "$LOG_DIR/stage00-build.log" \
        make -C "$ROOT_DIR/selfhost/stage00"
    run_logged "stage00 test" "$LOG_DIR/stage00-test.log" \
        make -C "$ROOT_DIR/selfhost/stage00" test
}

run_stage01() {
    echo "[stage01] assembler + archiver + linker + compiler regression"
    run_logged "stage01 asm test1" "$LOG_DIR/stage01-test1.log" \
        env STAGE01_EMU="$EMU" "$ROOT_DIR/selfhost/stage01/run-regression-as.sh" test1
    run_logged "stage01 ar test3" "$LOG_DIR/stage01-ar-test3.log" \
        env STAGE01_AR_EMU="$EMU" "$ROOT_DIR/selfhost/stage01/run-regression-ar.sh" test3
    run_logged "stage01 ld test3" "$LOG_DIR/stage01-ld-test3.log" \
        env STAGE01_LD_EMU="$EMU" "$ROOT_DIR/selfhost/stage01/run-regression-ld.sh" test3
    if [[ "$QUICK" -eq 0 ]]; then
        run_logged "stage01 ld archive" "$LOG_DIR/stage01-ld-archive.log" \
            env STAGE01_LD_EMU="$EMU" "$ROOT_DIR/selfhost/stage01/run-regression-ld.sh" archive
    else
        echo "[stage01] quick mode: skipping linker archive regression"
    fi
    if [[ "$SKIP_SELFHOST_KERNEL" -eq 0 && "$QUICK" -eq 0 ]]; then
        run_logged "stage01 selfhost-kernel" "$LOG_DIR/stage01-selfhost-kernel.log" \
            env SELFHOST_EMU="$EMU" "$ROOT_DIR/selfhost/stage01/run-selfhost-kernel.sh"
    elif [[ "$SKIP_SELFHOST_KERNEL" -eq 0 && "$QUICK" -eq 1 ]]; then
        echo "[stage01] quick mode: skipping selfhost kernel regeneration gate"
    else
        echo "[stage01] skipping selfhost kernel regeneration gate (--skip-selfhost-kernel)"
    fi
    echo "[stage01] compiler regression"
    run_logged "stage01 cc regression" "$LOG_DIR/stage01-cc-regression.log" \
        env STAGE01_CC_EMU="$EMU" "$ROOT_DIR/selfhost/stage01/run-regression-cc.sh"
    if [[ "$QUICK" -eq 0 ]]; then
        echo "[stage01] subset-c conformance"
        run_logged "stage01 cc subset" "$LOG_DIR/stage01-cc-subset.log" \
            env STAGE01_CC_EMU="$EMU" "$ROOT_DIR/selfhost/stage01/run-subset-conformance.sh"
        echo "[stage01] stage2-idiom conformance"
        run_logged "stage01 cc idioms" "$LOG_DIR/stage01-cc-idioms.log" \
            env STAGE01_CC_EMU="$EMU" "$ROOT_DIR/selfhost/stage01/run-subset-conformance.sh" \
                --manifest "$ROOT_DIR/selfhost/stage01/tests/manifests/subset-stage2-idioms.lst"
    else
        echo "[stage01] quick mode: skipping subset + idioms manifests"
    fi
}

run_stage02() {
    echo "[stage02] c-assembler + c-archiver replacement pipeline"
    run_logged "stage02 progressive-as test1" "$LOG_DIR/stage02-test1.log" \
        "$ROOT_DIR/selfhost/stage02/run-pipeline.sh" --mode progressive-as --test test1 --emu "$EMU"
    if [[ "$QUICK" -eq 0 ]]; then
        run_logged "stage02 progressive-as test2" "$LOG_DIR/stage02-test2.log" \
            "$ROOT_DIR/selfhost/stage02/run-pipeline.sh" --mode progressive-as --test test2 --emu "$EMU"
        run_logged "stage02 progressive-as test3" "$LOG_DIR/stage02-test3.log" \
            "$ROOT_DIR/selfhost/stage02/run-pipeline.sh" --mode progressive-as --test test3 --emu "$EMU"
    else
        echo "[stage02] quick mode: test1 only"
    fi
    echo "[stage02] c-archiver replacement smoke (c/rc/t/x/d/cs)"
    run_logged "stage02 ar-smoke" "$LOG_DIR/stage02-ar-smoke.log" \
        "$ROOT_DIR/selfhost/stage02/run-pipeline.sh" --mode stage6-ar-smoke --emu "$EMU"
    if [[ "$QUICK" -eq 0 ]]; then
        run_logged "stage02 ar-rc-smoke" "$LOG_DIR/stage02-ar-rc.log" \
            "$ROOT_DIR/selfhost/stage02/run-pipeline.sh" --mode stage6-ar-rc-smoke --emu "$EMU"
        run_logged "stage02 ar-tx-smoke" "$LOG_DIR/stage02-ar-tx.log" \
            "$ROOT_DIR/selfhost/stage02/run-pipeline.sh" --mode stage6-ar-tx-smoke --emu "$EMU"
        run_logged "stage02 ar-d-smoke" "$LOG_DIR/stage02-ar-d.log" \
            "$ROOT_DIR/selfhost/stage02/run-pipeline.sh" --mode stage6-ar-d-smoke --emu "$EMU"
        run_logged "stage02 ar-scan-smoke" "$LOG_DIR/stage02-ar-scan.log" \
            "$ROOT_DIR/selfhost/stage02/run-pipeline.sh" --mode stage6-ar-scan-smoke --emu "$EMU"
    else
        echo "[stage02] quick mode: c-only smoke"
    fi
    echo "[stage02] c-linker replacement spike"
    run_logged "stage02 ld-spike" "$LOG_DIR/stage02-ld.log" \
        env SELFHOST_EMU="$EMU" "$ROOT_DIR/selfhost/stage02/run-spike-ld.sh"
}

run_stage08() {
    echo "[stage08] pragmatic c-archiver parity gate"
    if [[ "$QUICK" -eq 0 ]]; then
        run_logged "stage08 regression" "$LOG_DIR/stage08.log" \
            "$ROOT_DIR/selfhost/stage08/run-regression.sh" --emu "$EMU"
    else
        run_logged "stage08 ar-smoke" "$LOG_DIR/stage08-smoke.log" \
            "$ROOT_DIR/selfhost/stage02/run-pipeline.sh" --mode stage6-ar-smoke --emu "$EMU"
        run_logged "stage08 cc-spike" "$LOG_DIR/stage08-cc.log" \
            "$ROOT_DIR/selfhost/stage08/run-cc-spike.sh" --emu "$EMU"
    fi
}

for st in stage00 stage01 stage02 stage08; do
    N="$(stage_num "$st")"
    if (( N < FROM_N || N > TO_N )); then
        continue
    fi
    "run_$st"
done

echo "OK: stage walk passed ($FROM -> $TO)"
echo "Emulator: $EMU"
echo "Mode: $([[ "$QUICK" -eq 1 ]] && echo quick || echo full)"
