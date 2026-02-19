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
  stage00 -> stage01 -> stage03 -> stage04 -> stage05 -> stage06 -> stage07 -> stage08

Options:
  --from stageNN   Start stage (stage00..stage08)
  --to stageNN     End stage (stage00..stage08)
  --emu path       Emulator for stage01..stage08 (default: slow32-fast, then stage00 s32-emu, then slow32)
  --skip-selfhost-kernel
                   Skip stage03 selfhost-kernel regeneration/boot gate (dev fast-path)
  --skip-purity-guard
                   Skip bootstrap purity guard for stage00..06 (debug only)
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
        stage03) echo 3 ;;
        stage04) echo 4 ;;
        stage05) echo 5 ;;
        stage06) echo 6 ;;
        stage07) echo 7 ;;
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
    echo "[stage01] assembler + archiver regression"
    run_logged "stage01 asm test1" "$LOG_DIR/stage01-test1.log" \
        env STAGE01_EMU="$EMU" "$ROOT_DIR/selfhost/stage01/run-regression-as.sh" test1
    run_logged "stage01 ar test3" "$LOG_DIR/stage01-ar-test3.log" \
        env STAGE01_AR_EMU="$EMU" "$ROOT_DIR/selfhost/stage01/run-regression-ar.sh" test3
}

run_stage03() {
    echo "[stage03] linker regression"
    run_logged "stage03 test3" "$LOG_DIR/stage03-test3.log" \
        env STAGE3_EMU="$EMU" "$ROOT_DIR/selfhost/stage03/run-regression.sh" test3
    if [[ "$QUICK" -eq 0 ]]; then
        run_logged "stage03 archive" "$LOG_DIR/stage03-archive.log" \
            env STAGE3_EMU="$EMU" "$ROOT_DIR/selfhost/stage03/run-regression.sh" archive
    else
        echo "[stage03] quick mode: skipping archive regression"
    fi
    if [[ "$SKIP_SELFHOST_KERNEL" -eq 0 && "$QUICK" -eq 0 ]]; then
        run_logged "stage03 selfhost-kernel" "$LOG_DIR/stage03-selfhost-kernel.log" \
            env SELFHOST_EMU="$EMU" "$ROOT_DIR/selfhost/stage03/run-selfhost-kernel.sh"
    elif [[ "$SKIP_SELFHOST_KERNEL" -eq 0 && "$QUICK" -eq 1 ]]; then
        echo "[stage03] quick mode: skipping selfhost kernel regeneration gate"
    else
        echo "[stage03] skipping selfhost kernel regeneration gate (--skip-selfhost-kernel)"
    fi
}

run_stage04() {
    echo "[stage04] compiler regression"
    run_logged "stage04 regression" "$LOG_DIR/stage04-regression.log" \
        env STAGE4_EMU="$EMU" "$ROOT_DIR/selfhost/stage04/run-regression.sh"
    if [[ "$QUICK" -eq 0 ]]; then
        echo "[stage04] subset-c conformance"
        run_logged "stage04 subset" "$LOG_DIR/stage04-subset.log" \
            env STAGE4_EMU="$EMU" "$ROOT_DIR/selfhost/stage04/run-subset-conformance.sh"
        echo "[stage04] stage5-idiom conformance"
        run_logged "stage04 idioms" "$LOG_DIR/stage04-idioms.log" \
            env STAGE4_EMU="$EMU" "$ROOT_DIR/selfhost/stage04/run-subset-conformance.sh" \
                --manifest "$ROOT_DIR/selfhost/stage04/tests/manifests/subset-stage5-idioms.lst"
    else
        echo "[stage04] quick mode: skipping subset + idioms manifests"
    fi
}

run_stage05() {
    echo "[stage05] c-assembler replacement pipeline"
    run_logged "stage05 progressive-as test1" "$LOG_DIR/stage05-test1.log" \
        "$ROOT_DIR/selfhost/stage05/run-pipeline.sh" --mode progressive-as --test test1 --emu "$EMU"
    if [[ "$QUICK" -eq 0 ]]; then
        run_logged "stage05 progressive-as test2" "$LOG_DIR/stage05-test2.log" \
            "$ROOT_DIR/selfhost/stage05/run-pipeline.sh" --mode progressive-as --test test2 --emu "$EMU"
        run_logged "stage05 progressive-as test3" "$LOG_DIR/stage05-test3.log" \
            "$ROOT_DIR/selfhost/stage05/run-pipeline.sh" --mode progressive-as --test test3 --emu "$EMU"
    else
        echo "[stage05] quick mode: test1 only"
    fi
}

run_stage06() {
    echo "[stage06] c-archiver replacement smoke (c/rc/t/x/d/cs)"
    run_logged "stage06 ar-smoke" "$LOG_DIR/stage06-smoke.log" \
        "$ROOT_DIR/selfhost/stage05/run-pipeline.sh" --mode stage6-ar-smoke --emu "$EMU"
    if [[ "$QUICK" -eq 0 ]]; then
        run_logged "stage06 ar-rc-smoke" "$LOG_DIR/stage06-rc.log" \
            "$ROOT_DIR/selfhost/stage05/run-pipeline.sh" --mode stage6-ar-rc-smoke --emu "$EMU"
        run_logged "stage06 ar-tx-smoke" "$LOG_DIR/stage06-tx.log" \
            "$ROOT_DIR/selfhost/stage05/run-pipeline.sh" --mode stage6-ar-tx-smoke --emu "$EMU"
        run_logged "stage06 ar-d-smoke" "$LOG_DIR/stage06-d.log" \
            "$ROOT_DIR/selfhost/stage05/run-pipeline.sh" --mode stage6-ar-d-smoke --emu "$EMU"
        run_logged "stage06 ar-scan-smoke" "$LOG_DIR/stage06-scan.log" \
            "$ROOT_DIR/selfhost/stage05/run-pipeline.sh" --mode stage6-ar-scan-smoke --emu "$EMU"
    else
        echo "[stage06] quick mode: c-only smoke"
    fi
}

run_stage07() {
    echo "[stage07] c-linker replacement spike (+ reloc)"
    if [[ "$QUICK" -eq 0 ]]; then
        run_logged "stage07 spike+reloc" "$LOG_DIR/stage07.log" \
            "$ROOT_DIR/selfhost/stage07/run-spike.sh" --emu "$EMU" --with-reloc-spike
    else
        run_logged "stage07 spike" "$LOG_DIR/stage07.log" \
            "$ROOT_DIR/selfhost/stage07/run-spike.sh" --emu "$EMU"
    fi
}

run_stage08() {
    echo "[stage08] pragmatic c-archiver parity gate"
    if [[ "$QUICK" -eq 0 ]]; then
        run_logged "stage08 regression" "$LOG_DIR/stage08.log" \
            "$ROOT_DIR/selfhost/stage08/run-regression.sh" --emu "$EMU"
    else
        run_logged "stage08 ar-smoke" "$LOG_DIR/stage08-smoke.log" \
            "$ROOT_DIR/selfhost/stage05/run-pipeline.sh" --mode stage6-ar-smoke --emu "$EMU"
        run_logged "stage08 cc-spike" "$LOG_DIR/stage08-cc.log" \
            "$ROOT_DIR/selfhost/stage08/run-cc-spike.sh" --emu "$EMU"
    fi
}

for st in stage00 stage01 stage03 stage04 stage05 stage06 stage07 stage08; do
    N="$(stage_num "$st")"
    if (( N < FROM_N || N > TO_N )); then
        continue
    fi
    "run_$st"
done

echo "OK: stage walk passed ($FROM -> $TO)"
echo "Emulator: $EMU"
echo "Mode: $([[ "$QUICK" -eq 1 ]] && echo quick || echo full)"
