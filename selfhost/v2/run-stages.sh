#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
ROOT_DIR="$(cd "$SCRIPT_DIR/../.." && pwd)"
if git -C "$SCRIPT_DIR" rev-parse --show-toplevel >/dev/null 2>&1; then
    ROOT_DIR="$(git -C "$SCRIPT_DIR" rev-parse --show-toplevel)"
fi

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

EMU_DEFAULT="$(choose_default_emu)"
EMU="$EMU_DEFAULT"

FROM="stage00"
TO="stage05"

usage() {
    cat <<USAGE
Usage: $0 [--from stage00] [--to stage05] [--emu <path>]

Runs ordered V2 stage checks so a clean checkout can be validated end-to-end.

Default sequence:
  stage00 -> stage01 -> stage02 -> stage03 -> stage04 -> stage05

Options:
  --from stageNN   Start stage (stage00..stage05)
  --to stageNN     End stage (stage00..stage05)
  --emu path       Emulator for stage01..stage05 (default: slow32-fast, then stage00 s32-emu, then slow32)
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

stage_num() {
    case "$1" in
        stage00) echo 0 ;;
        stage01) echo 1 ;;
        stage02) echo 2 ;;
        stage03) echo 3 ;;
        stage04) echo 4 ;;
        stage05) echo 5 ;;
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

run_stage00() {
    echo "[stage00] build + smoke"
    make -C "$ROOT_DIR/selfhost/v2/stage00" >/tmp/v2-stage00-build.log 2>&1
    make -C "$ROOT_DIR/selfhost/v2/stage00" test >/tmp/v2-stage00-test.log 2>&1
}

run_stage01() {
    echo "[stage01] assembler regression"
    STAGE01_EMU="$EMU" "$ROOT_DIR/selfhost/v2/stage01/run-regression.sh" test1 >/tmp/v2-stage01.log 2>&1
}

run_stage02() {
    echo "[stage02] archiver regression"
    STAGE2_AR_EMU="$EMU" "$ROOT_DIR/selfhost/v2/stage02/run-regression.sh" test3 >/tmp/v2-stage02.log 2>&1
}

run_stage03() {
    echo "[stage03] linker regression"
    STAGE3_EMU="$EMU" "$ROOT_DIR/selfhost/v2/stage03/run-regression.sh" test3 >/tmp/v2-stage03-test3.log 2>&1
    STAGE3_EMU="$EMU" "$ROOT_DIR/selfhost/v2/stage03/run-regression.sh" archive >/tmp/v2-stage03-archive.log 2>&1
}

run_stage04() {
    echo "[stage04] compiler regression"
    STAGE4_EMU="$EMU" "$ROOT_DIR/selfhost/v2/stage04/run-regression.sh" >/tmp/v2-stage04.log 2>&1
}

run_stage05() {
    echo "[stage05] c-assembler replacement pipeline"
    "$ROOT_DIR/selfhost/v2/stage05/run-pipeline.sh" --mode progressive-as --test test1 --emu "$EMU" >/tmp/v2-stage05-test1.log 2>&1
    "$ROOT_DIR/selfhost/v2/stage05/run-pipeline.sh" --mode progressive-as --test test2 --emu "$EMU" >/tmp/v2-stage05-test2.log 2>&1
    "$ROOT_DIR/selfhost/v2/stage05/run-pipeline.sh" --mode progressive-as --test test3 --emu "$EMU" >/tmp/v2-stage05-test3.log 2>&1
}

for st in stage00 stage01 stage02 stage03 stage04 stage05; do
    N="$(stage_num "$st")"
    if (( N < FROM_N || N > TO_N )); then
        continue
    fi
    "run_$st"
done

echo "OK: v2 stage walk passed ($FROM -> $TO)"
echo "Emulator: $EMU"
