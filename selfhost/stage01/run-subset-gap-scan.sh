#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
SELFHOST_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"
ROOT_DIR="$(cd "$SELFHOST_DIR/.." && pwd)"

EMU="${STAGE01_CC_EMU:-$SELFHOST_DIR/stage00/s32-emu}"
MANIFEST="${STAGE01_CC_GAP_MANIFEST:-$SCRIPT_DIR/tests/manifests/subset-known-gaps.lst}"
MODE="${STAGE01_CC_GAP_MODE:-baseline}"

usage() {
    cat <<USAGE
Usage: $0 [--emu <path>] [--manifest <path>] [--mode <pipeline-mode>]

Runs known-gap subset tests and reports pass/fail per case.
This scan is informational and always exits 0.
Pipeline mode defaults to baseline and is passed to stage02/run-pipeline.sh.
USAGE
}

while [[ $# -gt 0 ]]; do
    case "$1" in
        --emu)
            shift
            [[ $# -gt 0 ]] || { echo "--emu requires a path" >&2; exit 2; }
            EMU="$1"
            ;;
        --manifest)
            shift
            [[ $# -gt 0 ]] || { echo "--manifest requires a path" >&2; exit 2; }
            MANIFEST="$1"
            ;;
        --mode)
            shift
            [[ $# -gt 0 ]] || { echo "--mode requires a value" >&2; exit 2; }
            MODE="$1"
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

if [[ "$MANIFEST" != /* ]]; then
    MANIFEST="$SCRIPT_DIR/$MANIFEST"
fi

[[ -f "$MANIFEST" ]] || { echo "Missing manifest: $MANIFEST" >&2; exit 1; }

echo "[subset-gap-scan]"
echo "mode: $MODE"
pass=0
fail=0

while IFS= read -r src || [[ -n "$src" ]]; do
    [[ -z "$src" ]] && continue
    [[ "$src" =~ ^# ]] && continue
    if [[ "$src" != /* ]]; then
        src="$ROOT_DIR/$src"
    fi
    base="$(basename "$src" .c)"
    echo "  - $base"
    if selfhost/stage02/run-pipeline.sh --mode "$MODE" --test "$src" --emu "$EMU" >/tmp/stage4-gap-$base.log 2>&1; then
        echo "    PASS (gap closed)"
        pass=$((pass + 1))
    else
        echo "    FAIL (still open)"
        fail=$((fail + 1))
    fi
done < "$MANIFEST"

echo "Known-gap summary: PASS=$pass FAIL=$fail"
