#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
ROOT_DIR="${STAGE4_ROOT:-$(cd "$SCRIPT_DIR/../.." && pwd)}"
if git -C "$SCRIPT_DIR" rev-parse --show-toplevel >/dev/null 2>&1; then
    ROOT_DIR="$(git -C "$SCRIPT_DIR" rev-parse --show-toplevel)"
fi

EMU="${STAGE4_EMU:-$ROOT_DIR/tools/emulator/slow32-fast}"
MANIFEST="${STAGE4_GAP_MANIFEST:-$SCRIPT_DIR/tests/manifests/subset-known-gaps.lst}"

usage() {
    cat <<USAGE
Usage: $0 [--emu <path>] [--manifest <path>]

Runs known-gap subset tests and reports pass/fail per case.
This scan is informational and always exits 0.
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
if [[ "$MANIFEST" != /* ]]; then
    MANIFEST="$ROOT_DIR/$MANIFEST"
fi

[[ -f "$MANIFEST" ]] || { echo "Missing manifest: $MANIFEST" >&2; exit 1; }

echo "[subset-gap-scan]"
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
    if selfhost/v2/stage05/run-pipeline.sh --mode baseline --test "$src" --emu "$EMU" >/tmp/stage4-gap-$base.log 2>&1; then
        echo "    PASS (gap closed)"
        pass=$((pass + 1))
    else
        echo "    FAIL (still open)"
        fail=$((fail + 1))
    fi
done < "$MANIFEST"

echo "Known-gap summary: PASS=$pass FAIL=$fail"
