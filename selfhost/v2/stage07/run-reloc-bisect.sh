#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
ROOT_DIR="${SELFHOST_ROOT:-$(cd "$SCRIPT_DIR/../../.." && pwd)}"
if git -C "$SCRIPT_DIR" rev-parse --show-toplevel >/dev/null 2>&1; then
    ROOT_DIR="$(git -C "$SCRIPT_DIR" rev-parse --show-toplevel)"
fi

EMU="${STAGE7_EMU:-$ROOT_DIR/tools/emulator/slow32-fast}"
MANIFEST="${RELOC_BISECT_MANIFEST:-$ROOT_DIR/selfhost/v2/stage04/tests/manifests/reloc-bisect.lst}"
WITH_RELOC_SPIKE=0
KEEP_ARTIFACTS=0

usage() {
    cat <<USAGE
Usage: $0 [--emu <path>] [--manifest <path>] [--with-reloc-spike] [--keep-artifacts]

Runs Stage07 linker-source bisect corpus and stops at first failing variant.
Each entry is passed as STAGE7_SRC to stage07/run-spike.sh.
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
        --with-reloc-spike)
            WITH_RELOC_SPIKE=1
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
if [[ "$MANIFEST" != /* ]]; then
    MANIFEST="$ROOT_DIR/$MANIFEST"
fi

[[ -f "$EMU" ]] || { echo "Missing emulator: $EMU" >&2; exit 1; }
[[ -f "$MANIFEST" ]] || { echo "Missing manifest: $MANIFEST" >&2; exit 1; }

echo "[reloc-bisect] manifest: $MANIFEST"

count=0
while IFS= read -r src || [[ -n "$src" ]]; do
    [[ -z "$src" ]] && continue
    [[ "$src" =~ ^# ]] && continue
    if [[ "$src" != /* ]]; then
        src="$ROOT_DIR/$src"
    fi
    [[ -f "$src" ]] || { echo "Missing source: $src" >&2; exit 1; }

    count=$((count + 1))
    base="$(basename "$src")"
    echo "  - [$count] $base"

    cmd=("$ROOT_DIR/selfhost/v2/stage07/run-spike.sh" "--emu" "$EMU")
    if [[ "$WITH_RELOC_SPIKE" -eq 1 ]]; then
        cmd+=("--with-reloc-spike")
    fi
    if [[ "$KEEP_ARTIFACTS" -eq 1 ]]; then
        cmd+=("--keep-artifacts")
    fi

    set +e
    STAGE7_SRC="$src" "${cmd[@]}" >/tmp/stage7-reloc-bisect.log 2>&1
    rc=$?
    set -e
    if [[ "$rc" -ne 0 ]]; then
        echo "FAIL: first failing variant is $base (rc=$rc)"
        tail -n 60 /tmp/stage7-reloc-bisect.log
        exit 1
    fi
done < "$MANIFEST"

echo "OK: reloc bisect corpus passed ($count variants)"
