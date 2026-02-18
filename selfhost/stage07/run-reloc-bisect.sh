#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
SELFHOST_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"
ROOT_DIR="$(cd "$SELFHOST_DIR/.." && pwd)"

EMU="${STAGE7_EMU:-$SELFHOST_DIR/stage00/s32-emu}"
MANIFEST="${RELOC_BISECT_MANIFEST:-$SELFHOST_DIR/stage04/tests/manifests/reloc-bisect.lst}"
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

if [[ "$MANIFEST" != /* ]]; then
    MANIFEST="$SCRIPT_DIR/$MANIFEST"
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

    cmd=("$SCRIPT_DIR/run-spike.sh" "--emu" "$EMU")
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
