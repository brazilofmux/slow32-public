#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
SELFHOST_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"
ROOT_DIR="$(cd "$SELFHOST_DIR/.." && pwd)"

EMU="${STAGE8_EMU:-}"
EMU_EXPLICIT=0
PIPE="$SELFHOST_DIR/stage05/run-pipeline.sh"

choose_default_emu() {
    printf '%s\n' "$SELFHOST_DIR/stage00/s32-emu"
}

usage() {
    cat <<USAGE
Usage: $0 [--emu <path>]

Runs Stage08 pragmatic archiver parity gate using the subset-C s32-ar path:
  c/rc/t/x/d/m/v/p/cs
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

[[ -x "$PIPE" ]] || { echo "Missing pipeline runner: $PIPE" >&2; exit 1; }
[[ -x "$EMU" ]] || { echo "Missing emulator: $EMU" >&2; exit 1; }

echo "[stage08] subset-c archiver parity gate"
"$PIPE" --mode stage6-ar-smoke --emu "$EMU" >/tmp/v2-stage08-c.log 2>&1
"$PIPE" --mode stage6-ar-rc-smoke --emu "$EMU" >/tmp/v2-stage08-rc.log 2>&1
"$PIPE" --mode stage6-ar-tx-smoke --emu "$EMU" >/tmp/v2-stage08-tx.log 2>&1
"$PIPE" --mode stage6-ar-d-smoke --emu "$EMU" >/tmp/v2-stage08-d.log 2>&1
"$PIPE" --mode stage6-ar-m-smoke --emu "$EMU" >/tmp/v2-stage08-m.log 2>&1
"$PIPE" --mode stage6-ar-vp-smoke --emu "$EMU" >/tmp/v2-stage08-vp.log 2>&1
"$PIPE" --mode stage6-ar-scan-smoke --emu "$EMU" >/tmp/v2-stage08-cs.log 2>&1

echo "[stage08] cc-min compiler spike"
"$SCRIPT_DIR/run-cc-spike.sh" --emu "$EMU" >/tmp/v2-stage08-cc.log 2>&1

echo "OK: stage08 archiver parity gate"
echo "Emulator: $EMU"
