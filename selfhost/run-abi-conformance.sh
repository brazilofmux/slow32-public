#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
STAGE="all"

usage() {
    cat <<USAGE
Usage: $0 [--stage 03|04|05|06|all] [extra args...]

Run selfhost ABI conformance tests.
Extra args are forwarded to stage runners (for example: --emu, --keep-artifacts).
USAGE
}

while [[ $# -gt 0 ]]; do
    case "$1" in
        --stage)
            shift
            [[ $# -gt 0 ]] || { echo "--stage requires a value" >&2; exit 2; }
            STAGE="$1"
            ;;
        -h|--help)
            usage
            exit 0
            ;;
        *)
            break
            ;;
    esac
    shift
done

run_stage() {
    local s="$1"
    shift
    echo ""
    echo "=== ABI stage$s ==="
    set +e
    "$SCRIPT_DIR/stage${s}/run-abi-conformance.sh" "$@"
    local rc=$?
    set -e
    return "$rc"
}

case "$STAGE" in
    03) run_stage "03" "$@" ;;
    04) run_stage "04" "$@" ;;
    05) run_stage "05" "$@" ;;
    06) run_stage "06" "$@" ;;
    all)
        fail=0
        run_stage "03" "$@" || fail=1
        run_stage "04" "$@" || fail=1
        run_stage "05" "$@" || fail=1
        run_stage "06" "$@" || fail=1
        if [[ "$fail" -ne 0 ]]; then
            exit 1
        fi
        ;;
    *)
        echo "Unknown --stage value: $STAGE" >&2
        usage
        exit 2
        ;;
esac
