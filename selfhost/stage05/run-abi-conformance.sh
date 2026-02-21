#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
SELFHOST_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"
CC_EXE="$SCRIPT_DIR/cc.s32x"
if [[ ! -f "$CC_EXE" ]]; then
    CC_EXE="$SCRIPT_DIR/s12cc.s32x"
fi

exec "$SELFHOST_DIR/stage03/run-abi-conformance.sh" \
    --cc "$CC_EXE" \
    --as "$SCRIPT_DIR/s32-as.s32x" \
    --ld "$SCRIPT_DIR/s32-ld.s32x" \
    --runtime-dir "$SCRIPT_DIR" \
    --libc-dir "$SCRIPT_DIR/libc" \
    "$@"
