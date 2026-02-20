#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
SELFHOST_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"

exec "$SELFHOST_DIR/stage03/run-abi-conformance.sh" \
    --cc "$SCRIPT_DIR/s12cc.s32x" \
    --as "$SELFHOST_DIR/stage03/s32-as.s32x" \
    --ld "$SELFHOST_DIR/stage03/s32-ld.s32x" \
    --runtime-dir "$SCRIPT_DIR" \
    --libc-dir "$SCRIPT_DIR/libc" \
    "$@"
