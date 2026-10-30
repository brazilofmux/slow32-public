#!/bin/bash
# Host s32-xbc emission check for LOCAL vs PRIVATE (GitHub issue 113).
set -euo pipefail
CLIP_DIR="$(cd "$(dirname "$0")/.." && pwd)"
(cd "$CLIP_DIR" && gcc -Wall -Wextra -O2 -o s32-xbc s32-xbc.c)
mkdir -p "$CLIP_DIR/out"

"$CLIP_DIR/s32-xbc" "$CLIP_DIR/tests/funclocal.prg" -o "$CLIP_DIR/out/funclocal.c"
"$CLIP_DIR/s32-xbc" "$CLIP_DIR/tests/funcpriv.prg" -o "$CLIP_DIR/out/funcpriv.c"

fail=0
if grep -q 'clip_local(&scope, "x")' "$CLIP_DIR/out/funclocal.c"; then
    echo "  OK  LOCAL -> clip_local"
else
    echo "  FAIL LOCAL -> clip_local"
    fail=1
fi
if grep -q 'clip_private(&scope, "x")' "$CLIP_DIR/out/funclocal.c"; then
    echo "  FAIL LOCAL also emitted clip_private"
    fail=1
fi
if grep -q 'clip_private(&scope, "x")' "$CLIP_DIR/out/funcpriv.c"; then
    echo "  OK  PRIVATE -> clip_private"
else
    echo "  FAIL PRIVATE -> clip_private"
    fail=1
fi
if grep -q clip_local "$CLIP_DIR/out/funcpriv.c"; then
    echo "  FAIL PRIVATE emitted clip_local"
    fail=1
fi

if [ "$fail" -ne 0 ]; then
    echo "=== funclocal.c ---"
    cat "$CLIP_DIR/out/funclocal.c"
    exit 1
fi
echo "=== host clip LOCAL tests passed ==="
