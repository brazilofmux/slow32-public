#!/bin/bash
# Host gcc WK1 load checks (GitHub issue 115).
set -euo pipefail
DIR="$(cd "$(dirname "$0")" && pwd)"
SRC="$(dirname "$DIR")/src"
work="$(mktemp -d "${TMPDIR:-/tmp}/s32-wk1.XXXXXX")"
cleanup() { rm -rf "$work"; }
trap cleanup EXIT
cc -Wall -Wextra -O2 -I"$SRC" -o "$work/host_wk1" \
    "$SRC/sheet.c" "$SRC/formula.c" "$SRC/wk1.c" "$SRC/dbfio.c" \
    "$DIR/host_wk1.c"
(cd "$work" && ./host_wk1)
echo "=== host WK1 tests passed ==="
