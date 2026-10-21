#!/bin/bash
# Host gcc DBF load checks (GitHub issue 117).
set -euo pipefail
DIR="$(cd "$(dirname "$0")" && pwd)"
SRC="$(dirname "$DIR")/src"
work="$(mktemp -d "${TMPDIR:-/tmp}/s32-dbf.XXXXXX")"
cleanup() { rm -rf "$work"; }
trap cleanup EXIT
cc -Wall -Wextra -O2 -I"$SRC" -o "$work/host_dbf" \
    "$SRC/sheet.c" "$SRC/formula.c" "$SRC/wk1.c" "$SRC/dbfio.c" \
    "$DIR/host_dbf.c"
(cd "$work" && ./host_dbf)
echo "=== host DBF tests passed ==="
