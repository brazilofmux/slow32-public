#!/bin/bash
# Host gcc checks for N wrap and CTOD Feb 31 (GitHub issue 112).
set -euo pipefail
DIR="$(cd "$(dirname "$0")" && pwd)"
SRC="$(dirname "$DIR")/src"
cc -Wall -Wextra -O2 -I"$SRC" -o "$DIR/host_date_numeric" \
    "$SRC/date.c" "$SRC/field.c" \
    "$DIR/host_date_numeric.c" "$DIR/host_field_stubs.c"
"$DIR/host_date_numeric"
rm -f "$DIR/host_date_numeric"
echo "=== host date/numeric tests passed ==="
