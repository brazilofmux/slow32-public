#!/bin/bash
# Build s32-cobc (host) and libcob (guest).
set -eu
HERE="$(cd "$(dirname "$0")" && pwd)"
mkdir -p "$HERE/out"
CC="${CC:-cc}"
# picture_scan.c is Ragel -G2 output: its fallthrough and unused state
# constants are silenced in the file itself, so this line stays -Wall -Wextra.
$CC -std=c99 -O1 -Wall -Wextra -o "$HERE/out/s32-cobc" \
    "$HERE/src/s32-cobc.c" "$HERE/src/picture.c" "$HERE/src/picture_scan.c"
echo "built: $HERE/out/s32-cobc"
"$HERE/libcob/build.sh"
