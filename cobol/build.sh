#!/bin/bash
# Build s32-cobc (host) and libcob (guest).
set -eu
HERE="$(cd "$(dirname "$0")" && pwd)"
mkdir -p "$HERE/out"
# never under a running harness: its later programs would be compiled by
# a different compiler than its earlier ones, and its verdict would
# describe neither (it happened three times on 2026-09-30).  The harness
# itself may build (compile.sh does when an output is missing).
lock="$HERE/out/harness.lock"
if [ -z "${S32_HARNESS:-}" ] && [ -f "$lock" ] && kill -0 "$(cat "$lock" 2>/dev/null)" 2>/dev/null; then
    echo "build.sh: tests/run-tests.sh is running (pid $(cat "$lock")); build after it finishes" >&2
    exit 1
fi
CC="${CC:-cc}"
# picture_scan.c is Ragel -G2 output: its fallthrough and unused state
# constants are silenced in the file itself, so this line stays -Wall -Wextra.
$CC -std=c99 -O1 -Wall -Wextra -o "$HERE/out/s32-cobc" \
    "$HERE/src/s32-cobc.c" "$HERE/src/picture.c" "$HERE/src/picture_scan.c"
echo "built: $HERE/out/s32-cobc"
"$HERE/libcob/build.sh"
