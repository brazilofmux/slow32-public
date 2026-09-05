#!/usr/bin/env bash
# dfsort/build.sh -- s32sort for SLOW-32 (out/s32sort.s32x) and for the host (out/s32sort)
set -eu
HERE="$(cd "$(dirname "$0")" && pwd)"; ROOT="$(cd "$HERE/.." && pwd)"; mkdir -p "$HERE/out"
"$ROOT/slow32cc" --libc=mmio -O2 "$HERE/s32sort.c" -o "$HERE/out/s32sort.s32x" >/dev/null
cc -std=c99 -O2 -w -o "$HERE/out/s32sort" "$HERE/s32sort.c"
echo "built: $HERE/out/s32sort.s32x (SLOW-32), $HERE/out/s32sort (host)"
