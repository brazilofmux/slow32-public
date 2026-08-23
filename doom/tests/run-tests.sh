#!/bin/bash
# Doom as a test fixture: -timedemo demo3 is fully deterministic, so
# every engine must present the same 2173 frames and agree on the
# final frame hash, bit for bit.
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
DOOM_DIR="$(dirname "$SCRIPT_DIR")"
ROOT="$(dirname "$DOOM_DIR")"

if [ ! -f "$DOOM_DIR/doom1.wad" ]; then
    echo "SKIP: doom1.wad not present (run ./fetch-wad.sh)"
    exit 0
fi

echo "Building doom..."
(cd "$DOOM_DIR" && bash build.sh >/dev/null)

FRAMES=2173
LAST=002172.hash
HASH=75fd024951e9cf7f

work="$(mktemp -d "${TMPDIR:-/tmp}/s32-doom.XXXXXX")"
trap 'rm -rf "$work"' EXIT

fail=0
for EMU in "$ROOT/tools/emulator/slow32-fast" "$ROOT/tools/dbt/slow32-dbt"; do
    [ -x "$EMU" ] || continue
    name="$(basename "$EMU")"
    rm -rf "$work/td"
    (cd "$work" && S32_TUBE_DUMP="$work/td" \
        timeout 600 "$EMU" "$DOOM_DIR/doom.s32x" -iwad "$DOOM_DIR/doom1.wad" -timedemo demo3) \
        >"$work/out.txt" 2>&1 || true
    n="$(ls "$work/td" 2>/dev/null | wc -l | tr -d ' ')"
    h="$(cat "$work/td/$LAST" 2>/dev/null || echo missing)"
    if [ "$n" = "$FRAMES" ] && [ "$h" = "$HASH" ]; then
        echo "  OK  $name: $FRAMES frames, final $h"
    else
        echo "  FAIL $name: frames=$n hash=$h (want $FRAMES/$HASH)"
        tail -5 "$work/out.txt"
        fail=1
    fi
done

out="$(cd "$work" && "$ROOT/tools/emulator/slow32-fast" --deny tube \
    "$DOOM_DIR/doom.s32x" 2>/dev/null || true)"
if echo "$out" | grep -q "no tube"; then
    echo "  OK  deny: falls back with a message"
else
    echo "  FAIL deny"
    fail=1
fi

if [ "$fail" -ne 0 ]; then
    echo "=== TESTS FAILED ==="
    exit 1
fi
echo "=== All tests passed ==="
