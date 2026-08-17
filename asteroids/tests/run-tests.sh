#!/bin/bash
# Deterministic by design: fixed timestep, integer math, seeded RNG,
# scripted keys (one injected event per frame). Golden values below
# were captured at authoring time; a legitimate gameplay change
# refreshes them, an engine divergence fails only some engines.
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
AST_DIR="$(dirname "$SCRIPT_DIR")"
ROOT="$(dirname "$AST_DIR")"

echo "Building asteroids..."
(cd "$AST_DIR" && bash build.sh >/dev/null)

ENGINES=""
for e in "$ROOT/tools/emulator/slow32" "$ROOT/tools/emulator/slow32-fast" \
         "$ROOT/tools/dbt/slow32-dbt"; do
    if [ -x "$e" ]; then
        ENGINES="$ENGINES $e"
    fi
done
if [ -z "$ENGINES" ]; then
    echo "no emulators found"
    exit 1
fi

work="$(mktemp -d "${TMPDIR:-/tmp}/s32-ast.XXXXXX")"
trap 'rm -rf "$work"' EXIT

# Spin-and-spray timeline: LEFT held, then fire on alternate frames.
# Must stay under the 256-event injection queue cap.
python3 - "$work/keys.bin" <<'PY'
import struct, sys
evs = [(0x102, 1)]
for _ in range(120):
    evs.append((32, 1))
    evs.append((32, 0))
assert len(evs) <= 256
open(sys.argv[1], "wb").write(
    b"".join(struct.pack("<HBB", c, d, 0) for c, d in evs))
PY

IDLE_REPORT="report frames=120 score=0 lives=3 wave=1 rocks=4"
IDLE_HASH="8d2350fcadef9b5f"
PLAY_REPORT="report frames=320 score=280 lives=3 wave=1 rocks=12"
PLAY_HASH="1096a16e0df00f45"

fail=0
for EMU in $ENGINES; do
    name="$(basename "$EMU")"

    rm -rf "$work/idle"
    out="$(cd "$work" && S32_TUBE_DUMP="$work/idle" \
        "$EMU" "$AST_DIR/asteroids.s32x" --frames 120 --seed 7 2>/dev/null \
        | grep '^report' || true)"
    h="$(cat "$work/idle/000119.hash" 2>/dev/null || echo missing)"
    if [ "$out" = "$IDLE_REPORT" ] && [ "$h" = "$IDLE_HASH" ]; then
        echo "  OK  $name idle: $h"
    else
        echo "  FAIL $name idle: report='$out' hash=$h"
        fail=1
    fi

    rm -rf "$work/play"
    out="$(cd "$work" && S32_TUBE_KEYS="$work/keys.bin" S32_TUBE_DUMP="$work/play" \
        "$EMU" "$AST_DIR/asteroids.s32x" --frames 320 --seed 7 2>/dev/null \
        | grep '^report' || true)"
    h="$(cat "$work/play/000319.hash" 2>/dev/null || echo missing)"
    if [ "$out" = "$PLAY_REPORT" ] && [ "$h" = "$PLAY_HASH" ]; then
        echo "  OK  $name play: $h (score 280, rocks split)"
    else
        echo "  FAIL $name play: report='$out' hash=$h"
        fail=1
    fi
done

# Graceful degradation: no tube means a message, not a crash.
EMU="$ROOT/tools/emulator/slow32-fast"
out="$(cd "$work" && "$EMU" --deny tube "$AST_DIR/asteroids.s32x" --frames 10 2>/dev/null || true)"
if echo "$out" | grep -q "no tube"; then
    echo "  OK  deny: falls back with a message"
else
    echo "  FAIL deny: $out"
    fail=1
fi

if [ "$fail" -ne 0 ]; then
    echo "=== TESTS FAILED ==="
    exit 1
fi
echo "=== All tests passed ==="
