#!/bin/bash
# The ppu conformance reel: golden hashes across engines + spec-derived
# pixel assertions. Deterministic by construction (no keys, no clock).
set -u

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
REEL_DIR="$(dirname "$SCRIPT_DIR")"
ROOT_DIR="$(dirname "$REEL_DIR")"
REEL="$REEL_DIR/reel.s32x"
GOLDEN="$SCRIPT_DIR/golden"

if [ ! -f "$REEL" ]; then
    echo "reel.s32x not found — run build.sh first"
    exit 1
fi

ENGINES=""
for e in "$ROOT_DIR/tools/emulator/slow32" \
         "$ROOT_DIR/tools/emulator/slow32-fast" \
         "$ROOT_DIR/tools/dbt/slow32-dbt"; do
    [ -x "$e" ] && ENGINES="$ENGINES $e"
done

WORK="$(mktemp -d /tmp/ppu-reel-test.XXXXXX)"
trap 'rm -rf "$WORK"' EXIT

FAIL=0
FIRST=""
for e in $ENGINES; do
    name="$(basename "$e")"
    d="$WORK/$name"
    mkdir -p "$d"
    if ! (cd "$WORK" && S32_TUBE_DUMP="$d" S32_TUBE_DUMP_FULL=1 \
          timeout 120 "$e" "$REEL" >/dev/null 2>&1); then
        echo "FAIL: $name did not run the reel"
        FAIL=1
        continue
    fi
    n=$(ls "$d"/*.hash 2>/dev/null | wc -l | tr -d ' ')
    if [ "$n" != "14" ]; then
        echo "FAIL: $name journaled $n frames (want 14)"
        FAIL=1
        continue
    fi
    if [ -z "$FIRST" ]; then
        FIRST="$d"
        echo "OK:   $name (reference run)"
    elif diff -q <(cat "$FIRST"/*.hash) <(cat "$d"/*.hash) >/dev/null; then
        echo "OK:   $name frames identical to reference"
    else
        echo "FAIL: $name frame hashes diverge"
        FAIL=1
    fi
done

if [ -n "$FIRST" ]; then
    if [ -f "$GOLDEN/000000.hash" ]; then
        if diff -q <(cat "$GOLDEN"/*.hash) <(cat "$FIRST"/*.hash) >/dev/null; then
            echo "OK:   golden hashes match"
        else
            echo "FAIL: golden hashes diverge (spec change? regenerate deliberately)"
            FAIL=1
        fi
    else
        echo "NOTE: no golden hashes checked in — seeding from this run"
        cp "$FIRST"/*.hash "$GOLDEN/"
    fi
    if python3 "$SCRIPT_DIR/check-pixels.py" "$FIRST"; then
        :
    else
        FAIL=1
    fi
fi

if [ "$FAIL" = "0" ]; then
    echo "ppu-reel: PASS"
else
    echo "ppu-reel: FAIL"
fi
exit $FAIL
