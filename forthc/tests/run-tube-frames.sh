#!/bin/bash
# forthc M5 gate: the compiled ship's tube frames must hash identical
# to the DTC kernel's, for the same deterministic scene script.
set -u
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
FC_DIR="$(dirname "$SCRIPT_DIR")"
ROOT="$(dirname "$FC_DIR")"
EMU="$ROOT/tools/emulator/slow32-fast"

WORK="$(mktemp -d /tmp/forthc-tube.XXXXXX)"
trap 'rm -rf "$WORK"' EXIT

# The scene script, shared logic on both sides: two headings, a
# thrust+step, then the three-ship pinwheel via DEFER retargeting.
cat > "$WORK/scenes.fc.fth" <<'EOF'
: PINWHEEL 3 0 DO HDG @ 21 I * + 63 AND SHAPE LOOP ;
: MAIN
    TUBE-ON 0= IF EXIT THEN
    16 HDG ! FRAME
    24 HDG ! FRAME
    THRUST STEP FRAME
    ' PINWHEEL IS SHIP
    FRAME ;
EOF
cat "$FC_DIR/demo/ship-words.fth" "$WORK/scenes.fc.fth" > "$WORK/ship-scenes.fth"

if ! bash "$FC_DIR/compile.sh" --hosted "$WORK/ship-scenes.fth" "$WORK/ship-scenes.s32x" >/dev/null 2>&1; then
    echo "FAIL: compile"; exit 1
fi

mkdir -p "$WORK/dc" "$WORK/dd"
(cd "$WORK" && S32_TUBE_DUMP="$WORK/dc" timeout 60 "$EMU" ship-scenes.s32x >/dev/null 2>&1)

printf 'TUBE-ON DROP\n16 HDG ! FRAME\n24 HDG ! FRAME\nTHRUST STEP FRAME\n:NONAME 3 0 DO HDG @ 21 I * + 63 AND SHAPE LOOP ; IS SHIP\nFRAME\nBYE\n' \
    | cat "$ROOT/forth/prelude.fth" "$ROOT/forth/tube.fth" "$ROOT/forth/ship.fth" - \
    | (cd "$WORK" && S32_TUBE_DUMP="$WORK/dd" timeout 60 "$EMU" "$ROOT/forth/kernel.s32x" >/dev/null 2>&1)

NC=$(ls "$WORK/dc" 2>/dev/null | wc -l | tr -d ' ')
ND=$(ls "$WORK/dd" 2>/dev/null | wc -l | tr -d ' ')
if [ "$NC" != "4" ] || [ "$ND" != "4" ]; then
    echo "FAIL: frame counts compiled=$NC dtc=$ND (want 4)"; exit 1
fi
if diff <(cat "$WORK/dc"/*.hash) <(cat "$WORK/dd"/*.hash) >/dev/null; then
    echo "OK:   4 frames, compiled == DTC, hash-identical"
    echo "forthc-tube: PASS"
else
    echo "FAIL: frame hashes diverge"
    paste <(cat "$WORK/dc"/*.hash) <(cat "$WORK/dd"/*.hash)
    exit 1
fi
