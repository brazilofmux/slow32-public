#!/bin/bash
# Leg-3 milestone-2 gate: the tube demos and the ppu reel, built by
# stage08 cc, must reproduce the clang builds' frames exactly.
#   - ppu-reel: the 14 frozen golden hashes + the spec-math pixel oracle.
#   - vecscope/fire/sprites: frame-for-frame identical to a clang build
#     over each run's common prefix (instruction-capped headless runs;
#     S32_TUBE_DUMP counts as an attached viewer).
# Frame hashes are compiler-independent truths — this is the cheapest
# cross-compiler conformance test the platform owns.
set -u

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
CC08="$ROOT/selfhost/stage08/cc.s32x"
DBT="$ROOT/tools/dbt/slow32-dbt"
EMU="$ROOT/tools/emulator/slow32-fast"
AS="$ROOT/tools/assembler/slow32asm"
LD="$ROOT/tools/linker/s32-ld"

WORK="$(mktemp -d /tmp/s08-gfx.XXXXXX)"
trap 'rm -rf "$WORK"' EXIT
fail=0

cc08() { # cc08 <src> <out.s32x> [extra -I]
    local src="$1" out="$2"; shift 2
    "$DBT" "$CC08" -I"$ROOT/runtime/include" "$@" "$src" "$WORK/t.s" >/dev/null 2>&1 || return 1
    "$AS" "$WORK/t.s" "$WORK/t.s32o" >/dev/null 2>&1 || return 1
    "$LD" --mmio 64K -o "$out" "$ROOT/runtime/crt0.s32o" "$WORK/t.s32o" \
        "$ROOT/runtime/libc_mmio.s32a" "$ROOT/runtime/libs32.s32a" >/dev/null 2>&1
}

# --- ppu-reel against the frozen goldens ---
if cc08 "$ROOT/ppu-reel/src/reel.c" "$WORK/reel.s32x"; then
    mkdir -p "$WORK/reelf"
    (cd "$WORK" && S32_TUBE_DUMP="$WORK/reelf" S32_TUBE_DUMP_FULL=1 \
        timeout 120 "$EMU" "$WORK/reel.s32x" >/dev/null 2>&1)
    if diff <(cat "$WORK/reelf"/*.hash) <(cat "$ROOT/ppu-reel/tests/golden"/*.hash) >/dev/null 2>&1; then
        echo "  OK  reel: golden hashes match"
    else
        echo "  FAIL reel: golden hashes differ"; fail=1
    fi
    if python3 "$ROOT/ppu-reel/tests/check-pixels.py" "$WORK/reelf" 2>&1 | grep -q "^78/78"; then
        echo "  OK  reel: 78/78 pixel assertions"
    else
        echo "  FAIL reel: pixel assertions"; fail=1
    fi
else
    echo "  FAIL reel: build"; fail=1
fi

# --- the three tube demos, stage08 vs clang, common-prefix identical ---
CAP=400000000
for name in vecscope fire sprites; do
    ref="$ROOT/examples/$name.s32x"
    if [ ! -f "$ref" ]; then echo "  SKIP $name: no clang build"; continue; fi
    if ! cc08 "$ROOT/examples/$name.c" "$WORK/$name.s32x"; then
        echo "  FAIL $name: build"; fail=1; continue
    fi
    for side in ref s08; do
        d="$WORK/f_${name}_$side"; mkdir -p "$d"
        bin="$ref"; [ "$side" = s08 ] && bin="$WORK/$name.s32x"
        (cd "$WORK" && S32_TUBE_DUMP="$d" timeout 60 "$EMU" -c $CAP "$bin" >/dev/null 2>&1)
    done
    n=$(ls "$WORK/f_${name}_ref" | grep -c hash)
    m=$(ls "$WORK/f_${name}_s08" | grep -c hash)
    k=$((n<m?n:m))
    if [ "$k" -lt 10 ]; then
        echo "  FAIL $name: too few frames (ref=$n s08=$m)"; fail=1; continue
    fi
    ok=1; i=0
    while [ $i -lt $k ]; do
        f=$(printf "%06d.hash" $i)
        cmp -s "$WORK/f_${name}_ref/$f" "$WORK/f_${name}_s08/$f" || { ok=0; break; }
        i=$((i+1))
    done
    if [ $ok -eq 1 ]; then
        echo "  OK  $name: $k common frames identical"
    else
        echo "  FAIL $name: diverges at frame $i"; fail=1
    fi
done

if [ $fail -eq 0 ]; then echo "stage08-graphics: PASS"; else echo "stage08-graphics: FAIL"; exit 1; fi
