#!/bin/bash
# Leg-3 milestone-4 gate: DOOM built entirely by stage08 cc must play
# -timedemo demo3 to completion with all 2173 frames hash-identical to
# the clang build's goldens (final frame 75fd024951e9cf7f), on both
# slow32-fast and slow32-dbt.
set -u

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
CC08="$ROOT/selfhost/stage08/cc.s32x"
DBT="$ROOT/tools/dbt/slow32-dbt"
AS="$ROOT/tools/assembler/slow32asm"
LD="$ROOT/tools/linker/s32-ld"

if [ ! -f "$ROOT/doom/doom1.wad" ]; then
    echo "SKIP: doom1.wad not present (run doom/fetch-wad.sh)"
    exit 0
fi

WORK="$(mktemp -d /tmp/s08-doom.XXXXXX)"
trap 'rm -rf "$WORK"' EXIT
FRAMES=2173
HASH=75fd024951e9cf7f
DFLAGS="-DCMAP256 -DDOOMGENERIC_RESX=320 -DDOOMGENERIC_RESY=200"
SRCS="dummy am_map doomdef doomstat dstrings d_event d_items d_iwad d_loop d_main d_mode d_net f_finale f_wipe g_game hu_lib hu_stuff info i_cdmus i_endoom i_joystick i_scale i_sound i_system i_timer memio m_argv m_bbox m_cheat m_config m_controls m_fixed m_menu m_misc m_random p_ceilng p_doors p_enemy p_floor p_inter p_lights p_map p_maputl p_mobj p_plats p_pspr p_saveg p_setup p_sight p_spec p_switch p_telept p_tick p_user r_bsp r_data r_draw r_main r_plane r_segs r_sky r_things sha1 sounds statdump st_lib st_stuff s_sound tables v_video wi_stuff w_checksum w_file w_main w_wad z_zone w_file_stdc i_input i_video doomgeneric doomgeneric_slow32"

echo "Building doom with stage08 cc (84 TUs under the DBT)..."
OBJECTS=""
fail=0
for b in $SRCS; do
    "$DBT" "$CC08" $DFLAGS -I"$ROOT/doom/src" -I"$ROOT/runtime/include" \
        "$ROOT/doom/src/$b.c" "$WORK/$b.s" >/dev/null 2>&1
    [ -s "$WORK/$b.s" ] || { echo "  CC FAIL $b"; fail=1; continue; }
    "$AS" "$WORK/$b.s" "$WORK/$b.s32o" >/dev/null 2>&1 || { echo "  ASM FAIL $b"; fail=1; continue; }
    OBJECTS="$OBJECTS $WORK/$b.s32o"
done
[ $fail -ne 0 ] && { echo "stage08-doom: FAIL (build)"; exit 1; }

# __muldi3 lives in stage08's builtins64.s (libs32 lacks it); everything
# else 64-bit/unsigned-div comes from libs32.  fp64 wrappers for doubles.
python3 - "$ROOT/selfhost/stage08/builtins64.s" "$WORK/muldi3.s" <<'PYEOF'
import sys
src = open(sys.argv[1]).read().split("\n")
out = [".text"]; on = False
for ln in src:
    if ln.startswith(".global "):
        on = (ln.split()[1] == "__muldi3")
    if on: out.append(ln)
open(sys.argv[2], "w").write("\n".join(out) + "\n")
PYEOF
"$AS" "$WORK/muldi3.s" "$WORK/muldi3.s32o" >/dev/null
"$AS" "$ROOT/selfhost/stage08/builtins_fp64.s" "$WORK/bfp64.s32o" >/dev/null

"$LD" --mmio 1M --stack-size 1M --heap-size 64M -o "$WORK/doom_s08.s32x" \
    "$ROOT/runtime/crt0.s32o" $OBJECTS "$WORK/muldi3.s32o" "$WORK/bfp64.s32o" \
    "$ROOT/runtime/libc_mmio.s32a" "$ROOT/runtime/libs32.s32a" || { echo "stage08-doom: FAIL (link)"; exit 1; }

for EMU in "$ROOT/tools/emulator/slow32-fast" "$ROOT/tools/dbt/slow32-dbt"; do
    [ -x "$EMU" ] || continue
    name="$(basename "$EMU")"
    rm -rf "$WORK/td"; mkdir -p "$WORK/td" "$WORK/run"
    (cd "$WORK/run" && S32_TUBE_DUMP="$WORK/td" \
        timeout 1800 "$EMU" "$WORK/doom_s08.s32x" -iwad "$ROOT/doom/doom1.wad" -timedemo demo3) \
        >/dev/null 2>&1
    n="$(ls "$WORK/td" 2>/dev/null | wc -l | tr -d ' ')"
    h="$(cat "$WORK/td/002172.hash" 2>/dev/null || echo missing)"
    if [ "$n" = "$FRAMES" ] && [ "$h" = "$HASH" ]; then
        echo "  OK  $name: $FRAMES frames, final $h"
    else
        echo "  FAIL $name: frames=$n hash=$h (want $FRAMES/$HASH)"
        fail=1
    fi
done

if [ $fail -eq 0 ]; then echo "stage08-doom: PASS"; else echo "stage08-doom: FAIL"; exit 1; fi
