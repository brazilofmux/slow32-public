#!/bin/bash
set -e
cd "$(dirname "$0")"

LLVM_BIN="${LLVM_BIN:-$HOME/llvm-project/build/bin}"
CLANG=$LLVM_BIN/clang
LLC=$LLVM_BIN/llc
ASM=../tools/assembler/slow32asm
LD=../tools/linker/s32-ld
TARGET=slow32-unknown-none
CFLAGS="-target $TARGET -S -emit-llvm -nostdinc -fno-builtin \
  -I../runtime/include -Isrc \
  -DCMAP256 -DDOOMGENERIC_RESX=320 -DDOOMGENERIC_RESY=200 \
  -Wno-tautological-compare"
OPT="${OPT:--O1}"

SRCS="dummy am_map doomdef doomstat dstrings d_event d_items d_iwad
d_loop d_main d_mode d_net f_finale f_wipe g_game hu_lib hu_stuff
info i_cdmus i_endoom i_joystick i_scale i_sound i_system i_timer
memio m_argv m_bbox m_cheat m_config m_controls m_fixed m_menu
m_misc m_random p_ceilng p_doors p_enemy p_floor p_inter p_lights
p_map p_maputl p_mobj p_plats p_pspr p_saveg p_setup p_sight p_spec
p_switch p_telept p_tick p_user r_bsp r_data r_draw r_main r_plane
r_segs r_sky r_things sha1 sounds statdump st_lib st_stuff s_sound
tables v_video wi_stuff w_checksum w_file w_main w_wad z_zone
w_file_stdc i_input i_video doomgeneric doomgeneric_slow32"

echo "=== Building SLOW-32 Doom ==="
mkdir -p obj
OBJECTS=""
for base in $SRCS; do
    src="src/$base.c"
    obj="obj/$base.s32o"
    if [ ! -f "$obj" ] || [ "$src" -nt "$obj" ]; then
        echo "  CC $base.c"
        $CLANG $CFLAGS $OPT "$src" -o "obj/$base.ll"
        $LLC -mtriple=$TARGET "obj/$base.ll" -o "obj/$base.s"
        $ASM "obj/$base.s" "$obj" >/dev/null
        rm -f "obj/$base.ll" "obj/$base.s"
    fi
    OBJECTS="$OBJECTS $obj"
done

echo -n "  LD doom.s32x... "
$LD --mmio 1M --stack-size 1M --heap-size 64M -o doom.s32x \
    ../runtime/crt0.s32o \
    $OBJECTS \
    ../runtime/libc_mmio.s32a \
    ../runtime/libs32.s32a
echo "OK"
echo ""
echo "Needs doom1.wad (shareware) in this directory: ./fetch-wad.sh"
echo "Run:  ../tools/emulator/slow32-fast doom.s32x  (or slow32-dbt)"
echo "Then: ../tools/s32-crt-mac/s32-crt-mac"
