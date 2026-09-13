#!/bin/bash
# Assemble and link the DTC kernel, then run it with the prelude and stdin.
# Tree paths by default; S32_AS / S32_LD / S32_RT / EMU point elsewhere (the
# slow32:forth image builds under /opt/slow32), and FORTH_RUN=0 stops after
# the link.
set -e
cd "$(dirname "$0")"
ASM="${S32_AS:-../tools/assembler/slow32asm}"
LD="${S32_LD:-../tools/linker/s32-ld}"
RT="${S32_RT:-../runtime}"
EMU="${EMU:-../tools/emulator/slow32-fast}"

# Compile assembly
echo "Assembling..."
$ASM kernel.s kernel.s32o

# Link with MMIO libc
echo "Linking..."
$LD --mmio 64K --heap-size 8M --stack-size 256K --pack-sections \
    -o kernel.s32x "$RT/crt0.s32o" kernel.s32o "$RT/libc_mmio.s32a" "$RT/libs32.s32a"

[ "${FORTH_RUN:-1}" = 0 ] && exit 0

# Run (pipe prelude then interactive stdin)
echo "Running..."
cat prelude.fth - | $EMU kernel.s32x
