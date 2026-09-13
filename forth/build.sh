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

# Run: the prelude, then interactive stdin.  Not a plain pipe: `cat - | emu`
# leaves cat reading the terminal after the kernel has exited on BYE, and the
# shell waits for every member of a pipeline, so the prompt never comes back
# until the next typed line hits the closed pipe.  Feed through a FIFO from a
# background reader and kill that reader when the kernel exits.
echo "Running..."
fifo="$(mktemp -u "${TMPDIR:-/tmp}/forth.XXXXXX")"
mkfifo "$fifo"
# `exec cat`: the subshell BECOMES the terminal reader, so killing $feeder
# kills the reader itself (killing a subshell leaves its child cat orphaned,
# still holding the terminal -- that was the original symptom in another
# guise).  It may already be gone (EPIPE on its next write); either way it
# must not survive us, and neither must the FIFO.
# <&3: a background job in a non-interactive shell gets stdin from
# /dev/null unless explicitly redirected -- without this the reader sees
# EOF at once and the kernel halts right after the prelude.
exec 3<&0
( cat prelude.fth; exec cat ) <&3 > "$fifo" &
feeder=$!
trap 'kill "$feeder" 2>/dev/null || true; rm -f "$fifo"' EXIT
$EMU kernel.s32x < "$fifo"
