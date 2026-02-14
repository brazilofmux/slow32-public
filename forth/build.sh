#!/bin/bash
set -e

# Paths
ASM=../tools/assembler/slow32asm
LD=../tools/linker/s32-ld
EMU=../tools/emulator/slow32-fast

# Compile assembly
echo "Assembling..."
$ASM kernel.s kernel.s32o

# Link with MMIO libc
echo "Linking..."
$LD --mmio 64K --data-size 2M -o kernel.s32x ../runtime/crt0.s32o kernel.s32o ../runtime/libc_mmio.s32a ../runtime/libs32.s32a

# Run (pipe prelude then interactive stdin)
echo "Running..."
cat prelude.fth - | $EMU kernel.s32x
