#!/bin/bash
# Sync approved changes to public SLOW32 repository

PUBLIC_REPO="/ztank/secret/sdennis/slow32-public"
PRIVATE_REPO="/home/sdennis/slow-32"

echo "Syncing SLOW32 changes to public repository..."

# Files to sync with mapping (source:destination)
declare -A FILES_TO_SYNC=(
    ["assembler/slow32asm.c"]="tools/assembler/slow32asm.c"
    ["emulator/slow32.c"]="tools/emulator/slow32.c"
    ["emulator/slow32.h"]="tools/emulator/slow32.h"
    ["emulator/slow32-fast.c"]="tools/emulator/slow32-fast.c"
    ["emulator/memory_manager.h"]="tools/emulator/memory_manager.h"
    ["emulator/mmio_ring.c"]="tools/emulator/mmio_ring.c"
    ["emulator/mmio_ring.h"]="tools/emulator/mmio_ring.h"
    ["linker/s32-ld.c"]="tools/linker/s32-ld.c"
    ["runtime/builtins.c"]="runtime/builtins.c"
    ["runtime/Makefile"]="runtime/Makefile"
    ["runtime/debug_char.s"]="runtime/debug_char.s"
    ["Makefile"]="Makefile"
    ["tools/compile-c.sh"]="tools/compile-c.sh"
    ["CLAUDE.md"]="CLAUDE.md"
    ["tests/test_64bit_div_args.c"]="tests/test_64bit_div_args.c"
)

# Ensure public repo exists
if [ ! -d "$PUBLIC_REPO" ]; then
    echo "Error: Public repository not found at $PUBLIC_REPO"
    exit 1
fi

# Create necessary directories in public repo
mkdir -p "$PUBLIC_REPO/runtime"
mkdir -p "$PUBLIC_REPO/tools/assembler"  
mkdir -p "$PUBLIC_REPO/tools/emulator"
mkdir -p "$PUBLIC_REPO/tests"
mkdir -p "$PUBLIC_REPO/llvm-backend/SLOW32"

# Copy individual files
for src in "${!FILES_TO_SYNC[@]}"; do
    dest="${FILES_TO_SYNC[$src]}"
    if [ -f "$PRIVATE_REPO/$src" ]; then
        cp "$PRIVATE_REPO/$src" "$PUBLIC_REPO/$dest"
        echo "  ✓ Copied $src -> $dest"
    else
        echo "  ⚠ Warning: $src not found in private repo"
    fi
done

# Copy entire LLVM backend directory (overwriting the individual file copies above)
echo ""
echo "Syncing LLVM backend..."
if [ -d "$PRIVATE_REPO/llvm-backend/SLOW32" ]; then
    cp -r "$PRIVATE_REPO/llvm-backend/SLOW32/"* "$PUBLIC_REPO/llvm-backend/SLOW32/"
    echo "  ✓ Copied entire llvm-backend/SLOW32 directory"
else
    echo "  ⚠ Warning: llvm-backend/SLOW32 not found"
fi

echo ""
echo "Files synced. Please review changes in public repo before committing:"
echo "  cd $PUBLIC_REPO"
echo "  git status"
echo "  git diff"
echo ""
echo "To commit to public repo:"
echo "  git add -A"
echo "  git commit -m 'Add MMIO support and memory management improvements'"