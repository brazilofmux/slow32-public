#!/bin/bash
# Backup SLOW32 backend from ~/fpc to this repository
# This is a one-way sync to preserve work done in ~/fpc

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
BACKEND_DIR="$(dirname "$SCRIPT_DIR")"
FPC_DIR="$HOME/fpc"

if [ ! -d "$FPC_DIR/compiler/slow32" ]; then
    echo "Error: $FPC_DIR/compiler/slow32 does not exist"
    exit 1
fi

echo "Backing up SLOW32 backend from ~/fpc..."

# Backup the compiler backend (source files only, skip build artifacts)
rsync -av --delete \
    --include='*.pas' --include='*.inc' --include='*.dat' \
    --exclude='bin/' --exclude='units/' \
    --exclude='*.o' --exclude='*.ppu' --exclude='*.or' \
    "$FPC_DIR/compiler/slow32/" \
    "$BACKEND_DIR/compiler/slow32/"

# Backup the RTL slow32 directory
rsync -av --delete \
    "$FPC_DIR/rtl/slow32/" \
    "$BACKEND_DIR/rtl/slow32/"

# Backup the embedded RTL startup
rsync -av --delete \
    "$FPC_DIR/rtl/embedded/slow32/" \
    "$BACKEND_DIR/rtl/embedded/slow32/"

echo ""
echo "Backup complete!"
echo ""
echo "Don't forget to also run generate-patches.sh to update integration patches."
echo ""
echo "Next steps:"
echo "1. ./scripts/generate-patches.sh"
echo "2. Review changes with: git diff"
echo "3. Commit changes"
