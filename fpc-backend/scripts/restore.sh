#!/bin/bash
# Emergency restore script - copies backend FROM this repo TO ~/fpc
# USE WITH CAUTION - This will overwrite work in ~/fpc!

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
BACKEND_DIR="$(dirname "$SCRIPT_DIR")"
FPC_DIR="$HOME/fpc"

echo "WARNING: This will OVERWRITE the SLOW32 backend in ~/fpc!"
echo "This should only be used for disaster recovery."
read -p "Are you sure? (yes/no): " -r
if [[ ! $REPLY == "yes" ]]; then
    echo "Aborted."
    exit 1
fi

echo "Restoring SLOW32 backend to ~/fpc..."

# Restore compiler backend
rsync -av --delete \
    "$BACKEND_DIR/compiler/slow32/" \
    "$FPC_DIR/compiler/slow32/"

# Restore RTL slow32
rsync -av --delete \
    "$BACKEND_DIR/rtl/slow32/" \
    "$FPC_DIR/rtl/slow32/"

# Restore embedded RTL startup
rsync -av --delete \
    "$BACKEND_DIR/rtl/embedded/slow32/" \
    "$FPC_DIR/rtl/embedded/slow32/"

# Restore integration patches to existing files
echo ""
echo "Applying integration patches..."
cd "$FPC_DIR"
for patch in "$BACKEND_DIR/patches"/*.patch; do
    if [ -f "$patch" ] && [ -s "$patch" ]; then
        echo "  Applying $(basename "$patch")..."
        git apply "$patch" 2>/dev/null || patch -p1 < "$patch" || echo "  (may already be applied)"
    fi
done

echo ""
echo "Restore complete!"
echo "Remember to rebuild the compiler after restoring."
