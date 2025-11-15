#!/bin/bash
# Apply SLOW-32 patches to a QEMU source tree
# Run this in a fresh QEMU clone after applying patches

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PATCHES_DIR="$(dirname "$SCRIPT_DIR")/patches"
BACKEND_DIR="$(dirname "$SCRIPT_DIR")"

# Default to ~/qemu if no target specified
TARGET_DIR="${1:-$HOME/qemu}"

if [ ! -d "$TARGET_DIR" ]; then
    echo "Error: Target directory $TARGET_DIR does not exist"
    echo "Usage: $0 [target-qemu-directory]"
    exit 1
fi

cd "$TARGET_DIR"

# Check if this looks like a QEMU tree
if [ ! -f "VERSION" ] || [ ! -d "target" ]; then
    echo "Error: $TARGET_DIR doesn't look like a QEMU source tree"
    exit 1
fi

echo "Applying SLOW-32 patches to $TARGET_DIR..."
echo "==========================================="

# Apply patches in order
for patch in "$PATCHES_DIR"/*.patch; do
    if [ -s "$patch" ]; then
        echo "Applying $(basename "$patch")..."
        patch -p1 < "$patch" || {
            echo "Error: Failed to apply $(basename "$patch")"
            echo "You may need to resolve conflicts manually"
            exit 1
        }
    fi
done

echo ""
echo "Copying new files..."
echo "--------------------"

# Copy target/slow32
echo "  target/slow32/"
rsync -av "$BACKEND_DIR/target/slow32/" "$TARGET_DIR/target/slow32/"

# Copy hw/slow32
echo "  hw/slow32/"
rsync -av "$BACKEND_DIR/hw/slow32/" "$TARGET_DIR/hw/slow32/"

# Copy config files
echo "  configs/"
mkdir -p "$TARGET_DIR/configs/targets"
mkdir -p "$TARGET_DIR/configs/devices/slow32-softmmu"
rsync -av "$BACKEND_DIR/configs/targets/slow32-softmmu.mak" "$TARGET_DIR/configs/targets/"
rsync -av "$BACKEND_DIR/configs/devices/slow32-softmmu/" "$TARGET_DIR/configs/devices/slow32-softmmu/"

# Copy documentation
echo "  docs/slow32-tcg/"
rsync -av "$BACKEND_DIR/docs/" "$TARGET_DIR/docs/slow32-tcg/"

# Copy AGENTS.md and CLAUDE.md
echo "  AGENTS.md, CLAUDE.md"
rsync -av "$BACKEND_DIR/AGENTS.md" "$BACKEND_DIR/CLAUDE.md" "$TARGET_DIR/"

# Copy scripts/slow32 directory (if it exists in backup)
if [ -d "$BACKEND_DIR/scripts-qemu/slow32" ]; then
    echo "  scripts/slow32/"
    rsync -av "$BACKEND_DIR/scripts-qemu/slow32/" "$TARGET_DIR/scripts/slow32/"
fi

echo ""
echo "SLOW-32 integration complete!"
echo "=============================="
echo ""
echo "Next steps:"
echo "1. Configure QEMU: ./configure --target-list=slow32-softmmu"
echo "2. Build: make"
echo "3. Test: ./build/qemu-system-slow32 -machine slow32-tcg -kernel test.s32x -nographic"
