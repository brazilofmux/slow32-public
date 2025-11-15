#!/bin/bash
# Emergency restore script - copies backend FROM this repo TO ~/qemu
# USE WITH CAUTION - This will overwrite work in ~/qemu!

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
BACKEND_DIR="$(dirname "$SCRIPT_DIR")"

echo "WARNING: This will OVERWRITE the SLOW-32 backend in ~/qemu!"
echo "This should only be used for disaster recovery."
read -p "Are you sure? (yes/no): " -r
if [[ ! $REPLY == "yes" ]]; then
    echo "Aborted."
    exit 1
fi

echo "Restoring SLOW-32 QEMU backend to ~/qemu..."

# Restore the target/slow32 directory
rsync -av --delete \
    "$BACKEND_DIR/target/slow32/" \
    ~/qemu/target/slow32/

# Restore the hw/slow32 directory
rsync -av --delete \
    "$BACKEND_DIR/hw/slow32/" \
    ~/qemu/hw/slow32/

# Restore config files
rsync -av \
    "$BACKEND_DIR/configs/targets/slow32-softmmu.mak" \
    ~/qemu/configs/targets/
rsync -av --delete \
    "$BACKEND_DIR/configs/devices/slow32-softmmu/" \
    ~/qemu/configs/devices/slow32-softmmu/

# Restore documentation
rsync -av --delete \
    "$BACKEND_DIR/docs/" \
    ~/qemu/docs/slow32-tcg/

# Restore AGENTS.md and CLAUDE.md
rsync -av \
    "$BACKEND_DIR/AGENTS.md" \
    "$BACKEND_DIR/CLAUDE.md" \
    ~/qemu/

# Restore scripts/slow32 directory (if it exists in backup)
if [ -d "$BACKEND_DIR/scripts-qemu/slow32" ]; then
    rsync -av --delete \
        "$BACKEND_DIR/scripts-qemu/slow32/" \
        ~/qemu/scripts/slow32/
fi

echo "Restore complete!"
echo ""
echo "Remember to rebuild QEMU after restoring:"
echo "  cd ~/qemu/build && ninja"
