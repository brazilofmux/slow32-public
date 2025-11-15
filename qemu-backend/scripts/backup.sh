#!/bin/bash
# Backup SLOW-32 QEMU TCG backend from ~/qemu to this repository
# This is a one-way sync to preserve work done in ~/qemu

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
BACKEND_DIR="$(dirname "$SCRIPT_DIR")"

echo "Backing up SLOW-32 QEMU backend from ~/qemu..."

# Backup the target/slow32 directory
rsync -av --delete \
    ~/qemu/target/slow32/ \
    "$BACKEND_DIR/target/slow32/"

# Backup the hw/slow32 directory
rsync -av --delete \
    ~/qemu/hw/slow32/ \
    "$BACKEND_DIR/hw/slow32/"

# Backup config files
mkdir -p "$BACKEND_DIR/configs/targets"
mkdir -p "$BACKEND_DIR/configs/devices/slow32-softmmu"
rsync -av \
    ~/qemu/configs/targets/slow32-softmmu.mak \
    "$BACKEND_DIR/configs/targets/"
rsync -av \
    ~/qemu/configs/devices/slow32-softmmu/ \
    "$BACKEND_DIR/configs/devices/slow32-softmmu/"

# Backup documentation
rsync -av --delete \
    ~/qemu/docs/slow32-tcg/ \
    "$BACKEND_DIR/docs/"

# Backup AGENTS.md and CLAUDE.md
rsync -av \
    ~/qemu/AGENTS.md \
    ~/qemu/CLAUDE.md \
    "$BACKEND_DIR/"

# Backup scripts/slow32 directory (if it exists)
if [ -d ~/qemu/scripts/slow32 ]; then
    rsync -av --delete \
        ~/qemu/scripts/slow32/ \
        "$BACKEND_DIR/scripts-qemu/slow32/"
fi

echo "Backup complete!"
echo ""
echo "Don't forget to:"
echo "1. Review changes with: cd ~/slow-32/qemu-backend && git diff"
echo "2. Commit changes with: cd ~/slow-32/qemu-backend && git add -A && git commit -m 'Backup QEMU backend changes'"
