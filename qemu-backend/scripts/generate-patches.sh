#!/bin/bash
# Generate patches for SLOW-32 integration into QEMU
# This script generates patches by comparing against upstream QEMU

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PATCHES_DIR="$(dirname "$SCRIPT_DIR")/patches"
QEMU_DIR="/ztank/secret/sdennis/qemu"

echo "Generating SLOW-32 integration patches against upstream QEMU..."
echo "=============================================================="

# Ensure patches directory exists
mkdir -p "$PATCHES_DIR"

# Change to QEMU directory
cd "$QEMU_DIR"

# Get the base commit (before our SLOW-32 changes)
BASE_COMMIT="9febfa94b6"  # Last upstream commit before our work
CURRENT_COMMIT="HEAD"

echo "Using commit $BASE_COMMIT as base for patch generation..."
echo ""

# 1. Target Kconfig and meson.build
echo "1. Generating target build system patch..."
git diff $BASE_COMMIT $CURRENT_COMMIT -- target/Kconfig target/meson.build > "$PATCHES_DIR/01-target-build.patch" 2>/dev/null || true

if [ -s "$PATCHES_DIR/01-target-build.patch" ]; then
    lines=$(wc -l < "$PATCHES_DIR/01-target-build.patch")
    echo "   Created: 01-target-build.patch ($lines lines)"
else
    echo "   No changes in target build files"
    echo "" > "$PATCHES_DIR/01-target-build.patch"
fi

# 2. Hardware Kconfig and meson.build
echo "2. Generating hardware build system patch..."
git diff $BASE_COMMIT $CURRENT_COMMIT -- hw/Kconfig hw/meson.build > "$PATCHES_DIR/02-hw-build.patch" 2>/dev/null || true

if [ -s "$PATCHES_DIR/02-hw-build.patch" ]; then
    lines=$(wc -l < "$PATCHES_DIR/02-hw-build.patch")
    echo "   Created: 02-hw-build.patch ($lines lines)"
else
    echo "   No changes in hw build files"
    echo "" > "$PATCHES_DIR/02-hw-build.patch"
fi

# 3. Architecture support (arch_init.h)
echo "3. Generating architecture support patch..."
git diff $BASE_COMMIT $CURRENT_COMMIT -- include/system/arch_init.h > "$PATCHES_DIR/03-arch-support.patch" 2>/dev/null || true

if [ -s "$PATCHES_DIR/03-arch-support.patch" ]; then
    lines=$(wc -l < "$PATCHES_DIR/03-arch-support.patch")
    echo "   Created: 03-arch-support.patch ($lines lines)"
else
    echo "   No changes in arch_init.h"
    echo "" > "$PATCHES_DIR/03-arch-support.patch"
fi

# 4. README updates
echo "4. Generating README patch..."
git diff $BASE_COMMIT $CURRENT_COMMIT -- README.rst > "$PATCHES_DIR/04-readme.patch" 2>/dev/null || true

if [ -s "$PATCHES_DIR/04-readme.patch" ]; then
    lines=$(wc -l < "$PATCHES_DIR/04-readme.patch")
    echo "   Created: 04-readme.patch ($lines lines)"
else
    echo "   No changes in README"
    echo "" > "$PATCHES_DIR/04-readme.patch"
fi

# 5. .gitignore
echo "5. Generating gitignore patch..."
git diff $BASE_COMMIT $CURRENT_COMMIT -- .gitignore > "$PATCHES_DIR/05-gitignore.patch" 2>/dev/null || true

if [ -s "$PATCHES_DIR/05-gitignore.patch" ]; then
    lines=$(wc -l < "$PATCHES_DIR/05-gitignore.patch")
    echo "   Created: 05-gitignore.patch ($lines lines)"
else
    echo "   No changes in .gitignore"
    echo "" > "$PATCHES_DIR/05-gitignore.patch"
fi

# Summary
echo ""
echo "Patch generation complete!"
echo "=========================="
echo "Generated patches in: $PATCHES_DIR"
echo ""
ls -lh "$PATCHES_DIR"/*.patch 2>/dev/null || echo "No patches generated"

# Show which files are modified by the patches
echo ""
echo "Files modified by patches:"
echo "--------------------------"
for patch in "$PATCHES_DIR"/*.patch; do
    if [ -s "$patch" ]; then
        echo "$(basename "$patch"):"
        grep "^+++" "$patch" 2>/dev/null | sed 's/^+++ b\//  /' || true
    fi
done

echo ""
echo "New files added (not in patches, backed up separately):"
echo "--------------------------------------------------------"
echo "  target/slow32/"
echo "  hw/slow32/"
echo "  configs/targets/slow32-softmmu.mak"
echo "  configs/devices/slow32-softmmu/"
echo "  docs/slow32-tcg/"
echo "  AGENTS.md"
echo "  CLAUDE.md"
echo ""
echo "To apply patches to a fresh QEMU clone:"
echo "1. Clone QEMU: git clone https://gitlab.com/qemu-project/qemu.git /tmp/test-qemu"
echo "2. Apply patches: cd /tmp/test-qemu && for p in $PATCHES_DIR/*.patch; do patch -p1 < \$p; done"
echo "3. Copy new files: rsync -av ~/slow-32/qemu-backend/target/slow32 /tmp/test-qemu/target/"
echo "   (repeat for hw/slow32, configs/, docs/, etc.)"
echo ""
echo "Note: Run backup.sh first to ensure all files are up to date"
