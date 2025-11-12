#!/bin/bash
# Backup SLOW32 backend from ~/llvm-project to this repository
# This is a one-way sync to preserve work done in ~/llvm-project

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
BACKEND_DIR="$(dirname "$SCRIPT_DIR")"

echo "Backing up SLOW32 backend from ~/llvm-project..."

# Backup the SLOW32 target directory
rsync -av --delete \
    ~/llvm-project/llvm/lib/Target/SLOW32/ \
    "$BACKEND_DIR/SLOW32/"

# Backup the Clang target files
rsync -av \
    ~/llvm-project/clang/lib/Basic/Targets/SLOW32.* \
    "$BACKEND_DIR/clang/"

# Backup the LLVM test files
echo "Backing up SLOW32 test files..."
mkdir -p "$BACKEND_DIR/test"
rsync -av --delete \
    ~/llvm-project/llvm/test/CodeGen/SLOW32/ \
    "$BACKEND_DIR/test/"

echo "Backup complete!"
echo ""
echo "Don't forget to:"
echo "1. Review changes with: git diff"
echo "2. Commit changes with: git add -A && git commit -m 'Backup LLVM backend changes'"