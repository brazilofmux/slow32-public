#!/bin/bash
# Emergency restore script - copies backend FROM this repo TO ~/llvm-project
# USE WITH CAUTION - This will overwrite work in ~/llvm-project!

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
BACKEND_DIR="$(dirname "$SCRIPT_DIR")"

echo "WARNING: This will OVERWRITE the SLOW32 backend in ~/llvm-project!"
echo "This should only be used for disaster recovery."
read -p "Are you sure? (yes/no): " -r
if [[ ! $REPLY == "yes" ]]; then
    echo "Aborted."
    exit 1
fi

echo "Restoring SLOW32 backend to ~/llvm-project..."

# Restore the SLOW32 target directory
rsync -av --delete \
    "$BACKEND_DIR/SLOW32/" \
    ~/llvm-project/llvm/lib/Target/SLOW32/

# Restore the Clang target files
rsync -av \
    "$BACKEND_DIR/clang/SLOW32."* \
    ~/llvm-project/clang/lib/Basic/Targets/

# Restore the LLVM test files
if [ -d "$BACKEND_DIR/test" ]; then
    echo "Restoring SLOW32 test files..."
    mkdir -p ~/llvm-project/llvm/test/CodeGen/SLOW32
    rsync -av --delete \
        "$BACKEND_DIR/test/" \
        ~/llvm-project/llvm/test/CodeGen/SLOW32/
fi

echo "Restore complete!"
echo "Remember to rebuild LLVM after restoring."