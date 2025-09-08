#!/bin/bash
# Apply integration patches to a fresh LLVM checkout
# This helps someone set up SLOW32 in their own LLVM build

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PATCHES_DIR="$(dirname "$SCRIPT_DIR")/patches"

if [ -z "$1" ]; then
    echo "Usage: $0 <path-to-llvm-project>"
    echo "Example: $0 ~/llvm-project"
    exit 1
fi

LLVM_DIR="$1"

if [ ! -d "$LLVM_DIR" ]; then
    echo "Error: $LLVM_DIR does not exist"
    exit 1
fi

echo "Applying integration patches to $LLVM_DIR..."

cd "$LLVM_DIR"

# Apply patches in order
for patch in "$PATCHES_DIR"/*.patch; do
    if [ -f "$patch" ]; then
        echo "Applying $(basename "$patch")..."
        git apply "$patch" || patch -p1 < "$patch"
    fi
done

echo ""
echo "Patches applied successfully!"
echo "Next steps:"
echo "1. Copy the SLOW32 backend: cp -r $BACKEND_DIR/SLOW32 $LLVM_DIR/llvm/lib/Target/"
echo "2. Copy Clang files: cp $BACKEND_DIR/clang/* $LLVM_DIR/clang/lib/Basic/Targets/"
echo "3. Rebuild LLVM with: cmake --build build"