#!/bin/bash
# Apply SLOW32 patches and source to a fresh Free Pascal checkout
# Usage: ./apply-patches.sh <path-to-fpc-source>

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
BACKEND_DIR="$(dirname "$SCRIPT_DIR")"
PATCHES_DIR="$BACKEND_DIR/patches"

if [ -z "$1" ]; then
    echo "Usage: $0 <path-to-fpc-source>"
    echo "Example: $0 ~/fpc"
    exit 1
fi

FPC_DIR="$1"

if [ ! -f "$FPC_DIR/compiler/pp.pas" ]; then
    echo "Error: $FPC_DIR does not look like an FPC source tree"
    exit 1
fi

echo "Applying SLOW32 backend to $FPC_DIR..."
echo ""

cd "$FPC_DIR"

# Apply integration patches in order
echo "Applying integration patches..."
for patch in "$PATCHES_DIR"/*.patch; do
    if [ -f "$patch" ] && [ -s "$patch" ]; then
        echo "  Applying $(basename "$patch")..."
        git apply "$patch" 2>/dev/null || patch -p1 < "$patch"
    fi
done

# Copy backend source directories
echo ""
echo "Copying SLOW32 backend sources..."

echo "  compiler/slow32/"
rsync -av "$BACKEND_DIR/compiler/slow32/" "$FPC_DIR/compiler/slow32/"

echo "  rtl/slow32/"
rsync -av "$BACKEND_DIR/rtl/slow32/" "$FPC_DIR/rtl/slow32/"

echo "  rtl/embedded/slow32/"
rsync -av "$BACKEND_DIR/rtl/embedded/slow32/" "$FPC_DIR/rtl/embedded/slow32/"

echo ""
echo "SLOW32 backend applied successfully!"
echo ""
echo "Build the cross-compiler with:"
echo "  cd $FPC_DIR/compiler"
echo "  make CPU_TARGET=slow32 OS_TARGET=embedded CROSSOPT='-O1' all"
