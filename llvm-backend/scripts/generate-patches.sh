#!/bin/bash
# Generate patches for SLOW32 integration into LLVM
# This script generates patches by comparing against upstream LLVM

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PATCHES_DIR="$(dirname "$SCRIPT_DIR")/patches"
LLVM_DIR="/ztank/secret/sdennis/llvm-project"

echo "Generating SLOW32 integration patches against upstream LLVM..."
echo "=============================================================="

# Ensure patches directory exists
mkdir -p "$PATCHES_DIR"

# Change to LLVM directory
cd "$LLVM_DIR"

# Since this is a shallow clone, we'll compare against origin/HEAD
# which represents the upstream state when we cloned
echo "Using origin/HEAD as base for patch generation..."
echo ""

# 1. Triple.h and Triple.cpp (LLVM core architecture support)
echo "1. Generating LLVM Triple patch..."
git diff origin/HEAD HEAD -- llvm/include/llvm/TargetParser/Triple.h llvm/lib/TargetParser/Triple.cpp > "$PATCHES_DIR/01-llvm-triple.patch" 2>/dev/null || true

if [ -s "$PATCHES_DIR/01-llvm-triple.patch" ]; then
    lines=$(wc -l < "$PATCHES_DIR/01-llvm-triple.patch")
    echo "   Created: 01-llvm-triple.patch ($lines lines)"
else
    echo "   No changes in Triple files"
    echo "" > "$PATCHES_DIR/01-llvm-triple.patch"
fi

# 2. LLVM CMake files
echo "2. Generating LLVM CMake patch..."
git diff origin/HEAD HEAD -- llvm/CMakeLists.txt llvm/lib/Target/CMakeLists.txt > "$PATCHES_DIR/02-llvm-cmake.patch" 2>/dev/null || true

if [ -s "$PATCHES_DIR/02-llvm-cmake.patch" ]; then
    lines=$(wc -l < "$PATCHES_DIR/02-llvm-cmake.patch")
    echo "   Created: 02-llvm-cmake.patch ($lines lines)"
else
    echo "   No changes in LLVM CMake files"
    echo "" > "$PATCHES_DIR/02-llvm-cmake.patch"
fi

# 3. Clang Targets.cpp
echo "3. Generating Clang targets patch..."
git diff origin/HEAD HEAD -- clang/lib/Basic/Targets.cpp > "$PATCHES_DIR/03-clang-targets.patch" 2>/dev/null || true

if [ -s "$PATCHES_DIR/03-clang-targets.patch" ]; then
    lines=$(wc -l < "$PATCHES_DIR/03-clang-targets.patch")
    echo "   Created: 03-clang-targets.patch ($lines lines)"
else
    echo "   No changes in Clang Targets.cpp"
    echo "" > "$PATCHES_DIR/03-clang-targets.patch"
fi

# 4. Clang CMake files
echo "4. Generating Clang CMake patch..."
git diff origin/HEAD HEAD -- clang/lib/Basic/CMakeLists.txt clang/lib/Basic/Targets/CMakeLists.txt > "$PATCHES_DIR/04-clang-cmake.patch" 2>/dev/null || true

if [ -s "$PATCHES_DIR/04-clang-cmake.patch" ]; then
    lines=$(wc -l < "$PATCHES_DIR/04-clang-cmake.patch")
    echo "   Created: 04-clang-cmake.patch ($lines lines)"
else
    echo "   No changes in Clang CMake files"
    echo "" > "$PATCHES_DIR/04-clang-cmake.patch"
fi

# Summary
echo ""
echo "Patch generation complete!"
echo "=========================="
echo "Generated patches in: $PATCHES_DIR"
echo ""
ls -lh "$PATCHES_DIR"/*.patch

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
echo "Testing patch compatibility with Docker build..."
echo "================================================"
echo ""
echo "Docker build uses 'main' branch (LLVM 22 development)"
echo "Patches are generated against the same branch for compatibility"
echo ""
echo "To manually test patches:"
echo "1. Clone fresh LLVM: git clone --depth 1 --branch main https://github.com/llvm/llvm-project.git /tmp/test-llvm"
echo "2. Apply patches: cd /tmp/test-llvm && for p in $PATCHES_DIR/*.patch; do patch -p1 < \$p; done"
echo "3. If patches fail, you may need to regenerate against release/19.x instead of main"
echo ""
echo "Note: The backup.sh script should also be run to ensure SLOW32 backend files are up to date"