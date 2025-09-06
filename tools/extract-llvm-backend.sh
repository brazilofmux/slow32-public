#!/bin/bash
# Script to extract and prepare SLOW32 backend from private LLVM tree
# Run this from ~/llvm-project to extract the backend for public distribution

set -e

# Source and destination
LLVM_PROJECT_DIR="$HOME/llvm-project"
PUBLIC_REPO="$HOME/slow32-public"
OUTPUT_DIR="$PUBLIC_REPO/llvm-backend/SLOW32"

if [ ! -d "$LLVM_PROJECT_DIR/llvm/lib/Target/SLOW32" ]; then
    echo "Error: Cannot find $LLVM_PROJECT_DIR/llvm/lib/Target/SLOW32"
    echo "Make sure you're running from the correct location"
    exit 1
fi

echo "Extracting SLOW32 backend from private LLVM tree..."

# Create output directory
rm -rf "$OUTPUT_DIR"
mkdir -p "$OUTPUT_DIR"

# Copy the SLOW32 backend files
echo "Copying backend files..."
cp -r "$LLVM_PROJECT_DIR/llvm/lib/Target/SLOW32/"* "$OUTPUT_DIR/"

# Clean up build artifacts and private files
echo "Cleaning build artifacts..."
find "$OUTPUT_DIR" -name "*.o" -delete
find "$OUTPUT_DIR" -name "*.d" -delete
find "$OUTPUT_DIR" -name "*.inc" -delete
find "$OUTPUT_DIR" -name "*.cmake" -delete
find "$OUTPUT_DIR" -name "CMakeFiles" -type d -exec rm -rf {} + 2>/dev/null || true
find "$OUTPUT_DIR" -name ".*.swp" -delete
find "$OUTPUT_DIR" -name ".*.swo" -delete
find "$OUTPUT_DIR" -name "*~" -delete

# Remove any debug/development files that shouldn't be public
rm -f "$OUTPUT_DIR/TODO.md" 2>/dev/null || true
rm -f "$OUTPUT_DIR/NOTES.md" 2>/dev/null || true
rm -f "$OUTPUT_DIR/DEBUG.txt" 2>/dev/null || true

# Create a README for the SLOW32 directory
cat > "$OUTPUT_DIR/README.md" << 'EOF'
# SLOW32 LLVM Backend

This directory contains the complete SLOW32 backend for LLVM.

## Installation

Copy this entire directory to `llvm/lib/Target/SLOW32/` in your LLVM tree, then follow the integration instructions in the parent directory.

## Files

- `*.td` - TableGen target descriptions
- `*.cpp/h` - C++ implementation
- `CMakeLists.txt` - Build configuration
- `MCTargetDesc/` - Machine code descriptions
- `TargetInfo/` - Target information

## Version

Compatible with LLVM 19.0+

## License

This backend is distributed under the same license as LLVM (Apache 2.0 with LLVM exceptions).
EOF

# Count files
FILE_COUNT=$(find "$OUTPUT_DIR" -type f | wc -l)

echo ""
echo "Extraction complete!"
echo "Backend extracted to: $OUTPUT_DIR"
echo "Total files: $FILE_COUNT"
echo ""
echo "Files included:"
ls -la "$OUTPUT_DIR/" | head -20
echo ""
echo "To verify the extraction:"
echo "  cd $OUTPUT_DIR"
echo "  ls -la"
echo ""
echo "Remember to:"
echo "1. Review the files for any private information"
echo "2. Test that the backend builds with a fresh LLVM checkout"
echo "3. Update version information if needed"