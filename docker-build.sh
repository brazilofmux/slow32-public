#!/bin/bash
# Build script for SLOW-32 Docker containers
# Both containers work with a shared /data mount for .c files and .s32x executables

set -e

echo "Building SLOW-32 Docker containers..."
echo "======================================"

# Build toolchain container
echo ""
echo "Building toolchain container..."
docker build -f Dockerfile.toolchain -t slow32:toolchain .

# Build emulator container  
echo ""
echo "Building emulator container..."
docker build -f Dockerfile.emulator -t slow32:emulator .

echo ""
echo "Build complete!"
echo ""
echo "Usage examples (ephemeral containers with /data mount):"
echo "========================================================"
echo ""
echo "1. Compile all C files in ./data directory:"
echo "   docker run --rm -v \$(pwd)/data:/data slow32:toolchain"
echo ""
echo "2. Compile specific C file:"
echo "   docker run --rm -v \$(pwd)/data:/data slow32:toolchain s32cc /data/program.c /data/program.s32x"
echo ""
echo "3. Run all executables in ./data directory:"
echo "   docker run --rm -v \$(pwd)/data:/data slow32:emulator"
echo ""
echo "4. Run specific executable:"
echo "   docker run --rm -v \$(pwd)/data:/data slow32:emulator slow32 /data/program.s32x"
echo ""
echo "5. Run with debugging options:"
echo "   docker run --rm -v \$(pwd)/data:/data slow32:emulator slow32 -t /data/program.s32x"
echo ""
echo "6. Interactive session (for debugging):"
echo "   docker run --rm -it -v \$(pwd)/data:/data slow32:toolchain bash"
echo "   docker run --rm -it -v \$(pwd)/data:/data slow32:emulator bash"
echo ""
echo "Note: Containers are ephemeral (--rm flag) and work with /data mount"