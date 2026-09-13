#!/bin/bash
# Build script for SLOW-32 Docker containers
# Both containers work with a shared /data mount for .c files and .s32x executables

set -e

echo "Building SLOW-32 Docker containers..."
echo "======================================"

# The images are a chain -- emulator -> base -> toolchain -- so the order matters.
echo ""
echo "Building emulator container..."
docker build -f Dockerfile.emulator -t slow32:emulator .

echo ""
echo "Building base container (emulator + as/ld/ar/utilities + runtime)..."
docker build -f Dockerfile.base -t slow32:base .

echo ""
echo "Building toolchain container (base + LLVM + FPC + cc-x64/cc-a64)..."
docker build -f Dockerfile.toolchain -t slow32:toolchain .

echo ""
echo "Building COBOL 85 container (base + s32-cobc + libcob)..."
docker build -f Dockerfile.cobol -t slow32:cobol .

echo ""
echo "Building Fortran 77 container (base + f77 + libf77)..."
docker build -f Dockerfile.fortran -t slow32:fortran .

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
echo "6. Compile and run a COBOL 85 program:"
echo "   docker run --rm -v \$(pwd)/data:/data slow32:cobol s32cob -free prog.cbl -o prog.s32x"
echo "   docker run --rm -v \$(pwd)/data:/data slow32:cobol s32run prog.s32x"
echo ""
echo "7. Compile and run a Fortran 77 program:"
echo "   docker run --rm -v \$(pwd)/data:/data slow32:fortran s32f77 prog.f -o prog.s32x"
echo "   docker run --rm -v \$(pwd)/data:/data slow32:fortran s32run prog.s32x"
echo ""
echo "8. Interactive session (for debugging):"
echo "   docker run --rm -it -v \$(pwd)/data:/data slow32:toolchain bash"
echo "   docker run --rm -it -v \$(pwd)/data:/data slow32:emulator bash"
echo ""
echo "Note: Containers are ephemeral (--rm flag) and work with /data mount"