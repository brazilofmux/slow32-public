#!/bin/bash
# Script to prepare SLOW-32 binary distribution from private repository
# Run this from the private ~/slow-32 directory to create a public release

set -e

VERSION=${1:-1.0.0}
ARCH=$(uname -m)
OS=$(uname -s | tr '[:upper:]' '[:lower:]')
RELEASE_NAME="slow32-tools-${VERSION}-${OS}-${ARCH}"
RELEASE_DIR="/tmp/${RELEASE_NAME}"

echo "Preparing SLOW-32 release ${VERSION} for ${OS}-${ARCH}"

# Clean any previous build
rm -rf "${RELEASE_DIR}"
mkdir -p "${RELEASE_DIR}"/{bin,lib,examples,doc}

# Check that we're in the slow-32 directory
if [ ! -f "assembler/slow32asm" ]; then
    echo "Error: Run this script from the ~/slow-32 directory"
    echo "Current directory: $(pwd)"
    exit 1
fi

echo "Building all components..."
make clean
make all

echo "Copying binaries..."
# Core tools
cp assembler/slow32asm "${RELEASE_DIR}/bin/"
cp linker/s32-ld "${RELEASE_DIR}/bin/"
cp emulator/slow32 "${RELEASE_DIR}/bin/"
cp emulator/slow32-fast "${RELEASE_DIR}/bin/" 2>/dev/null || echo "Note: slow32-fast not found"

# Analysis tools
cp tools/slow32dis "${RELEASE_DIR}/bin/" 2>/dev/null || echo "Note: slow32dis not found"
cp tools/s32-objdump "${RELEASE_DIR}/bin/" 2>/dev/null || echo "Note: s32-objdump not found"
cp tools/s32-exedump "${RELEASE_DIR}/bin/" 2>/dev/null || echo "Note: s32-exedump not found"

echo "Copying runtime libraries..."
cp runtime/crt0.s32o "${RELEASE_DIR}/lib/"
cp runtime/intrinsics.s32o "${RELEASE_DIR}/lib/"
cp runtime/debug_char.s32o "${RELEASE_DIR}/lib/" 2>/dev/null || true

echo "Copying examples..."
# Copy public-friendly examples
cat > "${RELEASE_DIR}/examples/hello.s" << 'EOF'
        .section .rodata
msg:    .ascii  "Hello, SLOW-32!\n\0"

        .section .text
        .globl  _start
_start:
        li      r3, msg
.loop:
        ldbu    r2, r3+0
        beq     r2, r0, .done
        debug   r2
        addi    r3, r3, 1
        jal     r0, .loop
.done:
        halt
EOF

cat > "${RELEASE_DIR}/examples/fibonacci.s" << 'EOF'
        .section .text
        .globl _start
_start:
        li      r3, 10          # Calculate fib(10)
        li      r4, 0           # fib(0) = 0
        li      r5, 1           # fib(1) = 1
.loop:
        beq     r3, r0, .done
        add     r6, r4, r5      # next = current + previous
        add     r4, r0, r5      # previous = current
        add     r5, r0, r6      # current = next
        addi    r3, r3, -1
        jal     r0, .loop
.done:
        add     r1, r0, r5      # Return fib(10) = 55
        halt
EOF

# Create a simple Makefile for examples
cat > "${RELEASE_DIR}/examples/Makefile" << 'EOF'
SLOW32_HOME ?= ..
AS = $(SLOW32_HOME)/bin/slow32asm
LD = $(SLOW32_HOME)/bin/s32-ld
EMU = $(SLOW32_HOME)/bin/slow32
RUNTIME = $(SLOW32_HOME)/lib

all: hello.s32x fibonacci.s32x

%.s32o: %.s
	$(AS) -o $@ $<

%.s32x: %.s32o
	$(LD) -o $@ $(RUNTIME)/crt0.s32o $<

run-%: %.s32x
	$(EMU) $<

clean:
	rm -f *.s32o *.s32x

test: all
	@echo "Testing hello..."
	@$(EMU) -c 1000 hello.s32x
	@echo "Testing fibonacci..."
	@$(EMU) -c 1000 fibonacci.s32x
	@echo "All tests passed!"

.PHONY: all clean test
EOF

echo "Creating documentation..."
cat > "${RELEASE_DIR}/doc/README.txt" << 'EOF'
SLOW-32 Toolchain
=================

Version: VERSION_PLACEHOLDER
Architecture: 32-bit RISC
License: MIT

Quick Start
-----------
1. Add bin/ to your PATH:
   export PATH=$PWD/bin:$PATH
   export SLOW32_HOME=$PWD

2. Test the installation:
   cd examples
   make test

3. Run an example:
   make run-hello

Tools Included
--------------
- slow32asm    : Assembler
- s32-ld       : Linker
- slow32       : Emulator (debug)
- slow32-fast  : Emulator (optimized)
- slow32dis    : Disassembler
- s32-objdump  : Object file analyzer
- s32-exedump  : Executable analyzer

Runtime Libraries (in lib/)
---------------------------
- crt0.s32o      : C runtime startup
- intrinsics.s32o: Built-in functions

Documentation
-------------
Full documentation available at:
https://github.com/[username]/slow32-public

Support
-------
Report issues at:
https://github.com/[username]/slow32-public/issues
EOF

sed -i "s/VERSION_PLACEHOLDER/${VERSION}/g" "${RELEASE_DIR}/doc/README.txt"

# Create LICENSE file
cat > "${RELEASE_DIR}/doc/LICENSE.txt" << 'EOF'
MIT License

Copyright (c) 2025 SLOW-32 Project

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
EOF

# Set executable permissions
chmod +x "${RELEASE_DIR}"/bin/*

# Create the tarball
echo "Creating release archive..."
cd /tmp
tar -czf "${RELEASE_NAME}.tar.gz" "${RELEASE_NAME}"

# Create checksums
sha256sum "${RELEASE_NAME}.tar.gz" > "${RELEASE_NAME}.tar.gz.sha256"

echo ""
echo "Release prepared successfully!"
echo "Archive: /tmp/${RELEASE_NAME}.tar.gz"
echo "SHA256:  /tmp/${RELEASE_NAME}.tar.gz.sha256"
echo ""
echo "Archive contents:"
tar -tzf "${RELEASE_NAME}.tar.gz" | head -20
echo "..."
echo ""
echo "Size: $(du -h /tmp/${RELEASE_NAME}.tar.gz | cut -f1)"
echo ""
echo "To test the release:"
echo "  cd /tmp"
echo "  tar -xzf ${RELEASE_NAME}.tar.gz"
echo "  cd ${RELEASE_NAME}"
echo "  export PATH=\$PWD/bin:\$PATH"
echo "  cd examples && make test"