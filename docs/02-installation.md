# Installation Guide

This guide covers building the SLOW-32 toolchain from source.

## Required Tools

The SLOW-32 toolchain consists of these essential tools:

1. **slow32asm** - Assembler (converts .s to .s32o)
2. **s32-ld** - Linker (creates executables)
3. **slow32** - Reference emulator (with debugging)
4. **slow32-fast** - Fast emulator (optimized)
5. **slow32dis** - Disassembler
6. **s32-objdump** - Object file analyzer
7. **s32-exedump** - Executable analyzer
8. **Runtime libraries** - crt0.s32o, intrinsics.s32o

## Building from Source

### Option 1: Download Pre-built Binaries (Recommended)

```bash
# Download the latest release
wget https://github.com/brazilofmux/slow32-public/releases/latest/download/slow32-tools-linux-x86_64.tar.gz

# Extract to your preferred location
mkdir -p $HOME/slow32-tools
tar -xzf slow32-tools-linux-x86_64.tar.gz -C $HOME/slow32-tools

# Set up environment
export SLOW32_HOME=$HOME/slow32-tools
export PATH=$SLOW32_HOME/bin:$PATH

# Verify installation
slow32asm --version
```

### Option 2: Build from Source

```bash
# Clone the toolchain repository (private - requires access)
git clone https://github.com/brazilofmux/slow32-tools.git $HOME/slow32-tools
cd $HOME/slow32-tools

# Build all tools
make

# Set up environment
export SLOW32_HOME=$HOME/slow32-tools
export PATH=$SLOW32_HOME/bin:$PATH

# Tools are now in:
# $SLOW32_HOME/bin/slow32asm      - Assembler
# $SLOW32_HOME/bin/s32-ld         - Linker
# $SLOW32_HOME/bin/slow32         - Reference emulator
# $SLOW32_HOME/bin/slow32-fast    - Fast emulator
# $SLOW32_HOME/bin/slow32dis      - Disassembler
# $SLOW32_HOME/bin/s32-objdump    - Object file analyzer
# $SLOW32_HOME/bin/s32-exedump    - Executable analyzer
# $SLOW32_HOME/lib/*.s32o         - Runtime libraries
```

### Setting Up Your Environment

```bash
# Add to ~/.bashrc or ~/.zshrc
echo 'export SLOW32_HOME=$HOME/slow32-tools' >> ~/.bashrc
echo 'export PATH=$SLOW32_HOME/bin:$PATH' >> ~/.bashrc
source ~/.bashrc

# Verify installation
slow32asm --version
slow32 --help
```

## System Requirements

### Minimum Requirements
- Linux x86_64 (Ubuntu 20.04+, Debian 11+, Fedora 34+, or similar)
- 100MB disk space
- 512MB RAM

### For Building from Source
- GCC 9+ or Clang 10+ (C++17 support required)
- Make 4.0+
- CMake 3.16+ (for LLVM backend)
- 2GB RAM (16GB for LLVM backend)
- 1GB disk space (50GB for LLVM backend)

## Binary Distribution Contents

```
slow32-tools/
├── bin/
│   ├── slow32asm        # Assembler
│   ├── s32-ld           # Linker
│   ├── slow32           # Reference emulator (debug)
│   ├── slow32-fast      # Performance emulator
│   ├── slow32dis        # Disassembler
│   ├── s32-objdump      # Object file analyzer
│   └── s32-exedump      # Executable analyzer
├── lib/
│   ├── crt0.s32o        # C runtime startup (required)
│   ├── intrinsics.s32o  # Built-in functions (memcpy, memset)
│   └── debug_char.s32o  # Debug output helpers
├── examples/
│   ├── hello.s          # Hello world in assembly
│   ├── hello.c          # Hello world in C
│   ├── fibonacci.s      # Fibonacci calculator
│   └── Makefile         # Build all examples
└── doc/
    ├── README.txt       # Quick start guide
    └── LICENSE.txt      # MIT License
```

## Quick Test

After installation, test the toolchain:

```bash
# Create a simple test program
cat > test.s << 'EOF'
        .section .text
        .globl _start
_start:
        li r1, 42       # Return value 42
        halt
EOF

# Assemble
slow32asm -o test.s32o test.s

# Link (crt0.s32o must be first!)
s32-ld -o test.s32x $SLOW32_HOME/lib/crt0.s32o test.s32o

# Run
slow32 test.s32x
echo "Return value: $?"  # Should print 42

# Disassemble to verify
slow32dis test.s32x

# Clean up
rm test.s test.s32o test.s32x
```

## Tool Usage

### Assembler (slow32asm)
```bash
# Modern syntax (recommended)
slow32asm -o output.s32o input.s

# Legacy syntax (still supported)
slow32asm input.s output.s32o
```

### Linker (s32-ld)
```bash
# Basic linking (crt0.s32o MUST be first)
s32-ld -o program.s32x $SLOW32_HOME/lib/crt0.s32o main.s32o

# With intrinsics library (for C programs)
s32-ld -o program.s32x \
    $SLOW32_HOME/lib/crt0.s32o \
    main.s32o \
    $SLOW32_HOME/lib/intrinsics.s32o
```

### Emulators
```bash
# Normal execution
slow32 program.s32x

# Debug modes
slow32 -t program.s32x        # Trace every instruction
slow32 -s program.s32x        # Step through one at a time
slow32 -r program.s32x        # Show register changes
slow32 -b 0x100 program.s32x  # Set breakpoint
slow32 -c 1000 program.s32x   # Limit to 1000 cycles

# Fast execution (no debugging)
slow32-fast program.s32x      # ~350M instructions/sec
```

### Analysis Tools
```bash
# Disassemble executable
slow32dis program.s32x
slow32dis program.s32x 0x0 0x100  # Specific range

# Examine object file
s32-objdump file.s32o

# Examine executable
s32-exedump program.s32x
```

## Optional: LLVM Backend

For C/C++ compilation support, you'll need to build LLVM with the SLOW-32 backend.

### Getting the Backend

The SLOW-32 backend for LLVM is distributed separately:

```bash
# Download backend patches from releases
wget https://github.com/brazilofmux/slow32-public/releases/latest/download/slow32-llvm-backend.tar.gz

# Follow the instructions in the archive
tar -xzf slow32-llvm-backend.tar.gz
cat slow32-llvm-backend/README.txt
```

See [LLVM Backend Guide](35-llvm-backend.md) for detailed build instructions.

## Environment Setup

Add to `~/.bashrc` or `~/.zshrc`:

```bash
# SLOW-32 Toolchain
export SLOW32_HOME=$HOME/slow32-tools
export PATH=$SLOW32_HOME/bin:$PATH

# Optional: LLVM with SLOW-32 backend
# export SLOW32_LLVM=$HOME/llvm-slow32
# export PATH=$SLOW32_LLVM/bin:$PATH

# Convenience aliases
alias s32='slow32'
alias s32f='slow32-fast'
alias s32asm='slow32asm'
alias s32ld='s32-ld'
alias s32dis='slow32dis'

# Helper function for quick compile-and-run
s32run() {
    if [ -z "$1" ]; then
        echo "Usage: s32run program.s"
        return 1
    fi
    base=$(basename "$1" .s)
    slow32asm -o "$base.s32o" "$1" && \
    s32-ld -o "$base.s32x" $SLOW32_HOME/lib/crt0.s32o "$base.s32o" && \
    slow32 "$base.s32x"
}
```

## Platform-Specific Notes

### Ubuntu/Debian
```bash
# Install dependencies if building from source
sudo apt-get update
sudo apt-get install build-essential make

# The pre-built binaries should work on Ubuntu 20.04+
```

### macOS
```bash
# macOS binaries planned for future release
# Currently requires building from source or using Docker
```

### Windows (WSL2)
```bash
# Use Linux binaries under WSL2
# Native Windows support planned for future
```

### Docker
```dockerfile
# Dockerfile for SLOW-32 environment
FROM ubuntu:22.04
COPY slow32-tools-*.tar.gz /tmp/
RUN tar -xzf /tmp/slow32-tools-*.tar.gz -C /opt/ && \
    ln -s /opt/slow32-tools/bin/* /usr/local/bin/
```

## Troubleshooting

### "Command not found"
Ensure PATH is set correctly:
```bash
echo $PATH | grep slow32
# If missing, re-run: export PATH=$SLOW32_HOME/bin:$PATH
```

### "Cannot find crt0.s32o"
Specify the full path:
```bash
s32-ld -o prog.s32x $SLOW32_HOME/lib/crt0.s32o prog.s32o
```

### "Permission denied"
Make binaries executable:
```bash
chmod +x $SLOW32_HOME/bin/*
```

### "GLIBC version error"
The binaries require GLIBC 2.31+. Check your version:
```bash
ldd --version
```
If too old, you'll need to build from source or upgrade your system.

## Verifying Installation

Run the verification script:

```bash
# Quick verification
slow32asm --version && echo "✓ Assembler OK"
s32-ld --version && echo "✓ Linker OK"
slow32 --help > /dev/null && echo "✓ Emulator OK"
slow32dis --help > /dev/null && echo "✓ Disassembler OK"

# Full test
cd $SLOW32_HOME/examples
make test
```

## Getting Binaries

Binaries are distributed through:

1. **GitHub Releases** - https://github.com/brazilofmux/slow32-public/releases
2. **Platform Support**:
   - Linux x86_64 (Ubuntu 20.04+, Debian 11+)
   - macOS (planned)
   - Windows WSL2 (use Linux binaries)
3. **Source Code** - Private repository (contact for access)

## Support

- **Documentation** - See the docs/ directory
- **Examples** - See the examples/ directory
- **Issues** - Report problems on GitHub Issues
- **Updates** - Watch the repository for new releases

## Next Steps

1. Run through the [Quickstart](10-quickstart.md)
2. Try the [Example Programs](../examples/)
3. Read the [Programmer's Guide](20-programmers-guide.md)
4. Learn about the [Toolchain](15-toolchain-overview.md)