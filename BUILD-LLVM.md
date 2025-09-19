# Complete Build Guide for SLOW32 with LLVM Backend

This guide provides step-by-step instructions to build the complete SLOW32 toolchain including the LLVM backend.

## Prerequisites

- Linux system (tested on Ubuntu 22.04+)
- Git
- CMake 3.20+
- Ninja or Make
- GCC or Clang
- Python 3.8+
- ~20GB free disk space

## Step 1: Clone and Prepare LLVM

```bash
# Clone LLVM project
git clone https://github.com/llvm/llvm-project.git
cd llvm-project

# Note the path - you'll need it
export LLVM_DIR=$(pwd)
```

## Step 2: Install SLOW32 Backend

```bash
# Clone or download this repository
git clone https://github.com/yourusername/slow32-public.git
cd slow32-public
export SLOW32_DIR=$(pwd)

# Copy backend files to LLVM
cp -r llvm-backend/SLOW32 $LLVM_DIR/llvm/lib/Target/

# Apply patches
cd $LLVM_DIR
git apply $SLOW32_DIR/llvm-backend/patches/01-llvm-triple.patch

# Register the backend in CMakeLists.txt
echo "add_subdirectory(SLOW32)" >> llvm/lib/Target/CMakeLists.txt
```

## Step 3: Add Clang Support (Optional but Recommended)

This enables the `-target slow32-unknown-none` flag in Clang:

```bash
# Copy Clang target support files
cp $SLOW32_DIR/llvm-backend/clang-support/SLOW32.cpp $LLVM_DIR/clang/lib/Basic/Targets/
cp $SLOW32_DIR/llvm-backend/clang-support/SLOW32.h $LLVM_DIR/clang/lib/Basic/Targets/

# Edit clang/lib/Basic/Targets.cpp
# Add near other includes:
#   #include "Targets/SLOW32.h"
#
# Add in getTargetCreateFn() function:
#   case llvm::Triple::slow32:
#     return std::make_unique<SLOW32TargetInfo>(Triple, Opts);
```

## Step 4: Build LLVM with SLOW32 Backend

```bash
cd $LLVM_DIR
mkdir build
cd build

# Configure (with Ninja - faster)
cmake -G Ninja \
  -DCMAKE_BUILD_TYPE=Release \
  -DLLVM_ENABLE_PROJECTS="clang" \
  -DLLVM_TARGETS_TO_BUILD="SLOW32" \
  -DLLVM_DEFAULT_TARGET_TRIPLE="slow32-unknown-none" \
  ../llvm

# Build (this will take 10-30 minutes)
ninja

# Or with Make (slower):
cmake -DCMAKE_BUILD_TYPE=Release \
  -DLLVM_ENABLE_PROJECTS="clang" \
  -DLLVM_TARGETS_TO_BUILD="SLOW32" \
  ../llvm
make -j$(nproc)

# Optional: Install to system
sudo ninja install  # or: sudo make install
```

## Step 5: Build SLOW32 Tools

```bash
cd $SLOW32_DIR

# Build all tools
make clean
make

# This builds:
# - Assembler (tools/assembler/slow32asm)
# - Linker (tools/linker/s32-ld)
# - Emulator (tools/emulator/slow32)
# - Disassembler (tools/utilities/slow32dis)
# - Object/Exe dumpers
```

## Step 6: Set Up Environment

Add to your `.bashrc` or `.zshrc`:

```bash
# If you didn't install LLVM system-wide:
export PATH=$LLVM_DIR/build/bin:$PATH

# SLOW32 tools
export PATH=$SLOW32_DIR/tools/assembler:$PATH
export PATH=$SLOW32_DIR/tools/linker:$PATH
export PATH=$SLOW32_DIR/tools/emulator:$PATH
export PATH=$SLOW32_DIR/tools/utilities:$PATH
```

## Step 7: Verify Installation

```bash
# Check LLVM recognizes SLOW32
llc --version | grep slow32

# Run quick test
cd $SLOW32_DIR
./scripts/test-quick.sh

# Run regression tests (all should pass)
cd regression
./run-tests.sh
```

## Usage Examples

### Compile and Run C Program

```bash
# Write a simple C program
cat > hello.c <<EOF
void print_char(char c) {
    __asm__ __volatile__("debug %0" : : "r"(c));
}

void print_string(const char* str) {
    while (*str) {
        print_char(*str++);
    }
}

int main() {
    print_string("Hello, SLOW32!\n");
    return 0;
}
EOF

# Compile and run (using helper script)
./scripts/compile.sh hello.c hello.s32x
./tools/emulator/slow32 hello.s32x

# Or manually:
clang -target slow32-unknown-none -S -emit-llvm -O2 -Iruntime/include hello.c -o hello.ll
llc -mtriple=slow32-unknown-none hello.ll -o hello.s
./tools/assembler/slow32asm hello.s hello.s32o
./tools/linker/s32-ld -o hello.s32x runtime/crt0.s32o hello.s32o runtime/libs32.s32a runtime/libc.s32a
./tools/emulator/slow32 hello.s32x
```

### Use Standard Library

```bash
# Program using printf (varargs work!)
cat > test_printf.c <<EOF
#include <stdio.h>

int main() {
    printf("The answer is %d\n", 42);
    printf("Hex: 0x%x, Char: %c\n", 255, 'A');
    return 0;
}
EOF

./scripts/compile.sh test_printf.c
./tools/emulator/slow32 test_printf.s32x
```

## Docker Alternative

If you prefer not to build from source, use the Docker image:

```bash
# Build Docker image with everything pre-built
docker build -t slow32-toolchain -f Dockerfile.toolchain .

# Use it to compile programs
docker run --rm -v $(pwd):/workspace slow32-toolchain \
  bash -c "cd /workspace && ./scripts/compile.sh myprogram.c"
```

## Troubleshooting

### LLVM Build Fails
- Ensure you have enough RAM (8GB minimum)
- Limit parallel jobs: `ninja -j4` or `make -j4`
- Try Debug build if Release fails

### Backend Not Found
- Verify SLOW32 is in `llvm/lib/Target/CMakeLists.txt`
- Check all files were copied to `llvm/lib/Target/SLOW32/`

### Tests Failing
- Ensure you built with `-O2` optimization
- Check that runtime libraries were built: `ls runtime/*.s32a`
- Verify LLVM version compatibility

## Next Steps

- Read `docs/ARCHITECTURE.md` for CPU design details
- See `examples/` for more complex programs
- Check `docs/IMPROVEMENTS.md` for known issues and future work
- Join discussions at the GitHub repository

## Support

For issues or questions:
- Open an issue on GitHub
- Check existing issues and discussions
- See `CLAUDE.md` for AI assistant context