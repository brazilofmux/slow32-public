# Complete Build Guide for SLOW-32

This guide shows how to build everything from source: the SLOW-32 toolchain and LLVM with SLOW-32 support.

## Prerequisites

- Linux system (Ubuntu 20.04+ or similar)
- ~60GB disk space
- ~16GB RAM
- CMake 3.20+
- Ninja or Make
- GCC 9+ or Clang 10+

## Step 1: Clone Both Repositories

```bash
# Get LLVM source (their repo)
git clone --depth 1 https://github.com/llvm/llvm-project.git
cd llvm-project

# Get SLOW-32 (our repo - includes everything)
cd ..
git clone https://github.com/[username]/slow32-public.git
```

## Step 2: Add SLOW-32 Backend to LLVM

```bash
# Copy the SLOW32 backend into LLVM
cp -r slow32-public/llvm-backend/SLOW32 llvm-project/llvm/lib/Target/

# Apply the integration patches
cd llvm-project
patch -p1 < ../slow32-public/llvm-backend/patches/llvm-integration-examples.patch
patch -p1 < ../slow32-public/llvm-backend/patches/clang-integration-examples.patch

# Copy Clang support files
cp ../slow32-public/llvm-backend/clang-support/SLOW32.* clang/lib/Basic/Targets/
```

## Step 3: Build LLVM with SLOW-32

```bash
# Configure build
mkdir build && cd build
cmake -G Ninja \
    -DCMAKE_BUILD_TYPE=Release \
    -DLLVM_EXPERIMENTAL_TARGETS_TO_BUILD="SLOW32" \
    -DLLVM_TARGETS_TO_BUILD="X86;AArch64" \
    -DLLVM_ENABLE_PROJECTS="clang" \
    ../llvm

# Build (this takes 30-60 minutes)
ninja llc clang

# Verify SLOW-32 is available
./bin/llc -version | grep -i slow32
./bin/clang --print-targets | grep -i slow32
```

## Step 4: Build SLOW-32 Tools

The toolchain binaries should be provided, but if building from source:

```bash
cd ../../slow32-public/tools

# If source is available (currently private):
# make -C assembler
# make -C linker
# make -C emulator

# For now, use pre-built binaries from releases
wget https://github.com/[username]/slow32-public/releases/download/v1.0.0/slow32-tools.tar.gz
tar -xzf slow32-tools.tar.gz
```

## Step 5: Set Up Environment

```bash
# Add to ~/.bashrc
export SLOW32_HOME=$HOME/slow32-public/tools
export SLOW32_LLVM=$HOME/llvm-project/build
export PATH=$SLOW32_HOME/bin:$SLOW32_LLVM/bin:$PATH
```

## Step 6: Test Everything

```bash
# Test LLVM backend
cat > test.c << EOF
int main() { return 42; }
EOF

clang --target=slow32-unknown-none -S test.c -o test.s
cat test.s  # Should show SLOW-32 assembly

# Test assembler and emulator (if binaries available)
slow32asm -o test.o test.s
s32-ld -o test.exe $SLOW32_HOME/lib/crt0.s32o test.o
slow32 test.exe
echo $?  # Should print 42
```

## Complete Workflow Example

```bash
# Write C program
cat > hello.c << EOF
void debug_char(char c) {
    __asm__("debug %0" : : "r"(c));
}

void print(const char* str) {
    while (*str) debug_char(*str++);
}

int main() {
    print("Hello, SLOW-32!\n");
    return 0;
}
EOF

# Compile with Clang
clang --target=slow32-unknown-none -S -emit-llvm -O1 hello.c -o hello.ll
llc -mtriple=slow32-unknown-none hello.ll -o hello.s

# Assemble and link
slow32asm -o hello.o hello.s
s32-ld -o hello.exe $SLOW32_HOME/lib/crt0.s32o hello.o $SLOW32_HOME/lib/intrinsics.s32o

# Run
slow32 hello.exe
```

## Directory Layout After Setup

```
~/
├── llvm-project/                 # Their LLVM
│   ├── llvm/
│   │   └── lib/Target/SLOW32/   # Our backend (copied)
│   ├── clang/
│   │   └── lib/Basic/Targets/   # Our Clang support (copied)
│   └── build/
│       └── bin/                  # Built clang and llc
│
└── slow32-public/                # Our repository
    ├── llvm-backend/
    │   ├── SLOW32/               # Backend source (original)
    │   └── patches/              # Integration patches
    ├── tools/                    # Assembler, linker, emulator
    ├── docs/                     # Documentation
    └── examples/                 # Sample programs
```

## Troubleshooting

### LLVM doesn't recognize SLOW32
- Check patches applied correctly
- Verify SLOW32 in LLVM_EXPERIMENTAL_TARGETS_TO_BUILD
- Rebuild LLVM

### Clang doesn't support slow32-unknown-none
- Ensure clang-support files copied
- Check clang/lib/Basic/Targets/CMakeLists.txt includes SLOW32.cpp
- Rebuild clang

### Tools not found
- Check PATH includes $SLOW32_HOME/bin
- Verify binaries are executable: `chmod +x $SLOW32_HOME/bin/*`

## Summary

1. **Two repositories**: LLVM (theirs) + slow32-public (ours)
2. **One copy command**: SLOW32 → llvm/lib/Target/
3. **Two small patches**: Register SLOW32 in LLVM/Clang
4. **Build both**: LLVM and tools
5. **Everything works**: Full toolchain from C to execution

This approach:
- ✅ Users control their LLVM version
- ✅ Our backend is self-contained
- ✅ Patches are minimal and documented
- ✅ Everything needed is in slow32-public
- ✅ No hunting for files or versions