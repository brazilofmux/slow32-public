# Building the SLOW32 LLVM Backend

This guide walks through building LLVM with the SLOW32 backend from source.

## Prerequisites

- CMake 3.20 or later
- GCC or Clang compiler
- Ninja build system (optional but recommended)
- Python 3.8+
- About 20GB of disk space for build artifacts

## Quick Start

### 1. Clone LLVM

```bash
git clone https://github.com/llvm/llvm-project.git
cd llvm-project
```

### 2. Add SLOW32 Backend

Copy the SLOW32 backend files to the LLVM source tree:

```bash
# From the slow32-public repository root:
cp -r llvm-backend/SLOW32 /path/to/llvm-project/llvm/lib/Target/
```

### 3. Apply Patches

Apply the necessary patches to register SLOW32 with LLVM:

```bash
cd /path/to/llvm-project
git apply /path/to/slow32-public/llvm-backend/patches/01-llvm-triple.patch
```

### 4. Register the Backend

Edit `llvm/lib/Target/CMakeLists.txt` and add SLOW32 to the subdirectories list:

```cmake
add_subdirectory(SLOW32)
```

### 5. Configure and Build

```bash
mkdir build
cd build

# Configure with Ninja (recommended)
cmake -G Ninja \
  -DCMAKE_BUILD_TYPE=Release \
  -DLLVM_ENABLE_PROJECTS="clang" \
  -DLLVM_TARGETS_TO_BUILD="SLOW32" \
  -DLLVM_DEFAULT_TARGET_TRIPLE="slow32-unknown-none" \
  ../llvm

# Build
ninja

# Or with make (slower):
cmake -DCMAKE_BUILD_TYPE=Release \
  -DLLVM_ENABLE_PROJECTS="clang" \
  -DLLVM_TARGETS_TO_BUILD="SLOW32" \
  ../llvm
make -j$(nproc)
```

### 6. Install (Optional)

```bash
sudo ninja install
# Or: sudo make install
```

This will install to `/usr/local` by default. You can specify a different prefix:

```bash
cmake ... -DCMAKE_INSTALL_PREFIX=/opt/slow32-llvm
```

## Clang Integration

To add native Clang support for `-target slow32-unknown-none`:

1. Copy the Clang target files:
```bash
cp clang-target/SLOW32.cpp /path/to/llvm-project/clang/lib/Basic/Targets/
cp clang-target/SLOW32.h /path/to/llvm-project/clang/lib/Basic/Targets/
```

2. Register the target in `clang/lib/Basic/Targets.cpp`:
- Add `#include "Targets/SLOW32.h"` to the includes
- Add the SLOW32 case in `getTargetCreateFn()`:
```cpp
case llvm::Triple::slow32:
  return std::make_unique<SLOW32TargetInfo>(Triple, Opts);
```

3. Rebuild Clang

## Verification

After building, verify the installation:

```bash
# Check that SLOW32 is listed as a target
llc --version | grep slow32

# Test compilation
echo "int main() { return 42; }" > test.c
clang -target slow32-unknown-none -S -emit-llvm test.c -o test.ll
llc -mtriple=slow32-unknown-none test.ll -o test.s
```

## Building for Development

For development and debugging the backend:

```bash
cmake -G Ninja \
  -DCMAKE_BUILD_TYPE=Debug \
  -DLLVM_ENABLE_PROJECTS="clang" \
  -DLLVM_TARGETS_TO_BUILD="SLOW32;X86" \
  -DLLVM_ENABLE_ASSERTIONS=ON \
  -DLLVM_OPTIMIZED_TABLEGEN=ON \
  ../llvm

ninja
```

Including X86 allows you to compare generated code and debug issues.

## Common Issues

### CMake Can't Find Python
Specify Python explicitly:
```bash
cmake ... -DPython3_EXECUTABLE=/usr/bin/python3
```

### Out of Memory During Build
Limit parallel jobs:
```bash
ninja -j4  # Use only 4 parallel jobs
```

### TableGen Errors
If you see TableGen-related errors, ensure all `.td` files were copied correctly and that the patches were applied.

## Testing the Backend

Run the SLOW32 regression tests:

```bash
cd /path/to/slow32-public/regression
./run-tests.sh
```

All tests should pass with the correctly built backend.

## Docker Build

A Docker container with pre-built LLVM and SLOW32 backend is available:

```bash
docker build -t slow32-toolchain -f Dockerfile.toolchain .
docker run --rm -v $(pwd):/workspace slow32-toolchain
```

See `Dockerfile.toolchain` for the exact build steps used in the container.