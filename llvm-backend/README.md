# SLOW-32 LLVM Backend

This directory contains the SLOW-32 backend for LLVM and instructions for integrating it into your LLVM build.

## What's Included

This directory contains everything needed to add SLOW-32 support to LLVM:

1. **SLOW32/** - Complete backend implementation (copy to llvm/lib/Target/)
2. **patches/** - Small integration patches to register SLOW32 in LLVM/Clang
3. **clang-support/** - Clang target definition files
4. **INSTALL.md** - Detailed installation instructions

## Revision History

| Version | Date | Author | Description |
|---------|------|--------|-------------|
| 1.0.0 | 2025-09-05 | S. Dennis | Initial public release |
| 0.9.0 | 2025-09-04 | S. Dennis | Jump tables, varargs support |
| 0.8.0 | 2025-09-03 | S. Dennis | Fixed 32-bit constants (LUI+ADDI) |

## Prerequisites

- LLVM 19.0+ source code
- CMake 3.20+
- C++17 compatible compiler
- ~50GB disk space for full build
- ~16GB RAM for parallel builds

## Quick Installation

```bash
# 1. Get LLVM source
git clone --depth 1 https://github.com/llvm/llvm-project.git
cd llvm-project

# 2. Copy the SLOW32 backend from this repository
cp -r /path/to/slow32-public/llvm-backend/SLOW32 llvm/lib/Target/

# 3. Apply LLVM core patches (with examples showing exact locations)
patch -p1 < /path/to/slow32-public/llvm-backend/patches/llvm-integration-examples.patch

# 4. Apply Clang patches (with examples showing exact locations)
patch -p1 < /path/to/slow32-public/llvm-backend/patches/clang-integration-examples.patch

# 5. Copy Clang support files
cp /path/to/slow32-public/llvm-backend/clang-support/SLOW32.* clang/lib/Basic/Targets/

# 6. Build LLVM with SLOW-32
mkdir build && cd build
cmake -G "Unix Makefiles" \
    -DCMAKE_BUILD_TYPE=Release \
    -DLLVM_EXPERIMENTAL_TARGETS_TO_BUILD="SLOW32" \
    -DLLVM_TARGETS_TO_BUILD="X86" \
    -DLLVM_ENABLE_PROJECTS="clang" \
    ../llvm

make -j$(nproc) llc clang
```

## Manual Integration

If the patches don't apply cleanly, here are the manual steps:

### 1. Copy Backend Files

Copy the entire `SLOW32/` directory to `llvm/lib/Target/SLOW32/`

### 2. Register the Target

Edit `llvm/CMakeLists.txt`:
```cmake
# Around line 450, add SLOW32 to the list of all targets
set(LLVM_ALL_TARGETS
  AArch64
  AMDGPU
  ARM
  # ... other targets ...
  SLOW32    # <-- Add this line
  SPARC
  SystemZ
  # ...
)
```

### 3. Add to Target Build

Edit `llvm/lib/Target/CMakeLists.txt`:
```cmake
# Add this line in alphabetical order
add_subdirectory(SLOW32)
```

### 4. Configure Triple Support

Edit `llvm/lib/Support/Triple.cpp`:
```cpp
// In the ArchType enum (around line 45)
enum ArchType {
  UnknownArch,
  // ... other architectures ...
  slow32,     // SLOW-32 32-bit  <-- Add this
  sparc,      // Sparc: sparc
  // ...
};

// In getArchTypeName() (around line 200)
case slow32:         return "slow32";

// In parseArch() (around line 500)
.Case("slow32", Triple::slow32)

// In getDefaultFormat() (around line 900)
case Triple::slow32:
  return Triple::ELF;
```

### 5. Add ELF Support

Edit `llvm/include/llvm/BinaryFormat/ELF.h`:
```cpp
// Around line 300, add SLOW32 machine type
enum {
  // ... other machines ...
  EM_SLOW32      = 0x5332,  // SLOW-32 (using 'S2' in hex)
  // ...
};
```

### 6. Clang Target Support

Edit `clang/lib/Basic/Targets.cpp`:
```cpp
// Add near other target includes
#include "Targets/SLOW32.h"

// In getTargetDefines() or similar initialization
case llvm::Triple::slow32:
  return std::make_unique<SLOW32TargetInfo>(Triple, Opts);
```

Create `clang/lib/Basic/Targets/SLOW32.h`:
```cpp
// See provided SLOW32.h in clang-support/
```

Create `clang/lib/Basic/Targets/SLOW32.cpp`:
```cpp
// See provided SLOW32.cpp in clang-support/
```

### 7. Register Clang Target

Edit `clang/lib/Basic/Targets/CMakeLists.txt`:
```cmake
add_clang_library(clangBasicTargets
  # ... other files ...
  SLOW32.cpp   # <-- Add this
  # ...
)
```

## Backend Source

The SLOW32 backend source code is included in this repository:
- **Location**: `llvm-backend/SLOW32/` directory
- **Installation**: Copy entire directory to `llvm/lib/Target/SLOW32/`
- **Version**: Compatible with LLVM 19.0+

## Directory Structure

```
llvm-backend/
├── README.md                 # This file
├── INSTALL.md               # Detailed installation guide
├── SLOW32-STRUCTURE.md      # Backend directory structure
├── patches/
│   ├── llvm-integration-examples.patch  # LLVM changes with context
│   └── clang-integration-examples.patch # Clang changes with context
├── clang-support/
│   ├── SLOW32.h             # Clang target header
│   └── SLOW32.cpp           # Clang target implementation
└── tests/
    └── basic.ll             # Basic test file
```

## Testing the Backend

After building, verify the backend works:

```bash
# Check that SLOW32 is registered
build/bin/llc -version | grep -i slow32

# Test compilation
echo 'define i32 @main() { ret i32 42 }' > test.ll
build/bin/llc -mtriple=slow32-unknown-none test.ll -o test.s
cat test.s  # Should show SLOW-32 assembly

# Test Clang integration
build/bin/clang --print-targets | grep -i slow32

# Compile C code
cat > test.c << EOF
int main() { return 42; }
EOF
build/bin/clang --target=slow32-unknown-none -S test.c -o test.s
```

## Troubleshooting

### "Unknown target triple"
The patches didn't apply correctly. Check `Triple.cpp` modifications.

### "No available targets are compatible"
SLOW32 wasn't included in LLVM_EXPERIMENTAL_TARGETS_TO_BUILD during CMake.

### Clang doesn't recognize slow32-unknown-none
Clang patches missing. Check `clang/lib/Basic/Targets/`.

### Build errors in SLOW32 directory
Check C++ standard (needs C++17) and LLVM API compatibility.

## Current Features

✅ **Working:**
- Basic arithmetic and logic
- Function calls with proper ABI
- Global variables and arrays
- Stack frame management
- Jump tables for switch
- Varargs support
- Partial 64-bit support (ADD/SUB)

⚠️ **Partial:**
- 64-bit MUL/DIV (via libcalls)
- Inline assembly
- Debug information

❌ **Not Implemented:**
- Floating point
- SIMD/Vectors
- Exception handling
- Thread-local storage

## Contributing

When modifying the backend:

1. Follow LLVM coding standards
2. Add tests to `tests/` directory
3. Update revision history
4. Run regression tests: `cd ~/slow-32/regression && ./run-tests.sh`

## Support

- Check `docs/35-llvm-backend.md` for detailed documentation
- See examples in `tests/` directory
- Report issues with LLVM version and error messages