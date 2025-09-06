# Installing the SLOW-32 LLVM Backend

## Quick Install

```bash
# 1. Get LLVM 19+ source
git clone --depth 1 https://github.com/llvm/llvm-project.git
cd llvm-project

# 2. Copy the SLOW32 backend directory
cp -r /path/to/slow32-public/llvm-backend/SLOW32 llvm/lib/Target/

# 3. Apply integration patches (small changes to LLVM/Clang)
patch -p1 < /path/to/slow32-public/llvm-backend/patches/llvm-integration.patch
patch -p1 < /path/to/slow32-public/llvm-backend/patches/clang-integration.patch

# 4. Build
mkdir build && cd build
cmake -G Ninja \
    -DCMAKE_BUILD_TYPE=Release \
    -DLLVM_EXPERIMENTAL_TARGETS_TO_BUILD="SLOW32" \
    -DLLVM_TARGETS_TO_BUILD="X86;AArch64" \
    -DLLVM_ENABLE_PROJECTS="clang" \
    ../llvm
ninja llc clang
```

## What Gets Installed

### The SLOW32 Directory (Complete Backend)
The `SLOW32/` directory contains the complete backend implementation:
- Target description files (.td)
- Code generation (C++)
- Instruction selection
- Register allocation
- Assembly printing

This is provided as source files, not patches, making it easy to browse and modify.

### Integration Patches (Small Changes)
Two small patch files integrate SLOW32 into LLVM:

1. **llvm-integration.patch** - Registers SLOW32 as a target
   - Adds SLOW32 to CMakeLists.txt
   - Updates Triple.cpp for slow32 triple support
   - Adds ELF machine type

2. **clang-integration.patch** - Enables Clang support
   - Registers slow32-unknown-none target
   - Links target info classes

## Manual Integration (If Patches Fail)

### Step 1: Register SLOW32 in LLVM

Edit `llvm/CMakeLists.txt` (around line 450):
```cmake
set(LLVM_ALL_TARGETS
  AArch64
  # ... other targets ...
  SLOW32    # <-- Add this line alphabetically
  SPARC
  # ...
)
```

Edit `llvm/lib/Target/CMakeLists.txt`:
```cmake
# Add alphabetically
add_subdirectory(SLOW32)
```

### Step 2: Add Triple Support

Edit `llvm/lib/Support/Triple.cpp`:

In the ArchType enum:
```cpp
enum ArchType {
  // ... 
  slow32,     // SLOW-32 32-bit
  sparc,
  // ...
};
```

In getArchTypeName():
```cpp
case slow32:         return "slow32";
```

In parseArch():
```cpp
.Case("slow32", Triple::slow32)
```

In getDefaultFormat():
```cpp
case Triple::slow32:
  return Triple::ELF;
```

### Step 3: Add ELF Machine Type

Edit `llvm/include/llvm/BinaryFormat/ELF.h`:
```cpp
enum {
  // ...
  EM_SLOW32 = 0x5332,  // SLOW-32 ('S2' in hex)
  // ...
};
```

### Step 4: Enable Clang Support

Edit `clang/lib/Basic/Targets.cpp`:
```cpp
// Add include
#include "Targets/SLOW32.h"

// In AllocateTarget() switch statement
case llvm::Triple::slow32:
  return std::make_unique<SLOW32TargetInfo>(Triple, Opts);
```

Copy Clang support files:
```bash
cp clang-support/SLOW32.* clang/lib/Basic/Targets/
```

Edit `clang/lib/Basic/Targets/CMakeLists.txt`:
```cmake
add_clang_library(clangBasicTargets
  # ...
  SLOW32.cpp   # <-- Add this
  # ...
)
```

## Verification

```bash
# Check SLOW32 is available
build/bin/llc -version | grep -i slow32
build/bin/clang --print-targets | grep -i slow32

# Test compilation
cat > test.c << EOF
int main() { return 42; }
EOF

build/bin/clang --target=slow32-unknown-none -S test.c -o test.s
cat test.s  # Should show SLOW-32 assembly
```

## Troubleshooting

### CMake doesn't find SLOW32
- Ensure SLOW32 is in LLVM_EXPERIMENTAL_TARGETS_TO_BUILD
- Check that llvm/lib/Target/SLOW32/ exists
- Verify CMakeLists.txt changes applied

### "Unknown triple" error
- Triple.cpp changes missing
- Rebuild LLVM Support library

### Clang doesn't recognize target
- Clang patches not applied
- SLOW32.cpp/h not in clang/lib/Basic/Targets/
- Rebuild Clang

## File Structure

```
llvm-backend/
├── INSTALL.md           # This file
├── SLOW32/              # Complete backend (copy to llvm/lib/Target/)
│   ├── CMakeLists.txt
│   ├── SLOW32.td
│   ├── SLOW32*.cpp/h   # Implementation files
│   └── ...
├── patches/
│   ├── llvm-integration.patch   # LLVM core changes (small)
│   └── clang-integration.patch  # Clang changes (small)
└── clang-support/
    ├── SLOW32.h         # Clang target header
    └── SLOW32.cpp       # Clang target implementation
```

## Notes

- The SLOW32 directory is self-contained
- Patches are minimal - just registration/integration
- No changes to existing targets
- Easy to remove: delete SLOW32/ and revert patches