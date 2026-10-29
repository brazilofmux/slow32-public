# SLOW32 LLVM Backend

The `llvm-backend/` directory in this repository contains the complete SLOW32 backend for LLVM and Clang.

Status, features and tests: [llvm-backend/SLOW32/STATUS.md](../../llvm-backend/SLOW32/STATUS.md).

## Directory Structure

```
llvm-backend/
├── SLOW32/           # Complete SLOW32 target (from ~/llvm-project/llvm/lib/Target/SLOW32/)
├── clang/            # Clang target files (from ~/llvm-project/clang/lib/Basic/Targets/)
├── patches/          # Integration patches for LLVM build system
├── scripts/          # Maintenance scripts
└── test/             # lit tests (mirror of llvm/test/CodeGen/SLOW32/)
```

## Workflow

### Development

1. All development happens in `~/llvm-project`
2. Build and test there with native tools
3. When changes are ready, run backup script

### Backup Process
```bash
cd llvm-backend/scripts
./backup.sh
git diff  # Review changes
git add -A && git commit -m "Description of changes"
```

### Setting Up Fresh LLVM
For someone setting up SLOW32 in a new LLVM checkout:

```bash
# 1. Apply integration patches
./scripts/apply-patches.sh ~/path-to-llvm-project

# 2. Copy SLOW32 backend
cp -r SLOW32 ~/path-to-llvm-project/llvm/lib/Target/

# 3. Copy Clang files  
cp clang/* ~/path-to-llvm-project/clang/lib/Basic/Targets/

# 4. Build LLVM
cd ~/path-to-llvm-project
cmake --build build
```

### Emergency Recovery
If ~/llvm-project gets corrupted:
```bash
./scripts/restore.sh  # Will prompt for confirmation
```

## Features

See [STATUS.md](../../llvm-backend/SLOW32/STATUS.md).

## Testing

After any changes:
```bash
cd ~/slow-32/regression
./run-tests.sh
./run-llvm-verify.sh
```

## Integration Patches

The patches directory contains minimal changes needed to integrate SLOW32 into LLVM:

1. `01-llvm-triple.patch` - Add slow32 architecture to Triple.h/cpp
2. `02-llvm-cmake.patch` - Add SLOW32 to llvm/lib/Target/CMakeLists.txt
3. `03-clang-targets.patch` - Register SLOW32 target in Clang
4. `04-clang-cmake.patch` - Add SLOW32 to clang/lib/Basic/CMakeLists.txt
5. `05-clang-driver.patch` - The clang driver toolchain (`clang/lib/Driver/ToolChains/SLOW32.cpp`)
6. `06-libcxx.patch` - libc++ support for SLOW-32 (locale base API, filesystem)

These patches are automatically applied by `apply-patches.sh`.
