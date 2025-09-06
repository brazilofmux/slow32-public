# SLOW32 Backend Distribution Plan

## Current Situation

The SLOW32 LLVM backend exists in the private ~/llvm-project repository and needs to be made public.

## Distribution Options

### Option 1: Include Directly (Recommended)
**Process:**
1. Run `tools/extract-llvm-backend.sh` to extract from private repo
2. Review all files for private information
3. Commit the SLOW32/ directory to public repo
4. Users just copy the directory to their LLVM tree

**Pros:**
- Simple for users - everything in one place
- No separate downloads needed
- Version controlled with patches

**Cons:**
- Larger repository size
- Need to carefully review before committing

### Option 2: Separate Release Archive
**Process:**
1. Extract and create tar.gz
2. Upload to GitHub releases
3. Users download separately

**Pros:**
- Keeps repository smaller
- Can update independently

**Cons:**
- Extra step for users
- Need to maintain separate releases

### Option 3: Submodule
**Process:**
1. Create separate slow32-llvm-backend repo
2. Add as git submodule

**Pros:**
- Clean separation
- Can be updated independently

**Cons:**
- Git submodules are confusing for many users
- Extra complexity

## Recommendation

**Include the SLOW32 backend directly in llvm-backend/SLOW32/**

This is most user-friendly. The process would be:

1. **Extraction** (one-time by maintainer):
   ```bash
   cd ~/llvm-project
   ~/slow32-public/tools/extract-llvm-backend.sh
   ```

2. **Review** (important!):
   - Check for any private comments
   - Verify no hardcoded paths
   - Ensure no debug code remains

3. **Commit**:
   ```bash
   cd ~/slow32-public
   git add llvm-backend/SLOW32
   git commit -m "Add SLOW32 LLVM backend source"
   ```

4. **User Installation**:
   ```bash
   # Simple copy from public repo to LLVM
   cp -r slow32-public/llvm-backend/SLOW32 llvm/lib/Target/
   # Apply patches
   patch -p1 < slow32-public/llvm-backend/patches/llvm-integration-examples.patch
   ```

## What to Include

The SLOW32/ directory should contain:
- All .td files (TableGen descriptions)
- All .cpp/.h files (implementation)
- CMakeLists.txt files
- MCTargetDesc/ subdirectory
- TargetInfo/ subdirectory
- README with version info

## What NOT to Include

Do not include:
- Build artifacts (.o, .d, .inc files)
- CMake generated files
- Editor swap files
- Private TODO/NOTES files
- Debug/development code

## File Count Estimate

Based on typical LLVM backend:
- ~15-20 .cpp files
- ~10-15 .h files
- ~5-8 .td files
- ~3 CMakeLists.txt files
- Total: ~40-50 files

This is reasonable to include directly in the repository.