# SLOW32 Public Release - 64-bit Integer Support

## Release Status

- ✅ Development repository committed (`~/slow-32`)
- ✅ Public repository staged (`/ztank/secret/sdennis/slow32-public`)
- ⏳ Awaiting review before pushing externally

## Changes Ready for Public Release

### Core Features Added
1. **XORI Instruction** (opcode 0x1E)
   - Files: `assembler/slow32asm.c`, `emulator/slow32.c`, `emulator/slow32.h`, `emulator/slow32-fast.c`
   - Adds XOR immediate operation support

2. **64-bit Division/Remainder Runtime Support**
   - File: `runtime/builtins.c` - Compiler-rt compatible implementations
   - Functions: `__udivdi3`, `__divdi3`, `__umoddi3`, `__moddi3`
   - Plus helper functions: `__clzdi2`, `__ashrdi3`, `__lshrdi3`, `__ashldi3`

3. **Build System Improvements**
   - `runtime/Makefile` - Automated runtime library builds
   - `Makefile` - Updated to include runtime target
   - `tools/compile-c.sh` - Helper script for C compilation

4. **Documentation Updates**
   - `CLAUDE.md` - Updated feature list showing complete 64-bit support

### Files to Include in Public Release
```
✅ assembler/slow32asm.c          # XORI instruction support
✅ emulator/slow32.c              # XORI instruction support  
✅ emulator/slow32.h              # XORI opcode definition
✅ emulator/slow32-fast.c         # XORI in fast emulator
✅ runtime/builtins.c             # 64-bit division implementations
✅ runtime/Makefile               # Runtime build automation
✅ Makefile                       # Updated with runtime target
✅ tools/compile-c.sh             # C compilation helper
✅ CLAUDE.md                      # Updated documentation
✅ tests/test_64bit_div_args.c    # Example test showing usage
```

### Files NOT for Public Release (Internal/Development)
```
❌ llvm-backend/SLOW32/*          # LLVM backend (separate project)
❌ llvm-sync/*                    # Internal coordination files
❌ runtime/builtins.s             # Generated assembly (can be regenerated)
❌ tests/*.ll, tests/*.s          # Intermediate compilation files
❌ PUBLIC_RELEASE_NOTES.md        # This file (internal notes)
```

### LLVM Backend Changes
The LLVM backend changes in `llvm-backend/SLOW32/` should be synchronized with the main LLVM project repository, not the SLOW32 public repository. These include:
- `SLOW32ISelLowering.cpp` - libcall implementation mappings
- `SLOW32ISelLowering.h` - Header updates

### LLVM Backend Fixes
- Added explicit libcall mappings in `SLOW32ISelLowering.cpp`
- Ensured backend generates correct calls to compiler-rt builtins for i64 div/rem

### Build Infrastructure
- Created `runtime/Makefile` for automated runtime builds
- Updated top-level `Makefile` to include the runtime target
- Added `tools/compile-c.sh` helper script for streamlined compilation

### Testing Before Release
1. Clean build: `make clean && make all`
2. Compile sample: `./tools/compile-c.sh tests/test_64bit_div_args.c`
3. Run via emulator: `./emulator/slow32 tests/test_64bit_div_args.s32x` (expect "RUN")

### Public Repository Checklist

```
cd /ztank/secret/sdennis/slow32-public
git diff            # review staged changes
git add -A
git commit -m "Add complete 64-bit integer support

- Added XORI instruction (opcode 0x1E) for XOR immediate operations
- Implemented compiler-rt compatible 64-bit division/remainder builtins
- Created runtime build system with automated Makefile
- Added compile-c.sh helper script for easy C program compilation
- Updated documentation to reflect complete 64-bit integer support"
```

Push when ready: `git push origin main`

## Notes
- LLVM backend changes live in `~/llvm-project` and are mirrored in `llvm-backend/`
- Release is backward compatible; existing code paths continue to compile and run
- Keep `CLAUDE.md` in sync with public docs after publishing
