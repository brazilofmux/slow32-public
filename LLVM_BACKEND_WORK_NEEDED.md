# LLVM Backend Work Required

## Overview
This directory has been synchronized from `~/slow-32` and contains the latest SLOW32 toolchain changes including:
- 64-bit time support (`time_t` = `uint64_t`)
- `nanosleep()`, `usleep()`, `sleep()` MMIO syscalls
- `stat()`/`fstat()` MMIO syscalls
- Enhanced printf with double-dabble BCD converter (workaround for 64-bit div bug)

## Critical Issue: 64-bit Division/Modulo Bug

**See: `docs/issues/llvm-i64-division.md` for full details**

### Problem
The SLOW32 LLVM backend miscompiles `uint64_t / constant` and `uint64_t % constant` operations. Instead of emitting libcalls to `__udivdi3`/`__umoddi3`, the optimizer replaces them with multiply-by-magic-constant sequences that assume working 64-bit multiply-high. Our current `UMUL_LOHI` lowering produces incorrect results.

### Current Workarounds
1. `runtime/builtins.c` marks `__udivdi3`/`__umoddi3` with `__attribute__((optnone))` to prevent inlining
2. `runtime/printf_enhanced.c` uses double-dabble BCD conversion instead of `/` and `%` for `%llu` formatting

### Backend Location
The LLVM backend code is at:
- `~/llvm-project/llvm/lib/Target/SLOW32/` (in the main LLVM tree)
- This directory contains a copy at `llvm-backend/SLOW32/` (for reference/testing)

### Key Files to Investigate
- `~/llvm-project/llvm/lib/Target/SLOW32/SLOW32ISelLowering.cpp` - Main lowering logic
  - Line ~1700: `UMUL_LOHI` custom lowering (currently splits into 16×16 products)
  - Constructor: `setOperationAction(ISD::UDIV, MVT::i64, Expand)`
- `~/llvm-project/llvm/lib/Target/SLOW32/SLOW32TargetMachine.cpp` - Target hooks

### Proposed Fixes (choose one approach)

#### Option A: Force Libcalls for Constant Division
Override target hooks to prevent `DivRemByConstant` optimization:
```cpp
// In SLOW32TargetLowering constructor or relevant method
// Prevent strength reduction of i64 division by constants
setTargetDAGCombine(ISD::UDIV);
setTargetDAGCombine(ISD::UREM);
setTargetDAGCombine(ISD::SDIV);
setTargetDAGCombine(ISD::SREM);
```

Then implement `PerformDAGCombine` to block constant division transforms and force libcalls.

#### Option B: Fix UMUL_LOHI Lowering
Fix the existing multiply-high implementation to correctly handle all carries when splitting 64×64 → 128-bit multiplies. This is mathematically complex but would allow the optimizer to work normally.

### Testing
After making changes:
```bash
cd ~/slow-32/regression
./run-tests.sh
```

Key regression tests:
- `feature-i64-division` - Tests `__udivdi3`/`__umoddi3` calls
- `feature-i64-complete` - Comprehensive 64-bit operations
- `feature-printf` - Verifies printf still works

### Reproduction Test
```bash
cd ~/slow-32
cat > test_i64_div.c << 'EOF'
#include <stdint.h>
#include <stdio.h>

int main(void) {
    uint64_t value = 18446744073709551615ull;
    printf("Testing: value / 10 = %llu\n", value / 10);
    printf("Testing: value %% 10 = %llu\n", value % 10);
    return 0;
}
EOF

# Compile with LLVM backend
~/llvm-project/build/bin/clang -target slow32-unknown-none -S -emit-llvm -O2 \
    -Iruntime/include test_i64_div.c -o test_i64_div.ll
~/llvm-project/build/bin/llc -mtriple=slow32-unknown-none test_i64_div.ll -o test_i64_div.s

# Check if __udivdi3 appears in assembly
grep -c __udivdi3 test_i64_div.s
# Should be > 0, currently is 0 (bug!)

# Link and run
./tools/assembler/slow32asm test_i64_div.s test_i64_div.s32o
./tools/linker/s32-ld -o test_i64_div.s32x runtime/crt0.s32o test_i64_div.s32o \
    runtime/libs32.s32a runtime/libc_mmio.s32a
./tools/emulator/slow32 test_i64_div.s32x
```

Expected output:
```
Testing: value / 10 = 1844674407370955161
Testing: value % 10 = 5
```

Current (broken) output will show wrong numbers.

### Success Criteria
1. The test above produces correct output
2. All regression tests pass
3. `runtime/printf_enhanced.c` can be simplified back to using `/` and `%` for `%llu`
4. Remove `optnone` attributes from `runtime/builtins.c`

### Additional Context
- SLOW32 is a 32-bit RISC architecture with 32 registers
- It has native 32-bit multiply (`MUL`) producing lower 32 bits
- No native multiply-high instruction (must be synthesized)
- The backend implements `ISD::UMUL_LOHI` by splitting into four 16×16 → 32-bit products
- This lowering is used by both explicit 64-bit multiplies AND by the optimizer's division-by-constant expansion

### Contact
See `CLAUDE.md` for project overview and build instructions.
See `docs/issues/llvm-i64-division.md` for detailed bug analysis.
