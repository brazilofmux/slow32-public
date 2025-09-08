# CLAUDE.md - Quick Reference for AI Assistants

SLOW-32 is a 32-bit RISC CPU with complete toolchain. This file contains quick commands and context for AI assistance.

## Quick Commands

```bash
# Build everything
make

# Compile and run C program (BEST method - native SLOW32 target)
~/llvm-project/build/bin/clang --target slow32-unknown-none -S -emit-llvm -O1 program.c -o program.ll
~/llvm-project/build/bin/llc -mtriple=slow32-unknown-none program.ll -o program.s
./assembler/slow32asm program.s program.s32o
./linker/s32-ld -o program.s32x runtime/crt0.s32o program.s32o runtime/intrinsics.s32o
# For 64-bit operations, also link runtime/builtins.s32o
./emulator/slow32 program.s32x

# Alternative: Use standalone compiler (legacy)
clang-19 -S -emit-llvm -O1 program.c -o program.ll
./llvm-backend/standalone/slow32-compile program.ll > program.s
./assembler/slow32asm program.s program.s32o
./linker/s32-ld -o program.s32x runtime/crt0.s32o program.s32o runtime/intrinsics.s32o
./emulator/slow32 program.s32x

# Debug mode
./emulator/slow32 program.s32x -s  # step through each instruction
./emulator/slow32 -t program.s32x  # trace every instruction
./emulator/slow32 -r program.s32x  # show register changes
./emulator/slow32 -c 1000 program.s32x  # limit to 1000 cycles
./emulator/slow32 -b 0x100 program.s32x  # break at address
./emulator/slow32 -w 0x1000-0x2000 program.s32x  # watch memory range
```

## Key Architecture Points

- 32 registers: r0=0, r1-r2=return, r3-r10=args, r29=sp, r30=fp, r31=lr
- No condition codes - comparisons return 0/1 in registers
- W^X protection - code segment is execute-only (0x0-0xFFFFF)
- Stack at 0x0FFFFFF0 growing down
- DEBUG instruction outputs character (only I/O mechanism)

## Working Features

✅ Complete toolchain with proper linker (not concatenation!)  
✅ Object files (.s32o) with relocations and symbol tables  
✅ Executable format (.s32x) with resolved symbols  
✅ All comparison instructions (SEQ, SNE, SGT/U, SLE/U, etc.)  
✅ PHI node support for SSA form  
✅ LLVM intrinsics: memcpy, memset, lifetime, smax/smin  
✅ Varargs fully working with clang SLOW32 target!  
✅ Jump tables for switch statements (assembler, linker, LLVM backend all support)  
✅ 64-bit integers: FULLY COMPLETE including:
  - ADD/SUB with carry/borrow
  - MUL via UMUL_LOHI/SMUL_LOHI custom lowering
  - DIV/REM via compiler-rt libcalls (__divdi3, __udivdi3, __moddi3, __umoddi3)
  - All logical operations (AND, OR, XOR, shifts)
  - All comparison operations (EQ, NE, LT, GT, LE, GE for both signed/unsigned)
  - Comprehensive regression test coverage
✅ Native Clang target: `--target slow32-unknown-none`  
✅ XORI instruction (opcode 0x1E) for XOR immediate operations

⚠️ Requires -O1 or -O2 (unoptimized exhausts registers)  
⚠️ See docs/IMPROVEMENTS.md for known issues


## Important Notes for Toolchain

- **Always use the linker** - Never concatenate .s files!
- **crt0.s32o must be first** - Contains _start at address 0
- **Link order**: crt0.s32o, program.s32o, intrinsics.s32o
- **Use -O1 or -O2** - Unoptimized code runs out of registers

## Working C Examples

```c
// Printf with varargs
void test_printf() {
    printf("Hello, World!\n");
    printf("Number: %d\n", 42);
}

// Recursion
int factorial(int n) {
    if (n <= 1) return 1;
    return n * factorial(n - 1);
}

// Loops with PHI nodes
int sum_to_n(int n) {
    int sum = 0;
    for (int i = 1; i <= n; i++) {
        sum += i;
    }
    return sum;
}
```

## Testing Commands

```bash
# IMPORTANT: Run regression tests before committing any backend changes!
cd ~/slow-32/regression && ./run-tests.sh

# Run specific regression test
cd ~/slow-32/regression && ./run-tests.sh feature-arithmetic

# Analyze binaries
./tools/s32-objdump file.s32o   # Dump object file
./tools/s32-exedump file.s32x   # Dump executable
./tools/slow32dis file.s32x [start] [end]  # Disassembler

# Performance test
time ./emulator/slow32 program.s32x  # ~350M inst/sec
time ./emulator/slow32-fast program.s32x  # Optimized version
```

## Regression Testing

**CRITICAL:** Before committing any LLVM backend changes:
1. Run the regression suite: `cd ~/slow-32/regression && ./run-tests.sh`
2. Check that no previously passing tests now fail
3. Document any intentional behavior changes
4. Add tests for new features or bug fixes

The regression suite helps catch unintended breakage. See `~/slow-32/regression/README.md` for details.