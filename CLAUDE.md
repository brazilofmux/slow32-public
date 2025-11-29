# CLAUDE.md - Quick Reference for AI Assistants

SLOW-32 is a 32-bit RISC CPU with complete toolchain. This file contains quick commands and context for AI assistance.

## Quick Commands

```bash
# Build everything
make

# Quick test of toolchain
./scripts/test-quick.sh

# Compile C program with helper script
./scripts/compile.sh program.c [output.s32x]

# Compile and run C program (manual method - native SLOW32 target)
~/llvm-project/build/bin/clang -target slow32-unknown-none -S -emit-llvm -O2 -Iruntime/include program.c -o program.ll
~/llvm-project/build/bin/llc -mtriple=slow32-unknown-none program.ll -o program.s
./tools/assembler/slow32asm program.s program.s32o
./tools/linker/s32-ld -o program.s32x runtime/crt0.s32o program.s32o runtime/libs32.s32a runtime/libc.s32a
./tools/emulator/slow32 program.s32x

# Alternative: Standalone compiler (DEPRECATED - DO NOT USE)
# The standalone compiler in compiler/ is deprecated and should not be used.
# It cannot properly handle varargs and other complex C features.
# Always use the native LLVM backend method above.

# Debug mode
./tools/emulator/slow32 program.s32x -s  # step through each instruction
./tools/emulator/slow32 -t program.s32x  # trace every instruction
./tools/emulator/slow32 -r program.s32x  # show register changes
./tools/emulator/slow32 -c 1000 program.s32x  # limit to 1000 cycles
./tools/emulator/slow32 -b 0x100 program.s32x  # break at address
./tools/emulator/slow32 -w 0x1000-0x2000 program.s32x  # watch memory range
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
✅ Advanced relocations: %hi(symbol+offset), %lo(symbol+offset), %pcrel_hi, %pcrel_lo  
✅ 64-bit integers: FULLY COMPLETE including:
  - ADD/SUB with carry/borrow
  - MUL via UMUL_LOHI/SMUL_LOHI custom lowering
  - DIV/REM via compiler-rt libcalls (__divdi3, __udivdi3, __moddi3, __umoddi3)
  - All logical operations (AND, OR, XOR, shifts)
  - All comparison operations (EQ, NE, LT, GT, LE, GE for both signed/unsigned)
  - Comprehensive regression test coverage
✅ Native Clang target: `-target slow32-unknown-none` (note: single dash)  
✅ XORI instruction (opcode 0x1E) for XOR immediate operations
✅ LLVM backend updated for latest LLVM API (Sep 2025)
✅ Regression tests: ALL 14/14 PASSING! (Fixed MMIO bug, stack args offset)
✅ Runtime libraries built as archives: libs32.s32a (5.3KB), libc.s32a (14KB)
✅ Optimization passes fixed - no more LLC hangs
✅ MMIO support: Emulators use MMIO base from executable header, linker provides __mmio_base symbol
✅ Fixed emulator MMIO bug: no longer treats address 0x0-0xFFFF as MMIO when mmio_base=0
✅ Fixed LLVM backend stack argument bug: arguments 9+ now correctly accessed at fp+0, fp+4, etc.

✅ Runtime builds successfully with -O0, -O1, and -O2
✅ Some complex files (printf.c, stdio.c, etc.) use -O1 to avoid optimizer performance issues
✅ QEMU TCG backend with file I/O MMIO support
✅ Ragel state machine compiler compatible (goto-driven parsers work)

⚠️ See docs/IMPROVEMENTS.md for known issues


## Important Notes for Toolchain

- **Always use the linker** - Never concatenate .s files!
- **crt0.s32o must be explicitly linked first** - Contains _start at address 0 (not included in archives)
- **Link order**: crt0.s32o, program.s32o, libs32.s32a, libc.s32a
- **Use -O2** - Default optimization level, -O1 also works
- **Archives available**: libs32.s32a (runtime intrinsics + 64-bit builtins), libc.s32a (standard C library)
- **MMIO support**: Use `--mmio SIZE` flag with linker (e.g., `--mmio 64K`), access via __mmio_base symbol

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

## Docker Support

Docker containers provide a clean baseline for testing. Two containers available:
- `slow32-toolchain`: Full development environment with LLVM 22
- `slow32-emulator`: Lightweight runtime for testing executables (includes slow32, slow32-fast, qemu-system-slow32)

```bash
# Build Docker images (if not already built)
docker build -t slow32-toolchain -f Dockerfile.toolchain .
docker build -t slow32-emulator -f Dockerfile.emulator .

# Run programs with the emulator container (easy wrapper script)
./scripts/run-in-docker.sh program.s32x              # Use default slow32 emulator
./scripts/run-in-docker.sh --fast program.s32x      # Use optimized slow32-fast
./scripts/run-in-docker.sh --qemu program.s32x      # Use QEMU TCG emulator
./scripts/run-in-docker.sh -t program.s32x          # Trace mode (C++ emulators only)
./scripts/run-in-docker.sh --help                   # Show all options

# Manual Docker commands (if you prefer)
docker run --rm -v $(pwd):/data slow32-emulator s32run program.s32x
docker run --rm -v $(pwd):/data slow32-emulator s32run --fast program.s32x
docker run --rm -v $(pwd):/data slow32-emulator s32run --qemu program.s32x

# Test against clean baseline when unsure if changes broke something
docker run --rm -v $(pwd):/workspace slow32-toolchain bash -c "cd /workspace && make"

# Run tests in clean environment
docker run --rm -v $(pwd):/workspace slow32-toolchain bash -c "cd /workspace/regression && ./run-tests.sh"

# Test specific program compilation (uses clang/llc from /usr/local/bin in container)
docker run --rm -v $(pwd):/workspace slow32-toolchain bash -c "cd /workspace && clang -target slow32-unknown-none -S -emit-llvm -O2 -Iruntime/include test.c -o test.ll && llc -mtriple=slow32-unknown-none test.ll -o test.s"
```

## Benchmarks

Performance on a Ragel-generated CSV validator processing 15MB of data (285M instructions):

| Emulator | Time | Performance | Notes |
|----------|------|-------------|-------|
| slow32 (reference) | 5.99s | 47.6 MIPS | Portable C++ interpreter |
| slow32-fast | 0.89s | 318.6 MIPS | Optimized interpreter |
| QEMU TCG | 0.30s | ~950 MIPS | JIT compilation |

The 25KB executable processes CSV at 52 MB/s on QEMU. See `examples/validatecsv_ragel.c` for the benchmark.

## Testing Commands

```bash
# IMPORTANT: Run regression tests before committing any backend changes!
cd ~/slow-32/regression && ./run-tests.sh

# Run specific regression test
cd ~/slow-32/regression && ./run-tests.sh feature-arithmetic

# Analyze binaries
./tools/utilities/s32-objdump file.s32o   # Dump object file
./tools/utilities/s32-exedump file.s32x   # Dump executable
./tools/utilities/slow32dis file.s32x [start] [end]  # Disassembler

# Performance test
time ./tools/emulator/slow32 program.s32x  # ~48 MIPS
time ./tools/emulator/slow32-fast program.s32x  # ~319 MIPS
```

## Regression Testing

**CRITICAL:** Before committing any LLVM backend changes:
1. Run the regression suite: `cd ~/slow-32/regression && ./run-tests.sh`
2. Check that no previously passing tests now fail
3. Document any intentional behavior changes
4. Add tests for new features or bug fixes

The regression suite helps catch unintended breakage. See `~/slow-32/regression/README.md` for details.