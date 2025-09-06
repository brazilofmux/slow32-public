# SLOW-32 LLVM Backend

This document describes the LLVM backend for SLOW-32, enabling compilation from C/C++ and other LLVM-supported languages.

## Overview

The SLOW-32 backend is implemented as an experimental LLVM target, providing:
- Full code generation from LLVM IR
- Native Clang support via `--target slow32-unknown-none`
- Optimization passes tailored for SLOW-32
- Integration with standard LLVM tools

## Building the Backend

### Prerequisites
- LLVM source code (tested with LLVM 22.0.0git/main branch)
- CMake 3.20+
- C++17 compiler
- ~16GB RAM for full build
- ~50GB disk space

**Note:** The SLOW32 backend was developed against LLVM's main branch (22.0.0git). While it should work with LLVM 19.0+, it's actively tested on bleeding-edge LLVM.

### Build Steps

```bash
# Clone standard LLVM (no SLOW32 yet)
git clone https://github.com/llvm/llvm-project.git ~/llvm-project
cd ~/llvm-project

# Clone this repository to get SLOW32 backend
git clone https://github.com/brazilofmux/slow32-public.git ~/slow32-public

# Copy SLOW32 backend to LLVM
cp -r ~/slow32-public/llvm-backend/SLOW32 llvm/lib/Target/

# Apply integration patches
cd ~/llvm-project
patch -p1 < ~/slow32-public/llvm-backend/patches/llvm-integration.patch
patch -p1 < ~/slow32-public/llvm-backend/patches/clang-integration.patch

# Copy Clang support files
cp ~/slow32-public/llvm-backend/clang-support/SLOW32.h \
   clang/lib/Basic/Targets/
cp ~/slow32-public/llvm-backend/clang-support/SLOW32.cpp \
   clang/lib/Basic/Targets/

# Create build directory
mkdir build && cd build

# Configure with SLOW-32 as experimental target
cmake -S ../llvm -B . -G "Unix Makefiles" \
  -DCMAKE_BUILD_TYPE=Release \
  -DLLVM_TARGETS_TO_BUILD="X86;RISCV" \
  -DLLVM_EXPERIMENTAL_TARGETS_TO_BUILD="SLOW32" \
  -DLLVM_ENABLE_PROJECTS="clang"

# Build (parallel build recommended)
make -j$(nproc) llc clang

# Verify installation
./bin/llc -version | grep -i slow32
./bin/clang --print-targets | grep -i slow32
```

## Using the Backend

### Direct Clang Compilation

```bash
# Compile C to SLOW-32 assembly
clang --target slow32-unknown-none -S -O1 program.c -o program.s

# Compile to LLVM IR first (for inspection)
clang --target slow32-unknown-none -S -emit-llvm -O1 program.c -o program.ll
llc -mtriple=slow32-unknown-none program.ll -o program.s
```

### Compiler Flags

**Essential Flags:**
- `--target slow32-unknown-none` - Select SLOW-32 target
- `-O1` or `-O2` - Required (unoptimized exhausts registers)
- `-ffreestanding` - No standard library
- `-nostdlibinc` - No system headers

**Optimization Levels:**
- `-O0` - DO NOT USE (register exhaustion)
- `-O1` - Recommended minimum
- `-O2` - Good performance
- `-O3` - May increase code size significantly
- `-Os` - Optimize for size

### Data Layout

The SLOW-32 target uses this data layout string:
```
e-m:e-p:32:32-i32:32-i64:32-f32:32-f64:32-v64:32-v128:32-a:0:32-n32-S32
```

Meaning:
- Little-endian (`e`)
- 32-bit pointers
- 32-bit integers (default)
- 32-bit alignment for most types
- Native integer width is 32 bits

## Backend Implementation

### Key Files

Location: `llvm/lib/Target/SLOW32/`

```
SLOW32/
├── SLOW32.td                 # Target description
├── SLOW32RegisterInfo.td     # Register definitions
├── SLOW32InstrInfo.td        # Instruction definitions
├── SLOW32CallingConv.td      # Calling conventions
├── SLOW32ISelLowering.cpp    # Custom lowering
├── SLOW32ISelDAGToDAG.cpp    # Instruction selection
├── SLOW32AsmPrinter.cpp      # Assembly generation
└── SLOW32TargetMachine.cpp   # Target configuration
```

### Register Allocation

32 general-purpose registers:
- `r0` - Hardwired zero
- `r1` - Return value
- `r2` - Temporary
- `r3-r10` - Arguments
- `r11-r28` - Callee-saved
- `r29` - Stack pointer
- `r30` - Frame pointer
- `r31` - Link register

### Calling Convention

**C Calling Convention:**
```llvm
; Function arguments
; First 8 arguments in r3-r10
; Additional arguments on stack

; Return values
; Single value in r1
; 64-bit values in r1:r2 (low:high)

; Preserved registers
; Callee must save: r11-r28, r30
; Caller saves: r2-r10
```

### Instruction Selection

The backend handles these patterns:

**Arithmetic:**
- 32-bit: Direct mapping to SLOW-32 instructions
- 64-bit: Expanded to multiple instructions with carry

**Memory Access:**
- Byte/halfword/word loads and stores
- Address computation: base + offset only
- No complex addressing modes

**Control Flow:**
- Compare + branch pattern
- No condition codes
- Jump tables for switch statements

**Constants:**
- Small immediates: Single ADDI
- Large constants: LUI + ADDI (RISC-V style)

## Supported Features

### Working Features ✅

- Basic arithmetic (ADD, SUB, MUL, DIV, REM)
- Logical operations (AND, OR, XOR, shifts)
- All comparison operations
- Function calls and returns
- Stack frame management
- Global variables
- Arrays and pointers
- Switch statements (jump tables)
- Varargs functions
- Inline assembly (basic)
- 64-bit integers (ADD/SUB with carry)

### Partial Support ⚠️

- 64-bit multiply/divide (via libcalls)
- Struct passing (small structs only)
- Bit fields (some patterns)
- Atomics (no hardware support)

### Not Supported ❌

- Floating point (no hardware)
- SIMD/vectors
- Exceptions (C++)
- Thread-local storage
- Dynamic linking

## Intrinsics and Builtins

### Memory Operations

```c
// These expand to optimized sequences
memcpy(dest, src, n);  // Inline for n <= 32
memset(dest, val, n);  // Inline for n <= 32
```

### Debug Output

```c
// Direct DEBUG instruction
__builtin_slow32_debug(char c);

// Or inline assembly
asm("debug %0" : : "r"(c));
```

## Optimization Notes

### Register Pressure

SLOW-32 has limited registers. Help the compiler:

```c
// Bad: Many live variables
int a = x + y;
int b = x * y;
int c = x - y;
int d = x / y;
use(a, b, c, d);

// Better: Reduce live ranges
use(x + y, x * y, x - y, x / y);
```

### Memory Access

```c
// Prefer word-aligned access
struct aligned {
    int a;      // offset 0
    int b;      // offset 4
} __attribute__((packed));

// Avoid byte-wise access when possible
int sum = arr[i];  // Good: word load
char c = str[i];   // Okay but slower
```

### Loop Optimization

```c
// Backend recognizes these patterns
for (int i = 0; i < n; i++)     // Counted loop
while (p != end)                 // Pointer loop
do { ... } while (--n);          // Countdown loop
```

## Debugging Backend Issues

### View LLVM IR

```bash
clang --target slow32-unknown-none -S -emit-llvm -O1 test.c
cat test.ll
```

### View Selection DAG

```bash
llc -mtriple=slow32-unknown-none -view-dag-combine1-dags test.ll
```

### Debug Output

```bash
llc -mtriple=slow32-unknown-none -debug test.ll 2>&1 | less
```

### Common Issues

**Issue:** "Cannot select" error
**Cause:** Unsupported LLVM IR pattern
**Fix:** Simplify code or add custom lowering

**Issue:** Register allocation failed
**Cause:** Too many live variables
**Fix:** Use -O1 or higher, simplify expressions

**Issue:** Wrong calling convention
**Cause:** Mismatch between C and assembly
**Fix:** Check CallingConv.td definitions

## Testing the Backend

### Run LLVM Tests

```bash
cd ~/llvm-project/build
make check-llvm-codegen-slow32
```

### Regression Tests

```bash
cd ~/slow-32/regression
./run-tests.sh
```

### Add New Tests

Create `test.ll` with:
```llvm
; RUN: llc -mtriple=slow32-unknown-none < %s | FileCheck %s

define i32 @add(i32 %a, i32 %b) {
  ; CHECK-LABEL: add:
  ; CHECK: add r1, r3, r4
  %sum = add i32 %a, %b
  ret i32 %sum
}
```

## Contributing

### Adding Instructions

1. Define in `SLOW32InstrInfo.td`
2. Add selection pattern
3. Update `SLOW32AsmPrinter.cpp`
4. Add tests
5. Update documentation

### Fixing Bugs

1. Reproduce with minimal test case
2. Check if it's lowering or selection
3. Fix and add regression test
4. Run full test suite

## References

- [LLVM Backend Tutorial](https://llvm.org/docs/WritingAnLLVMBackend.html)
- [TableGen Documentation](https://llvm.org/docs/TableGen/)
- [LLVM Language Reference](https://llvm.org/docs/LangRef.html)
- SLOW-32 ISA documentation