# Known Limitations

This document lists current limitations of the SLOW-32 architecture and toolchain, along with workarounds where available.

## Architecture Limitations

### No Hardware Floating Point
**Impact:** All floating-point operations must be emulated in software.
**Workaround:** Use integer arithmetic or link soft-float library (planned).
**Status:** Floating point may be added in future versions.

### No Condition Codes/Flags
**Impact:** No carry flag for multi-precision arithmetic.
**Workaround:** Explicit comparison for carry/borrow detection.
```asm
# 64-bit addition with carry
add r3, r5, r7      # Low 32 bits
sltu r4, r3, r5     # Carry = (sum < operand)
add r4, r6, r8      # High 32 bits
add r4, r4, r4      # Add carry
```

### Single-Threaded Only
**Impact:** No atomic operations or synchronization primitives.
**Workaround:** Not applicable for concurrent code.
**Status:** May add basic atomics in future.

### Limited I/O
**Impact:** Only DEBUG instruction for output, no input mechanism.
**Workaround:** Use DEBUG for debugging output.
**Status:** MMIO and TRAP interfaces planned.

## Toolchain Limitations

### Compiler Issues

#### Register Exhaustion at -O0
**Impact:** Unoptimized code fails to compile.
**Workaround:** Always use -O1 or higher.
```bash
# Don't do this
clang --target slow32-unknown-none -O0 program.c  # FAILS

# Do this instead
clang --target slow32-unknown-none -O1 program.c  # Works
```

#### Limited 64-bit Support
**Impact:** 64-bit multiply/divide not fully implemented.
**Workaround:** Use 32-bit arithmetic or wait for libgcc port.
**Status:** ADD/SUB working, MUL/DIV in progress.

#### Struct Passing
**Impact:** Large structs passed by value may fail.
**Workaround:** Pass structs by pointer.
```c
// Problematic
void func(struct large_struct s);

// Better
void func(struct large_struct *s);
```

### Assembler Limitations

#### No Macro Support (Advanced)
**Impact:** Limited macro capabilities compared to GNU as.
**Workaround:** Use simple macros or preprocess separately.

#### Limited Directive Support
**Impact:** Some GNU assembler directives not supported.
**Supported:** `.text`, `.data`, `.rodata`, `.bss`, `.globl`, `.section`, `.word`, `.byte`, `.ascii`, `.asciz`
**Not Supported:** `.macro`, `.if`, `.include` (advanced features)

### Linker Limitations

#### No Dynamic Linking
**Impact:** All programs must be statically linked.
**Workaround:** Not applicable - static only.

#### Simple Relocations Only
**Impact:** Complex relocation types not supported.
**Supported:** R_SLOW32_ABS32, R_SLOW32_PC32, R_SLOW32_HI20, R_SLOW32_LO12

#### No Link-Time Optimization
**Impact:** No cross-file optimization.
**Workaround:** Use compiler LTO before assembly.

### Emulator Limitations

#### No System Calls
**Impact:** Cannot interact with host OS directly.
**Workaround:** Use DEBUG instruction for output.
**Status:** TRAP interface planned.

#### Fixed Memory Size
**Impact:** Programs limited to ~256MB total memory.
**Workaround:** Design within memory constraints.

#### No Memory Protection
**Impact:** Programs can corrupt their own memory.
**Workaround:** Careful programming, use debug mode.

## Language Support Limitations

### C++ Support
**Status:** Not tested, likely broken.
**Issues:**
- No exception handling
- No RTTI
- No standard library

### Other Languages
**Rust:** Theoretical support via LLVM, untested.
**Go:** Not supported (needs runtime).
**Python/JavaScript:** Not applicable (need interpreter).

## Performance Limitations

### No Instruction Cache Modeling
**Impact:** Performance characteristics differ from real hardware.
**Note:** This is by design - SLOW-32 prioritizes simplicity.

### Fixed Instruction Timing
**Impact:** Unrealistic for modern hardware.
- Memory: 3 cycles (always)
- Multiply: 32 cycles (always)
- Divide: 64 cycles (always)

### No Pipelining
**Impact:** No instruction-level parallelism.
**Note:** Intentional design choice for simplicity.

## Debug Limitations

### No Source-Level Debugging
**Impact:** Cannot use GDB or similar debuggers.
**Workaround:** Use emulator's trace mode.
```bash
slow32 -t program.s32x  # Trace execution
slow32 -s program.s32x  # Step mode
```

### Limited Symbol Information
**Impact:** Debugging shows addresses, not function names.
**Workaround:** Keep assembly listing for reference.

## Memory Model Limitations

### Fixed Memory Layout
**Impact:** Cannot change code/data/stack locations.
```
Code:  0x00000000 - 0x000FFFFF (1MB)
Data:  0x00100000 - 0x0FFFFFFF (255MB)
Stack: 0x0FFFFFF0 (grows down)
```
**Workaround:** Design within these constraints.

### No Memory-Mapped I/O (Yet)
**Impact:** Cannot implement device drivers.
**Status:** MMIO support planned.

## Workaround Summary

### For C Programming

```c
// Use these patterns for best results

// 1. Always initialize the stack properly
// (handled by crt0.s32o)

// 2. Keep functions small
void large_function() {
    // Split into smaller functions
    part1();
    part2();
}

// 3. Minimize live variables
// Instead of:
int a = calc1();
int b = calc2();
int c = calc3();
use(a, b, c);

// Do:
use(calc1(), calc2(), calc3());

// 4. Use word-aligned data
struct data {
    int field1;    // offset 0
    int field2;    // offset 4
} __attribute__((aligned(4)));

// 5. Pass large data by reference
void process(const struct large *data);
```

### For Assembly Programming

```asm
# Essential patterns

# 1. Function prologue/epilogue
func:
    addi sp, sp, -16
    stw r30, sp+12
    stw r31, sp+8
    # ... function body ...
    ldw r30, sp+12
    ldw r31, sp+8
    addi sp, sp, 16
    jalr r0, r31, 0

# 2. 32-bit constant loading
    lui r3, %hi(0xDEADBEEF)
    addi r3, r3, %lo(0xDEADBEEF)

# 3. Conditional execution
    slt r2, r3, r4
    beq r2, r0, skip
    # ... conditional code ...
skip:
```

## Future Improvements

### High Priority
1. Complete 64-bit integer support
2. Basic I/O beyond DEBUG
3. Soft-float library
4. Better error messages

### Medium Priority
1. MMIO support
2. TRAP system calls
3. Source-level debugging info
4. Link-time optimization

### Low Priority
1. Floating-point hardware
2. SIMD instructions
3. Multi-threading support
4. Dynamic linking

## Getting Help

If you encounter limitations not listed here:
1. Check the [FAQ](92-faq.md)
2. Review [Troubleshooting](93-troubleshooting.md)
3. See examples for workaround patterns
4. Consult implementation notes in source