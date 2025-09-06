# SLOW-32 Architecture Rationale

**Document:** SLOW32-AR-001  
**Revision:** 1.0.0  
**Date:** September 5, 2025  
**Author:** S. Dennis  

## Revision History

| Version | Date | Author | Changes |
|---------|------|--------|---------|
| 1.0.0 | 2025-09-05 | S. Dennis | Initial rationale document |
| - | Future | - | Add rationale for floating-point design |
| - | Future | - | Document SIMD decisions |

## Executive Summary

SLOW-32 is a 32-bit RISC architecture designed for simplicity, clarity, and ease of implementation. Every design decision prioritizes understanding over performance, making it ideal for education, compiler development, and emulation.

## Core Philosophy

### "The Emulator IS the OS"

Traditional CPUs carry decades of hardware baggage: protection rings, memory management units, interrupt controllers, and complex I/O subsystems. SLOW-32 delegates all of this to the host environment, treating the emulator as the operating system.

**Benefits:**
- No mode switching complexity
- No MMU means simple, fast address translation
- No interrupt handling in guest code
- Clean separation of concerns

**Trade-offs:**
- Cannot run on bare metal (by design)
- No true multitasking (intentional)
- Host-dependent I/O (flexibility over standardization)

## Design Decisions

### 1. No Condition Codes

**Decision:** No flags register (carry, overflow, zero, negative)

**Rationale:**
- Simplifies CPU state (just PC + 32 registers)
- Makes instruction semantics explicit
- Eliminates hidden dependencies between instructions
- Compiler can optimize comparisons independently

**Implementation:**
```asm
# Traditional (with flags):
add r1, r2, r3  # Sets flags
jc overflow     # Checks carry flag

# SLOW-32:
add r1, r2, r3
sltu r4, r1, r2  # Explicit carry detection
bne r4, r0, overflow
```

**Impact:** Slightly larger code for multi-precision arithmetic, but much clearer semantics.

### 2. Fixed 32-bit Instructions

**Decision:** All instructions are exactly 32 bits

**Rationale:**
- Trivial instruction fetch and decode
- No alignment issues
- Predictable code size
- Simple PC increment (always +4)

**Trade-off:** Less code density than variable-length encodings

### 3. Register-Rich Architecture

**Decision:** 32 general-purpose registers

**Rationale:**
- Reduces memory traffic
- Simplifies register allocation
- Enough for most functions without spilling
- Power of 2 simplifies encoding (5 bits per register)

**Register Allocation:**
- r0: Hardwired zero (simplifies many operations)
- r1: Return value (single location)
- r2: Temporary (scratch)
- r3-r10: Arguments (8 registers = most functions)
- r11-r28: Saved (18 registers = plenty)
- r29-r31: Special (sp, fp, lr)

### 4. Load/Store Architecture

**Decision:** Only load/store instructions access memory

**Rationale:**
- Uniform instruction timing
- Simple pipeline model (even if not pipelined)
- Clear separation of computation and memory access
- Easier to verify and debug

### 5. No Delay Slots

**Decision:** Branches take effect immediately

**Rationale:**
- Simpler mental model
- No confusing instruction reordering
- Easier for compilers
- Matches modern CPU behavior (even if they have delay slots internally)

### 6. Simple Addressing

**Decision:** Only base + offset addressing

**Rationale:**
- Covers 99% of use cases
- Simple to implement
- Fast to decode
- Sufficient for structs, arrays, and stack

**Not Included:**
- Scaled index (base + index * scale)
- Pre/post increment
- PC-relative data addressing

### 7. W^X Enforcement

**Decision:** Memory is either writable or executable, never both

**Rationale:**
- Security by design
- Eliminates self-modifying code complexity
- Enables host-side optimizations
- Clear mental model

**Implementation:**
- Code: 0x00000000-0x000FFFFF (execute-only)
- Data: 0x00100000-0x0FFFFFFF (read/write)
- Gap prevents accidental crossover

### 8. DEBUG Instruction

**Decision:** Special instruction for output during development

**Rationale:**
- Essential for debugging without full I/O
- Zero overhead when not used
- Simple to implement in any emulator
- Maps naturally to host stderr/stdout

**Future:** Will be supplemented (not replaced) by proper I/O

### 9. No Hardware Multiply/Divide

**Decision:** MUL/DIV are instructions but can be slow

**Rationale:**
- Keeps hardware model simple
- Allows implementation flexibility
- Software can optimize if needed
- Matches some embedded processors

**Current Timing:**
- MUL: 32 cycles
- DIV/REM: 64 cycles

## Comparisons with Other Architectures

### vs. RISC-V

**Similarities:**
- 32 registers with r0=0
- Load/store architecture
- Fixed instruction width (RV32I)

**Differences:**
- No compressed instructions (RVC)
- No CSR (control/status registers)
- Simpler instruction encoding
- No privilege modes

### vs. ARM

**Similarities:**
- Load/store architecture
- Rich register set

**Differences:**
- No conditional execution
- No Thumb mode
- No complex addressing modes
- Much simpler overall

### vs. x86

**Differences:** Everything
- Fixed vs. variable length
- 32 vs. 8 registers (in 32-bit mode)
- Load/store vs. register-memory
- No segments, no modes, no legacy

## Future Design Considerations

### Floating Point (Planned)

**Approach:** Separate register file (f0-f31)

**Rationale:**
- Keeps integer path simple
- Allows different implementations
- Can be fully software if needed

### 64-bit Operations (In Progress)

**Approach:** Register pairs for i64

**Rationale:**
- Maintains 32-bit focus
- Compiler handles complexity
- No mode switching

### SIMD (Under Consideration)

**Options:**
1. Packed operations in GPRs
2. Separate vector registers
3. Not supported (use loops)

**Likely:** Option 3, maintaining simplicity

### I/O Evolution

**Current:** DEBUG only
**Next:** MMIO regions
**Future:** TRAP instruction for system calls

Each stage maintains backward compatibility.

## What SLOW-32 Is NOT

1. **Not for performance** - Clarity over speed
2. **Not for production** - Education and experimentation
3. **Not for bare metal** - Requires host environment
4. **Not for complex OS** - Single address space
5. **Not backward compatible** - No legacy burden

## Success Metrics

SLOW-32 succeeds if:
1. A student can understand the entire ISA in one day
2. A compiler backend can be written in under 5000 lines
3. An emulator can be written in a weekend
4. The specification fits in 50 pages
5. No instruction requires a paragraph to explain

## Conclusion

SLOW-32 represents a return to simplicity in CPU design. By explicitly choosing education over performance, clarity over density, and implementation ease over features, it provides a platform for learning and experimentation without the complexity that burdens production architectures.

The architecture will evolve, but always with these principles:
- If it needs more than a sentence to explain, it's too complex
- If the emulator needs more than 1000 lines, reconsider
- If the compiler can't handle it simply, users can't either
- When in doubt, leave it out

## References

- Patterson & Hennessy: "Computer Organization and Design"
- RISC-V Specifications: https://riscv.org/specifications/
- ARM Architecture Reference Manual
- "The Case for RISC" - David Patterson, 1985