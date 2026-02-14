# SLOW-32 Optimization Roadmap

## Immediate Wins (Low Risk, High Impact)

### 1. Speed Optimization: Predecode + Dispatch Table ✓

- Pre-decode all instructions at load time into flat array
- Store decoded fields AND function pointer per instruction
- Eliminates ALL decode overhead from hot path
- Expected speedup: 2-3x over switch-based decode
- Status: IMPLEMENTED (slow32-predecoded.c)

### 2. Minimal Linker (`ld-lite`)
Support just two relocations for LUI/ORI pairs:

- `R_ABS_HI20(sym)`: Upper 20 bits for LUI
- `R_ABS_LO12(sym)`: Lower 12 bits for ORI
- Enables multi-file builds without complexity
- Simple format: concatenate .text + .data, patch relocations

### 3. Basic Dev Tools

- `nm-lite`: Print symbol table
- `objdump-lite`: Disassemble using predecode structs
- Reuse existing decoder infrastructure

## Host/Guest Communication

### 4. YIELD Enhancement
```asm
YIELD r1  # Reason code in r1
          # 1 = read from host
          # 2 = write to host  
          # 3 = sleep/wait
          # 4 = debug break
          # Args in r2-r5, result in r1
```

### 5. Memory-Mapped Ring Buffers
Fixed region at 0x20000000:
```c
struct RingBuffer {
    uint32_t head;     // Host updates
    uint32_t tail;     // Guest updates
    uint32_t size;
    uint8_t data[];
};
```

- RX ring: host → guest (stdin, network)
- TX ring: guest → host (stdout, network)

### 6. FENCE Instruction

- No-op in emulator (already ordered)
- Semantic memory barrier for documentation
- Marks synchronization points

## Compiler Enhancements

### 7. Essential Intrinsics
Already implemented in assembly (runtime/intrinsics.s):

- ✓ `llvm.memcpy.*`
- ✓ `llvm.memset.*`
- ✓ `llvm.lifetime.*`
- TODO: `llvm.bswap.i32` (4 shifts + masks)

### 8. Compiler Intrinsic Lowering
Instead of calling assembly functions, emit inline code:

- memcpy → word/byte copy loop
- memset → word/byte set loop
- Eliminates function call overhead

## Timing & Determinism

### 9. RDTIME Instruction
```asm
RDTIME rd  # Returns monotonic cycle count
           # Low 32 bits in rd
           # High 32 bits in rd+1 (or MMIO)
```

- Deterministic (cycle count, not wall clock)
- Useful for benchmarking and delays

## Future Enhancements

### 10. Green Threads (Cooperative)
```c
struct VCPU {
    uint32_t regs[32];
    uint32_t pc;
    // Each VCPU is a separate context
};
```

- Host scheduler multiplexes VCPUs
- `YIELD(reason=switch, r2=tid)`
- No interrupt controller needed initially

### 11. Basic Block Linking

- Cache pointers between decoded blocks
- Skip dispatch at block boundaries
- Further reduces overhead

### 12. Superinstructions
Combine common patterns in predecode:

- `addi + beq` → single operation
- `load + use` → fused load-op
- Pattern match during predecode phase

### 13. Full Interrupt Support (Later)
For running Xinu or other RTOS:

- Timer interrupt source
- Simple interrupt controller
- Trap/exception vectors
- Context save/restore

## Design Principles to Maintain

1. **W^X Protection**: Enables permanent predecode
2. **Reserved Registers**: r28=temp, r29=sp, r30=fp, r31=lr
3. **Branch Semantics**: PC+4 relative (B-format), PC relative (J-format)
4. **Determinism First**: Cycle-accurate by default
5. **Simple Before Complex**: Prove concepts with minimal implementation

## This Week's Milestones

- [x] Predecode table implementation
- [x] Function pointer dispatch
- [ ] Basic linker with HI20/LO12 relocs
- [ ] YIELD reason codes
- [ ] TX ring for putchar
- [ ] RDTIME instruction
- [ ] Two-file linking demo

## Linker Format Proposal

Simple binary format:
```
Header {
    uint32_t magic;
    uint32_t text_size;
    uint32_t data_size; 
    uint32_t num_symbols;
    uint32_t num_relocs;
}
[text bytes]
[data bytes]
[Symbol { char name[32]; uint32_t section; uint32_t value; }]
[Reloc { uint32_t section; uint32_t offset; uint32_t type; uint32_t symbol; }]
```

Keep it flat and simple - can upgrade later as needed.
