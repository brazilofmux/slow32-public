# SLOW-32 Coprocessor Model Design

## Philosophy

SLOW-32 operates as a **coprocessor** that processes work from shared memory, not a traditional CPU with I/O devices. This eliminates the need for complex MMIO, interrupts, or I/O instructions.

## Core Concepts

### 1. Shared Memory Model

The host process and SLOW-32 emulator share the same memory space. No copying, no synchronization protocols at the CPU level - just shared RAM.

### 2. YIELD Instruction

```asm
YIELD rs  # Pause execution, return control to host
```

- Preserves all CPU state (registers, PC, memory)
- Host can inspect/modify memory while CPU is yielded
- CPU resumes exactly where it left off
- NO protocol knowledge in the CPU - just "pause/resume"

### 3. Memory Ordering Instructions

For concurrent host access (multiple host threads), we add minimal ordering primitives:

```asm
FLDW rd, rs+offset   # Fenced Load Word (acquire semantics)
FSTW rs+offset, rd   # Fenced Store Word (release semantics)
```

These map directly to host atomic operations, ensuring memory consistency across CPU cores.

## Ring Buffer Convention (NOT CPU Architecture)

This is purely a software convention between host and SLOW-32 programs:

```c
// Example layout at agreed address (e.g., 0x00200000)
struct RingBuffer {
    uint32_t head;      // Written by producer
    uint32_t tail;      // Written by consumer  
    uint32_t size;      // Fixed at initialization
    uint32_t data[];    // The actual ring
};
```

### Producer Pattern (Host or SLOW-32)
```asm
produce:
    stw buffer+offset, r1    # Write data
    fstw ring_head, r2       # Fenced write to head (ensures data visible)
```

### Consumer Pattern (SLOW-32 or Host)
```asm
consume:
    fldw r1, ring_head       # Fenced read of head
    ldw r2, buffer+offset    # Normal read of data (already synchronized)
    stw ring_tail, r3        # Update tail (consumer owns it)
```

## Execution Models

### Model 1: Request/Response
```asm
main:
    call process_ring_buffer
    yield r0                 # Done, return to host
    beq r0, r0, main        # Resume here when host wants more work
```

### Model 2: Continuous Processing (with concurrent host producer)
```asm
main:
    ldw r1, ring_head
    ldw r2, ring_tail
    beq r1, r2, main        # Spin waiting for work (host thread is producing)
    call process_item
    beq r0, r0, main        # Never yield, just keep processing
```

### Model 3: Hybrid
```asm
main:
    call check_ring_buffer
    bne r1, r0, process     # Work available?
    yield r0                 # No work, sleep
    beq r0, r0, main
process:
    call handle_work
    beq r0, r0, main
```

## Implementation Requirements

### Emulator Changes

1. **YIELD implementation**:
```c
case OP_YIELD:
    cpu->state = CPU_YIELDED;
    return EXECUTION_YIELDED;  // Host regains control
```

2. **Fenced memory operations**:
```c
case OP_FLDW:  // Fenced load
    __atomic_load(&memory[addr], &reg[rd], __ATOMIC_ACQUIRE);
    break;

case OP_FSTW:  // Fenced store  
    __atomic_store(&memory[addr], &reg[rs], __ATOMIC_RELEASE);
    break;
```

### Host Interface

```c
// Basic API
void cpu_init(cpu_state_t* cpu, uint8_t* shared_memory);
int cpu_run(cpu_state_t* cpu);        // Returns YIELDED or HALTED
void cpu_resume(cpu_state_t* cpu);    // Continue from YIELD

// Example usage
while (1) {
    add_work_to_ring(shared_memory, work_item);
    if (cpu->state == CPU_YIELDED) {
        cpu_resume(cpu);
    }
    int result = cpu_run(cpu);
    if (result == EXECUTION_YIELDED) {
        process_output_ring(shared_memory);
    }
}
```

## Advantages

1. **No MMIO complexity** - It's all just memory
2. **No interrupt handling** - Cooperative multitasking via YIELD
3. **True parallelism possible** - Host producer thread + SLOW-32 consumer
4. **Zero-copy** - Shared memory, no marshaling
5. **Protocol agnostic** - CPU doesn't know about ring buffers
6. **Minimal instructions** - Just YIELD, FLDW, FSTW added

## What This Doesn't Need

- Interrupt vectors
- Exception handling  
- I/O address decoding
- DMA controllers
- Interrupt controllers
- Priority levels
- Kernel/user mode separation

## Use Cases

- **Crypto coprocessor** - Process blocks of data for hashing/encryption
- **Compression engine** - Compress/decompress data blocks
- **Regex matching** - Scan text buffers for patterns
- **DSP operations** - Process audio/signal data
- **Database operations** - Sort, filter, join operations on data pages

The CPU provides the mechanism (YIELD + fenced ops), applications define the protocol.