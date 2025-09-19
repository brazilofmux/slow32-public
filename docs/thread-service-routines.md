# Thread Service Routines (TSR) - Alternative to Interrupts

## Concept

Instead of traditional interrupts that save/restore context and preempt execution, Thread Service Routines are **separate hardware threads** that run in response to events. The emulator maintains multiple CPU contexts that can execute independently.

## Key Innovation: Vectors Outside Memory

Interrupt/TSR vectors are stored in the binary header, NOT in memory:

```c
struct slow32_header {
    uint32_t magic;
    uint32_t entry;
    uint32_t code_size;
    uint32_t data_size;
    // TSR entry points - not in memory map!
    uint32_t tsr_handlers[16];  // TSR0-15 addresses
};
```

This means:
- No memory access to fetch vectors (fast!)
- No wasted memory space for vector tables
- Vectors are program metadata, not runtime state
- Optional - programs without TSRs just set to 0

## Thread Model vs Interrupt Model

### Traditional Interrupts
```
Main: [running] -> [SAVE ALL] -> [ISR] -> [RESTORE ALL] -> [continue]
                   ^^^^^^^^^^             ^^^^^^^^^^^^
                   Expensive!             Expensive!
```

### Thread Service Routines
```
Main: [running] -----------------> [keeps running]
TSR:            \-> [fresh start] -> [runs independently] -> [halt]
```

## Implementation Sketch

```c
struct cpu_context {
    uint32_t regs[32];
    uint32_t pc;
    // Each context has its own state
};

struct emulator {
    struct cpu_context main;
    struct cpu_context tsr[MAX_TSR];
    uint32_t tsr_active;  // Bitmap of running TSRs
};

// On timer event (not interrupt!):
if (header.tsr_handlers[TSR_TIMER] != 0) {
    // Spawn TSR - no context save!
    emu->tsr[TSR_TIMER].pc = header.tsr_handlers[TSR_TIMER];
    emu->tsr[TSR_TIMER].regs[29] = TSR_STACK_BASE;  // Fresh stack
    emu->tsr_active |= (1 << TSR_TIMER);
}

// Execution can be parallel or round-robin:
while (1) {
    cpu_step(&emu->main);
    for (int i = 0; i < MAX_TSR; i++) {
        if (emu->tsr_active & (1 << i)) {
            cpu_step(&emu->tsr[i]);
            if (emu->tsr[i].halted) {
                emu->tsr_active &= ~(1 << i);
            }
        }
    }
}
```

## Communication Between Threads

Since TSRs can't access main thread registers, they communicate through memory:

```asm
# Main thread
main:
    stw work_queue, r1    # Post work
    fstw work_flag, r2    # Signal TSR (fenced)
    
# TSR thread  
tsr_timer:
    fldw r1, work_flag    # Check for work (fenced)
    beq r1, r0, tsr_exit
    ldw r2, work_queue    # Get work
    # Process...
    stw result_queue, r3
tsr_exit:
    halt                  # TSR done
```

## Advantages

1. **No context switch overhead** - TSRs start fresh
2. **Main thread never interrupted** - Truly concurrent
3. **Simple implementation** - No save/restore logic
4. **Natural parallelism** - Could run TSRs on different host cores
5. **Clean isolation** - TSRs can't corrupt main thread

## Disadvantages

1. **Not real interrupts** - Can't preempt main thread
2. **No register sharing** - Must communicate through memory
3. **Multiple stacks needed** - Each TSR needs stack space
4. **More complex scheduling** - Which thread runs when?

## Open Questions

1. **Scheduling**: Round-robin? Priority? Parallel on host threads?
2. **Stack allocation**: Fixed TSR stacks? Dynamic?
3. **TSR-to-TSR communication**: Can TSRs trigger other TSRs?
4. **Synchronization**: Do we need atomic ops between threads?
5. **Number of TSRs**: How many concurrent TSRs to support?

## Comparison to Other Models

- **Traditional interrupts**: Save/restore everything, complex but flexible
- **Hardware threads** (SMT): Share execution units, complex scheduling
- **Coprocessors**: Fully independent, communicate through memory
- **TSRs**: Middle ground - separate contexts but shared memory

## Future Considerations

This model could evolve into:
- Full hardware threading (multiple main threads)
- Asymmetric multiprocessing (big main thread, small TSRs)
- Event-driven architecture (TSRs for all I/O)

But for now, keeping it simple: one main thread, optional TSRs for events.