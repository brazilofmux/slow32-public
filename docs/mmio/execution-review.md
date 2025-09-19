# MMIO Ring Buffer Execution Model - Design Review

## The Problem

We built beautiful ring buffers, but haven't addressed the fundamental execution model problem:

### Current Design Has Three Polling Loops! 😱

1. **Guest code polls** waiting for ring space:
```c
while (ring_full(head, tail)) {
    tail = REQ_TAIL;  // Spin waiting
}
```

2. **Guest code polls** waiting for response:
```c
while (RESP_HEAD == resp_tail) {
    // Spin waiting for device
}
```

3. **Emulator must poll** checking for requests:
```c
while (running) {
    cpu_step();
    if (mmio_has_requests()) {  // Check every instruction?!
        process_requests();
    }
}
```

This is terrible! The guest spins, the host checks constantly, nobody wins.

## Real Hardware Solutions

### How NICs Actually Work
1. **DMA**: Hardware fetches descriptors independently
2. **Interrupts**: NIC interrupts CPU when done
3. **Doorbell**: CPU writes special register to notify NIC
4. **Coalescing**: Batch multiple operations before interrupt

### How We're Different
- No true parallelism (emulator is single-threaded)
- No real DMA (emulator controls all memory)
- No interrupts (yet)
- Guest and host are in same process!

## Potential Solutions

### Option 1: Check on Memory Barriers (Pragmatic)
- Check rings only on special instructions (FENCE, YIELD)
- Guest explicitly yields when waiting
- Pro: Simple, deterministic
- Con: Requires guest cooperation

### Option 2: Check Every N Instructions (Simple)
- Process rings every 100/1000 instructions
- Pro: No guest changes needed
- Con: Latency, arbitrary constant

### Option 3: Check on Ring Head Write (Smart)
- Trap writes to REQ_HEAD specifically
- Process immediately when guest submits
- Pro: Low latency, natural trigger
- Con: Need to detect specific address write

### Option 4: Dedicated MMIO Instruction (Clean)
```asm
MMIO rs1, rs2, rd  # New instruction
```
- Explicit MMIO operation
- Emulator handles immediately
- Pro: Clean separation, no polling
- Con: ISA change

### Option 5: Use YIELD as Doorbell (Hybrid)
- YIELD instruction triggers ring processing
- Guest yields when waiting for response
- Pro: Uses existing ISA, natural points
- Con: Still some polling in guest

## Recommended Approach

**Phase 1: Option 3 + 5 Hybrid**
1. Trap writes to REQ_HEAD (doorbell)
2. Process requests immediately
3. Guest uses YIELD when waiting
4. YIELD also processes rings

**Phase 2: Add Interrupts**
1. Add interrupt support to ISA
2. Device interrupts on completion
3. Guest can sleep instead of poll

**Phase 3: Threading (Optional)**
1. Separate I/O thread for devices
2. Real async processing
3. Lock-free ring buffers shine

## Code Changes Needed

### Emulator Side
```c
// In cpu_mem_write()
if (addr == REQ_HEAD) {
    mmio_ring_write(...);
    mmio_ring_process();  // Process immediately!
    return;
}

// In YIELD handler
case OP_YIELD:
    if (mmio_enabled) {
        mmio_ring_process();  // Check on yield
    }
    break;
```

### Guest Library
```c
// Modified request submission
void submit_request() {
    REQ_HEAD = new_head;  // Triggers processing
    
    // Wait for response with yields
    while (RESP_HEAD == resp_tail) {
        asm("yield");  // Let emulator process
    }
}
```

## Performance Analysis

### Current (Polling Everywhere)
- Guest: Wastes cycles spinning
- Host: Checks every instruction
- Overhead: Massive

### Proposed (Doorbell + Yield)
- Guest: Yields instead of spinning
- Host: Processes on doorbell + yield
- Overhead: Minimal

### Future (Interrupts)
- Guest: Blocks on WFI (wait for interrupt)
- Host: Signals completion
- Overhead: Near zero

## Decision Point

Before integration, we need to decide:

1. **Doorbell approach?** (Trap REQ_HEAD writes)
2. **YIELD processing?** (Check rings on yield)
3. **Polling fallback?** (Every N instructions as safety)
4. **Skip to interrupts?** (More complex but cleaner)

The ring buffers are good architecture, but without proper triggering
mechanisms, they become polling nightmares. We need to decide on the
execution model before integration.

## Next Steps

1. Choose execution model
2. Implement doorbell mechanism
3. Update guest library to use yields
4. Test with real programs
5. Consider interrupt support

The rings are right. The execution model needs work.