# MMIO Ring Buffer - Final Execution Model

## The Core Insight

TRAP, YIELD, and HALT are all **synchronization points** where the emulator can:
1. Process outgoing requests from guest
2. Deliver incoming data to guest  
3. Decide whether to continue execution

No interrupts needed. No threading needed. No polling loops needed.

## The Execution Model

### Guest Side
```c
// Submit request and wait for response
void io_request(void *data) {
    add_to_outgoing_ring(data);
    TRAP();  // "I have work for you"
    
    while (!response_ready()) {
        YIELD();  // "Check if you have work for me"
    }
}

// Background work with periodic checks
void main_loop() {
    while (1) {
        do_some_computation();
        YIELD();  // "Check for async messages"
    }
}

// Waiting for input
char getchar() {
    submit_read_request();
    while (!char_available()) {
        YIELD();  // "I'm waiting"
    }
    return get_char();
}
```

### Host Side (Emulator)
```c
// Unified handler for all sync points
void handle_sync_point(cpu_state_t *cpu, opcode_t op) {
    // Process any outgoing requests
    while (has_outgoing_requests()) {
        request_t *req = get_request();
        response_t *resp = process_request(req);
        add_response(resp);
    }
    
    // Deliver any incoming data (from host threads, network, etc)
    while (has_incoming_data()) {
        deliver_to_guest_ring();
    }
    
    // Handle specific opcode
    switch(op) {
        case OP_HALT:
            if (no_pending_work()) {
                cpu->halted = true;
            }
            // Otherwise continue - there's work!
            break;
            
        case OP_TRAP:
            // TRAP implies urgent outgoing work
            // Already processed above
            break;
            
        case OP_YIELD:
            // YIELD is cooperative check
            // Already processed above
            break;
    }
}

// In main emulator loop
case OP_TRAP:
case OP_YIELD:
case OP_HALT:
    handle_sync_point(cpu, inst.opcode);
    break;
```

## Why This Works

1. **No Polling**: Guest explicitly yields when waiting
2. **No Spinning**: YIELD processes rings then returns
3. **Natural Points**: I/O operations have built-in sync
4. **Simple**: No threads, no interrupts, no complexity
5. **Efficient**: Only check rings at meaningful points

## Implementation Details

### Ring Buffer Layout (Unchanged)
```
0x10000000: REQ_HEAD   (Guest writes)
0x10000004: REQ_TAIL   (Host writes)
0x10001000: REQ_RING   (4KB descriptors)

0x10002000: RESP_HEAD  (Host writes)
0x10002004: RESP_TAIL  (Guest writes)  
0x10003000: RESP_RING  (4KB descriptors)

0x10004000: DATA_BUF   (56KB shared data)
```

### Queue Roles and Direction

- `REQ_*` ring is **guest → host**. The guest is the sole producer: it allocates descriptors, fills them, and advances `REQ_HEAD`. The host is the sole consumer: it reads descriptors, performs the requested work, and advances `REQ_TAIL`.
- `RESP_*` ring is **host → guest**. Roles are inverted: the host produces completions and the guest consumes them.
- A future **high-priority host → guest ring** can sit alongside the response ring for urgent traffic (timers, async signals, debugger traps). Because we own both endpoints, it can share the same descriptor schema but be polled ahead of the normal completion ring whenever the guest executes TRAP/YIELD.

This mirrors a NIC, but inverted: instead of the host driver feeding descriptors to a device, the SLOW-32 guest feeds work units to the emulator. The host never allocates guest-visible buffers—it only borrows pointers provided inside the descriptor and must acknowledge completion through the response/HP rings.

### Descriptor Ownership and Lifecycle

1. **Guest produces**: write the opcode, payload pointers/lengths, and flags into the next free request descriptor. Issue a store-release before publishing the updated `REQ_HEAD`.
2. **Host consumes**: poll `REQ_HEAD` vs `REQ_TAIL`. For each new descriptor, perform the work (e.g., copy from/to the shared data buffer, touch host files, etc.). When done, write a completion descriptor into the response ring, including status/result fields, then store-release `RESP_HEAD`.
3. **Guest completes**: poll `RESP_HEAD` vs `RESP_TAIL`. Once a completion arrives, load the results, advance `RESP_TAIL`, and continue. If the guest is blocked, it executes `YIELD`, which guarantees the host will service the rings before returning control.

Because each ring is single-producer/single-consumer, simple head/tail counters with wraparound are sufficient. Memory fences only need to order “payload before head update” and “head observation before payload read,” which keeps the implementation lightweight on both sides.

### High-Priority Queue (Optional)

The optional HP ring is tiny (dozens of entries) and host-produced. Typical uses:

- Deliver timer expirations or watchdog pings without waiting for the standard response path.
- Notify the guest about debugger events, breakpoints, or host-driven shutdown requests.
- Surface asynchronous device arrivals (e.g., inbound socket ready) while large transfers continue in the normal ring.

The emulator must drain the HP ring before touching the normal response ring each time it wakes, ensuring low latency even when bulk transfers keep the regular ring busy. Guests simply poll both rings inside the same TRAP/YIELD handling loop.

### Linux-Style Contract

Think of the MMIO bridge as the user/kernel boundary: guest code is effectively **Linux userland**, while the host emulator stands in for the **kernel**. Every descriptor is a pseudo-syscall struct:

```
word 0: opcode      // which syscall
word 1: length      // main byte count (e.g., buffer size)
word 2: offset      // secondary value (file offset, data pointer index, etc.)
word 3: status/arg  // descriptor-specific flags, fd, errno, etc.
```

Payload bytes live in `DATA_BUF`; descriptors only carry small integers/handles so we keep the rings compact. Standard opcodes already mirror familiar Linux syscalls:

- `S32_MMIO_OP_OPEN`: path string in `DATA_BUF`, `length` = bytes incl. NUL, `status` carries `O_*`-style flags. Host opens/creates the file and returns a small integer fd that guest reuses, just like `open(2)`.
- `S32_MMIO_OP_READ`/`WRITE`: fd in `status`, byte count in `length`, data staged in/out of `DATA_BUF`, matching `read(2)`/`write(2)` semantics (short reads indicate EOF, short writes report error).
- `S32_MMIO_OP_SEEK`: fd in `status`, `DATA_BUF` holds packed `off_t`+`whence`, aligning with `lseek(2)`.
- `S32_MMIO_OP_CLOSE`, `S32_MMIO_OP_BRK`, `S32_MMIO_OP_EXIT` map directly to their Linux namesakes.
- `S32_MMIO_OP_STAT`: `status` carries either an fd (for `fstat`) or `S32_MMIO_STAT_PATH_SENTINEL` (for `stat`), and the response overwrites the caller’s buffer with an `s32_mmio_stat_result_t` (64-bit dev/ino/size, POSIX mode bits, nanosecond timestamps).
- `S32_MMIO_OP_GETTIME` (0x30) drops a `{seconds_lo, seconds_hi, nanoseconds}` tuple into `DATA_BUF` (16 bytes) so libc can implement `clock_gettime`, `time`, or `gettimeofday` without new firmware or Year-2038 limits.

Future extensions (sockets, timers, `poll`, `gettimeofday`, etc.) can keep building on this convention: drop the Linux syscall arguments into the descriptor/data buffer, let the host execute the real OS call, and post the result plus `errno` back through the completion ring. Because both halves follow the same schema, we can implement libc once and trust the emulator to provide kernel-like behavior without inventing bespoke firmware semantics.

### Critical Optimization
The host can check if guest is blocked in YIELD:
```c
if (cpu->waiting_for_io && has_responses()) {
    // Wake immediately, don't wait for next YIELD
    deliver_responses();
    cpu->waiting_for_io = false;
}
```

### Future Extension: Async Host I/O
If host has a second thread for real I/O:
```c
// I/O thread
void io_thread() {
    while (1) {
        wait_for_request();
        result = do_real_io();  // Actual file/network I/O
        add_to_incoming_queue(result);
        // Next YIELD will deliver it
    }
}
```

## Benefits

1. **Clean**: Clear separation of concerns
2. **Simple**: No IRQ complexity needed
3. **Efficient**: No wasted cycles polling
4. **Extensible**: Can add threads/IRQ later
5. **Practical**: Works today with current ISA

## The Key Realization

We don't need interrupts for I/O. We need them for preemption.
For cooperative I/O, TRAP/YIELD synchronization points are sufficient
and actually cleaner than IRQ-based designs.

The ring buffers handle data movement.
TRAP/YIELD handle control flow.
Together they make a complete I/O system.
