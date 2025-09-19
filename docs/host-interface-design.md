# SLOW-32 Host Interface Design

## Core Philosophy
"SLOW-32 has no I/O" - All I/O operations are delegated to the host through a clean interface.

## Memory Layout with MMIO

```
0x00000000 - 0x000FFFFF  Code segment (execute-only, size from .s32x)
0x00100000 - 0x00EFFFFF  Data segment (read/write, size from .s32x)
0x00F00000 - 0x00FFFFFF  MMIO region (host communication queues)
0x01000000 - top         Stack (grows down from top of allocated memory)
```

## Linker-Defined Symbols

The linker automatically provides these symbols when referenced:

```asm
__code_start:  .word 0x00000000  ; Start of code segment
__code_end:    .word 0x00100000  ; End of code segment
__data_start:  .word 0x00100000  ; Start of data segment  
__data_end:    .word 0x00E00000  ; End of data segment
__heap_start:  .word 0x00E00000  ; Heap begins here
__heap_end:    .word 0x00F00000  ; Heap ends at MMIO region
__mmio_base:   .word 0x00F00000  ; MMIO region start
__mmio_end:    .word 0x01000000  ; MMIO region end
__stack_base:  .word 0x0FFFFFF0  ; Initial stack pointer
```

Programs only pay for what they use - unreferenced symbols aren't included.

## MMIO Queue Structure

### Recommended Layout

```c
// Queue header structure (cache-line aligned)
typedef struct {
    uint32_t head;           // Producer writes here
    uint32_t _pad1[15];      // Padding to 64 bytes (cache line)
    
    uint32_t tail;           // Consumer writes here  
    uint32_t _pad2[15];      // Padding to 64 bytes
    
    uint32_t size;           // Total queue size in bytes
    uint32_t entry_size;     // Size of each entry
    uint32_t flags;          // Queue control flags
    uint32_t _pad3[13];      // Padding to 64 bytes
} queue_header_t;

// Memory layout at __mmio_base
#define MMIO_BASE           __mmio_base
#define HIGH_QUEUE_OFFSET   0x00000  // 64KB for high priority
#define NORMAL_QUEUE_OFFSET 0x10000  // 128KB for normal  
#define BULK_QUEUE_OFFSET   0x30000  // 512KB for bulk
#define HOST_TO_SLOW32_OFF  0x00000  // First half for host→slow32
#define SLOW32_TO_HOST_OFF  0x80000  // Second half for slow32→host

// Each queue starts with header, then ring buffer
typedef struct {
    queue_header_t header;   // 192 bytes (3 cache lines)
    uint8_t buffer[];        // Rest is ring buffer data
} queue_t;
```

### Queue Flags

```c
#define QUEUE_FLAG_ENABLE    0x0001  // Queue is active
#define QUEUE_FLAG_OVERFLOW  0x0002  // Queue overflowed (data lost)
#define QUEUE_FLAG_NOTIFY    0x0004  // Send notification on write
#define QUEUE_FLAG_BLOCKING  0x0008  // Block when full (vs drop)
```

## Message Format

```c
// Generic message header
typedef struct {
    uint16_t type;      // Message type (TRAP, SYSCALL, EVENT, etc.)
    uint16_t flags;     // Message-specific flags
    uint32_t length;    // Total message length including header
    uint32_t timestamp; // Host timestamp (optional)
} message_header_t;

// Common message types
#define MSG_TRAP_REQUEST    0x0001  // SLOW-32 trap to host
#define MSG_TRAP_RESPONSE   0x0002  // Host trap response
#define MSG_EVENT           0x0003  // Async event from host
#define MSG_DATA            0x0004  // Bulk data transfer
#define MSG_CONTROL         0x0005  // Control/admin message
```

## TRAP Interface Migration

Replace the DEBUG instruction with generic TRAP:

```asm
; Old (deprecated)
debug r1  ; Print character

; New
addi r3, r0, 0      ; TRAP 0 = putchar
trap                ; r4 contains character
```

### Standard TRAP Numbers

```c
// Basic I/O (backward compatibility)
#define TRAP_PUTCHAR         0  // r4 = character
#define TRAP_GETCHAR         1  // returns character in r3
#define TRAP_PUTS            2  // r4 = string pointer
#define TRAP_GETS            3  // r4 = buffer, r5 = size

// Time and scheduling  
#define TRAP_GET_TIME        10 // returns timestamp in r3
#define TRAP_YIELD           11 // yield to other instances
#define TRAP_SLEEP           12 // r4 = milliseconds

// Queue operations
#define TRAP_QUEUE_SEND      20 // r4 = queue_id, r5 = msg_ptr
#define TRAP_QUEUE_RECV      21 // r4 = queue_id, r5 = buffer
#define TRAP_QUEUE_POLL      22 // r4 = queue_id, returns status

// Memory management
#define TRAP_MMAP            30 // r4 = size, returns addr
#define TRAP_MUNMAP          31 // r4 = addr, r5 = size

// Network (when needed)
#define TRAP_SOCKET          40 // r4 = domain, r5 = type
#define TRAP_BIND            41 // r4 = sock, r5 = addr
#define TRAP_LISTEN          42 // r4 = sock, r5 = backlog
#define TRAP_ACCEPT          43 // r4 = sock
#define TRAP_RECV            44 // r4 = sock, r5 = buf, r6 = len
#define TRAP_SEND            45 // r4 = sock, r5 = buf, r6 = len
#define TRAP_CLOSE           46 // r4 = sock
```

## Queue Operations (Direct MMIO)

```c
// Producer (host writing to SLOW-32)
void enqueue(queue_t* q, void* data, size_t len) {
    queue_header_t* h = &q->header;
    uint32_t head = h->head;
    uint32_t tail = h->tail;  // Consumer's cache line
    
    uint32_t next_head = (head + len) % h->size;
    if (next_head == tail) {
        // Queue full - handle per flags
        if (h->flags & QUEUE_FLAG_BLOCKING) {
            // Wait for space
        } else {
            h->flags |= QUEUE_FLAG_OVERFLOW;
            return;
        }
    }
    
    // Copy data to ring buffer
    memcpy(&q->buffer[head], data, len);
    
    // Update head (producer's cache line)
    h->head = next_head;
}

// Consumer (SLOW-32 reading from host)
int dequeue(queue_t* q, void* buffer, size_t max_len) {
    queue_header_t* h = &q->header;
    uint32_t head = h->head;  // Producer's cache line
    uint32_t tail = h->tail;
    
    if (tail == head) {
        return 0;  // Queue empty
    }
    
    // Calculate available data
    uint32_t avail = (head - tail) % h->size;
    uint32_t to_read = (avail < max_len) ? avail : max_len;
    
    // Copy data from ring buffer
    memcpy(buffer, &q->buffer[tail], to_read);
    
    // Update tail (consumer's cache line)  
    h->tail = (tail + to_read) % h->size;
    
    return to_read;
}
```

## Priority Queue Usage Pattern

```c
// SLOW-32 main loop
void main_loop() {
    queue_t* high   = (queue_t*)(MMIO_BASE + HIGH_QUEUE_OFFSET);
    queue_t* normal = (queue_t*)(MMIO_BASE + NORMAL_QUEUE_OFFSET);
    queue_t* bulk   = (queue_t*)(MMIO_BASE + BULK_QUEUE_OFFSET);
    
    message_header_t msg;
    
    while (running) {
        // Check queues in priority order
        if (dequeue(high, &msg, sizeof(msg))) {
            handle_high_priority(&msg);
        } else if (dequeue(normal, &msg, sizeof(msg))) {
            handle_normal_priority(&msg);
        } else if (dequeue(bulk, &msg, sizeof(msg))) {
            handle_bulk_data(&msg);
        } else {
            // No work available
            trap_yield();  // Let other instances run
        }
    }
}
```

## Flow Control Strategy

### Per-Queue Strategies

**High Priority Queue**
- Small (64KB typical)
- Never blocks sender
- Drops connection if truly overwhelmed
- Used for control messages only

**Normal Priority Queue**
- Medium (128KB typical)
- Applies backpressure to sender
- Standard flow control via queue fill level

**Bulk Queue**
- Large (512KB+) or streaming
- Can be paused entirely under pressure
- Separate flow control from other queues

## Implementation Phases

### Phase 1: Basic TRAP Interface
1. Replace DEBUG with TRAP instruction
2. Implement basic TRAP handlers (putchar, yield)
3. Update runtime to use TRAPs

### Phase 2: Queue Infrastructure
1. Add MMIO region to memory map
2. Implement ring buffer structures
3. Add queue-based messaging

### Phase 3: Multi-Instance Support
1. Instance isolation
2. Per-instance MMIO regions
3. Host scheduler for instances

### Phase 4: Network Services
1. Socket TRAPs
2. Packet queuing
3. Example: echo server
4. Example: HTTP server

## Performance Considerations

### Cache Line Separation
- Producer (head) and consumer (tail) on different cache lines
- Prevents false sharing between host and SLOW-32
- 64-byte cache line alignment assumed

### Batch Operations
- Dequeue multiple messages per iteration
- Amortize synchronization overhead
- Reduce TRAP frequency

### Zero-Copy Possibilities
- Large messages stay in queue memory
- Pass pointers instead of copying
- Requires careful memory management

## Security Considerations

### Queue Validation
- Validate all pointers before access
- Check message sizes against queue capacity
- Sanitize message types and flags

### Instance Isolation  
- Each instance has separate MMIO region
- No shared memory between instances
- Host enforces isolation

### Resource Limits
- Per-instance memory limits
- Queue size limits
- Message rate limiting

## Example: Simple Echo Server

```c
// SLOW-32 echo server
int main() {
    int sock = trap_socket(AF_INET, SOCK_STREAM);
    trap_bind(sock, 8080);
    trap_listen(sock, 10);
    
    while (1) {
        int client = trap_accept(sock);
        
        char buffer[1024];
        int len = trap_recv(client, buffer, sizeof(buffer));
        
        if (len > 0) {
            trap_send(client, buffer, len);
        }
        
        trap_close(client);
    }
}
```

## Future Extensions

### DMA-Style Operations
- Host pre-processes data
- Places results directly in SLOW-32 memory
- Notification via queue message

### Event Aggregation
- Batch multiple events into single message
- Reduce message overhead
- Improve throughput

### Shared Memory Regions
- Read-only shared data between instances
- Copy-on-write semantics
- Careful synchronization required

## Multi-Instance vs Multi-Threading

### Why Not Threads?

SLOW-32 explicitly avoids multi-threading within instances. Instead, use multiple single-threaded instances with message passing. Here's why:

**The Threading Performance Trap**

Real-world threading experience shows that most developers get it catastrophically wrong:

1. **False Sharing**: As soon as threads touch memory that's merely *close* to what another thread is touching (same cache line), performance dies. Two threads incrementing different counters in the same cache line can be 10x slower than single-threaded.

2. **Cache Thrashing**: Threads touching too much memory overwhelm the cache. Every context switch reloads cache. Performance dies again.

3. **Mental Model Mismatch**: People use threads to solve conceptual problems ("I need to do two things at once") not performance problems. The right rule is roughly threads ≈ CPU cores, with algorithms designed to keep threads working in separate memory regions.

4. **Synchronization Overhead**: Every lock, every atomic operation, every memory barrier is a performance killer. The cost of coordination often exceeds the benefit of parallelism.

### The SLOW-32 Model: Instances, Not Threads

```c
// Virtual memory trick: share code, separate everything else
void* code_segment = mmap(..., PROT_READ | PROT_EXEC, ...);

// Each instance gets:
struct instance {
    void* code;       // Points to shared code_segment (read-only)
    void* data;       // Unique mmap'd data segment
    uint32_t regs[32]; // Private registers
    uint32_t pc;      // Private program counter
    queue_t* inbox;   // Message queue from other instances
};

// Run N instances on N threads
for (int i = 0; i < n; i++) {
    instances[i].code = code_segment;  // All share same code
    instances[i].data = mmap(...);     // Each gets own data
    // Each runs on separate host thread, no sharing
}
```

### W^X Protection via MMU

Instead of software checks in the emulator, use the MMU directly:

```c
// Map code segment as read+execute only
void* code = mmap(NULL, code_size, 
                  PROT_READ | PROT_EXEC,  // No PROT_WRITE!
                  MAP_PRIVATE, fd, 0);

// Map data segment as read+write only  
void* data = mmap(NULL, data_size,
                  PROT_READ | PROT_WRITE,  // No PROT_EXEC!
                  MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);

// Wrap instance execution in signal handler
struct sigaction sa = {
    .sa_sigaction = handle_segv,
    .sa_flags = SA_SIGINFO
};
sigaction(SIGSEGV, &sa, NULL);

// SIGSEGV handler for W^X violations
void handle_segv(int sig, siginfo_t* info, void* context) {
    if (instance_caused_fault(info->si_addr)) {
        // SLOW-32 instance violated W^X
        kill_instance(current_instance);
        log_violation(current_instance, info->si_addr);
        // Continue running other instances
    } else {
        // Real segfault in host - reraise
        abort();
    }
}
```

**Benefits:**
- Hardware-enforced W^X (zero overhead in fast path)
- MMU does the checking at memory speed
- Failed instance can't crash the host
- Other instances continue running
- No per-access software checks needed
- Code segment shared (memory efficient)
- OS enforces protection via MMU (hardware-speed)
- No false sharing (separate data segments)
- No cache line bouncing (separate memory regions)
- No synchronization primitives needed
- Trivial to debug (each instance is independent)

**Communication:**
- Message passing only
- Host-managed queues
- No shared memory, ever
- Clean, understandable, fast

### The Rule

"SLOW-32 has no I/O, no shared memory, no threads. Just computation and messages."

If you need real threading with shared memory, use native code. SLOW-32 is for orchestration, not parallel number crunching.

## Summary

This design provides:
- Clean separation between compute (SLOW-32) and I/O (host)
- Efficient queue-based communication
- Natural priority handling
- Cache-friendly memory layout
- Simple, understandable programming model
- Path to high-performance network services
- True parallelism via multiple instances, not threads

The key insight: SLOW-32 handles logic, host handles logistics. And parallelism comes from isolated instances passing messages, not from threads fighting over cache lines.