# SLOW-32 Ring Buffer MMIO Design

## Overview
High-performance I/O using ring buffers, inspired by modern NIC and NVMe designs.

## Ring Buffer Architecture

### Memory Layout
```
0x10000000: REQUEST_HEAD   // Producer index (CPU writes)
0x10000004: REQUEST_TAIL   // Consumer index (Device reads)
0x10001000: REQUEST_RING   // 4KB request descriptor ring

0x10002000: RESPONSE_HEAD  // Producer index (Device writes)  
0x10002004: RESPONSE_TAIL  // Consumer index (CPU reads)
0x10003000: RESPONSE_RING  // 4KB response descriptor ring

0x10004000: DATA_BUFFER    // 56KB shared data buffer
```

### Descriptor Format (16 bytes)
```c
typedef struct {
    uint32_t opcode;    // Operation type
    uint32_t length;    // Data length
    uint32_t offset;    // Offset in data buffer
    uint32_t status;    // Result/error code
} io_descriptor_t;
```

### Operation Flow
1. **CPU submits request**:
   - Write data to DATA_BUFFER
   - Fill descriptor at REQUEST_RING[head]
   - Increment REQUEST_HEAD

2. **Device processes**:
   - Read descriptor at REQUEST_RING[tail]
   - Process operation
   - Write response descriptor to RESPONSE_RING
   - Increment REQUEST_TAIL and RESPONSE_HEAD

3. **CPU gets response**:
   - Read descriptor at RESPONSE_RING[tail]
   - Read data from DATA_BUFFER if needed
   - Increment RESPONSE_TAIL

## Opcodes

```c
#define OP_NOP        0x00  // No operation
#define OP_PUTCHAR    0x01  // Write character
#define OP_GETCHAR    0x02  // Read character  
#define OP_WRITE      0x03  // Write bytes
#define OP_READ       0x04  // Read bytes
#define OP_OPEN       0x05  // Open file
#define OP_CLOSE      0x06  // Close file
#define OP_SEEK       0x07  // Seek in file
#define OP_BRK        0x08  // Adjust heap
#define OP_EXIT       0x09  // Exit program
#define OP_STAT       0x0A  // Get file info
```

## Ring Management

### Producer-Consumer Rules
```c
// Check if ring is full (producer side)
bool ring_full(uint32_t head, uint32_t tail, uint32_t size) {
    return ((head + 1) % size) == tail;
}

// Check if ring is empty (consumer side)
bool ring_empty(uint32_t head, uint32_t tail) {
    return head == tail;
}

// Available entries
uint32_t ring_available(uint32_t head, uint32_t tail, uint32_t size) {
    return (tail - head - 1) % size;
}
```

## Example: Console I/O

### Write String
```c
void write_string(const char *str) {
    uint32_t len = strlen(str);
    uint32_t head = *(volatile uint32_t*)REQUEST_HEAD;
    uint32_t tail = *(volatile uint32_t*)REQUEST_TAIL;
    
    // Wait for space
    while (ring_full(head, tail, 256)) {
        tail = *(volatile uint32_t*)REQUEST_TAIL;
    }
    
    // Copy data to buffer
    uint32_t data_offset = (head * 64) % DATA_BUFFER_SIZE;
    memcpy((void*)(DATA_BUFFER + data_offset), str, len);
    
    // Fill descriptor
    io_descriptor_t *desc = (io_descriptor_t*)(REQUEST_RING + head * 16);
    desc->opcode = OP_WRITE;
    desc->length = len;
    desc->offset = data_offset;
    desc->status = 1;  // stdout
    
    // Submit
    *(volatile uint32_t*)REQUEST_HEAD = (head + 1) % 256;
    
    // Wait for completion
    uint32_t resp_tail = *(volatile uint32_t*)RESPONSE_TAIL;
    while (*(volatile uint32_t*)RESPONSE_HEAD == resp_tail) {
        // Poll
    }
    
    // Acknowledge
    *(volatile uint32_t*)RESPONSE_TAIL = (resp_tail + 1) % 256;
}
```

### Read Character
```c
int read_char(void) {
    uint32_t head = *(volatile uint32_t*)REQUEST_HEAD;
    
    // Submit read request
    io_descriptor_t *desc = (io_descriptor_t*)(REQUEST_RING + head * 16);
    desc->opcode = OP_GETCHAR;
    desc->length = 1;
    desc->offset = 0;
    desc->status = 0;  // stdin
    
    *(volatile uint32_t*)REQUEST_HEAD = (head + 1) % 256;
    
    // Wait for response
    uint32_t resp_tail = *(volatile uint32_t*)RESPONSE_TAIL;
    while (*(volatile uint32_t*)RESPONSE_HEAD == resp_tail) {
        // Poll
    }
    
    // Get result
    io_descriptor_t *resp = (io_descriptor_t*)(RESPONSE_RING + resp_tail * 16);
    int ch = *(uint8_t*)(DATA_BUFFER + resp->offset);
    
    // Acknowledge
    *(volatile uint32_t*)RESPONSE_TAIL = (resp_tail + 1) % 256;
    
    return ch;
}
```

## Advantages

1. **Batching**: Can submit multiple operations before waiting
2. **Async**: CPU doesn't block on each operation
3. **Efficient**: Minimal synchronization overhead
4. **Scalable**: Same pattern works for network, disk, etc.
5. **Standard**: Follows industry best practices

## Implementation Notes

- Ring sizes: 256 entries (4KB each ring)
- Data buffer: 56KB (with 8KB for rings = 64KB MMIO total)
- Cacheline-aligned descriptors (16 bytes)
- Memory barriers needed for correctness
- Could add interrupt support later

This is how real hardware does I/O!