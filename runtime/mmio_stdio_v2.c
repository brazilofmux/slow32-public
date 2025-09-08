// SLOW-32 stdio implementation using ring buffer MMIO
// Version 2: Using TRAP/YIELD synchronization (no polling!)
#include "stdint_minimal.h"

// MMIO addresses
#define MMIO_BASE     0x10000000
#define REQ_HEAD      (*(volatile uint32_t*)(MMIO_BASE + 0x0000))
#define REQ_TAIL      (*(volatile uint32_t*)(MMIO_BASE + 0x0004))
#define REQ_RING      ((volatile uint32_t*)(MMIO_BASE + 0x1000))
#define RESP_HEAD     (*(volatile uint32_t*)(MMIO_BASE + 0x2000))
#define RESP_TAIL     (*(volatile uint32_t*)(MMIO_BASE + 0x2004))
#define RESP_RING     ((volatile uint32_t*)(MMIO_BASE + 0x3000))
#define DATA_BUFFER   ((volatile uint8_t*)(MMIO_BASE + 0x4000))

#define RING_SIZE     256
#define DESC_SIZE     4  // 4 words per descriptor

// Operation codes (match IO_OP_ from mmio_ring.h)
#define OP_PUTCHAR    0x01
#define OP_GETCHAR    0x02
#define OP_WRITE      0x03
#define OP_READ       0x04
#define OP_BRK        0x08
#define OP_EXIT       0x09
#define OP_FLUSH      0x0B

// YIELD instruction using function call
// This will be resolved by linking with a small assembly stub
extern void yield(void);

// Ring buffer utilities
static inline int ring_full(uint32_t head, uint32_t tail) {
    return ((head + 1) % RING_SIZE) == tail;
}

static inline int ring_empty(uint32_t head, uint32_t tail) {
    return head == tail;
}

// Submit a request and wait for response
// Key change: YIELD instead of spinning!
static uint32_t mmio_request(uint32_t opcode, uint32_t length, uint32_t offset, uint32_t status) {
    // Wait for request ring space (with YIELD)
    uint32_t req_head = REQ_HEAD;
    uint32_t req_tail = REQ_TAIL;
    
    while (ring_full(req_head, req_tail)) {
        yield();  // Let host process rings!
        req_tail = REQ_TAIL;
    }
    
    // Write descriptor (4 words)
    uint32_t desc_offset = req_head * DESC_SIZE;
    REQ_RING[desc_offset + 0] = opcode;
    REQ_RING[desc_offset + 1] = length;
    REQ_RING[desc_offset + 2] = offset;
    REQ_RING[desc_offset + 3] = status;
    
    // Submit request
    REQ_HEAD = (req_head + 1) % RING_SIZE;
    
    // YIELD to process request and check for response
    yield();
    
    // Wait for response (with more YIELDs if needed)
    uint32_t resp_tail = RESP_TAIL;
    uint32_t resp_head = RESP_HEAD;
    
    while (ring_empty(resp_head, resp_tail)) {
        yield();  // Let host deliver response!
        resp_head = RESP_HEAD;
    }
    
    // Read response
    uint32_t resp_offset = resp_tail * DESC_SIZE;
    uint32_t result = RESP_RING[resp_offset + 3];  // Status is in word 3
    
    // Acknowledge response
    RESP_TAIL = (resp_tail + 1) % RING_SIZE;
    
    return result;
}

// Batched write for efficiency
static void write_batch(const uint8_t *data, uint32_t len, int fd) {
    // Copy to data buffer
    for (uint32_t i = 0; i < len && i < 4096; i++) {
        DATA_BUFFER[i] = data[i];
    }
    
    // Submit write request
    mmio_request(OP_WRITE, len, 0, fd);
}

// Put a character to stdout
void putchar(int c) {
    // Buffer for line buffering
    static uint8_t line_buffer[256];
    static uint32_t buffer_pos = 0;
    
    line_buffer[buffer_pos++] = (uint8_t)c;
    
    // Flush on newline or buffer full
    if (c == '\n' || buffer_pos >= sizeof(line_buffer)) {
        write_batch(line_buffer, buffer_pos, 1);
        buffer_pos = 0;
    }
}

// Get a character from stdin
int getchar(void) {
    mmio_request(OP_GETCHAR, 1, 0, 0);  // fd=0 (stdin)
    return DATA_BUFFER[0];
}

// Write a string to stdout
void puts(const char *s) {
    uint32_t len = 0;
    while (s[len] && len < 1024) {
        len++;
    }
    
    // Copy string + newline to data buffer
    for (uint32_t i = 0; i < len; i++) {
        DATA_BUFFER[i] = s[i];
    }
    DATA_BUFFER[len] = '\n';
    
    mmio_request(OP_WRITE, len + 1, 0, 1);  // fd=1 (stdout)
}

// Write bytes to a file descriptor
int write(int fd, const void *buf, size_t count) {
    if (count > 4096) count = 4096;
    
    const uint8_t *src = (const uint8_t*)buf;
    for (size_t i = 0; i < count; i++) {
        DATA_BUFFER[i] = src[i];
    }
    
    mmio_request(OP_WRITE, count, 0, fd);
    return count;
}

// Read bytes from a file descriptor
int read(int fd, void *buf, size_t count) {
    if (count > 4096) count = 4096;
    
    uint32_t result = mmio_request(OP_READ, count, 0, fd);
    
    uint8_t *dest = (uint8_t*)buf;
    for (size_t i = 0; i < count && i < result; i++) {
        dest[i] = DATA_BUFFER[i];
    }
    
    return result;
}

// External halt function (from yield.s)
extern void halt(void);

// Exit the program
void exit(int status) {
    mmio_request(OP_EXIT, 0, 0, status);
    // Should not return, but just in case...
    halt();
}

// Flush output buffers
void fflush_stdout(void) {
    mmio_request(OP_FLUSH, 0, 0, 0);
}

// Simple malloc using brk
static uint32_t heap_current = 0;

void* sbrk(int increment) {
    if (heap_current == 0) {
        heap_current = mmio_request(OP_BRK, 0, 0, 0);
    }
    
    uint32_t old_break = heap_current;
    
    if (increment != 0) {
        uint32_t new_break = heap_current + increment;
        uint32_t result = mmio_request(OP_BRK, 0, 0, new_break);
        
        if (result != new_break) {
            return (void*)-1;
        }
        
        heap_current = new_break;
    }
    
    return (void*)old_break;
}

// Simple malloc implementation
void* malloc(size_t size) {
    size = (size + 7) & ~7;  // Align to 8 bytes
    
    void *ptr = sbrk(size);
    if (ptr == (void*)-1) {
        return NULL;
    }
    
    return ptr;
}

void free(void *ptr) {
    // Simple allocator - no free for now
    (void)ptr;
}