// SLOW-32 MMIO ring buffer flush implementation
// This is the backend for the MMIO version of libc

#include <stddef.h>
#include <stdint.h>

// FILE structure (must match stdio_buffered.c)
typedef struct FILE {
    char *buffer;
    char *ptr;
    size_t count;
    size_t size;
    int mode;
    int fd;
    int flags;
    void (*flush_fn)(struct FILE *);
} FILE;

// Linker-provided MMIO symbols
extern uint32_t __mmio_base;
extern uint32_t __mmio_end;

// Ring buffer layout (relative to __mmio_base)
#define REQ_HEAD_OFFSET      0x0000
#define REQ_TAIL_OFFSET      0x0004
#define REQ_RING_OFFSET      0x1000
#define RESP_HEAD_OFFSET     0x2000
#define RESP_TAIL_OFFSET     0x2004
#define RESP_RING_OFFSET     0x3000
#define DATA_BUFFER_OFFSET   0x4000

#define RING_SIZE     256
#define DESC_SIZE     4  // 4 words per descriptor

// Operation codes
#define OP_WRITE      0x03

// Helper to write a descriptor to the request ring
static void write_descriptor(volatile uint32_t *ring_base, uint32_t index, 
                             uint32_t opcode, uint32_t length, 
                             uint32_t offset, uint32_t status) {
    uint32_t desc_offset = index * DESC_SIZE;
    ring_base[desc_offset + 0] = opcode;
    ring_base[desc_offset + 1] = length;
    ring_base[desc_offset + 2] = offset;
    ring_base[desc_offset + 3] = status;
}

// Flush buffer using MMIO ring buffer
void __flush_mmio(FILE *stream) {
    if (!stream || stream->count == 0) {
        return;
    }
    
    volatile uint32_t *mmio = (volatile uint32_t *)&__mmio_base;
    
    // Get ring buffer pointers
    volatile uint32_t *req_head_ptr = &mmio[REQ_HEAD_OFFSET / 4];
    volatile uint32_t *req_tail_ptr = &mmio[REQ_TAIL_OFFSET / 4];
    volatile uint32_t *req_ring = &mmio[REQ_RING_OFFSET / 4];
    volatile uint8_t *data_buffer = (volatile uint8_t *)&mmio[DATA_BUFFER_OFFSET / 4];
    
    // Wait for space in request ring
    uint32_t req_head = *req_head_ptr;
    uint32_t req_tail = *req_tail_ptr;
    uint32_t next_head = (req_head + 1) % RING_SIZE;
    
    while (next_head == req_tail) {
        // Ring full, yield and retry
        __asm__ volatile("YIELD r0, r0, r0");
        req_tail = *req_tail_ptr;
    }
    
    // Copy data to MMIO data buffer
    // Using offset 0 for simplicity - could manage multiple buffers
    for (size_t i = 0; i < stream->count; i++) {
        data_buffer[i] = stream->buffer[i];
    }
    
    // Write descriptor
    write_descriptor(req_ring, req_head, OP_WRITE, stream->count, 0, stream->fd);
    
    // Advance head
    *req_head_ptr = next_head;
    
    // Yield to let emulator process the request
    __asm__ volatile("YIELD r0, r0, r0");
    
    // Reset buffer
    stream->ptr = stream->buffer;
    stream->count = 0;
}