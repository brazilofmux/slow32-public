// SLOW-32 MMIO ring buffer flush implementation
// This is the backend for the MMIO version of libc

#include <stddef.h>
#include <stdint.h>

#include "mmio_ring.h"

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

// Helper to write a descriptor to the request ring
static void write_descriptor(volatile uint32_t *ring_base, uint32_t index,
                             uint32_t opcode, uint32_t length,
                             uint32_t offset, uint32_t status) {
    uint32_t desc_offset = index * S32_MMIO_DESC_WORDS;
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

    // Get ring buffer pointers
    volatile uint32_t *req_head_ptr = s32_mmio_reg32_ptr(S32_MMIO_REQ_HEAD_OFFSET);
    volatile uint32_t *req_tail_ptr = s32_mmio_reg32_ptr(S32_MMIO_REQ_TAIL_OFFSET);
    volatile uint32_t *req_ring = S32_MMIO_REQ_RING;
    volatile uint8_t *data_buffer = S32_MMIO_DATA_BUFFER;

    // Wait for space in request ring
    uint32_t req_head = *req_head_ptr;
    uint32_t req_tail = *req_tail_ptr;
    uint32_t next_head = (req_head + 1u) % S32_MMIO_RING_ENTRIES;

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
    write_descriptor(req_ring, req_head, S32_MMIO_OP_WRITE, (uint32_t)stream->count, 0u, (uint32_t)stream->fd);

    // Advance head
    *req_head_ptr = next_head;

    // Yield to let emulator process the request
    __asm__ volatile("YIELD r0, r0, r0");

    // Reset buffer
    stream->ptr = stream->buffer;
    stream->count = 0;
}