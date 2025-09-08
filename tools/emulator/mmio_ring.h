// SLOW-32 Ring Buffer MMIO Implementation
#ifndef MMIO_RING_H
#define MMIO_RING_H

#include <stdint.h>
#include <stdbool.h>

// MMIO memory map - matches what's already defined in slow32.h
#define MMIO_BASE     0x10000000
#define REQ_HEAD      (MMIO_BASE + 0x0000)
#define REQ_TAIL      (MMIO_BASE + 0x0004)
#define REQ_RING      (MMIO_BASE + 0x1000)
#define RESP_HEAD     (MMIO_BASE + 0x2000)
#define RESP_TAIL     (MMIO_BASE + 0x2004)
#define RESP_RING     (MMIO_BASE + 0x3000)
#define DATA_BUFFER   (MMIO_BASE + 0x4000)

// Ring parameters
#define RING_SIZE     256           // Number of descriptors
#define DESC_SIZE     16            // Bytes per descriptor
#define DATA_BUF_SIZE (56 * 1024)   // 56KB data buffer

// I/O descriptor structure
typedef struct {
    uint32_t opcode;    // Operation type
    uint32_t length;    // Data length
    uint32_t offset;    // Offset in data buffer
    uint32_t status;    // fd for I/O ops, result/error for responses
} io_descriptor_t;

// Operation codes for I/O (prefixed to avoid conflicts)
typedef enum {
    IO_OP_NOP        = 0x00,
    IO_OP_PUTCHAR    = 0x01,
    IO_OP_GETCHAR    = 0x02,
    IO_OP_WRITE      = 0x03,
    IO_OP_READ       = 0x04,
    IO_OP_OPEN       = 0x05,
    IO_OP_CLOSE      = 0x06,
    IO_OP_SEEK       = 0x07,
    IO_OP_BRK        = 0x08,
    IO_OP_EXIT       = 0x09,
    IO_OP_STAT       = 0x0A,
    IO_OP_FLUSH      = 0x0B,
} io_opcode_t;

// Forward declaration
struct cpu_state;

// MMIO ring buffer state
typedef struct {
    // Ring indices
    uint32_t req_head;      // Request producer (CPU writes)
    uint32_t req_tail;      // Request consumer (device reads)
    uint32_t resp_head;     // Response producer (device writes)
    uint32_t resp_tail;     // Response consumer (CPU reads)
    
    // Ring buffers (allocated as part of MMIO memory)
    io_descriptor_t *req_ring;
    io_descriptor_t *resp_ring;
    uint8_t *data_buffer;
    
    // Heap management
    uint32_t brk_current;
    uint32_t brk_max;
    
    // Statistics
    uint64_t total_requests;
    uint64_t total_responses;
} mmio_ring_state_t;

// Initialize MMIO ring buffers
void mmio_ring_init(mmio_ring_state_t *mmio, uint32_t heap_base, uint32_t heap_size);

// Map MMIO memory region
void* mmio_ring_map(mmio_ring_state_t *mmio);

// MMIO read/write handlers
uint32_t mmio_ring_read(mmio_ring_state_t *mmio, uint32_t addr, int size);
void mmio_ring_write(mmio_ring_state_t *mmio, struct cpu_state *cpu, uint32_t addr, uint32_t value, int size);

// Process pending requests (called by emulator main loop)
void mmio_ring_process(mmio_ring_state_t *mmio, struct cpu_state *cpu);

// Check if there are pending requests
static inline bool mmio_has_requests(mmio_ring_state_t *mmio) {
    return mmio->req_head != mmio->req_tail;
}

// Ring buffer utilities
static inline bool ring_full(uint32_t head, uint32_t tail) {
    return ((head + 1) % RING_SIZE) == tail;
}

static inline bool ring_empty(uint32_t head, uint32_t tail) {
    return head == tail;
}

static inline uint32_t ring_next(uint32_t index) {
    return (index + 1) % RING_SIZE;
}

#endif // MMIO_RING_H