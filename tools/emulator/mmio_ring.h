// SLOW-32 Ring Buffer MMIO Implementation
#ifndef MMIO_RING_H
#define MMIO_RING_H

#include <stdint.h>
#include <stdbool.h>

#include "../../common/mmio_ring_layout.h"

#define S32_MMIO_MAX_FDS 128

// I/O descriptor structure
typedef struct {
    uint32_t opcode;    // Operation type
    uint32_t length;    // Data length
    uint32_t offset;    // Offset in data buffer
    uint32_t status;    // fd for I/O ops, result/error for responses
} io_descriptor_t;

// Operation codes for I/O (prefixed to avoid conflicts)
typedef enum s32_mmio_opcode io_opcode_t;

// Forward declaration
struct cpu_state;

// MMIO ring buffer state
typedef struct {
    // Ring indices
    uint32_t req_head;      // Request producer (CPU writes)
    uint32_t req_tail;      // Request consumer (device reads)
    uint32_t resp_head;     // Response producer (device writes)
    uint32_t resp_tail;     // Response consumer (CPU reads)

    // Base address of the MMIO window in guest memory
    uint32_t base_addr;
    
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

    // Cached host argument data
    uint32_t args_argc;
    uint32_t args_total_bytes;
    uint8_t *args_blob;

    int host_fds[S32_MMIO_MAX_FDS];
    bool host_fd_owned[S32_MMIO_MAX_FDS];
} mmio_ring_state_t;

// Common MMIO configuration shared by both emulators
typedef struct {
    bool enabled;
    bool initialized;
    uint32_t base;
    mmio_ring_state_t *state;
    void *mem;
} mmio_device_t;

// Minimal host-facing interface exposed to MMIO helpers
typedef struct {
    bool *halted;
    uint32_t *exit_status;  // optional pointer to place exit code
} mmio_cpu_iface_t;

// Initialize MMIO ring buffers
void mmio_ring_init(mmio_ring_state_t *mmio, uint32_t heap_base, uint32_t heap_size);

// Map MMIO memory region
void* mmio_ring_map(mmio_ring_state_t *mmio);

// MMIO read/write handlers
uint32_t mmio_ring_read(mmio_ring_state_t *mmio, uint32_t addr, int size);
void mmio_ring_write(mmio_ring_state_t *mmio, mmio_cpu_iface_t *cpu, uint32_t addr, uint32_t value, int size);

// Process pending requests (called by emulator main loop)
void mmio_ring_process(mmio_ring_state_t *mmio, mmio_cpu_iface_t *cpu);

// Check if there are pending requests
static inline bool mmio_has_requests(mmio_ring_state_t *mmio) {
    return mmio->req_head != mmio->req_tail;
}

// Ring buffer utilities
static inline bool ring_full(uint32_t head, uint32_t tail) {
    return ((head + 1u) % S32_MMIO_RING_ENTRIES) == tail;
}

static inline bool ring_empty(uint32_t head, uint32_t tail) {
    return head == tail;
}

static inline uint32_t ring_next(uint32_t index) {
    return (index + 1u) % S32_MMIO_RING_ENTRIES;
}

// Argument management helpers
int mmio_ring_set_args(mmio_ring_state_t *mmio,
                       uint32_t argc,
                       char *const *argv);

void mmio_ring_clear_args(mmio_ring_state_t *mmio);

#endif // MMIO_RING_H
