// SLOW-32 Ring Buffer MMIO Implementation
#ifndef MMIO_RING_H
#define MMIO_RING_H

#include <stdint.h>
#include <stdbool.h>
#include <dirent.h>

#include "../../common/mmio_ring_layout.h"

#define S32_MMIO_MAX_FDS 128

// Service negotiation limits
#define S32_MAX_SERVICES     16
#define S32_MAX_SVC_NAME     32

// File descriptor type tracking
typedef enum {
    S32_FD_TYPE_FILE = 0,
    S32_FD_TYPE_DIR  = 1,
} s32_fd_type_t;

// I/O descriptor structure
typedef struct {
    uint32_t opcode;    // Operation type
    uint32_t length;    // Data length
    uint32_t offset;    // Offset in data buffer
    uint32_t status;    // fd for I/O ops, result/error for responses
} io_descriptor_t;

// Operation codes for I/O (prefixed to avoid conflicts)
typedef enum s32_mmio_opcode io_opcode_t;

// Forward declarations
struct cpu_state;
typedef struct mmio_ring_state mmio_ring_state_t;

// Service session (one per granted service)
typedef struct {
    bool active;
    char name[S32_MAX_SVC_NAME];
    uint32_t base_opcode;
    uint32_t opcode_count;
    uint32_t version;
    void *state;
    void (*cleanup)(void *state);
    void (*handle)(void *state, mmio_ring_state_t *mmio,
                   uint32_t sub_opcode, io_descriptor_t *req,
                   io_descriptor_t *resp);
} svc_session_t;

// Policy engine
typedef struct {
    bool default_allow;
    char allow_list[S32_MAX_SERVICES][S32_MAX_SVC_NAME];
    int allow_count;
    char deny_list[S32_MAX_SERVICES][S32_MAX_SVC_NAME];
    int deny_count;
} svc_policy_t;

// MMIO ring buffer state
struct mmio_ring_state {
    // Ring indices
    uint32_t req_head;      // Request producer (CPU writes)
    uint32_t req_tail;      // Request consumer (device reads)
    uint32_t resp_head;     // Response producer (device writes)
    uint32_t resp_tail;     // Response consumer (CPU reads)

    // Base address of the MMIO window in guest memory
    uint32_t base_addr;

    // Guest memory range for Direct I/O (optional)
    void *guest_mem_base;
    uint32_t guest_mem_size;

    // Ring buffers (allocated as part of MMIO memory)
    io_descriptor_t *req_ring;
    io_descriptor_t *resp_ring;
    uint8_t *data_buffer;

    // Statistics
    uint64_t total_requests;
    uint64_t total_responses;

    // Cached host argument data
    uint32_t args_argc;
    uint32_t args_total_bytes;
    uint8_t *args_blob;

    // Cached host environment data
    uint32_t envp_envc;
    uint32_t envp_total_bytes;
    uint8_t *envp_blob;

    int host_fds[S32_MMIO_MAX_FDS];
    bool host_fd_owned[S32_MMIO_MAX_FDS];
    s32_fd_type_t fd_types[S32_MMIO_MAX_FDS];
    DIR *host_dirs[S32_MMIO_MAX_FDS];

    // Service negotiation
    svc_session_t services[S32_MAX_SERVICES];
    int num_services;
    svc_policy_t policy;
    uint32_t next_dynamic_opcode;  // Next available opcode for service allocation
};

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
void mmio_ring_init(mmio_ring_state_t *mmio);

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

// Environment management helpers
int mmio_ring_set_envp(mmio_ring_state_t *mmio,
                       char *const *envp);

void mmio_ring_clear_envp(mmio_ring_state_t *mmio);

// Service negotiation / policy
void mmio_set_policy(mmio_ring_state_t *mmio, const svc_policy_t *policy);
void mmio_cleanup_services(mmio_ring_state_t *mmio);
bool mmio_policy_allows(mmio_ring_state_t *mmio, const char *service_name);

#endif // MMIO_RING_H
