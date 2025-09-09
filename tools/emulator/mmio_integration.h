// MMIO Integration for SLOW-32 Emulator
#ifndef MMIO_INTEGRATION_H
#define MMIO_INTEGRATION_H

#include "slow32.h"
#include "mmio_ring.h"
#include "memory_manager.h"

// Extended CPU state with MMIO
typedef struct {
    cpu_state_t base;           // Original CPU state
    mmio_ring_state_t mmio;     // MMIO ring buffer state
    void *mmio_mem;             // Host pointer to MMIO memory
    bool mmio_enabled;          // MMIO enabled flag
} cpu_state_ext_t;

// Initialize CPU with MMIO support
static inline void cpu_init_with_mmio(cpu_state_ext_t *cpu) {
    // Initialize base CPU
    cpu_init(&cpu->base);
    
    // Initialize MMIO if requested
    cpu->mmio_enabled = false;
    cpu->mmio_mem = NULL;
}

// Enable MMIO for the CPU
static inline bool cpu_enable_mmio(cpu_state_ext_t *cpu, uint32_t heap_base, uint32_t heap_size) {
    // Initialize MMIO state
    mmio_ring_init(&cpu->mmio, heap_base, heap_size);
    
    // Map MMIO memory
    cpu->mmio_mem = mmio_ring_map(&cpu->mmio);
    if (!cpu->mmio_mem) {
        return false;
    }
    
    // Add MMIO as a memory region
    memory_region_t *mmio_region = mm_allocate_region(&cpu->base.mm, 
                                                      MMIO_BASE, 
                                                      0x10000,  // 64KB
                                                      PROT_READ | PROT_WRITE);
    if (!mmio_region) {
        munmap(cpu->mmio_mem, 0x10000);
        return false;
    }
    
    // Point the region to our MMIO memory
    mmio_region->host_addr = cpu->mmio_mem;
    
    cpu->mmio_enabled = true;
    return true;
}

// Memory read with MMIO support
static inline int cpu_mem_read(cpu_state_ext_t *cpu, uint32_t addr, void *dest, size_t size) {
    // Check if this is MMIO
    if (cpu->mmio_enabled && addr >= MMIO_BASE && addr < MMIO_BASE + 0x10000) {
        if (size == 4) {
            *(uint32_t*)dest = mmio_ring_read(&cpu->mmio, addr, size);
            return 0;
        } else if (size == 1) {
            // Byte read from MMIO
            uint32_t word = mmio_ring_read(&cpu->mmio, addr & ~3, 4);
            *(uint8_t*)dest = (word >> ((addr & 3) * 8)) & 0xFF;
            return 0;
        }
        return -1;  // Unsupported size
    }
    
    // Normal memory read
    return mm_read(&cpu->base.mm, addr, dest, size);
}

// Memory write with MMIO support
static inline int cpu_mem_write(cpu_state_ext_t *cpu, uint32_t addr, const void *src, size_t size) {
    // Check if this is MMIO
    if (cpu->mmio_enabled && addr >= MMIO_BASE && addr < MMIO_BASE + 0x10000) {
        if (size == 4) {
            mmio_ring_write(&cpu->mmio, &cpu->base, addr, *(uint32_t*)src, size);
            return 0;
        } else if (size == 1) {
            // Byte write to MMIO (read-modify-write for simplicity)
            uint32_t word = mmio_ring_read(&cpu->mmio, addr & ~3, 4);
            uint8_t byte = *(uint8_t*)src;
            int shift = (addr & 3) * 8;
            word = (word & ~(0xFF << shift)) | (byte << shift);
            mmio_ring_write(&cpu->mmio, &cpu->base, addr & ~3, word, 4);
            return 0;
        }
        return -1;  // Unsupported size
    }
    
    // Normal memory write
    return mm_write(&cpu->base.mm, addr, src, size);
}

// Process MMIO requests (call in main loop)
static inline void cpu_process_mmio(cpu_state_ext_t *cpu) {
    if (cpu->mmio_enabled && mmio_has_requests(&cpu->mmio)) {
        mmio_ring_process(&cpu->mmio, &cpu->base);
    }
}

// Cleanup
static inline void cpu_destroy_with_mmio(cpu_state_ext_t *cpu) {
    if (cpu->mmio_mem) {
        munmap(cpu->mmio_mem, 0x10000);
    }
    cpu_destroy(&cpu->base);
}

#endif // MMIO_INTEGRATION_H