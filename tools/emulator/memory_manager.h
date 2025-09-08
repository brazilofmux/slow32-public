// SLOW-32 Memory Manager - Sparse memory allocation with hardware protection
#ifndef MEMORY_MANAGER_H
#define MEMORY_MANAGER_H

#include <stdint.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>
#include <unistd.h>

#define PAGE_SIZE 4096
#define PAGE_MASK (PAGE_SIZE - 1)
#define PAGE_ALIGN(x) (((x) + PAGE_MASK) & ~PAGE_MASK)

// Memory region descriptor
typedef struct memory_region {
    uint32_t vaddr_start;   // Virtual address start
    uint32_t vaddr_end;     // Virtual address end
    void *host_addr;        // Host memory pointer
    uint32_t size;          // Size in bytes
    int prot_flags;         // PROT_READ | PROT_WRITE | PROT_EXEC
    struct memory_region *next;
} memory_region_t;

// Memory manager state
typedef struct {
    memory_region_t *regions;   // Linked list of allocated regions
    uint32_t total_allocated;    // Total host memory allocated
    bool wxorx_enabled;          // W^X flag from .s32x (protection always active via mprotect)
} memory_manager_t;

// Initialize memory manager
static inline void mm_init(memory_manager_t *mm, bool wxorx) {
    mm->regions = NULL;
    mm->total_allocated = 0;
    mm->wxorx_enabled = wxorx;
}

// Allocate a memory region with specific protection
static memory_region_t* mm_allocate_region(memory_manager_t *mm, 
                                          uint32_t vaddr, 
                                          uint32_t size,
                                          int prot_flags) {
    // Allocating region
    // Align to page boundaries
    uint32_t aligned_vaddr = vaddr & ~PAGE_MASK;
    uint32_t end_addr = vaddr + size;
    uint32_t aligned_size = PAGE_ALIGN(end_addr - aligned_vaddr);
    
    // Allocate host memory with specified protection
    void *host_mem = mmap(NULL, aligned_size, prot_flags, 
                         MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
    if (host_mem == MAP_FAILED) {
        return NULL;
    }
    
    // Create region descriptor
    memory_region_t *region = malloc(sizeof(memory_region_t));
    if (!region) {
        munmap(host_mem, aligned_size);
        return NULL;
    }
    
    region->vaddr_start = aligned_vaddr;
    region->vaddr_end = aligned_vaddr + aligned_size;
    region->host_addr = host_mem;
    region->size = aligned_size;
    region->prot_flags = prot_flags;
    region->next = mm->regions;
    mm->regions = region;
    mm->total_allocated += aligned_size;
    
    // Region allocated successfully
    return region;
}

// Find region containing a virtual address
static memory_region_t* mm_find_region(memory_manager_t *mm, uint32_t vaddr) {
    for (memory_region_t *r = mm->regions; r; r = r->next) {
        if (vaddr >= r->vaddr_start && vaddr < r->vaddr_end) {
            return r;
        }
    }
    return NULL;
}

// Translate virtual address to host address
static inline void* mm_translate(memory_manager_t *mm, uint32_t vaddr) {
    memory_region_t *region = mm_find_region(mm, vaddr);
    if (!region) return NULL;
    
    uint32_t offset = vaddr - region->vaddr_start;
    return (uint8_t*)region->host_addr + offset;
}

// Read memory with protection checking
static inline int mm_read(memory_manager_t *mm, uint32_t vaddr, void *dest, size_t size) {
    memory_region_t *region = mm_find_region(mm, vaddr);
    if (!region) return -1;
    
    // Check bounds
    if (vaddr + size > region->vaddr_end) return -1;
    
    // Check read permission
    if (!(region->prot_flags & PROT_READ)) return -1;
    
    uint32_t offset = vaddr - region->vaddr_start;
    memcpy(dest, (uint8_t*)region->host_addr + offset, size);
    return 0;
}

// Write memory with protection checking
static inline int mm_write(memory_manager_t *mm, uint32_t vaddr, const void *src, size_t size) {
    memory_region_t *region = mm_find_region(mm, vaddr);
    if (!region) return -1;
    
    // Check bounds
    if (vaddr + size > region->vaddr_end) return -1;
    
    // Check write permission
    if (!(region->prot_flags & PROT_WRITE)) return -1;
    
    uint32_t offset = vaddr - region->vaddr_start;
    memcpy((uint8_t*)region->host_addr + offset, src, size);
    return 0;
}

// Allocate regions based on .s32x requirements
static int mm_setup_from_s32x(memory_manager_t *mm,
                              uint32_t code_limit,
                              uint32_t rodata_limit, 
                              uint32_t data_limit,
                              uint32_t stack_base) {
    // Setting up memory from s32x header
    fflush(stderr);
    // Code segment: initially read+write for loading, will become read-only
    if (code_limit > 0) {
        // Allocating code segment
        fflush(stderr);
        if (!mm_allocate_region(mm, 0, code_limit, PROT_READ | PROT_WRITE)) {
            // Failed to allocate code segment
            fflush(stderr);
            return -1;
        }
        // Code segment allocated
        fflush(stderr);
    }
    
    // Read-only data segment: initially read+write for loading
    // rodata starts immediately after code
    if (rodata_limit > code_limit) {
        // Allocating rodata segment
        fflush(stderr);
        if (!mm_allocate_region(mm, code_limit, rodata_limit - code_limit, PROT_READ | PROT_WRITE)) {
            // Failed to allocate rodata segment
            fflush(stderr);
            return -1;
        }
        // Rodata segment allocated
        fflush(stderr);
    }
    
    // Read-write data segment
    // Data starts immediately after rodata
    if (data_limit > rodata_limit) {
        // Allocating data segment
        fflush(stderr);
        if (!mm_allocate_region(mm, rodata_limit, data_limit - rodata_limit, 
                                PROT_READ | PROT_WRITE)) {
            // Failed to allocate data segment
            fflush(stderr);
            return -1;
        }
        // Data segment allocated
        fflush(stderr);
    }
    
    // Heap region - from end of data to stack base
    // Heap typically starts at 0x3000 in our memory layout
    uint32_t heap_start = (data_limit + 0xFFF) & ~0xFFF;  // Round up to page
    if (heap_start < 0x3000) heap_start = 0x3000;  // Minimum heap start
    
    if (stack_base > heap_start) {
        // Allocating heap segment
        fflush(stderr);
        if (!mm_allocate_region(mm, heap_start, stack_base - heap_start, PROT_READ | PROT_WRITE)) {
            // Failed to allocate heap segment
            fflush(stderr);
            return -1;
        }
        // Heap segment allocated
        fflush(stderr);
    }
    
    // Stack region - small stack around the stack pointer
    // Stack grows down, so allocate from heap end to beyond stack base
    uint32_t stack_size = 64 * 1024;  // 64KB stack
    uint32_t stack_end = stack_base + 0x1000;  // A bit above stack base
    // Allocating stack segment
    fflush(stderr);
    if (!mm_allocate_region(mm, stack_base, stack_end - stack_base, PROT_READ | PROT_WRITE)) {
        // Failed to allocate stack segment
        fflush(stderr);
        return -1;
    }
    // Stack segment allocated
    fflush(stderr);
    
    return 0;
}

// Finalize memory protection after loading
static int mm_protect_regions(memory_manager_t *mm, uint32_t code_limit, uint32_t rodata_limit) {
    for (memory_region_t *r = mm->regions; r; r = r->next) {
        int new_prot = r->prot_flags;
        
        // Code region: make read-only (hardware-enforced write protection)
        if (r->vaddr_start == 0 && r->vaddr_end >= code_limit) {
            new_prot = PROT_READ;
        }
        // Read-only data region
        else if (r->vaddr_start >= code_limit && r->vaddr_start < rodata_limit) {
            new_prot = PROT_READ;
        }
        // Data region stays read+write
        
        if (new_prot != r->prot_flags) {
            if (mprotect(r->host_addr, r->size, new_prot) != 0) {
                return -1;
            }
            r->prot_flags = new_prot;
        }
    }
    return 0;
}

// Clean up all allocated regions
static void mm_destroy(memory_manager_t *mm) {
    memory_region_t *r = mm->regions;
    while (r) {
        memory_region_t *next = r->next;
        munmap(r->host_addr, r->size);
        free(r);
        r = next;
    }
    mm->regions = NULL;
    mm->total_allocated = 0;
}

// Debug: print memory map
static void mm_print_map(memory_manager_t *mm) {
    // Print memory map
    fflush(stderr);
    printf("Memory Map (Total allocated: %u bytes):\n", mm->total_allocated);
    for (memory_region_t *r = mm->regions; r; r = r->next) {
        printf("  0x%08X-0x%08X (%u bytes) ", r->vaddr_start, r->vaddr_end, r->size);
        if (r->prot_flags & PROT_READ) printf("R");
        if (r->prot_flags & PROT_WRITE) printf("W");
        if (r->prot_flags & PROT_EXEC) printf("X");
        printf("\n");
    }
    // Memory map printed
    fflush(stderr);
}

#endif // MEMORY_MANAGER_H