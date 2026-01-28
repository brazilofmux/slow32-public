// SLOW-32 Emulator with MMIO Support
// This is a modified version of slow32.c that includes ring buffer MMIO

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <getopt.h>
#include <time.h>
#include <inttypes.h>
#include "slow32.h"
#include "s32x_loader.h"
#include "memory_manager.h"
#include "mmio_ring.h"

// Enable to trap on unaligned LD/ST
#ifndef S32_ALLOW_UNALIGNED
#define S32_TRAP_ON_UNALIGNED 1
#endif

// Extended CPU state with MMIO
typedef struct {
    cpu_state_t cpu;           // Base CPU state
    mmio_ring_state_t mmio;    // MMIO ring buffer state
    void *mmio_mem;            // Host pointer to MMIO memory
    bool mmio_enabled;         // MMIO enabled flag
} cpu_state_mmio_t;

// Forward declarations from slow32.c
static instruction_t decode_instruction(uint32_t raw);
static uint32_t cpu_fetch(cpu_state_t *cpu, uint32_t addr);
static void cpu_step(cpu_state_t *cpu);

// Initialize CPU with MMIO support
static void cpu_init_mmio(cpu_state_mmio_t *cpu_ext, bool enable_mmio) {
    cpu_init(&cpu_ext->cpu);
    cpu_ext->mmio_enabled = enable_mmio;
    cpu_ext->mmio_mem = NULL;
    
    if (enable_mmio) {
        // Initialize MMIO with default heap at 64MB, 8MB size
        mmio_ring_init(&cpu_ext->mmio, 0x04000000, 0x00800000);
        
        // Map MMIO memory
        cpu_ext->mmio_mem = mmio_ring_map(&cpu_ext->mmio);
        if (!cpu_ext->mmio_mem) {
            fprintf(stderr, "Failed to map MMIO memory\n");
            cpu_ext->mmio_enabled = false;
            mmio_ring_clear_args(&cpu_ext->mmio);
            return;
        }
        
        // Add MMIO as a memory region
        memory_region_t *mmio_region = mm_allocate_region(&cpu_ext->cpu.mm, 
                                                          MMIO_BASE, 
                                                          0x10000,  // 64KB
                                                          PROT_READ | PROT_WRITE);
        if (!mmio_region) {
            munmap(cpu_ext->mmio_mem, 0x10000);
            cpu_ext->mmio_enabled = false;
            mmio_ring_clear_args(&cpu_ext->mmio);
            return;
        }
        
        mmio_region->host_addr = cpu_ext->mmio_mem;
    }
}

// Memory operations with MMIO support
static int cpu_mem_read_mmio(cpu_state_mmio_t *cpu_ext, uint32_t addr, uint32_t *value, int size) {
    // Check if this is MMIO
    if (cpu_ext->mmio_enabled && addr >= MMIO_BASE && addr < MMIO_BASE + 0x10000) {
        if (size == 4) {
            *value = mmio_ring_read(&cpu_ext->mmio, addr, size);
            return 0;
        }
        return -1;  // Only 32-bit MMIO reads for now
    }
    
    // Normal memory read
    return mm_read(&cpu_ext->cpu.mm, addr, value, size);
}

static int cpu_mem_write_mmio(cpu_state_mmio_t *cpu_ext, uint32_t addr, uint32_t value, int size) {
    // Check if this is MMIO
    if (cpu_ext->mmio_enabled && addr >= MMIO_BASE && addr < MMIO_BASE + 0x10000) {
        if (size == 4) {
            mmio_ring_write(&cpu_ext->mmio, NULL, addr, value, size);
            return 0;
        }
        return -1;  // Only 32-bit MMIO writes for now
    }
    
    // Normal memory write
    return mm_write(&cpu_ext->cpu.mm, addr, &value, size);
}

// Modified cpu_step with MMIO support at YIELD/HALT
static void cpu_step_mmio(cpu_state_mmio_t *cpu_ext) {
    cpu_state_t *cpu = &cpu_ext->cpu;
    
    if (cpu->halted) return;
    
    // Fetch and decode instruction
    uint32_t inst_raw = cpu_fetch(cpu, cpu->pc);
    instruction_t inst = decode_instruction(inst_raw);
    
    // Execute instruction (simplified - showing only YIELD/HALT)
    switch (inst.opcode) {
        case OP_YIELD:
            // YIELD is the ring buffer sync point
            if (cpu_ext->mmio_enabled) {
                mmio_cpu_iface_t iface = { .halted = &cpu->halted, .exit_status = &cpu->regs[REG_R1] };
                mmio_ring_process(&cpu_ext->mmio, &iface);
            }
            cpu->cycle_count++;
            break;
            
        case OP_HALT:
            // Process any final I/O before halting
            if (cpu_ext->mmio_enabled) {
                mmio_cpu_iface_t iface = { .halted = &cpu->halted, .exit_status = &cpu->regs[REG_R1] };
                mmio_ring_process(&cpu_ext->mmio, &iface);
            }
            cpu->halted = true;
            break;
            
        default:
            // Execute other instructions normally
            cpu_step(cpu);
            return;  // cpu_step already incremented PC
    }
    
    // Update PC for YIELD/HALT
    cpu->pc += 4;
    cpu->inst_count++;
}

// Main function with MMIO support
int main(int argc, char *argv[]) {
    cpu_state_mmio_t cpu_ext = {0};
    bool enable_mmio = false;
    bool verbose = false;
    char *filename = NULL;
    
    // Parse command line
    int opt;
    while ((opt = getopt(argc, argv, "mvh")) != -1) {
        switch (opt) {
            case 'm':
                enable_mmio = true;
                break;
            case 'v':
                verbose = true;
                break;
            case 'h':
                printf("Usage: %s [-m] [-v] program.s32x [-- <args...>]\n", argv[0]);
                printf("  -m  Enable MMIO ring buffers\n");
                printf("  -v  Verbose output\n");
                return 0;
            default:
                fprintf(stderr, "Unknown option: %c\n", opt);
                return 1;
        }
    }
    
    if (optind >= argc) {
        fprintf(stderr, "Error: No input file specified\n");
        return 1;
    }
    
    filename = argv[optind];
    int guest_argc = argc - optind;
    char **guest_argv = &argv[optind];
    
    // Initialize CPU with optional MMIO
    cpu_init_mmio(&cpu_ext, enable_mmio);
    
    if (enable_mmio && verbose) {
        printf("MMIO enabled at 0x%08X\n", MMIO_BASE);
    }
    
    // Load program
    if (!cpu_load_binary(&cpu_ext.cpu, filename)) {
        fprintf(stderr, "Failed to load program\n");
        return 1;
    }

    if (cpu_ext.mmio_enabled) {
        if (mmio_ring_set_args(&cpu_ext.mmio, (uint32_t)guest_argc, guest_argv) != 0) {
            fprintf(stderr, "Error: unable to stage guest arguments (too many bytes?)\n");
            if (cpu_ext.mmio_mem) {
                munmap(cpu_ext.mmio_mem, 0x10000);
                cpu_ext.mmio_mem = NULL;
            }
            mmio_ring_clear_args(&cpu_ext.mmio);
            cpu_destroy(&cpu_ext.cpu);
            return 1;
        }
        // Pass through host environment to guest
        extern char **environ;
        if (mmio_ring_set_envp(&cpu_ext.mmio, environ) != 0) {
            fprintf(stderr, "Warning: unable to stage host environment (too many bytes?)\n");
            // Non-fatal - continue without environment
        }
    } else if (guest_argc > 1) {
        fprintf(stderr, "Warning: guest arguments ignored because MMIO is disabled.\n");
    }
    
    // Run program
    if (verbose) {
        printf("Starting execution%s\n", enable_mmio ? " with MMIO" : "");
    }
    
    clock_t start = clock();
    
    while (!cpu_ext.cpu.halted) {
        cpu_step_mmio(&cpu_ext);
        
        // Optional: Process MMIO every N instructions for responsiveness
        if (enable_mmio && (cpu_ext.cpu.inst_count % 1000) == 0) {
            mmio_cpu_iface_t iface = { .halted = &cpu_ext.cpu.halted, .exit_status = &cpu_ext.cpu.regs[REG_R1] };
            mmio_ring_process(&cpu_ext.mmio, &iface);
        }
    }
    
    clock_t end = clock();
    double elapsed = (double)(end - start) / CLOCKS_PER_SEC;
    
    // Print statistics
    if (verbose) {
        printf("\nProgram halted.\n");
        printf("Instructions executed: %" PRIu64 "\n", cpu_ext.cpu.inst_count);
        printf("Cycles: %" PRIu64 "\n", cpu_ext.cpu.cycle_count);
        printf("Wall time: %.6f seconds\n", elapsed);
        printf("Performance: %.2f MIPS\n", cpu_ext.cpu.inst_count / (elapsed * 1e6));
        
        if (enable_mmio) {
            printf("MMIO requests: %" PRIu64 "\n", cpu_ext.mmio.total_requests);
            printf("MMIO responses: %" PRIu64 "\n", cpu_ext.mmio.total_responses);
        }
    }
    
    int exit_code = cpu_ext.cpu.regs[REG_R1];

    // Cleanup
    if (cpu_ext.mmio_mem) {
        munmap(cpu_ext.mmio_mem, 0x10000);
        cpu_ext.mmio_mem = NULL;
    }
    mmio_ring_clear_args(&cpu_ext.mmio);
    cpu_destroy(&cpu_ext.cpu);
    
    return exit_code;
}

// Include the rest of slow32.c implementation
// (decode_instruction, cpu_fetch, cpu_step, etc.)
// These functions remain unchanged from the original
