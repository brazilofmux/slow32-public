/*
 * runtime.c - Runtime support for SLOW-32 lifted binaries
 *
 * Provides: main(), debug output, halt, and memory initialization.
 * Linked with the LLVM-compiled lifted IR to produce a native executable.
 */

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <stdbool.h>
#include "../emulator/mmio_ring.h"

/* Symbols defined in the lifted IR */
extern uint32_t __s32_mem_size;
extern uint32_t __s32_stack_base;
extern uint32_t __s32_mmio_base;
extern void s32_init_memory(uint8_t *mem);
extern void s32_entry(uint8_t *mem);
extern uint32_t regs[32];

/* Global MMIO state */
static mmio_ring_state_t mmio_state;
static uint8_t *mem_ptr;
static uint64_t mem_limit = 0;
static bool cpu_halted = false;
static uint32_t cpu_exit_status = 0;

/* Bounds checking for guest memory access */
void s32_check_bounds(uint32_t addr, uint32_t size, uint32_t is_write, uint32_t pc, uint32_t sp) {
    uint64_t limit = mem_limit;
    uint64_t end = (uint64_t)addr + (uint64_t)size;
    if (end > limit) {
        if (is_write) {
            fprintf(stdout,
                    "Memory fault: Failed to write %u bytes at 0x%08x (PC=0x%08x SP=0x%08x)\n",
                    size, addr, pc, sp);
        } else {
            fprintf(stdout, "Memory fault: Failed to read %u bytes at 0x%08x\n", size, addr);
        }
        exit(139); /* SIGSEGV */
    }
}

/* Called by lifted code for DEBUG instruction */
void s32_debug(uint32_t val) {
    putchar(val & 0xFF);
}

/* Called by lifted code for HALT instruction */
void s32_halt(uint32_t code) {
    uint32_t exit_code = cpu_exit_status ? cpu_exit_status : code;
    exit(exit_code);
}

/* Called when dispatch fails (unknown target) */
void s32_dispatch_fail(uint32_t target) {
    fprintf(stderr, "s32-lift: dispatch failed, unknown target 0x%x\n", target);
    exit(1);
}

/* MMIO Service Routine */
void s32_mmio_service(void) {
    if (!mem_ptr || __s32_mmio_base == 0) return;

    /* Sync indices from guest memory to host state */
    mmio_state.req_head = *(uint32_t*)(mem_ptr + __s32_mmio_base + S32_MMIO_REQ_HEAD_OFFSET);
    mmio_state.resp_tail = *(uint32_t*)(mem_ptr + __s32_mmio_base + S32_MMIO_RESP_TAIL_OFFSET);

    /* Process requests */
    mmio_cpu_iface_t iface = { .halted = &cpu_halted, .exit_status = &cpu_exit_status };
    mmio_ring_process(&mmio_state, &iface);

    /* Sync indices back to guest memory */
    *(uint32_t*)(mem_ptr + __s32_mmio_base + S32_MMIO_REQ_TAIL_OFFSET) = mmio_state.req_tail;
    *(uint32_t*)(mem_ptr + __s32_mmio_base + S32_MMIO_RESP_HEAD_OFFSET) = mmio_state.resp_head;

    if (cpu_exit_status != 0) {
        regs[1] = cpu_exit_status;
    }

    if (cpu_halted) {
        exit(cpu_exit_status);
    }
}

int main(int argc, char **argv) {
    /* SLOW-32 memory layout:
     * RAM followed by MMIO (if enabled)
     */
    uint64_t msz = __s32_mem_size;
    if (msz == 0) msz = 0x10000000; // 256MB default
    
    /* If MMIO is enabled, ensure we have space for it */
    if (__s32_mmio_base != 0) {
        uint64_t mmio_end = (uint64_t)__s32_mmio_base + 0x10000u;
        if (mmio_end > msz) msz = mmio_end;
    }

    uint8_t *mem = calloc(1, (size_t)msz);
    if (!mem) {
        fprintf(stderr, "Failed to allocate %llu bytes of memory\n",
                (unsigned long long)msz);
        return 1;
    }
    mem_ptr = mem;
    mem_limit = msz;

    /* Initialize MMIO if enabled */
    if (__s32_mmio_base != 0) {
        mmio_ring_init(&mmio_state, 0x04000000, 0x00800000); // 64MB heap start, 8MB size
        mmio_state.base_addr = __s32_mmio_base;
        mmio_state.req_ring = (io_descriptor_t*)(mem + __s32_mmio_base + S32_MMIO_REQ_RING_OFFSET);
        mmio_state.resp_ring = (io_descriptor_t*)(mem + __s32_mmio_base + S32_MMIO_RESP_RING_OFFSET);
        mmio_state.data_buffer = (mem + __s32_mmio_base + S32_MMIO_DATA_BUFFER_OFFSET);
        mmio_state.guest_mem_base = mem;
        mmio_state.guest_mem_size = __s32_mmio_base;

        /* Setup guest arguments (skip host program name) */
        if (argc > 1) {
            mmio_ring_set_args(&mmio_state, argc - 1, argv + 1);
        } else {
            mmio_ring_set_args(&mmio_state, 0, NULL);
        }
        
        /* Setup host environment */
        extern char **environ;
        mmio_ring_set_envp(&mmio_state, environ);
    }

    /* Copy data/rodata sections into the simulated memory */
    s32_init_memory(mem);

    /* Run the lifted program */
    s32_entry(mem);

    free(mem);
    return 0;
}
