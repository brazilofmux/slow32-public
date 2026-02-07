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

/* Symbols defined in the lifted IR */
extern uint32_t __s32_mem_size;
extern uint32_t __s32_stack_base;
extern void s32_init_memory(uint8_t *mem);
extern void s32_entry(uint8_t *mem);

/* Called by lifted code for DEBUG instruction */
void s32_debug(uint32_t val) {
    putchar(val & 0xFF);
}

/* Called by lifted code for HALT instruction */
void s32_halt(void) {
    exit(0);
}

/* Called when dispatch fails (unknown target) */
void s32_dispatch_fail(uint32_t target) {
    fprintf(stderr, "s32-lift: dispatch failed, unknown target 0x%x\n", target);
    exit(1);
}

int main(int argc, char **argv) {
    uint32_t msz = __s32_mem_size;
    if (msz == 0) msz = 16 * 1024 * 1024;  /* 16MB default */

    uint8_t *mem = calloc(1, msz);
    if (!mem) {
        fprintf(stderr, "Failed to allocate %u bytes of memory\n", msz);
        return 1;
    }

    /* Copy data/rodata sections into the simulated memory */
    s32_init_memory(mem);

    /* Run the lifted program */
    s32_entry(mem);

    free(mem);
    return 0;
}
