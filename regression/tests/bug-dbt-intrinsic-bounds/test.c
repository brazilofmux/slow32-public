#include <stdint.h>
#include <stddef.h>
#include <string.h>

extern void memswap(void *a, void *b, size_t n);

int main(void) {
    register uint32_t exit_code asm("r1") = 77;
    asm volatile("" : "+r"(exit_code));

    volatile uint8_t *a = (uint8_t *)0x20000000u;
    volatile uint8_t *b = (uint8_t *)0x20001000u;

    memswap((void *)a, (void *)b, 16);

    // If we somehow don't fault, halt with a different exit code.
    return 0;
}
