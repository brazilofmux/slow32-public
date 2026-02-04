#include <stdint.h>
#include <string.h>

int main(void) {
    volatile uint8_t *dst = (uint8_t *)0x00001000u;
    volatile uint8_t *src = (uint8_t *)0x20000000u;

    memcpy((void *)dst, (const void *)src, 32);
    return 0;
}
