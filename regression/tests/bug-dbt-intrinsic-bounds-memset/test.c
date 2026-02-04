#include <stdint.h>
#include <string.h>

int main(void) {
    volatile uint8_t *dst = (uint8_t *)0x20000000u;

    memset((void *)dst, 0xAA, 16);
    return 0;
}
