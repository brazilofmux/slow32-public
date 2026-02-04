#include <stdint.h>
#include <string.h>

int main(void) {
    volatile uint8_t *p = (uint8_t *)0x00001000u;
    for (int i = 0; i < 256; i++) {
        p[i] = 'A';
    }

    // No terminator in the first 256 bytes; bounded strlen should fault.
    (void)strlen((const char *)p);
    return 0;
}
