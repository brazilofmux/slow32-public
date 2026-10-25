/* Linked by run-tests.sh with s32-ld --heap-size 96M: a 72 MB block
 * must then fit below the MMIO window, which the default 64 MB gap
 * cannot hold.  The harness also links it without the option and
 * requires exit 3, so a linker that ignores the flag fails here. */
#include <stdlib.h>

int main(void) {
    unsigned int len;
    unsigned int at;
    unsigned int total;
    unsigned char *block;

    len = 72u * 1024u * 1024u;
    block = (unsigned char *)malloc(len);
    if (block == 0) return 3;
    at = 0;
    while (at < len) {
        block[at] = (unsigned char)((at >> 16) + 7);
        at = at + 8192;
    }
    block[len - 1] = 0x5a;
    total = 0;
    at = 0;
    while (at < len) {
        total = total + block[at];
        at = at + 8192;
    }
    if (block[len - 1] != 0x5a) return 4;
    if (total == 0) return 5;
    free(block);
    return 0;
}
