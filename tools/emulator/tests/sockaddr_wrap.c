/* Host check of the GETSOCKNAME wrap rule (GitHub issue 110). */
#include "../../../common/mmio_ring_layout.h"
#include <stdio.h>

int main(void)
{
    uint32_t sz = (uint32_t)sizeof(s32_mmio_sockaddr_in_t);
    uint32_t cap = S32_MMIO_DATA_CAPACITY;
    int fail = 0;

    if (!s32_mmio_sockaddr_in_fits(0)) {
        fprintf(stderr, "offset 0 should fit\n");
        fail = 1;
    }
    if (!s32_mmio_sockaddr_in_fits(cap - sz)) {
        fprintf(stderr, "offset cap-sz should fit\n");
        fail = 1;
    }
    if (s32_mmio_sockaddr_in_fits(cap - sz + 1)) {
        fprintf(stderr, "offset cap-sz+1 should not fit\n");
        fail = 1;
    }
    if (s32_mmio_sockaddr_in_fits(cap)) {
        fprintf(stderr, "offset cap should not fit\n");
        fail = 1;
    }
    if (fail) {
        return 1;
    }
    printf("ok\n");
    return 0;
}
