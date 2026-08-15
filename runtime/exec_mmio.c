#include "errno.h"
#include "mmio_ring.h"
#include "unistd.h"

#include <string.h>

int s32_execv(const char *path, char *const argv[]) {
    volatile unsigned char *data = S32_MMIO_DATA_BUFFER;
    unsigned int used = 0;
    size_t n;
    int i;
    int r;

    if (!path || !path[0]) {
        errno = EINVAL;
        return -1;
    }
    n = strlen(path) + 1;
    if (n >= S32_MMIO_DATA_CAPACITY) {
        errno = EINVAL;
        return -1;
    }
    memcpy((void *)data, path, n);
    used = (unsigned int)n;

    if (argv) {
        for (i = 1; argv[i]; i++) {
            n = strlen(argv[i]) + 1;
            if (used + n >= S32_MMIO_DATA_CAPACITY) {
                errno = E2BIG;
                return -1;
            }
            memcpy((void *)(data + used), argv[i], n);
            used += (unsigned int)n;
        }
    }

    r = s32_mmio_request(S32_MMIO_OP_EXEC, used, 0u, 0u);
    if (r < 0) {
        return -1;
    }
    return r;
}
