#include <stddef.h>
#include <stdint.h>
#include <string.h>

#include "mmio_ring.h"
#include "time.h"

static int read_time_words(uint32_t *seconds, uint32_t *nanoseconds) {
    volatile unsigned char *data_buffer = S32_MMIO_DATA_BUFFER;
    unsigned int status = (unsigned int)s32_mmio_request(S32_MMIO_OP_GETTIME, 8u, 0u, 0u);
    if (status == 0xFFFFFFFFu) {
        return -1;
    }

    memcpy(seconds, (const void *)data_buffer, sizeof(uint32_t));
    memcpy(nanoseconds, (const void *)(data_buffer + sizeof(uint32_t)), sizeof(uint32_t));
    return 0;
}

int clock_gettime(int clock_id, struct timespec *ts) {
    if (!ts || clock_id != CLOCK_REALTIME) {
        return -1;
    }

    uint32_t seconds = 0;
    uint32_t nanos = 0;
    if (read_time_words(&seconds, &nanos) != 0) {
        return -1;
    }

    ts->tv_sec = (time_t)seconds;
    ts->tv_nsec = nanos;
    return 0;
}

time_t time(time_t *t) {
    uint32_t seconds = 0;
    uint32_t nanos = 0;
    (void)nanos;

    if (read_time_words(&seconds, &nanos) != 0) {
        if (t) {
            *t = (time_t)0xFFFFFFFFu;
        }
        return (time_t)0xFFFFFFFFu;
    }

    if (t) {
        *t = (time_t)seconds;
    }
    return (time_t)seconds;
}
