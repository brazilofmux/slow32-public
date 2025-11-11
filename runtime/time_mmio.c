#include <stddef.h>
#include <stdint.h>
#include <string.h>

#include "mmio_ring.h"
#include "time.h"
#include "unistd.h"

#define S32_NSEC_PER_SEC   1000000000u
#define S32_NSEC_PER_USEC        1000u
#define S32_USEC_PER_SEC      1000000u

static int read_time_words(uint64_t *seconds, uint32_t *nanoseconds) {
    volatile unsigned char *data_buffer = S32_MMIO_DATA_BUFFER;
    unsigned int status = (unsigned int)s32_mmio_request(S32_MMIO_OP_GETTIME,
                                                         sizeof(s32_mmio_timepair64_t),
                                                         0u,
                                                         0u);
    if (status == S32_MMIO_STATUS_ERR) {
        return -1;
    }

    s32_mmio_timepair64_t now = {0u, 0u, 0u, 0u};
    memcpy(&now, (const void *)data_buffer, sizeof(now));
    *seconds = ((uint64_t)now.seconds_hi << 32) | now.seconds_lo;
    *nanoseconds = now.nanoseconds;
    return 0;
}

static int mmio_sleep_request(uint64_t seconds,
                              uint32_t nanoseconds,
                              struct timespec *rem_out) {
    volatile unsigned char *data_buffer = S32_MMIO_DATA_BUFFER;
    s32_mmio_timepair64_t payload = {
        .seconds_lo = (uint32_t)(seconds & 0xFFFFFFFFu),
        .seconds_hi = (uint32_t)(seconds >> 32),
        .nanoseconds = nanoseconds,
        .reserved = 0u,
    };
    memcpy((void *)data_buffer, &payload, sizeof(payload));

    unsigned int status = (unsigned int)s32_mmio_request(S32_MMIO_OP_SLEEP,
                                                         sizeof(payload),
                                                         0u,
                                                         0u);
    if (status == S32_MMIO_STATUS_ERR) {
        return -1;
    }

    s32_mmio_timepair64_t remainder = {0u, 0u, 0u, 0u};
    memcpy(&remainder, (const void *)data_buffer, sizeof(remainder));

    if (rem_out) {
        uint64_t rem_seconds = ((uint64_t)remainder.seconds_hi << 32) | remainder.seconds_lo;
        rem_out->tv_sec = (time_t)rem_seconds;
        rem_out->tv_nsec = remainder.nanoseconds;
    }

    if (status == S32_MMIO_STATUS_EINTR) {
        return 1;
    }

    return 0;
}

int clock_gettime(int clock_id, struct timespec *ts) {
    if (!ts || clock_id != CLOCK_REALTIME) {
        return -1;
    }

    uint64_t seconds = 0;
    uint32_t nanos = 0;
    if (read_time_words(&seconds, &nanos) != 0) {
        return -1;
    }

    ts->tv_sec = (time_t)seconds;
    ts->tv_nsec = nanos;
    return 0;
}

time_t time(time_t *t) {
    uint64_t seconds = 0;
    uint32_t nanos = 0;
    (void)nanos;

    if (read_time_words(&seconds, &nanos) != 0) {
        if (t) {
            *t = (time_t)(~(time_t)0);
        }
        return (time_t)(~(time_t)0);
    }

    if (t) {
        *t = (time_t)seconds;
    }
    return (time_t)seconds;
}

int nanosleep(const struct timespec *req, struct timespec *rem) {
    if (!req) {
        return -1;
    }
    if (req->tv_nsec >= S32_NSEC_PER_SEC) {
        return -1;
    }

    uint64_t seconds = (uint64_t)req->tv_sec;
    uint32_t nanoseconds = (uint32_t)req->tv_nsec;

    struct timespec remainder = {0, 0};
    int sleep_result = mmio_sleep_request(seconds,
                                          nanoseconds,
                                          rem ? &remainder : NULL);
    if (sleep_result < 0) {
        if (rem) {
            rem->tv_sec = req->tv_sec;
            rem->tv_nsec = req->tv_nsec;
        }
        return -1;
    }

    if (rem) {
        rem->tv_sec = remainder.tv_sec;
        rem->tv_nsec = remainder.tv_nsec;
    }

    if (sleep_result > 0) {
        return -1;
    }
    return 0;
}

int usleep(unsigned int usec) {
    struct timespec req = {
        .tv_sec = (time_t)(usec / S32_USEC_PER_SEC),
        .tv_nsec = (unsigned int)((usec % S32_USEC_PER_SEC) * S32_NSEC_PER_USEC),
    };
    return nanosleep(&req, NULL);
}

unsigned int sleep(unsigned int seconds) {
    struct timespec req = {
        .tv_sec = (time_t)seconds,
        .tv_nsec = 0u,
    };
    struct timespec rem = {0, 0};
    if (nanosleep(&req, &rem) != 0) {
        return (unsigned int)rem.tv_sec;
    }
    return 0u;
}
