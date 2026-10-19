// runtime/include/sys/time.h (SLOW-32)
#ifndef SLOW32_SYS_TIME_H
#define SLOW32_SYS_TIME_H

#include <time.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef long suseconds_t;

struct timeval {
    time_t tv_sec;
    suseconds_t tv_usec;
};

// CLOCK_REALTIME in microseconds (libc_mmio; the debug libc has no clock
// and fails with ENOSYS).  The time zone argument is obsolete and ignored.
int gettimeofday(struct timeval *tv, void *tz);

#ifdef __cplusplus
}
#endif

#endif
