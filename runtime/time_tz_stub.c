#include <time.h>

// Debug/DEBUG-I/O build: no MMIO timezone service is available, so localtime()
// falls back to UTC. The MMIO build links the real query from time_mmio.c.
int __s32_query_tz(time_t when, long *gmtoff_sec, int *is_dst, char abbrev[8]) {
    (void)when;
    (void)gmtoff_sec;
    (void)is_dst;
    (void)abbrev;
    return -1;
}

/* No clock without MMIO: gettimeofday() fails, as clock_gettime would. */
#include <errno.h>
#include <sys/time.h>
int gettimeofday(struct timeval *tv, void *tz) {
    (void)tv; (void)tz;
    errno = ENOSYS;
    return -1;
}

/* No processor time without MMIO either: C's answer for "not available" */
clock_t clock(void) {
    return (clock_t)-1;
}
