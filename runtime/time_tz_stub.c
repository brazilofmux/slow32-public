#include <time.h>
#include <unistd.h>

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

int clock_gettime(int clock_id, struct timespec *ts) {
    if (!ts || (clock_id != CLOCK_REALTIME && clock_id != CLOCK_MONOTONIC &&
                clock_id != CLOCK_PROCESS_CPUTIME_ID)) {
        errno = EINVAL;
        return -1;
    }
    errno = ENOSYS;
    return -1;
}

int nanosleep(const struct timespec *req, struct timespec *rem) {
    (void)rem;
    if (!req || req->tv_nsec >= 1000000000u) {
        errno = EINVAL;
        return -1;
    }
    errno = ENOSYS;
    return -1;
}

unsigned int sleep(unsigned int seconds) {
    if (seconds == 0) return 0;
    errno = ENOSYS;
    return seconds;
}

int usleep(unsigned int usec) {
    (void)usec;
    errno = ENOSYS;
    return -1;
}

/* No processor time without MMIO either: C's answer for "not available" */
clock_t clock(void) {
    return (clock_t)-1;
}
