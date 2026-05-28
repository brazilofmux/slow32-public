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
