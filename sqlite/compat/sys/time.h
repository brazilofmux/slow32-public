/* the shell's .timer: struct timeval and gettimeofday from time(2) */
#ifndef SLOW32_SYS_TIME_H
#define SLOW32_SYS_TIME_H
#include <time.h>
struct timeval { long tv_sec; long tv_usec; };
struct timezone { int tz_minuteswest, tz_dsttime; };
static inline int gettimeofday(struct timeval *tv, void *tz) { (void)tz; if (tv) { tv->tv_sec = (long)time(0); tv->tv_usec = 0; } return 0; }
static inline int utimes(const char *p, const struct timeval t[2]) { (void)p; (void)t; return 0; }
#endif
