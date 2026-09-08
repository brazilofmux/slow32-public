/* time.h -- s12cc-compatible stub */
#ifndef _TIME_H
#define _TIME_H

typedef long time_t;

struct timespec {
    long tv_sec;
    long tv_nsec;
};

#define CLOCK_REALTIME  0
#define CLOCK_MONOTONIC 1
typedef int clockid_t;

struct tm {
    int tm_sec;
    int tm_min;
    int tm_hour;
    int tm_mday;
    int tm_mon;
    int tm_year;
    int tm_wday;
    int tm_yday;
    int tm_isdst;
    long tm_gmtoff;
    const char *tm_zone;
};

int clock_gettime(clockid_t clk, struct timespec *ts);
time_t time(time_t *t);
struct tm *gmtime(const time_t *t);
struct tm *localtime(const time_t *t);   /* host zone via the MMIO GETTZ op */

#endif
