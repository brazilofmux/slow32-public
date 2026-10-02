/* time.h -- the self-hosted library's (also the cross compilers') */
#ifndef _TIME_H
#define _TIME_H

#include <stddef.h>

typedef long time_t;

/* microseconds since the program first asked (libc/posix_time.c) */
typedef unsigned long clock_t;
#define CLOCKS_PER_SEC ((clock_t)1000000)

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
int nanosleep(const struct timespec *req, struct timespec *rem);
clock_t clock(void);
time_t time(time_t *t);
double difftime(time_t later, time_t earlier);
struct tm *gmtime(const time_t *t);
struct tm *localtime(const time_t *t);   /* host zone via the MMIO GETTZ op */
struct tm *gmtime_r(const time_t *t, struct tm *result);
struct tm *localtime_r(const time_t *t, struct tm *result);
time_t mktime(struct tm *tm);            /* localtime's inverse */
char *asctime(const struct tm *tm);
char *ctime(const time_t *t);
size_t strftime(char *s, size_t max, const char *format, const struct tm *tm);

/* The host's zone rules for an instant: seconds east of UTC, whether
 * daylight time is in effect, the zone's abbreviation.  -1 when there
 * is no zone service (then local time is UTC). */
int __s32_query_tz(time_t when, long *gmtoff, int *isdst, char *abbrev);

#endif
