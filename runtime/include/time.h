#ifndef SLOW32_TIME_H
#define SLOW32_TIME_H

#include <stddef.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef unsigned long long time_t;

struct timespec {
    time_t tv_sec;
    unsigned int tv_nsec;
};

struct tm {
    int tm_sec;     // Seconds [0,60]
    int tm_min;     // Minutes [0,59]
    int tm_hour;    // Hour [0,23]
    int tm_mday;    // Day of month [1,31]
    int tm_mon;     // Month of year [0,11]
    int tm_year;    // Years since 1900
    int tm_wday;    // Day of week [0,6] (Sunday=0)
    int tm_yday;    // Day of year [0,365]
    int tm_isdst;   // Daylight saving flag (>0 in effect, 0 not, <0 unknown)
    long tm_gmtoff;     // Seconds east of UTC (0 for gmtime)
    const char *tm_zone; // Timezone abbreviation (e.g. "UTC","PST")
};

#define CLOCK_REALTIME 0

typedef unsigned long clock_t;
#define CLOCKS_PER_SEC ((clock_t)1000000)

// Clock
clock_t clock(void);

// Time retrieval
int clock_gettime(int clock_id, struct timespec *ts);
time_t time(time_t *t);

// Sleep functions
int nanosleep(const struct timespec *req, struct timespec *rem);

// Time conversion. gmtime is UTC; localtime applies the host timezone offset
// reported by the GETTZ MMIO op (falls back to UTC when MMIO is unavailable).
// mktime treats its argument as UTC (i.e. inverse of gmtime).
struct tm *gmtime(const time_t *timer);
struct tm *localtime(const time_t *timer);
time_t mktime(struct tm *tm);

// Internal: query the host timezone for the given UTC instant. Returns 0 and
// fills the outputs on success, -1 if no timezone service is available (the
// weak default in time_extra.c). The strong implementation lives in time_mmio.c.
int __s32_query_tz(time_t when, long *gmtoff_sec, int *is_dst, char abbrev[8]);

// Time formatting
char *asctime(const struct tm *tm);
char *ctime(const time_t *timer);
size_t strftime(char *s, size_t maxsize, const char *format, const struct tm *tm);

// Time arithmetic
double difftime(time_t time1, time_t time0);

#ifdef __cplusplus
}
#endif

#endif
