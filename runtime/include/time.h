#ifndef SLOW32_TIME_H
#define SLOW32_TIME_H

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
    int tm_isdst;   // Daylight saving flag (always 0 for SLOW-32)
};

#define CLOCK_REALTIME 0

// Time retrieval
int clock_gettime(int clock_id, struct timespec *ts);
time_t time(time_t *t);

// Sleep functions
int nanosleep(const struct timespec *req, struct timespec *rem);

// Time conversion (UTC-based, no timezone support)
struct tm *gmtime(const time_t *timer);
struct tm *localtime(const time_t *timer);
time_t mktime(struct tm *tm);

// Time formatting
char *asctime(const struct tm *tm);
char *ctime(const time_t *timer);

// Time arithmetic
// Note: Returns long long instead of double (SLOW-32 has no FPU)
long long difftime(time_t time1, time_t time0);

#ifdef __cplusplus
}
#endif

#endif
