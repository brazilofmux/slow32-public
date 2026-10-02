/* posix_time.c -- time, gmtime/localtime, gettimeofday, getrusage,
 * clock, clock_gettime, nanosleep, sleep.
 * Split from posix_more.c (GitHub issue 65).  Plain C: stage07 compiles
 * this libc too. */

unsigned char *__s32_mmio_data(void);
int s32_mmio_request(unsigned int opcode, unsigned int length,
                     unsigned int offset, unsigned int status);

#define MMIO_DATA           (__s32_mmio_data())
#define MMIO_OP_GETTIME     0x30
#define MMIO_OP_GETTZ       0x35

extern int errno;

/* The reply is seconds_lo, seconds_hi, nanoseconds, reserved. */
long time(long *t) {
    unsigned int *p;
    long secs;
    if (s32_mmio_request(MMIO_OP_GETTIME, 16, 0, 0) == -1) {
        if (t) *t = -1;
        return -1;
    }
    p = (unsigned int *)MMIO_DATA;
    secs = (long)p[0];
    if (t) *t = secs;
    return secs;
}

/* --- gmtime, localtime (ported from runtime/time_extra.c) --- */

struct tm {                 /* must match include/time.h */
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

static int pm_days_in_month[12] = {31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31};
static struct tm pm_tm;
static char pm_zone[8];

static int pm_is_leap(int y) {
    return (y % 4 == 0 && y % 100 != 0) || (y % 400 == 0);
}

static struct tm *pm_gmtime_r(long t, struct tm *r) {
    long days;
    long rem;
    int year;
    int ylen;
    int m;
    int d;
    int dim;
    int wday;
    days = t / 86400;
    rem = t % 86400;
    if (rem < 0) {
        rem = rem + 86400;
        days = days - 1;
    }
    wday = (int)((days + 4) % 7);     /* 1970-01-01 was a Thursday */
    if (wday < 0) wday = wday + 7;
    year = 1970;
    if (days >= 0) {
        while (1) {
            ylen = 365;
            if (pm_is_leap(year)) ylen = 366;
            if (days < ylen) break;
            days = days - ylen;
            year = year + 1;
        }
    } else {
        while (days < 0) {
            year = year - 1;
            ylen = 365;
            if (pm_is_leap(year)) ylen = 366;
            days = days + ylen;
        }
    }
    r->tm_year = year - 1900;
    r->tm_yday = (int)days;
    d = (int)days;
    m = 0;
    while (m < 12) {
        dim = pm_days_in_month[m];
        if (m == 1 && pm_is_leap(year)) dim = dim + 1;
        if (d < dim) break;
        d = d - dim;
        m = m + 1;
    }
    r->tm_mon = m;
    r->tm_mday = d + 1;
    r->tm_hour = (int)(rem / 3600);
    rem = rem % 3600;
    r->tm_min = (int)(rem / 60);
    r->tm_sec = (int)(rem % 60);
    r->tm_isdst = 0;
    r->tm_wday = wday;
    r->tm_gmtoff = 0;
    r->tm_zone = "UTC";
    return r;
}

/* The query is a timepair (seconds lo/hi, nanoseconds, reserved); the
 * reply is gmtoff_sec, is_dst, abbrev[8]. */
int __s32_query_tz(long when, long *gmtoff, int *isdst, char *abbrev) {
    unsigned int *p;
    int *q;
    char *a;
    int i;
    p = (unsigned int *)MMIO_DATA;
    p[0] = (unsigned int)when;
    p[1] = 0;
    if (when < 0) p[1] = 0xFFFFFFFF;
    p[2] = 0;
    p[3] = 0;
    if (s32_mmio_request(MMIO_OP_GETTZ, 16, 0, 0) == -1) return -1;
    q = (int *)MMIO_DATA;
    *gmtoff = q[0];
    *isdst = q[1];
    a = (char *)MMIO_DATA + 8;
    i = 0;
    while (i < 7 && a[i]) {
        abbrev[i] = a[i];
        i = i + 1;
    }
    abbrev[i] = 0;
    return 0;
}

struct tm *gmtime_r(const long *t, struct tm *r) {
    return pm_gmtime_r(*t, r);
}

struct tm *gmtime(const long *t) {
    return pm_gmtime_r(*t, &pm_tm);
}

/* the zone's name is kept in one place for every result: it is the
 * zone's, not the call's (tzname, where there is one, is the same) */
struct tm *localtime_r(const long *t, struct tm *r) {
    long gmtoff;
    int isdst;
    char abbrev[8];
    int i;
    gmtoff = 0;
    isdst = 0;
    abbrev[0] = 0;
    if (__s32_query_tz(*t, &gmtoff, &isdst, abbrev) != 0) {
        return pm_gmtime_r(*t, r);           /* no zone service: UTC */
    }
    pm_gmtime_r(*t + gmtoff, r);
    r->tm_isdst = isdst;
    r->tm_gmtoff = gmtoff;
    if (abbrev[0]) {
        i = 0;
        while (i < 7 && abbrev[i]) {
            pm_zone[i] = abbrev[i];
            i = i + 1;
        }
        pm_zone[i] = 0;
        r->tm_zone = pm_zone;
    } else {
        r->tm_zone = "UTC";
    }
    return r;
}

struct tm *localtime(const long *t) {
    return localtime_r(t, &pm_tm);
}

struct pm_timeval { long tv_sec; long tv_usec; };
struct pm_rusage { struct pm_timeval ru_utime; struct pm_timeval ru_stime; };

/* getrusage: no per-process accounting on the host side; zero
 * times (SQLite's shell .timer shows 0.000). */
int getrusage(int who, struct pm_rusage *r) {
    (void)who;
    if (r) {
        r->ru_utime.tv_sec = 0; r->ru_utime.tv_usec = 0;
        r->ru_stime.tv_sec = 0; r->ru_stime.tv_usec = 0;
    }
    return 0;
}

/* gettimeofday over the same GETTIME request as time(); the shell's
 * .timer uses it.  tz is ignored (SQLite passes 0). */
int gettimeofday(struct pm_timeval *tv, void *tz) {
    unsigned int *p;
    (void)tz;
    if (s32_mmio_request(MMIO_OP_GETTIME, 16, 0, 0) == -1) return -1;
    p = (unsigned int *)MMIO_DATA;
    if (tv) {
        tv->tv_sec = (long)p[0];
        tv->tv_usec = (long)(p[2] / 1000u);
    }
    return 0;
}

struct pm_timespec { long tv_sec; long tv_nsec; };

/* one clock: the host's, by the same request as time() */
int clock_gettime(int clk, struct pm_timespec *ts) {
    unsigned int *p;
    (void)clk;
    if (s32_mmio_request(MMIO_OP_GETTIME, 16, 0, 0) == -1) return -1;
    p = (unsigned int *)MMIO_DATA;
    if (ts) {
        ts->tv_sec = (long)p[0];
        ts->tv_nsec = (long)p[2];
    }
    return 0;
}

/* clock: the processor time a program used is not something the host
 * reports, so this is the time that passed since the first call --
 * the standard fixes only differences between calls, and those are
 * right for a program that has the machine to itself.  Microseconds
 * (CLOCKS_PER_SEC is 1000000), wrapping after about 71 minutes as
 * POSIX's 32-bit clock_t does. */
static int pm_clock_set;
static long pm_clock_sec;
static long pm_clock_nsec;

unsigned long clock(void) {
    struct pm_timespec now;
    if (clock_gettime(0, &now) != 0) return (unsigned long)-1;
    if (!pm_clock_set) {
        pm_clock_set = 1;
        pm_clock_sec = now.tv_sec;
        pm_clock_nsec = now.tv_nsec;
    }
    return (unsigned long)(now.tv_sec - pm_clock_sec) * 1000000u + (unsigned long)((now.tv_nsec - pm_clock_nsec) / 1000);
}

int usleep(unsigned int usec);

/* nanosleep and sleep over usleep (mmio_no_start.s); nothing interrupts a
 * sleep here, so nothing is ever left over */
int nanosleep(const struct pm_timespec *req, struct pm_timespec *rem) {
    long sec;
    if (!req || req->tv_sec < 0 || req->tv_nsec < 0 || req->tv_nsec >= 1000000000) {
        errno = 22;
        return -1;
    }
    sec = req->tv_sec;
    while (sec > 0) {
        usleep(1000000);
        sec = sec - 1;
    }
    if (req->tv_nsec > 0) usleep((unsigned int)((req->tv_nsec + 999) / 1000));
    if (rem) {
        rem->tv_sec = 0;
        rem->tv_nsec = 0;
    }
    return 0;
}

unsigned int sleep(unsigned int seconds) {
    while (seconds > 0) {
        usleep(1000000);
        seconds = seconds - 1;
    }
    return 0;
}
