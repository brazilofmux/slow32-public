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

/* The reply is seconds_lo, seconds_hi, nanoseconds, reserved.  time_t
 * is long long: signed, 64 bits. */
static long long pm_seconds(unsigned int *p) {
    return (long long)(((unsigned long long)p[1] << 32) | (unsigned long long)p[0]);
}

long long time(long long *t) {
    long long secs;
    if (s32_mmio_request(MMIO_OP_GETTIME, 16, 0, 0) == -1) {
        if (t) *t = -1;
        return -1;
    }
    secs = pm_seconds((unsigned int *)MMIO_DATA);
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

static struct tm pm_tm;
static char pm_zone[8];

static int pm_is_leap(long long y) {
    return (y % 4 == 0 && y % 100 != 0) || (y % 400 == 0);
}

static int pm_days_before_month[12] = {0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334};

/* a / b rounded down, for times before 1970 */
static long long pm_floor_div(long long a, long long b) {
    long long q;
    q = a / b;
    if (a % b != 0 && (a < 0) != (b < 0)) q = q - 1;
    return q;
}

/* The broken-down UTC time of t + off seconds (off is a zone offset,
 * under a day either way, added after the split so the sum cannot
 * overflow).  The civil date comes straight from the day count, through
 * 400-year eras (Howard Hinnant's), so every time takes the same few
 * steps.  A year tm_year's int cannot hold is EOVERFLOW. */
static struct tm *pm_gmtime_r(long long t, long off, struct tm *r) {
    long long days;
    long long secs;
    long long z;
    long long era;
    long long doe;
    long long yoe;
    long long doy;
    long long mp;
    long long year;
    int mday;
    int mon;
    days = pm_floor_div(t, 86400);
    secs = t - days * 86400 + off;
    if (secs < 0) {
        secs = secs + 86400;
        days = days - 1;
    } else if (secs >= 86400) {
        secs = secs - 86400;
        days = days + 1;
    }
    z = days + 719468;                  /* days since 0000-03-01 */
    era = pm_floor_div(z, 146097);
    doe = z - era * 146097;
    yoe = (doe - doe / 1460 + doe / 36524 - doe / 146096) / 365;
    doy = doe - (365 * yoe + yoe / 4 - yoe / 100);     /* from March 1 */
    mp = (5 * doy + 2) / 153;                          /* March is 0 */
    mday = (int)(doy - (153 * mp + 2) / 5 + 1);
    if (mp < 10) mon = (int)(mp + 2);
    else mon = (int)(mp - 10);
    year = yoe + era * 400;
    if (mon < 2) year = year + 1;
    if (year - 1900 < -2147483647 - 1 || year - 1900 > 2147483647) {
        errno = 75;                     /* EOVERFLOW */
        return 0;
    }
    r->tm_year = (int)(year - 1900);
    r->tm_mon = mon;
    r->tm_mday = mday;
    r->tm_yday = pm_days_before_month[mon] + mday - 1;
    if (mon >= 2 && pm_is_leap(year)) r->tm_yday = r->tm_yday + 1;
    r->tm_hour = (int)(secs / 3600);
    r->tm_min = (int)(secs / 60 % 60);
    r->tm_sec = (int)(secs % 60);
    r->tm_wday = (int)(days + 4 - pm_floor_div(days + 4, 7) * 7);  /* 1970-01-01 was a Thursday */
    r->tm_isdst = 0;
    r->tm_gmtoff = 0;
    r->tm_zone = "UTC";
    return r;
}

/* The query is a timepair (seconds lo/hi, nanoseconds, reserved); the
 * reply is gmtoff_sec, is_dst, abbrev[8]. */
int __s32_query_tz(long long when, long *gmtoff, int *isdst, char *abbrev) {
    unsigned int *p;
    int *q;
    char *a;
    int i;
    p = (unsigned int *)MMIO_DATA;
    p[0] = (unsigned int)when;
    p[1] = (unsigned int)((unsigned long long)when >> 32);
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

struct tm *gmtime_r(const long long *t, struct tm *r) {
    return pm_gmtime_r(*t, 0, r);
}

struct tm *gmtime(const long long *t) {
    return pm_gmtime_r(*t, 0, &pm_tm);
}

/* the zone's name is kept in one place for every result: it is the
 * zone's, not the call's (tzname, where there is one, is the same) */
struct tm *localtime_r(const long long *t, struct tm *r) {
    long gmtoff;
    int isdst;
    char abbrev[8];
    int i;
    gmtoff = 0;
    isdst = 0;
    abbrev[0] = 0;
    if (__s32_query_tz(*t, &gmtoff, &isdst, abbrev) != 0) {
        return pm_gmtime_r(*t, 0, r);        /* no zone service: UTC */
    }
    if (pm_gmtime_r(*t, gmtoff, r) == 0) return 0;
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

struct tm *localtime(const long long *t) {
    return localtime_r(t, &pm_tm);
}

struct pm_timeval { long long tv_sec; long tv_usec; };
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
        tv->tv_sec = pm_seconds(p);
        tv->tv_usec = (long)(p[2] / 1000u);
    }
    return 0;
}

struct pm_timespec { long long tv_sec; long tv_nsec; };

/* one clock: the host's, by the same request as time() */
int clock_gettime(int clk, struct pm_timespec *ts) {
    unsigned int *p;
    (void)clk;
    if (s32_mmio_request(MMIO_OP_GETTIME, 16, 0, 0) == -1) return -1;
    p = (unsigned int *)MMIO_DATA;
    if (ts) {
        ts->tv_sec = pm_seconds(p);
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
static long long pm_clock_sec;
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
    long long sec;
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
