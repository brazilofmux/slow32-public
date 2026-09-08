/* posix_more.c -- what SQLite's port asked of this libc beyond
 * mmio_minimal.s: access, ftruncate and time over the MMIO ring (the
 * VFS), and fabs, strspn, strcspn (the library).  Plain C, since
 * stage07 compiles it too. */

/* The data buffer comes from mmio_no_start.s: stage07, which compiles
 * this libc, turns any extern object declaration into a definition,
 * which overrode the linker's absolute __mmio_base (selfhost ISSUES-67). */
unsigned char *__s32_mmio_data(void);
int s32_mmio_request(unsigned int opcode, unsigned int length,
                     unsigned int offset, unsigned int status);
int strlen(const char *s);
int write(int fd, const void *buf, int n);
void exit(int status);
void *memcpy(void *d, const void *s, unsigned int n);

#define MMIO_DATA           (__s32_mmio_data())
#define MMIO_DATA_CAPACITY  (48 * 1024)
#define MMIO_OP_FTRUNCATE   0x0D
#define MMIO_OP_ACCESS      0x25
#define MMIO_OP_GETTIME     0x30
#define MMIO_OP_GETTZ       0x35

int access(const char *path, int mode) {
    unsigned int len;
    if (path == 0) return -1;
    len = strlen(path) + 1;
    if (len > MMIO_DATA_CAPACITY) return -1;
    memcpy(MMIO_DATA, path, len);
    if (s32_mmio_request(MMIO_OP_ACCESS, len, 0, mode) != 0) return -1;
    return 0;
}

int ftruncate(int fd, int length) {
    int *p;
    if (fd < 0) return -1;
    p = (int *)MMIO_DATA;
    *p = length;
    if (s32_mmio_request(MMIO_OP_FTRUNCATE, 4, 0, fd) != 0) return -1;
    return 0;
}

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

double fabs(double x) {
    if (x < 0) return -x;
    if (x == 0) return 0.0;    /* -0.0 comes back positive */
    return x;
}

int strspn(const char *s, const char *accept) {
    int n;
    const char *a;
    n = 0;
    while (s[n]) {
        a = accept;
        while (*a && *a != s[n]) a = a + 1;
        if (*a == 0) return n;
        n = n + 1;
    }
    return n;
}

int strcspn(const char *s, const char *reject) {
    int n;
    const char *r;
    n = 0;
    while (s[n]) {
        r = reject;
        while (*r && *r != s[n]) r = r + 1;
        if (*r) return n;
        n = n + 1;
    }
    return n;
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

struct tm *gmtime(const long *t) {
    return pm_gmtime_r(*t, &pm_tm);
}

struct tm *localtime(const long *t) {
    long gmtoff;
    int isdst;
    char abbrev[8];
    int i;
    gmtoff = 0;
    isdst = 0;
    abbrev[0] = 0;
    if (__s32_query_tz(*t, &gmtoff, &isdst, abbrev) != 0) {
        return pm_gmtime_r(*t, &pm_tm);      /* no zone service: UTC */
    }
    pm_gmtime_r(*t + gmtoff, &pm_tm);
    pm_tm.tm_isdst = isdst;
    pm_tm.tm_gmtoff = gmtoff;
    if (abbrev[0]) {
        i = 0;
        while (i < 7 && abbrev[i]) {
            pm_zone[i] = abbrev[i];
            i = i + 1;
        }
        pm_zone[i] = 0;
        pm_tm.tm_zone = pm_zone;
    } else {
        pm_tm.tm_zone = "UTC";
    }
    return &pm_tm;
}

/* --- assert --- */

void abort(void) {
    exit(134);
}

static void af_puts(const char *z) {
    write(2, z, strlen(z));
}

void __assert_fail(const char *expr, const char *file, int line) {
    char num[12];
    int i;
    int n;
    af_puts("assertion failed: ");
    af_puts(expr);
    af_puts(" (");
    af_puts(file);
    af_puts(":");
    n = line;
    i = 11;
    num[i] = 0;
    if (n == 0) { i = i - 1; num[i] = 48; }
    while (n > 0) { i = i - 1; num[i] = 48 + n % 10; n = n / 10; }
    af_puts(num + i);
    af_puts(")\n");
    abort();
}
