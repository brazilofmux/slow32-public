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
#define MMIO_OP_STAT        0x0A
#define MMIO_OP_MKDIR       0x22
#define MMIO_OP_LSTAT       0x24
#define MMIO_OP_CHDIR       0x26
#define MMIO_OP_OPENDIR     0x28
#define MMIO_OP_READDIR     0x29
#define MMIO_OP_CLOSEDIR    0x2A
#define MMIO_OP_GETENV      0x64
#define MMIO_STATUS_EOF     (-3)
#define MMIO_STAT_PATH      (-1)

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

/* --- getrusage: no per-process accounting on the host side; zero
 * times (SQLite's shell .timer shows 0.000). --- */
struct pm_timeval { long tv_sec; long tv_usec; };
struct pm_rusage { struct pm_timeval ru_utime; struct pm_timeval ru_stime; };
int getrusage(int who, struct pm_rusage *r) {
    (void)who;
    if (r) {
        r->ru_utime.tv_sec = 0; r->ru_utime.tv_usec = 0;
        r->ru_stime.tv_sec = 0; r->ru_stime.tv_usec = 0;
    }
    return 0;
}

/* --- stat, lstat, mkdir, chdir (ported from runtime/fs_mmio.c,
 * stat_mmio.c): the path goes in the data buffer, the reply is the
 * stat record struct stat mirrors. --- */

void *malloc(unsigned int n);
void free(void *p);

static int pm_path_request(int op, const char *path, int arg) {
    unsigned int len;
    if (path == 0) return -1;
    len = strlen(path) + 1;
    if (len > MMIO_DATA_CAPACITY) return -1;
    memcpy(MMIO_DATA, path, len);
    return s32_mmio_request(op, len, 0, arg);
}

int stat(const char *path, void *st) {
    if (st == 0 || pm_path_request(MMIO_OP_STAT, path, MMIO_STAT_PATH) != 0) return -1;
    memcpy(st, MMIO_DATA, 112);
    return 0;
}

int lstat(const char *path, void *st) {
    if (st == 0 || pm_path_request(MMIO_OP_LSTAT, path, MMIO_STAT_PATH) != 0) return -1;
    memcpy(st, MMIO_DATA, 112);
    return 0;
}

int mkdir(const char *path, unsigned int mode) {
    return pm_path_request(MMIO_OP_MKDIR, path, mode) == 0 ? 0 : -1;
}

int chdir(const char *path) {
    return pm_path_request(MMIO_OP_CHDIR, path, 0) == 0 ? 0 : -1;
}

int fsync(int fd) { (void)fd; return 0; }               /* the host writes through */
int utimes(const char *path, const void *times) { (void)path; (void)times; return 0; }

/* --- directory streams (runtime/dirent_mmio.c) --- */
struct pm_dir { int dd_fd; int dd_loc; };
struct pm_dirent { long d_ino; long d_off; unsigned short d_reclen; unsigned char d_type; char d_name[256]; };
static struct pm_dirent pm_cur_dirent;

struct pm_dir *opendir(const char *name) {
    int r;
    struct pm_dir *d;
    r = pm_path_request(MMIO_OP_OPENDIR, name, 0);
    if (r == -1) return 0;
    d = (struct pm_dir *)malloc(sizeof(struct pm_dir));
    if (d == 0) { s32_mmio_request(MMIO_OP_CLOSEDIR, 0, 0, r); return 0; }
    d->dd_fd = r;
    d->dd_loc = 0;
    return d;
}

struct pm_dirent *readdir(struct pm_dir *d) {
    int r;
    unsigned int *w;
    if (d == 0 || d->dd_fd < 0) return 0;
    r = s32_mmio_request(MMIO_OP_READDIR, 272, 0, d->dd_fd);
    if (r != 0) return 0;              /* EOF or error */
    w = (unsigned int *)MMIO_DATA;     /* d_ino lo/hi, d_type, d_namlen, d_name[256] */
    pm_cur_dirent.d_ino = (long)w[0];
    pm_cur_dirent.d_off = d->dd_loc;
    pm_cur_dirent.d_reclen = sizeof(struct pm_dirent);
    pm_cur_dirent.d_type = (unsigned char)w[2];
    memcpy(pm_cur_dirent.d_name, (char *)MMIO_DATA + 16, 256);
    pm_cur_dirent.d_name[255] = 0;
    d->dd_loc = d->dd_loc + 1;
    return &pm_cur_dirent;
}

int closedir(struct pm_dir *d) {
    int r;
    if (d == 0) return -1;
    r = s32_mmio_request(MMIO_OP_CLOSEDIR, 0, 0, d->dd_fd);
    free(d);
    return r == 0 ? 0 : -1;
}

/* gettimeofday over the same GETTIME request as time(); the shell's
 * .timer uses it.  tz is ignored (SQLite passes 0). */
struct pm_timeval { long tv_sec; long tv_usec; };
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

/* signal: no signals are ever delivered on SLOW-32; accept the handler
 * (the shell installs one for SIGINT) and report the previous as default. */
typedef void (*pm_sighandler)(int);
pm_sighandler signal(int sig, pm_sighandler fn) {
    (void)sig; (void)fn;
    return (pm_sighandler)0;
}
