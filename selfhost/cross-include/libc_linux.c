/* libc_linux.c -- the POSIX calls the cross-built host tools need, over
 * Linux syscalls, for both libc_x64 and libc_a64.
 *
 * tools/emulator/mmio_ring.c -- the MMIO services: files, directories,
 * the terminal, sockets, the tube, deferred procedure calls -- is
 * compiled by cc-x64 and cc-a64 into dbt-x64 and dbt-a64 against these.
 * Before, each cross libc carried a stub of it (mmio_ring_x64.c,
 * mmio_ring_a64.c) that answered every service past plain file I/O with
 * an error, and 15 of the regression tests diverged under the
 * self-hosted DBTs (tools/dbt/ISSUES.md, DBT-27).
 *
 * Every call returns -1 and sets errno on failure, as POSIX has it; the
 * raw syscall returns -errno.  One code path for both hosts: the *at
 * calls, ppoll, pselect6 and clone exist on both (AArch64 has no open,
 * stat, poll, select or fork), and the syscall numbers are the only
 * difference.  Structures are the kernel's (../cross-include). */

#include <stdint.h>
#include <string.h>
#include <time.h>
#include <signal.h>
#include <fcntl.h>
#include <unistd.h>
#include <dirent.h>
#include <poll.h>
#include <termios.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <sys/select.h>
#include <sys/ioctl.h>
#include <sys/socket.h>
#include <sys/wait.h>
#include <netinet/in.h>

long __syscall();
char *malloc(int size);
void free(char *p);

#if defined(__aarch64__)
#define NR_getcwd       17
#define NR_dup3         24
#define NR_fcntl        25
#define NR_ioctl        29
#define NR_mkdirat      34
#define NR_unlinkat     35
#define NR_renameat     38
#define NR_ftruncate    46
#define NR_faccessat    48
#define NR_chdir        49
#define NR_fchmodat     53
#define NR_openat       56
#define NR_close        57
#define NR_pipe2        59
#define NR_getdents64   61
#define NR_lseek        62
#define NR_read         63
#define NR_write        64
#define NR_pselect6     72
#define NR_ppoll        73
#define NR_newfstatat   79
#define NR_fstat        80
#define NR_fsync        82
#define NR_nanosleep    101
#define NR_clock_gettime 113
#define NR_rt_sigaction 134
#define NR_getpid       172
#define NR_getuid       174
#define NR_socket       198
#define NR_bind         200
#define NR_listen       201
#define NR_accept       202
#define NR_connect      203
#define NR_getsockname  204
#define NR_getpeername  205
#define NR_sendto       206
#define NR_recvfrom     207
#define NR_setsockopt   208
#define NR_getsockopt   209
#define NR_shutdown     210
#define NR_clone        220
#define NR_execve       221
#define NR_wait4        260
#else
#define NR_read         0
#define NR_write        1
#define NR_close        3
#define NR_fstat        5
#define NR_lseek        8
#define NR_rt_sigaction 13
#define NR_ioctl        16
#define NR_nanosleep    35
#define NR_getpid       39
#define NR_socket       41
#define NR_connect      42
#define NR_accept       43
#define NR_sendto       44
#define NR_recvfrom     45
#define NR_shutdown     48
#define NR_bind         49
#define NR_listen       50
#define NR_getsockname  51
#define NR_getpeername  52
#define NR_setsockopt   54
#define NR_getsockopt   55
#define NR_clone        56
#define NR_execve       59
#define NR_wait4        61
#define NR_fcntl        72
#define NR_fsync        74
#define NR_ftruncate    77
#define NR_getcwd       79
#define NR_chdir        80
#define NR_getuid       102
#define NR_getdents64   217
#define NR_clock_gettime 228
#define NR_openat       257
#define NR_mkdirat      258
#define NR_newfstatat   262
#define NR_unlinkat     263
#define NR_renameat     264
#define NR_fchmodat     268
#define NR_faccessat    269
#define NR_pselect6     270
#define NR_ppoll        271
#define NR_dup3         292
#define NR_pipe2        293
#endif

int errno;

/* A raw result: -4095..-1 is -errno. */
static long sys_ret(long r) {
    if (r < 0 && r > -4096) {
        errno = (int)-r;
        return -1;
    }
    return r;
}

/* ---- files ---- */

int open(const char *path, int flags, int mode) {
    return (int)sys_ret(__syscall(NR_openat, AT_FDCWD, path, flags, mode, 0, 0));
}

int read(int fd, char *buf, int len) {
    return (int)sys_ret(__syscall(NR_read, fd, buf, len, 0, 0, 0));
}

int write(int fd, char *buf, int len) {
    return (int)sys_ret(__syscall(NR_write, fd, buf, len, 0, 0, 0));
}

int close(int fd) {
    return (int)sys_ret(__syscall(NR_close, fd, 0, 0, 0, 0, 0));
}

long lseek(int fd, long offset, int whence) {
    return sys_ret(__syscall(NR_lseek, fd, offset, whence, 0, 0, 0));
}

int stat(const char *path, struct stat *st) {
    return (int)sys_ret(__syscall(NR_newfstatat, AT_FDCWD, path, st, 0, 0, 0));
}

int lstat(const char *path, struct stat *st) {
    return (int)sys_ret(__syscall(NR_newfstatat, AT_FDCWD, path, st, AT_SYMLINK_NOFOLLOW, 0, 0));
}

int fstat(int fd, struct stat *st) {
    return (int)sys_ret(__syscall(NR_fstat, fd, st, 0, 0, 0, 0));
}

int fcntl(int fd, int cmd, long arg) {
    return (int)sys_ret(__syscall(NR_fcntl, fd, cmd, arg, 0, 0, 0));
}

int ftruncate(int fd, long length) {
    return (int)sys_ret(__syscall(NR_ftruncate, fd, length, 0, 0, 0, 0));
}

int fsync(int fd) {
    return (int)sys_ret(__syscall(NR_fsync, fd, 0, 0, 0, 0, 0));
}

int access(const char *path, int mode) {
    return (int)sys_ret(__syscall(NR_faccessat, AT_FDCWD, path, mode, 0, 0, 0));
}

int unlink(const char *path) {
    return (int)sys_ret(__syscall(NR_unlinkat, AT_FDCWD, path, 0, 0, 0, 0));
}

int rmdir(const char *path) {
    return (int)sys_ret(__syscall(NR_unlinkat, AT_FDCWD, path, AT_REMOVEDIR, 0, 0, 0));
}

int mkdir(const char *path, unsigned int mode) {
    return (int)sys_ret(__syscall(NR_mkdirat, AT_FDCWD, path, mode, 0, 0, 0));
}

int chmod(const char *path, unsigned int mode) {
    return (int)sys_ret(__syscall(NR_fchmodat, AT_FDCWD, path, mode, 0, 0, 0));
}

int rename(const char *from, const char *to) {
    return (int)sys_ret(__syscall(NR_renameat, AT_FDCWD, from, AT_FDCWD, to, 0, 0));
}

int chdir(const char *path) {
    return (int)sys_ret(__syscall(NR_chdir, path, 0, 0, 0, 0, 0));
}

char *getcwd(char *buf, unsigned long size) {
    if (sys_ret(__syscall(NR_getcwd, buf, size, 0, 0, 0, 0)) < 0) return 0;
    return buf;
}

int dup2(int oldfd, int newfd) {
    if (oldfd == newfd) {                        /* dup3 refuses the same fd */
        if (sys_ret(__syscall(NR_fcntl, oldfd, F_GETFD, 0, 0, 0, 0)) < 0) return -1;
        return newfd;
    }
    return (int)sys_ret(__syscall(NR_dup3, oldfd, newfd, 0, 0, 0, 0));
}

int pipe(int fds[2]) {
    return (int)sys_ret(__syscall(NR_pipe2, fds, 0, 0, 0, 0, 0));
}

/* realpath: getcwd and the path, with "." and ".." folded and repeated
 * slashes dropped.  Symlinks are not resolved (no readlink here); the
 * caller compares the result against a root it built the same way. */
char *realpath(const char *path, char *resolved) {
    char tmp[4096];
    char *out;
    int n;
    int i;
    struct stat st;
    if (path == 0 || path[0] == 0) { errno = 2; return 0; }
    if (stat(path, &st) < 0) return 0;          /* POSIX: the path must exist */
    out = resolved ? resolved : malloc(4096);
    if (out == 0) { errno = 12; return 0; }
    if (path[0] == '/') tmp[0] = 0;
    else if (getcwd(tmp, sizeof tmp) == 0) return 0;
    n = (int)strlen(tmp);
    if (n + 1 + (int)strlen(path) + 1 > (int)sizeof tmp) { errno = 36; return 0; }
    tmp[n++] = '/';
    strcpy(tmp + n, path);
    /* fold into out, component by component */
    n = 0;
    i = 0;
    while (tmp[i]) {
        int s;
        int len;
        while (tmp[i] == '/') i++;
        if (!tmp[i]) break;
        s = i;
        while (tmp[i] && tmp[i] != '/') i++;
        len = i - s;
        if (len == 1 && tmp[s] == '.') continue;
        if (len == 2 && tmp[s] == '.' && tmp[s + 1] == '.') {
            while (n > 0 && out[n - 1] != '/') n--;
            if (n > 0) n--;
            continue;
        }
        out[n++] = '/';
        memcpy(out + n, tmp + s, len);
        n += len;
    }
    if (n == 0) out[n++] = '/';
    out[n] = 0;
    return out;
}

/* ---- directories ---- */

DIR *opendir(const char *name) {
    DIR *d;
    int fd = open(name, O_RDONLY | O_DIRECTORY | O_CLOEXEC, 0);
    if (fd < 0) return 0;
    d = (DIR *)malloc(sizeof(DIR));
    if (d == 0) { close(fd); errno = 12; return 0; }
    d->fd = fd;
    d->pos = 0;
    d->len = 0;
    return d;
}

struct dirent *readdir(DIR *d) {
    struct dirent *e;
    if (d->pos >= d->len) {
        long n = sys_ret(__syscall(NR_getdents64, d->fd, d->buf, sizeof d->buf, 0, 0, 0));
        if (n <= 0) return 0;
        d->len = (int)n;
        d->pos = 0;
    }
    e = (struct dirent *)(d->buf + d->pos);
    d->pos += e->d_reclen;
    return e;
}

int closedir(DIR *d) {
    int r = close(d->fd);
    free((char *)d);
    return r;
}

void rewinddir(DIR *d) {
    lseek(d->fd, 0, SEEK_SET);
    d->pos = 0;
    d->len = 0;
}

int dirfd(DIR *d) {
    return d->fd;
}

/* ---- readiness ---- */

int poll(struct pollfd *fds, nfds_t nfds, int timeout) {
    struct timespec ts;
    struct timespec *tp = 0;
    if (timeout >= 0) {
        ts.tv_sec = timeout / 1000;
        ts.tv_nsec = (long)(timeout % 1000) * 1000000;
        tp = &ts;
    }
    return (int)sys_ret(__syscall(NR_ppoll, fds, nfds, tp, 0, 8, 0));
}

int select(int nfds, fd_set *r, fd_set *w, fd_set *e, struct timeval *timeout) {
    struct timespec ts;
    struct timespec *tp = 0;
    if (timeout) {
        ts.tv_sec = timeout->tv_sec;
        ts.tv_nsec = timeout->tv_usec * 1000;
        tp = &ts;
    }
    return (int)sys_ret(__syscall(NR_pselect6, nfds, r, w, e, tp, 0));
}

int nanosleep(const struct timespec *req, struct timespec *rem) {
    return (int)sys_ret(__syscall(NR_nanosleep, req, rem, 0, 0, 0, 0));
}

int clock_gettime(int clk, struct timespec *ts) {
    return (int)sys_ret(__syscall(NR_clock_gettime, clk, ts, 0, 0, 0, 0));
}

/* ---- the terminal ---- */

int ioctl(int fd, unsigned long request, void *arg) {
    return (int)sys_ret(__syscall(NR_ioctl, fd, request, arg, 0, 0, 0));
}

int isatty(int fd) {
    struct termios t;
    return ioctl(fd, TCGETS, &t) == 0;
}

int tcgetattr(int fd, struct termios *t) {
    return ioctl(fd, TCGETS, t);
}

int tcsetattr(int fd, int when, const struct termios *t) {
    unsigned long req = TCSETS;
    if (when == TCSADRAIN) req = TCSETSW;
    else if (when == TCSAFLUSH) req = TCSETSF;
    else if (when != TCSANOW) { errno = 22; return -1; }
    return ioctl(fd, req, (void *)t);
}

/* ---- sockets ---- */

int socket(int domain, int type, int protocol) {
    return (int)sys_ret(__syscall(NR_socket, domain, type, protocol, 0, 0, 0));
}

int bind(int fd, const struct sockaddr *a, socklen_t len) {
    return (int)sys_ret(__syscall(NR_bind, fd, a, len, 0, 0, 0));
}

int listen(int fd, int backlog) {
    return (int)sys_ret(__syscall(NR_listen, fd, backlog, 0, 0, 0, 0));
}

int accept(int fd, struct sockaddr *a, socklen_t *len) {
    return (int)sys_ret(__syscall(NR_accept, fd, a, len, 0, 0, 0));
}

int connect(int fd, const struct sockaddr *a, socklen_t len) {
    return (int)sys_ret(__syscall(NR_connect, fd, a, len, 0, 0, 0));
}

int getsockname(int fd, struct sockaddr *a, socklen_t *len) {
    return (int)sys_ret(__syscall(NR_getsockname, fd, a, len, 0, 0, 0));
}

int getpeername(int fd, struct sockaddr *a, socklen_t *len) {
    return (int)sys_ret(__syscall(NR_getpeername, fd, a, len, 0, 0, 0));
}

int setsockopt(int fd, int level, int name, const void *val, socklen_t len) {
    return (int)sys_ret(__syscall(NR_setsockopt, fd, level, name, val, len, 0));
}

int getsockopt(int fd, int level, int name, void *val, socklen_t *len) {
    return (int)sys_ret(__syscall(NR_getsockopt, fd, level, name, val, len, 0));
}

long send(int fd, const void *buf, unsigned long n, int flags) {
    return sys_ret(__syscall(NR_sendto, fd, buf, n, flags, 0, 0));
}

long recv(int fd, void *buf, unsigned long n, int flags) {
    return sys_ret(__syscall(NR_recvfrom, fd, buf, n, flags, 0, 0));
}

int shutdown(int fd, int how) {
    return (int)sys_ret(__syscall(NR_shutdown, fd, how, 0, 0, 0, 0));
}

/* ---- processes and signals ---- */

pid_t fork(void) {
    /* clone(SIGCHLD, 0, ...): what fork is on both kernels; the argument
     * orders differ only past the ones given as zero */
    return (pid_t)sys_ret(__syscall(NR_clone, 17, 0, 0, 0, 0, 0));
}

extern char **environ;

int execv(const char *path, char *const argv[]) {
    return (int)sys_ret(__syscall(NR_execve, path, argv, environ, 0, 0, 0));
}

pid_t waitpid(pid_t pid, int *status, int options) {
    return (pid_t)sys_ret(__syscall(NR_wait4, pid, status, options, 0, 0, 0));
}

int getpid(void) {
    return (int)__syscall(NR_getpid, 0, 0, 0, 0, 0, 0);
}

int getuid(void) {
    return (int)__syscall(NR_getuid, 0, 0, 0, 0, 0, 0);
}

/* signal: SIG_IGN and SIG_DFL only.  A handler needs a restorer
 * trampoline the kernel can return through, which nothing here has;
 * mmio_ring.c ignores SIGPIPE and that is all it asks. */
sighandler_t signal(int signo, sighandler_t handler) {
    unsigned long act[4];              /* kernel sigaction: handler, flags, restorer, mask */
    unsigned long old[4];
    if (handler != SIG_IGN && handler != SIG_DFL) { errno = 22; return SIG_ERR; }
    act[0] = (unsigned long)handler;
    act[1] = 0;
    act[2] = 0;
    act[3] = 0;
    if (sys_ret(__syscall(NR_rt_sigaction, signo, act, old, 8, 0, 0)) < 0) return SIG_ERR;
    return (sighandler_t)old[0];
}

/* ---- time ---- */

/* localtime_r: UTC, labelled so.  No zone database is read (yet); the
 * GETTZ service then reports UTC, which is what the hosts these run on
 * (kagura, the containers) are set to. */
struct tm *localtime_r(const time_t *t, struct tm *out) {
    long s = (long)*t;
    long days = s / 86400;
    long rem = s % 86400;
    long y;
    long era;
    long doe;
    long yoe;
    long doy;
    long mp;
    if (rem < 0) { rem += 86400; days -= 1; }
    out->tm_hour = (int)(rem / 3600);
    out->tm_min = (int)(rem % 3600 / 60);
    out->tm_sec = (int)(rem % 60);
    out->tm_wday = (int)((days % 7 + 11) % 7);          /* 1970-01-01 was a Thursday */
    /* civil from days (Howard Hinnant's algorithm) */
    days += 719468;
    era = (days >= 0 ? days : days - 146096) / 146097;
    doe = days - era * 146097;
    yoe = (doe - doe / 1460 + doe / 36524 - doe / 146096) / 365;
    y = yoe + era * 400;
    doy = doe - (365 * yoe + yoe / 4 - yoe / 100);
    mp = (5 * doy + 2) / 153;
    out->tm_mday = (int)(doy - (153 * mp + 2) / 5 + 1);
    out->tm_mon = (int)(mp < 10 ? mp + 2 : mp - 10);
    if (out->tm_mon <= 1) y += 1;
    out->tm_year = (int)(y - 1900);
    {
        static const int cum[12] = { 0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334 };
        int leap = (y % 4 == 0 && y % 100 != 0) || y % 400 == 0;
        out->tm_yday = cum[out->tm_mon] + out->tm_mday - 1 + (leap && out->tm_mon > 1);
    }
    out->tm_isdst = 0;
    out->tm_gmtoff = 0;
    out->tm_zone = "UTC";
    return out;
}

/* ---- strings ---- */

char *strchr(const char *s, int c) {
    while (*s) {
        if (*s == (char)c) return (char *)s;
        s++;
    }
    return c == 0 ? (char *)s : 0;
}

unsigned long strnlen(const char *s, unsigned long max) {
    unsigned long n = 0;
    while (n < max && s[n]) n++;
    return n;
}
