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

#include <stdarg.h>
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
char *getenv(const char *name);   /* TZ */

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
#define NR_readlinkat   78
#define NR_getitimer    102
#define NR_setitimer    103
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
#define NR_readlinkat   267
#define NR_getitimer    36
#define NR_setitimer    38
#define NR_mmap         9
#define NR_mprotect     10
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

/* open, fcntl and ioctl are variadic (POSIX): the third argument is
 * read whether or not the caller passed one -- unused then, as O_CREAT
 * and the commands that take one are what make it matter */
int open(const char *path, int flags, ...) {
    va_list ap;
    int mode;
    va_start(ap, flags);
    mode = va_arg(ap, int);
    va_end(ap);
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

int fcntl(int fd, int cmd, ...) {
    va_list ap;
    long arg;
    va_start(ap, cmd);
    arg = va_arg(ap, long);
    va_end(ap);
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

int dup(int fd) {
    return (int)sys_ret(__syscall(NR_fcntl, fd, F_DUPFD, 0, 0, 0, 0));
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

long readlink(const char *path, char *buf, unsigned long size) {
    return sys_ret(__syscall(NR_readlinkat, AT_FDCWD, path, buf, size, 0, 0));
}

/* realpath: the path made absolute, "." and ".." folded, repeated
 * slashes dropped and every symlink followed, as POSIX has it -- the
 * named file and each directory on the way must exist.  Components are
 * taken from the front of `rest`; a link's target replaces the
 * component and goes in front of what remains (from the root if it is
 * absolute).  40 links is ELOOP, as Linux counts. */
char *realpath(const char *path, char *resolved) {
    char rest[4096];
    char out[4096];
    char link[4096];
    int n;            /* length of out; out is "/" when n == 0 */
    int links = 0;
    int r;
    if (path == 0) { errno = 22; return 0; }
    if (path[0] == 0) { errno = 2; return 0; }
    if (strlen(path) >= sizeof rest) { errno = 36; return 0; }
    strcpy(rest, path);
    n = 0;
    if (path[0] != '/') {
        if (getcwd(out, sizeof out) == 0) return 0;
        n = (int)strlen(out);
        if (n == 1) n = 0;                       /* cwd is "/" */
    }
    r = 0;                                       /* read position in rest */
    for (;;) {
        int s0;
        int len;
        struct stat st;
        while (rest[r] == '/') r++;
        if (rest[r] == 0) break;
        s0 = r;
        while (rest[r] && rest[r] != '/') r++;
        len = r - s0;
        if (len == 1 && rest[s0] == '.') continue;
        if (len == 2 && rest[s0] == '.' && rest[s0 + 1] == '.') {
            while (n > 0 && out[n - 1] != '/') n--;
            if (n > 0) n--;
            continue;
        }
        if (n + 1 + len >= (int)sizeof out) { errno = 36; return 0; }
        out[n] = '/';
        memcpy(out + n + 1, rest + s0, len);
        out[n + 1 + len] = 0;
        if (lstat(out, &st) < 0) return 0;
        if (S_ISLNK(st.st_mode)) {
            long k;
            int tail;
            if (++links > 40) { errno = 40; return 0; }
            k = readlink(out, link, sizeof link - 1);
            if (k < 0) return 0;
            link[k] = 0;
            /* the target, then what was left after this component (its
             * slash with it: a slash only where the path had one, so a
             * link to a file is not taken for "file/") */
            tail = (int)strlen(rest + r);
            if (k + tail >= (long)sizeof rest) { errno = 36; return 0; }
            memmove(rest + k, rest + r, tail + 1);
            memcpy(rest, link, k);
            r = 0;
            if (link[0] == '/') n = 0;           /* absolute: from the root */
            /* relative: from the link's directory, which out still is */
            continue;
        }
        n = n + 1 + len;
        if (rest[r] != 0 && !S_ISDIR(st.st_mode)) { errno = 20; return 0; }
    }
    if (n == 0) { out[0] = '/'; n = 1; }
    out[n] = 0;
    if (resolved == 0) {
        resolved = malloc(n + 1);
        if (resolved == 0) { errno = 12; return 0; }
    }
    memcpy(resolved, out, n + 1);
    return resolved;
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

DIR *fdopendir(int fd) {
    DIR *d = (DIR *)malloc(sizeof(DIR));
    if (d == 0) { errno = 12; return 0; }
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

int ioctl(int fd, unsigned long request, ...) {
    va_list ap;
    void *arg;
    va_start(ap, request);
    arg = va_arg(ap, void *);
    va_end(ap);
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

/* sigaction.  The kernel's struct is handler, flags, restorer, mask;
 * <signal.h>'s keeps sa_handler and sa_sigaction apart, and SA_SIGINFO
 * says which is meant.  On x86-64 the kernel returns from a handler
 * through sa_restorer, which must make the rt_sigreturn syscall with the
 * stack exactly as the kernel left it -- no C function can promise
 * that, so it is nine bytes of machine code in a page of its own.
 * AArch64's kernel supplies its own (the vDSO's) when none is given. */
#define K_SA_RESTART  0x10000000
#define K_SA_RESTORER 0x04000000

#if !defined(__aarch64__)
static void *sig_restorer;

static void *sig_get_restorer(void) {
    if (sig_restorer == 0) {
        /* mov $15, %rax ; syscall      (rt_sigreturn) */
        static const unsigned char code[9] = { 0x48, 0xc7, 0xc0, 0x0f, 0x00, 0x00, 0x00, 0x0f, 0x05 };
        long p = __syscall(NR_mmap, 0, 4096, 3 /* RW */, 0x22 /* PRIVATE|ANON */, -1, 0);
        if (p < 0 && p > -4096) return 0;
        memcpy((char *)p, (char *)code, sizeof code);
        if (__syscall(NR_mprotect, p, 4096, 5 /* RX */, 0, 0, 0) != 0) return 0;
        sig_restorer = (void *)p;
    }
    return sig_restorer;
}
#endif

int sigaction(int signo, struct sigaction *act, struct sigaction *oldact) {
    unsigned long k[4];                /* handler, flags, restorer, mask */
    unsigned long ko[4];
    unsigned long *kp = 0;
    if (act) {
        k[1] = (unsigned long)(unsigned int)act->sa_flags;
        k[0] = (k[1] & SA_SIGINFO) ? (unsigned long)act->sa_sigaction
                                   : (unsigned long)act->sa_handler;
        k[2] = 0;
#if !defined(__aarch64__)
        k[2] = (unsigned long)sig_get_restorer();
        if (k[2] == 0) { errno = 12; return -1; }
        k[1] |= K_SA_RESTORER;
#endif
        k[3] = (unsigned long)act->sa_mask;
        kp = k;
    }
    ko[0] = 0; ko[1] = 0; ko[2] = 0; ko[3] = 0;
    if (sys_ret(__syscall(NR_rt_sigaction, signo, kp, ko, 8, 0, 0)) < 0) return -1;
    if (oldact) {
        oldact->sa_handler = (void (*)(int))ko[0];
        oldact->sa_sigaction = (void (*)(int, siginfo_t *, void *))ko[0];
        oldact->sa_flags = (int)(ko[1] & ~(unsigned long)K_SA_RESTORER);
        oldact->sa_mask = (sigset_t)ko[3];
        oldact->sa_restorer = 0;
    }
    return 0;
}

/* signal: BSD semantics, as glibc gives it -- the call restarts. */
sighandler_t signal(int signo, sighandler_t handler) {
    struct sigaction sa;
    struct sigaction old;
    memset((char *)&sa, 0, sizeof sa);
    sa.sa_handler = handler;
    sa.sa_flags = K_SA_RESTART;
    if (sigaction(signo, &sa, &old) < 0) return SIG_ERR;
    return old.sa_handler;
}

int setitimer(int which, struct itimerval *nv, struct itimerval *ov) {
    return (int)sys_ret(__syscall(NR_setitimer, which, nv, ov, 0, 0, 0));
}

int getitimer(int which, struct itimerval *cur) {
    return (int)sys_ret(__syscall(NR_getitimer, which, cur, 0, 0, 0, 0));
}

/* ---- time ---- */

/* The local time zone: the TZif file $TZ names (":Area/City", a path,
 * or a name under /usr/share/zoneinfo) or /etc/localtime, read once.
 * Its 64-bit transitions answer for the times they cover; past the
 * last one -- and for a $TZ that is a POSIX rule rather than a file,
 * "EST5EDT,M3.2.0,M11.1.0" -- the footer's rule does.  An empty $TZ or
 * nothing readable is UTC.  This is what GETTZ reports (gmtoff, isdst,
 * the abbreviation), so the guests' localtime is the host's. */

#define TZ_MAX_TIMES 2048
#define TZ_MAX_TYPES 64
#define TZ_MAX_CHARS 256

static struct {
    int loaded;
    int timecnt;
    int typecnt;
    long trans[TZ_MAX_TIMES];
    unsigned char idx[TZ_MAX_TIMES];
    long utoff[TZ_MAX_TYPES];
    int isdst[TZ_MAX_TYPES];
    int abbr[TZ_MAX_TYPES];
    char chars[TZ_MAX_CHARS + 1];
    /* the POSIX rule: std, and dst when has_dst */
    int has_rule;
    int has_dst;
    char std_name[16];
    char dst_name[16];
    long std_off;              /* seconds east of UTC */
    long dst_off;
    int  r_kind[2];            /* 'M', 'J' or 'n': start, end */
    int  r_m[2], r_w[2], r_d[2];
    long r_time[2];            /* seconds after local midnight */
} tz;

static long tz_be32(const unsigned char *p) {
    long v = ((long)p[0] << 24) | ((long)p[1] << 16) | ((long)p[2] << 8) | p[3];
    if (v & 0x80000000L) v -= 0x100000000L;
    return v;
}

static long tz_be64(const unsigned char *p) {
    unsigned long v = 0;
    int i;
    for (i = 0; i < 8; i++) v = (v << 8) | p[i];
    return (long)v;
}

/* POSIX rule pieces */
static const char *tz_name(const char *s, char *out) {
    int n = 0;
    if (*s == '<') {
        s++;
        while (*s && *s != '>') { if (n < 15) out[n++] = *s; s++; }
        if (*s == '>') s++;
    } else {
        while ((*s >= 'A' && *s <= 'Z') || (*s >= 'a' && *s <= 'z')) { if (n < 15) out[n++] = *s; s++; }
    }
    out[n] = 0;
    return n >= 3 ? s : 0;
}

static const char *tz_num(const char *s, long *v) {
    long n = 0;
    if (*s < '0' || *s > '9') return 0;
    while (*s >= '0' && *s <= '9') n = n * 10 + (*s++ - '0');
    *v = n;
    return s;
}

/* [+-]hh[:mm[:ss]] in seconds */
static const char *tz_hms(const char *s, long *v) {
    long sign = 1;
    long h;
    long m = 0;
    long sec = 0;
    if (*s == '+') s++;
    else if (*s == '-') { sign = -1; s++; }
    s = tz_num(s, &h);
    if (!s) return 0;
    if (*s == ':') { s = tz_num(s + 1, &m); if (!s) return 0; }
    if (*s == ':') { s = tz_num(s + 1, &sec); if (!s) return 0; }
    *v = sign * (h * 3600 + m * 60 + sec);
    return s;
}

static const char *tz_date(const char *s, int k) {
    long a;
    long b;
    long c;
    tz.r_time[k] = 7200;
    if (*s == 'M') {
        s = tz_num(s + 1, &a); if (!s || *s != '.') return 0;
        s = tz_num(s + 1, &b); if (!s || *s != '.') return 0;
        s = tz_num(s + 1, &c); if (!s) return 0;
        tz.r_kind[k] = 'M'; tz.r_m[k] = (int)a; tz.r_w[k] = (int)b; tz.r_d[k] = (int)c;
    } else if (*s == 'J') {
        s = tz_num(s + 1, &a); if (!s) return 0;
        tz.r_kind[k] = 'J'; tz.r_d[k] = (int)a;
    } else {
        s = tz_num(s, &a); if (!s) return 0;
        tz.r_kind[k] = 'n'; tz.r_d[k] = (int)a;
    }
    if (*s == '/') s = tz_hms(s + 1, &tz.r_time[k]);
    return s;
}

static int tz_rule(const char *s) {
    long off;
    s = tz_name(s, tz.std_name);
    if (!s) return 0;
    s = tz_hms(s, &off);
    if (!s) return 0;
    tz.std_off = -off;                   /* POSIX counts west */
    tz.has_dst = 0;
    if (*s && *s != ',') {
        s = tz_name(s, tz.dst_name);
        if (!s) return 0;
        tz.dst_off = tz.std_off + 3600;
        if (*s && *s != ',') {
            s = tz_hms(s, &off);
            if (!s) return 0;
            tz.dst_off = -off;
        }
        tz.has_dst = 1;
        if (*s == ',') {
            s = tz_date(s + 1, 0);
            if (!s || *s != ',') return 0;
            s = tz_date(s + 1, 1);
            if (!s) return 0;
        } else {                         /* POSIX's default: the US rule */
            tz.r_kind[0] = 'M'; tz.r_m[0] = 3; tz.r_w[0] = 2; tz.r_d[0] = 0; tz.r_time[0] = 7200;
            tz.r_kind[1] = 'M'; tz.r_m[1] = 11; tz.r_w[1] = 1; tz.r_d[1] = 0; tz.r_time[1] = 7200;
        }
    }
    tz.has_rule = 1;
    return 1;
}

static int tz_leap(long y) { return (y % 4 == 0 && y % 100 != 0) || y % 400 == 0; }

/* days from 1970-01-01 to y-m-d (m 1..12) */
static long tz_days(long y, long m, long d) {
    long era;
    long yoe;
    long doy;
    long doe;
    y -= m <= 2;
    era = (y >= 0 ? y : y - 399) / 400;
    yoe = y - era * 400;
    doy = (153 * (m + (m > 2 ? -3 : 9)) + 2) / 5 + d - 1;
    doe = yoe * 365 + yoe / 4 - yoe / 100 + doy;
    return era * 146097 + doe - 719468;
}

/* the day (days since the epoch) rule k names in year y */
static long tz_rule_day(int k, long y) {
    if (tz.r_kind[k] == 'J') {           /* 1..365, February 29 never counted */
        long d = tz.r_d[k];
        if (tz_leap(y) && d >= 60) d++;
        return tz_days(y, 1, 1) + d - 1;
    }
    if (tz.r_kind[k] == 'n') return tz_days(y, 1, 1) + tz.r_d[k];
    {
        long first = tz_days(y, tz.r_m[k], 1);
        long wd = ((first % 7) + 11) % 7;        /* weekday of the 1st, 0 = Sunday */
        long day = first + ((tz.r_d[k] - wd + 7) % 7) + 7 * (tz.r_w[k] - 1);
        static const int mdays[12] = { 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 };
        long dim = mdays[tz.r_m[k] - 1] + (tz.r_m[k] == 2 && tz_leap(y));
        while (day >= first + dim) day -= 7;    /* week 5 means the last */
        return day;
    }
}

/* the rule's answer for t: 1 when it is DST */
static int tz_rule_dst(long t) {
    long y;
    long start;
    long end;
    long days;
    if (!tz.has_dst) return 0;
    /* the year as standard time sees it; transitions near New Year are
     * checked against this year's pair, which is how zic writes them */
    days = (t + tz.std_off) / 86400;
    if ((t + tz.std_off) % 86400 < 0) days--;
    {
        long era = (days + 719468 >= 0 ? days + 719468 : days + 719468 - 146096) / 146097;
        long doe = days + 719468 - era * 146097;
        long yoe = (doe - doe / 1460 + doe / 36524 - doe / 146096) / 365;
        long doy = doe - (365 * yoe + yoe / 4 - yoe / 100);
        long mp = (5 * doy + 2) / 153;
        y = yoe + era * 400 + (mp >= 10);
    }
    start = tz_rule_day(0, y) * 86400 + tz.r_time[0] - tz.std_off;
    end = tz_rule_day(1, y) * 86400 + tz.r_time[1] - tz.dst_off;
    /* glibc (tzset.c, compute_change) counts a year's rule dates from
     * 1970-01-01 for every year up to 1970, so before 1970 a bare rule
     * never says DST in the north and always does in the south.  The
     * gcc-built DBT answers GETTZ through glibc on the same host; this
     * one agrees with it, quirk and all. */
    if (y < 1970) {
        start -= tz_days(y, 1, 1) * 86400;
        end -= tz_days(y, 1, 1) * 86400;
    }
    if (start < end) return t >= start && t < end;
    return !(t >= end && t < start);             /* southern hemisphere */
}

static int tz_load_file(const char *path) {
    static unsigned char buf[65536];
    int fd = open(path, O_RDONLY, 0);
    long n = 0;
    long got;
    const unsigned char *p;
    long cnt[6];
    long v1;
    int i;
    if (fd < 0) return 0;
    while (n < (long)sizeof buf && (got = read(fd, (char *)buf + n, (int)(sizeof buf - n))) > 0) n += got;
    close(fd);
    if (n < 44 || buf[0] != 'T' || buf[1] != 'Z' || buf[2] != 'i' || buf[3] != 'f') return 0;
    for (i = 0; i < 6; i++) cnt[i] = tz_be32(buf + 20 + 4 * i);
    /* isutcnt isstdcnt leapcnt timecnt typecnt charcnt */
    v1 = cnt[3] * 5 + cnt[4] * 6 + cnt[5] + cnt[2] * 8 + cnt[1] + cnt[0];
    p = buf + 44;
    if (buf[4] >= '2') {                         /* the 64-bit block after the 32-bit one */
        p = buf + 44 + v1;
        if (p + 44 > buf + n || p[0] != 'T') return 0;
        for (i = 0; i < 6; i++) cnt[i] = tz_be32(p + 20 + 4 * i);
        p += 44;
    }
    if (cnt[3] > TZ_MAX_TIMES || cnt[4] > TZ_MAX_TYPES || cnt[4] < 1 || cnt[5] > TZ_MAX_CHARS) return 0;
    {
        int tsz = buf[4] >= '2' ? 8 : 4;
        long need = cnt[3] * (tsz + 1) + cnt[4] * 6 + cnt[5] + cnt[2] * (tsz + 4) + cnt[1] + cnt[0];
        if (p + need > buf + n) return 0;
        tz.timecnt = (int)cnt[3];
        tz.typecnt = (int)cnt[4];
        for (i = 0; i < tz.timecnt; i++)
            tz.trans[i] = tsz == 8 ? tz_be64(p + 8 * i) : tz_be32(p + 4 * i);
        p += tz.timecnt * tsz;
        for (i = 0; i < tz.timecnt; i++) {
            tz.idx[i] = p[i];
            if (p[i] >= tz.typecnt) return 0;
        }
        p += tz.timecnt;
        for (i = 0; i < tz.typecnt; i++) {
            tz.utoff[i] = tz_be32(p);
            tz.isdst[i] = p[4];
            tz.abbr[i] = p[5] < cnt[5] ? p[5] : 0;
            p += 6;
        }
        memcpy(tz.chars, (char *)p, cnt[5]);
        tz.chars[cnt[5]] = 0;
        p += cnt[5] + cnt[2] * (tsz + 4) + cnt[1] + cnt[0];
        if (tsz == 8 && p < buf + n && *p == '\n') {     /* the footer rule */
            char rule[128];
            int k = 0;
            p++;
            while (p < buf + n && *p != '\n' && k < 127) rule[k++] = (char)*p++;
            rule[k] = 0;
            if (k > 0) tz_rule(rule);
        }
    }
    return 1;
}

static void tz_load(void) {
    char *e;
    char path[512];
    if (tz.loaded) return;
    tz.loaded = 1;
    e = getenv("TZ");
    if (e == 0) { tz_load_file("/etc/localtime"); return; }
    if (*e == ':') e++;
    if (*e == 0) return;                          /* TZ="" is UTC */
    if (*e == '/') { if (tz_load_file(e)) return; }
    else if (strlen(e) < 400) {
        memcpy(path, "/usr/share/zoneinfo/", 20);
        strcpy(path + 20, e);
        if (tz_load_file(path)) return;
    }
    tz_rule(e);                                   /* a POSIX rule, or nothing */
}

/* civil time of s seconds since the epoch */
static void tz_civil(long s, struct tm *out) {
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
    out->tm_wday = (int)(((days % 7) + 11) % 7);         /* 1970-01-01 was a Thursday */
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
        out->tm_yday = cum[out->tm_mon] + out->tm_mday - 1 + (tz_leap(y) && out->tm_mon > 1);
    }
}

struct tm *localtime_r(const time_t *tp, struct tm *out) {
    long t = (long)*tp;
    long off = 0;
    int dst = 0;
    const char *zone = "UTC";
    tz_load();
    if (tz.typecnt > 0 && (tz.timecnt == 0 || t < tz.trans[tz.timecnt - 1] || !tz.has_rule)) {
        int ty = 0;
        if (tz.timecnt > 0 && t >= tz.trans[0]) {
            int lo = 0;
            int hi = tz.timecnt - 1;           /* last transition <= t */
            while (lo < hi) {
                int mid = (lo + hi + 1) / 2;
                if (tz.trans[mid] <= t) lo = mid; else hi = mid - 1;
            }
            ty = tz.idx[lo];
        } else {
            int i;                             /* before the first: the first standard type */
            for (i = 0; i < tz.typecnt; i++) if (!tz.isdst[i]) { ty = i; break; }
        }
        off = tz.utoff[ty];
        dst = tz.isdst[ty];
        zone = tz.chars + tz.abbr[ty];
    } else if (tz.has_rule) {
        dst = tz_rule_dst(t);
        off = dst ? tz.dst_off : tz.std_off;
        zone = dst ? tz.dst_name : tz.std_name;
    }
    tz_civil(t + off, out);
    out->tm_isdst = dst;
    out->tm_gmtoff = off;
    out->tm_zone = zone;
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
