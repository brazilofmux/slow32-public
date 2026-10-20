/* Small POSIX and C99 pieces common to both C libraries: isatty, the
 * <fenv.h> functions, and the calls the machine has no service for
 * (utimes, statvfs), which say so with ENOSYS. */
#include <errno.h>
#include <fcntl.h>
#include <fenv.h>
#include <limits.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/statvfs.h>
#include <sys/time.h>
#include <unistd.h>

/* The machine has no terminal query: no descriptor is a terminal. */
int isatty(int fd) {
    (void)fd;
    errno = ENOTTY;
    return 0;
}

/* One rounding mode, no exception flags: the floating-point environment
 * holds nothing, so these succeed when asked for what there is and fail
 * otherwise (C99 7.6). */
int feclearexcept(int excepts) { return excepts == 0 ? 0 : -1; }
int feraiseexcept(int excepts) { return excepts == 0 ? 0 : -1; }
int fetestexcept(int excepts) { (void)excepts; return 0; }
int fegetexceptflag(fexcept_t *flagp, int excepts) {
    (void)excepts;
    if (flagp) *flagp = 0;
    return 0;
}
int fesetexceptflag(const fexcept_t *flagp, int excepts) {
    (void)flagp;
    return excepts == 0 ? 0 : -1;
}
int fegetround(void) { return FE_TONEAREST; }
int fesetround(int round) { return round == FE_TONEAREST ? 0 : -1; }
int fegetenv(fenv_t *envp) { if (envp) *envp = 0; return 0; }
int feholdexcept(fenv_t *envp) { if (envp) *envp = 0; return 0; }
int fesetenv(const fenv_t *envp) { (void)envp; return 0; }
int feupdateenv(const fenv_t *envp) { (void)envp; return 0; }

/* No service sets a file's times or reports a file system's space */
int utimes(const char *path, const struct timeval times[2]) {
    (void)path; (void)times;
    errno = ENOSYS;
    return -1;
}
int statvfs(const char *path, struct statvfs *buf) {
    (void)path; (void)buf;
    errno = ENOSYS;
    return -1;
}
int fstatvfs(int fd, struct statvfs *buf) {
    (void)fd; (void)buf;
    errno = ENOSYS;
    return -1;
}

/* ---- file-system calls std::filesystem needs ---- */

int truncate(const char *path, long long length) {
    if (length < 0 || length > 0x7FFFFFFFLL) { errno = EINVAL; return -1; }
    int fd = open(path, O_WRONLY);
    if (fd < 0) return -1;
    int r = ftruncate(fd, (int)length);
    int e = errno;
    close(fd);
    errno = e;
    return r;
}

int link(const char *oldpath, const char *newpath) {
    (void)oldpath; (void)newpath;
    errno = ENOSYS;
    return -1;
}

int symlink(const char *target, const char *linkpath) {
    (void)target; (void)linkpath;
    errno = ENOSYS;
    return -1;
}

long readlink(const char *path, char *buf, size_t size) {
    struct stat st;
    (void)buf; (void)size;
    if (lstat(path, &st) != 0) return -1;
    errno = S_ISLNK(st.st_mode) ? ENOSYS : EINVAL;
    return -1;
}

long pathconf(const char *path, int name) {
    (void)path;
    if (name == _PC_PATH_MAX) return PATH_MAX;
    if (name == _PC_NAME_MAX) return NAME_MAX;
    errno = EINVAL;
    return -1;
}

long fpathconf(int fd, int name) {
    (void)fd;                       /* the limits are the same everywhere */
    return pathconf(NULL, name);
}

static int perms_already(const struct stat *st, mode_t mode) {
    return (st->st_mode & 07777) == (mode & 07777);
}

int chmod(const char *path, mode_t mode) {
    struct stat st;
    if (stat(path, &st) != 0) return -1;
    if (perms_already(&st, mode)) return 0;
    errno = ENOSYS;
    return -1;
}

int fchmod(int fd, mode_t mode) {
    struct stat st;
    if (fstat(fd, &st) != 0) return -1;
    if (perms_already(&st, mode)) return 0;
    errno = ENOSYS;
    return -1;
}

char *realpath(const char *path, char *resolved) {
    char buf[PATH_MAX];
    size_t n = 0;
    struct stat st;
    if (!path) { errno = EINVAL; return NULL; }
    if (path[0] == '\0') { errno = ENOENT; return NULL; }
    if (stat(path, &st) != 0) return NULL;          /* it must exist */
    if (path[0] != '/') {
        if (!getcwd(buf, sizeof buf)) return NULL;
        n = strlen(buf);
    }
    /* walk the components onto buf, which holds "/a/b" with no trailing / */
    if (n == 1 && buf[0] == '/') n = 0;
    const char *p = path;
    while (*p) {
        while (*p == '/') p++;
        if (!*p) break;
        const char *q = p;
        while (*q && *q != '/') q++;
        size_t len = (size_t)(q - p);
        if (len == 1 && p[0] == '.') {
            /* nothing */
        } else if (len == 2 && p[0] == '.' && p[1] == '.') {
            while (n > 0 && buf[n - 1] != '/') n--;
            if (n > 0) n--;                           /* drop the slash too */
        } else {
            if (n + 1 + len >= sizeof buf) { errno = ENAMETOOLONG; return NULL; }
            buf[n++] = '/';
            memcpy(buf + n, p, len);
            n += len;
        }
        p = q;
    }
    if (n == 0) buf[n++] = '/';
    buf[n] = '\0';
    if (!resolved) {
        resolved = malloc(n + 1);
        if (!resolved) { errno = ENOMEM; return NULL; }
    }
    memcpy(resolved, buf, n + 1);
    return resolved;
}
