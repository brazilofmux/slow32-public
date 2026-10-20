/* Small POSIX and C99 pieces common to both C libraries: isatty, the
 * <fenv.h> functions, and the calls the machine has no service for
 * (utimes, statvfs), which say so with ENOSYS. */
#include <errno.h>
#include <fenv.h>
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
