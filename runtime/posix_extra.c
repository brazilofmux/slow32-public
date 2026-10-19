/* Small POSIX and C99 pieces common to both C libraries: isatty and the
 * <fenv.h> functions. */
#include <errno.h>
#include <fenv.h>
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
