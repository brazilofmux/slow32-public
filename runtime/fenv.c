/* fenv.c -- C11 7.6, for both C libraries.  The machine rounds to nearest,
 * ties to even, and has no other mode and no exception flags (docs/SPEC.md):
 * <fenv.h> defines FE_TONEAREST and FE_ALL_EXCEPT 0, as 7.6p6-p8 allow. */
#include <fenv.h>

/* The floating-point environment holds nothing, so these succeed when
 * asked for what there is and fail otherwise. */
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

