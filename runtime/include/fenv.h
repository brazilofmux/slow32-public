// runtime/include/fenv.h (SLOW-32)
// The machine rounds to nearest, ties to even, and has no other rounding
// mode and no floating-point exception flags (docs/SPEC.md), so this
// defines what C allows a target with neither to define: FE_TONEAREST,
// and FE_ALL_EXCEPT as 0.
#ifndef SLOW32_FENV_H
#define SLOW32_FENV_H

#ifdef __cplusplus
extern "C" {
#endif

typedef unsigned int fenv_t;
typedef unsigned int fexcept_t;

#define FE_TONEAREST  0
#define FE_ALL_EXCEPT 0
#define FE_DFL_ENV    ((const fenv_t *)0)

int feclearexcept(int excepts);
int fegetexceptflag(fexcept_t *flagp, int excepts);
int feraiseexcept(int excepts);
int fesetexceptflag(const fexcept_t *flagp, int excepts);
int fetestexcept(int excepts);
int fegetround(void);
int fesetround(int round);
int fegetenv(fenv_t *envp);
int feholdexcept(fenv_t *envp);
int fesetenv(const fenv_t *envp);
int feupdateenv(const fenv_t *envp);

#ifdef __cplusplus
}
#endif

#endif
