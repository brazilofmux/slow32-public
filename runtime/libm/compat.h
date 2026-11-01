/* Included ahead of every musl source (each is compiled through a stub of
 * two #includes, so musl's files stay as released):
 *  - hidden and weak_alias are musl's private features.h, GCC attributes
 *    stage08 does not have.  Internal symbols need no hiding in a static
 *    library; weak_alias only made non-C11 extras (drem, pow10, lgamma_r,
 *    signgam), which SLOW-32 goes without.
 *  - rename.h (gen.py) gives musl's C11 functions internal names; the
 *    public ones are errno.c's. */
#define hidden
#define weak_alias(old, new) extern int __s32_no_weak_alias
#include "rename.h"

/* Functions beyond C11 that musl builds and its sources call one from
 * another (POSIX's Bessel functions, GNU's exp10 and sincos).  They are
 * in the library, undeclared by the public <math.h>. */
double exp10(double);
float exp10f(float);
void sincos(double, double *, double *);
void sincosf(float, float *, float *);
double j0(double);
double j1(double);
double jn(int, double);
double y0(double);
double y1(double);
double yn(int, double);
float j0f(float);
float j1f(float);
float jnf(int, float);
float y0f(float);
float y1f(float);
float ynf(int, float);
