/* Hardware square root for the stage08 libc.  __fp64_sqrt / __fp32_sqrt
 * are the libcall names the stage08 backend lowers to FSQRT.D / FSQRT.S
 * inline, so each wrapper is one instruction.  Built in phase 2 only:
 * the stage07 bootstrap compiler does not know these names.  regal's
 * pow (runtime/math_soft.c) reaches sqrt through asin. */
#include <errno.h>
double __fp64_sqrt(double x);
float __fp32_sqrt(float x);
/* a negative argument is a domain error (7.12.7.5p2; math_errhandling is
 * MATH_ERRNO); NaN and -0 are not less than 0.  fabs and fabsf are
 * musl's (runtime/libm). */
double sqrt(double x) { if (x < 0) errno = EDOM; return __fp64_sqrt(x); }
float sqrtf(float x) { if (x < 0) errno = EDOM; return __fp32_sqrt(x); }
