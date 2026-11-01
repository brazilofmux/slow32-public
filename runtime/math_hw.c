/*
 * SLOW-32 Math Hardware Helpers
 *
 * Small wrapper functions that should lower to native SLOW-32
 * floating-point instructions. These are compiled with -fno-builtin
 * -fno-math-errno to avoid recursive lowering and math-errno fences.
 */

#include <errno.h>
#include <math.h>

double fabs(double x) {
    return __builtin_fabs(x);  /* emits FABS.D */
}

/* sqrt of a negative number is a domain error (7.12.7.5p2): math_errhandling
 * is MATH_ERRNO (runtime/libm).  NaN and -0 are not less than 0. */
double sqrt(double x) {
    if (x < 0) errno = EDOM;
    return __builtin_sqrt(x);  /* emits FSQRT.D */
}

float fabsf(float x) {
    return __builtin_fabsf(x);  /* emits FABS.S */
}

float sqrtf(float x) {
    if (x < 0) errno = EDOM;
    return __builtin_sqrtf(x);  /* emits FSQRT.S */
}
