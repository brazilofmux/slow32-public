/* math.h -- s12cc-compatible stub
 * Declarations for the math functions actually used by ported C
 * (currently dbt's shadow interpreter and translate.c).  More can
 * be added as needed; libc_a64/math_stubs.c is the implementation.
 */
#ifndef _MATH_H
#define _MATH_H

double sqrt(double x);
double pow(double x, double y);
double exp(double x);
double log(double x);
double sin(double x);
double cos(double x);
double tan(double x);
double fabs(double x);
double floor(double x);
double ceil(double x);
double fmod(double x, double y);
double ldexp(double x, int e);
double frexp(double x, int *e);
double modf(double x, double *iptr);

float  sqrtf(float x);
float  powf(float x, float y);
float  expf(float x);
float  logf(float x);
float  sinf(float x);
float  cosf(float x);
float  tanf(float x);
float  fabsf(float x);
float  floorf(float x);
float  ceilf(float x);
float  fmodf(float x, float y);
float  ldexpf(float x, int e);
float  frexpf(float x, int *e);
float  modff(float x, float *iptr);

/* The rest of what the library defines (runtime/math_soft.c, built into
 * the self-hosted library too).  Until 2026-10 these had no declaration
 * here, and a caller got an int back in place of its double. */
double asin(double x);
double acos(double x);
double atan(double x);
double atan2(double y, double x);
double sinh(double x);
double cosh(double x);
double tanh(double x);
double log10(double x);
double log2(double x);
double cbrt(double x);
double round(double x);
double trunc(double x);
double copysign(double x, double y);

float  asinf(float x);
float  acosf(float x);
float  atanf(float x);
float  atan2f(float y, float x);
float  sinhf(float x);
float  coshf(float x);
float  tanhf(float x);
float  log10f(float x);
float  log2f(float x);
float  cbrtf(float x);
float  roundf(float x);
float  truncf(float x);
float  copysignf(float x, float y);

int    isnan(double x);
int    isinf(double x);
int    isfinite(double x);


/* Constants (POSIX), needed by the runtime's CORDIC core. */
#define M_E        2.7182818284590452354
#define M_LN2      0.69314718055994530942
#define M_LN10     2.30258509299404568402
#define M_PI       3.14159265358979323846
#define M_PI_2     1.57079632679489661923
#define M_PI_4     0.78539816339744830962
#define M_1_PI     0.31830988618379067154
#define M_2_PI     0.63661977236758134308
#define M_SQRT2    1.41421356237309504880
#define M_SQRT1_2  0.70710678118654752440

#endif
