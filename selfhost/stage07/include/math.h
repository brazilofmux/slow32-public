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

int    isnan(double x);
int    isinf(double x);
int    isfinite(double x);

#endif
