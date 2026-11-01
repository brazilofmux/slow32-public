/* math.h -- C11 7.12, for both SLOW-32 C libraries (runtime/include and
 * selfhost/stage08/include hold the same file).
 *
 * The library is musl's libm (runtime/libm, MIT); the prototypes below
 * are taken from musl's public <math.h>.  long double is double on
 * SLOW-32 (both compilers), so FLT_EVAL_METHOD is 0, float_t and double_t
 * are float and double, and the classification macros need two cases.
 *
 * Errors: math_errhandling is MATH_ERRNO.  SLOW-32 has no floating-point
 * exception flags, so a domain error sets errno to EDOM, and a pole error
 * or overflow to ERANGE (runtime/libm/errno.c); underflow does not set it.
 */
#ifndef _MATH_H
#define _MATH_H

#ifdef __cplusplus
extern "C" {
#endif

typedef float float_t;
typedef double double_t;

#define HUGE_VAL   (__builtin_huge_val())
#define HUGE_VALF  (__builtin_huge_valf())
#define HUGE_VALL  (__builtin_huge_vall())
#define INFINITY   (__builtin_inff())
#define NAN        (__builtin_nanf(""))

#define FP_NAN       0
#define FP_INFINITE  1
#define FP_ZERO      2
#define FP_SUBNORMAL 3
#define FP_NORMAL    4

#define FP_ILOGBNAN  (-1 - 0x7fffffff)
#define FP_ILOGB0    FP_ILOGBNAN

#define MATH_ERRNO      1
#define MATH_ERREXCEPT  2
#define math_errhandling MATH_ERRNO

/* POSIX constants (the runtime's CORDIC core and ported programs use them) */
#define M_E        2.7182818284590452354
#define M_LOG2E    1.4426950408889634074
#define M_LOG10E   0.43429448190325182765
#define M_LN2      0.69314718055994530942
#define M_LN10     2.30258509299404568402
#define M_PI       3.14159265358979323846
#define M_PI_2     1.57079632679489661923
#define M_PI_4     0.78539816339744830962
#define M_1_PI     0.31830988618379067154
#define M_2_PI     0.63661977236758134308
#define M_2_SQRTPI 1.12837916709551257390
#define M_SQRT2    1.41421356237309504880
#define M_SQRT1_2  0.70710678118654752440

int __fpclassify(double);
int __fpclassifyf(float);
int __fpclassifyl(long double);
int __signbit(double);
int __signbitf(float);
int __signbitl(long double);

/* Classification and comparison (7.12.3, 7.12.14) are macros in C; each
 * argument is evaluated once (sizeof does not evaluate it).  C++ has them
 * as overloads in libc++'s <cmath>, which these would clash with. */
#ifndef __cplusplus
#define __S32_FP(x, f, d) (sizeof(x) == sizeof(float) ? f((float)(x)) : d((double)(x)))
#define fpclassify(x)  __S32_FP(x, __fpclassifyf, __fpclassify)
#define isinf(x)       (fpclassify(x) == FP_INFINITE)
#define isnan(x)       (fpclassify(x) == FP_NAN)
#define isnormal(x)    (fpclassify(x) == FP_NORMAL)
#define isfinite(x)    (fpclassify(x) > FP_INFINITE)
#define signbit(x)     __S32_FP(x, __signbitf, __signbit)

static inline int __s32_isunordered(double x, double y) { return x != x || y != y; }
static inline int __s32_isless(double x, double y) { return !__s32_isunordered(x, y) && x < y; }
static inline int __s32_islessequal(double x, double y) { return !__s32_isunordered(x, y) && x <= y; }
static inline int __s32_islessgreater(double x, double y) { return !__s32_isunordered(x, y) && x != y; }
/* float converts to double exactly, so one set of helpers serves both */
#define isunordered(x, y)    __s32_isunordered((x), (y))
#define isless(x, y)         __s32_isless((x), (y))
#define islessequal(x, y)    __s32_islessequal((x), (y))
#define isgreater(x, y)      __s32_isless((y), (x))
#define isgreaterequal(x, y) __s32_islessequal((y), (x))
#define islessgreater(x, y)  __s32_islessgreater((x), (y))
#endif

double acos(double);
float acosf(float);
long double acosl(long double);
double acosh(double);
float acoshf(float);
long double acoshl(long double);
double asin(double);
float asinf(float);
long double asinl(long double);
double asinh(double);
float asinhf(float);
long double asinhl(long double);
double atan(double);
float atanf(float);
long double atanl(long double);
double atan2(double, double);
float atan2f(float, float);
long double atan2l(long double, long double);
double atanh(double);
float atanhf(float);
long double atanhl(long double);
double cbrt(double);
float cbrtf(float);
long double cbrtl(long double);
double ceil(double);
float ceilf(float);
long double ceill(long double);
double copysign(double, double);
float copysignf(float, float);
long double copysignl(long double, long double);
double cos(double);
float cosf(float);
long double cosl(long double);
double cosh(double);
float coshf(float);
long double coshl(long double);
double erf(double);
float erff(float);
long double erfl(long double);
double erfc(double);
float erfcf(float);
long double erfcl(long double);
double exp(double);
float expf(float);
long double expl(long double);
double exp2(double);
float exp2f(float);
long double exp2l(long double);
double expm1(double);
float expm1f(float);
long double expm1l(long double);
double fabs(double);
float fabsf(float);
long double fabsl(long double);
double fdim(double, double);
float fdimf(float, float);
long double fdiml(long double, long double);
double floor(double);
float floorf(float);
long double floorl(long double);
double fma(double, double, double);
float fmaf(float, float, float);
long double fmal(long double, long double, long double);
double fmax(double, double);
float fmaxf(float, float);
long double fmaxl(long double, long double);
double fmin(double, double);
float fminf(float, float);
long double fminl(long double, long double);
double fmod(double, double);
float fmodf(float, float);
long double fmodl(long double, long double);
double frexp(double, int *);
float frexpf(float, int *);
long double frexpl(long double, int *);
double hypot(double, double);
float hypotf(float, float);
long double hypotl(long double, long double);
int ilogb(double);
int ilogbf(float);
int ilogbl(long double);
double ldexp(double, int);
float ldexpf(float, int);
long double ldexpl(long double, int);
double lgamma(double);
float lgammaf(float);
long double lgammal(long double);
long long llrint(double);
long long llrintf(float);
long long llrintl(long double);
long long llround(double);
long long llroundf(float);
long long llroundl(long double);
double log(double);
float logf(float);
long double logl(long double);
double log10(double);
float log10f(float);
long double log10l(long double);
double log1p(double);
float log1pf(float);
long double log1pl(long double);
double log2(double);
float log2f(float);
long double log2l(long double);
double logb(double);
float logbf(float);
long double logbl(long double);
long lrint(double);
long lrintf(float);
long lrintl(long double);
long lround(double);
long lroundf(float);
long lroundl(long double);
double modf(double, double *);
float modff(float, float *);
long double modfl(long double, long double *);
double nan(const char *);
float nanf(const char *);
long double nanl(const char *);
double nearbyint(double);
float nearbyintf(float);
long double nearbyintl(long double);
double nextafter(double, double);
float nextafterf(float, float);
long double nextafterl(long double, long double);
double nexttoward(double, long double);
float nexttowardf(float, long double);
long double nexttowardl(long double, long double);
double pow(double, double);
float powf(float, float);
long double powl(long double, long double);
double remainder(double, double);
float remainderf(float, float);
long double remainderl(long double, long double);
double remquo(double, double, int *);
float remquof(float, float, int *);
long double remquol(long double, long double, int *);
double rint(double);
float rintf(float);
long double rintl(long double);
double round(double);
float roundf(float);
long double roundl(long double);
double scalbln(double, long);
float scalblnf(float, long);
long double scalblnl(long double, long);
double scalbn(double, int);
float scalbnf(float, int);
long double scalbnl(long double, int);
double sin(double);
float sinf(float);
long double sinl(long double);
double sinh(double);
float sinhf(float);
long double sinhl(long double);
double sqrt(double);
float sqrtf(float);
long double sqrtl(long double);
double tan(double);
float tanf(float);
long double tanl(long double);
double tanh(double);
float tanhf(float);
long double tanhl(long double);
double tgamma(double);
float tgammaf(float);
long double tgammal(long double);
double trunc(double);
float truncf(float);
long double truncl(long double);

#ifdef __cplusplus
}
#endif

#endif
