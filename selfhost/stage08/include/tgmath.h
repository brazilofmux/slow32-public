/* tgmath.h -- C11 7.25, for both SLOW-32 C libraries (runtime/include and
 * selfhost/stage08/include hold the same file).  Real types only:
 * complex is absent (__STDC_NO_COMPLEX__).
 *
 * A macro calls the float, double or long double function by its
 * arguments' types (7.25p3): an integer argument counts as double, and
 * with several generic arguments the widest wins.  __S32_TG_T maps an
 * argument to a value of that type, and the sum of two such values has
 * the type the usual conversions give; _Generic does not evaluate its
 * controlling expression, so each argument is evaluated once.  long
 * double is double here but a type of its own, so it has its branch. */
#ifndef _TGMATH_H
#define _TGMATH_H

#include <math.h>

#ifndef __cplusplus
#define __S32_TG_T(x) _Generic((x), float: (float)0, long double: (long double)0, default: (double)0)
#define __S32_TG_FN(t, fn) _Generic((t), float: fn##f, long double: fn##l, default: fn)
#define __S32_TG1(fn, x)        __S32_TG_FN(__S32_TG_T(x), fn)(x)
#define __S32_TG2(fn, x, y)     __S32_TG_FN(__S32_TG_T(x) + __S32_TG_T(y), fn)(x, y)
#define __S32_TG3(fn, x, y, z)  __S32_TG_FN(__S32_TG_T(x) + __S32_TG_T(y) + __S32_TG_T(z), fn)(x, y, z)

#define acos(x) __S32_TG1(acos, x)
#define asin(x) __S32_TG1(asin, x)
#define atan(x) __S32_TG1(atan, x)
#define acosh(x) __S32_TG1(acosh, x)
#define asinh(x) __S32_TG1(asinh, x)
#define atanh(x) __S32_TG1(atanh, x)
#define cos(x) __S32_TG1(cos, x)
#define sin(x) __S32_TG1(sin, x)
#define tan(x) __S32_TG1(tan, x)
#define cosh(x) __S32_TG1(cosh, x)
#define sinh(x) __S32_TG1(sinh, x)
#define tanh(x) __S32_TG1(tanh, x)
#define exp(x) __S32_TG1(exp, x)
#define log(x) __S32_TG1(log, x)
#define sqrt(x) __S32_TG1(sqrt, x)
#define fabs(x) __S32_TG1(fabs, x)
#define cbrt(x) __S32_TG1(cbrt, x)
#define ceil(x) __S32_TG1(ceil, x)
#define erf(x) __S32_TG1(erf, x)
#define erfc(x) __S32_TG1(erfc, x)
#define exp2(x) __S32_TG1(exp2, x)
#define expm1(x) __S32_TG1(expm1, x)
#define floor(x) __S32_TG1(floor, x)
#define lgamma(x) __S32_TG1(lgamma, x)
#define log10(x) __S32_TG1(log10, x)
#define log1p(x) __S32_TG1(log1p, x)
#define log2(x) __S32_TG1(log2, x)
#define logb(x) __S32_TG1(logb, x)
#define nearbyint(x) __S32_TG1(nearbyint, x)
#define rint(x) __S32_TG1(rint, x)
#define round(x) __S32_TG1(round, x)
#define tgamma(x) __S32_TG1(tgamma, x)
#define trunc(x) __S32_TG1(trunc, x)
#define ilogb(x) __S32_TG1(ilogb, x)
#define llrint(x) __S32_TG1(llrint, x)
#define llround(x) __S32_TG1(llround, x)
#define lrint(x) __S32_TG1(lrint, x)
#define lround(x) __S32_TG1(lround, x)
#define pow(x, y) __S32_TG2(pow, x, y)
#define atan2(x, y) __S32_TG2(atan2, x, y)
#define copysign(x, y) __S32_TG2(copysign, x, y)
#define fdim(x, y) __S32_TG2(fdim, x, y)
#define fmax(x, y) __S32_TG2(fmax, x, y)
#define fmin(x, y) __S32_TG2(fmin, x, y)
#define fmod(x, y) __S32_TG2(fmod, x, y)
#define hypot(x, y) __S32_TG2(hypot, x, y)
#define nextafter(x, y) __S32_TG2(nextafter, x, y)
#define remainder(x, y) __S32_TG2(remainder, x, y)
#define frexp(x, n) __S32_TG_FN(__S32_TG_T(x), frexp)(x, n)
#define ldexp(x, n) __S32_TG_FN(__S32_TG_T(x), ldexp)(x, n)
#define scalbn(x, n) __S32_TG_FN(__S32_TG_T(x), scalbn)(x, n)
#define scalbln(x, n) __S32_TG_FN(__S32_TG_T(x), scalbln)(x, n)
#define nexttoward(x, n) __S32_TG_FN(__S32_TG_T(x), nexttoward)(x, n)
#define fma(x, y, z) __S32_TG3(fma, x, y, z)
#define remquo(x, y, q) __S32_TG_FN(__S32_TG_T(x) + __S32_TG_T(y), remquo)(x, y, q)
#endif

#endif
