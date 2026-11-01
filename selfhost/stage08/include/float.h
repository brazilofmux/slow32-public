/* float.h -- C11 5.2.4.2.2, for both SLOW-32 C libraries (runtime/include
 * and selfhost/stage08/include hold the same file).  IEEE 754 binary32
 * and binary64; long double is double. */
#ifndef _FLOAT_H
#define _FLOAT_H

#define FLT_RADIX 2
#define FLT_ROUNDS 1          /* to nearest: the machine's only mode */
#define FLT_EVAL_METHOD 0     /* each type evaluates in its own precision */
#define DECIMAL_DIG 17        /* digits that round-trip the widest type */

/* float (32-bit IEEE 754 single precision) */
#define FLT_DIG        6
#define FLT_EPSILON    1.19209290e-07F
#define FLT_MANT_DIG   24
#define FLT_MAX        3.40282347e+38F
#define FLT_MAX_10_EXP 38
#define FLT_MAX_EXP    128
#define FLT_MIN        1.17549435e-38F
#define FLT_MIN_10_EXP (-37)
#define FLT_MIN_EXP    (-125)
#define FLT_DECIMAL_DIG 9
#define FLT_HAS_SUBNORM 1
#define FLT_TRUE_MIN   0x1p-149F

/* double (64-bit IEEE 754 double precision) */
#define DBL_DIG        15
#define DBL_EPSILON    2.2204460492503131e-16
#define DBL_MANT_DIG   53
#define DBL_MAX        1.7976931348623157e+308
#define DBL_MAX_10_EXP 308
#define DBL_MAX_EXP    1024
#define DBL_MIN        2.2250738585072014e-308
#define DBL_MIN_10_EXP (-307)
#define DBL_MIN_EXP    (-1021)
#define DBL_DECIMAL_DIG 17
#define DBL_HAS_SUBNORM 1
#define DBL_TRUE_MIN   0x1p-1074

/* long double (same as double on SLOW-32) */
#define LDBL_DIG        DBL_DIG
#define LDBL_EPSILON    DBL_EPSILON
#define LDBL_MANT_DIG   DBL_MANT_DIG
#define LDBL_MAX        DBL_MAX
#define LDBL_MAX_10_EXP DBL_MAX_10_EXP
#define LDBL_MAX_EXP    DBL_MAX_EXP
#define LDBL_MIN        DBL_MIN
#define LDBL_MIN_10_EXP DBL_MIN_10_EXP
#define LDBL_MIN_EXP    DBL_MIN_EXP
#define LDBL_DECIMAL_DIG DBL_DECIMAL_DIG
#define LDBL_HAS_SUBNORM DBL_HAS_SUBNORM
#define LDBL_TRUE_MIN   DBL_TRUE_MIN

#endif
