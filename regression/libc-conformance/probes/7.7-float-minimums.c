/* 7.7, 5.2.4.2.2p9-p13: <float.h> values meet the standard's minimum magnitudes */
#include <float.h>
#include <stdio.h>

int main(void) {
    printf("FLT_RADIX >= 2: %d\n", FLT_RADIX >= 2);
    printf("DIG: %d %d %d\n", FLT_DIG >= 6, DBL_DIG >= 10, LDBL_DIG >= 10);
    printf("MIN_10_EXP: %d %d %d\n", FLT_MIN_10_EXP <= -37, DBL_MIN_10_EXP <= -37, LDBL_MIN_10_EXP <= -37);
    printf("MAX_10_EXP: %d %d %d\n", FLT_MAX_10_EXP >= 37, DBL_MAX_10_EXP >= 37, LDBL_MAX_10_EXP >= 37);
    printf("MAX: %d %d %d\n", FLT_MAX >= 1e37f, DBL_MAX >= 1e37, LDBL_MAX >= 1e37L);
    printf("EPSILON: %d %d %d\n", FLT_EPSILON <= 1e-5f, DBL_EPSILON <= 1e-9, LDBL_EPSILON <= 1e-9L);
    printf("MIN: %d %d %d\n", FLT_MIN <= 1e-37f, DBL_MIN <= 1e-37, LDBL_MIN <= 1e-37L);
    printf("MANT_DIG positive: %d %d %d\n", FLT_MANT_DIG > 0, DBL_MANT_DIG > 0, LDBL_MANT_DIG > 0);
    return 0;
}
