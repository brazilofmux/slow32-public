/* 7.7, 6.2.5p10: double's range and precision include float's, and long double's include double's */
#include <float.h>
#include <stdio.h>

int main(void) {
    printf("DIG %d %d\n", DBL_DIG >= FLT_DIG, LDBL_DIG >= DBL_DIG);
    printf("MANT_DIG %d %d\n", DBL_MANT_DIG >= FLT_MANT_DIG, LDBL_MANT_DIG >= DBL_MANT_DIG);
    printf("EPSILON %d %d\n", DBL_EPSILON <= FLT_EPSILON, LDBL_EPSILON <= DBL_EPSILON);
    printf("MAX %d %d\n", DBL_MAX >= FLT_MAX, LDBL_MAX >= DBL_MAX);
    printf("MIN %d %d\n", DBL_MIN <= FLT_MIN, LDBL_MIN <= DBL_MIN);
    printf("MAX_EXP %d %d\n", DBL_MAX_EXP >= FLT_MAX_EXP, LDBL_MAX_EXP >= DBL_MAX_EXP);
    printf("MIN_EXP %d %d\n", DBL_MIN_EXP <= FLT_MIN_EXP, LDBL_MIN_EXP <= DBL_MIN_EXP);
    printf("sizes %d %d\n", sizeof(double) >= sizeof(float), sizeof(long double) >= sizeof(double));
    return 0;
}
