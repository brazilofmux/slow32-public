/* 7.7, 5.2.4.2.2p9-p13: the C11 additions FLT_EVAL_METHOD, *_HAS_SUBNORM, *_DECIMAL_DIG, DECIMAL_DIG and *_TRUE_MIN */
#include <float.h>
#include <stdio.h>

int main(void) {
    printf("EVAL_METHOD >= -1: %d\n", FLT_EVAL_METHOD >= -1);
    printf("HAS_SUBNORM in -1..1: %d %d %d\n", FLT_HAS_SUBNORM >= -1 && FLT_HAS_SUBNORM <= 1,
           DBL_HAS_SUBNORM >= -1 && DBL_HAS_SUBNORM <= 1, LDBL_HAS_SUBNORM >= -1 && LDBL_HAS_SUBNORM <= 1);
    printf("DECIMAL_DIG: %d %d %d %d\n", FLT_DECIMAL_DIG >= 6, DBL_DECIMAL_DIG >= 10, LDBL_DECIMAL_DIG >= 10,
           DECIMAL_DIG >= 10);
    printf("DECIMAL_DIG > DIG: %d %d %d\n", FLT_DECIMAL_DIG > FLT_DIG, DBL_DECIMAL_DIG > DBL_DIG,
           LDBL_DECIMAL_DIG > LDBL_DIG);
    printf("DECIMAL_DIG >= LDBL_DECIMAL_DIG: %d\n", DECIMAL_DIG >= LDBL_DECIMAL_DIG);
    printf("TRUE_MIN in (0, MIN]: %d %d %d\n", FLT_TRUE_MIN > 0 && FLT_TRUE_MIN <= FLT_MIN,
           DBL_TRUE_MIN > 0 && DBL_TRUE_MIN <= DBL_MIN, LDBL_TRUE_MIN > 0 && LDBL_TRUE_MIN <= LDBL_MIN);
    return 0;
}
