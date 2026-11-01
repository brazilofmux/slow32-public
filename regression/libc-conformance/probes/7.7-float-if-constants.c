/* 7.7, 5.2.4.2.2: the integer-valued <float.h> macros (other than FLT_ROUNDS) are usable in #if */
#include <float.h>
#include <stdio.h>

#if FLT_RADIX >= 2 && FLT_MANT_DIG > 0 && DBL_MANT_DIG >= FLT_MANT_DIG && LDBL_MANT_DIG >= DBL_MANT_DIG \
    && FLT_DIG >= 6 && DBL_DIG >= 10 && LDBL_DIG >= 10 \
    && FLT_MIN_EXP < 0 && DBL_MIN_EXP < 0 && LDBL_MIN_EXP < 0 \
    && FLT_MIN_10_EXP <= -37 && DBL_MIN_10_EXP <= -37 && LDBL_MIN_10_EXP <= -37 \
    && FLT_MAX_EXP > 0 && DBL_MAX_EXP > 0 && LDBL_MAX_EXP > 0 \
    && FLT_MAX_10_EXP >= 37 && DBL_MAX_10_EXP >= 37 && LDBL_MAX_10_EXP >= 37
#define OK 1
#else
#define OK 0
#endif

int main(void) {
    printf("%d\n", OK);
    return 0;
}
