/* 7.12p8 FP_ILOGB0, FP_ILOGBNAN: ilogb's results for zero and NaN, the values the macros name */
#include <math.h>
#include <stdlib.h>
#include <stdio.h>

int main(void) {
    volatile double z = 0.0;
    double n = strtod("nan", 0);
    printf("%d %d\n", ilogb(z) == FP_ILOGB0, ilogb(n) == FP_ILOGBNAN);
    printf("%d %d %d\n", ilogb(1.0), ilogb(1024.5), ilogb(0.25));
    return 0;
}
