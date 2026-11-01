/* 7.12p3 HUGE_VAL: HUGE_VAL, HUGE_VALF and HUGE_VALL are positive constants of type double, float and long double */
#include <math.h>
#include <stdio.h>

int main(void) {
    printf("%d %d %d\n", HUGE_VAL > 0, HUGE_VALF > 0, HUGE_VALL > 0);
    printf("%d %d %d\n", sizeof HUGE_VAL == sizeof(double), sizeof HUGE_VALF == sizeof(float),
           sizeof HUGE_VALL == sizeof(long double));
    return 0;
}
