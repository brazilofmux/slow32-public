/* 7.25 tgmath.h: a type-generic macro picks the float function for a float argument */
#include <tgmath.h>
#include <stdio.h>

int main(void) {
    volatile float f = 2.0f;
    volatile double d = 2.0;
    printf("%d %d %.5g\n", (int)sizeof(sqrt(f)), (int)sizeof(sqrt(d)), (double)sqrt(f));
    return 0;
}
