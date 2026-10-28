// 6.10.6p2  the standard STDC pragmas with each on-off-switch value
#include <stdio.h>
#pragma STDC FP_CONTRACT OFF
#pragma STDC FENV_ACCESS OFF
#pragma STDC CX_LIMITED_RANGE DEFAULT
static double mul_add(double a, double b, double c) {
#pragma STDC FP_CONTRACT ON
    return a * b + c;
}
int main(void) {
#pragma STDC CX_LIMITED_RANGE ON
    printf("%d\n", (int)mul_add(3.0, 4.0, 5.0));
    return 0;
}
