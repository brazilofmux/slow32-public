/* 7.12p2 float_t, double_t: floating types at least as wide as float and double */
#include <math.h>
#include <stdio.h>

int main(void) {
    float_t f = 1.5f;
    double_t d = 2.5;
    printf("%d %d %g %g\n", sizeof(float_t) >= sizeof(float), sizeof(double_t) >= sizeof(double),
           (double)f, (double)d);
    return 0;
}
