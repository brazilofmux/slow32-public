/* 7.12p4-p5 INFINITY, NAN: float constants for positive infinity and a quiet NaN */
#include <math.h>
#include <stdio.h>

int main(void) {
    float i = INFINITY, n = NAN;
    printf("%d %d %d\n", sizeof INFINITY == sizeof(float), isinf(i) != 0, i > 0);
    printf("%d %d %d\n", sizeof NAN == sizeof(float), isnan(n) != 0, n != n);
    return 0;
}
