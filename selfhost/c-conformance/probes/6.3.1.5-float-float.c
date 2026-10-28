// 6.3.1.5p1  float<->double: widening exact, narrowing rounds
#include <stdio.h>
int main(void) {
    volatile double d = 0.1;
    float f = (float)d;
    double back = f;
    volatile float g = 1.5f;
    double w = g;
    float big = (float)1e30;
    printf("%.17g %.17g %.17g %d\n", back, w, (double)big, (double)f == d);
    return 0;
}
