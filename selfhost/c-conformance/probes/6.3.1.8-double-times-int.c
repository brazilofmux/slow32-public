// 6.3.1.8p1  double * integer constant: the int converts to double (power-of-two constants)
#include <stdio.h>
int main(void) {
    volatile double v = 3.0;
    double d = v;
    printf("%.3f %.3f %.3f %.3f\n", d * 2, d * 3, d * 4, 8 * d);
    return 0;
}
