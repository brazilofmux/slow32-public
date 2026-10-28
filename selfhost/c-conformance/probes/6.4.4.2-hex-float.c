// 6.4.4.2p1,p3  hexadecimal floating constants with binary exponent
#include <stdio.h>
int main(void) {
    double a = 0x1p4, b = 0x1.8p1, c = 0X.8P0, d = 0xAp-2;
    float f = 0x1.fffffep127f;
    printf("%.3f %.3f %.3f %.3f %d\n", a, b, c, d, f > 3e38f);
    return 0;
}
