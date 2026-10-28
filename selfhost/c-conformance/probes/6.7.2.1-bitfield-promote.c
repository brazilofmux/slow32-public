// 6.7.2.1p10/6.3.1.1p2  an unsigned bit-field narrower than int promotes to int
#include <stdio.h>
struct b { unsigned u : 4; };
int main(void) {
    struct b x; x.u = 1;
    printf("%d\n", x.u - 2 < 0);
    return 0;
}
