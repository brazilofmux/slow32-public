// 6.3.1.1p2  an unsigned bit-field narrower than int promotes to (signed) int
#include <stdio.h>
struct bf { unsigned u3 : 3; unsigned u31 : 31; };
int main(void) {
    struct bf f;
    f.u3 = 7; f.u31 = 1;
    printf("%d %d %d\n", -f.u3 < 0, f.u3 - 8 < 0, -f.u31 < 0);
    return 0;
}
