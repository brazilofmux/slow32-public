// 6.7.2p2/6.3.1.2p1  a _Bool object holds 0 or 1: nonzero values convert to 1 on init and assignment
#include <stdio.h>
int main(void) {
    _Bool n = 2; _Bool m; m = 256; static _Bool s = 5; int k = 7; _Bool q = k; _Bool d = 0.5;
    printf("%d %d %d %d %d\n", n, m, s, q, d);
    return 0;
}
