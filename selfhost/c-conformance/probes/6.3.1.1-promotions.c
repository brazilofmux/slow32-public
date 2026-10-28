// 6.3.1.1p2  integer promotions: char/short/bit-field operands of arithmetic become int
#include <stdio.h>
struct bf { unsigned u3 : 3; int s3 : 3; };
int main(void) {
    unsigned char a = 200, b = 100;
    signed char sc = -1;
    short sh = 30000;
    struct bf f;
    f.u3 = 7; f.s3 = -1;
    int sum = a + b;                       /* 300, not 44 */
    int cmp = sc < (unsigned char)1;       /* both promote to int: -1 < 1 */
    int prod = sh * 2;                     /* 60000 in int */
    int bfv = f.u3 + 1;                    /* 8 */
    int bfs = f.s3 - 1;                    /* -2 */
    printf("%d %d %d %d %d\n", sum, cmp, prod, bfv, bfs);
    return 0;
}
