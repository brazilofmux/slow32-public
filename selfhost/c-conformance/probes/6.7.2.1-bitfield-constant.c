// 6.7.2.1p10/6.3.1.3  a constant that does not fit a bit-field's width is truncated, and clang warns
// expect: warnings
#include <stdio.h>
struct bf { unsigned u3 : 3; int s3 : 3; int s1 : 1; _Bool b : 1; };
static struct bf g = { 9, 3, -1, 2 };
static struct bf h = { -1, 0x7, 0, 0 };
int main(void) {
    struct bf l = { 7, -4, 1, 1 };
    l.u3 = 8;
    l.s3 = -5;
    printf("%u %d %d %d %u %d %u %d %d\n", g.u3, g.s3, g.s1, g.b, h.u3, h.s3, l.u3, l.s3, l.s1);
    return 0;
}
