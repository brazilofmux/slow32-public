// 6.5.6p9  the difference of two pointers is ptrdiff_t, a signed integer: negative differences divide and compare as such
#include <stdio.h>
int main(void) {
    unsigned char buf[16];
    volatile unsigned char *p = buf + 2, *q = buf + 10;
    long d = p - q;                          /* -8 */
    int half = (p - q) / 2;                  /* -4, not an unsigned quotient */
    int neg = (p - q) < 0;                   /* 1 */
    int sz = (int)sizeof(p - q);
    printf("%ld %d %d %d\n", d, half, neg, sz);
    return 0;
}
