// 6.5.3.3p2-5  unary - negates, ~ complements after promotion, ! yields int 0/1
#include <stdio.h>
int main(void) {
    unsigned char uc = 1; unsigned u = 1;
    double d = 2.5; void *np = 0;
    printf("%d %d\n", ~uc, (int)sizeof(!d));
    printf("%u %d %.1f %d %d\n", -u, -(-3), -d, !np, !d);
    long long ll = 5; printf("%lld %lld\n", -ll, ~ll);
    return 0;
}
