// 6.5.5p3-6  * / %: integer division truncates toward zero; (a/b)*b + a%b == a
#include <stdio.h>
int main(void) {
    volatile int a = -7, b = 2;
    volatile unsigned ua = 7; volatile long long la = -7000000000LL;
    printf("%d %d %d %d\n", a / b, a % b, -a / -b, a % -b);
    printf("%u %u %lld %lld\n", ua / 2, ua % 3, la / 3, la % 3);
    printf("%d %.3f\n", (a / b) * b + a % b == a, 7.0 / 2 * 3);
    return 0;
}
