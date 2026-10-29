// 6.5.3.4p5, 7.19p2  sizeof and offsetof have type size_t, an unsigned type
#include <stdio.h>
#include <stddef.h>
struct s { int a; char b; double c; };
int main(void) {
    int n = -1;
    long long w = (n + 1) * sizeof(double) - 8;   /* size_t arithmetic: wraps, then widens */
    long long x = -sizeof(struct s);
    long long y = -offsetof(struct s, c);
    int lt = n < sizeof(int);                     /* n converts to size_t */
    printf("%lld %lld %lld %d\n", w, x, y, lt);
    return 0;
}
