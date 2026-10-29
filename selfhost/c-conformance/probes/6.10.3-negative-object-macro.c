// 6.10.3p9/6.4.4.1p5  an object-like macro whose replacement is a negative integer: the expression it forms is a signed int
#include <stdio.h>
#define N -2000
#define M (-7)
int main(void) {
    int a = N / 2;
    long long b = N;
    int c = N < 0;
    int d = M % 3;
    printf("%d %lld %d %d %d\n", a, b, c, d, (int)sizeof(N));
    return 0;
}
