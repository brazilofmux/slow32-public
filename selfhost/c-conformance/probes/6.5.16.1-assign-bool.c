// 6.5.16.1p2 (6.3.1.2)  assigning or initializing a _Bool from a nonzero double, wide int or pointer stores 1
#include <stdio.h>
#include <stdbool.h>
int main(void) {
    volatile double h = 0.1; volatile int big = 512; int x;
    bool a = h, b = big, c = &x;
    bool d; d = h; bool e; e = big; bool f; f = &x;
    printf("%d %d %d %d %d %d\n", a, b, c, d, e, f);
    return 0;
}
