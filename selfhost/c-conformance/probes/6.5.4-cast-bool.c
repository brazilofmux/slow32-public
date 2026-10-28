// 6.5.4p5 (6.3.1.2)  a cast to _Bool yields 1 for any nonzero scalar (double, wide int, pointer)
#include <stdio.h>
int main(void) {
    volatile double h = 0.5; volatile int big = 256; int x;
    printf("%d %d %d\n", (_Bool)h, (_Bool)big, (_Bool)&x);
    return 0;
}
