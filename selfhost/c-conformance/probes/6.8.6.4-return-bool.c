// 6.8.6.4p3  returning an int from a _Bool function converts it as assignment does (nonzero -> 1)
#include <stdio.h>
static _Bool tob(int x) { return x; }
int main(void) {
    printf("%d %d %d\n", tob(256), tob(2), tob(0));
    return 0;
}
